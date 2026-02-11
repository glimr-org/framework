import morphdom from "morphdom";
import { CONFIG, EVENT_TYPES } from "@/config";
import type { EventPayload, Modifiers, ServerMessage } from "@/types";
import {
  buildWsUrl,
  collectSpecialVars,
  getNodeKey,
  parseModifiers,
  restoreFocus,
  saveFocus,
} from "@/live/utils";

/**
 * Debounce timers are stored per-element in a WeakMap so that
 * removed DOM elements automatically release their timer
 * references through garbage collection, avoiding memory leaks
 * across repeated morphdom patches.
 */
const debounceTimers = new WeakMap<Element, ReturnType<typeof setTimeout>>();

/**
 * Each live container on the page needs its own isolated
 * WebSocket connection, event wiring, and DOM patching
 * lifecycle. Encapsulating all of this in a class ties the
 * connection lifetime to the container element and prevents
 * cross-contamination between multiple live regions on the same
 * page.
 *
 * The two-phase initialization (connect first, then wait for
 * the server to accept the init message) lets the class queue
 * events that fire before the handshake completes, so no user
 * interactions are lost during the brief startup window.
 */
export class LoomLive {
  private container: HTMLElement;
  private module: string;
  private initialProps: string;
  private wsUrlOverride: string | null;
  private socket: WebSocket | null = null;
  private reconnectAttempts = 0;
  private connected = false;
  private initialized = false;
  private pendingEvents: EventPayload[] = [];

  constructor(container: HTMLElement) {
    this.container = container;
    this.module = container.dataset.lLive!;
    this.initialProps = container.dataset.lProps || "{}";
    this.wsUrlOverride = container.dataset.lWs || null;

    this.connect();
    this.attachEventListeners();
  }

  /**
   * The WebSocket URL must adapt to the page's protocol (ws/wss)
   * and host so the client works behind proxies and on any domain
   * without hardcoded URLs. The optional data-l-ws override
   * exists for development setups where the WebSocket server runs
   * on a different port than the page server.
   *
   * Wiring all four socket callbacks here keeps the full
   * connection lifecycle visible in one place rather than
   * scattered across the class.
   */
  private connect(): void {
    const wsUrl = buildWsUrl(this.wsUrlOverride, window.location);

    this.socket = new WebSocket(wsUrl);

    this.socket.onopen = () => {
      console.log("[Loom] Connected:", this.module);

      this.connected = true;
      this.reconnectAttempts = 0;
      this.sendInit();
    };

    this.socket.onmessage = (event) => {
      this.handleMessage(JSON.parse(event.data));
    };

    this.socket.onclose = () => {
      console.log("[Loom] Disconnected:", this.module);

      this.connected = false;
      this.initialized = false;
      this.attemptReconnect();
    };

    this.socket.onerror = (error) => {
      console.error("[Loom] WebSocket error:", error);
    };
  }

  /**
   * The server-side actor can't start until it knows which
   * template module to run and what the initial props are.
   * Sending this as the first message after connection lets the
   * server validate the module name against its registry before
   * accepting any events.
   *
   * Flushing pendingEvents immediately after ensures that any
   * interactions the user performed during the brief connection
   * window are delivered in order.
   */
  private sendInit(): void {
    this.socket!.send(
      JSON.stringify({
        type: "init",
        module: this.module,
        props: this.initialProps,
      }),
    );
    this.initialized = true;

    while (this.pendingEvents.length > 0) {
      this.send(this.pendingEvents.shift()!);
    }
  }

  /**
   * Network interruptions and server restarts are expected in
   * production. Exponential backoff avoids hammering the server
   * with rapid reconnect attempts while still recovering quickly
   * from brief hiccups. The attempt cap prevents infinite retries
   * when the server is genuinely down.
   */
  private attemptReconnect(): void {
    if (this.reconnectAttempts >= CONFIG.maxReconnectAttempts) {
      console.error("[Loom] Max reconnect attempts reached");
      return;
    }

    this.reconnectAttempts++;
    const delay =
      CONFIG.reconnectInterval * Math.pow(2, this.reconnectAttempts - 1);

    console.log(
      `[Loom] Reconnecting in ${delay}ms (attempt ${this.reconnectAttempts})`,
    );

    setTimeout(() => {
      if (!this.connected) {
        this.connect();
      }
    }, delay);
  }

  /**
   * Events can fire before the WebSocket handshake completes —
   * for example, a user clicking a button while the connection is
   * still opening. Queuing these events and flushing them after
   * init ensures no user interaction is silently dropped.
   */
  private send(data: EventPayload): void {
    if (
      this.connected &&
      this.initialized &&
      this.socket?.readyState === WebSocket.OPEN
    ) {
      this.socket.send(JSON.stringify(data));
    } else {
      this.pendingEvents.push(data);
    }
  }

  /**
   * The server sends three message types: patches with new HTML,
   * redirects for navigation, and errors for diagnostics. Routing
   * them through a single handler keeps the protocol contract
   * explicit and makes it easy to extend with new message types
   * later.
   */
  private handleMessage(message: ServerMessage): void {
    switch (message.type) {
      case "patch":
        this.applyPatch(message.html!);
        break;
      case "redirect":
        window.location.href = message.url!;
        break;
      case "error":
        console.error("[Loom] Server error:", message.error);
        break;
      default:
        console.warn("[Loom] Unknown message type:", message.type);
    }
  }

  /**
   * Replacing innerHTML wholesale would destroy input state,
   * focus, scroll position, and trigger unnecessary reflows.
   * morphdom diffs the old and new DOM trees and applies only the
   * minimal set of mutations, preserving unaffected nodes. The
   * getNodeKey callback uses data-l-* attributes so morphdom can
   * track elements across re-renders even when their position in
   * the tree changes.
   *
   * Event listeners must be re-attached after patching because
   * morphdom may insert new elements that weren't present in the
   * previous render.
   */
  private applyPatch(html: string): void {
    const saved = saveFocus();

    const temp = document.createElement("div");
    temp.innerHTML = html;

    morphdom(this.container, temp, {
      childrenOnly: true,
      getNodeKey,
      onBeforeElUpdated: (fromEl, toEl) => {
        if (fromEl === saved.element && saved.isInput) {
          (toEl as HTMLInputElement).value = (fromEl as HTMLInputElement).value;
        }
        return true;
      },
    });

    this.attachEventListeners();
    restoreFocus(this.container, saved);
  }

  /**
   * Live templates declare event bindings declaratively via
   * data-l-* attributes in the HTML rather than imperative JS.
   * This method bridges that gap by scanning the container for
   * all event-bearing attributes and wiring up native DOM
   * listeners that forward to the WebSocket.
   */
  private attachEventListeners(): void {
    const elements = this.container.querySelectorAll(
      "[data-l-click], [data-l-input], [data-l-change], [data-l-submit], [data-l-keydown], [data-l-keyup], [data-l-focus], [data-l-blur]",
    );

    elements.forEach((el) => this.attachElementListeners(el as HTMLElement));
  }

  /**
   * The _loomAttached flag prevents duplicate listeners when
   * attachEventListeners runs after every patch. Without this
   * guard, each morphdom cycle would stack another set of
   * handlers on elements that survived the diff, causing events
   * to fire multiple times.
   */
  private attachElementListeners(
    element: HTMLElement & { _loomAttached?: boolean },
  ): void {
    if (element._loomAttached) return;
    element._loomAttached = true;

    const modifiers = parseModifiers(element);

    EVENT_TYPES.forEach((eventType) => {
      const handlerId =
        element.dataset[
          `l${eventType.charAt(0).toUpperCase() + eventType.slice(1)}`
        ];
      if (!handlerId) return;

      element.addEventListener(eventType, (e) => {
        this.handleEvent(e, eventType, handlerId, modifiers);
      });
    });
  }

  /**
   * Central dispatch point for all DOM events captured by live
   * bindings. Applying modifiers, collecting special variables,
   * and routing through debounce in one place keeps the per-event
   * -type listener code minimal — each listener only needs to
   * call this with its handler ID.
   */
  private handleEvent(
    e: Event,
    eventType: string,
    handlerId: string,
    modifiers: Modifiers,
  ): void {
    if (modifiers.prevent) e.preventDefault();
    if (modifiers.stop) e.stopPropagation();

    const payload: EventPayload = {
      handler: handlerId,
      event: eventType,
      special_vars: collectSpecialVars(e),
    };

    if (modifiers.shouldDebounce) {
      this.debouncedSend(e.target as Element, payload, modifiers.debounce);
    } else {
      this.send(payload);
    }
  }

  /**
   * Input events fire on every keystroke, but sending each one
   * would flood the server with redundant updates. Debouncing
   * waits until the user pauses before sending, dramatically
   * reducing WebSocket traffic for text inputs while still
   * delivering the final value promptly.
   *
   * Timers are keyed per-element so debouncing one input doesn't
   * delay events from a different input.
   */
  private debouncedSend(
    element: Element,
    payload: EventPayload,
    delay: number,
  ): void {
    clearTimeout(debounceTimers.get(element));
    debounceTimers.set(
      element,
      setTimeout(() => {
        debounceTimers.delete(element);
        this.send(payload);
      }, delay),
    );
  }

  /**
   * Explicitly closing the socket and resetting state prevents
   * the reconnect logic from firing when a container is
   * intentionally removed — for example, during SPA navigation.
   * Without this, the auto-reconnect would keep trying to revive
   * a connection for a component that no longer exists in the DOM.
   */
  destroy(): void {
    if (this.socket) {
      this.socket.close();
      this.socket = null;
    }

    this.connected = false;
    this.initialized = false;
  }
}
