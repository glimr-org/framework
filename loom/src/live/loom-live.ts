import morphdom from "morphdom";
import { EVENT_TYPES } from "@/config";
import type { EventPayload, Modifiers, ServerMessage } from "@/types";
import type { LoomSocket } from "@/live/loom-socket";
import {
  collectSpecialVars,
  getNodeKey,
  parseModifiers,
  restoreFocus,
  saveFocus,
} from "@/live/utils";
import { applyDiff, reconstruct } from "@/live/tree";

/**
 * Debounce timers are stored per-element in a WeakMap so that
 * removed DOM elements automatically release their timer
 * references through garbage collection, avoiding memory leaks
 * across repeated morphdom patches.
 */
const debounceTimers = new WeakMap<Element, ReturnType<typeof setTimeout>>();

/**
 * Each live container on the page gets its own LoomLive
 * instance that manages event wiring and DOM patching, but
 * shares a single WebSocket connection via LoomSocket. The
 * component registers itself with an auto-assigned ID and
 * re-joins automatically after reconnects.
 */
export class LoomLive {
  private container: HTMLElement;
  private module: string;
  private token: string;
  private id: string;
  private loomSocket: LoomSocket;
  private initialized = false;
  private pendingEvents: EventPayload[] = [];
  private statics: any | null = null;
  private dynamics: any[] | null = null;

  constructor(container: HTMLElement, loomSocket: LoomSocket) {
    this.container = container;
    this.module = container.dataset.lLive!;
    this.token = container.dataset.lToken!;
    this.loomSocket = loomSocket;
    this.id = loomSocket.allocateId();

    loomSocket.register(this.id, (msg) => this.handleMessage(msg));
    loomSocket.onReconnect(() => this.rejoin());

    this.sendJoin();
    this.attachEventListeners();
  }

  /**
   * Sends a join message to register this component with the
   * server. Flushes any events that were queued before the
   * join completed.
   */
  private sendJoin(): void {
    this.loomSocket.send({
      type: "join",
      id: this.id,
      module: this.module,
      token: this.token,
    });
    this.initialized = true;

    while (this.pendingEvents.length > 0) {
      this.sendEvent(this.pendingEvents.shift()!);
    }
  }

  /**
   * Called after a reconnect to re-register the component with
   * the server. Resets statics/dynamics so the initial trees
   * message is accepted as fresh state.
   */
  private rejoin(): void {
    this.initialized = false;
    this.statics = null;
    this.dynamics = null;
    this.sendJoin();
  }

  /**
   * Sends an event payload through the shared socket, tagged
   * with this component's id and type. Events fired before the
   * join completes are queued.
   */
  private sendEvent(data: EventPayload): void {
    if (this.initialized) {
      this.loomSocket.send(data);
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
      case "trees":
        // Store statics + dynamics from initial tree.
        // Don't apply to DOM — server-rendered HTML is already there.
        this.statics = message.s!;
        this.dynamics = message.d!;
        break;
      case "patch":
        if (this.statics && this.dynamics) {
          // Apply diff to dynamics, reconstruct, and morph
          applyDiff(this.dynamics, message.d);
          const html = reconstruct(this.statics, this.dynamics);
          this.applyPatch(html);
        }
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
      type: "event",
      id: this.id,
      handler: handlerId,
      event: eventType,
      special_vars: collectSpecialVars(e),
    };

    if (modifiers.shouldDebounce) {
      this.debouncedSend(e.target as Element, payload, modifiers.debounce);
    } else {
      this.sendEvent(payload);
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
        this.sendEvent(payload);
      }, delay),
    );
  }

  /**
   * Unregisters from the shared socket so the server stops the
   * actor. Called when the container is intentionally removed,
   * e.g. during SPA navigation.
   */
  destroy(): void {
    this.loomSocket.unregister(this.id);
    this.initialized = false;
  }
}
