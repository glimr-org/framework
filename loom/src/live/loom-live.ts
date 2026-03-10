import morphdom from "morphdom";
import { EVENT_TYPES } from "~/config";
import type { EventPayload, Modifiers, ServerMessage } from "~/types";
import type { LoomSocket } from "~/live/loom-socket";
import {
  collectSpecialVars,
  getNodeKey,
  parseModifiers,
  restoreFocus,
  saveFocus,
} from "~/live/utils";
import { applyDiff, reconstruct } from "~/live/tree";

/**
 * Debounce timers are stored per-element in a WeakMap so that
 * removed DOM elements automatically release their timer
 * references through garbage collection, avoiding memory leaks
 * across repeated morphdom patches.
 */
const debounceTimers = new WeakMap<Element, ReturnType<typeof setTimeout>>();

/**
 * Loading indicators modify element state (disabled, text,
 * child visibility) during a server round-trip. Capturing the
 * original state before modification lets clearLoadingStates
 * reverse the changes exactly, even for nested remote targets,
 * without re-reading the DOM or guessing what the pre-loading
 * state was.
 */
interface ChildToggleState {
  shownIndicators: HTMLElement[];
  hiddenSiblings: { el: HTMLElement; originalDisplay: string }[];
}

interface RemoteLoadingState extends ChildToggleState {
  element: HTMLElement;
}

interface LoadingState extends ChildToggleState {
  originalText: string | null;
  wasDisabled: boolean;
  remoteTargets: RemoteLoadingState[];
}

/**
 * Each live container on the page needs its own event wiring,
 * DOM patching, and loading state, but opening a WebSocket per
 * component would waste connections and complicate server-side
 * routing. Sharing a single LoomSocket and multiplexing via
 * component IDs gives each instance isolation while keeping the
 * connection count to one. The reconnect subscription ensures
 * the component re-joins the server after network disruptions
 * without manual intervention.
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
  private loadingElements = new Map<HTMLElement, LoadingState>();
  private unsubReconnect: () => void;

  constructor(container: HTMLElement, loomSocket: LoomSocket) {
    this.container = container;
    this.module = container.dataset.lLive!;
    this.token = container.dataset.lToken!;
    this.loomSocket = loomSocket;
    this.id = loomSocket.allocateId();

    loomSocket.register(this.id, (msg) => this.handleMessage(msg));
    this.unsubReconnect = loomSocket.onReconnect(() => this.rejoin());

    this.sendJoin();
    this.attachEventListeners();
    this.hideLoadingIndicators();
  }

  /**
   * The server-side actor for this component can't process events
   * until it knows which template module to run and has validated
   * the token. Sending the join first and then flushing queued
   * events ensures no user interaction is lost during the brief
   * startup window.
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
   * After a reconnect the server has no memory of this component
   * — its actor was stopped when the socket closed. Re-joining
   * spawns a fresh actor, and clearing statics/dynamics ensures
   * the next "trees" message is treated as initial state rather
   * than a diff against stale data.
   */
  private rejoin(): void {
    this.initialized = false;
    this.statics = null;
    this.dynamics = null;
    this.sendJoin();
  }

  /**
   * Events can fire before the join handshake completes — for
   * example, a user clicking a button while the socket is still
   * opening. Queuing these events and flushing them after join
   * ensures no interaction is silently dropped.
   */
  private sendEvent(data: EventPayload): void {
    if (this.initialized) {
      this.loomSocket.send(data);
    } else {
      this.pendingEvents.push(data);
    }
  }

  /**
   * The server sends trees (initial state), patches (diffs
   * against the current dynamics), and errors. The "trees"
   * message stores statics and dynamics without touching the DOM
   * because the server-rendered HTML is already correct on first
   * load. Subsequent "patch" messages apply diffs to the stored
   * dynamics and reconstruct full HTML from the static/dynamic
   * tree before morphing. Clearing loading state on every
   * response type ensures the UI never gets stuck in a loading
   * state.
   */
  private handleMessage(message: ServerMessage): void {
    switch (message.type) {
      case "trees":
        // Store statics + dynamics from initial tree.
        // Don't apply to DOM — server-rendered HTML is already there.
        this.statics = message.s!;
        this.dynamics = message.d!;
        this.clearLoadingStates();
        break;
      case "patch":
        this.clearLoadingStates();
        if (this.statics && this.dynamics) {
          // Apply diff to dynamics, reconstruct, and morph
          applyDiff(this.dynamics, message.d);
          const html = reconstruct(this.statics, this.dynamics);
          this.applyPatch(html);
        }
        break;
      case "error":
        this.clearLoadingStates();
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
   * minimal set of mutations, preserving unaffected nodes.
   *
   * The onBeforeElUpdated check skips nested live containers so a
   * parent patch doesn't overwrite a child component's
   * independently-managed DOM. Event listeners and loading
   * indicator visibility are re-established after every patch
   * because morphdom may insert new elements that weren't in the
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
        // Don't let parent patches overwrite nested live components
        if (fromEl !== this.container && fromEl.hasAttribute("data-l-live")) {
          return false;
        }
        if (fromEl === saved.element && saved.isInput) {
          (toEl as HTMLInputElement).value = (fromEl as HTMLInputElement).value;
        }
        return true;
      },
    });

    this.attachEventListeners();
    this.hideLoadingIndicators();
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

    elements.forEach((el) => {
      // Skip elements owned by a nested live component
      if (el.closest("[data-l-live]") !== this.container) return;
      this.attachElementListeners(el as HTMLElement);
    });
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
        this.handleEvent(e, eventType, handlerId, modifiers, element);
      });
    });
  }

  /**
   * Central dispatch point for all DOM events captured by live
   * bindings. Applying modifiers, collecting special variables,
   * and routing through debounce in one place keeps the per-event
   * -type listener code minimal — each listener only needs to
   * call this with its handler ID.
   *
   * For click and submit events, loading state is applied to the
   * element: an "l-loading" CSS class is added, the element is
   * auto-disabled (unless it has l-no-disable), and the text is
   * swapped if l-loading-text is present. Loading state is cleared
   * when the server responds with a patch.
   */
  private handleEvent(
    e: Event,
    eventType: string,
    handlerId: string,
    modifiers: Modifiers,
    element: HTMLElement,
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
      this.debouncedSend(element, payload, modifiers.debounce);
    } else {
      // Apply loading state for discrete actions (click, submit)
      if (eventType === "click" || eventType === "submit") {
        this.applyLoadingState(element);
      }
      this.sendEvent(payload);
    }
  }

  /**
   * Users need visual feedback that something is happening during
   * the server round-trip — without it, clicks feel unresponsive.
   * This applies multiple feedback signals (CSS class, disabled
   * state, text swap, indicator children) so template authors can
   * style loading however they want using CSS alone. Remote
   * targets (linked by ID) let a button trigger loading
   * indicators on a different part of the page, like a table
   * that's being refreshed. All original state is captured so
   * clearLoadingStates can reverse everything exactly.
   */
  private applyLoadingState(element: HTMLElement): void {
    const wasDisabled = element.hasAttribute("disabled");

    // Add loading CSS class
    element.classList.add("l-loading");

    // Auto-disable unless opted out
    if (!element.hasAttribute("l-no-disable")) {
      element.setAttribute("disabled", "");
    }

    // Swap text if l-loading-text is specified
    const loadingText = element.getAttribute("l-loading-text");
    let originalText: string | null = null;
    if (loadingText !== null) {
      originalText = element.textContent;
      element.textContent = loadingText;
    }

    // Toggle l-loading indicator children (only when not using l-loading-text)
    const { shownIndicators, hiddenSiblings } =
      loadingText === null
        ? this.toggleLoadingChildren(element)
        : { shownIndicators: [], hiddenSiblings: [] };

    // Activate remote loading targets linked by id
    const remoteTargets: RemoteLoadingState[] = [];
    const elementId = element.id;
    if (elementId) {
      this.container
        .querySelectorAll(
          `[l-loading="${elementId}"], [data-l-loading="${elementId}"]`,
        )
        .forEach((remote) => {
          const remoteEl = remote as HTMLElement;
          remoteEl.classList.add("l-loading");
          const toggle = this.toggleLoadingChildren(remoteEl);
          remoteTargets.push({ element: remoteEl, ...toggle });
        });
    }

    this.loadingElements.set(element, {
      originalText,
      wasDisabled,
      shownIndicators,
      hiddenSiblings,
      remoteTargets,
    });
  }

  /**
   * Loading indicators are hidden by default and only revealed
   * during a round-trip. Toggling visibility via inline display
   * styles rather than adding/removing DOM nodes avoids layout
   * shifts and keeps the indicator elements stable for CSS
   * transitions. Returning the toggle state lets
   * clearLoadingStates reverse the changes without re-querying
   * the DOM.
   */
  private toggleLoadingChildren(element: HTMLElement): ChildToggleState {
    const shownIndicators: HTMLElement[] = [];
    const hiddenSiblings: { el: HTMLElement; originalDisplay: string }[] = [];

    for (const child of Array.from(element.children) as HTMLElement[]) {
      if (this.isLoadingIndicator(child)) {
        child.style.display = "";
        shownIndicators.push(child);
      }
    }

    if (shownIndicators.length > 0) {
      for (const child of Array.from(element.children) as HTMLElement[]) {
        if (!this.isLoadingIndicator(child)) {
          hiddenSiblings.push({
            el: child,
            originalDisplay: child.style.display,
          });
          child.style.display = "none";
        }
      }
    }

    return { shownIndicators, hiddenSiblings };
  }

  /**
   * The l-loading attribute serves double duty: an empty value
   * marks the element as an inline indicator to show/hide, while
   * a non-empty value links it as a remote loading scope.
   * Distinguishing the two prevents remote scopes from being
   * incorrectly hidden during the initial indicator scan.
   */
  private isLoadingIndicator(el: HTMLElement): boolean {
    return (
      el.getAttribute("l-loading") === "" ||
      el.getAttribute("data-l-loading") === ""
    );
  }

  /**
   * Every server response — trees, patch, or error — signals the
   * end of a round-trip, so all loading indicators must be
   * reversed. Using the captured original state ensures elements
   * return to exactly their pre-loading condition, including the
   * correct disabled state and text content, even if multiple
   * elements were loading simultaneously.
   */
  private clearLoadingStates(): void {
    this.loadingElements.forEach((state, element) => {
      element.classList.remove("l-loading");

      // Only remove disabled if we added it
      if (!state.wasDisabled) {
        element.removeAttribute("disabled");
      }

      // Restore original text if it was swapped
      if (state.originalText !== null) {
        element.textContent = state.originalText;
      }

      this.reverseChildToggle(state);

      // Reverse remote targets
      state.remoteTargets.forEach((remote) => {
        remote.element.classList.remove("l-loading");
        this.reverseChildToggle(remote);
      });
    });
    this.loadingElements.clear();
  }

  /**
   * Shared reversal logic for both the triggering element and its
   * remote targets. Re-hiding indicators and restoring sibling
   * display values from the captured state avoids duplicating
   * this logic in clearLoadingStates for each target type.
   */
  private reverseChildToggle(state: ChildToggleState): void {
    state.shownIndicators.forEach((el) => {
      el.style.display = "none";
    });
    state.hiddenSiblings.forEach(({ el, originalDisplay }) => {
      el.style.display = originalDisplay;
    });
  }

  /**
   * Loading indicators must be hidden on init and after every
   * patch so they only appear during an active round-trip.
   * Running this after morphdom catches any new indicator
   * elements introduced by the patch. Only empty-value l-loading
   * attributes are targeted — non-empty values are remote scopes
   * that manage their own visibility through applyLoadingState.
   */
  private hideLoadingIndicators(): void {
    this.container
      .querySelectorAll('[l-loading=""], [data-l-loading=""]')
      .forEach((el) => {
        (el as HTMLElement).style.display = "none";
      });
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
   * Unregistering from the socket sends a "leave" message so the
   * server stops the actor immediately rather than waiting for a
   * timeout. Unsubscribing from reconnect events prevents a
   * destroyed component from attempting to rejoin after a future
   * reconnect.
   */
  destroy(): void {
    this.unsubReconnect();
    this.loomSocket.unregister(this.id);
    this.initialized = false;
  }
}
