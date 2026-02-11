import { CONFIG } from "@/config";
import type { FocusState, Modifiers, SpecialVars } from "@/types";

/**
 * The WebSocket URL must match the page's protocol and host to
 * work behind proxies and across environments. Accepting
 * location as a parameter instead of reading window.location
 * directly makes this testable without a browser context. The
 * override path lets development setups point at a different
 * port without changing the default production path.
 */
export function buildWsUrl(
  wsUrlOverride: string | null,
  location: { protocol: string; host: string },
): string {
  if (wsUrlOverride && wsUrlOverride.startsWith("ws")) {
    return wsUrlOverride;
  }

  const protocol = location.protocol === "https:" ? "wss:" : "ws:";
  const path = wsUrlOverride || CONFIG.wsPath;
  return `${protocol}//${location.host}${path}`;
}

/**
 * Reading modifier data attributes once at listener attach time
 * avoids repeated DOM access on every event fire. Returning a
 * typed Modifiers struct lets handleEvent branch on simple
 * boolean checks instead of re-parsing attribute strings in the
 * hot path.
 */
export function parseModifiers(element: HTMLElement): Modifiers {
  return {
    prevent: element.dataset.lPrevent === "true",
    stop: element.dataset.lStop === "true",
    shouldDebounce: element.dataset.lDebounce !== undefined,
    debounce:
      parseInt(element.dataset.lDebounce || "") || CONFIG.defaultDebounce,
  };
}

/**
 * Server-side handlers need browser-only state to update props
 * — the current input value, checkbox state, or pressed key.
 * Extracting these from the DOM event here means the server can
 * process the event in a single message without a round-trip to
 * read form state. Only the variables relevant to the event
 * type are populated, keeping the payload minimal.
 */
export function collectSpecialVars(e: Event): SpecialVars {
  const vars: SpecialVars = {};
  const target = e.target as HTMLInputElement;

  if (target.value !== undefined) {
    vars.value = target.value;
  }
  if (target.type === "checkbox" || target.type === "radio") {
    vars.checked = target.checked;
  }
  if ((e as KeyboardEvent).key !== undefined) {
    vars.key = (e as KeyboardEvent).key;
  }

  return vars;
}

/**
 * morphdom replaces DOM nodes during patching, which destroys
 * the browser's active focus and cursor position. Capturing
 * this state before each patch lets restoreFocus re-establish
 * it afterward, so the user can keep typing without
 * interruption. The handlerId uses data-l-* attributes as a
 * stable key to re-find the "same" input after its DOM node is
 * replaced.
 */
export function saveFocus(): FocusState {
  const el = document.activeElement;
  const input = el as HTMLInputElement | null;
  const isInput = !!el && (el.tagName === "INPUT" || el.tagName === "TEXTAREA");
  return {
    element: el,
    isInput,
    selectionStart: input?.selectionStart ?? null,
    selectionEnd: input?.selectionEnd ?? null,
    handlerId:
      (el as HTMLElement)?.dataset?.lInput ||
      (el as HTMLElement)?.dataset?.lChange ||
      el?.id,
  };
}

/**
 * After morphdom patches the DOM, the previously focused input
 * exists as a new element. This re-finds it by handler ID,
 * restores focus, and resets the cursor so the user's typing
 * position is preserved. The try/catch guards against input
 * types like date or color that don't support setSelectionRange.
 */
export function restoreFocus(container: HTMLElement, saved: FocusState): void {
  if (!saved.isInput || !saved.handlerId) return;

  const el = container.querySelector(
    `[data-l-input="${saved.handlerId}"], [data-l-change="${saved.handlerId}"], #${saved.handlerId}`,
  ) as HTMLInputElement | null;
  if (!el) return;

  el.focus();
  if (
    typeof saved.selectionStart === "number" &&
    typeof saved.selectionEnd === "number"
  ) {
    try {
      el.setSelectionRange(saved.selectionStart, saved.selectionEnd);
    } catch {
      // Not all elements support setSelectionRange
    }
  }
}

/**
 * morphdom needs stable keys to track elements across
 * re-renders — without them, it can't tell whether an element
 * moved or was replaced, leading to unnecessary DOM mutations
 * and lost input state. Using data-l-* handler attributes as
 * keys aligns identity with the live binding system so event-
 * bearing elements are always matched correctly across patches.
 */
export function getNodeKey(node: Node): string | null {
  if (node.nodeType !== 1) return null;
  const el = node as HTMLElement;
  return (
    el.dataset?.lInput ||
    el.dataset?.lClick ||
    el.dataset?.lChange ||
    el.dataset?.lSubmit ||
    el.id ||
    null
  );
}
