/**
 * The WebSocket protocol uses a discriminated union on "type"
 * so the client can route each message to the correct handler
 * without inspecting the payload. Optional fields keep the
 * interface flat — each message type only populates the fields
 * it needs, avoiding wrapper objects for what are simple one-
 * payload messages.
 */
export interface ServerMessage {
  type: "trees" | "patch" | "redirect" | "error";
  s?: any[];
  d?: any;
  html?: string;
  url?: string;
  error?: string;
}

/**
 * Matches the server-side ClientEvent structure so the JSON
 * serialized by the client deserializes directly into the Gleam
 * type without transformation. Keeping the shape identical on
 * both sides avoids a translation layer and makes the protocol
 * self-documenting.
 */
export interface EventPayload {
  handler: string;
  event: string;
  special_vars: SpecialVars;
}

/**
 * Server-side handlers need browser-only state — input values,
 * checkbox state, pressed keys — to update props without a
 * round-trip. All fields are optional because each event type
 * only provides the variables relevant to it: clicks have none,
 * inputs have value, checkboxes have checked, and keyboard
 * events have key.
 */
export interface SpecialVars {
  value?: string;
  checked?: boolean;
  key?: string;
}

/**
 * Event modifiers are parsed once from data attributes when
 * listeners attach, then reused on every event fire. Typing
 * them as a struct avoids re-reading the DOM on each event and
 * makes the modifier logic in handleEvent a simple property
 * check rather than attribute parsing.
 */
export interface Modifiers {
  prevent: boolean;
  stop: boolean;
  shouldDebounce: boolean;
  debounce: number;
}

/**
 * morphdom replaces DOM nodes during patching, destroying the
 * browser's focus and cursor position. Capturing this state
 * before a patch and restoring it after lets the user keep
 * typing uninterrupted. The handlerId provides a stable lookup
 * key to re-find the "same" input in the post-patch DOM since
 * the element reference itself may no longer exist.
 */
export interface FocusState {
  element: Element | null;
  isInput: boolean;
  selectionStart: number | null;
  selectionEnd: number | null;
  handlerId: string | undefined;
}
