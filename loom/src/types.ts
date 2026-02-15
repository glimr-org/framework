/**
 * The WebSocket protocol uses a discriminated union on "type"
 * so the client can route each message to the correct handler
 * without inspecting the payload. The "id" field identifies
 * which multiplexed component the message belongs to (absent
 * for page-global messages like redirect).
 */
export interface ServerMessage {
  type: "trees" | "patch" | "redirect" | "error";
  id?: string;
  s?: any[];
  d?: any;
  html?: string;
  url?: string;
  error?: string;
}

/**
 * Sent by the client to register a new live component on the
 * multiplexed WebSocket connection.
 */
export interface JoinPayload {
  type: "join";
  id: string;
  module: string;
  token: string;
}

/**
 * Matches the server-side ClientEvent structure so the JSON
 * serialized by the client deserializes directly into the Gleam
 * type without transformation. The type and id fields route the
 * event to the correct server-side actor.
 */
export interface EventPayload {
  type: "event";
  id: string;
  handler: string;
  event: string;
  special_vars: SpecialVars;
}

/**
 * Sent by the client when a live component is destroyed (e.g.
 * SPA navigation) to tell the server to stop the actor.
 */
export interface LeavePayload {
  type: "leave";
  id: string;
}

/**
 * Union of all client-to-server message types.
 */
export type ClientMessage = JoinPayload | EventPayload | LeavePayload;

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
 * Cached page data from a prefetch or navigation fetch. Stores
 * the parsed body HTML, document title, and head elements so the
 * DOM can be swapped without re-fetching.
 */
export interface NavCacheEntry {
  html: string;
  title: string;
  headMeta: string[];
  headLinks: string[];
  headStyles: string[];
  resolvedUrl?: string;
  timestamp: number;
}

/**
 * Stored in history.state so popstate can identify Loom-managed
 * entries and restore the correct scroll position.
 */
export interface NavHistoryState {
  loomNavId: string;
  url: string;
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
