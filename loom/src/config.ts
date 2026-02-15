/**
 * Centralizing tunables here lets the runtime behave
 * consistently across all LoomLive instances and gives
 * developers a single place to adjust reconnect timing or
 * debounce thresholds without hunting through class internals.
 * Exposing CONFIG on window.Loom also allows runtime overrides
 * for debugging and development.
 */
export const CONFIG = {
  wsPath: "/loom/ws",
  reconnectInterval: 1000,
  maxReconnectAttempts: 10,
  defaultDebounce: 150,
  navPrefetchDelay: 65,
  navCacheTTL: 30_000,
  navCacheMaxEntries: 20,
};

/**
 * This list defines which DOM events LoomLive will scan for via
 * data-l-* attributes. Keeping it as a const tuple rather than
 * scattering string literals through attachEventListeners
 * ensures the set of supported events is declared once and
 * stays in sync between listener attachment and type
 * definitions.
 */
export const EVENT_TYPES = [
  "click",
  "input",
  "change",
  "submit",
  "keydown",
  "keyup",
  "focus",
  "blur",
] as const;
