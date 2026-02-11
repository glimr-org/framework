/**
 * Loom Live — Client Runtime
 *
 * Live templates render initial HTML on the server, but
 * subsequent interactions need a persistent channel to push UI
 * updates without full page reloads. This entry point
 * bootstraps that channel by scanning the DOM for live
 * containers and wiring each one to its own WebSocket-backed
 * LoomLive instance.
 *
 * Auto-initializing on DOMContentLoaded and exposing
 * window.Loom lets the runtime work both as a bundled script
 * tag and as an imperatively-controlled library for SPA-style
 * navigation that adds live containers after the initial page
 * load.
 */

import { CONFIG } from "@/config";
import { LoomLive } from "@/live/loom-live";

/**
 * Multiple live containers can exist on the same page, each
 * backed by a different template module. Scanning for all
 * [data-l-live] elements and instantiating them in one pass
 * keeps initialization centralized rather than requiring each
 * template to boot itself.
 *
 * The _loomInstance guard prevents double-initialization when
 * init is called again after SPA navigation — the existing
 * instance keeps its WebSocket connection and state intact
 * instead of being replaced.
 */
const init = () => {
  const containers = document.querySelectorAll<HTMLElement>("[data-l-live]");

  containers.forEach(
    (container: HTMLElement & { _loomInstance?: LoomLive }) => {
      if (container._loomInstance) {
        return;
      }

      container._loomInstance = new LoomLive(container);
    },
  );

  console.log(`[Loom] Initialized ${containers.length} live component(s)`);
};

init();

/**
 * Exposing Loom on the window object serves two purposes: it
 * lets server-rendered script tags call Loom.reinit() after
 * dynamic content insertion, and it gives debugging tools
 * direct access to the CONFIG and LoomLive class without
 * requiring a module bundler. The reinit alias signals intent —
 * callers re-scanning for new containers — even though the
 * implementation is identical to init.
 */
declare global {
  interface Window {
    Loom: {
      init: typeof init;
      reinit: typeof init;
      LoomLive: typeof LoomLive;
      CONFIG: typeof CONFIG;
    };
  }
}

window.Loom = { init, reinit: init, LoomLive, CONFIG };
