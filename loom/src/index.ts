/**
 * Loom Live — Client Runtime
 *
 * Live templates render initial HTML on the server, but
 * subsequent interactions need a persistent channel to push UI
 * updates without full page reloads. This entry point
 * bootstraps a single shared WebSocket connection and wires
 * each live container to it via LoomLive instances.
 *
 * Auto-initializing on DOMContentLoaded and exposing
 * window.Loom lets the runtime work both as a bundled script
 * tag and as an imperatively-controlled library for SPA-style
 * navigation that adds live containers after the initial page
 * load.
 */

import { CONFIG } from "@/config";
import { LoomSocket } from "@/live/loom-socket";
import { LoomLive } from "@/live/loom-live";

let sharedSocket: LoomSocket | null = null;

/**
 * Multiple live containers can exist on the same page, each
 * backed by a different template module. A single LoomSocket
 * is created lazily on the first init that finds containers,
 * and shared across all LoomLive instances.
 *
 * The _loomInstance guard prevents double-initialization when
 * init is called again after SPA navigation — the existing
 * instance keeps its state intact instead of being replaced.
 */
const init = () => {
  const containers = document.querySelectorAll<HTMLElement>("[data-l-live]");

  if (containers.length > 0 && !sharedSocket) {
    const wsUrlOverride = containers[0].dataset.lWs || null;
    sharedSocket = new LoomSocket(wsUrlOverride);
  }

  containers.forEach(
    (container: HTMLElement & { _loomInstance?: LoomLive }) => {
      if (container._loomInstance) {
        return;
      }

      container._loomInstance = new LoomLive(container, sharedSocket!);
    },
  );

  console.log(`[Loom] Initialized ${containers.length} live component(s)`);
};

init();

/**
 * Exposing Loom on the window object serves two purposes: it
 * lets server-rendered script tags call Loom.reinit() after
 * dynamic content insertion, and it gives debugging tools
 * direct access to the CONFIG, LoomSocket, and LoomLive class
 * without requiring a module bundler. The reinit alias signals
 * intent — callers re-scanning for new containers — even though
 * the implementation is identical to init.
 */
declare global {
  interface Window {
    Loom: {
      init: typeof init;
      reinit: typeof init;
      LoomLive: typeof LoomLive;
      LoomSocket: typeof LoomSocket;
      CONFIG: typeof CONFIG;
      socket: LoomSocket | null;
    };
  }
}

window.Loom = {
  init,
  reinit: init,
  LoomLive,
  LoomSocket,
  CONFIG,
  get socket() {
    return sharedSocket;
  },
};
