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
import { LoomNav } from "@/live/loom-nav";

let sharedSocket: LoomSocket | null = null;
let activeLiveInstances: LoomLive[] = [];

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

      const instance = new LoomLive(container, sharedSocket!);
      container._loomInstance = instance;
      activeLiveInstances.push(instance);
    },
  );

  console.log(`[Loom] Initialized ${containers.length} live component(s)`);
};

/**
 * Destroys all active LoomLive instances and clears the
 * tracking array. Called by LoomNav before a DOM swap so each
 * component sends its "leave" message to the server.
 */
const destroyLiveComponents = () => {
  activeLiveInstances.forEach((instance) => instance.destroy());
  activeLiveInstances = [];

  document
    .querySelectorAll<HTMLElement & { _loomInstance?: LoomLive }>(
      "[data-l-live]",
    )
    .forEach((container) => {
      delete container._loomInstance;
    });
};

/**
 * Scans for new [data-l-live] containers and initializes them.
 * Called by LoomNav after a DOM swap to bring up live components
 * on the new page.
 */
const initLiveComponents = () => {
  init();
};

const nav = new LoomNav(destroyLiveComponents, initLiveComponents);
nav.enable();

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
      LoomNav: typeof LoomNav;
      CONFIG: typeof CONFIG;
      socket: LoomSocket | null;
      nav: LoomNav | null;
      navigate: (url: string) => Promise<void>;
    };
  }
}

window.Loom = {
  init,
  reinit: init,
  LoomLive,
  LoomSocket,
  LoomNav,
  CONFIG,
  get socket() {
    return sharedSocket;
  },
  get nav() {
    return nav;
  },
  navigate: (url: string) => nav.navigate(url),
};
