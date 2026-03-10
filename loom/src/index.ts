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

import { CONFIG } from "~/config";
import { LoomSocket } from "~/live/loom-socket";
import { LoomLive } from "~/live/loom-live";
import { LoomNav } from "~/live/loom-nav";

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
const initLiveComponents = () => {
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
 * Live components hold server-side actor state that must be
 * cleaned up when the page changes. LoomNav calls this before
 * swapping the DOM so each instance can send its "leave"
 * message and release its server actor. Clearing the
 * _loomInstance flag on containers ensures that if the same
 * element survives a partial DOM swap, it will be re-initialized
 * fresh rather than treated as already attached.
 */
const destroyLiveComponents = () => {
  activeLiveInstances.forEach((instance) => instance.destroy());
  activeLiveInstances = [];

  document
    .querySelectorAll<
      HTMLElement & { _loomInstance?: LoomLive }
    >("[data-l-live]")
    .forEach((container) => {
      delete container._loomInstance;
    });
};

const nav = new LoomNav(destroyLiveComponents, initLiveComponents);
nav.enable();

initLiveComponents();

/**
 * Exposing Loom on the window object lets server-rendered
 * script tags call Loom.reinit() after dynamic content
 * insertion and gives debugging tools direct access to
 * internals without requiring a module bundler. The socket and
 * nav getters provide live references to the shared instances,
 * and navigate() offers a programmatic API for triggering
 * client-side navigation from custom scripts.
 */
declare global {
  interface Window {
    Loom: {
      init: typeof initLiveComponents;
      reinit: typeof initLiveComponents;
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
  init: initLiveComponents,
  reinit: initLiveComponents,
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
