/**
 * Loom Live - Client Runtime
 *
 * Provides client-side functionality for Loom live templates.
 * Establishes WebSocket connection, captures events, and applies DOM patches.
 *
 * Usage:
 *   <script src="/loom.js"></script>
 *   <div data-l-live="compiled/loom/counter" data-l-props='{"count":0}'>
 *     <!-- live template content -->
 *   </div>
 */

(function () {
  "use strict";

  // Configuration
  const CONFIG = {
    wsPath: "/loom/ws",
    reconnectInterval: 1000,
    maxReconnectAttempts: 10,
    defaultDebounce: 150,
    morphdomCdn: "https://unpkg.com/morphdom@2.7.4/dist/morphdom-umd.min.js",
  };

  // Event types that support special variables
  const SPECIAL_VAR_EVENTS = {
    input: ["value"],
    change: ["value", "checked"],
    keydown: ["key"],
    keyup: ["key"],
    keypress: ["key"],
  };

  // Debounce timers per element
  const debounceTimers = new WeakMap();

  // Morphdom reference (loaded dynamically)
  let morphdom = null;

  /**
   * Load morphdom from CDN
   */
  function loadMorphdom() {
    return new Promise((resolve, reject) => {
      // Check if already loaded
      if (window.morphdom) {
        morphdom = window.morphdom;
        resolve(morphdom);
        return;
      }

      const script = document.createElement("script");
      script.src = CONFIG.morphdomCdn;
      script.onload = () => {
        morphdom = window.morphdom;
        console.log("[Loom] Morphdom loaded");
        resolve(morphdom);
      };
      script.onerror = () => {
        console.error("[Loom] Failed to load morphdom, falling back to innerHTML");
        reject(new Error("Failed to load morphdom"));
      };
      document.head.appendChild(script);
    });
  }

  /**
   * Loom Live instance - manages a single live component
   */
  class LoomLive {
    constructor(container) {
      this.container = container;
      this.module = container.dataset.lLive;
      this.initialProps = container.dataset.lProps || "{}";
      this.wsUrlOverride = container.dataset.lWs || null;
      this.socket = null;
      this.reconnectAttempts = 0;
      this.connected = false;
      this.initialized = false;
      this.pendingEvents = [];

      this.connect();
      this.attachEventListeners();
    }

    /**
     * Establish WebSocket connection
     */
    connect() {
      let wsUrl;
      if (this.wsUrlOverride && this.wsUrlOverride.startsWith("ws")) {
        // Explicit WebSocket URL provided (dev mode)
        wsUrl = this.wsUrlOverride;
      } else {
        // Relative path - construct from current host
        const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
        const path = this.wsUrlOverride || CONFIG.wsPath;
        wsUrl = `${protocol}//${window.location.host}${path}`;
      }

      this.socket = new WebSocket(wsUrl);

      this.socket.onopen = () => {
        console.log("[Loom] Connected:", this.module);
        this.connected = true;
        this.reconnectAttempts = 0;

        // Send init message with module and props
        this.sendInit();
      };

      this.socket.onmessage = (event) => {
        this.handleMessage(JSON.parse(event.data));
      };

      this.socket.onclose = () => {
        console.log("[Loom] Disconnected:", this.module);
        this.connected = false;
        this.initialized = false;
        this.attemptReconnect();
      };

      this.socket.onerror = (error) => {
        console.error("[Loom] WebSocket error:", error);
      };
    }

    /**
     * Send initialization message with module and props
     */
    sendInit() {
      const initMessage = {
        type: "init",
        module: this.module,
        props: this.initialProps,
      };

      this.socket.send(JSON.stringify(initMessage));
      this.initialized = true;

      // Send any pending events
      while (this.pendingEvents.length > 0) {
        const event = this.pendingEvents.shift();
        this.send(event);
      }
    }

    /**
     * Attempt to reconnect after disconnection
     */
    attemptReconnect() {
      if (this.reconnectAttempts >= CONFIG.maxReconnectAttempts) {
        console.error("[Loom] Max reconnect attempts reached");
        return;
      }

      this.reconnectAttempts++;
      const delay =
        CONFIG.reconnectInterval * Math.pow(2, this.reconnectAttempts - 1);

      console.log(
        `[Loom] Reconnecting in ${delay}ms (attempt ${this.reconnectAttempts})`,
      );

      setTimeout(() => {
        if (!this.connected) {
          this.connect();
        }
      }, delay);
    }

    /**
     * Send event to server
     */
    send(data) {
      if (
        this.connected &&
        this.initialized &&
        this.socket.readyState === WebSocket.OPEN
      ) {
        this.socket.send(JSON.stringify(data));
      } else {
        // Queue event for when connection is restored
        this.pendingEvents.push(data);
      }
    }

    /**
     * Handle incoming message from server
     */
    handleMessage(message) {
      switch (message.type) {
        case "patch":
          this.applyPatch(message.html);
          break;

        case "redirect":
          window.location.href = message.url;
          break;

        case "error":
          console.error("[Loom] Server error:", message.error);
          break;

        default:
          console.warn("[Loom] Unknown message type:", message.type);
      }
    }

    /**
     * Apply HTML patch to the container using morphdom for intelligent DOM diffing.
     * Preserves focus, selection, and scroll position automatically.
     */
    applyPatch(html) {
      // Save focus state before patching
      const activeElement = document.activeElement;
      const isInputFocused = activeElement &&
        (activeElement.tagName === "INPUT" || activeElement.tagName === "TEXTAREA");
      const selectionStart = activeElement?.selectionStart;
      const selectionEnd = activeElement?.selectionEnd;
      const activeHandlerId = activeElement?.dataset?.lInput ||
                              activeElement?.dataset?.lChange ||
                              activeElement?.id;

      if (morphdom) {
        // Create a temporary container to parse the new HTML
        const temp = document.createElement("div");
        temp.innerHTML = html;

        // Use morphdom to diff and patch the DOM
        morphdom(this.container, temp, {
          childrenOnly: true,
          // Use data-l-* attributes as stable keys for element matching
          getNodeKey: (node) => {
            if (node.nodeType !== 1) return null; // Not an element
            // Use data-l-input/click/etc as unique key
            return node.dataset?.lInput ||
                   node.dataset?.lClick ||
                   node.dataset?.lChange ||
                   node.dataset?.lSubmit ||
                   node.id ||
                   null;
          },
          onBeforeElUpdated: (fromEl, toEl) => {
            // Preserve input values for focused elements
            if (fromEl === activeElement && isInputFocused) {
              // Keep the current DOM value, don't let morphdom overwrite it
              toEl.value = fromEl.value;
            }
            return true;
          },
        });

        // Re-attach event listeners to new elements
        this.attachEventListeners();

        // Restore focus after morphdom patch
        if (isInputFocused && activeHandlerId) {
          const newElement = this.container.querySelector(
            `[data-l-input="${activeHandlerId}"], [data-l-change="${activeHandlerId}"], #${activeHandlerId}`
          );
          if (newElement) {
            newElement.focus();
            if (typeof selectionStart === "number" && typeof selectionEnd === "number") {
              try {
                newElement.setSelectionRange(selectionStart, selectionEnd);
              } catch (e) {
                // Not all elements support setSelectionRange
              }
            }
          }
        }
      } else {
        // Fallback to innerHTML if morphdom not loaded
        this.applyPatchFallback(html);
      }
    }

    /**
     * Fallback patch method using innerHTML (less optimal)
     */
    applyPatchFallback(html) {
      // Save focus state
      const activeElement = document.activeElement;
      const activeId = activeElement?.id;
      const selectionStart = activeElement?.selectionStart;
      const selectionEnd = activeElement?.selectionEnd;

      // Apply the patch
      this.container.innerHTML = html;

      // Restore focus
      if (activeId) {
        const element = document.getElementById(activeId);
        if (element) {
          element.focus();
          if (
            typeof selectionStart === "number" &&
            typeof selectionEnd === "number"
          ) {
            try {
              element.setSelectionRange(selectionStart, selectionEnd);
            } catch (e) {
              // Not all elements support setSelectionRange
            }
          }
        }
      }

      // Re-attach event listeners
      this.attachEventListeners();
    }

    /**
     * Attach event listeners to elements with data-l-* attributes
     */
    attachEventListeners() {
      // Find all elements with data-l-* attributes
      const elements = this.container.querySelectorAll(
        "[data-l-click], [data-l-input], [data-l-change], [data-l-submit], [data-l-keydown], [data-l-keyup], [data-l-focus], [data-l-blur]",
      );

      elements.forEach((element) => {
        this.attachElementListeners(element);
      });
    }

    /**
     * Attach listeners to a single element
     */
    attachElementListeners(element) {
      // Skip if already attached
      if (element._loomAttached) return;
      element._loomAttached = true;

      const eventTypes = [
        "click",
        "input",
        "change",
        "submit",
        "keydown",
        "keyup",
        "focus",
        "blur",
      ];

      eventTypes.forEach((eventType) => {
        const handlerId =
          element.dataset[
            `l${eventType.charAt(0).toUpperCase() + eventType.slice(1)}`
          ];
        if (!handlerId) return;

        // Parse modifiers from handler ID or data attributes
        const modifiers = this.parseModifiers(element, eventType);

        element.addEventListener(eventType, (e) => {
          this.handleEvent(e, eventType, handlerId, modifiers);
        });
      });
    }

    /**
     * Parse event modifiers from data attributes
     */
    parseModifiers(element, eventType) {
      // Check for modifier data attributes
      const hasDebounce = element.dataset.lDebounce !== undefined;
      const debounceValue = parseInt(element.dataset.lDebounce) || CONFIG.defaultDebounce;

      const modifiers = {
        prevent: element.dataset.lPrevent === "true",
        stop: element.dataset.lStop === "true",
        // Only debounce if explicitly requested via .debounce modifier
        shouldDebounce: hasDebounce,
        debounce: debounceValue,
      };

      return modifiers;
    }

    /**
     * Handle DOM event
     */
    handleEvent(e, eventType, handlerId, modifiers) {
      // Apply modifiers
      if (modifiers.prevent) {
        e.preventDefault();
      }
      if (modifiers.stop) {
        e.stopPropagation();
      }

      // Collect special variables
      const specialVars = this.collectSpecialVars(e, eventType);

      // Build event payload
      const payload = {
        handler: handlerId,
        event: eventType,
        special_vars: specialVars,
      };

      // Handle debouncing for input events
      if (modifiers.shouldDebounce) {
        this.debouncedSend(e.target, payload, modifiers.debounce);
      } else {
        this.send(payload);
      }
    }

    /**
     * Collect special variables from event
     */
    collectSpecialVars(e, eventType) {
      const vars = {};
      const target = e.target;

      // $value - input value
      if (target.value !== undefined) {
        vars.value = target.value;
      }

      // $checked - checkbox/radio state
      if (target.type === "checkbox" || target.type === "radio") {
        vars.checked = target.checked;
      }

      // $key - key pressed
      if (e.key !== undefined) {
        vars.key = e.key;
      }

      return vars;
    }

    /**
     * Debounced send - delays sending until user stops typing
     */
    debouncedSend(element, payload, delay) {
      // Clear existing timer for this element
      const existingTimer = debounceTimers.get(element);
      if (existingTimer) {
        clearTimeout(existingTimer);
      }

      // Set new timer
      const timer = setTimeout(() => {
        debounceTimers.delete(element);
        this.send(payload);
      }, delay);

      debounceTimers.set(element, timer);
    }

    /**
     * Disconnect and cleanup
     */
    destroy() {
      if (this.socket) {
        this.socket.close();
        this.socket = null;
      }
      this.connected = false;
      this.initialized = false;
    }
  }

  /**
   * Initialize all live components on the page
   */
  function init() {
    const containers = document.querySelectorAll("[data-l-live]");

    containers.forEach((container) => {
      // Skip if already initialized
      if (container._loomInstance) return;

      container._loomInstance = new LoomLive(container);
    });

    console.log(`[Loom] Initialized ${containers.length} live component(s)`);
  }

  /**
   * Reinitialize after dynamic content changes
   */
  function reinit() {
    init();
  }

  // Load morphdom first, then initialize
  loadMorphdom()
    .catch(() => {
      // Continue without morphdom (will use fallback)
    })
    .finally(() => {
      // Auto-initialize on DOM ready
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", init);
      } else {
        init();
      }
    });

  // Expose API globally
  window.Loom = {
    init: init,
    reinit: reinit,
    LoomLive: LoomLive,
    CONFIG: CONFIG,
  };
})();
