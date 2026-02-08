/**
 * Loom Live - Client Runtime
 *
 * Provides client-side functionality for Loom live templates.
 * Establishes WebSocket connection, captures events, and applies DOM patches.
 *
 * Usage:
 *   <script src="/loom.js"></script>
 *   <div data-l-live="component_id">
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

  /**
   * Loom Live instance - manages a single live component
   */
  class LoomLive {
    constructor(container) {
      this.container = container;
      this.componentId = container.dataset.lLive;
      this.socket = null;
      this.reconnectAttempts = 0;
      this.connected = false;
      this.pendingEvents = [];

      this.connect();
      this.attachEventListeners();
    }

    /**
     * Establish WebSocket connection
     */
    connect() {
      const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
      const wsUrl = `${protocol}//${window.location.host}${CONFIG.wsPath}?component=${this.componentId}`;

      this.socket = new WebSocket(wsUrl);

      this.socket.onopen = () => {
        console.log("[Loom] Connected:", this.componentId);
        this.connected = true;
        this.reconnectAttempts = 0;

        // Send any pending events
        while (this.pendingEvents.length > 0) {
          const event = this.pendingEvents.shift();
          this.send(event);
        }
      };

      this.socket.onmessage = (event) => {
        this.handleMessage(JSON.parse(event.data));
      };

      this.socket.onclose = () => {
        console.log("[Loom] Disconnected:", this.componentId);
        this.connected = false;
        this.attemptReconnect();
      };

      this.socket.onerror = (error) => {
        console.error("[Loom] WebSocket error:", error);
      };
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
      if (this.connected && this.socket.readyState === WebSocket.OPEN) {
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
     * Apply HTML patch to the container
     * TODO: Use morphdom for better DOM diffing
     */
    applyPatch(html) {
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
      // For now, check for common modifier data attributes
      // In the future, these could be encoded in the handler ID or separate attributes
      const modifiers = {
        prevent: element.dataset.lPrevent === "true",
        stop: element.dataset.lStop === "true",
        immediate: element.dataset.lImmediate === "true",
        debounce: parseInt(element.dataset.lDebounce) || CONFIG.defaultDebounce,
      };

      // Input events are debounced by default unless immediate
      if (eventType === "input" && !modifiers.immediate) {
        modifiers.shouldDebounce = true;
      }

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
        type: "event",
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

  // Auto-initialize on DOM ready
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }

  // Expose API globally
  window.Loom = {
    init: init,
    reinit: reinit,
    LoomLive: LoomLive,
    CONFIG: CONFIG,
  };
})();
