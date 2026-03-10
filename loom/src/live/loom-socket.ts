import { CONFIG } from "~/config";
import type { ClientMessage, ServerMessage } from "~/types";
import { buildWsUrl } from "~/live/utils";

/**
 * Opening a WebSocket per live component would waste connections
 * and complicate server-side routing. A single shared socket
 * multiplexes all components on the page by routing messages via
 * an "id" field. Centralizing reconnect logic here means every
 * component re-joins automatically after one reconnect cycle
 * instead of each managing its own backoff and retry state.
 */
export class LoomSocket {
  private socket: WebSocket | null = null;
  private connected = false;
  private reconnectAttempts = 0;
  private nextId = 0;
  private handlers = new Map<string, (msg: ServerMessage) => void>();
  private reconnectCallbacks: Array<() => void> = [];
  private pendingMessages: ClientMessage[] = [];
  private wsUrlOverride: string | null;

  constructor(wsUrlOverride?: string | null) {
    this.wsUrlOverride = wsUrlOverride ?? null;
    this.connect();
  }

  /**
   * The server needs a stable ID to route messages to the correct
   * component actor. Auto-incrementing from the socket guarantees
   * uniqueness within the session without requiring coordination
   * between LoomLive instances or the server.
   */
  allocateId(): string {
    return `c${this.nextId++}`;
  }

  /**
   * Each component needs to receive only the messages intended
   * for it. Registering a handler keyed by ID lets routeMessage
   * dispatch incoming frames to the correct LoomLive instance
   * without the socket knowing anything about component internals.
   */
  register(id: string, handler: (msg: ServerMessage) => void): void {
    this.handlers.set(id, handler);
  }

  /**
   * Server-side actors hold state and consume resources, so they
   * must be explicitly stopped when a component is removed.
   * Sending the leave message here keeps the cleanup centralized
   * — LoomLive.destroy only needs to call unregister rather than
   * knowing about the wire protocol.
   */
  unregister(id: string): void {
    this.handlers.delete(id);
    this.send({ type: "leave", id });
  }

  /**
   * Components may fire events before the socket is fully open —
   * for example, a user clicking a button during reconnection.
   * Queuing messages and flushing on open ensures no interaction
   * is silently dropped regardless of connection state.
   */
  send(message: ClientMessage): void {
    if (this.connected && this.socket?.readyState === WebSocket.OPEN) {
      this.socket.send(JSON.stringify(message));
    } else {
      this.pendingMessages.push(message);
    }
  }

  /**
   * After a reconnect the server has no memory of any component —
   * all actors were stopped when the socket closed. Components
   * subscribe here so they can re-send their join messages once
   * the new connection opens. The returned unsubscribe function
   * prevents destroyed components from attempting to rejoin after
   * a future reconnect.
   */
  onReconnect(callback: () => void): () => void {
    this.reconnectCallbacks.push(callback);
    return () => {
      const idx = this.reconnectCallbacks.indexOf(callback);
      if (idx !== -1) this.reconnectCallbacks.splice(idx, 1);
    };
  }

  /**
   * Notifying the server about each component before closing lets
   * it stop all actors immediately rather than waiting for a
   * timeout. Nulling onclose prevents the reconnect logic from
   * firing when the socket closes intentionally — without this,
   * destroy would trigger an unwanted reconnect cycle.
   */
  destroy(): void {
    for (const id of this.handlers.keys()) {
      this.send({ type: "leave", id });
    }
    this.handlers.clear();
    this.reconnectCallbacks = [];

    if (this.socket) {
      this.socket.onclose = null;
      this.socket.close();
      this.socket = null;
    }
    this.connected = false;
  }

  /**
   * Wiring all four socket callbacks in one place keeps the full
   * connection lifecycle visible together. The onopen handler
   * flushes pending messages first (so queued events arrive in
   * order), then notifies components to rejoin — this ordering
   * ensures join messages go out before any stale queued events.
   */
  private connect(): void {
    const wsUrl = buildWsUrl(this.wsUrlOverride, window.location);

    this.socket = new WebSocket(wsUrl);

    this.socket.onopen = () => {
      console.log("[Loom] Socket connected");
      this.connected = true;
      this.reconnectAttempts = 0;
      this.flushPending();

      if (this.reconnectCallbacks.length > 0) {
        this.reconnectCallbacks.forEach((cb) => cb());
      }
    };

    this.socket.onmessage = (event) => {
      this.routeMessage(JSON.parse(event.data));
    };

    this.socket.onclose = () => {
      console.log("[Loom] Socket disconnected");
      this.connected = false;
      this.attemptReconnect();
    };

    this.socket.onerror = (error) => {
      console.error("[Loom] Socket error:", error);
    };
  }

  /**
   * Messages queued during disconnection must be sent in order
   * once the socket opens. Draining the queue here rather than
   * in onopen keeps the callback focused on state transitions
   * and makes the flush independently testable.
   */
  private flushPending(): void {
    while (this.pendingMessages.length > 0) {
      const msg = this.pendingMessages.shift()!;
      this.socket!.send(JSON.stringify(msg));
    }
  }

  /**
   * Most messages target a specific component via the "id" field,
   * but redirects are page-global — they affect the entire browser
   * tab regardless of which component triggered them. Handling
   * redirects before the ID lookup avoids requiring a component
   * handler for what is fundamentally a navigation action.
   * Using LoomNav when available keeps redirects within the SPA
   * flow instead of causing a full page reload.
   */
  private routeMessage(msg: ServerMessage): void {
    if (msg.type === "redirect") {
      if (window.Loom?.nav) {
        window.Loom.nav.navigate(msg.url!);
      } else {
        window.location.href = msg.url!;
      }
      return;
    }

    if (msg.id) {
      const handler = this.handlers.get(msg.id);
      if (handler) {
        handler(msg);
      }
    }
  }

  /**
   * Network interruptions and server restarts are expected in
   * production. Exponential backoff avoids hammering the server
   * with rapid reconnect attempts while still recovering quickly
   * from brief hiccups. The attempt cap prevents infinite retries
   * when the server is genuinely down.
   */
  private attemptReconnect(): void {
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
}
