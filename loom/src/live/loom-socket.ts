import { CONFIG } from "@/config";
import type { ClientMessage, ServerMessage } from "@/types";
import { buildWsUrl } from "@/live/utils";

/**
 * Shared WebSocket connection manager that multiplexes all live
 * components on a single page over one socket. Each component
 * registers a handler keyed by its auto-assigned ID; incoming
 * messages are routed by the "id" field. Reconnect logic lives
 * here so all components re-join after a single reconnect cycle.
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
   * Returns the next component ID ("c0", "c1", ...). Each
   * LoomLive instance calls this once during construction to
   * get a unique, collision-free identifier for the session.
   */
  allocateId(): string {
    return `c${this.nextId++}`;
  }

  /**
   * Registers a message handler for a component. Messages with
   * a matching "id" field are dispatched to this handler.
   */
  register(id: string, handler: (msg: ServerMessage) => void): void {
    this.handlers.set(id, handler);
  }

  /**
   * Unregisters a component's handler and sends a leave message
   * to the server so the actor is stopped.
   */
  unregister(id: string): void {
    this.handlers.delete(id);
    this.send({ type: "leave", id });
  }

  /**
   * Sends a message through the shared socket. If the socket
   * isn't ready yet, the message is queued and flushed once the
   * connection opens.
   */
  send(message: ClientMessage): void {
    if (this.connected && this.socket?.readyState === WebSocket.OPEN) {
      this.socket.send(JSON.stringify(message));
    } else {
      this.pendingMessages.push(message);
    }
  }

  /**
   * Registers a callback invoked after a successful reconnect.
   * Components use this to re-send their join messages.
   */
  onReconnect(callback: () => void): void {
    this.reconnectCallbacks.push(callback);
  }

  /**
   * Tears down the shared socket. Sends leave messages for all
   * registered components, then closes the connection.
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

  private flushPending(): void {
    while (this.pendingMessages.length > 0) {
      const msg = this.pendingMessages.shift()!;
      this.socket!.send(JSON.stringify(msg));
    }
  }

  /**
   * Routes an incoming server message to the correct component
   * handler by its "id" field. Redirects are page-global and
   * applied immediately without routing.
   */
  private routeMessage(msg: ServerMessage): void {
    if (msg.type === "redirect") {
      window.location.href = msg.url!;
      return;
    }

    if (msg.id) {
      const handler = this.handlers.get(msg.id);
      if (handler) {
        handler(msg);
      }
    }
  }

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
