import { describe, expect, test, beforeEach } from "bun:test";
import { Window } from "happy-dom";
import { LoomSocket } from "@/live/loom-socket";

let window: Window;
let capturedWs: any;

beforeEach(() => {
  window = new Window({ url: "http://localhost:3000" });
  globalThis.window = window as any;
  globalThis.document = window.document as unknown as Document;
  globalThis.location = window.location as any;

  capturedWs = null;
  (globalThis as any).WebSocket = class MockWebSocket {
    static OPEN = 1;
    onopen: any = null;
    onclose: any = null;
    onmessage: any = null;
    onerror: any = null;
    readyState = 0;

    constructor(_url: string) {
      capturedWs = this;
    }

    send(_data: string) {}
    close() {}

    _open() {
      this.readyState = 1;
      this.onopen?.();
    }
  };
});

describe("onReconnect", () => {
  test("returns an unsubscribe function", () => {
    const socket = new LoomSocket();
    const unsub = socket.onReconnect(() => {});
    expect(typeof unsub).toBe("function");
  });

  test("unsubscribed callback is not called on reconnect", () => {
    const socket = new LoomSocket();
    capturedWs._open(); // initial connection

    let called = false;
    const unsub = socket.onReconnect(() => {
      called = true;
    });
    unsub();

    capturedWs._open(); // simulate reconnect
    expect(called).toBe(false);
  });

  test("other callbacks still fire after one unsubscribes", () => {
    const socket = new LoomSocket();
    capturedWs._open();

    const calls: string[] = [];
    socket.onReconnect(() => calls.push("a"));
    const unsub = socket.onReconnect(() => calls.push("b"));
    socket.onReconnect(() => calls.push("c"));

    unsub();

    capturedWs._open();
    expect(calls).toEqual(["a", "c"]);
  });
});
