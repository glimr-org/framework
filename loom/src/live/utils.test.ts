import { describe, expect, test, beforeEach } from "bun:test";
import { Window } from "happy-dom";
import {
  buildWsUrl,
  collectSpecialVars,
  getNodeKey,
  parseModifiers,
  restoreFocus,
  saveFocus,
} from "@/live/utils";

let window: Window;
let document: Document;

beforeEach(() => {
  window = new Window();
  document = window.document as unknown as Document;
  globalThis.document = document;
});

describe("buildWsUrl", () => {
  test("uses override when it starts with ws", () => {
    const url = buildWsUrl("ws://custom:8080/ws", {
      protocol: "https:",
      host: "example.com",
    });
    expect(url).toBe("ws://custom:8080/ws");
  });

  test("uses override when it starts with wss", () => {
    const url = buildWsUrl("wss://custom:8080/ws", {
      protocol: "https:",
      host: "example.com",
    });
    expect(url).toBe("wss://custom:8080/ws");
  });

  test("builds ws url from http location", () => {
    const url = buildWsUrl(null, {
      protocol: "http:",
      host: "localhost:3000",
    });
    expect(url).toBe("ws://localhost:3000/loom/ws");
  });

  test("builds wss url from https location", () => {
    const url = buildWsUrl(null, {
      protocol: "https:",
      host: "example.com",
    });
    expect(url).toBe("wss://example.com/loom/ws");
  });

  test("uses override as path when not a full ws url", () => {
    const url = buildWsUrl("/custom/path", {
      protocol: "https:",
      host: "example.com",
    });
    expect(url).toBe("wss://example.com/custom/path");
  });
});

describe("parseModifiers", () => {
  test("returns defaults when no data attributes set", () => {
    const el = document.createElement("button");
    const mods = parseModifiers(el as unknown as HTMLElement);
    expect(mods).toEqual({
      prevent: false,
      stop: false,
      shouldDebounce: false,
      debounce: 150,
    });
  });

  test("parses prevent modifier", () => {
    const el = document.createElement("button");
    el.dataset.lPrevent = "true";
    const mods = parseModifiers(el as unknown as HTMLElement);
    expect(mods.prevent).toBe(true);
  });

  test("parses stop modifier", () => {
    const el = document.createElement("button");
    el.dataset.lStop = "true";
    const mods = parseModifiers(el as unknown as HTMLElement);
    expect(mods.stop).toBe(true);
  });

  test("parses debounce with custom value", () => {
    const el = document.createElement("input");
    el.dataset.lDebounce = "300";
    const mods = parseModifiers(el as unknown as HTMLElement);
    expect(mods.shouldDebounce).toBe(true);
    expect(mods.debounce).toBe(300);
  });

  test("uses default debounce when attribute is empty", () => {
    const el = document.createElement("input");
    el.dataset.lDebounce = "";
    const mods = parseModifiers(el as unknown as HTMLElement);
    expect(mods.shouldDebounce).toBe(true);
    expect(mods.debounce).toBe(150);
  });
});

describe("collectSpecialVars", () => {
  test("collects value from input event", () => {
    const input = document.createElement("input");
    (input as unknown as HTMLInputElement).value = "hello";
    const event = new (window as any).Event("input", { bubbles: true });
    Object.defineProperty(event, "target", { value: input });

    const vars = collectSpecialVars(event as unknown as Event);
    expect(vars.value).toBe("hello");
    expect(vars.checked).toBeUndefined();
    expect(vars.key).toBeUndefined();
  });

  test("collects checked from checkbox", () => {
    const checkbox = document.createElement("input");
    checkbox.type = "checkbox";
    (checkbox as any).checked = true;
    const event = new (window as any).Event("change", { bubbles: true });
    Object.defineProperty(event, "target", { value: checkbox });

    const vars = collectSpecialVars(event as unknown as Event);
    expect(vars.checked).toBe(true);
  });

  test("collects checked from radio", () => {
    const radio = document.createElement("input");
    radio.type = "radio";
    (radio as any).checked = true;
    const event = new (window as any).Event("change", { bubbles: true });
    Object.defineProperty(event, "target", { value: radio });

    const vars = collectSpecialVars(event as unknown as Event);
    expect(vars.checked).toBe(true);
  });

  test("collects key from keyboard event", () => {
    const input = document.createElement("input");
    const event = new (window as any).KeyboardEvent("keydown", {
      key: "Enter",
    });
    Object.defineProperty(event, "target", { value: input });

    const vars = collectSpecialVars(event as unknown as Event);
    expect(vars.key).toBe("Enter");
  });
});

describe("getNodeKey", () => {
  test("returns null for non-element nodes", () => {
    const text = document.createTextNode("hello");
    expect(getNodeKey(text as unknown as Node)).toBeNull();
  });

  test("returns data-l-input as key", () => {
    const el = document.createElement("input");
    el.dataset.lInput = "handle_input";
    expect(getNodeKey(el as unknown as Node)).toBe("handle_input");
  });

  test("returns data-l-click as key", () => {
    const el = document.createElement("button");
    el.dataset.lClick = "handle_click";
    expect(getNodeKey(el as unknown as Node)).toBe("handle_click");
  });

  test("returns data-l-change as key", () => {
    const el = document.createElement("select");
    el.dataset.lChange = "handle_change";
    expect(getNodeKey(el as unknown as Node)).toBe("handle_change");
  });

  test("returns data-l-submit as key", () => {
    const el = document.createElement("form");
    el.dataset.lSubmit = "handle_submit";
    expect(getNodeKey(el as unknown as Node)).toBe("handle_submit");
  });

  test("falls back to id", () => {
    const el = document.createElement("div");
    el.id = "my-div";
    expect(getNodeKey(el as unknown as Node)).toBe("my-div");
  });

  test("returns null when no identifying attribute", () => {
    const el = document.createElement("div");
    expect(getNodeKey(el as unknown as Node)).toBeNull();
  });

  test("prefers data-l-input over id", () => {
    const el = document.createElement("input");
    el.dataset.lInput = "handle_input";
    el.id = "my-input";
    expect(getNodeKey(el as unknown as Node)).toBe("handle_input");
  });
});

describe("saveFocus", () => {
  test("returns non-input state when no element is focused", () => {
    const state = saveFocus();
    expect(state.isInput).toBe(false);
    expect(state.selectionStart).toBeNull();
    expect(state.selectionEnd).toBeNull();
  });

  test("captures input focus state", () => {
    const input = document.createElement("input");
    input.dataset.lInput = "search";
    document.body.appendChild(input);
    (input as any).focus();

    const state = saveFocus();
    expect(state.isInput).toBe(true);
    expect(state.handlerId).toBe("search");
  });

  test("captures textarea focus state", () => {
    const textarea = document.createElement("textarea");
    textarea.dataset.lChange = "update_text";
    document.body.appendChild(textarea);
    (textarea as any).focus();

    const state = saveFocus();
    expect(state.isInput).toBe(true);
    expect(state.handlerId).toBe("update_text");
  });
});

describe("restoreFocus", () => {
  test("does nothing when saved state is not an input", () => {
    const container = document.createElement("div") as unknown as HTMLElement;
    restoreFocus(container, {
      element: null,
      isInput: false,
      selectionStart: null,
      selectionEnd: null,
      handlerId: undefined,
    });
    // Should not throw
  });

  test("does nothing when handlerId is undefined", () => {
    const container = document.createElement("div") as unknown as HTMLElement;
    restoreFocus(container, {
      element: null,
      isInput: true,
      selectionStart: 0,
      selectionEnd: 0,
      handlerId: undefined,
    });
    // Should not throw
  });

  test("restores focus to element by data-l-input", () => {
    const container = document.createElement("div");
    const input = document.createElement("input");
    input.dataset.lInput = "search";
    container.appendChild(input);
    document.body.appendChild(container);

    restoreFocus(container as unknown as HTMLElement, {
      element: null,
      isInput: true,
      selectionStart: 3,
      selectionEnd: 3,
      handlerId: "search",
    });

    expect(document.activeElement).toBe(input);
  });

  test("restores focus to element by data-l-change", () => {
    const container = document.createElement("div");
    const select = document.createElement("select");
    select.dataset.lChange = "pick";
    container.appendChild(select);
    document.body.appendChild(container);

    restoreFocus(container as unknown as HTMLElement, {
      element: null,
      isInput: true,
      selectionStart: null,
      selectionEnd: null,
      handlerId: "pick",
    });

    expect(document.activeElement).toBe(select);
  });
});
