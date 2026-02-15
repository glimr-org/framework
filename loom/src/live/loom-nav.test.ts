import { describe, expect, test, beforeEach } from "bun:test";
import { Window } from "happy-dom";
import { LoomNav } from "@/live/loom-nav";
import { CONFIG } from "@/config";

let window: Window;
let document: Document;
let nav: LoomNav;

beforeEach(() => {
  window = new Window({ url: "http://localhost:3000/home" });
  document = window.document as unknown as Document;
  globalThis.document = document;
  globalThis.window = window as any;
  globalThis.location = window.location as any;
  globalThis.DOMParser = window.DOMParser as any;

  nav = new LoomNav(
    () => {},
    () => {},
  );
});

// ----------------------------------------------------------
// shouldInterceptLink
// ----------------------------------------------------------

describe("shouldInterceptLink", () => {
  const makeAnchor = (attrs: Record<string, string>): HTMLAnchorElement => {
    const a = document.createElement("a") as unknown as HTMLAnchorElement;
    for (const [k, v] of Object.entries(attrs)) {
      a.setAttribute(k, v);
    }
    return a;
  };

  test("intercepts same-origin link", () => {
    const a = makeAnchor({ href: "/about" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(true);
  });

  test("skips external link", () => {
    const a = makeAnchor({ href: "https://example.com/page" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });

  test("skips mailto link", () => {
    const a = makeAnchor({ href: "mailto:test@example.com" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });

  test("skips tel link", () => {
    const a = makeAnchor({ href: "tel:+1234567890" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });

  test("skips link with download attribute", () => {
    const a = makeAnchor({ href: "/file.pdf", download: "" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });

  test("skips link with target=_blank", () => {
    const a = makeAnchor({ href: "/about", target: "_blank" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });

  test("allows link with target=_self", () => {
    const a = makeAnchor({ href: "/about", target: "_self" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(true);
  });

  test("skips link with data-l-no-nav", () => {
    const a = makeAnchor({ href: "/about", "data-l-no-nav": "" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });

  test("skips link inside ancestor with data-l-no-nav", () => {
    const wrapper = document.createElement("div");
    wrapper.setAttribute("data-l-no-nav", "");
    const a = makeAnchor({ href: "/about" });
    wrapper.appendChild(a as any);
    document.body.appendChild(wrapper);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });

  test("skips link with l-no-nav", () => {
    const a = makeAnchor({ href: "/about", "l-no-nav": "" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });

  test("skips link inside ancestor with l-no-nav", () => {
    const wrapper = document.createElement("div");
    wrapper.setAttribute("l-no-nav", "");
    const a = makeAnchor({ href: "/about" });
    wrapper.appendChild(a as any);
    document.body.appendChild(wrapper);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });

  test("skips same-page hash-only change", () => {
    const a = makeAnchor({ href: "/home#section" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });

  test("intercepts same path with different query", () => {
    const a = makeAnchor({ href: "/home?page=2" });
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(true);
  });

  test("skips link without href", () => {
    const a = makeAnchor({});
    document.body.appendChild(a as any);
    expect(nav.shouldInterceptLink(a)).toBe(false);
  });
});

// ----------------------------------------------------------
// shouldInterceptForm
// ----------------------------------------------------------

describe("shouldInterceptForm", () => {
  const makeForm = (attrs: Record<string, string>): HTMLFormElement => {
    const form = document.createElement("form") as unknown as HTMLFormElement;
    for (const [k, v] of Object.entries(attrs)) {
      form.setAttribute(k, v);
    }
    return form;
  };

  test("intercepts GET form", () => {
    const form = makeForm({ method: "GET", action: "/search" });
    document.body.appendChild(form as any);
    expect(nav.shouldInterceptForm(form)).toBe(true);
  });

  test("intercepts form with no method (defaults to GET)", () => {
    const form = makeForm({ action: "/search" });
    document.body.appendChild(form as any);
    expect(nav.shouldInterceptForm(form)).toBe(true);
  });

  test("skips POST form", () => {
    const form = makeForm({ method: "POST", action: "/submit" });
    document.body.appendChild(form as any);
    expect(nav.shouldInterceptForm(form)).toBe(false);
  });

  test("skips form with data-l-no-nav", () => {
    const form = makeForm({
      method: "GET",
      action: "/search",
      "data-l-no-nav": "",
    });
    document.body.appendChild(form as any);
    expect(nav.shouldInterceptForm(form)).toBe(false);
  });

  test("skips form inside ancestor with data-l-no-nav", () => {
    const wrapper = document.createElement("div");
    wrapper.setAttribute("data-l-no-nav", "");
    const form = makeForm({ method: "GET", action: "/search" });
    wrapper.appendChild(form as any);
    document.body.appendChild(wrapper);
    expect(nav.shouldInterceptForm(form)).toBe(false);
  });

  test("skips form with l-no-nav", () => {
    const form = makeForm({
      method: "GET",
      action: "/search",
      "l-no-nav": "",
    });
    document.body.appendChild(form as any);
    expect(nav.shouldInterceptForm(form)).toBe(false);
  });

  test("skips form with target=_blank", () => {
    const form = makeForm({
      method: "GET",
      action: "/search",
      target: "_blank",
    });
    document.body.appendChild(form as any);
    expect(nav.shouldInterceptForm(form)).toBe(false);
  });

  test("skips form with multipart enctype", () => {
    const form = makeForm({
      method: "GET",
      action: "/search",
      enctype: "multipart/form-data",
    });
    document.body.appendChild(form as any);
    expect(nav.shouldInterceptForm(form)).toBe(false);
  });

  test("skips form with external action", () => {
    const form = makeForm({
      method: "GET",
      action: "https://example.com/search",
    });
    document.body.appendChild(form as any);
    expect(nav.shouldInterceptForm(form)).toBe(false);
  });
});

// ----------------------------------------------------------
// Cache
// ----------------------------------------------------------

describe("cache", () => {
  const makeEntry = (overrides?: Partial<import("@/types").NavCacheEntry>) => ({
    html: "<p>test</p>",
    title: "Test",
    headMeta: [],
    headLinks: [],
    headStyles: [],
    timestamp: Date.now(),
    ...overrides,
  });

  test("stores and retrieves entries", () => {
    const entry = makeEntry();
    nav.cacheSet("http://localhost:3000/about", entry);
    expect(nav.cacheGet("http://localhost:3000/about")).toEqual(entry);
  });

  test("returns null for missing entry", () => {
    expect(nav.cacheGet("http://localhost:3000/missing")).toBeNull();
  });

  test("returns null for expired entry", () => {
    const entry = makeEntry({
      timestamp: Date.now() - CONFIG.navCacheTTL - 1,
    });
    nav.cacheSet("http://localhost:3000/old", entry);
    expect(nav.cacheGet("http://localhost:3000/old")).toBeNull();
  });

  test("evicts oldest entry when at capacity", () => {
    for (let i = 0; i < CONFIG.navCacheMaxEntries; i++) {
      nav.cacheSet(`http://localhost:3000/page${i}`, makeEntry());
    }

    // Adding one more should evict the first
    nav.cacheSet("http://localhost:3000/new", makeEntry());
    expect(nav.cacheGet("http://localhost:3000/page0")).toBeNull();
    expect(nav.cacheGet("http://localhost:3000/new")).not.toBeNull();
  });

  test("normalizes URLs", () => {
    expect(nav.normalizeUrl("/about")).toBe("http://localhost:3000/about");
    expect(nav.normalizeUrl("http://localhost:3000/about")).toBe(
      "http://localhost:3000/about",
    );
  });
});

// ----------------------------------------------------------
// parsePage
// ----------------------------------------------------------

describe("parsePage", () => {
  test("extracts body, title, meta, links, and styles", () => {
    const html = `<!DOCTYPE html>
<html>
<head>
  <title>About Us</title>
  <meta name="description" content="About page">
  <meta property="og:title" content="About">
  <link rel="stylesheet" href="/styles/about.css">
  <style>body { background: red; }</style>
</head>
<body>
  <h1>About</h1>
  <p>Content here</p>
</body>
</html>`;

    const entry = nav.parsePage(html);
    expect(entry.title).toBe("About Us");
    expect(entry.html).toContain("<h1>About</h1>");
    expect(entry.html).toContain("<p>Content here</p>");
    expect(entry.headMeta).toHaveLength(2);
    expect(entry.headMeta[0]).toContain('name="description"');
    expect(entry.headMeta[1]).toContain('property="og:title"');
    expect(entry.headLinks).toHaveLength(1);
    expect(entry.headLinks[0]).toContain('href="/styles/about.css"');
    expect(entry.headStyles).toHaveLength(1);
    expect(entry.headStyles[0]).toContain("background: red");
  });

  test("handles page with no meta, links, or styles", () => {
    const html = `<html><head><title>Simple</title></head><body><p>Hi</p></body></html>`;
    const entry = nav.parsePage(html);
    expect(entry.title).toBe("Simple");
    expect(entry.html).toContain("<p>Hi</p>");
    expect(entry.headMeta).toHaveLength(0);
    expect(entry.headLinks).toHaveLength(0);
    expect(entry.headStyles).toHaveLength(0);
  });

  test("extracts multiple inline style tags", () => {
    const html = `<html><head>
  <title>Multi</title>
  <style>.a { color: red; }</style>
  <style>.b { color: blue; }</style>
</head><body></body></html>`;

    const entry = nav.parsePage(html);
    expect(entry.headStyles).toHaveLength(2);
    expect(entry.headStyles[0]).toContain("color: red");
    expect(entry.headStyles[1]).toContain("color: blue");
  });

  test("sets timestamp to current time", () => {
    const before = Date.now();
    const entry = nav.parsePage(
      "<html><head><title>T</title></head><body></body></html>",
    );
    const after = Date.now();
    expect(entry.timestamp).toBeGreaterThanOrEqual(before);
    expect(entry.timestamp).toBeLessThanOrEqual(after);
  });
});

// ----------------------------------------------------------
// mergeHead
// ----------------------------------------------------------

describe("mergeHead", () => {
  test("removes stale meta tags not in the new page", () => {
    const meta = document.createElement("meta");
    meta.setAttribute("name", "robots");
    meta.setAttribute("content", "noindex");
    document.head.appendChild(meta);

    // New page has no meta tags
    const entry = nav.parsePage(
      "<html><head><title>New</title></head><body></body></html>",
    );
    (nav as any).mergeHead(entry);

    expect(document.head.querySelector('meta[name="robots"]')).toBeNull();
  });
});
