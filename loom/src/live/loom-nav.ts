import { CONFIG } from "@/config";
import type { NavCacheEntry, NavHistoryState } from "@/types";

/**
 * SPA-like navigation that intercepts link clicks, fetches pages
 * over HTTP, and swaps the body — keeping the WebSocket open and
 * recycling live components instead of tearing everything down.
 *
 * Takes lifecycle callbacks from index.ts so it can destroy old
 * LoomLive instances before a swap and initialize new ones after.
 */
export class LoomNav {
  private destroyLive: () => void;
  private initLive: () => void;
  private cache = new Map<string, NavCacheEntry>();
  private scrollPositions = new Map<string, { x: number; y: number }>();
  private navCounter = 0;
  private currentNavId: string;
  private prefetchTimer: ReturnType<typeof setTimeout> | null = null;
  private prefetchController: AbortController | null = null;
  private inflightFetches = new Map<string, Promise<NavCacheEntry | null>>();
  private navigationController: AbortController | null = null;
  private enabled = false;

  private boundHandleClick: (e: MouseEvent) => void;
  private boundHandleMouseOver: (e: MouseEvent) => void;
  private boundHandleMouseOut: (e: MouseEvent) => void;
  private boundHandlePopState: (e: PopStateEvent) => void;
  private boundHandleSubmit: (e: SubmitEvent) => void;

  constructor(
    destroyLiveComponents: () => void,
    initLiveComponents: () => void,
  ) {
    this.destroyLive = destroyLiveComponents;
    this.initLive = initLiveComponents;
    this.currentNavId = this.nextNavId();

    this.boundHandleClick = this.handleClick.bind(this);
    this.boundHandleMouseOver = this.handleMouseOver.bind(this);
    this.boundHandleMouseOut = this.handleMouseOut.bind(this);
    this.boundHandlePopState = this.handlePopState.bind(this);
    this.boundHandleSubmit = this.handleSubmit.bind(this);
  }

  /**
   * Attaches delegated click/mouseover/mouseout/popstate/submit
   * listeners on document. Seeds history.state for the initial
   * page so popstate can identify Loom-managed entries.
   */
  enable(): void {
    if (this.enabled) return;
    this.enabled = true;

    history.replaceState(
      { loomNavId: this.currentNavId, url: location.href } as NavHistoryState,
      "",
      location.href,
    );

    document.addEventListener("click", this.boundHandleClick);
    document.addEventListener("mouseover", this.boundHandleMouseOver);
    document.addEventListener("mouseout", this.boundHandleMouseOut);
    document.addEventListener("submit", this.boundHandleSubmit);
    window.addEventListener("popstate", this.boundHandlePopState);
  }

  /**
   * Removes all listeners. Safe to call multiple times.
   */
  disable(): void {
    if (!this.enabled) return;
    this.enabled = false;

    document.removeEventListener("click", this.boundHandleClick);
    document.removeEventListener("mouseover", this.boundHandleMouseOver);
    document.removeEventListener("mouseout", this.boundHandleMouseOut);
    document.removeEventListener("submit", this.boundHandleSubmit);
    window.removeEventListener("popstate", this.boundHandlePopState);

    this.cancelPrefetch();
  }

  /**
   * Programmatic navigation. Returns a promise that resolves
   * after the DOM swap and live component re-initialization.
   */
  async navigate(url: string): Promise<void> {
    await this.performNavigation(url, false);
  }

  // ----------------------------------------------------------
  // Link interception
  // ----------------------------------------------------------

  private handleClick(e: MouseEvent): void {
    if (e.button !== 0) return;
    if (e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;

    const anchor = (e.target as Element).closest?.("a");
    if (!anchor) return;
    if (!this.shouldInterceptLink(anchor)) return;

    e.preventDefault();
    this.performNavigation(anchor.href, false);
  }

  /**
   * Determines whether a link should be intercepted for SPA
   * navigation. Exported via the test helper for unit tests.
   */
  shouldInterceptLink(anchor: HTMLAnchorElement): boolean {
    if (anchor.closest("[data-l-no-nav], [l-no-nav]")) return false;
    if (anchor.hasAttribute("download")) return false;

    const target = anchor.getAttribute("target");
    if (target && target !== "_self") return false;

    const href = anchor.getAttribute("href");
    if (!href) return false;

    const protocol = anchor.protocol;
    if (protocol && protocol !== "http:" && protocol !== "https:") return false;

    if (anchor.origin !== location.origin) return false;

    // Same-page hash-only change — let the browser handle it
    if (
      anchor.pathname === location.pathname &&
      anchor.search === location.search &&
      anchor.hash !== ""
    ) {
      return false;
    }

    return true;
  }

  // ----------------------------------------------------------
  // Form interception
  // ----------------------------------------------------------

  private handleSubmit(e: SubmitEvent): void {
    const form = e.target as HTMLFormElement;
    if (!this.shouldInterceptForm(form)) return;

    e.preventDefault();

    const action = new URL(form.action || location.href, location.origin);
    const params = new URLSearchParams(new FormData(form) as any);
    action.search = params.toString();

    this.performNavigation(action.href, false);
  }

  shouldInterceptForm(form: HTMLFormElement): boolean {
    const method = (form.method || "GET").toUpperCase();
    if (method !== "GET") return false;
    if (form.closest("[data-l-no-nav], [l-no-nav]")) return false;

    const target = form.getAttribute("target");
    if (target && target !== "_self") return false;

    if (form.enctype === "multipart/form-data") return false;

    const action = form.action
      ? new URL(form.action, location.origin)
      : new URL(location.href);
    if (action.origin !== location.origin) return false;

    return true;
  }

  // ----------------------------------------------------------
  // Prefetch
  // ----------------------------------------------------------

  private handleMouseOver(e: MouseEvent): void {
    const anchor = (e.target as Element).closest?.("a");
    if (!anchor || !this.shouldInterceptLink(anchor)) return;

    const url = this.normalizeUrl(anchor.href);
    if (this.cacheGet(url)) return;

    this.cancelPrefetch();

    this.prefetchTimer = setTimeout(() => {
      this.prefetchController = new AbortController();
      const promise = this.fetchPage(url, this.prefetchController.signal);
      this.inflightFetches.set(url, promise);
      promise.then(
        (entry) => {
          this.inflightFetches.delete(url);
          if (entry) this.cacheSet(url, entry);
        },
        () => {
          this.inflightFetches.delete(url);
        },
      );
    }, CONFIG.navPrefetchDelay);
  }

  private handleMouseOut(e: MouseEvent): void {
    const anchor = (e.target as Element).closest?.("a");
    if (!anchor) return;
    this.cancelPrefetch();
  }

  private cancelPrefetch(): void {
    if (this.prefetchTimer) {
      clearTimeout(this.prefetchTimer);
      this.prefetchTimer = null;
    }
    if (this.prefetchController) {
      this.prefetchController.abort();
      this.prefetchController = null;
    }
  }

  // ----------------------------------------------------------
  // Cache
  // ----------------------------------------------------------

  normalizeUrl(href: string): string {
    return new URL(href, location.origin).href;
  }

  cacheGet(url: string): NavCacheEntry | null {
    const entry = this.cache.get(url);
    if (!entry) return null;
    if (Date.now() - entry.timestamp > CONFIG.navCacheTTL) {
      this.cache.delete(url);
      return null;
    }
    return entry;
  }

  cacheSet(url: string, entry: NavCacheEntry): void {
    if (this.cache.size >= CONFIG.navCacheMaxEntries) {
      const oldest = this.cache.keys().next().value!;
      this.cache.delete(oldest);
    }
    this.cache.set(url, entry);
  }

  // ----------------------------------------------------------
  // Fetch + parse
  // ----------------------------------------------------------

  async fetchPage(
    url: string,
    signal?: AbortSignal,
  ): Promise<NavCacheEntry | null> {
    try {
      const resp = await fetch(url, {
        headers: { Accept: "text/html" },
        signal,
      });

      if (!resp.ok) return null;

      const ct = resp.headers.get("Content-Type") || "";
      if (!ct.includes("text/html")) return null;

      const html = await resp.text();
      const entry = this.parsePage(html);
      if (resp.redirected) {
        entry.resolvedUrl = resp.url;
      }
      return entry;
    } catch {
      return null;
    }
  }

  parsePage(html: string): NavCacheEntry {
    const doc = new DOMParser().parseFromString(html, "text/html");

    const headMeta: string[] = [];
    doc
      .querySelectorAll("head meta[name], head meta[property]")
      .forEach((el) => headMeta.push(el.outerHTML));

    const headLinks: string[] = [];
    doc
      .querySelectorAll('head link[rel="stylesheet"]')
      .forEach((el) => headLinks.push(el.outerHTML));

    const headStyles: string[] = [];
    doc
      .querySelectorAll("head style")
      .forEach((el) => headStyles.push(el.outerHTML));

    return {
      html: doc.body.innerHTML,
      title: doc.title,
      headMeta,
      headLinks,
      headStyles,
      timestamp: Date.now(),
    };
  }

  // ----------------------------------------------------------
  // Navigation execution
  // ----------------------------------------------------------

  private async performNavigation(
    url: string,
    isPopState: boolean,
  ): Promise<void> {
    // Abort any in-flight navigation
    if (this.navigationController) {
      this.navigationController.abort();
    }
    this.navigationController = new AbortController();
    const signal = this.navigationController.signal;

    const normalizedUrl = this.normalizeUrl(url);

    // Dispatch cancelable before-navigate event
    const beforeEvent = new CustomEvent("loom:before-navigate", {
      cancelable: true,
      detail: { url: normalizedUrl, isPopState },
    });
    if (!document.dispatchEvent(beforeEvent)) return;

    // Get page from cache, inflight fetch, or new fetch
    let entry = this.cacheGet(normalizedUrl);
    if (!entry) {
      const inflight = this.inflightFetches.get(normalizedUrl);
      if (inflight) {
        entry = await inflight;
      } else {
        entry = await this.fetchPage(normalizedUrl, signal);
      }
    }

    // If aborted during fetch, bail out
    if (signal.aborted) return;

    // Fallback to full navigation on failure
    if (!entry) {
      window.location.href = url;
      return;
    }

    // Use the final URL after redirects if the server redirected
    const targetUrl = entry.resolvedUrl || normalizedUrl;

    // Save scroll position for current page
    this.scrollPositions.set(this.currentNavId, {
      x: window.scrollX,
      y: window.scrollY,
    });

    // Destroy existing live components
    this.destroyLive();

    // Swap DOM
    this.swapBody(entry.html);
    document.title = entry.title;
    this.mergeHead(entry);
    this.reExecuteScripts();

    // Update history
    if (!isPopState) {
      this.currentNavId = this.nextNavId();
      history.pushState(
        { loomNavId: this.currentNavId, url: targetUrl } as NavHistoryState,
        "",
        targetUrl,
      );
    }

    // Initialize live components on the new page
    this.initLive();

    // Scroll handling
    if (isPopState) {
      const state = history.state as NavHistoryState | null;
      const navId = state?.loomNavId;
      if (navId) {
        const saved = this.scrollPositions.get(navId);
        if (saved) {
          window.scrollTo(saved.x, saved.y);
        }
      }
    } else {
      const hash = new URL(targetUrl).hash;
      if (hash) {
        const target = document.querySelector(hash);
        if (target) {
          target.scrollIntoView();
        } else {
          window.scrollTo(0, 0);
        }
      } else {
        window.scrollTo(0, 0);
      }
    }

    document.dispatchEvent(
      new CustomEvent("loom:after-navigate", {
        detail: { url: targetUrl, isPopState },
      }),
    );
  }

  /**
   * Swaps head elements to match the new page. Replaces meta
   * tags, does a full swap of stylesheets and inline styles so
   * page-specific CSS doesn't leak across navigations. Shared
   * stylesheet links (same href) are kept in place to avoid
   * FOUC; only stale ones are removed.
   */
  private mergeHead(entry: NavCacheEntry): void {
    // Build set of meta identifiers the new page has
    const newMetaKeys = new Set<string>();
    for (const metaHtml of entry.headMeta) {
      const temp = document.createElement("div");
      temp.innerHTML = metaHtml;
      const meta = temp.firstChild as HTMLMetaElement;
      if (!meta) continue;
      const name = meta.getAttribute("name");
      const property = meta.getAttribute("property");
      if (name) newMetaKeys.add(`name:${name}`);
      else if (property) newMetaKeys.add(`property:${property}`);
    }

    // Remove stale meta tags not in the new page
    document.head
      .querySelectorAll("meta[name], meta[property]")
      .forEach((existing) => {
        const name = existing.getAttribute("name");
        const property = existing.getAttribute("property");
        const key = name ? `name:${name}` : property ? `property:${property}` : null;
        if (key && !newMetaKeys.has(key)) {
          existing.remove();
        }
      });

    // Update or add meta tags from the new page
    for (const metaHtml of entry.headMeta) {
      const temp = document.createElement("div");
      temp.innerHTML = metaHtml;
      const newMeta = temp.firstChild as HTMLMetaElement;
      if (!newMeta) continue;

      const name = newMeta.getAttribute("name");
      const property = newMeta.getAttribute("property");
      const selector = name
        ? `meta[name="${name}"]`
        : property
          ? `meta[property="${property}"]`
          : null;

      if (selector) {
        const existing = document.head.querySelector(selector);
        if (existing) {
          existing.replaceWith(newMeta);
        } else {
          document.head.appendChild(newMeta);
        }
      }
    }

    // Build set of hrefs the new page needs
    const newLinkHrefs = new Set<string>();
    for (const linkHtml of entry.headLinks) {
      const temp = document.createElement("div");
      temp.innerHTML = linkHtml;
      const link = temp.firstChild as HTMLLinkElement;
      const href = link?.getAttribute("href");
      if (href) newLinkHrefs.add(href);
    }

    // Remove stylesheet links not present in the new page
    document.head
      .querySelectorAll('link[rel="stylesheet"]')
      .forEach((existing) => {
        const href = existing.getAttribute("href");
        if (href && !newLinkHrefs.has(href)) {
          existing.remove();
        }
      });

    // Add stylesheet links not already in the current head
    for (const linkHtml of entry.headLinks) {
      const temp = document.createElement("div");
      temp.innerHTML = linkHtml;
      const newLink = temp.firstChild as HTMLLinkElement;
      if (!newLink) continue;

      const href = newLink.getAttribute("href");
      if (href && !document.head.querySelector(`link[href="${href}"]`)) {
        document.head.appendChild(newLink);
      }
    }

    // Replace all inline <style> tags with the new page's styles
    document.head.querySelectorAll("style").forEach((el) => el.remove());
    for (const styleHtml of entry.headStyles) {
      const temp = document.createElement("div");
      temp.innerHTML = styleHtml;
      const style = temp.firstChild;
      if (style) document.head.appendChild(style);
    }
  }

  /**
   * Replaces the body's children with parsed HTML content.
   * Uses replaceChildren instead of innerHTML so the browser
   * fires disconnectedCallback on custom elements and properly
   * detaches old nodes from the DOM tree.
   */
  swapBody(html: string): void {
    const template = document.createElement("template");
    template.innerHTML = html;
    document.body.replaceChildren(...template.content.childNodes);
  }

  /**
   * Inline scripts in innerHTML are not executed by the browser.
   * Clone and replace each one so the browser treats it as new.
   */
  private reExecuteScripts(): void {
    document.body.querySelectorAll("script").forEach((old) => {
      const replacement = document.createElement("script");
      for (const attr of old.attributes) {
        replacement.setAttribute(attr.name, attr.value);
      }
      replacement.textContent = old.textContent;
      old.replaceWith(replacement);
    });
  }

  // ----------------------------------------------------------
  // Popstate
  // ----------------------------------------------------------

  private handlePopState(e: PopStateEvent): void {
    const state = e.state as NavHistoryState | null;
    if (!state?.loomNavId) {
      window.location.reload();
      return;
    }

    this.currentNavId = state.loomNavId;
    this.performNavigation(state.url, true);
  }

  // ----------------------------------------------------------
  // Helpers
  // ----------------------------------------------------------

  private nextNavId(): string {
    return `nav-${++this.navCounter}`;
  }
}
