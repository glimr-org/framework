import { CONFIG } from "~/config";
import type { NavCacheEntry, NavHistoryState } from "~/types";

/**
 * Full page reloads tear down the WebSocket, destroy all live
 * component state, and force the browser to re-parse the entire
 * document. Intercepting same-origin link clicks and swapping
 * just the body avoids all of that, keeping the shared socket
 * alive and only cycling the live components that actually
 * changed.
 *
 * Lifecycle callbacks from index.ts let this class stay
 * decoupled from LoomLive — it only knows how to destroy old
 * components and initialize new ones, not how they work
 * internally.
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
   * Delegated listeners on document avoid per-element binding and
   * automatically cover dynamically added links. Seeding
   * history.state on the initial page ensures the first popstate
   * (back button) can identify this as a Loom-managed entry
   * rather than falling through to a full reload.
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
   * Tearing down listeners cleanly lets consuming code disable
   * SPA navigation at runtime — for example, before a full-page
   * form submission or when switching to a non-Loom section of
   * the app. The enabled guard makes repeated calls safe without
   * tracking external state.
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
   * Exposes SPA navigation as a programmatic API so custom
   * scripts and the window.Loom.navigate helper can trigger
   * client-side transitions without simulating link clicks. The
   * returned promise lets callers await the full swap-and-reinit
   * cycle.
   */
  async navigate(url: string): Promise<void> {
    await this.performNavigation(url, false);
  }

  /**
   * Only left-clicks without modifier keys should trigger SPA
   * navigation — middle-click, ctrl-click, etc. are intentional
   * "open in new tab" gestures that the browser should handle
   * natively. Delegating from document and using closest("a")
   * catches clicks on child elements inside links (e.g. icons or
   * spans).
   */
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
   * Not every link should be intercepted — downloads, external
   * origins, target="_blank", non-HTTP protocols, and hash-only
   * changes all need native browser handling. The l-no-nav opt-
   * out lets template authors exclude specific links or regions
   * from SPA navigation. Kept as a separate method so it can be
   * tested in isolation.
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

  /**
   * GET forms (e.g. search, filters) are functionally equivalent
   * to link navigations — their result is a new page at a URL
   * with query parameters. Intercepting them lets search and
   * filter forms benefit from SPA navigation without requiring
   * template authors to wire up custom JavaScript.
   */
  private handleSubmit(e: SubmitEvent): void {
    const form = e.target as HTMLFormElement;
    if (!this.shouldInterceptForm(form)) return;

    e.preventDefault();

    const action = new URL(form.action || location.href, location.origin);
    const params = new URLSearchParams(new FormData(form) as any);
    action.search = params.toString();

    this.performNavigation(action.href, false);
  }

  /**
   * Only GET forms with same-origin actions can be safely turned
   * into SPA navigations — POST/PUT/DELETE forms have side
   * effects that require a real server round-trip, and multipart
   * forms may carry file uploads. The l-no-nav opt-out lets
   * authors exclude specific forms from interception.
   */
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

  /**
   * Prefetching on hover exploits the ~200-400ms gap between
   * hovering over a link and actually clicking it. The delay
   * timer avoids fetching for accidental mouse-overs, and the
   * abort controller lets mouseout cancel an in-flight prefetch.
   * Cached and in-flight URLs are skipped to avoid redundant
   * requests.
   */
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

  /**
   * Cancels the prefetch timer and any in-flight request when the
   * mouse leaves a link, preventing wasted bandwidth for links
   * the user didn't intend to visit.
   */
  private handleMouseOut(e: MouseEvent): void {
    const anchor = (e.target as Element).closest?.("a");
    if (!anchor) return;
    this.cancelPrefetch();
  }

  /**
   * Both the delay timer and the fetch request need to be
   * cancelled together — clearing only the timer would leave an
   * orphaned fetch, and aborting only the fetch could still fire
   * a stale timer callback.
   */
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

  /**
   * URLs must be normalized before use as cache keys so that
   * "about", "/about", and "https://host/about" all resolve to
   * the same entry. Using the URL constructor against the current
   * origin handles relative paths, trailing slashes, and port
   * normalization.
   */
  normalizeUrl(href: string): string {
    return new URL(href, location.origin).href;
  }

  /**
   * Stale cache entries could serve outdated content, so entries
   * are evicted on read when they exceed the TTL. Returning null
   * on miss lets the caller fall through to a network fetch
   * transparently.
   */
  cacheGet(url: string): NavCacheEntry | null {
    const entry = this.cache.get(url);
    if (!entry) return null;
    if (Date.now() - entry.timestamp > CONFIG.navCacheTTL) {
      this.cache.delete(url);
      return null;
    }
    return entry;
  }

  /**
   * Unbounded caches would grow indefinitely during long sessions.
   * Evicting the oldest entry when the cap is reached keeps
   * memory usage predictable while still caching the most
   * recently visited pages for instant back-navigation.
   */
  cacheSet(url: string, entry: NavCacheEntry): void {
    if (this.cache.size >= CONFIG.navCacheMaxEntries) {
      const oldest = this.cache.keys().next().value!;
      this.cache.delete(oldest);
    }
    this.cache.set(url, entry);
  }

  /**
   * Fetches a page as HTML and parses it into a cache entry. The
   * Accept header ensures the server returns HTML rather than
   * JSON or other formats. Redirect tracking via resp.redirected
   * stores the final URL so the address bar reflects where the
   * user actually landed. Returning null on failure lets the
   * caller fall back to a full browser navigation.
   */
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

  /**
   * Only the body HTML, title, and specific head elements need to
   * change between pages — scripts, base tags, and charset
   * declarations stay the same. Parsing into separate categories
   * lets mergeHead do targeted updates (diff stylesheets, replace
   * meta tags, swap inline styles) instead of replacing the
   * entire head, which would cause FOUC from re-downloading
   * shared CSS.
   */
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

  /**
   * Orchestrates the full navigation lifecycle: abort any in-
   * flight navigation, fire a cancelable before-event, resolve
   * the page (cache/inflight/fetch), destroy old live components,
   * swap the DOM, update history, init new live components, and
   * restore scroll. The AbortController prevents stale fetches
   * from completing after a newer navigation has started.
   * The before/after custom events let application code hook
   * into the transition for analytics, loading indicators, or
   * transition animations.
   */
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
   * Each page may have different meta tags, stylesheets, and
   * inline styles. Naively replacing the entire head would force
   * the browser to re-download shared CSS, causing a flash of
   * unstyled content. Instead, this diffs each category: meta
   * tags are matched by name/property, stylesheet links are
   * matched by href (shared ones stay in place), and inline
   * styles are fully swapped since they're typically page-
   * specific.
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
        const key = name
          ? `name:${name}`
          : property
            ? `property:${property}`
            : null;
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
   * Using replaceChildren instead of innerHTML ensures the
   * browser fires disconnectedCallback on any custom elements and
   * properly detaches old nodes, preventing memory leaks from
   * orphaned event listeners or observers on the replaced subtree.
   */
  swapBody(html: string): void {
    const template = document.createElement("template");
    template.innerHTML = html;
    document.body.replaceChildren(...template.content.childNodes);
  }

  /**
   * The browser's HTML parser skips script execution for content
   * inserted via innerHTML or replaceChildren. Cloning each
   * script into a fresh element forces the browser to treat it as
   * newly added, triggering execution. This is necessary for
   * inline analytics snippets or page-specific initialization
   * scripts.
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

  /**
   * The popstate event fires for all history entries, not just
   * Loom-managed ones. Checking for loomNavId in state
   * distinguishes our entries from external ones — unknown
   * entries trigger a full reload to avoid rendering stale or
   * foreign content. For known entries, the isPopState flag tells
   * performNavigation to restore scroll position instead of
   * pushing a new history entry.
   */
  private handlePopState(e: PopStateEvent): void {
    const state = e.state as NavHistoryState | null;
    if (!state?.loomNavId) {
      window.location.reload();
      return;
    }

    this.currentNavId = state.loomNavId;
    this.performNavigation(state.url, true);
  }

  /**
   * Each navigation gets a unique ID so scroll positions can be
   * stored and restored per history entry. A simple counter
   * suffices because IDs only need to be unique within the
   * current page session — they're never persisted or shared.
   */
  private nextNavId(): string {
    return `nav-${++this.navCounter}`;
  }
}
