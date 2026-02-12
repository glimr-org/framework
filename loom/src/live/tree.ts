/**
 * Pure functions for the statics/dynamics tree wire format.
 *
 * A "tree" is { s: string[], d: any[] } where statics and
 * dynamics interleave: s[0] + render(d[0]) + s[1] + ...
 *
 * A dynamic value can be:
 * - string: leaf value
 * - { s, d }: nested subtree (conditional/component)
 * - array of { s, d }: list of subtrees (loop output)
 */

/**
 * Reconstruct an HTML string from statics and dynamics by
 * interleaving them: statics[0] + render(dynamics[0]) +
 * statics[1] + render(dynamics[1]) + ...
 */
export function reconstruct(statics: string[], dynamics: any[]): string {
  let result = "";
  for (let i = 0; i < statics.length; i++) {
    result += statics[i];
    if (i < dynamics.length) {
      result += renderDynamic(dynamics[i]);
    }
  }
  return result;
}

/**
 * Render a single dynamic value to an HTML string. Handles
 * leaf strings, nested trees, and lists of trees.
 */
export function renderDynamic(dyn: any): string {
  if (typeof dyn === "string") {
    return dyn;
  }
  if (Array.isArray(dyn)) {
    // List of trees
    return dyn.map((tree: any) => reconstruct(tree.s, tree.d)).join("");
  }
  if (dyn && typeof dyn === "object" && "s" in dyn && "d" in dyn) {
    // Nested tree
    return reconstruct(dyn.s, dyn.d);
  }
  return "";
}

/**
 * Apply a diff object to a dynamics array in place. The diff
 * keys are string indices into the array. Values can be:
 * - string: replace leaf
 * - array: replace full list
 * - { s, d }: replace full subtree (branch flip)
 * - { d }: nested diff into existing subtree/list
 */
export function applyDiff(dynamics: any[], diff: any): void {
  if (!diff || !dynamics) return;
  for (const key of Object.keys(diff)) {
    const idx = parseInt(key, 10);
    const value = diff[key];
    if (typeof value === "string") {
      dynamics[idx] = value;
    } else if (Array.isArray(value)) {
      dynamics[idx] = value;
    } else if (value && typeof value === "object") {
      if ("s" in value && "d" in value) {
        dynamics[idx] = value;
      } else if ("d" in value) {
        const existing = dynamics[idx];
        if (existing && typeof existing === "object") {
          if (Array.isArray(existing)) {
            applyListDiff(existing, value.d);
          } else if ("d" in existing) {
            applySubtreeDiff(existing, value.d);
          }
        }
      }
    }
  }
}

/**
 * Apply a diff to a subtree's dynamics array.
 */
export function applySubtreeDiff(tree: any, diff: any): void {
  if (!diff || !tree.d) return;
  for (const key of Object.keys(diff)) {
    const idx = parseInt(key, 10);
    const value = diff[key];
    if (typeof value === "string") {
      tree.d[idx] = value;
    } else if (Array.isArray(value)) {
      tree.d[idx] = value;
    } else if (value && typeof value === "object") {
      if ("s" in value && "d" in value) {
        tree.d[idx] = value;
      } else if ("d" in value) {
        const existing = tree.d[idx];
        if (existing && typeof existing === "object") {
          if (Array.isArray(existing)) {
            applyListDiff(existing, value.d);
          } else if ("d" in existing) {
            applySubtreeDiff(existing, value.d);
          }
        }
      }
    }
  }
}

/**
 * Apply a diff to a list of trees. Each key in the diff is
 * an item index, and each value is either a full tree
 * replacement or a nested diff.
 */
export function applyListDiff(list: any[], diff: any): void {
  if (!diff) return;
  for (const key of Object.keys(diff)) {
    const idx = parseInt(key, 10);
    const value = diff[key];
    if (value && typeof value === "object") {
      if ("s" in value && "d" in value) {
        list[idx] = value;
      } else if ("d" in value) {
        if (list[idx] && typeof list[idx] === "object" && "d" in list[idx]) {
          applySubtreeDiff(list[idx], value.d);
        }
      }
    }
  }
}
