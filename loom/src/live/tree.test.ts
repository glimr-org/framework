import { describe, expect, test } from "bun:test";
import {
  reconstruct,
  renderDynamic,
  applyDiff,
  applySubtreeDiff,
  applyListDiff,
} from "@/live/tree";

describe("reconstruct", () => {
  test("simple string dynamic", () => {
    expect(reconstruct(["<p>Count: ", "</p>"], ["42"])).toBe(
      "<p>Count: 42</p>",
    );
  });

  test("multiple dynamics", () => {
    expect(reconstruct(["<p>", " and ", "</p>"], ["hello", "world"])).toBe(
      "<p>hello and world</p>",
    );
  });

  test("no dynamics", () => {
    expect(reconstruct(["<p>Static only</p>"], [])).toBe(
      "<p>Static only</p>",
    );
  });

  test("nested subtree dynamic", () => {
    const inner = { s: ["<b>", "</b>"], d: ["bold"] };
    expect(reconstruct(["<p>", "</p>"], [inner])).toBe("<p><b>bold</b></p>");
  });

  test("list of trees dynamic", () => {
    const list = [
      { s: ["<li>", "</li>"], d: ["a"] },
      { s: ["<li>", "</li>"], d: ["b"] },
    ];
    expect(reconstruct(["<ul>", "</ul>"], [list])).toBe(
      "<ul><li>a</li><li>b</li></ul>",
    );
  });

  test("mixed dynamic types", () => {
    const subtree = { s: ["<b>", "</b>"], d: ["bold"] };
    const list = [
      { s: ["<li>", "</li>"], d: ["x"] },
    ];
    expect(
      reconstruct(["<div>", " ", " ", "</div>"], ["text", subtree, list]),
    ).toBe("<div>text <b>bold</b> <li>x</li></div>");
  });

  test("deeply nested subtrees", () => {
    const inner = { s: ["<i>", "</i>"], d: ["deep"] };
    const outer = { s: ["<b>", "</b>"], d: [inner] };
    expect(reconstruct(["<p>", "</p>"], [outer])).toBe(
      "<p><b><i>deep</i></b></p>",
    );
  });
});

describe("renderDynamic", () => {
  test("string leaf", () => {
    expect(renderDynamic("hello")).toBe("hello");
  });

  test("nested tree", () => {
    expect(renderDynamic({ s: ["<b>", "</b>"], d: ["x"] })).toBe("<b>x</b>");
  });

  test("list of trees", () => {
    const list = [
      { s: ["<li>", "</li>"], d: ["a"] },
      { s: ["<li>", "</li>"], d: ["b"] },
    ];
    expect(renderDynamic(list)).toBe("<li>a</li><li>b</li>");
  });

  test("null returns empty string", () => {
    expect(renderDynamic(null)).toBe("");
  });

  test("undefined returns empty string", () => {
    expect(renderDynamic(undefined)).toBe("");
  });

  test("number returns empty string", () => {
    expect(renderDynamic(42)).toBe("");
  });
});

describe("applyDiff", () => {
  test("replaces string leaf", () => {
    const dynamics = ["0", "hello"];
    applyDiff(dynamics, { "0": "1" });
    expect(dynamics).toEqual(["1", "hello"]);
  });

  test("replaces multiple leaves", () => {
    const dynamics = ["a", "b", "c"];
    applyDiff(dynamics, { "0": "x", "2": "z" });
    expect(dynamics).toEqual(["x", "b", "z"]);
  });

  test("replaces full list (array value)", () => {
    const dynamics: any[] = [
      [{ s: ["<li>", "</li>"], d: ["a"] }],
    ];
    const newList = [
      { s: ["<li>", "</li>"], d: ["a"] },
      { s: ["<li>", "</li>"], d: ["b"] },
    ];
    applyDiff(dynamics, { "0": newList });
    expect(dynamics[0]).toEqual(newList);
  });

  test("replaces full subtree (branch flip)", () => {
    const dynamics: any[] = [
      { s: ["<p>yes</p>"], d: [] },
    ];
    const newTree = { s: ["<p>no</p>"], d: [] };
    applyDiff(dynamics, { "0": newTree });
    expect(dynamics[0]).toEqual(newTree);
  });

  test("nested diff into subtree", () => {
    const dynamics: any[] = [
      { s: ["<p>", "</p>"], d: ["old"] },
    ];
    applyDiff(dynamics, { "0": { d: { "0": "new" } } });
    expect(dynamics[0].d[0]).toBe("new");
  });

  test("nested diff into list", () => {
    const dynamics: any[] = [
      [
        { s: ["<li>", "</li>"], d: ["a"] },
        { s: ["<li>", "</li>"], d: ["b"] },
      ],
    ];
    applyDiff(dynamics, { "0": { d: { "1": { d: { "0": "c" } } } } });
    expect(dynamics[0][0].d[0]).toBe("a");
    expect(dynamics[0][1].d[0]).toBe("c");
  });

  test("no-op with null diff", () => {
    const dynamics = ["a", "b"];
    applyDiff(dynamics, null);
    expect(dynamics).toEqual(["a", "b"]);
  });

  test("no-op with empty diff", () => {
    const dynamics = ["a", "b"];
    applyDiff(dynamics, {});
    expect(dynamics).toEqual(["a", "b"]);
  });
});

describe("applySubtreeDiff", () => {
  test("replaces leaf in subtree", () => {
    const tree = { s: ["<p>", " ", "</p>"], d: ["hello", "world"] };
    applySubtreeDiff(tree, { "1": "gleam" });
    expect(tree.d).toEqual(["hello", "gleam"]);
  });

  test("replaces full subtree inside subtree", () => {
    const tree = {
      s: ["", ""],
      d: [{ s: ["<p>old</p>"], d: [] }],
    };
    applySubtreeDiff(tree, { "0": { s: ["<p>new</p>"], d: [] } });
    expect(tree.d[0]).toEqual({ s: ["<p>new</p>"], d: [] });
  });

  test("nested diff into child subtree", () => {
    const tree = {
      s: ["", ""],
      d: [{ s: ["<p>", "</p>"], d: ["old"] }],
    };
    applySubtreeDiff(tree, { "0": { d: { "0": "new" } } });
    expect(tree.d[0].d[0]).toBe("new");
  });

  test("no-op with null diff", () => {
    const tree = { s: ["<p>", "</p>"], d: ["hello"] };
    applySubtreeDiff(tree, null);
    expect(tree.d).toEqual(["hello"]);
  });
});

describe("applyListDiff", () => {
  test("replaces full tree in list", () => {
    const list = [
      { s: ["<li>", "</li>"], d: ["a"] },
      { s: ["<li>", "</li>"], d: ["b"] },
    ];
    applyListDiff(list, { "1": { s: ["<li>", "</li>"], d: ["c"] } });
    expect(list[0].d[0]).toBe("a");
    expect(list[1].d[0]).toBe("c");
  });

  test("nested diff into list item", () => {
    const list = [
      { s: ["<li>", "</li>"], d: ["a"] },
      { s: ["<li>", "</li>"], d: ["b"] },
    ];
    applyListDiff(list, { "0": { d: { "0": "x" } } });
    expect(list[0].d[0]).toBe("x");
    expect(list[1].d[0]).toBe("b");
  });

  test("no-op with null diff", () => {
    const list = [{ s: ["<li>", "</li>"], d: ["a"] }];
    applyListDiff(list, null);
    expect(list[0].d[0]).toBe("a");
  });

  test("ignores diff for non-existent index", () => {
    const list = [{ s: ["<li>", "</li>"], d: ["a"] }];
    applyListDiff(list, { "5": { d: { "0": "x" } } });
    expect(list).toHaveLength(1);
    expect(list[0].d[0]).toBe("a");
  });
});

describe("reconstruct + applyDiff integration", () => {
  test("apply diff then reconstruct produces updated HTML", () => {
    const statics = ["<p>Count: ", "</p>"];
    const dynamics: any[] = ["0"];

    applyDiff(dynamics, { "0": "1" });
    expect(reconstruct(statics, dynamics)).toBe("<p>Count: 1</p>");
  });

  test("multiple patches accumulate correctly", () => {
    const statics = ["<p>", " - ", "</p>"];
    const dynamics: any[] = ["a", "b"];

    applyDiff(dynamics, { "0": "x" });
    expect(reconstruct(statics, dynamics)).toBe("<p>x - b</p>");

    applyDiff(dynamics, { "1": "y" });
    expect(reconstruct(statics, dynamics)).toBe("<p>x - y</p>");
  });

  test("branch flip updates reconstruction", () => {
    const statics = ["<div>", "</div>"];
    const dynamics: any[] = [{ s: ["<p>yes</p>"], d: [] }];

    expect(reconstruct(statics, dynamics)).toBe("<div><p>yes</p></div>");

    applyDiff(dynamics, { "0": { s: ["<p>no</p>"], d: [] } });
    expect(reconstruct(statics, dynamics)).toBe("<div><p>no</p></div>");
  });

  test("list item diff updates reconstruction", () => {
    const statics = ["<ul>", "</ul>"];
    const dynamics: any[] = [
      [
        { s: ["<li>", "</li>"], d: ["a"] },
        { s: ["<li>", "</li>"], d: ["b"] },
      ],
    ];

    expect(reconstruct(statics, dynamics)).toBe(
      "<ul><li>a</li><li>b</li></ul>",
    );

    applyDiff(dynamics, { "0": { d: { "1": { d: { "0": "c" } } } } });
    expect(reconstruct(statics, dynamics)).toBe(
      "<ul><li>a</li><li>c</li></ul>",
    );
  });
});
