import {
  ExtensionContext,
  languages,
  SemanticTokens,
  SemanticTokensBuilder,
  SemanticTokensLegend,
  TextDocument,
} from "vscode";

import { defineExtension } from "reactive-vscode";
import { logger } from "./utils";

// -------- Semantic token types --------
// Use standard semantic token types wherever possible for best theme support
const tokenTypes = [
  // Standard-ish for TOML-like config
  "comment",
  "punctuation",
  "property", // config key
  "operator", // '='
  "number",
  "boolean",
  "string",

  // TM-specific
  "state",
  "stateSection",
  "input",
  "tape",
  "blank",
  "wildcard",
  "read",
  "write",
  "move",
] as const;

type TokenType = (typeof tokenTypes)[number];
const legend = new SemanticTokensLegend(tokenTypes as unknown as string[], []);

// -------- Data model --------
type Span = { start: number; end: number };
type Token = { text: string; span: Span; line: number; colStart: number; colEnd: number };
type Line = { line: number; text: string; tokens: Token[]; comment?: Span };
type TMConfig = { numTapes: number };
type Spec = {
  lines: Line[];
  config: TMConfig;
  Q: Set<string>;
  S: Set<string>;
  G: Set<string>;
  q0?: string;
  b?: string;
  F: Set<string>;
  transitionStartLine: number | null;
};

// -------- Lex --------
function isSpace(ch: string): boolean {
  return /\s/.test(ch);
}

function lexDocument(doc: TextDocument): Line[] {
  const out: Line[] = [];
  for (let li = 0; li < doc.lineCount; li++) {
    const text = doc.lineAt(li).text;
    const hash = text.indexOf("#");
    const upto = hash === -1 ? text.length : hash;

    const tokens: Token[] = [];
    let i = 0;
    while (i < upto) {
      while (i < upto && isSpace(text[i])) i++;
      if (i >= upto) break;
      let j = i;
      while (j < upto && !isSpace(text[j])) j++;
      const tokText = text.slice(i, j);
      tokens.push({
        text: tokText,
        span: { start: i, end: j },
        line: li,
        colStart: i,
        colEnd: j,
      });
      i = j;
    }

    const comment = hash === -1 ? undefined : ({ start: hash, end: text.length } as Span);
    out.push({ line: li, text, tokens, comment });
  }
  return out;
}

// -------- Config parsing (for num_tapes) --------
function parseConfig(lines: Line[]): TMConfig {
  let inBlock = false;
  let numTapes = 1;

  for (const L of lines) {
    const trimmed = L.text.trimStart();
    if (/^#\s*\/\/\/\s*config\s*$/.test(trimmed)) {
      inBlock = true;
      continue;
    }
    if (inBlock && /^#\s*\/\/\/\s*$/.test(trimmed)) {
      inBlock = false;
      continue;
    }
    if (!inBlock) continue;
    if (!trimmed.startsWith("#")) continue;

    // treat it like toml key=value
    const afterHash = trimmed.slice(1).trimStart();
    const eq = afterHash.indexOf("=");
    if (eq === -1) continue;

    const key = afterHash.slice(0, eq).trim();
    const value = afterHash.slice(eq + 1).trim();

    if (key === "num_tapes") {
      const unquoted = value.replace(/^["']|["']$/g, "");
      const n = Number(unquoted);
      if (Number.isFinite(n) && n >= 1) numTapes = Math.floor(n);
    }
  }

  return { numTapes };
}

// -------- Spec parsing (sections, transitions) --------
function parseSpec(lines: Line[]): Spec {
  const spec: Spec = {
    lines,
    config: parseConfig(lines),
    Q: new Set(),
    S: new Set(),
    G: new Set(),
    q0: undefined,
    b: undefined,
    F: new Set(),
    transitionStartLine: null,
  };

  const nonEmptyIdx: number[] = [];
  for (let i = 0; i < lines.length; i++) if (lines[i].tokens.length > 0) nonEmptyIdx.push(i);
  let p = 0;

  if (p < nonEmptyIdx.length) for (const t of lines[nonEmptyIdx[p++]].tokens) spec.Q.add(t.text);
  if (p < nonEmptyIdx.length) for (const t of lines[nonEmptyIdx[p++]].tokens) spec.S.add(t.text);
  if (p < nonEmptyIdx.length) for (const t of lines[nonEmptyIdx[p++]].tokens) spec.G.add(t.text);

  if (p < nonEmptyIdx.length && lines[nonEmptyIdx[p]].tokens.length)
    spec.q0 = lines[nonEmptyIdx[p++]].tokens[0].text;

  if (p < nonEmptyIdx.length && lines[nonEmptyIdx[p]].tokens.length)
    spec.b = lines[nonEmptyIdx[p++]].tokens[0].text;

  if (p < nonEmptyIdx.length) {
    const L = lines[nonEmptyIdx[p]];
    const looksLikeSingle =
      spec.config.numTapes === 1 &&
      L.tokens.length === 5 &&
      spec.Q.has(L.tokens[0].text) &&
      spec.G.has(L.tokens[1].text) &&
      spec.Q.has(L.tokens[2].text) &&
      spec.G.has(L.tokens[3].text) &&
      /^[LRS]$/.test(L.tokens[4].text);

    const looksLikeMulti =
      spec.config.numTapes > 1 &&
      L.tokens.length === 2 + spec.config.numTapes * 3 &&
      spec.Q.has(L.tokens[0].text) &&
      spec.Q.has(L.tokens[1 + spec.config.numTapes]?.text ?? "");

    if (!looksLikeSingle && !looksLikeMulti) {
      for (const t of L.tokens) spec.F.add(t.text);
      p++;
    }
  }

  spec.transitionStartLine = p < nonEmptyIdx.length ? nonEmptyIdx[p] : null;
  return spec;
}

// -------- Build semantic tokens (returns SemanticTokens) --------
function buildTokens(doc: TextDocument): SemanticTokens {
  const builder = new SemanticTokensBuilder(legend);
  const lines = lexDocument(doc);
  const spec = parseSpec(lines);

  const idxOf = (t: TokenType) => tokenTypes.indexOf(t);
  const push = (t: Token, type: TokenType) =>
    builder.push(t.line, t.colStart, Math.max(0, t.colEnd - t.colStart), idxOf(type));

  // 1) Comments and TOML-like config without overshadowing
  let inConfig = false;
  for (const L of lines) {
    const trimmed = L.text.trimStart();
    const isConfigStart = /^#\s*\/\/\/\s*config\s*$/.test(trimmed);
    const isConfigEnd = /^#\s*\/\/\/\s*$/.test(trimmed);

    if (isConfigStart) {
      // Only color the '#' so the rest can be TOML tokens
      const hashCol = L.text.indexOf("#");
      if (hashCol >= 0) builder.push(L.line, hashCol, 1, idxOf("punctuation"));
      inConfig = true;
      continue;
    }
    if (inConfig && isConfigEnd) {
      const hashCol = L.text.indexOf("#");
      if (hashCol >= 0) builder.push(L.line, hashCol, 1, idxOf("punctuation"));
      inConfig = false;
      continue;
    }

    if (inConfig) {
      // Inside config block: highlight like TOML (key = value) after '#'
      const hashCol = L.text.indexOf("#");
      if (hashCol >= 0) {
        // '#' punctuation
        builder.push(L.line, hashCol, 1, idxOf("punctuation"));

        const after = L.text.slice(hashCol + 1);
        // key = value
        const m = after.match(/^\s*([A-Za-z0-9_-]+)\s*(=)\s*(.+?)\s*$/);
        if (m) {
          const [, key, eq, value] = m;
          const full = m[0];
          const base = hashCol + 1;

          // key
          const keyStart = base + full.indexOf(key);
          builder.push(L.line, keyStart, key.length, idxOf("property"));

          // '='
          const eqStart = base + full.indexOf(eq);
          builder.push(L.line, eqStart, 1, idxOf("operator"));

          // value classification
          const rawValStart = base + full.indexOf(value);
          const v = value.trim();
          const vStart = rawValStart + value.indexOf(v);

          if (/^"(?:[^"\\]|\\.)*"$/.test(v) || /^'(?:[^'\\]|\\.)*'$/.test(v)) {
            builder.push(L.line, vStart, v.length, idxOf("string"));
          } else if (/^(true|false)$/.test(v)) {
            builder.push(L.line, vStart, v.length, idxOf("boolean"));
          } else if (/^\d+$/.test(v)) {
            builder.push(L.line, vStart, v.length, idxOf("number"));
          } else {
            // enums/bare identifiers (e.g., right-only, simultaneous)
            builder.push(L.line, vStart, v.length, idxOf("string"));
          }
        }
      }
      continue;
    }

    // Outside config: regular comment treatment (whole trailing part)
    if (L.comment) {
      // '#' punctuation
      builder.push(L.line, L.comment.start, 1, idxOf("punctuation"));
      // rest as comment
      const rest = L.comment.end - (L.comment.start + 1);
      if (rest > 0) {
        builder.push(L.line, L.comment.start + 1, rest, idxOf("comment"));
      }
    }
  }

  // 2) Sections (before transitions)
  const nonEmptyIdx: number[] = [];
  for (let i = 0; i < lines.length; i++) if (lines[i].tokens.length > 0) nonEmptyIdx.push(i);

  let p = 0;
  const markStatesLine = (L: Line) => {
    for (const t of L.tokens) push(t, "stateSection");
  };
  const markSymbolsLine = (L: Line, kind: TokenType) => {
    for (const t of L.tokens) {
      if (t.text === "_") push(t, "blank");
      else if (t.text === "*") push(t, "wildcard");
      else push(t, kind);
    }
  };

  // Q
  if (p < nonEmptyIdx.length) markStatesLine(lines[nonEmptyIdx[p++]]);
  // Σ
  if (p < nonEmptyIdx.length) markSymbolsLine(lines[nonEmptyIdx[p++]], "input");
  // Γ
  if (p < nonEmptyIdx.length) markSymbolsLine(lines[nonEmptyIdx[p++]], "tape");
  // q0
  if (p < nonEmptyIdx.length) markStatesLine(lines[nonEmptyIdx[p++]]);
  // b
  if (p < nonEmptyIdx.length) markSymbolsLine(lines[nonEmptyIdx[p++]], "tape");

  // Optional F (if next line is not a transition)
  if (p < nonEmptyIdx.length) {
    const L = lines[nonEmptyIdx[p]];
    const looksSingle =
      spec.config.numTapes === 1 &&
      L.tokens.length === 5 &&
      spec.Q.has(L.tokens[0].text) &&
      spec.G.has(L.tokens[1].text) &&
      spec.Q.has(L.tokens[2].text) &&
      spec.G.has(L.tokens[3].text) &&
      /^[LRS]$/.test(L.tokens[4].text);

    const looksMulti =
      spec.config.numTapes > 1 &&
      L.tokens.length === 2 + spec.config.numTapes * 3 &&
      spec.Q.has(L.tokens[0].text) &&
      spec.Q.has(L.tokens[1 + spec.config.numTapes]?.text ?? "");

    if (!looksSingle && !looksMulti) {
      markStatesLine(L); // F line
      p++;
    }
  }

  // 3) Transitions
  const start = p < nonEmptyIdx.length ? nonEmptyIdx[p] : null;
  if (start !== null) {
    for (let li = start; li < lines.length; li++) {
      const T = lines[li].tokens;
      if (T.length === 0) continue;

      const markRead = (tok: Token) => {
        if (tok.text === "_") push(tok, "blank");
        else if (tok.text === "*") push(tok, "wildcard");
        else push(tok, "read");
      };
      const markWrite = (tok: Token) => {
        if (tok.text === "_") push(tok, "blank");
        else if (tok.text === "*") push(tok, "wildcard");
        else push(tok, "write");
      };
      const markMove = (tok: Token) => {
        if (/^[LRS]$/.test(tok.text)) push(tok, "move");
      };

      if (spec.config.numTapes === 1) {
        // from read to write move
        if (T.length !== 5) continue;
        const [from, read, to, write, move] = T;
        push(from, "state");
        markRead(read);
        push(to, "state");
        markWrite(write);
        markMove(move);
      } else {
        // from r1..rN to w1 m1 ... wN mN
        const N = spec.config.numTapes;
        const expected = 2 + 3 * N;
        if (T.length !== expected) continue;

        push(T[0], "state"); // from

        for (let i = 0; i < N; i++) markRead(T[1 + i]);

        push(T[1 + N], "state"); // to

        for (let i = 0; i < N; i++) {
          const write = T[1 + N + 1 + 2 * i];
          const move = T[1 + N + 1 + 2 * i + 1];
          markWrite(write);
          markMove(move);
        }
      }
    }
  }

  return builder.build();
}

// -------- Register provider --------
export = defineExtension((context: ExtensionContext) => {
  logger.info("Extension Activated");

  const selector = [{ language: "turing" }, { pattern: "**/*.turing" }];

  context.subscriptions.push(
    languages.registerDocumentSemanticTokensProvider(
      selector,
      {
        provideDocumentSemanticTokens(document): SemanticTokens {
          return buildTokens(document);
        },
      },
      legend
    )
  );
});