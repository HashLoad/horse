# Contributing to Horse

*Read this in [English](./CONTRIBUTING.md) or [Português (BR)](./CONTRIBUTING.pt-BR.md).*

Thanks for your interest in improving Horse. This document explains how to report bugs, suggest features, contribute code, and — importantly — keep our **bilingual documentation** (English / Portuguese-BR) in sync.

If you only want to fix a typo or open a small PR, skip to [Pull Request Process](#pull-request-process).

---

## Reporting bugs

Open a GitHub issue at [`HashLoad/horse/issues`](https://github.com/HashLoad/horse/issues) with:

1. **What you expected** to happen.
2. **What actually happened** (include stack traces, response bodies, HTTP status).
3. **Minimal reproduction** — a `.dpr` of 30 lines or less is ideal. Strip everything that isn't required to trigger the bug.
4. **Environment**:
   - Delphi or FPC version (e.g. `Delphi 12 Athens` / `FPC 3.2.2 + Lazarus 2.2`).
   - Target platform (`Win64`, `Linux64`, `macOS ARM64`, …).
   - Active provider define(s): `HORSE_CROSSSOCKET`, `HORSE_VCL`, `HORSE_DAEMON`, etc. (none = default Console + Indy).
   - Horse version (or commit SHA).

The pre-built provider samples in `samples/delphi/` and `samples/lazarus/` are good baselines to compare against. If the bug reproduces in an unmodified sample, mention which one.

## Suggesting features

Open an issue describing **the problem you want to solve**, not the implementation you have in mind. We can discuss approaches before code is written. New transport providers, new core types, and new public API surfaces are best discussed before a PR — small refactors and bug fixes can go straight to a PR.

## Contributing code

### Branching and commits

- Branch off `master` for fixes and small features; off `dev` for ongoing larger work (when present).
- Use [Conventional Commits](https://www.conventionalcommits.org/) prefixes: `feat:`, `fix:`, `chore:`, `docs:`, `refactor:`, `test:`, `perf:`.
- One logical change per commit. Squash WIP commits before pushing.

### Code style

Pascal style follows the surrounding code. The project does not enforce a formatter, but in summary:
- 2-space indent (no tabs).
- `PascalCase` for types and routines; `camelCase` for local variables.
- `T` prefix for classes, `I` prefix for interfaces, `F` prefix for fields, `E` prefix for exceptions, `H` for helpers when needed.
- `try / finally` for every resource; free what you create.
- Always test mentally against **both** `dcc32` (Delphi) and `fpc` (FPC) for changes to shared units. The most common slip-ups are inline `var` (Delphi 10.3+ only) and Delphi-only RTL types like `TWebRequest`. See [Compiler Support](./doc/compiler-support.md) for the guard patterns to use.

### Cross-compiler requirements

Every change to `src/Horse.*.pas` must compile under both:

- Delphi 10.4 Sydney (or newer)
- FPC 3.2.0 (or newer) with Lazarus

Use `{$IF DEFINED(FPC)} ... {$ELSE} ... {$ENDIF}` to branch where the RTL differs (most often `Web.HTTPApp` vs `fpHTTP` / `HTTPDefs`).

### Tests

Add or extend tests under `tests/` when you change behaviour. Run them locally before pushing:

```sh
# From the project root, after boss install
boss test
```

CI runs on Delphi 11 / 12 against the same suite. A green CI is a prerequisite for merge.

## Contributing documentation

The documentation lives in two layers:

- **`README.md`** + **`README.pt-BR.md`** — landing pages with the quickstart, the provider overview, and pointers to `doc/`.
- **`doc/`** — the wiki: eight topic pages, each in two languages (`.md` for English, `.pt-BR.md` for Portuguese).

### The bilingual rule

**Every doc page must exist in both languages.** When you edit one, you edit the other in the same PR. PRs that touch only one language will be asked to add the matching translation before merge.

| You edit | You must also edit |
|---|---|
| `README.md` | `README.pt-BR.md` |
| `doc/<topic>.md` | `doc/<topic>.pt-BR.md` |
| Add new page `doc/new-topic.md` | Add `doc/new-topic.pt-BR.md` and update both `doc/index.md` and `doc/index.pt-BR.md` |

The bilingual rule keeps the two language versions from drifting. We've seen projects where the secondary language was added once and then never updated — the docs become misleading for half the users. This project avoids that by treating both languages as primary.

### If you only speak one of the two languages

That's fine — open the PR with your edits in the language you're comfortable with and write the matching file with a clear placeholder:

```markdown
# <Title — Translated>

*Read this in [English](./<file>.md) or [Português (BR)](./<file>.pt-BR.md).*

> **Translation pending.** This page mirrors [`<file>.md`](./<file>.md) but hasn't been translated yet.
> Reviewers fluent in this language are welcome to update it.
```

A reviewer or follow-up PR will fill in the translation. The structure (headings, code blocks, link targets) should match the source language's page so a future translator only has to translate prose.

### Language switcher banner

Every doc page (including the README) starts with this banner immediately after the title:

```markdown
*Read this in [English](./<file>.md) or [Português (BR)](./<file>.pt-BR.md).*
```

For the README, which uses HTML `<p align="center">` for its top section, the equivalent block is:

```html
<p align="center">
  <i>Read this in <a href="./README.md">English</a> or <a href="./README.pt-BR.md">Português (BR)</a>.</i>
</p>
```

The Portuguese version reads `Leia em [English](...) ou [Português (BR)](...)`.

### Style notes

- Keep paragraphs short — most readers scan rather than read.
- Code blocks should be **runnable** — paste them into a fresh project and they compile.
- For technical terms widely used in English in the Delphi/FPC Brazilian community (`provider`, `middleware`, `thread`, `pool`, `framework`, `pull request`), keep the English form in the Portuguese text — it reads more naturally than a forced translation.
- Tables and headings translate fully; preserve the original column order.
- When you reference another doc, link to the same-language version: in `routing.pt-BR.md`, link to `request-response.pt-BR.md`, not `request-response.md`. The language switcher at the top of every page handles the cross-language jump.

### Verifying internal links

Before opening the PR, run this from the repo root to catch broken cross-references:

```bash
for f in README.md README.pt-BR.md doc/*.md; do
  grep -oE '\]\(\.\/[^)]+\.md[^)]*\)' "$f" \
    | sed -E 's|\]\(\./||; s|\)$||; s|#.*||' \
    | while read target; do
        if [ -n "$target" ]; then
          dir=$(dirname "$f")
          [ -f "$dir/$target" ] || echo "BROKEN in $f: $target"
        fi
      done
done
```

Silent output = every internal link resolves.

## Pull request process

1. **Fork** `HashLoad/horse` and create a topic branch in your fork.
2. **Make focused commits** — one logical change per commit, conventional-commit message.
3. **Test locally** — code changes must pass `boss test`; doc changes must pass the link-checker above.
4. **Update both languages** for any doc change. See [the bilingual rule](#the-bilingual-rule).
5. **Open the PR** against `HashLoad/horse:master` (or `:dev` if the repo currently uses a development branch).
6. **Describe the change** in the PR body: what changes, why, and any user-visible impact.
7. **Respond to review** in the same PR — please don't open a new PR for review feedback; push fixup commits to the existing branch.

The maintainers aim to triage every PR within a week. If you don't hear back in two weeks, a polite ping on the PR is welcome.

## Reporting security issues

For security-sensitive reports — e.g. remote crash, header smuggling, certificate validation bypass — please **do not** open a public issue. Reach out via the Telegram channel ([@hashload](https://t.me/hashload)) or e-mail one of the maintainers listed in `git log` directly. We'll coordinate a fix and a coordinated disclosure timeline.

## Community

- Telegram: [@hashload](https://t.me/hashload) — the most active venue for questions and announcements.
- Issues: [`HashLoad/horse/issues`](https://github.com/HashLoad/horse/issues) — for bugs and feature discussions.
- PRs: [`HashLoad/horse/pulls`](https://github.com/HashLoad/horse/pulls) — for code.

Thanks for contributing.
