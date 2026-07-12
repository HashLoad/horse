---
name: horse-advanced-routing
description: "Guidelines and workflows for developing and maintaining regular expressions and optional parameter routing features in the Horse framework."
---

# Horse Advanced Routing Skill

This skill guides AI agents in maintaining, expanding, and debugging Advanced Routing features (Regular Expression constraints and Optional path parameters) within the Horse framework.

## 💡 Syntax Reference

- **Optional parameters:** `:paramName?`
  - *Example:* `/users/:id?`
- **Regex constraints:** `:paramName(regexPattern)`
  - *Example:* `/users/:id(\d+)`
- **Legacy segment Regex:** `(regexPattern)` (Keep for backwards compatibility only)
  - *Example:* `/users/(\d+)`

## ⚙️ Architecture and Execution Flow

### 1. Regex Helper (`src/Horse.Core.Regex.pas`)
Always use the unified `THorseRegex` abstraction. Do NOT import `System.RegularExpressions` (Delphi) or `RegExpr` (FPC) directly in routers to prevent cross-compilation breaks.
- Use `THorseRegex.Create(APattern)` to instantiate.
- Use `THorseRegex.Match(AContent)` which returns `Boolean` indicating if the exact string matches the pattern.
- Always free the regex matcher instance inside the node destructions (`TRadixNode.Destroy` / `THorseRouterTree.Destroy`).

### 2. Parameter Injection
When a path segment matches a route parameter, always ensure it is registered in `THorseRequest.Params` even if optional and empty:
- Optional parameter missing → `Params.Items['param'] = ''`.
- Ensure backtracking logic in `CallNextPath` (Tree) and `FindNode` (Radix) clears parameters during path rejection to prevent parameters from bleeding into other matched paths.

### 3. Route Precedence
When routing, the engines prioritize matches as follows:
1. Exact static segment matches (`/users/new`)
2. Regular expression matched parameters (`/users/:id(\d+)`)
3. Optional or general parameters (`/users/:id?`)

## 🧪 Testing Guidelines
- Always write integration tests in `tests/src/tests/Tests.Integration.AdvancedRouting.pas` verifying both `THorseRadixRouter` and `THorseRouterTree`.
- Test real network calls with `System.Net.HttpClient` to confirm full transport compatibility.
- Ensure ports are freed properly during TearDown using the `ClearGlobalState` helper.
