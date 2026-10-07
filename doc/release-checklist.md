# Release checklist

*Read this in [English](./release-checklist.md) or [Português (BR)](./release-checklist.pt-BR.md).*

Horse releases use a dotted tag without a `v` prefix (for example, `3.3.6`).
Prepare the version in a PR, merge it into `master`, and create the GitHub
Release from that merge commit. Do not tag an unmerged branch.

## Version map

| Item | When to update | Purpose |
|---|---|---|
| [`boss.json`](../boss.json) `version` | In the release PR | Version distributed to Boss users. |
| [`src/Horse.Constants.pas`](../src/Horse.Constants.pas) `HORSE_VERSION` | In the same PR | Value returned by `THorseCore.Version`. Must equal `boss.json.version`. |
| Git tag and GitHub Release | After merging the PR | Both use the exact same dotted version and point to the merged `master` commit. |
| [`tests/src/boss-lock.json`](../tests/src/boss-lock.json) transitively resolved `github.com/hashload/horse` | **After** the release is public | Test dependency snapshot. Regenerate with Boss in a follow-up PR; do not hand-edit its version or hashes. |

`tests/src/boss.json` has its own `version` for the test project; it is **not**
the Horse release version. Compiler numbers such as FPC 3.3.1, sample package
versions, and example server ports are also unrelated to the Horse release.

## Before publishing

1. Review changes since the last tag, choose the next version, and update both
   release values above in one PR. Search the repository for old Horse-version
   references; update user documentation when API behavior or installation
   instructions changed. Keep English and Portuguese pages in sync.
2. Build and run relevant tests for the changed providers and the default
   provider. Check FPC/Linux where applicable. Confirm whether the GitHub tests
   workflow is enabled; if it is disabled, record the local validation in the PR
   and release notes.
3. Merge the PR, then verify that `master`, `boss.json.version`, and
   `HORSE_VERSION` agree and that the new tag does not already exist.
4. Create a GitHub Release tagged with the same version, targeting the merge
   commit. Include the changes, validation, and a comparison link to the
   previous tag. Verify the published release is not a draft/prerelease and
   that the tag resolves to the intended commit. Synchronize the local checkout
   and maintainer fork with upstream.

## Provider validation

For provider changes, run these commands from the repository root:

```powershell
pwsh -File tests/run_compile_matrix.ps1 -DockerImage fpc-test:latest
pwsh -File tests/run_delphi_tests.ps1
pwsh -File tests/provider-lifecycle/run-provider-lifecycle.ps1
pwsh -File tests/provider-config/run-query-decode.ps1
pwsh -File tests/provider-lifecycle/run-daemon-lifecycle.ps1 -Version 23.0
pwsh -File tests/provider-lifecycle/run-daemon-lifecycle.ps1 -Version 23.0 -Radix
pwsh -File tests/run_e2e_integration_tests.ps1
pwsh -File tests/httpsys-lifetime/run-httpsys-lifetime-test.ps1
pwsh -File tests/console-stability/run-console-stability-test.ps1
```

Use an available Docker image containing FPC and the required Linux libraries;
the image name above is local, not a published prerequisite. Run
`tests/provider-config/run-provider-config.sh` inside that Linux environment.
The static matrix checks compilation, not hosted Apache/CGI/ISAPI behavior.
For actual hosted HTTP execution, follow
[`tests/hosted-providers/README.md`](../tests/hosted-providers/README.md).
Include `run-iis-hosted.ps1` (Delphi 10–13), `run-delphi-apache.ps1`
(Delphi 12/13 Linux), Docker CGI/FastCGI and FPC 3.3.1 keep-alive.
Failures reproduced by the Apache control without Horse are external
limitations; record them separately and do not report an all-provider pass.
The FPC default-provider E2E executable is process-scoped because legacy
`HTTPApplication` has no `StopListen`; it does not prove graceful shutdown.

Matrix, lifecycle, and E2E reports are stored under `benchmarks/results/`.
Check nonzero test counts, errors, failures, and memory-leak output. Preserve
failed reports and investigate before accepting a successful rerun. Do not run
two full Delphi suites concurrently: they share build files and listener ports.
The isolated lifecycle runner uses port 19131 and HTTP.sys lifetime uses 19195.

Document the intentional compatibility change: built-in `ListenWithConfig`
implementations reject unsupported non-default TLS settings before changing
the port or starting a listener. Defaults remain accepted; other unsupported
CrossSocket-specific settings are still unused. Configure TLS through the
selected provider's supported API rather than disabling required security.

## Publishing multiline text with GitHub CLI

Always write multiline PR descriptions, issue comments, and release notes to a
UTF-8 Markdown file and pass that file to GitHub CLI. Do not pass text such as
`"First line\n\nSecond line"` to `--body` or `--notes`: PowerShell and `gh` may
send the backslash and `n` literally, and GitHub will display `\n` instead of a
line break.

```powershell
gh pr create --body-file pr-body.md
gh pr edit <number> --body-file pr-body.md
gh issue comment <number> --body-file issue-comment.md
gh release create <version> --notes-file release-notes.md
gh release edit <version> --notes-file release-notes.md
```

Before deleting the temporary Markdown file, read the saved body back and
confirm that headings, lists, code blocks, accented characters, and line breaks
were preserved:

```powershell
gh pr view <number> --json body --jq .body
gh release view <version> --json body --jq .body
```

For issue comments, also open the returned comment URL or query the comment
through the GitHub API. This verification is part of publishing; a successful
CLI exit code only confirms that GitHub accepted the text, not that it was
formatted as intended.

## After publishing: refresh the test lockfile

1. In a separate branch, run `boss update` from `tests/src` using a known Boss
   CLI version. The Jhonson dependency pulls Horse transitively; a newly
   published Horse release cannot be resolved before its tag exists.
2. Review the generated `boss-lock.json`: its Horse version should match the
   new release. Boss may also update Jhonson/RESTRequest4Delphi and rename
   directories under `modules/`. Review `tests/src/boss.json`, the Delphi
   project search paths, the FPC test workflow, and the compilation matrix for
   those side effects. Do not commit unrelated generated project changes.
3. Rebuild and run the affected test suites with the regenerated dependencies.
   Commit the lockfile and required path changes in a follow-up PR. The tests
   compile Horse from `../../src`; the lockfile is a dependency snapshot, not
   the framework's published version.

Useful final checks: `git status --short`, `git ls-remote --tags upstream <version>`,
`gh release view <version> --repo HashLoad/horse`, and a search for
the previous Horse version in release metadata. Never change a published tag
to hide an omission; fix it in a new PR and release if needed.
