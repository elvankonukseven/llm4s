Test Coverage (scoverage)

Overview
- Uses `sbt-scoverage` to measure line and branch coverage for Scala 2.13 and Scala 3 builds.
- Global defaults: minimum 80% coverage, build fails if below.

How To Run Locally
- Quick aliases:
  - From sbt shell: `cov` (aggregate + single root HTML) or `covReport` (current module HTML).
  - From terminal: `sbt cov` or `sbt covReport`.

- Per-module coverage report:
  1) `sbt clean coverage test coverageReport`
  2) Open `target/scala-*/scoverage-report/index.html` for the current module.

- Aggregate coverage across aggregated modules (root project):
  1) `sbt clean coverage test coverageAggregate coverageReport`
  2) The alias config avoids per-module HTML; open root `target/scala-*/scoverage-report/index.html` for the combined report.

CI Usage
- Typical sequence: `clean; coverage; test; coverageAggregate`.
- XML output (for CI tooling) is written to `target/scala-*/scoverage-report/scoverage.xml`.

Configuration
- Defined in `build.sbt`:
  - `coverageMinimumStmtTotal := 80` — required minimum statement coverage; adjust to taste.
  - `coverageFailOnMinimum := false` — fail build if under the minimum.
  - `coverageHighlighting := true` — better highlighting for coverage in reports.
  - `coverageExcludedPackages` — excludes runner and samples packages by default.

On‑the‑fly Overrides
- For a one-off run: `sbt "set coverageMinimumStmtTotal := 85" clean coverage test coverageReport`
- To disable fail on minimum for local runs: `sbt "set coverageFailOnMinimum := false" coverage test`

Expectations
- Target at least 80% coverage for core library code.
- Generated/entrypoint code (e.g., runner, samples) is excluded to keep focus on library logic.

Per‑module vs aggregate
- By default, the setup generates a single HTML report at the root when using `cov`.
- If you want individual project HTML reports, run `coverageReport` after switching to each project with `project <name>`.

Troubleshooting
- If coverage is 0% across the board, ensure you ran `coverage` before `test`.
- For multi-module runs, use `coverageAggregate` at the root project.
