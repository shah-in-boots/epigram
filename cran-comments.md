# recast

## R CMD check results

0 errors | 0 warnings | 0 notes

`devtools::check(args = c("--no-manual"))` (which runs under `--as-cran`) is
clean locally, including vignette rebuilding and the full test suite.

* This is a new submission.

* The package was previously prepared for submission under an earlier,
  substantially different implementation (see `NEWS.md` and
  [blueprint.md](https://github.com/shah-in-boots/recast/blob/main/blueprint.md)
  for the full rebuild history). The term, formula, fitting, collection, and
  table layers have all been reworked since; the notes from that earlier
  attempt (reference formatting, `@return`/`\value` documentation,
  `.GlobalEnv` avoidance, a stray LaTeX symbol) no longer apply to the
  current source and are not repeated here.

* Before submitting, bump `Version` in `DESCRIPTION` from the development
  placeholder (`0.0.0.9000`) to the intended release version, and confirm
  `NEWS.md`'s top entry is finalized.

## Downstream dependencies

There are no downstream dependencies, as this is a new submission.
