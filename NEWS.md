# recast (developmental version)

## Writing the interaction report with the tables

* An estimate whose interval could not be bounded is now blank, not a
  number. The render step already blanked a cell whose interval ran to
  `Inf`, but `add_estimates(view = "separate")` hands the estimate to its
  own column without the interval, and a limit the likelihood profile
  never found comes back as `NA` rather than `Inf` (R's own `confint()` on
  a separated `glm` does this), so `13203562.47` stood in the table with
  nothing beside it. The estimate is blanked with its interval at the build
  stage, before the two are split into cells

* `modify_labels()` reaches the stratum and its levels. The label was
  accepted and stored, and nothing read it: the stratum band rendered the
  bare column name, and the only remedy was a `gt::text_transform()` on the
  rendered table. Strata now plan, fit, and display in level order (a
  factor's own, else sorted) rather than in order of first appearance in
  the data, so a vector of level labels applies predictably

* `estimate_interaction()` returns `statistic` and `df` beside `p_value`,
  so a table can say "joint Wald, 6 df" without recounting levels by hand

* `estimate_interaction()` refuses a numeric interaction variable that is
  not coded 0/1. The within-level effect is the exposure coefficient plus
  the level's interaction coefficient, which is the model's own contrast
  only for a factor or 0/1; a modifier coded 1/2 was given a wrong number
  silently (its level-2 effect is the exposure coefficient plus *twice* the
  interaction coefficient), and one with three or more values matched the
  same bare key for every level and died in `solve()` with "system is
  exactly singular". The message names the variable and says to `cut()` it
  or make it a factor

* `model_diagnostics()` replaces `model_failures()` and `model_warnings()`:
  one row per model with its `status`, `nobs`, `terms`, the number of
  coefficients `aliased` or `unbounded` (a limit at infinity, or one the
  profile never found), `max_std_error`, the collected `warnings`, and the
  `error` of a failed fit. `fit_status` stays `TRUE` for a separated `glm`,
  because convergence is not estimability -- IRLS converges onto the
  boundary -- and each analyst was inventing a standard-error threshold to
  find them. Both retired helpers were a filter on this one

* `add_strata()` says so when it promotes an existing covariate to a
  stratum: every adjustment set in the family changes, and a caption
  written before the call does not know

* Documented that a variable holding a term name is spliced into the fluent
  and paring verbs with `!!` (`add_strata(f, !!v)`), a vector with `!!!`;
  `add_strata(f, v)` takes `v` itself as the term. Both forms already worked

* Single-use helpers folded into their callers: `count_messages()`,
  `fmls_ptype2()` / `fmls_cast()`, `match_term_keys()`,
  `effect_frame_fields()`, `semantic_context_match()`,
  `empty_group_frame()`, `semantic_missing()`, and
  `collect_cell_group_ids()`, and the `possible_tidy()` /
  `possible_glance()` wrappers (a `tryCatch()` inside the tidiers does the
  same, and `{purrr}` leaves Imports with them); the dead `.models`
  vocabulary and two `.onLoad()` locals that never escaped their frame are
  gone

## Next steps

* A likelihood-ratio option for the interaction test. The Wald test
  understates the evidence on a separated fit (Hauck and Donner 1977; 0.86
  against 0.12 by likelihood ratio on the report's insurance by deprivation
  model), and only the tidy blueprint of a model is stored, so the test
  needs a refit

## Interaction tables, tried against a real analysis

* `add_interaction()` no longer attributes a modifier to a model by role
  alone. The term table is table-wide, so in a table that combines
  `.x(age) + .i(sex) + race` with `.x(age) + .i(race) + sex` each family's
  modifier is the other's plain covariate; the first model was sent after
  an `age:race` coefficient it never had and the render failed. A modifier
  now belongs to a model only when the crossed component
  (`exposure:modifier`, either order) is a member of that model's formula

* A model with no interaction term is set aside with a message when
  `add_interaction()` meets it on a bare mesa (the crude rung of a ladder),
  and refused with the rung named when it was asked for by
  `select_adjustment()` or `select_terms()`. An "Unadjusted" label put on
  the bare-exposure rung used to vanish from the table without a word

* A categorical exposure's reference level is now a contrast column of the
  interaction table, rendered as the reference text, so the table names
  what its odds ratios are against; `nobs` within a stratified model counts
  the model's own stratum (every stratum used to report the whole frame's
  counts) and is `NA` under a `subset_data()` rule, whose rows are not
  recoverable

* Within an adjustment spanner, N now leads and P trails the estimate
  columns; a group lacking the inner column dimension sorted after every
  contrast regardless of its placement

* `estimate_interaction()` names polynomial contrasts as the cause when an
  ordered factor leaves no per-level coefficient to match (`ndi.L`, `ndi.Q`,
  ... where `ndiLevel` was expected) and says to refit with
  `factor(x, ordered = FALSE)`. The refusal was correct before but gave no
  reason

* A bare `add_interaction()` over an adjustment ladder, or over strata, now
  lays the rungs or strata out as the outer row bands on its own; the
  `"interaction"` preset maps only modifier and contrast, so two selected
  rungs used to collide on one coordinate and error. Rows rather than
  columns because a table twice as wide does not fit a portrait page, which
  is where these tables end up. A layout declared with `modify_layout()`
  that omits the dimension still errors, naming it

* An inner row dimension constant across the table -- the one modifier of
  an interaction table laid out in adjustment bands -- leaves the stub and
  names the stubhead, so a row reads "Least Deprivation" rather than
  "NDI quartile › Least Deprivation"

* A standalone column group sits before or after the body by its placement:
  P for interaction used to sort ahead of the odds ratios it tested whenever
  no column dimension applied to it

* Filtering a model table now keeps exactly the term definitions its remaining
  rows point at, and renumbers the formula matrix against the pruned table.
  A cell of the matrix is an ordinal naming which definition of its term a
  formula reads, so a term defined twice -- `race` as one family's exposure
  and another's modifier -- was pruned by role and membership alone: the
  other family's definition stayed when the bare term was a covariate here,
  and a definition an ordinal still pointed at could be dropped, which
  surfaced as `subscript out of bounds` inside `mdl_gt()` on a filtered
  table. Combining tables (`model_table(x, y)`, `vec_c()`, and a `mdl`
  vector fit from several families) re-points each side's ordinals at the
  merged term table the way `c()` on formulas already did; before, a
  combined table's rows could silently read another family's definition of
  a shared term

* `estimate_interaction()` leaves an aliased interaction coefficient (`NA`,
  from an empty cell of the exposure by modifier cross-classification) out
  of the joint Wald test with its degree of freedom, the way `anova()` drops
  aliased terms, instead of returning `NA` for the whole test; the
  within-level effect that cell would have formed stays `NA`. So is the
  within-level effect of a cell with no observations of the exposure level
  or its reference: the model returns a finite coefficient there, but after
  aliasing it measures another cell's contrast (a "No Info" insurance group
  absent from the least deprived quartile showed that quartile an odds ratio
  of 5.33 that was in fact the most deprived quartile's)

* A cell whose statistic is not finite renders as the missing text. A
  separated logistic fit exponentiates to an interval running to `Inf`, and
  the estimate beside it (`4857318.63`) is a finite number that means
  nothing; the cell used to print it

* `fit()` records the warnings a model raised while fitting on the model
  itself, `model_warnings()` lists them, and the model table's print says how
  many models carry one. Sixteen logistic fits on a sparse
  cross-classification raised more than fifty `glm.fit` warnings that
  scrolled past on the console and were invisible on a pipeline worker.
  `raw = TRUE` re-emits them, since a bare fit has nowhere to keep them

This development cycle works through Milestones 0–7 of [blueprint.md](https://github.com/shah-in-boots/recast/blob/main/blueprint.md), rebuilding the term, formula, fitting, collection, and table layers so the package feels fluid to play with, and documenting the result. Design decisions are recorded in `DESIGN.md`.

## Telling the story (Milestone 7)

* One vignette per grammar layer, in order: `vignette("terms")`, `vignette("formulas")`, `vignette("playing")`, `vignette("fitting")`, `vignette("mesa")` — each teaches its layer with runnable examples, building on the last

* `vignette("causal-reasoning")` is the intellectual home of the package: how the role vocabulary maps onto the estimands it exists to make fluent (total and direct effects, effect modification), with Hill (1965), Pearl (2010), VanderWeele and Robins (2007), and Figueiras et al. (1998)

* The README is rewritten around one dataset and one causal question — terms to table in a single narrative arc — replacing the old class-by-class tour; the development-log article now summarizes the blueprint's milestone arc alongside its original background and inspirations

* The stale `getting_started` vignette (describing a pre-Milestone-5 API) is retired in favor of the layer vignettes and the README

## The interface refinement pass, continued (Milestones 12 through 14)

* **Breaking table-layer rebuild:** `mdl_gt` is now an effect-and-presentation
  grammar compiled through effects, atomic measures, named cell groups,
  semantic layout, an explicit cell frame, and finally `{gt}`. Cell groups are
  keyed by stable id, so independent `add_*()` call order does not control
  placement; `place_cells()` moves groups among columns, statistic rows, and
  the semantic body; and `inspect_mdl_gt()` exposes every stage before render

* Strata, modifiers, outcomes, adjustment sets, terms, and contrasts are
  ordinary movable dimensions in one compiler. Conditional interaction
  effects and coefficient effects now share the same schema, formula metadata
  discovers every declared modifier, and group-scoped interaction p-values
  retain their full nested condition band

* The six procedural table files have been consolidated into four pipeline
  files: specification and verbs, semantic build, layout compilation, and
  rendering. Forest plots consume the same estimate measures as text cells;
  their axes are typed render metadata instead of magic semantic `.axis` rows

* One gesture per decision: `add_interaction()` now implies the `"interaction"` layout instead of requiring a separate `modify_layout(preset = "interaction")` call, and `add_events()` infers `followup` from a `Surv()` outcome's time argument, needed explicitly only for a plain outcome or an outcome an explicit `followup` still overrides

* The forest column now renders native to its table: a plot column turns the whole body borderless (the journal booktabs look — top rule, header rule, bottom rule, nothing inside) instead of hiding its own cell borders, which punched gaps in the header and bottom rules under CSS border collapsing; the axis strip is pinned to exactly the cells' width; the dashed reference line continues down into the axis; and `add_forest(axis = list(title = ))` draws an axis title beneath the tick labels

* The statistics vocabulary (known names, aliases, default headers) lives in
  one registry instead of hand-kept lists across the pipeline

* Every `mdl_tbl` now carries its family structure as three columns — `family` (the id grouping rows into one analysis), `pattern` (`sequential`, `parallel`, `mediation`, `direct`), and `relation` (`varied exposures`, the wide-table shape; `varied outcomes`) — recovered from the formulas' causal roles. They are derived, not supplied: recomputed automatically whenever the table is built or reshaped, so a subset that dissolves a `varied exposures` relation or renumbers the families is always reflected. A mediation triad binds into one family across its outcome boundaries. `keep_families()` pares by them; the standalone `identify_family()` function is gone (the identification is now an internal detail of the model table)

* **Breaking**: `fit_plan()` is renamed `plan_fit()` — the old name read as a fitting function; the new one says what it does, plan the fit (pre-release, so no deprecation cycle)

* A stratifying term missing from the fitting data is now an error at `plan_fit()` time; the zero-level expansion used to erase every model of its formula from the plan silently, so `fit()` returned an empty `mdl` vector with no warning

## The table grammar (Milestone 6)

* Tables are now grown, not configured: `mesa()` lays a fitted `mdl_tbl` out as a declarative specification, pipeable verbs refine it one decision at a time (`select_outcomes()`, `select_exposures()`, `select_terms()`, `select_adjustment()`, `select_strata()`, `modify_labels()`, `modify_layout()`, `modify_style()`, and the `add_*` column verbs), and `as_gt()` realizes it — verbs compose in any order, a repeated verb replaces its instruction with a message, and a bare `mesa(mt) |> as_gt()` already renders estimate + CI

* Every table reduces to one **cell frame** — a long tibble, one row per rendered cell — and the renderer consumes nothing else: spanners, merges by pattern, labels, stub indentation, alignment, and missing text are emitted from one place, and table regressions diff as plain tibbles in snapshot tests

* Column verbs: `add_estimates()` (estimate/CI/p, exponentiation deferred to the model-family inference by default), `add_n()` (the recorded `nobs` — no attached data needed), `add_events()` and `add_rate_difference()` (events, incidence rates, and the two-level rate difference from the attached data via `survival::pyears()`), `add_forest()` (a forest column any table can carry, drawn at render on one shared x-scale, with a working `invert`), and `add_interaction()` (effect-modification rows under the `"interaction"` layout, the across-levels p-value floating over each band)

* `modify_layout()` selects the launch presets — `"adjustment"`, `"levels"`, `"interaction"`, the shapes of the retired monolith tables; `modify_style()` generalizes the old accents (criteria on any statistic, e.g. `estimate > 1`; instructions beyond bold: italic, colors) and controls digits, missing text, and padding

* `estimate_interaction()` is generalized: categorical (not just binary) interaction variables, exact term matching by identity, the variance–covariance matrix indexed by coefficient name, and a joint Wald across-levels p-value for multi-level modifiers (#30 adjacent; Figueiras et al. 1998)

* Selection matches by identity, never substring: term `am` no longer selects `gam`, adjustment sets are the sequential model index within an outcome × exposure family (colliding term counts stay distinct), and categorical levels resolve through the attached data

* Defects fixed on the way, each with a regression test: the hazard tables displayed log-hazards labeled `HR (95% CI)` (the family inference now exponentiates); the rate-difference interval used `qnorm(0.9725)` where `qnorm(0.975)` belongs and ignored `person_years` (#30); the dichotomous gate `length(levels(x) == 2)` was truthy for any level count; `tbl_beta()` accents recognized only a `p <` criterion and hard-coded bold; `tbl_interaction_forest()`'s `invert` was dead code; the forest cells drew on `gt::ggplot_image()`'s fixed 5-inch canvas and squashed to sub-pixel invisibility — they now render at their true displayed size (`plot_image()`), with the interval caps pinned to the cell height and the reserved `.axis` row's stub label suppressed

* **Breaking**: the `tbl_*` monoliths — `tbl_beta()`, `tbl_dichotomous_hazard()`, `tbl_categorical_hazard()`, `tbl_interaction_forest()` — are deleted; their tables are documented grammar chains under `?mesa` (the package is pre-release, so they retire without a deprecation cycle)

* One replacement rule for every verb: `modify_style()` and `modify_labels()` now merge per-field/per-name like `modify_layout()` and the `add_*` blocks already did, so `modify_style(digits = 3)` no longer wipes accents recorded by an earlier call, and relabeling one term or column late no longer requires restating the rest; `mesa()` errors on unused arguments, and `modify_labels(columns = )` errors at realization when a name does not match a column on the mesa (previously a silent no-op)

## The model table (Milestone 5)

* Printing a `mdl_tbl` now reports the state of the analysis at a glance: how many models are fitted, failed, or awaiting `fit()`; which datasets are attached; the strata and subsets in play; then one readable line per model, ending with pointers to the next move (#18)

* `summary()` maps the fleet — models grouped by dataset, fitting function, outcome, and exposure with their adjustment ranges — lists the terms by causal role, and explains each failure with its error message

* New helpers: `model_failures()` (the attempted-and-errored models with their messages), `term_table()`, `formula_matrix()`, and `model_data()` (documented accessors for the table's attributes)

* `flatten_models()` infers exponentiation from each model's family and link — Cox models and log/logit/cloglog GLMs come back as ratios, `lm` stays linear — with an `exponentiated` marker column, a message when inference kicks in, and `exponentiate = TRUE/FALSE` (or `which =`) as explicit overrides; unfit rows are dropped with a message instead of silently

* Combining model tables is now trustworthy: `model_table(x, y)` combines tables directly, attached datasets survive combination (#26), formula matrices stay parallel to the table's rows, and term tables deduplicate left-most-wins

* `dplyr` verbs reconcile the table's attributes (#23): `filter()`, `arrange()`, `slice()`, `mutate()`, and `[` prune the formula matrix, term table, and data list down to the remaining models (stale strata and role entries are removed — the old #26 symptom); dropping an invariant column returns a plain `data.frame` with a message naming the columns, and `bind_rows()` across unrelated tables points back to `model_table()`

* `model_table()` validates its inputs: raw fitted models are rejected with directions toward `fit(..., raw = FALSE)` or `mdl()` (#46), every construction runs `validate_model_table()`, and the invariant columns are documented in `?model_table`

* A `number` column (the count of right-hand-side terms, i.e. the adjustment degree) is now part of every table; `level` and other provenance columns are type-stable so tables from different datasets combine

* Naming convention settled: spelled-out names are canonical for the public API (`model_table()`), abbreviated forms remain as documented aliases (`mdl_tbl()`) and as the class name

## New features

* `set_data()` stamps `type`, `distribution`, and observed `level`s onto `tm` and `fmls` objects from a dataset, making strata and interactions data-aware before anything is fit (#5)

* Fluent verbs for playing with formula families: `add_strata()`, `remove_strata()`, `add_terms()`, `remove_terms()`, `swap_outcome()`, and `subset_data()` — each pipeable, each returning a modified `fmls`

* Random effects joined the role vocabulary: `.r(id)` (or lme4-native `(1 | id)`) parses, prints, rebuilds as `(1 | id)`, and fits through `{lme4}` with `{broom.mixed}` tidiers

* `fit()` accepts a fitting function, its name, or a `{parsnip}` model specification, resolved by name rather than position; the hard-coded model whitelist is retired

* `plan_fit()` exposes the fitting plan — formula x stratum x subset — as an inspectable table before anything runs

* Fitting fails softly: one failed model records its error (`fit_status = FALSE` in a `mdl_tbl`) instead of sinking the batch

* Subset instructions ride the plan: `subset_data(f, am == 1)` fits the family per subset and lands as a `subset` provenance column in the model table

* Patterns are an open registry: `register_pattern()` makes user-defined expansion patterns available to `fmls()` by name; `formula_patterns()`, `term_roles()`, and `term_transformations()` expose the vocabularies

* `fmls` families combine with `c()`; conflicting term definitions resolve left-most-wins with an explicit message (#42)

* Printing a `fmls` now leads with a deck summary: formula count, outcomes, exposures, strata (with levels), random effects, and subsets

* Term and formula printing now uses `cli` named ANSI colors by role, with `recast.color` for user control

## Fixes

* The `tm.formula()` parser is a recursive walk of the formula syntax tree, replacing positional `all.names()` scanning; nested runes like `.x(log(x))` now parse correctly, and group tiers accept multiple digits (`.g10`)

* Grouped covariates in the parallel pattern were dropped for every tier except zero; they now stay together (was `group == 0L`)

* `fit()` no longer misreads `.fn` when arguments are supplied in a different order

* Labeling formulas with vector values (e.g. `am ~ c("Manual", "Automatic")`) now evaluate properly instead of deparsing to a string

* `degrees_freedom` follows each model family's own accounting (`df.residual()` with a fallback) instead of an `lm`-shaped guess

* The unfinished `apply_rolling_interaction_pattern()` stub was removed; it will return as a registered pattern

## Housekeeping

* Internal vocabularies moved from `sysdata.rda` into `R/vocabulary.R`; `{lifecycle}` declared; `{survival}` moved to Suggests behind guards; `{parsnip}`, `{lme4}`, and `{broom.mixed}` added to Suggests

* Author-only tests against private datasets moved to `tests/manual/`; R CMD check runs clean

* Renamed package from `{rmdl}` to `{mesa}`, then to `{epigram}`, and now to `{recast}` — a verb for what the package does to variables (casting them into causal roles) and to models (melting them down and pouring them again). The color option follows as `recast.color`; the old `epigram.color` and `mesa.color` names still work as fallbacks.

* Remove additional imports, e.g. `{janitor}`, with bespoke function rewrites, to help decrease dependency burden

* Updated package title as software has evolved

# mesa 0.1.0

This first CRAN release contains the basic functions for the package, and introduces the new basic classes. 

* `tm` gives variables in formulas specific roles and behaviors (vectorized)

* `fmls` expands the base formula class into a list of related formulas (vectorized)

* `mdl` are a thin wrapper (vectorized) for statistical models, with important metadata maintained, and are used to generate `mdl_tbl` objects, which serve as a reference `data.frame` of a family of modeling objects
