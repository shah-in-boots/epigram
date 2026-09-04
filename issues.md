# Issues

Working list for pre-release repair, ordered by urgency. Found during the
codebase review that produced `vignettes/articles/usage.qmd` and
`vignettes/articles/internals.qmd`; file references are approximate anchors.
Items verified by execution are marked ✓.

## High — wrong results or broken contracts

- [x] ✓ **`apply_fundamental_pattern()` violates the pattern contract**
  ([patterns.R:106](R/patterns.R#L106)). *Fixed 2026-07-09*: the pattern now
  returns the documented `outcome`/`exposure`/`covariate_1` columns — the
  exposure keeps its key-pair column, every other RHS term rides one row as
  the single covariate — so `check_groups()` shields the outcome again.
- [x] ✓ **Fundamental pattern turns strata into covariates**
  ([patterns.R:133](R/patterns.R#L133)). *Fixed 2026-07-09*: documented as
  intentional decomposition. `fmls(pattern = "fundamental")` demotes meta
  terms (strata, random) to plain predictors *before* expansion, with a
  message, and the demotion is recorded in the family's term table.
- [x] ✓ **Strata/random leak when families combine**
  ([formulas.R:600](R/formulas.R#L600)). *Fixed 2026-07-09*: meta terms are
  now recorded in the formula matrix like any other member, and
  `formulas_to_terms()`/`key_terms()` read membership alone — a stratum only
  applies to the family that declared it. `add_strata()`/`remove_strata()`
  write the matrix membership too.
- [x] **Interaction layout collides when several models share an interaction
  term**. *Superseded 2026-07-14*: conditional effects use the ordinary effect
  schema; adjustment, stratum, modifier, and modifier-level dimensions pass
  through one compiler. Formula metadata discovers every modifier even though
  the scalar `mdl_tbl$interaction` convenience column mirrors only the first.
- [x] **`estimate_interaction()` cannot handle a categorical exposure**
  ([interaction.R:113](R/interaction.R#L113)). *Fixed 2026-07-09*: a factor
  exposure resolves through its `exposureLEVEL` keys; one row-set per
  non-reference exposure level comes back with an `exposure_level` column,
  and the joint Wald p-value spans all interaction coefficients. The
  interaction *layout* still shows one contrast (a two-level exposure) and
  errors otherwise.
- [x] **Two definitions of "categorical"**. *Fixed 2026-07-09*: binary and
  categorical are different definitions now. `classify_distribution()`
  stamps a numeric 2-valued column (e.g. a 0/1 survival outcome) as
  `binary`, keeping the continuous type — matching how the model treats it —
  and only factor-like columns are categorical. Strata get their levels
  stamped regardless of type. `add_events()` still requires a factor, and
  its error now says how to convert.

## Medium — design friction and visible quality

- [x] **Forest column reads as pasted-on, not native**
  ([table-render.R:376](R/table-render.R#L376)). Three render defects,
  each verified against the usage-vignette interaction example
  (2026-07-10). (1) *Uneven row and rule lines*: the plot cells' `gt::
  cell_borders(style = "hidden")` meets `border-collapse: collapse`, where
  CSS gives `hidden` top priority on a shared edge — so the forest column
  punches gaps in the header rule and the table's bottom rule while every
  other column keeps its 1px row hlines. (2) *Axis strip width drifts off
  the cells' scale*: `plot_image()` pins only `height:` in em and lets the
  width follow the SVG's intrinsic aspect ratio, but `grDevices::svg()`
  rounds the canvas to whole points (cells 22.5 → 22pt, axis 16.5 → 16pt),
  distorting the two aspect ratios differently, so the axis renders a
  different width than the cells above it. (3) *The reference line dies at
  the last row*: `draw_forest_axis()` draws no intercept line, so the
  dashed vline stops instead of running down into the axis. *Fixed
  2026-07-10*: `plot_image()` pins both `width:` and `height:` in em from
  the requested pixel size; the hidden-border style is gone — a plot
  column now defaults the *whole body* borderless
  (`table_body.hlines.style = "none"`, quiet `row_group.border.*` and
  `stub.border.*`), the booktabs look journals use, so every column is
  even and the outer rules run continuously across the forest column; the
  dashed intercept carries into the axis strip; and the block grew
  `axis = list(title = )`, an axis title drawn beneath the tick labels
  (the titled strip takes 34px against the untitled 22). Remaining
  follow-on polish: "favors left/right" annotations via the block's
  `axis` options.
- [x] ✓ **Strata is invisible in every formula representation**
  ([terms.R:719](R/terms.R#L719)). *Fixed 2026-07-09*: the printed family
  (`format.fmls`) now annotates meta terms in their declared rune form —
  `.s(am)`, `.r(id)` — after the right-hand terms. `formula()`,
  `formula_call`, and the model's recorded call deliberately stay the true
  fitted (unstratified) formula: they are consumed programmatically
  (re-fitting, term counting), so annotating them would corrupt real syntax.
  The engine-native strata question is decided below: `strata()` is
  conditioning and passes through; `.s()` is the data split.
- [x] **`fit()` defaults to `raw = TRUE`**. *Fixed 2026-07-09*: the default
  is `raw = FALSE` — a `mdl` vector, the grammar's main path — and the
  vignettes no longer repeat the argument. `raw = TRUE` remains the opt-out
  for a quick look at the plain fits.
- [x] ✓ **`formula_index` column is not an index**. *Fixed 2026-07-09*:
  dropped. The formula matrix's rows stay parallel to the table's rows, so
  a row-index column carried no information; nothing consumed it.
- [x] **Forest cells are fixed-size PNGs**
  ([table-render.R:432](R/table-render.R#L432)). *Fixed 2026-07-09*: cells
  draw as inline vector SVG (each its own base64 `data:` document, so glyph
  ids cannot collide), sized in `em` units so they scale with the text
  beside them. A build without cairo falls back to the old PNG path.
- [x] **Group-scoped cell mask is white text**. *Fixed 2026-07-09*: the
  rowspan emulation blanks duplicates with a `gt::text_transform()` — a
  content substitution rather than styling, which holds on dark themes and
  on every output format (LaTeX/RTF/Word).
- [x] **`mesa()`'s one-family check compares `model_call` strings only**.
  *Fixed 2026-07-09*: the check now folds in each model's recorded link
  (`summary_info$model_link`), so two `glm`s on different links error as
  `glm (logit)` vs `glm (identity)`.
- [x] **`selection_data()` picks the first referenced dataset**. *Fixed
  2026-07-09*: `selection_data()` returns all attached datasets in
  reference order, and `resolve_term_metadata()` stamps each term's levels
  from the first dataset that carries its column — models spanning several
  datasets each find their own terms stamped. (When two datasets share a
  column name with different levels, the first-referenced one still wins:
  the term table is table-wide by design.)
- [x] **Silent exponentiation skip in the interaction realizer**. *Fixed
  2026-07-09*: a scale flag that does not resolve to one decision per
  interaction term errors, pointing at `add_estimates(exponentiate = )`,
  instead of falling silently to the linear scale.
- [x] **Two `mdl()` construction paths in `fit.fmls`**. *Fixed 2026-07-09*:
  one shared context (formulas, data name, strata, subset) feeds both
  paths; an error only swaps the model object for its message.
- [x] **`_pkgdown.yml` articles index references `causal-reasoning`**.
  *Fixed 2026-07-09*: the references are dropped (`_pkgdown.yml` and the
  `mesa.Rmd` closing pointer, which now sends readers to the terms
  vignette). Writing the article remains an option for later.
- [x] **`attach_data()` matches by deparsed name**. *Fixed 2026-07-09*: an
  inline expression passed as `data` now takes a stable content-derived id
  (`data_<hash>`) at `fit()`, `model_table()`, and `attach_data()` alike, so
  identical content meets itself; and a frame arriving under a different
  name is aliased — with a message — to the one referenced-but-detached
  `data_id` whose models' variables it fully carries. An explicit `name = `
  always wins.

## Low — parsimony and clarity

- [x] **Role-extraction block copy-pasted 6×**. *Fixed 2026-07-09*:
  `pattern_roles()` pulls every role once and `key_pair_grid()` replaces the
  outcome × exposure ladder; the patterns and `check_mediation()` draw from
  them, and `check_groups()` (which never used the roles) dropped the pulls.
- [x] **`construct_table_from_models()` / `construct_table_from_formulas()`
  are ~80% identical**. *Improved 2026-07-09*: the four near-identical
  role-pulls in each collapsed into shared `role_term()` /
  `interaction_term()` helpers. The two constructors themselves stay
  separate — their sources (fitted `mdl` fields vs a bare `fmls` matrix)
  differ enough that a full merge would obscure more than it saves.
- [x] **`mdl.lm` / `mdl.lmerMod` are ~80% identical**. *Fixed 2026-07-09*:
  both are thin wrappers over `new_model_from_fit()`, each computing only
  its own tidy/glance pieces (and the S4 call slot).
- [x] **Formula matrix built via `table() |> rbind()` per row**. *Fixed
  2026-07-09*: membership (0/1) is built directly from each precursor row's
  unique non-`NA` terms, and the downstream membership tests read `>= 1`.
- [x] **`my_tidy()` exposes an `exponentiate` argument it ignores**. *Fixed
  2026-07-09*: parameter dropped; exponentiation stays deferred to
  `flatten_models()`, and the docstring says so.
- [x] **`rhs.formula()` splits deparsed text on `+|-`**. *Fixed
  2026-07-09*: `split_additive()` walks the expression tree, so `I(a + b)`
  stays one term and labels containing `+`/`-` (e.g. `"Weight (+/- SD)"`)
  stay whole; `lhs.formula()` uses the same walk.
- [x] **`apply_sequential_pattern()` generates all 2^n rows then culls**.
  *Fixed 2026-07-09*: the n+1 covariate prefixes are built directly (the
  bare key-pair row only when an exposure anchors it).
- [x] **`format.fmls` brace/indent confusion**. *Fixed 2026-07-09*: both
  branches now select their sides and one shared tail formats and returns.
- [x] **Duplicated section header** in table-render.R. *Fixed 2026-07-09*.
- [x] **`frame_context(dec, spec)` never uses `dec`**. *Fixed 2026-07-09*:
  the parameter is gone; `frame_context(spec)`.
- [x] **Dead role pulls** in the direct, sequential, and parallel patterns.
  *Fixed 2026-07-09*: subsumed by `pattern_roles()` — each pattern reads
  only the roles it uses.

## Decided — the former open design questions (2026-07-09)

- [x] **Engine-native strata is conditioning; `.s()` is the data split**.
  The two constructs work differently and both stay available:
  `strata(x)` (bare or `survival::`-qualified) *conditions within* one
  model, so it passes through the formula untouched — whole, as one term,
  which the engine consumes — traced as `transformation = "strata"`.
  `.s()` remains the grammar's own stratum: an actual segregation of the
  data, one fit per level. Neither is rewritten into the other. (An
  earlier convert-to-`.s()` approach was walked back 2026-07-09: the
  mechanisms are not interchangeable.)
- [x] **The effect is the semantic unit**. *Rebuilt 2026-07-14*: outcome ×
  focal term/contrast × model context, optionally conditioned by stratum and
  modifier levels. Atomic measures declare their grain; stable cell groups
  present them; `modify_layout()` moves semantic dimensions and
  `place_cells()` moves groups without recomputation.
- [x] **Attached data attaches whole, at the `mdl_tbl` level only**. The
  full frame is retained: later work routinely reaches for columns no
  current formula names (an `add_events()` follow-up column, a variable
  for the next family of models added to the same table), so pruning to
  referenced columns was tried and walked back 2026-07-09. What stands is
  the layering: `set_data()` on a `tm` or `fmls` only *teaches* (stamps
  type/distribution/levels) and retains nothing, since formulas stay
  abstract and source data keeps evolving; the `mdl_tbl` — where formulas
  and data come together — is the one layer that retains data.

## Family identity (2026-07-10)

- [x] ✓ **A stratum missing from the data silently erased its models**
  ([fit.R:204](R/fit.R#L204)). `data[[strataVar]]` on a missing column
  returned `NULL`, the stratum table expanded to zero rows, and
  `expand_grid()` dropped the formula's every model from the plan —
  `fit()` returned `<model[0]>` with no message. *Fixed 2026-07-10*:
  `plan_fit()` errors when a stratifying term is not a column of `data`,
  pointing at `remove_strata()`; without `data` the plan still forms with
  unresolved levels. Regression test in test-fit.R.
- [x] **`fit_plan()` renamed `plan_fit()`** (2026-07-10). The old name read
  as a fitting function; the new one says what it does — plan the fit.
  Pre-release, so renamed without a deprecation cycle.
- [x] **`identify_family()` recovers family structure from causal roles**
  ([family.R](R/family.R), 2026-07-10). A `fmls` is born as one family but
  `c()` records no lineage; downstream, `family_adjustment_index()` derives
  families as outcome × exposure groups — which misfiles a mediation triad
  (its `mediator ~ exposure` member has a different left-hand side).
  `identify_family()` reads the roles directly: formulas group by outcome ×
  exposure, a mediator binds its triad across that boundary, adjustment
  sets decide the pattern (`sequential` when nested, `parallel` when not,
  `direct` for a lone formula, `mediation`), and families sharing an
  adjustment-ladder signature relate as `varied exposures` (same outcome —
  the wide-table shape) or `varied outcomes` (same exposure). Strata ride
  along without splitting the family; `data` stamps their observed levels.
- [x] **Wire family identity into the table layer** — *the mdl_tbl → mdl_gt
  handoff restructured 2026-07-11*. The division of labor is now: **the
  `mdl_tbl` decides which models; the `mdl_gt` decides how they show.**
  - `identify_family()` gained a `mdl_tbl` method ([family.R](R/family.R)):
    the table's formula matrix + term table are reconstructed as a `fmls`
    (rows parallel, stratum-expanded rows repeating their formula's row)
    and the identification is *stamped on* as ordinary `family` / `pattern`
    / `relation` columns, so paring is plain `dplyr::filter()`. The
    print method surfaces the stamp (family/pattern columns, a relations
    context line, a families count in the header).
  - `mdl_gt()` is the gate: after the fn/link check it verifies the fitted
    rows form *one presentable analysis* — a single family, or several
    families sharing a relation (`varied exposures` / `varied outcomes`,
    the wide-table shapes) — errors otherwise naming the families and
    pointing at `identify_family() |> dplyr::filter()`, and records the
    verified structure as `spec$family` (shown by `print.mdl_gt`).
  - `select_outcomes()` / `select_exposures()` / `select_strata()` retired
    (pre-release, no deprecation cycle): model narrowing is the table's
    job. `select_adjustment()` and `select_terms()` remain the granular
    *display* selections; outcome labeling moved to `modify_labels()` (an
    outcome name relabels its row group; a mediator name relabels both its
    term and its outcome appearances). `resolve_selection()` slimmed to
    terms + adjustment.
  - *Still open, in order of value*: a family id carried from `fmls`
    through `fit()` so lineage survives `c()` without recomputation; a
    `mediation` layout preset; auto-choosing the layout preset from the
    verified pattern.

- [x] **Adjustment alignment keyed by set identity; causal paring verbs**
  — *2026-07-11*.
  - `family_adjustment_index()` (positional: order-by-`number` within an
    outcome × exposure family) is replaced by `adjustment_set_index()`
    (now in `table-build.R`): each *distinct covariate
    set* is one rung, numbered by set size then order of first appearance,
    table-wide. Models carrying the same covariates share a rung wherever
    they sit, so related families' rows align on the mesa by the actual
    adjustment — two parallel families built in opposite row orders no
    longer cross-pair (regression coverage in `test-table-build.R`).
  - The `mdl_gt()` gate tightened accordingly: several families must share
    a relation over **one** ladder (`ladder_signature()`); two
    varied-exposure pairs on different ladders — which share the relation
    label — now error as several analyses side by side.
  - `adjustment_sets()` (mdl_tbl and mdl_gt methods) *shows* the rungs: one
    row per set — the `select_adjustment()` index, the covariates, what
    each rung adds over the one below (when nested), model and family
    counts — so the user never has to remember the ladder they built.
  - Paring is a *family of verbs* speaking the causal language, one per
    dimension (the single argument-loaded `keep_models()` was tried
    2026-07-11 and replaced 2026-07-12 — a speakable pipe is more
    informative): `keep_outcomes()` / `keep_exposures()` and their
    `drop_*()` complements (causal role); `keep_families(ids, pattern =,
    relation =)` (identified structure — ids require the
    `identify_family()` stamp the user has seen; pattern/relation identify
    on the spot); `restrict_to(strata =, level =, subset =, data =)` (the
    population, in the epidemiologic sense of restriction); `adjusting_for()`
    (models whose adjustment set carries the named covariates); and
    `excluding()` (models whose formulas avoid the named terms entirely —
    setting aside a mediator or collider). All exact-matching, bare names
    or strings; every requested value is validated against what the table
    holds (a typo errors with the available values); each verb messages
    what it kept; `dplyr` verbs still work. One shared doc page
    (`?paring`, model-table-helpers.R; the topic and internals were named
    `whittling` for a day — renamed to *paring* 2026-07-13).

## Parsimony pass (2026-07-10)

- [x] **Single-use internal helpers inlined at their call sites.** A sweep
  of every internal function's call count found ~30 helpers called exactly
  once, most under 25 lines. Inlined (their doc comments kept as plain
  comments): the `message_*()`/`warning_*()` wrappers (output.R is gone;
  `has_cli()` moved to utils.R), `validate_classes()`, `new_mesa()` and its
  four `default_*()` slot builders, `table_statistic_names()`/`_aliases()`,
  `accent_style()`, `pe_or_na()`, `model_link_function()`,
  `model_degrees_freedom()`, `validate_model_table()`,
  `model_table_reconstructable()`, `data_expression_name()`,
  `model_table_nobs()`, `infer_exponentiation()`, `data_id_candidates()`,
  `infer_followup_column()`, `outcome_event_column()`,
  `expand_term_keys()`, `selection_data()`, `key_to_level()`,
  `row_qualifier()`, and `classify_distribution()`. Dead code removed:
  `check_classes()`, `message_empty_models()`, `message_formula_to_fmls()`.
  *Deliberately kept*: the named pipeline stages (`parse_formula_terms()` →
  `demote_orphan_roles()` → `expand_shortcut_interactions()` → ... in
  terms.R; the realize/lay-out/render stages), S3 methods, vctrs
  cast/ptype2 boilerplate, and multi-use utilities — single-use but
  load-bearing units like `draw_forest_cell()` and `apply_group_scoped()`
  stay because inlining them would bloat their callers past reading.
- [x] **Test suite trimmed of cosmetic and duplicated tests.** Removed the
  ANSI-palette assertion test (the `format(color =)` behavior test stays),
  a duplicate order-independence render test, a duplicate `print.mesa`
  block-description test, an empty test, a trivial row-count test, the
  internals-only `my_tidy()` smoke test, and merged the two overlapping
  `fmls`-combination tests. The rest encode one grammar promise each and
  stay.

## Found building the anthracycline interaction report (2026-09-01)

First real use of the interaction grammar on a manuscript revision: five
exposure × modifier pairs on incident AF (n = 354, 64 events), each fit
unadjusted and fully adjusted through `fmls()` → `fit()` → `model_table()`,
then laid out per pair with `mdl_gt() |> add_interaction() |> add_n() |>
select_adjustment() |> add_estimates() |> modify_labels() |> as_gt()` and
rendered to a typst PDF from a `{targets}` pipeline. The chain is about ten
lines per table and the within-level odds ratios and joint Wald test match a
by-hand Figueiras computation to the third decimal. What follows is what
broke or read wrong on the way. Fixed items carry a regression test and a
NEWS entry.

### Fixed

- [x] ✓ **A modifier was attributed to a model by role alone**
  ([table-build.R](R/table-build.R), `model_modifier_terms()`). The term
  table is table-wide, so with `.x(age) + .i(sex) + race` beside
  `.x(age) + .i(race) + sex` each family's modifier is the other's covariate;
  the first model was sent after an `age:race` coefficient it never had and
  the render died inside `estimate_interaction()`. A modifier now belongs to
  a model only when the crossed component is a member of that model's
  formula.
- [x] ✓ **A selected rung without the interaction term vanished silently.**
  `select_adjustment(1 ~ "Unadjusted", ...)` on a sequential ladder, where
  rung 1 is the bare exposure, produced a table with the "Unadjusted" band
  simply absent. Refused now with the rung named when the rung (or an
  exposure with no `.i()`) was asked for; set aside with a message on a
  bare mesa, so exploration over a whole ladder still lays out what it can.
- [x] ✓ **`nobs` within a stratified model counted the whole frame.** Every
  stratum reported 11 / 7 / 14 for `mtcars` `cyl` under `.s(am)`, where the
  strata hold 3 / 4 / 12 and 8 / 3 / 2. Counts now come from the model's own
  stratum; under a `subset_data()` rule they are `NA`, since only the rule's
  name is recorded on the model (see the open item below).
- [x] ✓ **The exposure's reference level was invisible in the interaction
  layout.** It is now a contrast column rendered as `reference_text`
  ("1.00 (ref)" in the report), so the table names its comparison.
- [x] ✓ **N sorted after every contrast column under a spanner.** A group
  lacking the inner column dimension now sits at the near edge of it, so N
  leads and P trails within each adjustment band.
- [x] ✓ **Ordered factor → polynomial contrasts → opaque refusal.** NDI was
  an `ordered` factor, so the model carried `ndi.L/.Q/.C` and no level key
  could match; the error said only that it could not find the key. It now
  names the ordered factor as the cause and the `factor(x, ordered = FALSE)`
  refit as the fix. (The same coding is why the original report's
  Supplemental Table 4 shows `ndi.L/.Q/.C` rows, which the manuscript read as
  quartile odds ratios; the interaction report carries a note.)
- [x] ✓ **A bare interaction mesa over two rungs (or strata) errored on a
  collision.** The `"interaction"` preset maps modifier and contrast only.
  An undeclared layout now takes the varying `adjustment` (and `stratum` /
  `stratum_level`) as the outer *row* bands by itself -- rows, because the
  side-by-side version put eleven columns on a portrait page and typst wrapped
  every interval mid-number and ran "1.00 (ref)" into the next cell. A
  declared layout without the dimension still errors naming it.
- [x] ✓ **Stub read `NDI quartile › Least Deprivation` with adjustment on
  rows.** A constant inner row dimension now leaves the stub and names the
  stubhead.
- [ ] **The floating P lands on a level row in print.** The mid-band float
  is the intended reading (kept, per Anish, 2026-09-03), but typst and Word
  drop the vertical alignment Quarto's table conversion cannot carry, so on
  a four-level band the value sits on the second row's baseline as if it
  belonged to that level. A typst-native emitter with a real `rowspan`
  would place it properly; until then the caption has to say what the
  column is.
- [x] ✓ **A standalone P sorted ahead of the odds ratios it tested.** Every
  standalone column group sorted before the body; they now sit before or
  after it by their placement order.
- [x] ✓ **Filtering a combined table left formula-matrix ordinals pointing
  past the pruned term table** ([model-table.R](R/model-table.R),
  `df_reconstruct()`). Cells are definition ordinals (`resolve_formula_row()`)
  but pruning went by role and membership, so `race` defined twice (exposure
  in one family, modifier in another) kept the wrong definition and
  `mdl_gt()` failed with `subscript out of bounds` on the third pair. Pruning
  now keeps exactly the definitions the remaining rows point at and remaps
  the ordinals. The combine paths (`mdl_tbl_ptype2()`, `mdl_tbl_cast()`,
  `construct_table_from_models()`) never remapped either, so a combined
  table's rows could silently read another family's definition of a shared
  term; they remap now, as `c.fmls()` already did.
- [x] ✓ **Joint Wald test returned `NA` when one product term was aliased.**
  No "No Info" patient in the least deprived quartile made
  `insuranceNo Info:ndiMost Deprivation` `NA`, and `solve()` voided the whole
  test. Aliased coefficients leave the test with their degree of freedom, as
  `anova()` does; their within-level effect stays `NA`.
- [x] ✓ **Separated cells printed as `4857318.63 (0.00, Inf)`.** A
  non-finite statistic now renders the cell as the missing text.
- [x] ✓ **Fit warnings were lost.** Sixteen logistic fits raised more than
  fifty `glm.fit` warnings — almost all from `confint()` profiling inside
  `broom::tidy()`, not from the fit itself — that scrolled past on the
  console and were invisible on the crew worker. `fit()` now records them on
  the model (one line per distinct message with a count), `model_warnings()`
  reads them back, the model table's print counts the models that carry any,
  and `raw = TRUE` re-emits them.

### Open — design questions

- [ ] **Sequential pattern with `.i()` puts the pair on a rung of its own.**
  `fmls(y ~ .x(a) + .i(b) + c, pattern = "sequential")` gives `y ~ a`,
  `y ~ a + b + a:b`, `y ~ a + b + a:b + c`. For an interaction analysis every
  rung should carry the pair; the bare-exposure rung has nothing for
  `add_interaction()` to show (hence the refusal above). `test-formulas.R`
  encodes the current count with a "?" in its comment. Proposal: interaction
  terms ride with the key pair in every rung of `sequential` and `parallel`,
  the way strata do. Worked around by combining two `direct` families
  (minimal, full) with `c()`, which `adjustment_sets()` numbers 1 and 2.
- [x] ✓ **`estimate_interaction()` should return the test's degrees of freedom**
  (and how many aliased terms it dropped), as columns or attributes, so a
  table can say "joint Wald, 6 df" without recounting levels by hand. *Fixed
  2026-09-03*: `statistic` and `df` columns beside `p_value`; the aliased
  count is the difference between `df` and the product terms the levels
  imply, and the rows those terms would have formed are `NA`.
- [ ] **`add_events()` wants a `Surv()` outcome or follow-up.** A binary
  outcome should give events / N per level with no follow-up; the report's
  events-per-cell tables were built by hand with `dplyr` + `gt`.
- [ ] **A pare verb for the modifier.** `mdl_gt()` needs one pair at a time,
  reached with `dplyr::filter(exposure == , interaction == )`; a
  `keep_modifiers()` beside `keep_exposures()` would let the pare read like
  the rest of the grammar.
- [ ] **PDF and Word deliverables.** The report went to typst through
  Quarto's HTML-table conversion, which carries the text cells and two-level
  spanners but cannot carry `add_forest()`'s inline SVG. A non-HTML fallback
  (a ggplot figure emitted beside the table) is needed before the forest
  column is usable in a manuscript.

### Open — layout polish (all seen on the report's tables)

- [ ] `place_cells(.before = "effect")` is silently ignored when the anchor
  group sits on another axis (the effect group lives in the body); an anchor
  off-axis should error. (`.before = "body"` now takes effect for column
  groups through the sort-edge rule above.)
- [ ] `subset_data()` records only the rule's name on the model, so
  `estimate_interaction()` cannot recover the rows for its counts (now `NA`
  there). Recording the rule's expression in `dataArgs` would let counts,
  events, and the empty-cell check work under subsets.
- [ ] Default `reference_text` is `""`, so a reference row or column is a
  blank cell unless `modify_style(reference_text = )` is set. Journals want
  "1.00 (ref)" or "Reference"; a non-empty default would bring the bare mesa
  closer to publication-ready.
- [ ] The renderer has no source note. A cell blanked for a non-finite or
  aliased estimate looks the same as an empty cell; an automatic note
  ("— not estimable") would let the table stand without its caption.
- [ ] Condition-grain groups (`n`) repeat under every adjustment spanner
  though identical across rungs; they should be placeable once, outside the
  spanner band.
- [ ] `place_cells("p", axis = "rows")` labels the P row's column with the
  adjustment label rather than "P for interaction".
- [ ] A statistic constant over an *outer* row dimension but varying over an
  inner one cannot be placed: with `rows = c("modifier_level", "adjustment")`
  the P for interaction (one per rung, the same for every level) fell out as
  a dash in every row, since the compiler scopes a group-level value only to
  an outer band. Broadcasting it into every band would make the
  level-outer / rung-inner layout usable; it is the most compact of all.
- [ ] Typst column widths are equal by default and `gt` widths do not
  survive Quarto's HTML-table conversion, so intervals wrap mid-number
  (`1.43 (0.08,` / `27.18)`) and a stub as long as "Below Average
  Deprivation" takes three lines beside single-number columns. A
  non-breaking space inside the interval was tried and reverted: where the
  column is narrower than the interval, typst overflows into the neighbour
  (`1.00 (ref) 2.20 (0.20, 24.29)` ran together), which is worse than the
  wrap. The fix is column widths -- a typst-native emitter, or Quarto's
  `tbl-colwidths` on the chunk.
- [ ] `fmls()` messages "Interaction term `x` was applied to exposure term
  `y`" once per formula: sixteen lines on a sixteen-model pipeline. Once per
  family, or a `quiet` argument.
- [ ] The formula matrix of `c()`-combined families carries `NA` (not 0) for
  a term absent from a family; `merge_formula_matrices()` zero-fills but the
  `c.fmls()` → `model_table()` path does not. Two membership tests are now
  `isTRUE(x >= 1)`-guarded; the matrix should be normalised at construction
  instead.
- [ ] `estimate_interaction()` takes its critical value from `qt()` on the
  residual degrees of freedom for every model family. For a binomial `glm`
  the dispersion is fixed and the Wald reference is normal, so the interval
  is slightly conservative (1.967 against 1.960 at 342 df here — the third
  decimal of the hand check above). It should follow the family: `t` for
  `lm`/gaussian, `z` otherwise, as `confint()` and `broom` do.
- [x] ✓ **An empty cell at the modifier's reference level showed another
  cell's effect.** "No Info" insurance had no patient in the least deprived
  quartile; `glm()` aliased `No Info:Most Deprivation` instead, so the
  `No Info` main effect became the contrast *within the most deprived
  quartile* and `estimate_interaction()` labelled it least deprived: a real
  odds ratio (5.33, exactly 5/5 vs 9/48) under the wrong row. A cell with no
  observations of the exposure level or its reference is now `NA`.
- [ ] **Aliasing still shifts the parametrization of the remaining rows.**
  With one product term aliased, the identified within-level contrasts could
  be recovered from the estimable functions (or by refitting with the empty
  level dropped) rather than reported as `NA` beside rows whose meaning has
  moved. Worth doing before the interaction grammar is trusted on sparse
  tables without a hand check.

## Found writing the interaction report as a document (2026-09-03)

Eight findings from using the tables in the manuscript itself, all
reproduced against the code. Fixed items carry a regression test and a NEWS
entry.

- [x] ✓ **A separated fit printed a number.** The render step blanked a cell
  whose interval ran to `Inf`, but `add_estimates(view = "separate")` hands
  the estimate to its own column without the interval, and on current R
  `confint()` on a separated `glm` returns `NA` for the limit it cannot
  find, not `Inf`, so `13203562.47` stood alone. The estimate, interval, and
  Wald p are blanked together at the build stage
  (`realize_mdl_gt_effects()`), where all three are still one row.
- [x] **`add_strata(fms, i)` captured the symbol `i`.** Documented, not
  changed: `!!i`, `!!!vars`, and strings already worked through
  `rlang::ensyms()`. Evaluating a bare name that happens to be bound in the
  workspace was considered and rejected as magic.
- [x] ✓ **Strata could not be labelled.** `modify_labels()` stored the label
  and nothing read it: the effect frame had no `stratum_label`. It does now,
  `apply_effect_labels()` is one loop over term / modifier / stratum, and
  `dimension_label()` reads the label column for both stratum dimensions.
  Strata also plan and display in level order rather than order of first
  appearance, so a vector of level labels applies predictably.
- [x] ✓ **`fit_status` was `TRUE` for an unestimable model**, and there was
  no supported route to per-model diagnostics. `model_diagnostics()`
  replaces `model_failures()` and `model_warnings()`: every model with
  `status`, `nobs`, `terms`, `aliased`, `unbounded` (a limit at infinity or
  one the profile never found), `max_std_error`, `warnings`, `error`.
  `fit_status` stays the error flag it is.
- [x] ✓ **A continuous modifier died inside LAPACK.** Worse than reported:
  the within-level arithmetic assumes a factor or 0/1, so a modifier coded
  1/2 returned wrong numbers silently. `estimate_interaction()` now refuses
  any numeric modifier not coded 0/1, naming it. The `lung$sex` test fixture
  was coded 1/2 and only checked column names.
- [x] ✓ **`add_strata()` silently repurposed a covariate.** One `message()`
  naming the term and its old role.
- [ ] **Joint test is Wald only.** A likelihood-ratio option needs a refit,
  since only the tidy blueprint of a model is stored. Deferred; the Wald
  test's weakness on a separated fit (Hauck–Donner) is now visible through
  `model_diagnostics()`.

Cut in the same pass: `count_messages()`, `fmls_ptype2()` / `fmls_cast()`,
`match_term_keys()`, `effect_frame_fields()`, `semantic_context_match()`,
`empty_group_frame()`, `semantic_missing()`, `collect_cell_group_ids()`,
the dead `.models` vocabulary, and two `.onLoad()` locals. Kept on purpose:
`parse_surv_outcome()` (also passed as a value to `lapply()` in
`add_events()`, which a call-count grep misses), the named `build_mdl_gt()`
stages that `inspect_mdl_gt()` exposes, and the render stages whose inlining
would push `render_cell_frame()` past reading.

Noted, not done: `remove_strata()` deletes the term rather than restoring
it to a covariate; `add_terms()` on an existing term ignores `role =`; the
export surface (69 functions) is the larger "easy to remember" question.
