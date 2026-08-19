# The <mdl_gt> specification ---------------------------------------------------
#
# `mdl_gt()` lifts a fitted `mdl_tbl` into a declarative S7 specification.
# Small pipeable verbs record effect, cell-group, layout, label, and style
# instructions; the build pipeline resolves them only at inspection or render.
# Cell groups are keyed by stable ids, so add-verb call order cannot determine
# placement.
#
# This file owns the S7 class, constructor, public grammar verbs, validation,
# and print method. The semantic build, layout compiler, and renderer live in
# table-build.R, table-layout.R, and table-render.R respectively.

# `mdl_tbl` is a vctrs vector type and stays one; this registers it as an S3
# class *object* so S7 can name it -- as the type of the spec's `mdl_tbl`
# property below, and (in principle) as a dispatch class for S7 generics.
# Registering, not converting: the two class systems refer to each other, and
# `mdl_tbl` keeps all its vctrs vectorization. See `vignette("s7")`.
#' @keywords internal
#' @noRd
S7_mdl_tbl <- S7::new_S3_class("mdl_tbl")

#' The `<mdl_gt>` table specification
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `mdl_gt()` lays a collection of fitted models out on the *mesa* -- the table
#' upon which the models are displayed -- as a declarative specification. The
#' division of labor with [model_table()] is deliberate: **which models sit on
#' the mesa is decided on the `mdl_tbl`**, with ordinary `dplyr` verbs over its
#' provenance columns (`outcome`, `exposure`, `strata`, ...) and the family
#' columns (`family`, `pattern`, `relation`) the table carries -- pare, print,
#' verify. `mdl_gt()`
#' is then the gate: it verifies what arrives is *one presentable analysis*
#' (see Details) and takes no selection arguments, putting everything fitted
#' on the mesa with default labels drawn from the term table and the attached
#' data. From there the table is grown one **display** decision at a time with
#' pipeable verbs, exactly the way a `{ggplot2}` plot is grown:
#'
#' - `select_adjustment()` chooses which rungs of the adjustment ladder are
#'   displayed (e.g. only the crude and fully adjusted rows --
#'   [adjustment_sets()] shows the rungs and the numbers they are selected
#'   by), and `select_terms()` which terms' cells are shown;
#' - the `add_*()` verbs activate named cell groups derived from models and
#'   attached data (estimates, events, rates, sample sizes, forest views);
#' - `modify_layout()` maps semantic dimensions onto rows and columns, while
#'   [place_cells()] moves whole cell groups without recomputing statistics;
#' - `modify_labels()` relabels outcomes, terms, and levels late, without
#'   reselecting;
#' - [inspect_mdl_gt()] exposes the effects, measures, groups, and cells before
#'   `{gt}` rendering;
#' - `as_gt()` renders the specification to a `{gt}` table (see [as_gt()]).
#'
#' Because the specification is declarative -- the verbs record instructions and
#' resolution happens only at [as_gt()]/`print()` -- the verbs may arrive in any
#' order. **A verb replaces only what you name.** Repeating a verb with the
#' same instruction -- the same selection dimension, group id, style field,
#' or label name -- replaces just that instruction with a message, and leaves
#' everything else recorded on the specification standing (the `{ggplot2}`
#' `labs()` merge behavior); calling a `select_*()` verb with no arguments
#' clears that dimension, which is the way to undo an earlier selection. A
#' bare `mdl_gt(mt) |> as_gt()` already renders a minimal estimate-and-interval
#' table, so the grammar is usable from the first verb.
#'
#' @details
#'
#' The semantic unit is an **effect**: outcome x focal term/contrast x model
#' context, optionally conditioned by stratum and modifier levels. Estimates,
#' confidence bounds, p-values, counts, events, and rates are atomic measures
#' attached at their declared semantic grain. Named cell groups present those
#' measures: `effect` may merge or separate the estimate and interval, while
#' `forest` reads the same three measures without calculating new statistics.
#' Layout changes only project those stable effects and measures into cells.
#'
#' `mdl_gt()` validates before it builds. The object must be a `mdl_tbl`; only
#' its fitted rows are laid out (failed and unfit rows are set aside); the
#' table must hold a single model family (one fitting function, on one link)
#' or it errors; and more than one attached dataset is reported with a
#' message.
#'
#' It then verifies the models form **one presentable analysis**, in the terms
#' of the table's family columns: a single formula family (an adjustment ladder,
#' a mediation triad, parallel adjustment sets, or a lone model), or several
#' families bound by a shared relation -- `varied exposures` or `varied
#' outcomes` over one adjustment ladder, the wide-table shapes. Unrelated
#' families error, pointing back at the `mdl_tbl`: read its `family` column,
#' `dplyr::filter()` it down, and lay out one analysis at
#' a time. See [model_table()] for the collection these are drawn from and
#' [attach_data()] for supplying the data that categorical levels and
#' data-derived statistics are read from.
#'
#' @param object A `mdl_tbl` object holding at least one fitted model
#'
#' @param x A `<mdl_gt>` specification (for the verbs and `print()`)
#'
#' @param ... For `mdl_gt()`, unused -- it deliberately takes no selection
#'   arguments, and an unused argument errors; for the selection verbs,
#'   labeled-formula selection input (a `formula`, a `list` of formulas, or a
#'   `character` vector -- see [labeled_formulas_to_named_list()]), or nothing
#'   at all to clear that dimension's selection; for `print()`, unused
#'
#' @return `mdl_gt()` and the verbs return a `<mdl_gt>` specification object.
#'
#' @examples
#' # The "adjustment" chain -- adjustment sets on rows, outcomes as row
#' # groups, a statistic block per term (the retired `tbl_beta()` shape)
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' mt <-
#'   fmls(mpg ~ .x(wt) + hp + cyl, pattern = "sequential") |>
#'   fit(.fn = lm, data = d, raw = FALSE) |>
#'   model_table(data = d)
#' mt |>
#'   mdl_gt() |>
#'   select_adjustment(1 ~ "Unadjusted", 3 ~ "Fully adjusted") |>
#'   add_estimates(columns = list(beta ~ "Estimate", conf ~ "95% CI")) |>
#'   modify_labels(wt ~ "Weight (1000 lbs)") |>
#'   as_gt()
#'
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' # The "levels" chain -- event and rate rows over adjusted estimates, term
#' # levels on columns (the retired hazard-table shape)
#' lung <- survival::lung
#' lung$sex <- factor(lung$sex, levels = 1:2, labels = c("Male", "Female"))
#' mt <-
#'   fmls(Surv(time, status) ~ .x(sex) + age, pattern = "sequential") |>
#'   fit(.fn = survival::coxph, data = lung, raw = FALSE) |>
#'   model_table(data = lung)
#' mt |>
#'   mdl_gt() |>
#'   modify_layout(preset = "levels") |>
#'   select_adjustment(1 ~ "Unadjusted", 2 ~ "Age-adjusted") |>
#'   add_events(followup = time) |>
#'   add_rate_difference() |>
#'   add_estimates(columns = list(beta ~ "HR", conf ~ "95% CI")) |>
#'   as_gt()
#'
#' @seealso [inspect_mdl_gt()] to troubleshoot, [place_cells()] to move cell
#'   groups, [as_gt()] to render, [model_table()] for the model collection
#'
#' @name mdl_gt
#' @export
mdl_gt <- S7::new_class(
	"mdl_gt",
	package = "recast",
	# Note this class reports itself as `recast::mdl_gt`: S7 namespaces classes
	# defined inside a package (whether or not `package =` is passed). So base
	# `inherits(x, "mdl_gt")` and S3 dispatch on the bare name do *not* match it.
	# recast keeps its `inherits()`-based `validate_class()` and its S3
	# `adjustment_sets()` generic working anyway -- the former strips the `pkg::`
	# prefix before comparing, the latter gains the spec through an S7 *method*.
	# See `vignette("s7")` for the full interop story.

	# The spec's slots become *typed properties*. The vector types stay vctrs;
	# `mdl_tbl` is named through its registered S3 shim (top of file), so a
	# non-`mdl_tbl` can never be stored here. The instruction slots are the plain
	# lists the verbs fill.
	properties = list(
		mdl_tbl   = S7_mdl_tbl,
		family    = S7::class_data.frame,
		selection = S7::class_list,
		labels    = S7::class_list,
		effects   = S7::class_list,
		groups    = S7::class_list,
		layout    = S7::class_list,
		style     = S7::class_list
	),

	# The validator runs on construction *and* on every `@<-` modification, so
	# the invariants the verbs must preserve are declared once here instead of
	# being re-checked in each verb. A verb that drove the spec into a bad state
	# -- `modify_layout()` with an unknown preset, a negative `digits` -- is
	# caught the instant it assigns, not later at render. (Return `NULL`/an empty
	# character vector when valid; one string per violation otherwise.)
	validator = function(self) {
		msg <- character()
		preset <- self@layout$preset
		if (length(preset) &&
				!preset %in% c("adjustment", "levels", "interaction")) {
			msg <- c(msg, sprintf(
				paste0("@layout$preset must be \"adjustment\", \"levels\", or ",
							 "\"interaction\", not \"%s\""),
				preset
			))
		}
		dimensions <- c(
			"outcome", "term", "contrast", "adjustment", "modifier",
			"modifier_level", "stratum", "stratum_level", "subset", "dataset",
			"model"
		)
		for (axis in c("rows", "columns")) {
			value <- self@layout[[axis]]
			if (!is.null(value) && (!is.character(value) || anyNA(value) ||
					any(!value %in% dimensions) || anyDuplicated(value))) {
				msg <- c(msg, paste0(
					"@layout$", axis, " must contain unique semantic dimensions: ",
					paste(dimensions, collapse = ", ")
				))
			}
		}
		if (length(intersect(self@layout$rows, self@layout$columns))) {
			msg <- c(msg, "A semantic dimension cannot appear on both layout axes")
		}
		if (length(self@groups) && (is.null(names(self@groups)) ||
				any(!nzchar(names(self@groups))) || anyDuplicated(names(self@groups)))) {
			msg <- c(msg, "@groups must be a uniquely named list")
		}
		if (length(self@layout$placements) &&
				(is.null(names(self@layout$placements)) ||
				 anyDuplicated(names(self@layout$placements)))) {
			msg <- c(msg, "@layout$placements must be named by cell-group id")
		}
		d <- self@style$digits
		if (!is.null(d) &&
				(!is.numeric(d) || length(d) != 1 || is.na(d) || d < 0)) {
			msg <- c(msg, "@style$digits must be a single non-negative number or NULL")
		}
		if (length(self@selection) &&
				!all(names(self@selection) %in% c("terms", "adjustment"))) {
			msg <- c(msg, "@selection may only name `terms` and `adjustment`")
		}
		msg
	},

	# The constructor is the gate: `mdl_gt(mt)` runs every check a table must
	# pass before it can be laid out. That verification is the bulk of the work,
	# so it lives in `build_mdl_gt()`, which returns the finished property
	# values; the constructor hands them to `new_object()`. S7 requires the
	# `new_object()` call to appear in the constructor body itself, so it lives
	# here rather than in the builder.
	constructor = function(object, ...) {
		v <- new_mdl_gt_spec(object, ...)
		new_object(
			S7_object(),
			mdl_tbl = v$mdl_tbl, family = v$family, selection = v$selection,
			labels = v$labels, effects = v$effects, groups = v$groups,
			layout = v$layout,
			style = v$style
		)
	}
)

#' Build and gate a `<mdl_gt>` specification
#'
#' The construction logic behind [mdl_gt()]: keep only the fitted rows, verify
#' the table is one model family on one link and one presentable analysis (per
#' its family columns), stamp the verified family table on the spec, and return
#' the finished property values as a named list for the constructor to seal into
#' the object. Kept apart from the class's `validator`, which guards the spec's
#' internal invariants on every later modification.
#' @keywords internal
#' @noRd
new_mdl_gt_spec <- function(object, ...) {

	validate_class(object, "mdl_tbl")

	dots <- list(...)
	if (length(dots) > 0) {
		nms <- names(dots)
		unnamed <- is.null(nms) || !any(nzchar(nms))
		stop(
			"`mdl_gt()` takes no selection arguments; it lays out everything ",
			"fitted with default labels, and the `select_*()` verbs narrow it ",
			"afterward.",
			if (!unnamed) {
				paste0(
					" Unused argument", if (sum(nzchar(nms)) > 1) "s" else "", ": ",
					paste0("`", nms[nzchar(nms)], "`", collapse = ", "), "."
				)
			},
			call. = FALSE
		)
	}

	# Only fitted rows go on the mesa; failed and unfit rows are set aside
	status <- model_table_status(object)
	fitted <- object[status == "fitted", , drop = FALSE]
	if (nrow(fitted) == 0) {
		stop(
			"A `mdl_gt` needs fitted models, but none of the ", nrow(object),
			" row(s) are fitted. Fit formulas with ",
			"`fit(..., raw = FALSE)` first",
			if (any(status == "failed")) " (`model_failures()` shows why some failed)",
			".",
			call. = FALSE
		)
	}

	# One table, one model family: mixing e.g. `lm` and `coxph` estimates -- or
	# two `glm`s on different links (logit vs identity), whose estimates live
	# on different scales -- in a single table is not interpretable
	links <- vapply(fitted$model_summary, function(s) {
		if (is.list(s) && length(s$model_link) == 1 && !is.na(s$model_link)) {
			s$model_link
		} else {
			NA_character_
		}
	}, character(1))
	families <- unique(stats::na.omit(
		paste0(fitted$model_call,
					 ifelse(is.na(links), "", paste0(" (", links, ")")))
	))
	if (length(families) > 1) {
		stop(
			"A `mdl_gt` holds a single model family, but this table mixes ",
			paste0("`", families, "`", collapse = ", "),
			". Subset to one family (e.g. with `dplyr::filter(model_call == ...)`) ",
			"before laying it out.",
			call. = FALSE
		)
	}

	# One dataset is the common case; more than one is allowed but worth noting,
	# because data-derived statistics resolve through the attached data
	datasets <- unique(stats::na.omit(fitted$data_id))
	if (length(datasets) > 1) {
		message(
			"This table's models reference more than one dataset (",
			paste(datasets, collapse = ", "),
			"); data-derived statistics resolve each model against its own."
		)
	}

	# One mesa, one analysis: a single formula family, or several families
	# bound by a shared relation over *one* adjustment ladder (varied
	# exposures / varied outcomes -- the wide-table shapes). Anything looser
	# has no coherent layout, so it is turned back toward the model table's
	# own verbs
	fam <- identify_families(model_table_formulas(fitted))
	famTab <- fam[!duplicated(fam$family),
								c("family", "pattern", "relation", "outcome", "exposure"),
								drop = FALSE]
	if (nrow(famTab) > 1) {
		relSets <- lapply(famTab$relation, function(r) {
			if (is.na(r)) character() else trimws(strsplit(r, ",")[[1]])
		})
		shared <- Reduce(intersect, relSets)
		if (length(shared) == 0) {
			lines <- paste0(
				"family ", famTab$family, " (", famTab$pattern, "): ",
				famTab$outcome,
				ifelse(is.na(famTab$exposure), "",
							 paste0(" ~ ", famTab$exposure))
			)
			stop(
				"A `mdl_gt` holds one analysis: a single family, or families ",
				"related by varied exposures/outcomes over a shared adjustment ",
				"ladder. This table holds unrelated families:\n  ",
				paste(lines, collapse = "\n  "),
				"\nPare the model table down first: the `family`/`pattern`/`relation` ",
				"columns show the structure, then `keep_families(1)`, ",
				"`keep_outcomes()`, or `keep_exposures()` to cut.",
				call. = FALSE
			)
		}
		# A relation label shared across two *different* ladders is two
		# analyses standing side by side (e.g. two varied-exposure pairs on
		# unrelated adjustment sets) -- their rows could never align
		ladders <- vapply(
			split(fam$covariates, fam$family),
			ladder_signature, character(1)
		)
		if (length(unique(ladders)) > 1) {
			stop(
				"A `mdl_gt` holds one analysis, but these families share the ",
				paste0("`", shared, "`", collapse = "/"),
				" relation across ", length(unique(ladders)),
				" different adjustment ladders -- several analyses side by ",
				"side. Pare the model table to one ladder first ",
				"(the `family`/`relation` columns show the structure; ",
				"`keep_families()` or `adjusting_for()` to cut).",
				call. = FALSE
			)
		}
	}

	# The spec starts with empty defaults: everything fitted is selected, the
	# layout groups adjustment-set rows by outcome (`modify_layout()` swaps
	# presets), and style is unset until `modify_style()` records instructions
	# (the renderer resolves fallbacks -- digits 2, missing text "", padding
	# from the blocks -- at render, after any column block's own `digits`)
	list(
		mdl_tbl = fitted,
		family = famTab,
		selection = list(terms = NULL, adjustment = NULL),
		labels = list(relabels = list(), columns = list()),
		effects = list(interaction = FALSE, conf_level = 0.95),
		groups = list(
			effect = list(
				id = "effect", view = "merged", show_estimate = TRUE,
				show_confidence = TRUE, exponentiate = NULL, digits = NULL,
				labels = list(estimate = "Estimate", confidence = "95% CI"),
				implicit = TRUE
			)
		),
		layout = list(
			preset = "adjustment", rows = NULL, columns = NULL,
			placements = list(), declared = FALSE
		),
		style = list(accents = list(), digits = NULL, missing_text = NULL,
								 padding = NULL, theme = "journal", widths = list(),
								 align = list(), reference_text = "")
	)
}

# Selection verbs -------------------------------------------------------------

#' Collect labeled-formula verb input into a single object
#'
#' The verbs accept the documented labeled-formula forms directly
#' (`select_terms(m, cyl ~ "Cylinders")`), a list (`list(...)`), a character
#' vector, or several formulas as separate arguments
#' (`select_adjustment(m, 1 ~ "Crude", 3 ~ "Adjusted")`). One argument is
#' passed through untouched; several formulas are gathered into a list, which
#' [labeled_formulas_to_named_list()] understands.
#' @keywords internal
#' @noRd
collect_labeled <- function(...) {
	dots <- list(...)
	if (length(dots) == 0) {
		return(NULL)
	}
	if (length(dots) == 1) {
		return(dots[[1]])
	}
	dots
}

#' Record a selection instruction, replacing any earlier one with a message.
#' Calling the verb with no arguments records `NULL`, which clears the
#' dimension -- the documented way to undo an earlier `select_*()` call.
#' @keywords internal
#' @noRd
record_selection <- function(x, dimension, input, verb) {
	if (!is.null(x@selection[[dimension]])) {
		message("`", verb, "()` replaces the earlier ", dimension, " selection.")
	}
	# `@<-` re-validates the whole spec (here: that `@selection` still names only
	# the two dimensions). Assigning `NULL` clears the dimension, the documented
	# undo -- base list semantics drop the element, and the validator is content.
	x@selection[[dimension]] <- input
	x
}

#' @rdname mdl_gt
#' @export
select_terms <- function(x, ...) {
	validate_class(x, "mdl_gt")
	record_selection(x, "terms", collect_labeled(...), "select_terms")
}

#' @rdname mdl_gt
#' @export
select_adjustment <- function(x, ...) {
	validate_class(x, "mdl_gt")
	record_selection(x, "adjustment", collect_labeled(...), "select_adjustment")
}

#' Relabel terms, levels, or columns late
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `modify_labels()` rethinks a label without reselecting anything. It absorbs
#' the old `level_labels` argument of the retired `tbl_*` functions, so its
#' labeled formulas may name either a term (the variable) or a specific level:
#'
#' - a variable with a scalar label relabels the **term**
#'   (`smoking ~ "Smoking status"`);
#' - a variable with a vector label relabels the term's **levels** in
#'   ascending order (`am ~ c("Manual", "Automatic")`);
#' - a bare level value relabels that **level** wherever it appears
#'   (`0 ~ "Absent"`);
#' - an outcome relabels its **row group** (`mpg ~ "Miles per gallon"`); a
#'   name that is both a displayed term and an outcome -- a mediator --
#'   relabels both.
#'
#' Column (statistic) relabelings are supplied through `columns` and consumed
#' by the column verbs. Like every verb, `modify_labels()` merges: naming a
#' term, level, or column again replaces just that one label, with a message
#' naming it, while every other label already recorded -- from this call or an
#' earlier one -- stands (the `{ggplot2}` `labs()` merge behavior). So
#' rethinking one label late never forces restating the rest.
#'
#' @param x A `<mdl_gt>` specification
#' @param ... Labeled formulas relabeling terms or levels (see Description)
#' @param columns A labeled-formula input relabeling statistic columns
#'
#' @return The modified `<mdl_gt>` specification.
#'
#' @seealso [mdl_gt()]
#' @export
modify_labels <- function(x, ..., columns = NULL) {

	validate_class(x, "mdl_gt")

	relabels <- collect_labeled(...)
	if (is.null(relabels) && is.null(columns)) {
		return(x)
	}

	if (!is.null(relabels)) {
		new <- labeled_formulas_to_named_list(relabels)
		repeated <- intersect(names(new), names(x@labels$relabels))
		if (length(repeated) > 0) {
			message(
				"`modify_labels()` replaces the earlier label for ",
				paste0("`", repeated, "`", collapse = ", "), "."
			)
		}
		x@labels$relabels[names(new)] <- new
	}

	if (!is.null(columns)) {
		new <- labeled_formulas_to_named_list(columns)
		repeated <- intersect(names(new), names(x@labels$columns))
		if (length(repeated) > 0) {
			message(
				"`modify_labels()` replaces the earlier column label for ",
				paste0("`", repeated, "`", collapse = ", "), "."
			)
		}
		x@labels$columns[names(new)] <- new
	}

	x
}

#' Map semantic dimensions onto a `<mdl_gt>` layout
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `modify_layout()` either selects a declarative preset or maps semantic
#' dimensions directly onto ordered `rows` and `columns`. Outer row dimensions
#' become row groups; outer column dimensions become spanners. Available
#' dimensions include `outcome`, `adjustment`, `term`, `contrast`, `modifier`,
#' `modifier_level`, `stratum`, `stratum_level`, `subset`, `dataset`, and
#' `model`.
#'
#' The built-in presets are:
#'
#' - `"adjustment"` (the default): outcome/adjustment rows and term/contrast
#'   columns.
#' - `"levels"`: statistic rows (the event count and incidence rate when
#'   [add_events()] is on the mesa, then one row per adjustment set), term
#'   levels on columns, with effect cells in the semantic body.
#' - `"interaction"`: modifier/modifier-level rows and contrast columns, with
#'   the across-level interaction p-value scoped to each modifier band.
#'
#' Every varying effect dimension must be placed on an axis or selected away;
#' otherwise compilation fails with the missing dimensions named. Use
#' [place_cells()] independently to position `effect`, `p`, `n`, `events`,
#' `rate`, `rate_difference`, and `forest` on columns, rows, or the body.
#'
#' @param x A `<mdl_gt>` specification
#' @param preset One of `"adjustment"`, `"levels"`, or `"interaction"`
#' @param rows,columns Ordered character vectors of semantic dimensions.
#'
#' @return The modified `<mdl_gt>` specification.
#'
#' @seealso [place_cells()], [inspect_mdl_gt()], [mdl_gt()], [as_gt()]
#' @export
modify_layout <- function(x, preset = NULL, rows = NULL, columns = NULL) {

	validate_class(x, "mdl_gt")

	if (is.null(preset) && is.null(rows) && is.null(columns)) {
		return(x)
	}

	presets <- c("adjustment", "levels", "interaction")
	if (!is.null(preset)) {
		if (!is.character(preset) || length(preset) != 1 ||
				!preset %in% presets) {
			stop(
				"`preset` must be one of the launch layout presets: ",
				paste0("`\"", presets, "\"`", collapse = ", "), ".",
				call. = FALSE
			)
		}
	}
	dimensions <- mdl_gt_dimensions()
	validate_axis <- function(value, name) {
		if (is.null(value)) return(NULL)
		if (!is.character(value) || anyNA(value) || any(!nzchar(value)) ||
				anyDuplicated(value)) {
			stop("`", name, "` must be a character vector of unique dimensions.",
					 call. = FALSE)
		}
		unknown <- setdiff(value, dimensions)
		if (length(unknown)) {
			stop(
				"`", name, "` contains unknown dimensions: ",
				paste0("`", unknown, "`", collapse = ", "), ". Available: ",
				paste0("`", dimensions, "`", collapse = ", "), ".",
				call. = FALSE
			)
		}
		value
	}
	rows <- validate_axis(rows, "rows")
	columns <- validate_axis(columns, "columns")
	if (length(intersect(rows, columns))) {
		stop(
			"A semantic dimension cannot appear on both `rows` and `columns`: ",
			paste0("`", intersect(rows, columns), "`", collapse = ", "), ".",
			call. = FALSE
		)
	}

	if (isTRUE(x@layout$declared)) {
		message("`modify_layout()` replaces the earlier layout instruction.")
	}

	# Each assignment re-validates: the verb is the friendly front door and the
	# class validator remains the backstop.
	if (!is.null(preset)) {
		x@layout$preset <- preset
		# A preset is a complete default map. Explicit placements survive because
		# they are independent instructions and therefore remain order-independent.
		x@layout$rows <- NULL
		x@layout$columns <- NULL
	}
	if (!is.null(rows)) x@layout$rows <- rows
	if (!is.null(columns)) x@layout$columns <- columns
	x@layout$declared <- TRUE
	x
}

#' Place cell groups on rows, columns, or the semantic body
#'
#' `place_cells()` changes presentation only. It never changes which effects
#' or measures are computed, and it may be called before or after the matching
#' `add_*()` verb. Group ids are the stable presentation vocabulary:
#' `effect`, `p`, `n`, `events`, `rate`, `rate_difference`, and `forest`.
#'
#' @param x A `<mdl_gt>` specification.
#' @param ... Cell-group ids, as bare names or character vectors.
#' @param axis One of `"columns"`, `"rows"`, or `"body"`.
#' @param .before,.after Optional group id (or `"body"`) that fixes relative
#'   placement. Supply at most one.
#'
#' @return The modified `<mdl_gt>` specification.
#' @export
place_cells <- function(x, ..., axis = c("columns", "rows", "body"),
							.before = NULL, .after = NULL) {

	validate_class(x, "mdl_gt")
	axis <- match.arg(axis)
	ids <- collect_cell_group_ids(...)
	if (!length(ids)) {
		stop("`place_cells()` needs at least one cell-group id.", call. = FALSE)
	}
	unknown <- setdiff(ids, mdl_gt_group_ids())
	if (length(unknown)) {
		stop(
			"Unknown cell group", if (length(unknown) > 1) "s" else "", ": ",
			paste0("`", unknown, "`", collapse = ", "), ". Available groups: ",
			paste0("`", mdl_gt_group_ids(), "`", collapse = ", "), ".",
			call. = FALSE
		)
	}
	registry <- mdl_gt_group_registry()
	unsupported <- ids[!vapply(ids, function(id) {
		axis %in% registry[[id]]$supported_axes
	}, logical(1))]
	if (length(unsupported)) {
		stop(
			"Cell group", if (length(unsupported) > 1) "s " else " ",
			paste0("`", unsupported, "`", collapse = ", "),
			" cannot be placed on the `", axis, "` axis.", call. = FALSE
		)
	}
	if (!is.null(.before) && !is.null(.after)) {
		stop("Supply only one of `.before` and `.after`.", call. = FALSE)
	}
	validate_anchor <- function(value, name) {
		if (is.null(value)) return(NULL)
		if (!is.character(value) || length(value) != 1 || is.na(value) ||
				!value %in% c("body", mdl_gt_group_ids())) {
			stop("`", name, "` must name a cell group or `\"body\"`.",
					 call. = FALSE)
		}
		value
	}
	.before <- validate_anchor(.before, ".before")
	.after <- validate_anchor(.after, ".after")

	# Ordering within one call is explicit. Encode it as stable constraints,
	# rather than an insertion index, so separate calls remain order-independent.
	for (i in seq_along(ids)) {
		id <- ids[[i]]
		before <- if (i < length(ids)) ids[[i + 1L]] else .before
		after <- if (i == 1L) .after else ids[[i - 1L]]
		x@layout$placements[[id]] <- list(
			axis = axis, before = before, after = after
		)
	}
	knownPlacements <- mdl_gt_group_ids()[mdl_gt_group_ids() %in%
		names(x@layout$placements)]
	x@layout$placements <- x@layout$placements[knownPlacements]
	x
}

#' @keywords internal
#' @noRd
collect_cell_group_ids <- function(...) {
	exprs <- as.list(substitute(list(...)))[-1]
	if (!length(exprs)) return(character())
	caller <- parent.frame()
	ids <- unlist(lapply(exprs, function(expr) {
		if (is.symbol(expr)) return(as.character(expr))
		value <- eval(expr, envir = caller)
		if (!is.character(value)) {
			stop("Cell-group ids must be bare names or character vectors.",
					 call. = FALSE)
		}
		value
	}), use.names = FALSE)
	unique(ids)
}

#' @keywords internal
#' @noRd
mdl_gt_dimensions <- function() {
	c(
		"outcome", "term", "contrast", "adjustment", "modifier",
		"modifier_level", "stratum", "stratum_level", "subset", "dataset",
		"model"
	)
}

#' @keywords internal
#' @noRd
mdl_gt_group_ids <- function() {
	c("effect", "p", "n", "events", "rate", "rate_difference", "forest")
}

# Cell-group verbs ------------------------------------------------------------

#' Add model-effect cell groups
#'
#' `add_estimates()` selects the atomic model measures and their built-in text
#' presentation. `view = "merged"` combines estimate and confidence interval
#' in one cell; `"separate"` gives them independent leaf columns. The p-value
#' is always an independently movable `p` group.
#'
#' @param x A `<mdl_gt>` specification.
#' @param columns Labeled formulas naming `beta`, `conf`, and/or `p`.
#' @param exponentiate `NULL` to infer scale, or a logical override.
#' @param digits Numeric display precision.
#' @param view `"merged"` or `"separate"`.
#' @return The modified specification.
#' @export
add_estimates <- function(x,
		columns = list(beta ~ "Estimate", conf ~ "95% CI", p ~ "P value"),
		exponentiate = NULL, digits = NULL,
		view = c("merged", "separate")) {

	validate_class(x, "mdl_gt")
	view <- match.arg(view)
	statistics <- labeled_formulas_to_named_list(columns)
	known <- c("beta", "conf", "p")
	unknown <- setdiff(names(statistics), known)
	if (!length(statistics) || length(unknown)) {
		stop(
			"`columns` must name one or more of `beta`, `conf`, and `p`",
			if (length(unknown)) paste0("; unknown: `", paste(unknown, collapse = "`, `"), "`") else "",
			".", call. = FALSE
		)
	}
	if (!is.null(exponentiate) &&
			(!is.logical(exponentiate) || length(exponentiate) != 1 ||
			 is.na(exponentiate))) {
		stop("`exponentiate` must be `NULL`, `TRUE`, or `FALSE`.", call. = FALSE)
	}
	validate_scalar(digits, "digits", min = 0, allow_null = TRUE)

	showEffect <- any(c("beta", "conf") %in% names(statistics))
	if (showEffect) {
		x <- record_cell_group(x, "effect", list(
			id = "effect", view = view,
			show_estimate = "beta" %in% names(statistics),
			show_confidence = "conf" %in% names(statistics),
			exponentiate = exponentiate,
			digits = if (is.null(digits)) NULL else as.integer(digits),
			labels = list(
				estimate = if ("beta" %in% names(statistics)) as.character(statistics$beta) else NULL,
				confidence = if ("conf" %in% names(statistics)) as.character(statistics$conf) else NULL
			),
			implicit = FALSE
		), "add_estimates")
	} else {
		x@groups$effect <- NULL
	}
	if ("p" %in% names(statistics)) {
		x <- record_cell_group(x, "p", list(
			id = "p", label = as.character(statistics$p), digits = 3L
		), "add_estimates")
	} else {
		x@groups$p <- NULL
	}
	x
}

#' Add model sample-size cells
#' @param label Column or row label.
#' @rdname add_estimates
#' @export
add_n <- function(x, label = "N") {
	validate_class(x, "mdl_gt")
	validate_scalar(label, "label", type = "string")
	record_cell_group(x, "n", list(id = "n", label = label), "add_n")
}

#' Add event-count and incidence-rate cells
#'
#' @param x A `<mdl_gt>` specification.
#' @param followup Follow-up column, as a bare name or string. It is inferred
#'   from a common `Surv()` outcome when omitted.
#' @param person_years Rate denominator.
#' @param scale Divisor converting follow-up units to years.
#' @param digits Rate precision.
#' @export
add_events <- function(x, followup, person_years = 100, scale = 365.25,
		digits = 1) {
	validate_class(x, "mdl_gt")
	if (missing(followup)) {
		outcomes <- unique(stats::na.omit(x@mdl_tbl$outcome))
		surv <- lapply(outcomes, parse_surv_outcome)
		followup <- NULL
		if (length(outcomes) && !any(vapply(surv, is.null, logical(1)))) {
			times <- unique(vapply(surv, `[[`, character(1), "time"))
			if (length(times) == 1) followup <- times
		}
		if (is.null(followup)) {
			stop("`add_events()` needs `followup`; it can only be inferred from a common `Surv()` time argument.",
					 call. = FALSE)
		}
	} else {
		expr <- substitute(followup)
		followup <- if (is.symbol(expr)) as.character(expr) else followup
	}
	validate_scalar(followup, "followup", type = "string")
	validate_scalar(person_years, "person_years", min = 0, inclusive = FALSE)
	validate_scalar(scale, "scale", min = 0, inclusive = FALSE)
	validate_scalar(digits, "digits", min = 0)
	config <- list(
		followup = followup, person_years = as.numeric(person_years),
		scale = as.numeric(scale), digits = as.integer(digits)
	)
	x <- record_cell_group(x, "events", c(list(id = "events", label = "Events"), config),
		"add_events")
	record_cell_group(x, "rate", c(list(
		id = "rate", label = paste0("Rate per ", person_years, " person-years")
	), config), "add_events")
}

#' Add a term-scoped incidence-rate difference
#' @param conf_level Confidence level.
#' @rdname add_events
#' @export
add_rate_difference <- function(x, conf_level = 0.95) {
	validate_class(x, "mdl_gt")
	validate_scalar(conf_level, "conf_level", min = 0, max = 1,
							 inclusive = FALSE)
	record_cell_group(x, "rate_difference", list(
		id = "rate_difference", conf_level = as.numeric(conf_level),
		label = paste0("Rate difference (", format(conf_level * 100), "% CI)")
	), "add_rate_difference")
}

#' Add conditional effects for model interaction terms
#'
#' This changes the effect source, not the presentation. Modifier levels become
#' ordinary semantic dimensions and can be combined with adjustment and strata.
#' @param x A `<mdl_gt>` specification.
#' @param conf_level Confidence level for conditional effects.
#' @export
add_interaction <- function(x, conf_level = 0.95) {
	validate_class(x, "mdl_gt")
	validate_scalar(conf_level, "conf_level", min = 0, max = 1,
							 inclusive = FALSE)
	if (isTRUE(x@effects$interaction)) {
		message("`add_interaction()` replaces the earlier interaction-effect instruction.")
	}
	x@effects$interaction <- TRUE
	x@effects$conf_level <- as.numeric(conf_level)
	if (!isTRUE(x@layout$declared)) x@layout$preset <- "interaction"
	x
}

#' Add a forest presentation of effect estimates
#'
#' @param x A `<mdl_gt>` specification.
#' @param axis Named options: `limits`, `breaks`, `intercept`, `log`, `title`,
#'   `left`, and `right`.
#' @param width Cell width in pixels.
#' @param invert Draw reciprocal effects.
#' @export
add_forest <- function(x, axis = list(), width = 120, invert = FALSE) {
	validate_class(x, "mdl_gt")
	if (!is.list(axis) || (length(axis) && is.null(names(axis)))) {
		stop("`axis` must be a named list.", call. = FALSE)
	}
	known <- c("limits", "breaks", "intercept", "log", "title", "left", "right")
	unknown <- setdiff(names(axis), known)
	if (length(unknown)) {
		stop("Unknown forest-axis options: ",
				 paste0("`", unknown, "`", collapse = ", "), ".", call. = FALSE)
	}
	if (!is.null(axis$limits) &&
			(!is.numeric(axis$limits) || length(axis$limits) != 2 || anyNA(axis$limits))) {
		stop("`axis$limits` must be a length-2 numeric vector.", call. = FALSE)
	}
	validate_scalar(width, "width", min = 0, inclusive = FALSE)
	if (!is.logical(invert) || length(invert) != 1 || is.na(invert)) {
		stop("`invert` must be `TRUE` or `FALSE`.", call. = FALSE)
	}
	record_cell_group(x, "forest", list(
		id = "forest", label = "", axis = axis, width = as.numeric(width),
		invert = invert
	), "add_forest")
}

#' @keywords internal
#' @noRd
record_cell_group <- function(x, id, config, verb) {
	old <- x@groups[[id]]
	if (!is.null(old) && !isTRUE(old$implicit)) {
		message("`", verb, "()` replaces the earlier `", id, "` cell group.")
	}
	x@groups[[id]] <- config
	order <- mdl_gt_group_ids()[mdl_gt_group_ids() %in% names(x@groups)]
	x@groups <- x@groups[order]
	x
}

#' Style a `<mdl_gt>` at render
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `modify_style()` records style instructions, applied when the table is
#' rendered by [as_gt()]:
#'
#' - `accents` emphasize the cells that meet a criterion. Each accent is a
#'   two-sided formula whose **left side** is a criterion on any displayed
#'   statistic (`p < 0.05`, `estimate > 1`, `rate >= 10`) and whose **right
#'   side** is the instruction: `"bold"`, `"italic"`, or a text color (any
#'   color name or hex value); several may be combined
#'   (`p < 0.05 ~ c("bold", "italic")`). A criterion is evaluated once per
#'   term-level within each row, against all of that context's statistics, and
#'   accents every cell of the context -- so `p < 0.05 ~ "bold"` bolds both the
#'   estimate and the p-value it belongs to. The recognized statistic names
#'   are `estimate` (alias `beta`), `conf_low`, `conf_high`, `p` (alias
#'   `p_value`), `n`, `events`, `rate`, and `rate_difference`.
#' - `digits` sets the table-wide formatting default. A column block's own
#'   `digits` still wins for its columns; p-values keep their three-decimal
#'   rule.
#' - `missing_text` fills every cell with nothing to show (reference levels,
#'   estimates a model did not produce). The default is an empty cell.
#' - `padding` scales the table's vertical row padding (as
#'   [gt::opt_vertical_padding()] does). A table carrying a forest column
#'   defaults to `0` -- the dense canvas its plot cells need to read as one --
#'   and this overrides that default.
#'
#' Like every verb, `modify_style()` merges: naming `digits` again after an
#' earlier `accents` call replaces only `digits`, with a message naming it --
#' the accents already recorded stand. Each of `accents`, `digits`,
#' `missing_text`, and `padding` replaces only itself.
#'
#' @param x A `<mdl_gt>` specification
#' @param accents A formula or list of formulas: `criterion ~ instruction`
#'   (see Description)
#' @param digits Number of digits estimates are formatted to
#' @param missing_text Text shown in cells with nothing to display
#' @param padding Vertical padding scale, `0` (dense) upward
#' @param theme Built-in HTML theme: `"journal"`, `"compact"`, or `"plain"`.
#' @param widths Named list of pixel widths keyed by cell-group id.
#' @param align Named list of `"left"`, `"center"`, or `"right"` alignment
#'   values keyed by cell-group id.
#' @param reference_text Text used for reference effects.
#'
#' @return The modified `<mdl_gt>` specification.
#'
#' @seealso [mdl_gt()], [as_gt()]
#' @export
modify_style <- function(x, accents = NULL, digits = NULL,
												 missing_text = NULL, padding = NULL,
												 theme = NULL, widths = NULL, align = NULL,
												 reference_text = NULL) {

	validate_class(x, "mdl_gt")

	if (is.null(accents) && is.null(digits) && is.null(missing_text) &&
			is.null(padding) && is.null(theme) && is.null(widths) &&
			is.null(align) && is.null(reference_text)) {
		return(x)
	}

	if (!is.null(accents)) {
		if (inherits(accents, "formula")) {
			accents <- list(accents)
		}
		accents <- lapply(accents, validate_accent)
	}
	validate_scalar(digits, "digits", min = 0, allow_null = TRUE)
	validate_scalar(missing_text, "missing_text", type = "string",
									 allow_null = TRUE)
	validate_scalar(padding, "padding", min = 0, allow_null = TRUE)
	if (!is.null(theme)) {
		if (!is.character(theme) || length(theme) != 1 ||
				!theme %in% c("journal", "compact", "plain")) {
			stop("`theme` must be `\"journal\"`, `\"compact\"`, or `\"plain\"`.",
					 call. = FALSE)
		}
	}
	validate_named_list <- function(value, name) {
		if (is.null(value)) return(NULL)
		if (!is.list(value) || is.null(names(value)) || any(!nzchar(names(value)))) {
			stop("`", name, "` must be a named list keyed by cell-group id.",
					 call. = FALSE)
		}
		unknown <- setdiff(names(value), mdl_gt_group_ids())
		if (length(unknown)) {
			stop("`", name, "` names unknown cell groups: ",
					 paste0("`", unknown, "`", collapse = ", "), ".", call. = FALSE)
		}
		value
	}
	widths <- validate_named_list(widths, "widths")
	align <- validate_named_list(align, "align")
	validate_scalar(reference_text, "reference_text", type = "string",
							 allow_null = TRUE)

	# Each argument replaces only its own field, with a message only when that
	# field was already recorded -- the other style instructions stand (M6.11)
	if (!is.null(accents)) {
		if (length(x@style$accents) > 0) {
			message("`modify_style()` replaces the earlier `accents` instruction.")
		}
		x@style$accents <- accents
	}
	if (!is.null(digits)) {
		if (!is.null(x@style$digits)) {
			message("`modify_style()` replaces the earlier `digits` instruction.")
		}
		x@style$digits <- as.integer(digits)
	}
	if (!is.null(missing_text)) {
		if (!is.null(x@style$missing_text)) {
			message(
				"`modify_style()` replaces the earlier `missing_text` instruction."
			)
		}
		x@style$missing_text <- missing_text
	}
	if (!is.null(padding)) {
		if (!is.null(x@style$padding)) {
			message("`modify_style()` replaces the earlier `padding` instruction.")
		}
		x@style$padding <- as.numeric(padding)
	}
	if (!is.null(theme)) x@style$theme <- theme
	if (!is.null(widths)) x@style$widths[names(widths)] <- widths
	if (!is.null(align)) x@style$align[names(align)] <- align
	if (!is.null(reference_text)) x@style$reference_text <- reference_text

	x
}

#' Validate one accent formula into a criterion + instruction pair
#'
#' The criterion (LHS) must be a comparison over the recognized statistic
#' names; the instruction (RHS) must evaluate to a character vector of
#' `"bold"`, `"italic"`, and/or a text color. Validated at verb time --
#' recording a bad accent and failing at render would waste the laziness.
#' @keywords internal
#' @noRd
validate_accent <- function(f) {

	if (!inherits(f, "formula") || length(f) != 3) {
		stop(
			"Each accent must be a two-sided formula: `criterion ~ instruction`, ",
			"e.g. `p < 0.05 ~ \"bold\"`.",
			call. = FALSE
		)
	}

	criterion <- f[[2]]
	# The accent vocabulary: every alias of every accentable statistic
	known <- unlist(
		lapply(Filter(function(s) isTRUE(s$accentable), table_statistics()),
					 `[[`, "aliases"),
		use.names = FALSE
	)
	used <- all.vars(criterion)
	unknown <- setdiff(used, known)
	if (length(used) == 0 || length(unknown) > 0) {
		stop(
			"An accent criterion compares a displayed statistic",
			if (length(unknown) > 0) {
				paste0(", but `", paste(unknown, collapse = "`, `"),
							 "` is not one. The recognized statistics are: ")
			} else {
				". The recognized statistics are: "
			},
			paste0("`", known, "`", collapse = ", "), ".",
			call. = FALSE
		)
	}

	instruction <- tryCatch(
		eval(f[[3]], envir = environment(f)),
		error = function(e) NULL
	)
	if (!is.character(instruction) || length(instruction) == 0 ||
			anyNA(instruction)) {
		stop(
			"An accent instruction is a character vector: `\"bold\"`, ",
			"`\"italic\"`, and/or a text color (`\"red\"`, `\"#B22222\"`).",
			call. = FALSE
		)
	}

	list(criterion = criterion, instruction = instruction)
}

# Printing --------------------------------------------------------------------

# `print()` and `format()` are base S3 generics; registering S7 methods on them
# (rather than writing `print.mdl_gt`) is the idiomatic S7 route. Call the
# replacement function directly so package loading does not create local
# `print`/`format` bindings; those bindings would make NAMESPACE register the
# package's existing S3 methods against the wrong generic. No `@export`:
# `S7::methods_register()` in `.onLoad` wires these methods into base dispatch.
S7::`method<-`(base::print, mdl_gt, function(x, ...) {
	cat(base::format(x, ...), sep = "\n")
	invisible(x)
})

S7::`method<-`(base::format, mdl_gt, function(x, ...) {

	mt <- x@mdl_tbl
	family <- unique(stats::na.omit(mt$model_call))
	datasets <- unique(stats::na.omit(mt$data_id))

	header <- paste0(
		"<mdl_gt> specification: ", nrow(mt),
		if (nrow(mt) == 1) " fitted model" else " fitted models",
		if (length(family) > 0) paste0(" \u00d7 ", paste(family, collapse = ", "))
	)

	dataLine <-
		if (length(datasets) > 0) {
			datLs <- attr(mt, "dataList")
			marks <- vapply(datasets, function(d) {
				if (d %in% names(datLs)) d else paste0(d, " [detached]")
			}, character(1))
			paste0("  data: ", paste(marks, collapse = ", "))
		}

	# The verified analysis: its family pattern(s), and the relation binding
	# several families when the mesa is a wide-table shape
	familyLine <-
		if (!is.null(x@family) && nrow(x@family) > 0) {
			rels <- unique(stats::na.omit(x@family$relation))
			paste0(
				"  analysis: ", nrow(x@family),
				if (nrow(x@family) == 1) " family (" else " families (",
				paste(unique(x@family$pattern), collapse = ", "), ")",
				if (length(rels) > 0) paste0(" -- ", paste(rels, collapse = "; "))
			)
		}

	preset <- mdl_gt_preset(x@layout$preset)
	rows <- first_of(x@layout$rows, preset$rows)
	columns <- first_of(x@layout$columns, preset$columns)
	layoutLine <- paste0(
		"  layout: ", x@layout$preset,
		" (rows: ", paste(rows, collapse = " > "),
		"; columns: ", paste(columns, collapse = " > "), ")"
	)
	effectLine <- paste0(
		"  effects: ",
		if (isTRUE(x@effects$interaction)) "conditional by modifier" else
			"model coefficients"
	)

	# Declared selection, one line per dimension that has been narrowed
	selLines <- character()
	for (d in names(x@selection)) {
		input <- x@selection[[d]]
		if (!is.null(input)) {
			nm <- names(labeled_formulas_to_named_list(input))
			selLines <- c(selLines, paste0("    ", d, ": ", paste(nm, collapse = ", ")))
		}
	}
	selectionBlock <-
		if (length(selLines) > 0) {
			c("  selection:", selLines)
		} else {
			"  selection: everything fitted (bare mesa)"
		}

	groupLine <- paste0("  cell groups: ", paste(names(x@groups), collapse = ", "))
	placementLines <- character()
	if (length(x@layout$placements)) {
		placementLines <- c(
			"  placements:",
			vapply(names(x@layout$placements), function(id) {
				p <- x@layout$placements[[id]]
				paste0("    ", id, ": ", p$axis,
						 if (!is.null(p$before)) paste0(" before ", p$before) else "",
						 if (!is.null(p$after)) paste0(" after ", p$after) else "")
			}, character(1))
		)
	}

	labelsLine <-
		if (length(x@labels$relabels) > 0 || length(x@labels$columns) > 0) {
			relabelled <- unique(c(names(x@labels$relabels), names(x@labels$columns)))
			paste0("  labels: ", paste(relabelled, collapse = ", "))
		}

	hint <- paste0(
		"# `as_gt()` renders; `select_*()` / `modify_labels()` refine the mesa"
	)

	c(header, dataLine, familyLine, effectLine, layoutLine, selectionBlock,
		groupLine, placementLines, labelsLine, "", hint)
})
