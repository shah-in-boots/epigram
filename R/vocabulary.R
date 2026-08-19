# Vocabulary -------------------------------------------------------------------

# These constants were previously frozen into `R/sysdata.rda`. They now live
# here as inspectable code so the vocabulary can grow with the package (and be
# read by anyone wondering what the grammar supports).

# Roles a term may play in a causal formula, mapped to their shortcut runes
.roles <- list(
	'outcome' = '.o',
	'exposure' = '.x',
	'predictor' = '.p',
	'confounder' = '.c',
	'mediator' = '.m',
	'strata' = '.s',
	'interaction' = '.i',
	'random' = '.r'
)

# Expansion patterns understood by `fmls()`; the registry in `patterns.R` is
# the authoritative lookup, this vector names the built-ins
.patterns <- c(
	'fundamental',
	'direct',
	'sequential',
	'parallel'
)

# Recognized transformation wrappers. A wrapped term keeps its full call as
# its name (so formulas rebuild losslessly); the wrapper is additionally
# recorded in the `transformation` field for downstream interpretation.
.transformations <- c(
	'log',
	'log1p',
	'log2',
	'log10',
	'exp',
	'sqrt',
	'scale',
	'factor',
	'ordered',
	'poly',
	'ns',
	'bs',
	'I'
)

# Legacy list of directly-supported fitting functions. `fit()` no longer
# gates on this (any fitting function or `{parsnip}` specification is
# accepted); it remains for `mdl.character()` bookkeeping and messages.
.models <- c(
	'model_fit',
	'lm',
	'glm',
	'coxph',
	'lmer',
	'glmer'
)

# Role-to-ANSI style map for terminal printing (see `format.tm()`)
.role_color_styles <- c(
	"outcome" = "col_yellow",
	"exposure" = "col_blue",
	"predictor" = "col_grey",
	"mediator" = "col_cyan",
	"confounder" = "col_green",
	"strata" = "col_magenta",
	"interaction" = "col_br_magenta",
	"random" = "col_br_blue",
	"unknown" = "col_silver"
)

#' Color term labels by role with cli's named ANSI colors
#' @keywords internal
#' @noRd
mdl_gt_color_roles <- function(text,
														 role,
														 color = recast_color()) {
	if (!isTRUE(color) || !has_cli()) {
		return(text)
	}

	numColors <- getExportedValue("cli", "num_ansi_colors")()
	if (numColors <= 1) {
		return(text)
	}

	styles <- .role_color_styles[as.character(role)]
	styles[is.na(styles)] <- .role_color_styles[["unknown"]]
	styles <- unname(styles)

	out <- text
	for (style in unique(styles)) {
		idx <- styles == style
		out[idx] <- getExportedValue("cli", style)(out[idx])
	}

	out
}

#' The `recast` vocabulary
#'
#' @description
#'
#' The grammar of `{recast}` is built on small, controlled vocabularies: the
#' causal *roles* a term can play, the *patterns* by which formulas expand,
#' and the *transformations* a term can carry. These accessors expose the
#' vocabularies so they can be inspected (and so error messages can point
#' somewhere authoritative).
#'
#' - `term_roles()` returns the named list of roles and their formula
#'   shortcuts (e.g. `exposure = ".x"`)
#'
#' - `term_transformations()` returns the recognized transformation wrappers
#'   (e.g. `log()`); a wrapped term keeps its full call as its name and
#'   records the wrapper in its `transformation` field
#'
#' - `formula_patterns()` returns the names of the registered expansion
#'   patterns available to [fmls()] (see [register_pattern()] to add one)
#'
#' @return A named list (`term_roles()`) or character vector (others)
#' @name vocabulary
NULL

#' @rdname vocabulary
#' @export
term_roles <- function() {
	.roles
}

#' @rdname vocabulary
#' @export
term_transformations <- function() {
	.transformations
}

# The table statistics registry (M6.13) -----------------------------------

# The `mesa` table grammar's statistics vocabulary, in one place: each
# recognized statistic's block-declaration name (the name a labeled formula
# or a block-argument uses, e.g. `add_estimates(columns = list(beta ~ ...))`),
# the accent-criterion field name(s) it exposes on a decorated cell (its
# `aliases`), the verb that adds it, its default column header, and whether
# an accent criterion may compare it. Before this registry, the vocabulary
# was written out separately in `add_estimates()`'s known-statistic check,
# `validate_accent()`'s known-name check, `apply_accents()`'s alias patching,
# and `frame_context()`'s default headers — four places that had to agree by
# hand. Adding a statistic is now adding one entry here.
.table_statistics <- list(
	beta = list(
		aliases = c("estimate", "beta"),
		verb = "add_estimates",
		header = "Estimate",
		accentable = TRUE
	),
	conf = list(
		aliases = c("conf_low", "conf_high"),
		verb = "add_estimates",
		header = "95% CI",
		accentable = TRUE
	),
	p = list(
		aliases = c("p", "p_value"),
		verb = "add_estimates",
		header = "P value",
		accentable = TRUE
	),
	n = list(
		aliases = "n",
		verb = "add_n",
		header = "N",
		accentable = TRUE
	),
	events = list(
		aliases = "events",
		verb = "add_events",
		header = "Events",
		accentable = TRUE
	),
	rate = list(
		aliases = "rate",
		verb = "add_events",
		header = "Rate",
		accentable = TRUE
	),
	rate_difference = list(
		aliases = "rate_difference",
		verb = "add_rate_difference",
		header = "Rate difference",
		accentable = TRUE
	),
	forest = list(
		aliases = "forest",
		verb = "add_forest",
		header = "",
		accentable = FALSE
	)
)

#' The table grammar's statistics registry
#'
#' Each column block's statistic (or statistics — `add_estimates()` declares
#' several at once) is one entry: its block-declaration name, the field
#' name(s) an accent criterion or `apply_accents()` reads it by (`aliases`),
#' the verb that records it, and its default column header. See
#' "The interface refinement pass" in `DESIGN.md` (M6.13).
#'
#' @param verb Restrict to the statistics one `add_*()` verb declares (e.g.
#'   `"add_estimates"`); `NULL` (the default) returns every statistic
#'
#' @return A named list, one entry per statistic.
#' @keywords internal
#' @noRd
table_statistics <- function(verb = NULL) {
	if (is.null(verb)) {
		return(.table_statistics)
	}
	Filter(function(s) identical(s$verb, verb), .table_statistics)
}
