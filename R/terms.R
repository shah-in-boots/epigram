### Term class -----------------------------------------------------------------

#' Create vectorized terms
#'
#' `r lifecycle::badge('experimental')`
#'
#' A vectorized term object that allows for additional information to be carried
#' with the variable name.
#'
#' @details
#'
#' This is not meant to replace traditional [stats::terms()], but to supplement
#' it using additional information that is more informative for causal modeling.
#'
#' # Roles
#'
#' Specific roles the variable plays within the formula. These are of particular
#' importance, as they serve as special terms that can effect how a formula is
#' interpreted. Each role has a causal definition, and each role changes
#' behavior downstream — in how formulas expand ([fmls()]), how models are fit
#' ([fit()]), and how results are displayed.
#'
#' | Role | Shortcut | Definition | Downstream behavior |
#' | --- | --- | --- | --- |
#' | outcome | `.o(...)` | the dependent variable; the effect being studied | anchors the LHS; multiple outcomes multiply the formula family |
#' | exposure | `.x(...)` | the variable whose causal effect is under study | anchored in every expanded formula; pairs with interactions |
#' | predictor | `.p(...)` | a covariate with no asserted causal position | expanded by the chosen pattern (adjusted for, or rotated through) |
#' | confounder | `.c(...)` | a common cause of exposure and outcome | treated as a covariate; flagged for adjustment displays |
#' | mediator | `.m(...)` | on the causal pathway between exposure and outcome | triggers the mediation triad of formulas (see [fmls()]) |
#' | interaction | `.i(...)` | a candidate effect modifier of the exposure | crossed with each exposure (`x:i`), grouped so the pair travels together |
#' | strata | `.s(...)` | a variable defining subpopulations for separate fits | not a covariate; [fit()] fits one model per stratum level |
#' | random | `.r(...)` | a grouping variable for random (hierarchical) effects | rendered as `(1 \| term)` for mixed-model engines; excluded from covariate expansion |
#' | group | `.g(...)` | not a role, but a tier marker for terms that travel together | grouped terms enter and leave expanded formulas as one block |
#' | _unknown_ | `-` | not yet assigned | treated as a predictor at expansion |
#'
#' Formulas can be condensed by applying their specific role to individual runes
#' as a function/wrapper. For example, `y ~ .x(x1) + x2 + x3`. This would
#' signify that `x1` has the specific role of an _exposure_.
#'
#' Grouped variables are slightly different in that they are placed together in
#' a hierarchy or tier. To indicate the group and the tier, the shortcut can
#' have an `integer` following the `.g` (multi-digit tiers such as `.g10` are
#' allowed). If no number is given, then it is assumed they are all on the same
#' tier (tier zero). Ex: `y ~ x1 + .g1(x2) + .g1(x3)`
#'
#' # Transformations
#'
#' A term wrapped in a recognized transformation (see [term_transformations()])
#' keeps its full call as its name — `log(x)` remains `log(x)` so formulas
#' rebuild losslessly — and additionally records the wrapper in its
#' `transformation` field for downstream interpretation. Unrecognized calls
#' (e.g. `survival::Surv(time, status)`) are carried as opaque term names.
#'
#' # Pluralized Labeling Arguments
#'
#' For a single argument, e.g. for the `tm.formula()` method, such as to
#' identify variable __X__ as an exposure, a `formula` should be given with the
#' term of interest on the *LHS*, and the description or instruction on the
#' *RHS*. This would look like `role = "exposure" ~ X`.
#'
#' For the arguments that would be dispatched for objects that are plural, e.g.
#' containing multiple terms, each `formula()` should be placed within a
#' `list()`. For example, the __role__ argument would be written:
#'
#' `role = list(X ~ "exposure", M ~ "mediator", C ~ "confounder")`
#'
#' Further implementation details can be seen in the implementation of
#' [labeled_formulas_to_named_list()].
#'
#' @section Printing colors:
#'
#' Term printing uses ANSI colors from `cli`, so the user's console or IDE theme
#' chooses how each named color appears. Set `options(recast.color = FALSE)` to
#' disable colors.
#'
#' @param x An object that can be coerced to a `tm` object.
#'
#' @param role Specific roles the variable plays within the formula. Please see
#'   the _Roles_ section for the taxonomy: outcome, exposure, predictor,
#'   confounder, mediator, interaction, strata, random, unknown.
#'
#' @param side Which side of a formula should the term be on. Options are
#'   `c("left", "right", "meta", "unknown")`. The _meta_ option refers to a term
#'   that may apply globally to other terms (e.g. strata, random effects).
#'
#' @param label Display-quality label describing the variable
#'
#' @param group Grouping variable name for modeling or placing terms together.
#'   An integer value is given to identify which group the term will be in.
#'
#' @param type Type of variable, either categorical (qualitative) or
#'   continuous (quantitative)
#'
#' @param distribution How the variable itself is more specifically
#'   subcategorized, e.g. ordinal, continuous, dichotomous, etc
#'
#' @param description Option for further descriptions or definitions needed for
#'   the tm, potentially part of a data dictionary
#'
#' @param transformation Modification of the term to be applied when
#'   combining with data. See [term_transformations()] for the recognized
#'   vocabulary.
#'
#' @param level The observed levels of a categorical term, given as a
#'   `character` vector (or a `list` of such vectors when creating several
#'   terms at once). Usually stamped on by [set_data()] once a term has met a
#'   dataset.
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return A `tm` object, which is a series of individual terms with
#'   corresponding attributes, including the role, formula side, label,
#'   grouping, levels, and other related features.
#'
#' @name tm
#' @export
tm <- function(x = unspecified(), ...) {
	UseMethod("tm", object = x)
}

#' @rdname tm
#' @export
tm.character <- function(x,
												 role = character(),
												 side = character(),
												 label = character(),
												 group = integer(),
												 type = character(),
												 distribution = character(),
												 description = character(),
												 transformation = character(),
												 level = list(),
												 ...) {

	# Early Break if needed
	stopifnot("Missing/NA value not accepted for `tm` object" = !is.na(x))
	if (length(x) == 0) {
		return(new_tm())
	}

	# Casting
	x <- vec_cast(x, character())
	role <- vec_cast(role, character())
	side <- vec_cast(side, character())
	label <- vec_cast(label, character())
	group <- vec_cast(group, integer())
	description <- vec_cast(description, character())
	type <- vec_cast(type, character())
	distribution <- vec_cast(distribution, character())
	transformation <- vec_cast(transformation, character())
	if (is.character(level)) {
		level <- list(level)
	}

	new_tm(
		term = x,
		side = tm_field(side, length(x), default = "unknown"),
		role = tm_field(role, length(x), default = "unknown"),
		label = tm_field(label, length(x)),
		group = tm_field(group, length(x), default = NA_integer_),
		description = tm_field(description, length(x)),
		type = tm_field(type, length(x)),
		distribution = tm_field(distribution, length(x)),
		transformation = tm_field(transformation, length(x)),
		level = level
	)
}

#' @rdname tm
#' @importFrom stats formula
#' @export
tm.formula <- function(x,
											 role = formula(),
											 label = formula(),
											 group = formula(),
											 type = formula(),
											 distribution = formula(),
											 description = formula(),
											 transformation = formula(),
											 ...) {

	# Early Break if needed
	if (length(x) == 0) {
		return(new_tm())
	}

	# Validate the labeling arguments and convert them to named lists
	namedArgs <- list(
		role = role,
		label = label,
		group = group,
		type = type,
		distribution = distribution,
		description = description,
		transformation = transformation
	)
	for (nm in names(namedArgs)) {
		if (!inherits(namedArgs[[nm]], c("list", "formula"))) {
			stop("`", nm, "` needs to inherit from `c(list, formula)`.",
					 call. = FALSE)
		}
	}
	namedArgs <- lapply(namedArgs, labeled_formulas_to_named_list)

	# Walk the formula tree into a table of term records, then finalize roles
	parsed <- parse_formula_terms(x)
	parsed <- demote_orphan_roles(parsed)
	parsed <- expand_shortcut_interactions(parsed)
	parsed <- apply_default_roles(parsed)
	parsed <- apply_term_arguments(parsed, namedArgs)

	# Meta terms apply across a formula rather than sitting on one side
	parsed$side[parsed$role %in% c("strata", "random")] <- "meta"

	new_tm(
		term = parsed$term,
		side = parsed$side,
		role = parsed$role,
		label = parsed$label,
		group = parsed$group,
		description = parsed$description,
		type = parsed$type,
		distribution = parsed$distribution,
		transformation = parsed$transformation
	)
}

#' @rdname tm
#' @export
tm.fmls <- function(x, ...) {
	key_terms(x)
}

#' @rdname tm
#' @export
tm.tm <- function(x, ...) {
	x
}

#' @rdname tm
#' @export
tm.default <- function(x = unspecified(), ...) {
	# Early break
	if (length(x) == 0) {
		return(new_tm())
	}

	stop("`tm()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE
	)
}

### Formula parsing -------------------------------------------------------------

#' Walk a formula's syntax tree into term records
#'
#' Returns a `data.frame` with one row per term: term, side, role, group,
#' transformation. Roles/groups here are only those explicitly declared by
#' shortcut runes (`.x()`, `.g1()`, ...) or structure (`a:b`); defaults are
#' assigned later.
#'
#' @keywords internal
#' @noRd
parse_formula_terms <- function(x) {

	stopifnot(inherits(x, "formula"))

	if (length(x) == 3) {
		records <- c(
			collect_formula_terms(x[[2]], side = "left"),
			collect_formula_terms(x[[3]], side = "right")
		)
	} else {
		records <- collect_formula_terms(x[[2]], side = "right")
	}

	data.frame(
		term = vapply(records, function(.x) .x$term, character(1)),
		side = vapply(records, function(.x) .x$side, character(1)),
		role = vapply(records, function(.x) .x$role, character(1)),
		group = vapply(records, function(.x) .x$group, integer(1)),
		transformation = vapply(records, function(.x) .x$transformation, character(1)),
		label = NA_character_,
		type = NA_character_,
		distribution = NA_character_,
		description = NA_character_,
		stringsAsFactors = FALSE
	)
}

#' @keywords internal
#' @noRd
term_record <- function(term,
												side,
												role = NA_character_,
												group = NA_integer_,
												transformation = NA_character_) {
	list(
		term = term,
		side = side,
		role = role,
		group = group,
		transformation = transformation
	)
}

#' Recursive descent through one side of a formula
#' @keywords internal
#' @noRd
collect_formula_terms <- function(expr, side) {

	# Leaves: symbols and literals are terms as-is
	if (!is.call(expr)) {
		return(list(term_record(deparse1(expr), side)))
	}

	fn <- deparse1(expr[[1]])

	# Additive structure recurses; unary +/- and parentheses pass through
	if (fn %in% c("+", "-") && length(expr) == 3) {
		return(c(
			collect_formula_terms(expr[[2]], side),
			collect_formula_terms(expr[[3]], side)
		))
	}
	if (fn %in% c("+", "-") && length(expr) == 2) {
		return(collect_formula_terms(expr[[2]], side))
	}
	if (fn == "(") {
		return(collect_formula_terms(expr[[2]], side))
	}

	# `a * b` expands to `a + b + a:b`
	if (fn == "*" && length(expr) == 3) {
		product <- paste0(deparse1(expr[[2]]), ":", deparse1(expr[[3]]))
		return(c(
			collect_formula_terms(expr[[2]], side),
			collect_formula_terms(expr[[3]], side),
			list(term_record(product, side, role = "interaction"))
		))
	}

	# Explicit `a:b` terms are interactions in whole
	if (fn == ":") {
		return(list(term_record(deparse1(expr), side, role = "interaction")))
	}

	# Hierarchical syntax: `(1 | id)` marks a random intercept on `id`;
	# random slopes such as `(wt | id)` are carried whole
	if (fn == "|" && length(expr) == 3) {
		if (identical(expr[[2]], 1) || identical(expr[[2]], 1L)) {
			return(list(term_record(deparse1(expr[[3]]), side, role = "random")))
		}
		return(list(term_record(deparse1(expr), side, role = "random")))
	}

	# Engine-native stratification, e.g. `survival::strata(site)` in a coxph
	# formula, is *conditioning within one model* — a different mechanism from
	# the grammar's `.s()`, which splits the data into separate fits. The call
	# passes through whole (the engine understands it; it rides the formula
	# like any covariate), with its origin traced in `transformation`.
	if (fn %in% c("strata", "survival::strata")) {
		return(list(
			term_record(deparse1(expr), side, transformation = "strata")
		))
	}

	# Role shortcut runes, e.g. `.x(term)`
	if (fn %in% unlist(.roles)) {
		roleName <- names(.roles)[match(fn, unlist(.roles))]
		inner <- collect_formula_terms(expr[[2]], side)
		return(lapply(inner, function(.x) {
			.x$role <- roleName
			.x
		}))
	}

	# Group tier runes: `.g(term)` (tier 0) or `.g<n>(term)` for any n
	if (grepl("^\\.g[0-9]*$", fn)) {
		tier <- sub("^\\.g", "", fn)
		tier <- if (nzchar(tier)) as.integer(tier) else 0L
		inner <- collect_formula_terms(expr[[2]], side)
		return(lapply(inner, function(.x) {
			.x$group <- tier
			.x
		}))
	}

	# Recognized transformations keep their full call as the term name
	if (fn %in% .transformations) {
		return(list(term_record(deparse1(expr), side, transformation = fn)))
	}

	# Any other call (e.g. `Surv(time, status)`, `cluster(id)`) is opaque
	list(term_record(deparse1(expr), side))
}

#' Interaction and mediation shortcuts require an exposure to anchor to;
#' without one they demote to plain predictors (with a warning)
#' @keywords internal
#' @noRd
demote_orphan_roles <- function(parsed) {

	hasExposure <- any(parsed$role == "exposure", na.rm = TRUE)
	if (hasExposure) {
		return(parsed)
	}

	# Only shortcut-declared interactions demote; explicit `a:b` terms stand
	shortcutInt <- parsed$role == "interaction" & !grepl(":", parsed$term)
	shortcutInt[is.na(shortcutInt)] <- FALSE
	if (any(shortcutInt)) {
		warning(
			"The interaction term(s) `",
			paste0(parsed$term[shortcutInt], collapse = " + "),
			"` was/were specified but an exposure variable was not found. ",
			"The result will treat the term(s) as regular predictor variables."
		)
		parsed$role[shortcutInt] <- "predictor"
	}

	mediators <- parsed$role == "mediator"
	mediators[is.na(mediators)] <- FALSE
	if (any(mediators)) {
		warning(
			"The mediator term(s) `",
			paste0(parsed$term[mediators], collapse = " + "),
			"` was/were specified but an exposure variable was not found. ",
			"The result will treat these term(s) as regular predictor variables."
		)
		parsed$role[mediators] <- "predictor"
	}

	parsed
}

#' Cross each `.i()` shortcut with each exposure, creating explicit `x:i`
#' product terms placed directly after their base term, grouped so the pair
#' travels together through formula expansion
#' @keywords internal
#' @noRd
expand_shortcut_interactions <- function(parsed) {

	exposures <- parsed$term[which(parsed$role == "exposure")]
	intIdx <- which(parsed$role == "interaction" & !grepl(":", parsed$term))

	if (length(exposures) == 0 || length(intIdx) == 0) {
		return(parsed)
	}

	pieces <- list()
	lastCut <- 0L
	for (i in intIdx) {

		# The base term and its products share a group tier so that patterns
		# keep them together; allocate the next free tier if none is set
		if (is.na(parsed$group[i])) {
			used <- parsed$group[!is.na(parsed$group)]
			parsed$group[i] <- if (length(used) == 0) 0L else max(used) + 1L
		}
		tier <- parsed$group[i]

		products <- lapply(exposures, function(e) {
			message(
				"Interaction term `", parsed$term[i],
				"` was applied to exposure term `", e, "`"
			)
			data.frame(
				term = paste0(e, ":", parsed$term[i]),
				side = parsed$side[i],
				role = "interaction",
				group = tier,
				transformation = NA_character_,
				label = NA_character_,
				type = NA_character_,
				distribution = NA_character_,
				description = NA_character_,
				stringsAsFactors = FALSE
			)
		})

		pieces <- c(
			pieces,
			list(parsed[(lastCut + 1):i, , drop = FALSE]),
			products
		)
		lastCut <- i
	}
	if (lastCut < nrow(parsed)) {
		pieces <- c(pieces, list(parsed[(lastCut + 1):nrow(parsed), , drop = FALSE]))
	}

	out <- do.call(rbind, pieces)
	rownames(out) <- NULL
	out
}

#' Terms without a declared role receive one from formula position
#' @keywords internal
#' @noRd
apply_default_roles <- function(parsed) {

	missingRole <- is.na(parsed$role)
	parsed$role[missingRole & parsed$side == "left"] <- "outcome"
	parsed$role[missingRole & parsed$side == "right" &
								grepl(":", parsed$term)] <- "interaction"
	parsed$role[is.na(parsed$role) & parsed$side == "right"] <- "predictor"

	parsed
}

#' Explicit labeling arguments override what was parsed
#' @keywords internal
#' @noRd
apply_term_arguments <- function(parsed, namedArgs) {

	for (field in names(namedArgs)) {
		vals <- namedArgs[[field]]
		for (nm in names(vals)) {
			hit <- parsed$term == nm
			if (any(hit)) {
				if (field == "group") {
					parsed$group[hit] <- as.integer(vals[[nm]])
				} else {
					parsed[[field]][hit] <- as.character(vals[[nm]])
				}
			}
		}
	}

	parsed
}

### Construction ----------------------------------------------------------------

#' Recycle or default a field against the number of terms
#' @keywords internal
#' @noRd
tm_field <- function(value, n, default = NA_character_) {
	if (length(value) == 0) {
		return(rep(default, n))
	}
	vec_recycle(value, n)
}

#' Initialize new term record vector
#' @keywords internal
#' @noRd
new_tm <- function(term = character(),
									 side = character(),
									 role = character(),
									 label = character(),
									 group = integer(),
									 type = character(),
									 distribution = character(),
									 description = character(),
									 transformation = character(),
									 level = list(),
									 order = integer()) {

	# Validation
	vec_assert(term, ptype = character())
	stopifnot(is.list(level))

	n <- length(term)
	if (n > 0) {
		side <- vec_recycle(tm_field(side, n, "unknown"), n)
		role <- vec_recycle(tm_field(role, n, "unknown"), n)
		label <- vec_recycle(tm_field(label, n), n)
		group <- vec_recycle(tm_field(group, n, NA_integer_), n)
		type <- vec_recycle(tm_field(type, n), n)
		distribution <- vec_recycle(tm_field(distribution, n), n)
		description <- vec_recycle(tm_field(description, n), n)
		transformation <- vec_recycle(tm_field(transformation, n), n)
		if (length(level) == 0) {
			level <- rep(list(character(0)), n)
		}
		level <- vec_recycle(level, n)
		order <- rep(0L, n)
	}

	vec_assert(role, ptype = character())
	vec_assert(side, ptype = character())
	vec_assert(label, ptype = character())
	vec_assert(group, ptype = integer())
	vec_assert(description, ptype = character())
	vec_assert(type, ptype = character())
	vec_assert(distribution, ptype = character())
	vec_assert(transformation, ptype = character())
	vec_assert(order, ptype = integer())

	new_rcrd(
		list(
			"term" = term,
			"role" = role,
			"side" = side,
			"label" = label,
			"group" = group,
			"description" = description,
			"type" = type,
			"distribution" = distribution,
			"transformation" = transformation,
			"level" = level,
			"order" = order
		),
		class = "tm"
	)
}

#' @rdname tm
#' @export
is_tm <- function(x) {
	inherits(x, "tm")
}

#' @export
format.tm <- function(x,
											color = recast_color(),
											...) {

	if (vec_size(x) == 0) {
		return(character())
	}

	tms <- vec_data(x)
	mdl_gt_color_roles(tms$term, tms$role, color = color)
}

#' @export
obj_print_data.tm <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_tm()
	} else if (vec_size(x) > 1) {
		cat(format(x, ...), sep = "\n")
	} else {
		cat(format(x, ...))
	}
}

#' @export
vec_ptype_full.tm <- function(x, ...) {
	"term"
}

#' @export
vec_ptype_abbr.tm <- function(x, ...) {
	"tm"
}

#' @export
methods::setOldClass(c("tm", "vctrs_rcrd"))

### Coercion methods -----------------------------------------------------------

#' @export
vec_ptype2.tm.tm <- function(x, y, ...) x

#' @export
vec_cast.tm.tm <- function(x, to, ...) x

# CHARACTER

#' @export
vec_ptype2.tm.character <- function(x, y, ...) y # X = tm

#' @export
vec_ptype2.character.tm <- function(x, y, ...) x # X = character

#' @export
vec_cast.tm.character <- function(x, to, ...) {
	# order is flipped, such that `x` is character
	# Cast from character into terms
	attributes(x) <- NULL
	x[[1]]
}

#' @export
vec_cast.character.tm <- function(x, to, ...) {
	# order is flipped, such that `x` is tm
	attributes(x) <- NULL
	x[[1]]
}

# FORMULA

#' @export
vec_ptype2.tm.formula <- function(x, y, ...) {
	x # A formula and term should vectorize to a term object
}

#' @export
vec_ptype2.formula.tm <- function(x, y, ...) {
	y # A formula and term should vectorize to a term object
}

#' @export
vec_cast.tm.formula <- function(x, to, ...) {
	# order is flipped, such that `x` is formula
	# Cast from formula into terms
	tm(x)
}

#' @export
vec_cast.formula.tm <- function(x, to, ...) {
	# order is flipped, such that `x` is tm
	stats::formula(x)
}

# FMLS

#' @export
vec_ptype2.tm.fmls <- function(x, y, ...) {
	x # A fmls and term should vectorize to a term object
}

#' @export
vec_ptype2.fmls.tm <- function(x, y, ...) {
	y # A fmls and term should vectorize to a term object
}

#' @export
vec_cast.tm.fmls <- function(x, to, ...) {
	# order is flipped, such that `x` is fmls
	# Cast from fmls into terms
	tm(x)
}

#' @export
vec_cast.fmls.tm <- function(x, to, ...) {
	# order is flipped, such that `x` is tm
	# Cast from `tm` into `fmls`
	fmls(x)
}

### Term Helpers ---------------------------------------------------------------

#' @export
formula.tm <- function(x, env = parent.frame(), ...) {

	# Create vec_data / proxy to help re-arrange terms as needed
	# Lose information when converting to just character
	y <- vec_proxy(x)

	# Create basic structure for formula
	# 	Handle mediator equations differently than standard formulas
	#		Results a left and right hand side
	if ("mediator" %in% y$role & !("outcome" %in% y$role)) {
		left <- y[y$role == "mediator", ]$term
		right <- y[y$side == "right" & y$role != "mediator", ]$term
	} else {
		left <- y[y$side == "left", ]$term
		right <- y[y$side == "right", ]$term
	}

	# Random-effect terms render in their hierarchical form; terms that carry
	# their own grouping syntax (random slopes) are wrapped as-is
	random <- y[y$role == "random", ]$term
	if (length(random) > 0) {
		right <- c(
			right,
			ifelse(
				grepl("|", random, fixed = TRUE),
				paste0("(", random, ")"),
				paste0("(1 | ", random, ")")
			)
		)
	}

	f <- paste0(paste0(left, collapse = " + "),
							sep = " ~ ",
							paste0(right, collapse = " + "))

	stats::formula(f, env = env)

}

#' Update `tm` objects
#'
#' This updates properties or attributes of a `tm` vector. This only updates
#' objects that already exist.
#'
#' @param object A `tm` object
#'
#' @param ... A series of `field = term ~ value` pairs that represent the
#'   attribute to be updated. Can have a value of `NA` if the goal is to
#'   remove an attribute or property.
#'
#' @return A `tm` object with updated attributes
#' @export
update.tm <- function(object, ...) {

	# Early break
	if (missing(..1) | length(..1) == 0) {
		return(object)
	}

	# Get update options and original data
	dots <- list(...)
	if (length(dots) == 1 && is.list(dots[[1]])) {
		dots <- dots[[1]]
	}
	termData <- vec_proxy(object)

	# Property management loop
	for (i in names(termData[-1])) {
		if (!is.null(dots[[i]])) {
			newProps <-
				dots[[i]] |>
				labeled_formulas_to_named_list()

			# Term management loop
			for (j in names(newProps)) {
				if (is.list(termData[[i]])) {
					termData[[i]][termData$term == j] <- list(newProps[[j]])
				} else {
					termData[termData$term == j, i] <- newProps[[j]]
				}
			}
		}
	}

	# Restore and return
	vec_restore(termData, to = tm())
}

#' Extending `dplyr` for `tm` class
#'
#' The `filter()` function extension subsets `tm` that satisfy set conditions.
#' To be retained, the `tm` object must produce a value of `TRUE` for all conditions.
#' Note that when a condition evaluates to `NA`, the row will be dropped, unlike
#' base subsetting with `[`.
#'
#' @return An object of the same type as `.data`. The output as the following properties:
#'
#' * `tm` objects are a subset of the input, but appear in the same order
#'
#' * Underlying `data.frame` columns are not modified
#'
#' * Underlying `data.frame` object's attributes are preserved
#'
#' @inheritParams dplyr::filter
#'
#' @seealso [dplyr::filter()] for examples of generic implementation
#'
#' @name dplyr_extensions
#' @importFrom dplyr filter
#' @export
filter.tm <- function(.data, ...) {

	x <-
		.data |>
		vec_proxy() |>
		dplyr::filter(...)

	vec_restore(x, to = tm())

}

#' Describe attributes of a `tm` vector
#'
#' The printed form of a `fmls` object shows roles through term coloring
#' rather than spelling them out; `describe()` is the explicit route to that
#' information (e.g. `describe(f, "role")` on a `fmls` object).
#'
#' @param x A vector of `tm` objects, or a `fmls` object (whose key terms are
#'   described)
#'
#' @param property A character vector of the following attributes of a `tm`
#'   object: role, side, label, group, description, type, distribution, level
#'
#' @return A list of `term = property` pairs, where the term is the name of the
#'   element (e.g. could be the `role' of the term).
#'
#' @examples
#' f <- .o(output) ~ .x(input) + .m(mediator) + random
#' t <- tm(f)
#' describe(t, "role")
#'
#' describe(fmls(mpg ~ .x(wt) + hp), "role")
#'
#' @export
describe <- function(x, property) {

	if (is_fmls(x)) {
		x <- tm(x)
	}
	validate_class(x, "tm")
	fieldNames <- fields(x)
	if (!(property %in% fieldNames)) {
		stop("`", property, "` is not an accessible property for this `tm` object.")
	}

	y <- vec_proxy(x)
	z <- y[[property]]

	# A term may carry several definitions (a plain predictor in one family,
	# the mediator of another), so each term's entry collects the property
	# across its definitions, in order of appearance
	terms <- unique(y$term)
	out <- lapply(terms, function(t) {
		v <- z[y$term == t]
		if (is.list(v) && length(v) == 1) v[[1]] else unname(v)
	})
	names(out) <- terms

	# Return in list format
	out

}
