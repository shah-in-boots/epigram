# Meeting the data ---------------------------------------------------------------

#' Stamp data-derived attributes onto terms
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' Terms begin as names; once they meet a dataset they gain a character. The
#' `set_data()` function inspects each term's column in `data` and stamps on
#' its `type` (categorical or continuous), `distribution` (dichotomous,
#' ordinal, nominal, binary, or continuous), and observed `level`s. This is
#' the step that makes strata and interactions *data-aware* — a stratifying
#' term knows its levels before anything is fit.
#'
#' Binary and categorical are different definitions: a *numeric* 0/1 column
#' (e.g. a survival event indicator) is stamped `binary` and keeps the
#' continuous type, matching how a model treats it (one quantitative
#' coefficient); only factor-like columns are categorical and carry levels.
#' Strata are the exception — a stratum splits the data by its observed
#' values whatever their type, so its levels are always stamped.
#'
#' Terms wrapped in transformations (e.g. `log(x)`) are classified from their
#' underlying variable. Terms without a matching column are left untouched.
#'
#' @param x A `tm` or `fmls` object
#'
#' @param data A `data.frame` containing the variables the terms refer to
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return An object of the same class as `x` with `type`, `distribution`,
#'   and `level` fields filled in from the data
#'
#' @examples
#' t <- tm(mpg ~ wt + .s(am))
#' t <- set_data(t, mtcars)
#' describe(t, "level")
#'
#' @export
set_data <- function(x, data, ...) {
	UseMethod("set_data", object = x)
}

#' @rdname set_data
#' @export
set_data.tm <- function(x, data, ...) {

	checkmate::assert_data_frame(data)
	tmTab <- vec_proxy(x)

	for (i in seq_len(nrow(tmTab))) {

		# Terms may be plain names or calls such as `log(x)`; classify from the
		# underlying variable in either case
		varName <- tmTab$term[i]
		if (!varName %in% names(data)) {
			vars <- tryCatch(
				all.vars(str2lang(varName)),
				error = function(e) character(0)
			)
			vars <- vars[vars %in% names(data)]
			if (length(vars) == 0) {
				next
			}
			varName <- vars[1]
		}

		values <- data[[varName]]

		# A declared transformation changes the term's character: `factor(cyl)`
		# is categorical even when `cyl` is stored as a number
		transformation <- tmTab$transformation[i]
		if (!is.na(transformation) && transformation %in% c("factor", "ordered")) {
			values <- factor(values, ordered = transformation == "ordered")
		}

		# Binary and categorical are different definitions: a numeric 0/1 column
		# (e.g. a survival event indicator) is *binary* and models as one
		# quantitative coefficient, while a two-level factor is *dichotomous*
		# and models as level contrasts; only the latter is a categorical type
		n <- length(stats::na.omit(unique(values)))
		tmTab$distribution[i] <-
			if (is.ordered(values)) {
				"ordinal"
			} else if (is.numeric(values)) {
				if (n == 2) "binary" else "continuous"
			} else if (n == 2) {
				"dichotomous"
			} else {
				"nominal"
			}
		tmTab$type[i] <-
			if (tmTab$distribution[i] %in% c("continuous", "binary")) {
				"continuous"
			} else {
				"categorical"
			}
		# Levels are stamped for categorical terms, and for strata regardless of
		# type: a stratum splits the data by its observed values either way
		if (tmTab$type[i] == "categorical" || tmTab$role[i] == "strata") {
			tmTab$level[[i]] <- levels(factor(values))
		}
	}

	vec_restore(tmTab, to = tm())
}

#' @rdname set_data
#' @export
set_data.fmls <- function(x, data, ...) {

	tmTab <- attr(x, "termTable")
	updated <-
		vec_restore(tmTab, to = tm()) |>
		set_data(data)
	attr(x, "termTable") <- vec_proxy(updated)

	x
}

# Fluent verbs ---------------------------------------------------------------

#' Fluent verbs for playing with formula families
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' A family of formulas should feel like something to play with: pick terms up
#' off a dataset, snap them together, swap the pieces, and watch the family of
#' models unfold. These pipeable verbs each take a `fmls` object and return a
#' modified one, so a modeling deck can be grown interactively:
#'
#' - `add_strata()` / `remove_strata()`: mark terms as stratifying variables
#'   ([fit()] will fit one model per stratum level)
#'
#' - `add_terms()` / `remove_terms()`: add or drop covariates from every
#'   formula in the family
#'
#' - `swap_outcome()`: exchange one outcome for another, keeping the rest of
#'   the family intact
#'
#' - `subset_data()`: record data-filtering instructions (e.g. `sex == "F"`)
#'   that [fit()] will apply, fitting the family once per subset
#'
#' Terms may be given as bare names or strings; a variable holding a name is
#' spliced in with `!!` (`add_strata(f, !!v)`), a vector of names with `!!!`.
#' For `swap_outcome()`, a two-sided formula `old ~ new` swaps a specific
#' outcome; a bare name is allowed when the family has a single outcome.
#'
#' @param x A `fmls` object
#'
#' @param ... Terms as bare names or strings; for `subset_data()`, one or more
#'   logical expressions in the data's variables; for `remove_strata()`, if
#'   empty, all strata are removed
#'
#' @param spec For `swap_outcome()`, either a two-sided formula `old ~ new` or
#'   a bare name/string of the replacement outcome
#'
#' @param role For `add_terms()`, the role the new terms should carry
#'   (defaults to `"predictor"`)
#'
#' @return A modified `fmls` object
#'
#' @examples
#' f <- fmls(mpg ~ .x(wt) + hp)
#' f |>
#'   add_strata(am) |>
#'   add_terms(cyl) |>
#'   subset_data(disp > 100)
#'
#' # A name held in a variable is spliced in
#' v <- "am"
#' add_strata(f, !!v)
#'
#' @name fluent_verbs
NULL

#' Collect bare names or strings from dots
#' @keywords internal
#' @noRd
vars_from_dots <- function(...) {
	vapply(rlang::ensyms(...), rlang::as_string, character(1))
}

#' @rdname fluent_verbs
#' @export
add_strata <- function(x, ...) {
	UseMethod("add_strata", object = x)
}

#' @rdname fluent_verbs
#' @export
add_strata.fmls <- function(x, ...) {

	vars <- vars_from_dots(...)
	tmTab <- attr(x, "termTable")
	inst <- attr(x, "instructions")
	fmMat <- vec_data(x)

	for (v in vars) {
		if (v %in% tmTab$term) {
			# Promoting a covariate changes every adjustment set in the family,
			# which a caption written earlier does not know
			was <- paste(unique(tmTab$role[tmTab$term == v]), collapse = "/")
			if (was != "strata") {
				message(
					"`", v, "` was a ", was, " of this family; it now stratifies ",
					"every formula instead, so the adjustment sets have changed."
				)
			}
			tmTab$role[tmTab$term == v] <- "strata"
			tmTab$side[tmTab$term == v] <- "meta"
		} else {
			newRow <- vec_proxy(tm(v, role = "strata", side = "meta"))
			tmTab <- rbind(tmTab, newRow)
		}
		# The stratum applies to every formula of this family; membership in
		# the formula matrix is what keeps it scoped when families combine
		fmMat[[v]] <- 1
	}

	new_fmls(fmMat, termTable = tmTab, instructions = inst)
}

#' @rdname fluent_verbs
#' @export
remove_strata <- function(x, ...) {
	UseMethod("remove_strata", object = x)
}

#' @rdname fluent_verbs
#' @export
remove_strata.fmls <- function(x, ...) {

	vars <- vars_from_dots(...)
	tmTab <- attr(x, "termTable")
	inst <- attr(x, "instructions")
	fmMat <- vec_data(x)

	drop <- tmTab$role == "strata"
	if (length(vars) > 0) {
		drop <- drop & tmTab$term %in% vars
	}

	dropped <- tmTab$term[drop]
	fmMat <- fmMat[, !names(fmMat) %in% dropped, drop = FALSE]

	new_fmls(fmMat, termTable = tmTab[!drop, ], instructions = inst)
}

#' @rdname fluent_verbs
#' @export
add_terms <- function(x, ...) {
	UseMethod("add_terms", object = x)
}

#' @rdname fluent_verbs
#' @export
add_terms.fmls <- function(x, ..., role = "predictor") {

	vars <- vars_from_dots(...)
	tmTab <- attr(x, "termTable")
	inst <- attr(x, "instructions")
	fmMat <- vec_data(x)

	for (v in vars) {
		if (!v %in% tmTab$term) {
			newRow <- vec_proxy(tm(v, role = role, side = "right"))
			tmTab <- rbind(tmTab, newRow)
		}
		# The new term joins every formula in the family
		fmMat[[v]] <- 1
	}

	new_fmls(fmMat, termTable = tmTab, instructions = inst)
}

#' @rdname fluent_verbs
#' @export
remove_terms <- function(x, ...) {
	UseMethod("remove_terms", object = x)
}

#' @rdname fluent_verbs
#' @export
remove_terms.fmls <- function(x, ...) {

	vars <- vars_from_dots(...)
	tmTab <- attr(x, "termTable")
	inst <- attr(x, "instructions")
	fmMat <- vec_data(x)

	fmMat <- fmMat[, !names(fmMat) %in% vars, drop = FALSE]
	tmTab <- tmTab[!tmTab$term %in% vars, ]

	new_fmls(fmMat, termTable = tmTab, instructions = inst)
}

#' @rdname fluent_verbs
#' @export
swap_outcome <- function(x, spec) {
	UseMethod("swap_outcome", object = x)
}

#' @rdname fluent_verbs
#' @export
swap_outcome.fmls <- function(x, spec) {

	tmTab <- attr(x, "termTable")
	inst <- attr(x, "instructions")
	fmMat <- vec_data(x)
	outcomes <- tmTab$term[tmTab$role == "outcome"]

	# `spec` may be `old ~ new`, a bare name, or a string; a bare name must not
	# be evaluated (it usually names a column, not an object in scope)
	specExpr <- rlang::enexpr(spec)
	old <- NULL
	value <- tryCatch(spec, error = function(e) NULL)
	if (inherits(value, "formula")) {
		old <- lhs(value)
		new <- rhs(value)
	} else if (is.character(value) && length(value) == 1) {
		new <- value
	} else if (rlang::is_symbol(specExpr)) {
		new <- rlang::as_string(specExpr)
	} else {
		stop(
			"`spec` should be a two-sided formula `old ~ new`, ",
			"a bare name, or a string.",
			call. = FALSE
		)
	}

	if (is.null(old)) {
		if (length(outcomes) != 1) {
			stop(
				"The family has ", length(outcomes), " outcomes; ",
				"use `old ~ new` to say which one to swap.",
				call. = FALSE
			)
		}
		old <- outcomes
	}

	if (!old %in% outcomes) {
		stop("`", old, "` is not an outcome in this family.", call. = FALSE)
	}
	if (new %in% tmTab$term) {
		stop(
			"`", new, "` is already a term in this family; ",
			"remove it first if it should become the outcome.",
			call. = FALSE
		)
	}

	tmTab$term[tmTab$term == old & tmTab$role == "outcome"] <- new
	names(fmMat)[names(fmMat) == old] <- new

	new_fmls(fmMat, termTable = tmTab, instructions = inst)
}

#' @rdname fluent_verbs
#' @export
subset_data <- function(x, ...) {
	UseMethod("subset_data", object = x)
}

#' @rdname fluent_verbs
#' @export
subset_data.fmls <- function(x, ...) {

	subsets <- rlang::enquos(..., .named = TRUE)
	inst <- attr(x, "instructions")
	if (is.null(inst)) {
		inst <- list(subsets = list())
	}
	inst$subsets <- c(inst$subsets, subsets)

	attr(x, "instructions") <- inst
	x
}

# The deck -----------------------------------------------------------------------

#' Summarize the deck of formulas about to be fit
#' @keywords internal
#' @noRd
fmls_deck_header <- function(x) {

	tmTab <- attr(x, "termTable")
	inst <- attr(x, "instructions")
	n <- nrow(vec_data(x))

	lines <- paste0("<fmls: ", n, if (n == 1) " formula>" else " formulas>")

	# Only context the printed formulas cannot show: roles are visible in the
	# formula coloring (and retrievable with `describe(tm(x), "role")`), but
	# strata, random effects, and subsets do not sit on either formula side
	for (r in c("strata", "random")) {
		hits <- which(tmTab$role == r)
		if (length(hits) == 0) {
			next
		}
		vals <- vapply(hits, function(i) {
			lv <- if ("level" %in% names(tmTab)) tmTab$level[[i]] else character(0)
			if (r %in% c("strata", "random") && length(lv) > 0) {
				paste0(tmTab$term[i], " (", length(lv), " levels)")
			} else {
				tmTab$term[i]
			}
		}, character(1))
		lines <- c(lines, paste0("  ", r, ": ", paste(vals, collapse = ", ")))
	}

	if (!is.null(inst) && length(inst$subsets) > 0) {
		labels <- vapply(
			inst$subsets,
			function(q) deparse1(rlang::quo_get_expr(q)),
			character(1)
		)
		lines <- c(lines, paste0("  subsets: ", paste(labels, collapse = ", ")))
	}

	lines
}
