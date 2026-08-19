### Formula Helpers -----------------------------------------------------------

#' Tools for working with formula-like objects
#' 
#' @return A `character` describing part of a `formula` or `fmls` object
#'
#' @param x A formula-like object
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @name formula_helpers
NULL

#' @rdname formula_helpers
#' @export
lhs <- function(x, ...) {
	UseMethod("lhs", object = x)
}

#' @rdname formula_helpers
#' @export
rhs <- function(x, ...) {
	UseMethod("rhs", object = x)
}

#' Split one side of a formula on its additive structure
#'
#' Walks the expression tree, so only true `+`/`-` joins split; a call that
#' merely contains one (`I(a + b)`, a label like `"Weight (+/- SD)"`) stays
#' one chunk. Each chunk deparses as written (runes such as `.x(wt)` keep
#' their text).
#' @keywords internal
#' @noRd
split_additive <- function(expr) {
	if (is.call(expr) &&
			deparse1(expr[[1]]) %in% c("+", "-") &&
			length(expr) == 3) {
		c(split_additive(expr[[2]]), split_additive(expr[[3]]))
	} else {
		deparse1(expr)
	}
}

#' @rdname formula_helpers
#' @export
rhs.formula <- function(x, ...) {

	# Handles name, call, and character options
	if (inherits(x[[length(x)]], 'character')) {
		y <-
			x[[length(x)]] |>
			trimws()
	} else {
		y <-
			x[[length(x)]] |>
			split_additive() |>
			trimws() |>
			{
				\(.x) gsub('"', "", .x)
			}()
	}

	# Handle special interaction terms in original formula
	# Will expand from `a * b` -> `a + b + a:b`
	pos <- grep("\\*", y)
	npos <- grep("\\*", y, invert = TRUE)

	ints <- character()
	if (length(pos) > 0) {
	  ints <-
	    y[pos] |>
	    strsplit("\\*") |>
	    unlist() |>
	    trimws() |>
	    {
	      \(.x) c(.x[1], .x[2], paste0(.x[1], ":", .x[2]))
	    }()
	}

	# Return
	c(ints, y[npos])

}

#' @rdname formula_helpers
#' @export
lhs.formula <- function(x, ...) {
	if (length(x) == 2) {
		y <- character()
	} else if (length(x) == 3) {
		y <-
			x[[2]] |>
			split_additive() |>
			trimws() |>
			{
				\(.x) gsub('"', "", .x)
			}()
	}

	y
}


#' @keywords internal
#' @noRd
has_cli <- function() {
  isTRUE(requireNamespace("cli", quietly = TRUE))
}

#' Package color option with the old option names as compatibility fallbacks
#' @keywords internal
#' @noRd
recast_color <- function(default = TRUE) {
	getOption(
		"recast.color",
		getOption("epigram.color", getOption("mesa.color", default))
	)
}

#' A stable content-derived dataset id for frames passed as inline
#' expressions rather than names (`data_<hash>`); identical content gets the
#' identical id at [fit()], [model_table()], and [attach_data()], so the
#' pieces meet without the user retyping anything
#' @keywords internal
#' @noRd
data_content_name <- function(data) {
	paste0("data_", substr(rlang::hash(data), 1, 8))
}

#' Convert labeling formulas to named lists
#'
#' @description
#' Take list of formulas, or a similar construct, and returns a named list. The
#' convention here is similar to reading from left to right, where the name or
#' position is the term is the on the *LHS* and the output label or target
#' instruction is on the *RHS*.
#'
#' If no label is desired, then the *LHS* can be left empty, such as `~ x`.
#'
#' @return A named list with the index as a `character` representing the term
#'   or variable of interest, and the value at that position as a `character`
#'   representing the label value.
#'
#' @param x An argument that may represent a formula to label variables, or can
#'   be converted to one. This includes, `list`, `formula`, or
#'   `character` objects. Other types will error.
#'
#' @export
labeled_formulas_to_named_list <- function(x) {

	# Check to see if its a single formula or a list of formulas
	stopifnot("Should be applied to individual or list of formulas" =
							inherits(x, c("list", "formula", "character")))

	# Empty, list, or formula management
	if (length(x) == 0) { # If an empty formula or list, return an empty list
		y <- list()
	} else if (inherits(x, "formula")) { # If a single formula
		nm <- lhs(x)
		val <- labeled_formula_value(x)
		# If unnamed, then give it the same value as the name
		if (length(nm) == 0) {
			nm <- as.character(val)[1]
		}
		if (length(val) > 1) {
			y <- stats::setNames(list(val), nm)
		} else {
			names(val) <- nm
			y <- as.list(val)
		}
	} else if (inherits(x, "list")) { # If a list that contains formulas
		# Confirm each item is formula
		stopifnot("If a list is provided, each element must be a `formula`"
							= all(sapply(x, inherits, "formula")))

		y <- sapply(x, function(.x) {
			nm <- lhs(.x)
			val <- labeled_formula_value(.x)
			# If unnamed, then give it the same value as the name
			if (length(nm) == 0) {
				nm <- as.character(val)[1]
			}
			if (length(val) > 1) {
				return(stats::setNames(list(val), nm))
			}
			if (grepl("^[[:digit:]]$", val)) {
				val <- as.integer(val)
			}
			names(val) <- nm
			.y <- as.list(val)
		})
	} else if (inherits(x, "character")) {
		nm <- x
		val <- x
		names(val) <- nm
		y <- as.list(val)
	}

	# Return
	y
}

#' Extract the value a labeling formula assigns
#'
#' Vector values written as calls, e.g. `am ~ c("Manual", "Automatic")`, are
#' evaluated in the formula's environment; anything else falls back to the
#' deparsed right-hand side.
#'
#' @keywords internal
#' @noRd
labeled_formula_value <- function(x) {
	rhsExpr <- x[[length(x)]]
	if (is.call(rhsExpr) && identical(rhsExpr[[1]], as.name("c"))) {
		eval(rhsExpr, envir = environment(x))
	} else {
		rhs(x)
	}
}
