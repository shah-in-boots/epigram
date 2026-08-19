# Class definition -------------------------------------------------------------

#' Vectorized formulas
#'
#' @description
#'
#' This function defines a modified `formula` class that has been
#' vectorized. The `fmls` serves as a set of instructions or a _script_ for the
#' formula and its tm. It expands upon the functionality of formulas,
#' allowing for additional descriptions and relationships to exist between the
#' tm.
#'
#' @details
#'
#' This is not meant to supersede a [stats::formula()] object, but provide a
#' series of relationships that can be helpful in causal modeling. All `fmls`
#' can be converted to a traditional `formula` with ease. The base for this
#' object is built on the [tm()] object.
#'
#' # Patterns
#'
#' The expansion pattern allows for instructions on how the covariates should be
#' included in different formulas. Below, assuming that _x1_, _x2_, and _x3_ are
#' covariates...
#'
#' \deqn{y = x1 + x2 + x3}
#'
#' __Direct__:
#'
#' \deqn{y = x1 + x2 + x3}
#'
#' __Seqential__:
#'
#' \deqn{y = x1}
#' \deqn{y = x1 + x2}
#' \deqn{y = x1 + x2 + x3}
#'
#' __Parallel__:
#'
#' \deqn{y = x1}
#' \deqn{y = x2}
#' \deqn{y = x3}
#'
#' New patterns can be registered by name through [register_pattern()].
#'
#' # Mediation
#'
#' When a term carries the _mediator_ role (`.m()`), the expansion generates
#' the causal triad used to reason about mediation, alongside the covariates
#' already requested by the pattern:
#'
#' 1. `outcome ~ exposure + mediator + covariates` — the exposure effect with
#' the pathway through the mediator held open
#'
#' 1. `mediator ~ exposure` — the exposure's effect on the mediator itself
#'
#' 1. `outcome ~ mediator + exposure` — the mediator's effect on the outcome
#' in the presence of the exposure
#'
#' Comparing the exposure estimate across these formulas is what allows an
#' epidemiologist to judge how much of the total effect travels through the
#' mediator (per VanderWeele and Robins).
#'
#' # Combining
#'
#' Families of formulas combine with [c()] or [vctrs::vec_c()]. Each family
#' keeps its own definition of every term: `hp` may adjust one family as a
#' plain predictor and mediate another (`.m(hp)`), and after combining, each
#' formula still reads its own family's definition — definitions are told
#' apart by their role, and the merged term table holds one row per
#' definition. Only when the same term arrives twice in the *same* role but
#' with different decoration (a conflicting label, say) does the first
#' (left-most) definition win, with a message.
#'
#' @inheritSection tm Roles
#' @inheritSection tm Pluralized Labeling Arguments
#' @inheritSection tm Printing colors
#'
#' @param x Objects of the following types can be used as inputs
#'
#'   - `tm`
#'
#'   - `formula`
#'
#' @param pattern A `character` from the following choices for pattern
#'   expansion. This is how the formula will be expanded, and decides how the
#'   covariates will incorporated. See the details for further explanation.
#'
#'   - direct: the covariates will all be included in each formula
#'
#'   - sequential: the covariates will be added sequentially, one by one, or by groups, as indicated
#'
#'   - parallel: the covariates or groups of covariates will be placed in parallel
#'
#'   - fundamental: every formula will be decomposed to a single outcome and predictor in an atomic fashion
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return An object of class `fmls`
#' @name fmls
#' @importFrom rlang !!! :=
#' @export
fmls <- function(x = unspecified(),
								 pattern = c("direct",
								 						"sequential",
								 						"parallel",
								 						"fundamental"),
								 ...) {

	# Return early if nothing is given
	if (length(x) == 0) {
		return(new_fmls())
	}

	# Convert to term object if possible
	# Notably, if an interaction term is present, will be a separate variable
	validate_class(x, c("tm", "formula"))
	if (inherits(x, "formula")) {
		x <- tm(x)
	}

	# Check pattern against the registry (see `register_pattern()`)
	if (length(pattern) > 1) {
		pattern <- pattern[1] # Direct
	}
	if (!pattern %in% formula_patterns()) {
		stop("The pattern ",
				 deparse(pattern),
				 " is not registered. Available patterns: ",
				 paste(formula_patterns(), collapse = ", "),
				 call. = FALSE)
	}

	## Meta terms under fundamental decomposition
	# The fundamental pattern breaks every formula down to a single left- and
	# right-hand term, so "meta" terms (strata, random effects) cannot keep
	# their global position; they demote to plain predictors, with a message
	if (pattern == "fundamental") {
		tmTab <- vec_proxy(x)
		meta <- which(tmTab$side == "meta")
		if (length(meta) > 0) {
			message(
				"Fundamental decomposition: meta term(s) `",
				paste0(tmTab$term[meta], collapse = "`, `"),
				"` will be treated as plain predictor(s)."
			)
			tmTab$role[meta] <- "predictor"
			tmTab$side[meta] <- "right"
			x <- vec_restore(tmTab, to = tm())
		}
	}

	## Patterns
	# If pattern is acceptable can send for pattern tracing
	# Shuttled through the parent function `trace_pattern()`
	tbl <- apply_pattern(x, pattern)

	## Mediation
	# If mediation is needed
	# Mediation should be done only if covariates are already added
	# Function `check_mediation()` is internal only
	med <- with(vec_proxy(x), term[role == "mediator"])
	if (length(med) > 0 & pattern != "fundamental") {
		tbl <- check_mediation(x, tbl)
	}

	## Groups
	# Will remove rows if they will cause an error
	tbl <- check_groups(x, tbl)

	## Meta terms
	# Strata and random effects ride with every formula of this family -- and
	# only this family. Recording them in the formula matrix is what scopes
	# them: when families combine with `c()`, a stratum declared here cannot
	# leak into formulas that never asked for it.
	metaTerms <- with(vec_proxy(x), term[side == "meta"])
	for (i in seq_along(metaTerms)) {
		tbl[[paste0("meta_", i)]] <- metaTerms[i]
	}

	# The formula matrix: one row per precursor row, one column per term. A
	# cell records membership as a definition ordinal — cell `k` means the
	# formula uses the k-th definition of that term in the term table (see
	# `remap_definition_ordinals()`). Within one family every term has a
	# single definition, so every membership cell is 1; higher ordinals only
	# appear after families combine. A term repeated within one precursor row
	# still counts once
	rowTerms <- apply(tbl, MARGIN = 1, function(.x) {
		unique(as.character(stats::na.omit(unname(.x))))
	}, simplify = FALSE)
	allTerms <- unique(unlist(rowTerms))
	fmMat <- as.data.frame(
		do.call(rbind, lapply(rowTerms, function(.x) {
			as.integer(allTerms %in% .x)
		}))
	)
	names(fmMat) <- allTerms

	new_fmls(formulaMatrix = fmMat,
					 termTable = vec_proxy(x))

}

#' Initialize new formula-based data frame
#' @keywords internal
#' @noRd
new_fmls <- function(formulaMatrix = data.frame(),
										 termTable = data.frame(),
										 instructions = list()) {

	stopifnot(is.data.frame(formulaMatrix))
	stopifnot(is.data.frame(termTable))
	stopifnot(is.list(instructions))

	new_data_frame(
		x = formulaMatrix,
		termTable = termTable,
		instructions = instructions,
		class = "fmls"
	)

}

#' @rdname fmls
#' @export
is_fmls <- function(x) {
	inherits(x, "fmls")
}

#' @rdname fmls
#' @export
key_terms <- function(x) {

	# If a formula object, must pull only terms that are available
	if (is_fmls(x)) {
		# Formula matrix records every member, meta terms included, so
		# membership alone decides which terms belong to these formulas. The
		# cell ordinal names which *definition* of the term a formula uses, so
		# a combined object keeps e.g. `hp` the predictor and `hp` the
		# mediator as separate key terms
		fm <- vec_data(x)
		fm[is.na(fm)] <- 0
		tmTab <- attr(x, 'termTable')

		idx <- integer()
		for (t in names(fm)) {
			used <- sort(unique(fm[[t]][fm[[t]] >= 1]))
			idx <- c(idx, which(tmTab$term == t)[used])
		}

		tmTab[sort(idx), , drop = FALSE] |>
			vec_restore(to = tm())
	} else {
		NULL
	}
}

#' @export
format.fmls <- function(x,
												color = recast_color(),
												...) {

	# Break into matrix and key
	fmMat <- vec_data(x)
	tmTab <- attr(x, "termTable")

	fmt <-
		apply(
			fmMat,
			MARGIN = 1,
			FUN = function(.x) {
				.y <- resolve_formula_row(.x, tmTab)

				if ("mediator" %in% .y$role & !("outcome" %in% .y$role)) {
					# Handle mediation formula
					left <- .y[.y$role == "mediator", ]
					right <- .y[.y$side == "right" & .y$role != "mediator", ]
				} else {
					left <- .y[.y$side == "left", ]
					right <- .y[.y$side == "right", ]
				}

				.l <-
					vec_restore(left, to = tm()) |>
					format(color = color) |>
					paste0(collapse = " + ")

				.r <-
					vec_restore(right, to = tm()) |>
					format(color = color) |>
					paste0(collapse = " + ")

				# Meta terms sit on neither side of the fitted formula (a stratum
				# splits the data, a random effect conditions it), but hiding them
				# reads as an unstratified model — so they print in their declared
				# rune form (`.s(am)`, `.r(id)`) after the right-hand terms
				meta <- .y[.y$side == "meta", ]
				if (nrow(meta) > 0) {
					runes <- unlist(.roles)[meta$role]
					runes[is.na(runes)] <- ""
					fmtMeta <-
						vec_restore(meta, to = tm()) |>
						format(color = color)
					.r <- paste(
						c(.r[nzchar(.r)], paste0(runes, "(", fmtMeta, ")")),
						collapse = " + "
					)
				}

				paste(.l, sep = " ~ ", .r)
			}
		)

	# Return
	fmt
}

#' @export
print.fmls <- function(x,
											 color = recast_color(),
											 ...) {

	# The deck summary: what is about to be fit, at a glance
	if (length(x) > 0) {
		cat(fmls_deck_header(x), sep = "\n")
	}

	# Colorful printing
	if (length(x) > 1) {
		cat(format(x, color = color), sep = "\n")
	} else if (length(x) == 1) {
		cat(format(x, color = color))
	}

	invisible(x)
}

#' @export
vec_ptype_full.fmls <- function(x, ...) {
	"formulas"
}

#' @export
vec_ptype_abbr.fmls <- function(x, ...) {
	"fmls"
}

#' @export
methods::setOldClass(c("fmls", "vctrs_rcrd"))

# Coercion methods -----------------------------------------------------------

# SELF

#' @keywords internal
#' @noRd
fmls_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {

	# Creates a "empty" data frame with appropraite fields
	newMatrix <- df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)

	# One row per definition: a term serving different roles in different
	# families keeps every one of those definitions
	newTmTab <- combine_term_tables(attr(x, "termTable"), attr(y, "termTable"))

	new_fmls(newMatrix,
					 termTable = newTmTab,
					 instructions = combine_instructions(x, y))
}

#' The identity of a term definition
#'
#' A term may serve different causal roles in different families (`hp` as a
#' plain predictor in one, the mediator of another), so the atom a term table
#' holds is not the term name but the *definition*: the (term, side, role)
#' triple. Decorations (label, group, ...) hang off a definition rather than
#' extending its identity.
#' @keywords internal
#' @noRd
term_definition_key <- function(tmTab) {
	paste(tmTab$term, tmTab$side, tmTab$role, sep = "\r")
}

#' Merge term tables, keeping every distinct definition
#'
#' Rows that agree on (term, side, role) but differ in decoration are the
#' only true conflicts left; there the first (left-most) wins, with a message
#' unless `quiet` (casting re-treads ground the ptype2 already reported).
#' @keywords internal
#' @noRd
combine_term_tables <- function(xTm, yTm, quiet = FALSE) {

	tmTab <-
		rbind(xTm, yTm) |>
		unique()

	dups <- duplicated(term_definition_key(tmTab))

	if (any(dups) && !quiet) {
		message(
			"Combining formulas with conflicting definitions for: `",
			paste0(unique(tmTab$term[dups]), collapse = "`, `"),
			"`. The first (left-most) definition of each term is kept."
		)
	}

	tmTab[!dups, , drop = FALSE]
}

#' Re-point formula matrix cells at a combined term table
#'
#' A membership cell holds a definition ordinal: cell `k` in column `t` means
#' the formula uses the k-th definition of term `t` in its term table. When
#' term tables combine, a family's definitions may land at new ordinals (its
#' `hp` may now be the second `hp`), so its cells are re-pointed here. This
#' scoping is what lets a stratum, a role, or a label stay with the family
#' that declared it.
#' @keywords internal
#' @noRd
remap_definition_ordinals <- function(fmMat, from, to) {

	if (nrow(fmMat) == 0 || is.null(from) || nrow(from) == 0) {
		return(fmMat)
	}

	fromKey <- term_definition_key(from)
	toKey <- term_definition_key(to)

	for (t in intersect(names(fmMat), from$term)) {
		map <- match(fromKey[from$term == t], toKey[to$term == t])
		cells <- fmMat[[t]]
		hit <- which(!is.na(cells) & cells >= 1)
		fmMat[[t]][hit] <- map[cells[hit]]
	}

	fmMat
}

#' Resolve one formula matrix row to its term-table rows
#'
#' Membership names the terms; each cell's ordinal names which definition of
#' its term this formula reads. Rows return in term-table order.
#' @keywords internal
#' @noRd
resolve_formula_row <- function(rowVals, tmTab) {

	vals <- unlist(rowVals)
	vals[is.na(vals)] <- 0
	members <- which(vals >= 1)

	idx <- vapply(members, function(j) {
		which(tmTab$term == names(vals)[j])[[vals[j]]]
	}, integer(1))

	tmTab[sort(idx), , drop = FALSE]
}

#' Merge subset instructions when families combine (union by name)
#' @keywords internal
#' @noRd
combine_instructions <- function(x, y) {
	xInst <- attr(x, "instructions")
	yInst <- attr(y, "instructions")
	subsets <- c(
		if (is.null(xInst)) list() else xInst$subsets,
		if (is.null(yInst)) list() else yInst$subsets
	)
	subsets <- subsets[!duplicated(names(subsets))]
	if (length(subsets) == 0) {
		return(list())
	}
	list(subsets = subsets)
}

#' @export
c.fmls <- function(...) {
	vec_c(...)
}

#' @keywords internal
#' @noRd
fmls_cast <- function(x, to, ..., x_arg = "", to_arg = "") {

	# Definitions merge quietly here: in a `c()` the ptype2 step has already
	# reported any true conflicts
	xTm <- attr(x, "termTable")
	newTmTab <- combine_term_tables(attr(to, "termTable"), xTm, quiet = TRUE)

	# When casting, the matrices need to be similar in columns; then each of
	# x's membership cells is re-pointed at its definition's ordinal within
	# the combined term table
	newMatrix <-
		df_cast(
			x,
			to,
			...,
			x_arg = x_arg,
			to_arg = to_arg
		) |>
		remap_definition_ordinals(from = xTm, to = newTmTab)

	new_fmls(newMatrix,
					 termTable = newTmTab,
					 instructions = combine_instructions(x, to))
}

#' @export
vec_ptype2.fmls.fmls <- function(x, y, ...) {
	fmls_ptype2(x, y, ...)
}

#' @export
vec_cast.fmls.fmls <- function(x, to, ...) {
	fmls_cast(x, to, ...)
}

# CHARACTER

#' @export
vec_ptype2.fmls.character <- function(x, y, ...) y # X = fmls

#' @export
vec_ptype2.character.fmls <- function(x, y, ...) x # X = character

#' @export
vec_cast.fmls.character <- function(x, to, ...) {
	# order is flipped, such that `x` is character
	# Cast from character into fmls
	x |>
		stats::as.formula(env = .GlobalEnv) |>
		fmls()
}

#' @export
vec_cast.character.fmls <- function(x, to, ...) {
	# Going from fmls to character
	# order is flipped, such that `x` is fmls
	as.character(x)
}

#' @export
as.character.fmls <- function(x, ...) {
	formulas_to_terms(x) |>
		lapply(stats::formula) |>
		sapply(deparse1)

}

# FORMULA

#' @export
formula.fmls <- function(x, ...) {
	x |>
		formulas_to_terms() |>
		lapply(stats::as.formula, env = .GlobalEnv)
}


#' @export
vec_ptype2.fmls.formula <- function(x, y, ...) {
	x
}

#' @export
vec_ptype2.formula.fmls <- function(x, y, ...) {
	y
}

#' @export
vec_cast.formula.fmls <- function(x, to, ...) {
	# Cast from `fmls` into `formula`
	# Returns a list of formulas
	stats::formula(x)

}

#' @export
vec_cast.fmls.formula <- function(x, to, ...) {
	# Cast from `formula` into `fmls`
	fmls(x)
}

# Formula internals ------------------------------------------------------------

#' @keywords internal
#' @noRd
check_mediation <- function(x, tbl) {

	# Roles (the mediation triad needs the outcome, exposure, and mediator)
	roles <- pattern_roles(vec_proxy(x))
	out <- roles$outcome
	exp <- roles$exposure
	med <- roles$mediator

	# Requires a table from the `apply_*_pattern()` functions
	# Each row has been expanded for exposure and outcome
	# This will triple the number of rows subsequently
	validate_class(tbl, "tbl_df")

	# Mediation...
	# 	The combinations of mediation are based on causal reasoning
	# 	outcome ~ exposure + mediator + predictors
	#		mediator ~ exposure
	# 	outcome ~ mediator + exposure

	# 'outcome ~ exposure + mediator + predictors'
	# 	Covariates exists in each row already
	# 	Simply add mediator
	m1 <- tidyr::expand_grid(tbl, mediator = med)

	# 'mediator ~ exposure'
	# 	No other variables allowed
	# 	Add a new row of just this
	m2 <- tidyr::expand_grid(mediator = med, exposure = exp)

	# 'outcome ~ mediator + exposure'
	#		Only looking for effect of mediator on outcome WITH exposure
	m3 <- tidyr::expand_grid(outcome = out, mediator = med, exposure = exp)

	# Bind all the tables together
	tbl <-
		m1 |>
		dplyr::bind_rows(m2) |>
		dplyr::bind_rows(m3) |>
		unique()

	# Return
	tbl

}

#' @keywords internal
#' @noRd
check_groups <- function(x, tbl) {

	# Global variables
	group <- NULL

	# Only the grouping tiers matter here; roles play no part
	tmTab <- vec_proxy(x)

	# Requires a table from the `apply_*_pattern()` functions
	validate_class(tbl, "tbl_df")

	# Grouping variables now must be assessed
	# IF a group is present, the row must have its full group present OR ELSE
	# The term table from above serves as the reference
	# Grouping doesn't need to be checked IF no grouping variables in that row

	groupLevels <- with(tmTab, unique(group[!is.na(group)]))
	rowNums <- seq(nrow(tbl))
	badRows <- integer()

	for (g in groupLevels) {
		groupedPredictors <- subset(tmTab, group == g)$term
		for (r in rowNums) {
			# All predictors
			allPredictors <-
				tbl[r,] |>
				dplyr::select(-dplyr::any_of(c("outcome", "exposure"))) |>
				unlist() |>
				unname() |>
				stats::na.omit()

			# IF any grouping variables are present, then ALL must be present
			if (any(groupedPredictors %in% allPredictors)) {
				# THEN check if all are present
				if (!all(groupedPredictors %in% allPredictors)) {
					badRows <- c(badRows, r)
				}
			}

		}
	}

	# Now remove the "bad rows"
	ntbl <- tbl[badRows,]
	tbl <- suppressMessages(dplyr::anti_join(tbl, ntbl))
	stopifnot(
		"Based on restrictions from the chosen terms and pattern, no `fmls` can be generated."
		= nrow(tbl) > 0
	)

	# Return
	tbl

}

#' @keywords internal
#' @noRd
formulas_to_terms <- function(x) {

	validate_class(x, "fmls")

	fmMat <- vec_data(x)
	tmTab <- attr(x, "termTable")

	# Membership in the formula matrix is the whole story: strata and random
	# effects are recorded there like any other term, so a stratum stays with
	# the formula that declared it instead of leaking across combined
	# families — and the cell ordinals scope roles the same way, so each
	# formula reads its own family's definition of a shared term
	tms <-
		apply(
			fmMat,
			MARGIN = 1,
			FUN = function(.x) {
				.y <- resolve_formula_row(.x, tmTab)
				vec_restore(.y, to = tm())
		})

	# Return
	tms

}
