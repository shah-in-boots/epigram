# Status -----------------------------------------------------------------------

#' Classify each row of a model table as fitted, failed, or unfit
#' @return A character vector of "fitted", "failed", or "unfit" per row:
#'   "failed" rows were attempted and recorded an error; "unfit" rows are
#'   formulas that have not met [fit()] yet
#' @keywords internal
#' @noRd
model_table_status <- function(x) {
	vapply(seq_len(nrow(x)), function(i) {
		if (isTRUE(x$fit_status[i])) {
			return("fitted")
		}
		s <- x$model_summary[[i]]
		if (is.list(s) && !is.null(s$error) && !all(is.na(s$error))) {
			"failed"
		} else {
			"unfit"
		}
	}, character(1))
}

#' Display symbols for model status, with ASCII fallbacks
#' @keywords internal
#' @noRd
model_status_symbols <- function() {
	if (has_cli() && getExportedValue("cli", "is_utf8_output")()) {
		c(fitted = "\u2714", failed = "\u2716", unfit = "\u25cb")
	} else {
		c(fitted = "+", failed = "x", unfit = "o")
	}
}

#' Placeholder for missing cells in the model table display
#' @keywords internal
#' @noRd
model_display_na <- function() {
	if (has_cli() && getExportedValue("cli", "is_utf8_output")()) {
		"\u2014"
	} else {
		"-"
	}
}

#' Color a status symbol when the terminal supports it
#' @keywords internal
#' @noRd
color_status <- function(symbol, status) {
	if (!isTRUE(recast_color()) || !has_cli()) {
		return(symbol)
	}
	fn <- switch(
		status,
		fitted = getExportedValue("cli", "col_green"),
		failed = getExportedValue("cli", "col_red"),
		unfit = getExportedValue("cli", "col_grey"),
		identity
	)
	fn(symbol)
}

# Printing ---------------------------------------------------------------------

#' @describeIn model_table The print method leads with the state of the
#'   analysis: how many models are fitted, failed, or still unfit; which
#'   datasets are attached; then one line per model. Control the number of
#'   rows shown with `n`.
#' @param n Number of models to show when printing (default 10)
#' @export
print.mdl_tbl <- function(x, ..., n = 10) {
	cat(format(x, n = n), sep = "\n")
	invisible(x)
}

#' @export
format.mdl_tbl <- function(x, ..., n = 10) {

	status <- model_table_status(x)
	sym <- model_status_symbols()
	dash <- model_display_na()

	# Header: the state of the fleet at a glance
	nFormulas <- length(unique(stats::na.omit(x$formula_call)))
	nFamilies <-
		if ("family" %in% names(x)) {
			length(unique(stats::na.omit(x[["family"]])))
		} else {
			0L
		}
	header <- paste0(
		"<model_table> ", nrow(x),
		if (nrow(x) == 1) " model" else " models",
		if (nFormulas > 0) {
			paste0(" \u00d7 ", nFormulas,
						 if (nFormulas == 1) " formula" else " formulas")
		},
		if (nFamilies > 0) {
			paste0(" \u00d7 ", nFamilies,
						 if (nFamilies == 1) " family" else " families")
		}
	)

	counts <- table(factor(status, levels = c("fitted", "failed", "unfit")))
	statusParts <- vapply(names(counts)[counts > 0], function(s) {
		paste0(color_status(sym[[s]], s), " ", counts[[s]], " ", s)
	}, character(1))
	statusLine <-
		if (length(statusParts) > 0) {
			paste0("  ", paste(statusParts, collapse = "  "))
		}

	# Datasets: referenced by models and/or attached to the table
	datLs <- attr(x, "dataList")
	dataIds <- unique(c(stats::na.omit(x$data_id), names(datLs)))
	dataLine <-
		if (length(dataIds) > 0) {
			marks <- vapply(dataIds, function(d) {
				if (d %in% names(datLs)) {
					paste0(d, " [attached]")
				} else {
					paste0(d, " [not attached]")
				}
			}, character(1))
			paste0("  data: ", paste(marks, collapse = ", "))
		}

	# Fitting context, when present
	contextParts <- character()
	stas <- unique(stats::na.omit(x$strata))
	if (length(stas) > 0) {
		contextParts <- c(contextParts, paste0("strata: ", paste(stas, collapse = ", ")))
	}
	subs <- unique(stats::na.omit(x$subset))
	if (length(subs) > 0) {
		contextParts <- c(contextParts, paste0("subsets: ", paste(subs, collapse = ", ")))
	}
	if ("relation" %in% names(x)) {
		rels <- unique(stats::na.omit(x[["relation"]]))
		if (length(rels) > 0) {
			contextParts <- c(contextParts,
												paste0("relations: ", paste(rels, collapse = "; ")))
		}
	}
	contextLine <-
		if (length(contextParts) > 0) {
			paste0("  ", paste(contextParts, collapse = " | "))
		}

	lines <- c(header, statusLine, dataLine, contextLine)

	if (nrow(x) == 0) {
		return(c(
			lines,
			"",
			paste0("  No models yet: build with ",
						 "`fmls() |> fit(.fn, data = , raw = FALSE) |> model_table()`")
		))
	}

	# Body: one line per model, columns shown only when they carry information
	shown <- seq_len(min(n, nrow(x)))
	naTo <- function(v, fill = dash) ifelse(is.na(v), fill, as.character(v))
	truncTo <- function(v, w) {
		ifelse(nchar(v) > w, paste0(substr(v, 1, w - 1), "\u2026"), v)
	}

	body <- list(` ` = as.character(shown))
	# The family identification the table always carries
	if ("family" %in% names(x)) {
		body$family <- naTo(as.character(x[["family"]][shown]))
		body$pattern <- naTo(x[["pattern"]][shown])
	}
	if (any(!is.na(x$name))) {
		body$name <- naTo(x$name[shown])
	}
	body$model <- naTo(x$model_call[shown])
	body$formula <- truncTo(naTo(x$formula_call[shown]), 40)
	body$outcome <- truncTo(naTo(x$outcome[shown]), 20)
	if (any(!is.na(x$exposure))) {
		body$exposure <- naTo(x$exposure[shown])
	}
	if (any(!is.na(x$mediator))) {
		body$mediator <- naTo(x$mediator[shown])
	}
	if (any(!is.na(x$interaction))) {
		body$interaction <- naTo(x$interaction[shown])
	}
	if (any(!is.na(x$strata))) {
		body$strata <- ifelse(
			is.na(x$strata[shown]),
			dash,
			paste0(x$strata[shown],
						 ifelse(is.na(x$level[shown]), "",
						 			 paste0("=", x$level[shown])))
		)
	}
	if (any(!is.na(x$subset))) {
		body$subset <- naTo(x$subset[shown])
	}
	# Number of observations per model, when the fit recorded one
	nObs <- vapply(x$model_summary, function(s) {
		if (is.list(s) && !is.null(s$nobs) && length(s$nobs) == 1 &&
				!is.na(s$nobs)) {
			as.integer(s$nobs)
		} else {
			NA_integer_
		}
	}, integer(1))
	if (any(!is.na(nObs))) {
		body$n <- naTo(nObs[shown])
	}

	# Pad the plain columns (the colored status column rides unpadded in front)
	padded <- lapply(names(body), function(col) {
		vals <- c(col, body[[col]])
		formatC(vals, width = max(nchar(vals)), flag = "-")
	})
	rowLines <- do.call(paste, c(padded, list(sep = "  ")))
	statusCol <- c(" ", vapply(shown, function(i) {
		color_status(sym[[status[i]]], status[i])
	}, character(1)))
	rowLines <- paste0("  ", statusCol, " ", rowLines)

	lines <- c(lines, "", rowLines)

	if (nrow(x) > n) {
		lines <- c(lines, paste0("  ", dash, " and ", nrow(x) - n,
														 " more models: print(n = ", nrow(x), ")"))
	}

	# Footers point at the next move
	hints <- character()
	if (any(status == "failed")) {
		hints <- c(hints, paste0("# ", sum(status == "failed"),
														 " model(s) failed: `model_failures()` shows why"))
	}
	if (any(status == "unfit")) {
		hints <- c(hints, paste0("# ", sum(status == "unfit"),
														 " formula(s) await `fit()`"))
	}
	hints <- c(hints,
						 "# `summary()` maps the fleet; `flatten_models()` extracts estimates")

	c(lines, hints)

}

#' @describeIn model_table The summary method maps the fleet: models grouped
#'   by dataset, fitting function, outcome, and exposure, with their
#'   adjustment ranges and stratification; the terms in play by causal role;
#'   and any failures with their messages. Returns the overview grouping
#'   invisibly as a `tibble`.
#' @param object A `mdl_tbl` object (for `summary()`)
#' @export
summary.mdl_tbl <- function(object, ...) {

	# Global variables
	data <- model <- outcome <- exposure <- number <- strata <- fitted <- NULL

	x <- object
	status <- model_table_status(x)
	sym <- model_status_symbols()
	dash <- model_display_na()

	overview <-
		tibble::tibble(
			data = ifelse(is.na(x$data_id), dash, x$data_id),
			model = ifelse(is.na(x$model_call), dash, x$model_call),
			outcome = ifelse(is.na(x$outcome), dash, x$outcome),
			exposure = ifelse(is.na(x$exposure), dash, x$exposure),
			number = x$number,
			strata = x$strata,
			fitted = x$fit_status
		) |>
		dplyr::group_by(data, model, outcome, exposure) |>
		dplyr::summarise(
			models = dplyr::n(),
			fitted = sum(fitted),
			adjustment = if (all(is.na(number))) {
				NA_character_
			} else {
				paste0(min(number, na.rm = TRUE), "\u2013",
							 max(number, na.rm = TRUE), " terms")
			},
			strata = if (all(is.na(strata))) {
				NA_character_
			} else {
				paste(unique(stats::na.omit(strata)), collapse = ", ")
			},
			.groups = "drop"
		)

	cat(paste0("<model_table> summary: ", nrow(x),
						 if (nrow(x) == 1) " model" else " models"), sep = "\n")

	# The fleet, one line per family of models
	ovr <- overview
	ovr$adjustment[is.na(ovr$adjustment)] <- dash
	ovr$strata[is.na(ovr$strata)] <- dash
	ovr$fitted <- paste0(ovr$fitted, "/", ovr$models)
	ovr$models <- NULL
	padded <- lapply(names(ovr), function(col) {
		vals <- c(col, as.character(ovr[[col]]))
		formatC(vals, width = max(nchar(vals)), flag = "-")
	})
	cat(paste0("  ", do.call(paste, c(padded, list(sep = "  ")))), sep = "\n")

	# Terms by causal role
	tmTab <- attr(x, "termTable")
	if (is.data.frame(tmTab) && nrow(tmTab) > 0) {
		roleParts <- vapply(unique(tmTab$role), function(r) {
			paste0(r, ": ", paste(unique(tmTab$term[tmTab$role == r]), collapse = ", "))
		}, character(1))
		cat("", paste0("  terms | ", paste(roleParts, collapse = " | ")), sep = "\n")
	}

	# Failures deserve their messages
	fails <- model_failures(x)
	if (nrow(fails) > 0) {
		cat("", sep = "\n")
		for (i in seq_len(nrow(fails))) {
			where <- c(
				if (!is.na(fails$strata[i])) {
					paste0(fails$strata[i], "=", fails$level[i])
				},
				if (!is.na(fails$subset[i])) fails$subset[i],
				if (!is.na(fails$data_id[i])) fails$data_id[i]
			)
			cat(paste0(
				"  ", color_status(sym[["failed"]], "failed"), " ",
				ifelse(is.na(fails$model_call[i]), "", paste0(fails$model_call[i], ": ")),
				fails$formula_call[i],
				if (length(where) > 0) paste0(" [", paste(where, collapse = ", "), "]"),
				" \u2014 ", fails$error[i]
			), sep = "\n")
		}
	}

	invisible(overview)

}

# Paring --------------------------------------------------------------------

#' Pare a model table down to one analysis
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' A model table accumulates every model of a project; a mesa holds one
#' analysis. These verbs pare the table down along its causal dimensions,
#' in language that says what the kept models share — the step before
#' [mdl_gt()], whose gate admits only one presentable analysis:
#'
#' - `keep_outcomes()` / `keep_exposures()` keep the models carrying a term
#'   in that causal role; `drop_outcomes()` / `drop_exposures()` remove them.
#'
#' - `keep_families()` keeps identified families — by the ids the table's
#'   `family` column carries (`keep_families(x, 1, 2)`; the column shows them),
#'   or by `pattern` (`"sequential"`, `"parallel"`, `"mediation"`, `"direct"`)
#'   and `relation` (`"varied exposures"`, `"varied outcomes"`).
#'
#' - `restrict_to()` restricts the population or source: the stratifying
#'   term (`strata`), a stratum (`level`), a [subset_data()] rule
#'   (`subset`), or the dataset (`data`).
#'
#' - `adjusting_for()` keeps the models whose adjustment set carries every
#'   named covariate ([adjustment_sets()] shows the sets and their rungs).
#'
#' - `excluding()` keeps the models whose formulas avoid the named terms
#'   entirely, whatever the role — the way to set aside a mediator or a
#'   collider before laying the rest out.
#'
#' Terms are given as bare names or strings, matched by exact identity —
#' never by substring — and every requested value is checked against what
#' the table holds, so a typo errors with the available values instead of
#' silently keeping nothing. Each verb reports what it kept with a message,
#' and they chain in any order. Ordinary `dplyr` verbs still work on the
#' table; these are the causal-aware shorthand whose result is likelier to
#' pass `mdl_gt()`'s gate.
#'
#' Every `mdl_tbl` carries `family`/`pattern`/`relation` columns describing the
#' identification; they refresh automatically each time the table is reshaped,
#' so after paring the ids renumber from 1 against the rows that remain.
#'
#' @param x A `mdl_tbl` object
#'
#' @param ... The terms (bare names or strings) the verb keeps, drops, or
#'   excludes; for `keep_families()`, the family id(s) from the stamped
#'   `family` column
#'
#' @param pattern For `keep_families()`: keep families with this pattern
#'
#' @param relation For `keep_families()`: keep families carrying this
#'   relation (a family holding several matches any of its own)
#'
#' @param strata,level,subset,data For `restrict_to()`: the stratifying
#'   term, stratum level, subset rule (as recorded in the `subset` column),
#'   or dataset name to restrict to
#'
#' @return The pared `mdl_tbl`, its attributes pruned to the remaining
#'   models; a message reports how many models were kept.
#'
#' @examples
#' d <- mtcars
#' mt <-
#'   c(
#'     fmls(mpg ~ .x(wt) + hp + cyl, pattern = "sequential"),
#'     fmls(mpg ~ .x(disp) + hp + cyl, pattern = "sequential"),
#'     fmls(qsec ~ .x(am))
#'   ) |>
#'   fit(.fn = lm, data = d) |>
#'   model_table(data = d)
#'
#' # By causal role
#' mt |> keep_outcomes(mpg)
#' mt |> drop_outcomes(qsec) |> keep_exposures(wt, disp)
#'
#' # By adjustment and by avoidance
#' mt |> adjusting_for(hp)
#' mt |> excluding(cyl)
#'
#' # By identified family (the `family`/`pattern`/`relation` columns show it)
#' mt |> keep_families(relation = "varied exposures")
#' mt |> keep_families(1)
#'
#' @seealso [adjustment_sets()] to see the adjustment rungs, [mdl_gt()] for the
#'   gate these prepare for
#' @name paring
NULL

#' @rdname paring
#' @export
keep_outcomes <- function(x, ...) {
	validate_class(x, "mdl_tbl")
	wanted <- pare_terms(..., what = "outcome", available = x$outcome)
	pare_result(x, x$outcome %in% wanted)
}

#' @rdname paring
#' @export
drop_outcomes <- function(x, ...) {
	validate_class(x, "mdl_tbl")
	wanted <- pare_terms(..., what = "outcome", available = x$outcome)
	pare_result(x, !x$outcome %in% wanted)
}

#' @rdname paring
#' @export
keep_exposures <- function(x, ...) {
	validate_class(x, "mdl_tbl")
	wanted <- pare_terms(..., what = "exposure", available = x$exposure)
	pare_result(x, x$exposure %in% wanted)
}

#' @rdname paring
#' @export
drop_exposures <- function(x, ...) {
	validate_class(x, "mdl_tbl")
	wanted <- pare_terms(..., what = "exposure", available = x$exposure)
	pare_result(x, !x$exposure %in% wanted)
}

#' @rdname paring
#' @export
keep_families <- function(x, ..., pattern = NULL, relation = NULL) {

	validate_class(x, "mdl_tbl")

	ids <- unlist(rlang::list2(...))
	ids <- if (length(ids) == 0) NULL else as.character(ids)
	if (is.null(ids) && is.null(pattern) && is.null(relation)) {
		stop(
			"`keep_families()` needs a family id, a `pattern`, or a `relation`. ",
			"The table's `family`/`pattern`/`relation` columns show them.",
			call. = FALSE
		)
	}

	# Every `mdl_tbl` carries the identification as columns, refreshed each time
	# the table is built or reshaped, so the family structure is read straight
	# off the rows that remain
	keep <- rep(TRUE, nrow(x))

	if (!is.null(ids)) {
		keep_available(ids, as.character(x[["family"]]), "family")
		keep <- keep & as.character(x[["family"]]) %in% ids
	}

	if (!is.null(pattern)) {
		keep_available(pattern, x[["pattern"]], "pattern")
		keep <- keep & x[["pattern"]] %in% pattern
	}

	if (!is.null(relation)) {
		# A family may carry several relations, comma-joined; match by membership
		relList <- strsplit(ifelse(is.na(x[["relation"]]), "", x[["relation"]]),
												",[ ]*")
		keep_available(relation, unlist(relList), "relation")
		keep <- keep &
			vapply(relList, function(r) any(relation %in% r), logical(1))
	}

	pare_result(x, keep)
}

#' @rdname paring
#' @export
restrict_to <- function(x, strata = NULL, level = NULL, subset = NULL,
												data = NULL) {

	validate_class(x, "mdl_tbl")
	env <- parent.frame()

	dims <- list(
		strata = pare_input(substitute(strata), env),
		level = pare_input(substitute(level), env),
		subset = pare_input(substitute(subset), env),
		data = pare_input(substitute(data), env)
	)
	if (all(vapply(dims, is.null, logical(1)))) {
		stop(
			"`restrict_to()` needs a population dimension: `strata`, `level`, ",
			"`subset`, or `data`.",
			call. = FALSE
		)
	}

	columns <- c(strata = "strata", level = "level", subset = "subset",
							 data = "data_id")
	keep <- rep(TRUE, nrow(x))
	for (dim in names(dims)) {
		wanted <- dims[[dim]]
		if (is.null(wanted)) {
			next
		}
		values <- x[[columns[[dim]]]]
		keep_available(wanted, values, dim)
		keep <- keep & values %in% wanted
	}

	pare_result(x, keep)
}

#' @rdname paring
#' @export
adjusting_for <- function(x, ...) {

	validate_class(x, "mdl_tbl")
	wanted <- vars_from_dots(...)
	if (length(wanted) == 0) {
		stop(
			"`adjusting_for()` needs the covariate(s) the kept models must ",
			"adjust for; `adjustment_sets()` shows this table's sets.",
			call. = FALSE
		)
	}

	sets <- identify_families(model_table_formulas(x))$covariates
	keep_available(wanted, unlist(sets), "adjustment covariate")
	pare_result(x, vapply(sets, function(s) {
		all(wanted %in% s)
	}, logical(1)))
}

#' @rdname paring
#' @export
excluding <- function(x, ...) {

	validate_class(x, "mdl_tbl")
	wanted <- vars_from_dots(...)
	if (length(wanted) == 0) {
		stop(
			"`excluding()` needs the term(s) the kept models must avoid.",
			call. = FALSE
		)
	}

	# Membership in the formula matrix covers every role a term could hold
	fmMat <- attr(x, "formulaMatrix")
	present <- names(fmMat)[colSums(fmMat, na.rm = TRUE) > 0]
	keep_available(wanted, present, "term")

	hit <- rep(FALSE, nrow(x))
	for (term in intersect(wanted, names(fmMat))) {
		hit <- hit | (!is.na(fmMat[[term]]) & fmMat[[term]] >= 1)
	}
	pare_result(x, !hit)
}

#' The pared table, with the one-line report every verb shares
#' @keywords internal
#' @noRd
pare_result <- function(x, keep) {
	out <- x[keep, , drop = FALSE]
	message("Kept ", nrow(out), " of ", nrow(x),
					if (nrow(x) == 1) " model." else " models.")
	out
}

#' Bare names or strings from a paring verb's dots, validated against a
#' provenance column; empty dots error with what the table holds, so the
#' verbs teach their own vocabulary
#' @keywords internal
#' @noRd
pare_terms <- function(..., what, available) {
	wanted <- vars_from_dots(...)
	if (length(wanted) == 0) {
		available <- unique(stats::na.omit(available))
		stop(
			"Name the ", what, "(s) to pare by. This table holds: ",
			if (length(available) > 0) {
				paste0("`", available, "`", collapse = ", ")
			} else {
				"none"
			},
			".", call. = FALSE
		)
	}
	keep_available(wanted, available, what)
	wanted
}


#' Bare names, strings, numbers, or `c()` combinations from a paring
#' argument, as a character vector; `NULL` stays `NULL`. A bare name is
#' taken as a name (the paring verbs' house style), never evaluated.
#' @keywords internal
#' @noRd
pare_input <- function(expr, env) {
	if (is.null(expr)) {
		return(NULL)
	}
	if (is.symbol(expr)) {
		return(as.character(expr))
	}
	if (is.call(expr) && identical(expr[[1L]], quote(c))) {
		return(unlist(lapply(as.list(expr)[-1L], pare_input, env = env)))
	}
	as.character(eval(expr, env))
}

#' Ensure requested paring values exist in the table, erroring with what
#' is available instead of silently keeping nothing
#' @keywords internal
#' @noRd
keep_available <- function(requested, available, what) {
	available <- unique(stats::na.omit(available))
	missing <- setdiff(requested, available)
	if (length(missing) > 0) {
		stop(
			"No models have the ", what, " ",
			paste0("`", missing, "`", collapse = ", "), ". ",
			"Available ", what,
			if (length(available) > 0) {
				paste0(": ", paste0("`", available, "`", collapse = ", "))
			} else {
				": none"
			},
			".", call. = FALSE
		)
	}
	invisible(TRUE)
}

#' The adjustment sets of a model table
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `adjustment_sets()` shows the adjustment rungs a table's models climb:
#' one row per distinct covariate set, numbered the way [mdl_gt()]'s
#' `select_adjustment()` selects them. The index is the *identity* of the
#' set — models carrying the same covariates share a rung wherever they sit,
#' so related families' rows align on the mesa by the actual adjustment —
#' and rungs are ordered by set size and then order of first appearance,
#' which for a `"sequential"` [fmls()] pattern is the order the ladder was
#' built.
#'
#' @param x A `mdl_tbl` object, or a `<mdl_gt>` specification (read from its
#'   fitted models)
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return A `tibble` with one row per adjustment set: `adjustment` (the
#'   index `select_adjustment()` uses), `covariates` (the set, `+`-joined;
#'   `(unadjusted)` when empty), `adds` (what the rung adds over the
#'   previous one, when it nests), `models` (how many models carry it), and
#'   `families` (which families, by the table's `family` ids).
#'
#' @examples
#' d <- mtcars
#' mt <-
#'   fmls(mpg ~ .x(wt) + hp + cyl, pattern = "sequential") |>
#'   fit(.fn = lm, data = d) |>
#'   model_table(data = d)
#' adjustment_sets(mt)
#'
#' @seealso The [paring] verbs (especially [adjusting_for()]), [mdl_gt()]
#'
#' @include table-spec.R
#' @export
adjustment_sets <- function(x, ...) {
	UseMethod("adjustment_sets", object = x)
}

#' @rdname adjustment_sets
#' @export
adjustment_sets.mdl_tbl <- function(x, ...) {

	fam <- identify_families(model_table_formulas(x))
	idx <- adjustment_set_index(x)

	if (length(idx) == 0) {
		return(tibble::tibble(
			adjustment = integer(),
			covariates = character(),
			adds = character(),
			models = integer(),
			families = character()
		))
	}

	# The table's own family ids, refreshed against its current rows
	famIds <- x[["family"]]

	rungs <- sort(unique(idx))
	sets <- lapply(rungs, function(k) fam$covariates[[match(k, idx)]])

	covLabel <- vapply(sets, function(s) {
		if (length(s) == 0) "(unadjusted)" else paste(s, collapse = " + ")
	}, character(1))
	adds <- vapply(seq_along(sets), function(i) {
		if (i == 1) {
			return(NA_character_)
		}
		prev <- sets[[i - 1]]
		cur <- sets[[i]]
		if (all(prev %in% cur) && length(cur) > length(prev)) {
			paste("+", paste(setdiff(cur, prev), collapse = " + "))
		} else {
			NA_character_
		}
	}, character(1))

	tibble::tibble(
		adjustment = rungs,
		covariates = covLabel,
		adds = adds,
		models = vapply(rungs, function(k) sum(idx == k), integer(1)),
		families = vapply(rungs, function(k) {
			paste(sort(unique(famIds[idx == k])), collapse = ", ")
		}, character(1))
	)
}

# `adjustment_sets()` stays an S3 generic (it also dispatches on the vctrs
# `mdl_tbl`). The S7 `<mdl_gt>` joins that dispatch through an S7 *method*
# registered on the S3 generic: a plain `adjustment_sets.mdl_gt` would never be
# found, because the spec's class is the namespaced `recast::mdl_gt`, not the bare
# `mdl_gt` S3 dispatch looks for. `S7::methods_register()` (in `.onLoad`) wires
# this method into S3 dispatch at load, so no `@export` is needed.
method(adjustment_sets, mdl_gt) <- function(x, ...) {
	adjustment_sets(x@mdl_tbl, ...)
}

# Model Table Helper Functions ------------------------------------------------

#' Model table helper functions
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' These functions manage and interrogate a `mdl_tbl` object -- they are the
#' working verbs of the notebook of models:
#'
#' - `attach_data()`: attaches a dataset to the table for later recall
#'
#' - `model_failures()`: returns the models that were attempted but failed,
#'   with their error messages
#'
#' - `term_table()`: the terms behind the table, as a `tm` vector with their
#'   causal roles
#'
#' - `formula_matrix()`: the model-by-term membership matrix
#'
#' - `model_data()`: the datasets attached to the table
#'
#' See [flatten_models()] for extracting parameter estimates.
#'
#' # Attaching Data
#'
#' When models are built, oftentimes the included matrix of data is available
#' within the raw model, however when handling many models, this can be
#' expensive in terms of memory and space. By attaching datasets independently
#' that persist regardless of the underlying models, and by knowing which
#' models used which datasets, it can be easy to back-transform information.
#' The dataset is stored under the name it was passed as (or an explicit
#' `name`), and should match the `data_id` column of the models that used it.
#'
#' Attached data lives at the `mdl_tbl` level — the layer where formulas and
#' data come together. The frame attaches whole: later work routinely
#' reaches for columns no current formula names (a follow-up column for
#' [add_events()], a variable for the next family of models). By contrast,
#' [set_data()] on a `tm` or `fmls` only *teaches* — it stamps term
#' properties (type, distribution, levels) and retains no data at all.
#'
#' Two conveniences keep the name match from depending on retyping: a `data`
#' passed as an inline expression takes the stable content-derived id
#' (`data_<hash>`) that [fit()] gives the identical frame, and a frame
#' arriving under a different name than the models recorded is aliased to the
#' referenced `data_id` — with a message — when it is the only detached
#' dataset and carries every variable those models use.
#'
#' @param x A `mdl_tbl` object (or, for `term_table()` and
#'   `formula_matrix()`, a `fmls` object)
#'
#' @param data A `data.frame` object that has been used by models
#'
#' @param name For `attach_data()`, the name to store the dataset under
#'   (defaults to the name `data` was passed as); for `model_data()`,
#'   the name of a single attached dataset to return (when `NULL`, the full
#'   named list is returned)
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return `attach_data()` returns the modified `mdl_tbl`; `model_failures()`
#'   returns a `tibble` with one row per failed model and its `error`
#'   message; `term_table()` returns a `tm` vector; `formula_matrix()`
#'   returns a `tibble`; `model_data()` returns a named `list` of data frames
#'   (or a single `data.frame` when `name` is given).
#'
#' @name model_table_helpers
NULL

#' @rdname model_table_helpers
#' @export
attach_data <- function(x, data, name = NULL, ...) {

  validate_class(x, "mdl_tbl")
  validate_class(data, "data.frame")

	# Get name of object that will be the dataset; an inline expression takes
	# the content-derived id that `fit()` gives the identical frame
	mc <- match.call()
	explicit <- !is.null(name)
	if (!explicit) {
		if (is.symbol(mc$data)) {
			name <- deparse1(mc$data)
		} else {
			name <- data_content_name(data)
			message(
				"`data` is an expression, not a name; attaching it as `", name, "`."
			)
		}
	}
	datLs <- attr(x, "dataList")

	# The same frame under another name: when the table references exactly one
	# detached dataset and this frame carries every variable its models use,
	# alias it to that id instead of stranding the models without data. An
	# explicit `name` is always honored as given.
	if (!explicit && nrow(x) > 0 && !name %in% x$data_id) {
		detached <- setdiff(unique(stats::na.omit(x$data_id)), names(datLs))
		fmMat <- attr(x, "formulaMatrix")
		candidates <-
			if (length(detached) == 0 || !is.data.frame(fmMat) ||
					nrow(fmMat) != nrow(x)) {
				character()
			} else {
				Filter(function(id) {
					rows <- which(!is.na(x$data_id) & x$data_id == id)
					tms <- names(fmMat)[
						colSums(fmMat[rows, , drop = FALSE], na.rm = TRUE) > 0
					]
					vars <- unique(unlist(lapply(tms, function(t) {
						tryCatch(all.vars(str2lang(t)), error = function(e) t)
					})))
					length(vars) > 0 && all(vars %in% names(data))
				}, detached)
			}
		if (length(candidates) == 1) {
			message(
				"`", name, "` carries every variable of the models fit on `",
				candidates, "`; attaching it as `", candidates,
				"`. (Pass `name = \"", name, "\"` to keep it separate.)"
			)
			name <- candidates
		}
	}

	if (name %in% names(datLs)) {
		message("Replacing the dataset `", name, "` already attached to this table.")
	}
	if (nrow(x) > 0 && !name %in% x$data_id) {
		message(
			"No models in this table reference `", name,
			"`; it is attached for later use. ",
			"(The models reference: ",
			paste(unique(stats::na.omit(x$data_id)), collapse = ", "),
			")"
		)
	}

	# Add to data list and update attributes
	datLs[[name]] <- data
	attr(x, "dataList") <- datLs

	# Return
	x
}

#' @rdname model_table_helpers
#' @export
model_failures <- function(x, ...) {

	validate_class(x, "mdl_tbl")

	status <- model_table_status(x)
	err <- vapply(x$model_summary, function(s) {
		if (is.list(s) && !is.null(s$error)) {
			as.character(s$error)[1]
		} else {
			NA_character_
		}
	}, character(1))

	out <- tibble::tibble(
		name = x$name,
		model_call = x$model_call,
		formula_call = x$formula_call,
		data_id = x$data_id,
		strata = x$strata,
		level = x$level,
		subset = x$subset,
		error = err
	)

	out[status == "failed", , drop = FALSE]

}

#' @rdname model_table_helpers
#' @export
term_table <- function(x, ...) {
	UseMethod("term_table", x)
}

#' @rdname model_table_helpers
#' @export
term_table.mdl_tbl <- function(x, ...) {
	vec_restore(attr(x, "termTable"), to = tm())
}

#' @rdname model_table_helpers
#' @export
term_table.fmls <- function(x, ...) {
	vec_restore(attr(x, "termTable"), to = tm())
}

#' @rdname model_table_helpers
#' @export
formula_matrix <- function(x, ...) {
	UseMethod("formula_matrix", x)
}

#' @rdname model_table_helpers
#' @export
formula_matrix.mdl_tbl <- function(x, ...) {
	tibble::as_tibble(attr(x, "formulaMatrix"))
}

#' @rdname model_table_helpers
#' @export
formula_matrix.fmls <- function(x, ...) {
	tibble::as_tibble(vec_data(x))
}

#' @rdname model_table_helpers
#' @export
model_data <- function(x, name = NULL, ...) {

	validate_class(x, "mdl_tbl")
	datLs <- attr(x, "dataList")

	if (is.null(name)) {
		return(datLs)
	}

	if (!name %in% names(datLs)) {
		stop(
			"No dataset named `", name, "` is attached to this table",
			if (length(datLs) > 0) {
				paste0(" (attached: ", paste(names(datLs), collapse = ", "), ")")
			} else {
				" (none are attached; see `attach_data()`)"
			},
			".",
			call. = FALSE
		)
	}

	datLs[[name]]

}

# Flattening -------------------------------------------------------------------

#' Flatten a model table to its parameter estimates
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' A `mdl_tbl` object can be flattened to its specific parameters, their
#' estimates, and model-level summary statistics -- one row per model term.
#' Models that were not fit (or failed) are dropped with a message; see
#' [model_failures()] for why they failed. This relies on the [broom::tidy()]
#' output stored when the models were made.
#'
#' # Exponentiation
#'
#' By default (`exponentiate = NULL`), estimates and confidence intervals are
#' exponentiated when the model family calls for it -- Cox models and
#' generalized linear models on a log, logit, or complementary log-log link
#' (giving hazard, odds, or rate ratios) -- and left on the linear scale
#' otherwise (e.g. `lm`). A message reports when this happens, and the
#' `exponentiated` column records the decision per row. Set `exponentiate`
#' to `TRUE` or `FALSE` to override the inference for every model, or use
#' `which` to exponentiate only the models named there.
#'
#' @param x A `mdl_tbl` object
#'
#' @param exponentiate Controls exponentiation of estimates and confidence
#'   intervals. The default `NULL` infers per model from its family and link;
#'   `TRUE`/`FALSE` forces the decision for all models.
#'
#' @param which A `character` vector of model names (the `name` column) to
#'   exponentiate, overriding the family-based inference for everything else.
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return A `tibble` with one row per model parameter, carrying the model
#'   context columns (`formula_call`, `outcome`, `strata`, ...), model-level
#'   statistics from [broom::glance()], the parameter estimates from
#'   [broom::tidy()], and an `exponentiated` marker.
#'
#' @export
flatten_models <- function(x, exponentiate = NULL, which = NULL, ...) {

	# Remove global variables
	model_statistic <- model_p_value <- model_parameters <- model_summary <-
		fit_status <- formula_call <- estimate <- conf_low <- conf_high <- NULL

	validate_class(x, "mdl_tbl")

	# Unfit rows have no estimates to flatten; say what is being left behind
	status <- model_table_status(x)
	if (any(status != "fitted")) {
		nFailed <- sum(status == "failed")
		nUnfit <- sum(status == "unfit")
		message(
			"Dropping ",
			paste(c(
				if (nFailed > 0) {
					paste0(nFailed, " failed model(s) (see `model_failures()`)")
				},
				if (nUnfit > 0) paste0(nUnfit, " unfit formula(s)")
			), collapse = " and "),
			"."
		)
	}

	y <-
		x |>
		as.data.frame() |>
		tibble::as_tibble() |>
		dplyr::filter(fit_status == TRUE) |>
		dplyr::select(dplyr::any_of(c(
			"id",
			"formula_call",
			"model_call",
			"data_id",
			"name",
			"number",
			"outcome",
			"exposure",
			"mediator",
			"interaction",
			"strata",
			"level",
			"subset",
			"model_parameters",
			"model_summary"
		))) |>
		tidyr::unnest_wider(model_summary) |>
		dplyr::rename(dplyr::any_of(c(
			model_statistic = 'statistic',
			model_p_value = 'p_value'
		))) |>
		tidyr::unnest(model_parameters)

	if (nrow(y) == 0) {
		return(y)
	}

	# Older objects may predate the recorded link function
	if (!"model_link" %in% names(y)) {
		y$model_link <- NA_character_
	}

	# Which rows land on a ratio scale: Cox-family models and generalized
	# linear models on a multiplicative link report ratios when exponentiated;
	# everything else stays linear
	inferred <-
		ifelse(is.na(y$model_call), "", y$model_call) %in%
			c("coxph", "clogit", "coxme") |
		ifelse(is.na(y$model_link), "", y$model_link) %in%
			c("log", "logit", "cloglog")
	if (is.null(exponentiate)) {
		rows <-
			if (is.null(which)) {
				inferred
			} else {
				y$name %in% which
			}
		if (is.null(which) && any(rows)) {
			kinds <- unique(paste0(
				y$model_call[rows],
				ifelse(is.na(y$model_link[rows]), "",
							 paste0("(", y$model_link[rows], ")"))
			))
			message(
				"Exponentiating estimates for ", length(unique(y$id[rows])),
				" model(s) on a ratio scale (", paste(kinds, collapse = ", "),
				"); use `exponentiate = FALSE` for the linear scale."
			)
		}
	} else if (isTRUE(exponentiate)) {
		rows <-
			if (is.null(which)) {
				rep(TRUE, nrow(y))
			} else {
				y$name %in% which
			}
	} else {
		rows <- rep(FALSE, nrow(y))
	}

	y <-
		y |>
		dplyr::mutate(dplyr::across(
			dplyr::any_of(c("estimate", "conf_low", "conf_high")),
			~ dplyr::if_else(rows, exp(.x), .x)
		)) |>
		dplyr::mutate(exponentiated = rows) |>
		dplyr::select(-dplyr::any_of("id"))

	# Return
	y

}
