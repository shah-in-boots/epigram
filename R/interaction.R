#' Estimating interaction effect estimates
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' When a model in a `mdl_tbl` carries an interaction term, the exposure's
#' effect *within each level* of the interaction variable — and its
#' confidence interval — can be derived from the stored coefficients and
#' variance-covariance matrix, without refitting. The approach follows
#' Figueiras et al. (1998): within the reference level the effect is the
#' exposure coefficient; within level *j* it is the exposure coefficient plus
#' the level's interaction coefficient, with variance
#' `var(b_exp) + var(b_j) + 2 cov(b_exp, b_j)`.
#'
#' @details
#'
#' `estimate_interaction()` requires a `mdl_tbl` subset to a single row;
#' filter before calling. The interaction variable may be **binary or
#' categorical**: every level of the attached-data factor yields a row, the
#' reference level first. Terms are matched to the model's coefficients by
#' **identity** (the tidy keys `exposure:interactionLevel`, either variable
#' order), and the variance-covariance matrix is indexed by coefficient name
#' — never by `grepl()` position.
#'
#' The exposure may also be **categorical**: its non-reference levels each
#' carry their own coefficient (`exposureLEVEL`), so the within-level effect
#' is derived per exposure level, and the returned tibble gains an
#' `exposure_level` column naming the exposure contrast each row belongs to.
#' A numeric (including binary 0/1) exposure keeps the eight-column shape.
#'
#' The interaction variable must be a factor or a numeric coded 0/1. The
#' within-level effect is the exposure coefficient plus the level's
#' interaction coefficient, which is the model's own contrast only under
#' those codings; any other numeric modifier is refused, since there are no
#' levels to condition on (`cut()` it into groups, or convert it to a
#' factor, and refit).
#'
#' The `p_value` is the single across-levels test of interaction: with one
#' interaction coefficient (a binary interaction) it is that coefficient's
#' p-value; with several (a categorical interaction, a categorical exposure,
#' or both) it is the joint Wald chi-square test of all the interaction
#' coefficients against zero. `statistic` and `df` carry the test alongside
#' it, so a table can say "joint Wald, 6 df" without recounting levels. The
#' Wald test is known to understate the evidence when a fit is separated
#' (Hauck and Donner 1977); [model_diagnostics()] shows which fits are. An
#' interaction coefficient the model could not estimate (`NA`, from an empty
#' cell of the exposure by interaction cross-classification) is left out of
#' the joint test with its degree of freedom, and the within-level effect it
#' would have formed is `NA`. So is
#' the within-level effect of any cell that holds no observations of the
#' exposure level or of the exposure's reference level: the model returns a
#' finite coefficient there, but after aliasing it measures a different
#' cell's contrast, and the remaining rows should be read knowing that the
#' parametrization has shifted (refitting with the empty level dropped is
#' the clean remedy).
#'
#' @param object A `mdl_tbl` object subset to a single row
#'
#' @param exposure The exposure variable in the model
#'
#' @param interaction The interaction variable in the model
#'
#' @param conf_level The confidence level for the confidence interval
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return A `tibble` with one row per level of the interaction variable
#'   (the reference level first) and eight columns:
#'
#'   - estimate: the exposure's effect within the interaction level
#'
#'   - conf_low: lower bound of the confidence interval for the estimate
#'
#'   - conf_high: upper bound of the confidence interval for the estimate
#'
#'   - p_value: p-value for the overall interaction effect *across levels*
#'     (the same value on every row)
#'
#'   - statistic: the test behind `p_value` -- the Wald chi-square when
#'     several interaction coefficients are tested jointly, and the
#'     coefficient's own statistic (z or t, as the engine reports it) when
#'     there is one
#'
#'   - df: the number of interaction coefficients the test spans, aliased
#'     ones excluded
#'
#'   - nobs: number of observations within the interaction level, counted
#'     on the rows the model was fit on (a stratified model's own stratum);
#'     `NA` for a model fit under a [subset_data()] rule, whose rows are not
#'     recoverable from the attached data
#'
#'   - level: level of the interaction term
#'
#'   When the exposure is categorical, one such set of rows is returned per
#'   non-reference exposure level, and an `exposure_level` column names the
#'   exposure contrast.
#'
#' @references
#' A. Figueiras, J. M. Domenech-Massons, and Carmen Cadarso, 'Regression models:
#' calculating the confidence intervals of effects in the presence of
#' interactions', Statistics in Medicine, 17, 2099-2105 (1998)
#'
#' W. W. Hauck and A. Donner, 'Wald's test as applied to hypotheses in logit
#' analysis', Journal of the American Statistical Association, 72, 851-853
#' (1977) \doi{10.1080/01621459.1977.10479969}
#'
#' @export
estimate_interaction <- function(object,
																 exposure,
																 interaction,
																 conf_level = 0.95,
																 ...) {

	validate_class(object, "mdl_tbl")
	if (nrow(object) > 1) {
		stop(
			"The `mdl_tbl` object must be subset to single row to estimate ",
			"interactions.",
			call. = FALSE
		)
	}

	if (!exposure %in% object$exposure) {
		stop("The exposure variable is not in the model set.", call. = FALSE)
	}
	# Identity, not `grepl()`: membership in this model's formula matrix admits
	# every declared modifier, rather than only the first one mirrored in the
	# scalar `mdl_tbl$interaction` convenience column.
	fm <- formula_matrix(object)
	tmProxy <- vec_proxy(term_table(object))
	isModifier <- interaction %in% tmProxy$term[tmProxy$role == "interaction"]
	inModel <- interaction %in% names(fm) && nrow(fm) == 1 &&
		isTRUE(fm[[interaction]][1] >= 1)
	if (!isModifier || !inModel) {
		stop("The interaction variable is not in the model set.", call. = FALSE)
	}

	datLs <- attr(object, "dataList")
	if (length(datLs) == 0 || !object$data_id %in% names(datLs)) {
		stop(
			"The model table object does not have the data available. ",
			"Attach the fitting data with `attach_data()`.",
			call. = FALSE
		)
	}
	dat <- datLs[[object$data_id]]
	# The counts must come from the rows the model saw: a stratified model was
	# fit on one level of its stratum, and counting the whole attached frame
	# reported every stratum the same `n`. A subset instruction is recorded
	# only by name, so its rows cannot be recovered here and the counts are
	# `NA` rather than the whole frame's
	subsetRule <- object$subset
	if (!is.na(object$strata) && object$strata %in% names(dat)) {
		dat <- dat[which(as.character(dat[[object$strata]]) == object$level), ,
							 drop = FALSE]
	}
	if (!interaction %in% names(dat)) {
		stop(
			"The interaction variable `", interaction, "` is not a column of ",
			"the attached dataset `", object$data_id, "`.",
			call. = FALSE
		)
	}

	# The model's coefficients, on the linear scale, with the stored
	# variance-covariance matrix and residual degrees of freedom
	mod <- suppressMessages(flatten_models(object, exponentiate = FALSE))
	nms <- mod$term
	coefs <- stats::setNames(mod$estimate, nms)
	varCov <- mod$var_cov[[1]]
	degFree <- unique(mod$degrees_freedom)[1]

	# The exposure's coefficient key(s): the bare name when it was modeled
	# numerically, or one key per non-reference level (`exposureLEVEL`, the
	# treatment-contrast naming) when the exposure is categorical
	expRef <- NA_character_
	if (exposure %in% nms) {
		expKeys <- stats::setNames(exposure, NA_character_)
	} else if (exposure %in% names(dat)) {
		expLvls <- levels(factor(dat[[exposure]]))
		candidates <- paste0(exposure, expLvls[-1])
		found <- candidates %in% nms
		if (length(expLvls) < 2 || !any(found)) {
			stop(
				"The exposure term `", exposure, "` was not found among the ",
				"model's terms, neither as itself nor as level coefficients (",
				paste0("`", nms, "`", collapse = ", "), ").",
				ordered_contrast_hint(dat, exposure),
				call. = FALSE
			)
		}
		if (!all(found)) {
			stop(
				"The exposure `", exposure, "` is categorical, but the level ",
				"coefficient(s) ",
				paste0("`", candidates[!found], "`", collapse = ", "),
				" were not found among the model's terms. The attached dataset `",
				object$data_id, "` may not be the data the model was fit on.",
				ordered_contrast_hint(dat, exposure),
				call. = FALSE
			)
		}
		expKeys <- stats::setNames(candidates, expLvls[-1])
		expRef <- expLvls[1]
	} else {
		stop(
			"The exposure term `", exposure, "` was not found among the model's ",
			"terms (", paste0("`", nms, "`", collapse = ", "), ").",
			call. = FALSE
		)
	}

	# The within-level effect below is `b_exp + b_int` at the non-reference
	# level, which is the model's own contrast only when the modifier is a
	# factor or is coded 0/1. A numeric modifier coded 1/2 got a wrong number
	# silently (the level-2 effect is `b_exp + 2 b_int`), and one with three
	# or more values matched the same bare key for every level, so `solve()`
	# died on a singular matrix
	modVals <- dat[[interaction]]
	if (is.numeric(modVals) && !all(stats::na.omit(modVals) %in% c(0, 1))) {
		stop(
			"The interaction variable `", interaction, "` is numeric, so there ",
			"are no levels to condition on. Convert it to a factor, or `cut()` ",
			"it into groups, and refit.",
			call. = FALSE
		)
	}

	# Levels and per-level counts come from the attached data
	intFct <- factor(modVals)
	lvls <- levels(intFct)
	if (length(lvls) < 2) {
		stop(
			"The interaction variable `", interaction, "` needs at least two ",
			"levels.",
			call. = FALSE
		)
	}
	counts <- table(intFct)
	if (!is.na(subsetRule)) {
		counts[] <- NA_integer_
	}

	# The tidy key of a level's interaction coefficient, by identity: the
	# factor form (`exposure:interactionLevel`, either variable order), or the
	# bare form when the interaction was modeled numerically
	interaction_key <- function(expKey, lvl) {
		candidates <- c(
			paste0(expKey, ":", interaction, lvl),
			paste0(interaction, lvl, ":", expKey),
			paste0(expKey, ":", interaction),
			paste0(interaction, ":", expKey)
		)
		hit <- candidates[candidates %in% nms]
		if (length(hit) == 0) {
			stop(
				"No interaction coefficient matches level `", lvl, "` of `",
				interaction, "` (looked for ",
				paste0("`", candidates[1:2], "`", collapse = ", "),
				" among the model's terms).",
				ordered_contrast_hint(dat, c(exposure, interaction)),
				call. = FALSE
			)
		}
		hit[1]
	}
	intKeys <- unlist(lapply(expKeys, function(k) {
		vapply(lvls[-1], interaction_key, character(1), expKey = k)
	}), use.names = FALSE)

	# The variance-covariance matrix is indexed by coefficient name
	if (is.null(rownames(varCov))) {
		stop(
			"The stored variance-covariance matrix carries no coefficient names, ",
			"so terms cannot be matched by identity.",
			call. = FALSE
		)
	}
	vc <- function(a, b) varCov[a, b]

	# The single across-levels interaction p-value: the coefficient's own test
	# when there is one, the joint Wald chi-square when there are several. An
	# aliased coefficient (`NA`: an empty cell of the cross-classification, so
	# the model could not estimate it) leaves the test with its degree of
	# freedom, the way `anova()` drops aliased terms, rather than voiding the
	# whole test; its own within-level effect stays `NA` below
	testKeys <- intKeys[!is.na(coefs[intKeys])]
	testDf <- length(testKeys)
	if (testDf == 0) {
		testStat <- pval <- NA_real_
	} else if (testDf == 1) {
		testStat <- mod$statistic[match(testKeys, nms)]
		pval <- mod$p_value[match(testKeys, nms)]
	} else {
		b <- coefs[testKeys]
		testStat <- drop(t(b) %*% solve(varCov[testKeys, testKeys]) %*% b)
		pval <- stats::pchisq(testStat, df = testDf, lower.tail = FALSE)
	}

	critical <-
		if (is.na(degFree)) {
			stats::qnorm(conf_level / 2 + 0.5)
		} else {
			stats::qt(conf_level / 2 + 0.5, df = degFree)
		}

	# A within-level contrast needs both of its cells populated. When the
	# exposure level (or the exposure's reference) has no observations at one
	# modifier level, the model still returns a finite number for that cell,
	# because `lm()`/`glm()` alias whichever product term comes last and the
	# remaining coefficients silently take on a different meaning: with the
	# empty cell at the modifier's reference level, the exposure main effect
	# becomes the contrast within the *last* level, and reading it as the
	# reference-level effect reports a real effect under the wrong label. An
	# empty cell is therefore `NA`, whatever the coefficients say
	empty_cell <- function(expLvl, lvl) {
		if (is.na(expLvl) || !is.na(subsetRule)) {
			return(FALSE)
		}
		expFct <- as.character(dat[[exposure]])
		intChr <- as.character(intFct)
		sum(expFct == expLvl & intChr == lvl, na.rm = TRUE) == 0 ||
			sum(expFct == expRef & intChr == lvl, na.rm = TRUE) == 0
	}

	# One row per interaction level (per exposure level, when the exposure is
	# categorical): the reference level is the exposure coefficient alone;
	# level j adds its interaction coefficient, with the covariance in the
	# variance
	rows <- list()
	for (e in seq_along(expKeys)) {
		expKey <- expKeys[[e]]
		expLvl <- names(expKeys)[e]
		rows[[length(rows) + 1]] <- list(
			estimate = if (empty_cell(expLvl, lvls[1])) NA_real_ else coefs[[expKey]],
			variance = vc(expKey, expKey),
			level = lvls[1],
			nobs = counts[[lvls[1]]],
			exposure_level = expLvl
		)
		for (lvl in lvls[-1]) {
			key <- interaction_key(expKey, lvl)
			rows[[length(rows) + 1]] <- list(
				estimate = if (empty_cell(expLvl, lvl)) NA_real_ else
					coefs[[expKey]] + coefs[[key]],
				variance = vc(expKey, expKey) + vc(key, key) +
					2 * vc(expKey, key),
				level = lvl,
				nobs = counts[[lvl]],
				exposure_level = expLvl
			)
		}
	}

	out <- dplyr::bind_rows(lapply(rows, function(r) {
		half <- critical * sqrt(r$variance)
		tibble::tibble(
			estimate = r$estimate,
			conf_low = r$estimate - half,
			conf_high = r$estimate + half,
			p_value = pval,
			statistic = testStat,
			df = testDf,
			nobs = r$nobs,
			level = r$level,
			exposure_level = r$exposure_level
		)
	}))

	# The `exposure_level` column only appears when the exposure is
	# categorical; a numeric exposure keeps the documented eight columns
	if (all(is.na(out$exposure_level))) {
		out$exposure_level <- NULL
	}

	out
}

#' Name polynomial contrasts as the cause of a missing level coefficient
#'
#' Under R's default `options("contrasts")` an ordered factor is fit with
#' polynomial contrasts, so its coefficients are `term.L`, `term.Q`, ...
#' rather than one per level, and no `exposure:modifierLevel` key can exist
#' for [estimate_interaction()] to find. The refusal that follows is correct
#' but says nothing about why; this appends the cause and the fix. It is only
#' consulted after the key search has failed, so a session that sets
#' treatment contrasts for ordered factors is never refused on the check.
#'
#' @param dat The attached dataset the model was fit on
#' @param vars The variable names whose coefficients were sought
#' @return A single string: the hint, or `""` when no variable is ordered
#' @keywords internal
#' @noRd
ordered_contrast_hint <- function(dat, vars) {
	vars <- vars[vars %in% names(dat)]
	isOrdered <- vapply(vars, function(v) is.ordered(dat[[v]]), logical(1))
	ordered <- vars[isOrdered]
	if (!length(ordered)) {
		return("")
	}
	paste0(
		" Note that ", paste0("`", ordered, "`", collapse = " and "),
		if (length(ordered) > 1) " are ordered factors" else " is an ordered factor",
		" in the attached data, so the model was fit with polynomial contrasts (`",
		ordered[1], ".L`, `", ordered[1], ".Q`, ...) rather than one coefficient ",
		"per level. Refit with `factor(", ordered[1],
		", ordered = FALSE)` to derive within-level effects."
	)
}
