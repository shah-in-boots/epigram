# Class ------------------------------------------------------------------------

#' Model Prototypes
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x Model object or representation
#'
#' @param formulas Formula(s) given as either an `formula` or as a `fmls` object
#'
#' @param parameter_estimates A `data.frame` that contains columns
#'   representing terms and individual estimates or coefficients, can be
#'   accompanied by additional statistic columns. By default, assumes
#'
#'  * __term__ = term name
#'
#'  * __estimate__ = estimate or coefficient
#'
#' @param summary_info A `list` that contains columns representing
#'   summary statistic of a model. By default, assumes...
#'
#' * __nobs__ = number of observations
#'
#' * __degrees_freedom__ = degrees of freedom
#'
#' * __statistic__ = test statistic
#'
#' * __p_value__ = p-value for overall model
#'
#' * __var_cov__ = variance-covariance matrix for predicted coefficients
#'
#' @param data_name String representing name of dataset that was used
#'
#' @param strata_variable String of a term that served as a stratifying
#'   variable
#'
#' @param strata_level Value of the level of the term specified by
#'   `strata_variable`
#'
#' @param subset_name Label of the [subset_data()] instruction the model was
#'   fit under, if any (e.g. `"am == 1"`)
#'
#' @param ... Arguments to be passed to or from other methods
#'
#' @return An object of the `mdl` class, which is essentially an equal-length
#'   list of parameters that describe a single model. It retains the original
#'   formula call and the related roles in the formula.
#'
#' @name models
#' @export
mdl <- function(x = unspecified(), ...) {

	# Early break for empty objects
	if (length(x) == 0) {
		return(new_model())
	}

	UseMethod("mdl", object = x)
}

#' @rdname models
#' @export
mdl.character <- function(x,
													formulas,
													parameter_estimates = data.frame(),
													summary_info = list(),
													data_name,
													strata_variable = NA_character_,
													strata_level = NA_character_,
													subset_name = NA_character_,
													...) {

	# Any fitting approach can be named; it only needs to be named something
	checkmate::assert_string(x, min.chars = 1)

	# Ensure equal length objects for the data frames
	if (length(parameter_estimates) == 0) {
		parameter_estimates <- tibble::tibble(
			term = NA_character_,
			estimate = NA
		)
	}

	if (length(summary_info) == 0) {
		summary_info <- list(
			nobs = NA,
			p_value = NA,
			statistic = NA,
			degrees_freedom = NA_integer_,
			var_cov = NA
		)
	}

	# Data arguments
	dtArgs <-
		list(dataName = data_name,
				 strataVariable = strata_variable,
				 strataLevel = strata_level,
				 subsetName = subset_name)

	# Assume additional arguments are for the model (from the dots)
	dots <- list(...)

	new_model(
		modelCall = x,
		modelFormula = formulas,
		modelArgs = dots,
		parameterEstimates = parameter_estimates,
		summaryInfo = summary_info,
		dataArgs = dtArgs
	)

}

#' @rdname models
#' @export
mdl.lm <- function(x = unspecified(),
									 formulas = fmls(),
									 data_name = character(),
									 strata_variable = character(),
									 strata_level = character(),
									 subset_name = character(),
									 ...) {

	# Get parameter information
	# None of these values are exponentiated
	# This must be done at the end through an additional function
	pe <- my_tidy(x)

	# Get model information
	si <-
		my_glance(x) |>
		as.list()

	new_model_from_fit(
		x,
		cl = x$call,
		parameterEstimates = pe,
		summaryInfo = si,
		formulas = formulas,
		data_name = data_name,
		strata_variable = strata_variable,
		strata_level = strata_level,
		subset_name = subset_name
	)
}

#' @rdname models
#' @export
mdl.glm <- mdl.lm

#' @rdname models
#' @export
mdl.coxph <- mdl.lm

#' @rdname models
#' @export
mdl.lmerMod <- function(x = unspecified(),
												formulas = fmls(),
												data_name = character(),
												strata_variable = character(),
												strata_level = character(),
												subset_name = character(),
												...) {

	if (!requireNamespace("broom.mixed", quietly = TRUE)) {
		stop(
			"The {broom.mixed} package is required to describe mixed models.",
			call. = FALSE
		)
	}

	# Fixed effects are what flows into tables; random effects are context
	pe <- tryCatch(
		broom.mixed::tidy(x, conf.int = TRUE, effects = "fixed") |>
			dplyr::rename_with(.fn = ~ gsub("\\.", "_", x = .x)),
		error = function(e) tibble::tibble(term = NA_character_, estimate = NA)
	)

	si <- tryCatch(
		as.list(broom.mixed::glance(x)),
		error = function(e) list()
	)
	names(si) <- gsub("\\.", "_", names(si))

	# Mixed models are S4; their call lives in a slot
	new_model_from_fit(
		x,
		cl = x@call,
		parameterEstimates = pe,
		summaryInfo = si,
		formulas = formulas,
		data_name = data_name,
		strata_variable = strata_variable,
		strata_level = strata_level,
		subset_name = subset_name
	)
}

#' @rdname models
#' @export
mdl.glmerMod <- mdl.lmerMod

#' Assemble a `mdl` from a fitted model's pieces
#'
#' The shared trunk of the fitted-model methods (`mdl.lm`, `mdl.lmerMod`, and
#' their aliases): the recorded call supplies the extra fitting arguments and
#' the fallback data name, the strata/subset context defaults in, the formula
#' rebuilds from the fit when none was given (the walker understands
#' `(1 | id)` syntax), and the summary gains the fields every model carries
#' (degrees of freedom, the variance-covariance matrix, the link).
#' @keywords internal
#' @noRd
new_model_from_fit <- function(x,
															 cl,
															 parameterEstimates,
															 summaryInfo,
															 formulas = fmls(),
															 data_name = character(),
															 strata_variable = character(),
															 strata_level = character(),
															 subset_name = character()) {

	# Class check
	validate_class(formulas, "fmls")
	validate_class(data_name, "character")
	validate_class(strata_variable, "character")

	# Model class/type
	mc <- class(x)[1]

	# Model formula
	if (length(formulas) == 0) {
		mf <-
			stats::formula(x) |>
			fmls()
	} else {
		mf <- formulas
	}

	# Model arguments
	ma <- list()
	nms <- names(cl)[!names(cl) %in% c("formula", "data", "")]
	for (i in seq_along(nms)) {
		ma[[nms[i]]] <- cl[[nms[i]]]
	}

	# Model data, if not specified
	if (length(data_name) == 0) {
		data_name <- deparse1(cl[["data"]])
	}
	if (length(strata_variable) == 0 | length(strata_level) == 0) {
		strata_variable <- NA
		strata_level <- NA
	}
	if (length(subset_name) == 0) {
		subset_name <- NA
	}

	da <-
		list(dataName = data_name,
				 strataVariable = strata_variable,
				 strataLevel = strata_level,
				 subsetName = subset_name)

	si <- summaryInfo

	# Degrees of freedom by each model family's own accounting
	dfRes <- tryCatch(stats::df.residual(x), error = function(e) NULL)
	if (is.null(dfRes) || length(dfRes) == 0 || is.na(dfRes)) {
		dfRes <- tryCatch(
			stats::nobs(x) - length(stats::coef(x)),
			error = function(e) NA_integer_
		)
	}
	si$degrees_freedom <- dfRes

	si$var_cov <- tryCatch(as.matrix(stats::vcov(x)), error = function(e) NA)

	# The link function a model's estimates live on, when it declares one
	# (e.g. "logit" for a binomial `glm`); lets `flatten_models()` decide
	# whether exponentiation gives a meaningful ratio
	si$model_link <- tryCatch({
		link <- stats::family(x)$link
		if (is.character(link) && length(link) == 1) {
			link
		} else {
			NA_character_
		}
	}, error = function(e) NA_character_)

	# `my_tidy()` falls back to a scalar `NA` when tidying fails; the
	# constructor needs a one-row frame in that case
	if (!is.data.frame(parameterEstimates)) {
		parameterEstimates <- tibble::tibble(term = NA_character_, estimate = NA)
	}

	# Creation
	new_model(
		modelCall = mc,
		modelFormula = mf,
		modelArgs = ma,
		parameterEstimates = parameterEstimates,
		summaryInfo = si,
		dataArgs = da
	)
}

#' @rdname models
#' @export
mdl.model_fit <- function(x, ...) {

	if (!requireNamespace("parsnip", quietly = TRUE)) {
		stop(
			"The {parsnip} package is required to unwrap fitted model specifications.",
			call. = FALSE
		)
	}

	# Delegate to the engine's own fit (e.g. the `lm` inside `linear_reg()`)
	mdl(parsnip::extract_fit_engine(x), ...)
}

#' @rdname models
#' @export
mdl.default <- function(x, ...) {
	stop("`mdl()` is not defined for a `",
			 class(x)[1],
			 "` object.",
			 call. = FALSE
	)
}

#' @rdname models
#' @export
model <- mdl

#' Model vector definition
#' @keywords internal
#' @noRd
new_model <- function(modelCall = character(),
											modelFormula = fmls(),
											modelArgs = list(),
											parameterEstimates = data.frame(),
											summaryInfo = list(),
											dataArgs = list()) {

	# Model description is essentially deconstructed here
	# Allows for reconstruction of the model, but lightweight, like a blueprint
	# Everything needs to be the same length

	# Arguments...
	# 	modelCall = fitting function
	# 	modelFormula = fmls() version of standard formula
	#		modelArgs = additional arguments passed to model as a named list
	#		parameterEstimates = values of terms and their estimates + statistics
	#		summaryInfo = model summary fit information, e.g. R-squared
	#		dataArgs = how and what context the model was fit

	validate_class(modelCall, "character")
	validate_class(modelFormula, "fmls")
	validate_class(modelArgs, "list")
	validate_class(parameterEstimates, "data.frame")
	validate_class(summaryInfo, "list")
	validate_class(dataArgs, "list")

	if (length(modelCall) == 0) {
		mc <- list()
		ma <- list()
		mf <- list()
		pe <- list()
		si <- list()
		da <- list()
	} else {
		mc <- list(modelCall)
		ma <- list(modelArgs)
		mf <- list(modelFormula)
		pe <- list(parameterEstimates)
		si <- list(summaryInfo)
		da <- list(dataArgs)
	}

	new_rcrd(
		fields = list(
			"modelCall" = mc,
			"modelFormula" = mf,
			"modelArgs" = ma,
			"parameterEstimates" = pe,
			"summaryInfo" = si,
			"dataArgs" = da
		),
		class = "mdl"
	)
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("mdl", "vctrs_vctr"))

# Output -----------------------------------------------------------------------

#' @export
format.mdl <- function(x, ...) {

	# Character representation of formula
	if (vec_size(x) == 0) {
		return()
	} else {
		fmt <-
			sapply(x, FUN = function(.x) {
				f <- as.character(field(.x, "modelFormula")[[1]])
				cl <- field(.x, "modelCall")
				paste0(cl, "(", f, ")")
			})
	}
	# Return
	fmt

}

#' @export
obj_print_data.mdl <- function(x, ...) {
	if (vec_size(x) == 0) {
		new_model()
	}

	if (vec_size(x) >= 1) {
		cat(format(x), sep = "\n")
	} else {
		cat(format(x))
	}
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.mdl <- function(x, ...) {
	out <- format(x)
	pillar::new_pillar_shaft_simple(out, align = "left")
}

#' @export
vec_ptype_full.mdl <- function(x, ...) {
	"model"
}

#' @export
vec_ptype_abbr.mdl <- function(x, ...) {
	"mdl"
}

# Casting and coercion ---------------------------------------------------------

#' @export
vec_ptype2.mdl.mdl <- function(x, y, ...) {
	x
}

#' @export
vec_cast.mdl.mdl <- function(x, to, ...) {
	x
}
