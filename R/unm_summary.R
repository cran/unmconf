#' Generate synthetic data
#'
#' [unm_summary()] produces result summaries of the results from the model fitting
#' function, [unm_glm()]. The table of results are summarized from the MCMC draws of
#' the posterior distribution.
#'
#'
#' @param mod Output from [unm_glm()].
#' @param data The data `mod` was generated with.
#' @param quantiles A numeric vector of quantiles.
#' @return A `tibble`
#'
#' @name unm_summary
#' @examples
#'
#' \donttest{
#'
#' # ~~ One Unmeasured Confounder Examples (II-Stage Model) ~~
#'
#' # normal response, normal confounder model with internally validated data
#' (df <- runm(20, response = "norm"))
#'
#' (unm_mod <- unm_glm(
#'   y ~ x + z1 + z2 + z3 + u1, family1 = gaussian(),
#'   u1 ~ x + z1 + z2 + z3,     family2 = gaussian(),
#'   data = df
#' ))
#'
#' glm(y ~ x + z1 + z2 + z3, data = df)
#'
#' coef(unm_mod)
#' jags_code(unm_mod)
#' unm_summary(unm_mod)
#' unm_summary(unm_mod, df) # true values known df
#'
#' # impute missing values with model
#' unm_backfill(df, unm_mod)
#'
#' }
#'



#' @export
#' @rdname unm_summary
unm_summary <- function(mod, data, quantiles = c(.025, .975)) {

  param <- NULL; rm(param)
  true_value <- NULL; rm(true_value)
  sd <- NULL; rm(sd)

  summary <- summary(mod, quantiles = quantiles)

  stats <- summary$statistics |>
    as.data.frame() |>
    tibble::as_tibble(rownames = "param") |>
    janitor::clean_names() |>
    dplyr::select(param, mean, sd)

  cis <- summary$quantiles |>
    as.data.frame() |>
    tibble::as_tibble(rownames = "param")

  out <- stats |>
    dplyr::left_join(cis, by = "param")

  if (!missing(data)) {
    out <- out |>
      dplyr::left_join(
        tibble::enframe(attr(data, "param"), name = "param", value = "true_value"),
        by = "param"
      ) |>
      dplyr::relocate(true_value, .after = param)
  }

  out

}



#' @export
#' @rdname unm_summary
unm_backfill <- function(data, mod) {

  if(inherits(attr(mod, "form3"), "formula")) {
    # get names
    confounder1_name <- deparse(attr(mod, "form2")[[2]])
    confounder2_name <- deparse(attr(mod, "form3")[[2]])

    # grab data
    u1 <- data[[confounder1_name]]
    u2 <- data[[confounder2_name]]

    # grab fitted things
    fitted_u1 <- as.numeric(coef(attr(mod, "jm"))[["U"]][, 1])
    fitted_u2 <- as.numeric(coef(attr(mod, "jm"))[["U"]][, 2])

    if (length(fitted_u1) > 0) u1 <- apply(cbind(u1, fitted_u1), 1, sum, na.rm = TRUE)
    if (length(fitted_u2) > 0) u2 <- apply(cbind(u2, fitted_u2), 1, sum, na.rm = TRUE)

    if (length(fitted_u1) > 0) data[[confounder1_name]] <- u1
    if (length(fitted_u2) > 0) data[[confounder2_name]] <- u2

    if (length(fitted_u1) > 0) data[[paste0(confounder1_name, "_observed")]] <- is.na(fitted_u1)
    if (length(fitted_u2) > 0) data[[paste0(confounder2_name, "_observed")]] <- is.na(fitted_u1)

    data
  }else {
    # get names
    confounder1_name <- deparse(attr(mod, "form2")[[2]])

    # grab data
    u1 <- data[[confounder1_name]]

    # grab fitted things
    fitted_u1 <- as.numeric(coef(attr(mod, "jm"))[["U"]][, 1])

    if (length(fitted_u1) > 0) u1 <- apply(cbind(u1, fitted_u1), 1, sum, na.rm = TRUE)

    if (length(fitted_u1) > 0) data[[confounder1_name]] <- u1

    if (length(fitted_u1) > 0) data[[paste0(confounder1_name, "_observed")]] <- is.na(fitted_u1)

    data
  }
}


#' @export
#' @rdname unm_summary
unm_dic <- function(mod) {
  with(
    attributes(mod),
    dic.samples(jm, n.iter, thin, progress.bar = getOption("unm_progress.bar"))
  )
}





