## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library("unmconf")
library("bayesplot")
library("ggplot2"); theme_set(theme_minimal())

set.seed(13L)
df <- 
  runm(n = 100,
       type = "int", 
       missing_prop = .75,
       covariate_fam_list = list("norm", "bin", "norm"),
       covariate_param_list = list(c(mean = 0, sd = 1), c(.3), c(0, 2)),
       unmeasured_fam_list = list("norm", "bin"),
       unmeasured_param_list = list(c(mean = 0, sd = 1), c(.3)),
       treatment_model_coefs = 
         c("int" = -1, "z1" = .4, "z2" = .5, "z3" = .4, 
           "u1" = .75, "u2" = .75),
       response_model_coefs =
         c("int" = -1, "z1" = .4, "z2" = .5, "z3" = .4,
           "u1" = .75, "u2" = .75, "x" = .75),
       response = "norm",
       response_param = c("si_y" = 1))

rbind(head(df, 5), tail(df, 5))

## -----------------------------------------------------------------------------
# Main Study Data
M <- tibble::tibble(
  "y" = rbinom(100, 1, .5),
  "x" = rbinom(100, 1, .3),
  "z1" = rnorm(100, 0, 1),
  "z2" = rnorm(100, 0, 1),
  "z3" = rnorm(100, 0, 1),
  "u1" = NA, 
  "u2" = NA
)

# External Validation Data
EV <- tibble::tibble(
  "y" = rbinom(100, 1, .5),
  "z1" = rnorm(100, 0, 1),
  "z2" = rnorm(100, 0, 1),
  "u1" = rnorm(100, 0, 1), 
  "u2" = rnorm(100, 0, 1)
)

df_ext <- dplyr::bind_rows(M, EV) |> 
  dplyr::mutate(x = ifelse(is.na(x), 0, x))

rbind(head(df_ext, 5), tail(df_ext, 5))

## -----------------------------------------------------------------------------
unm_mod <- 
  unm_glm(form1 = y ~ x + z1 + z2 + z3 + u1 + u2,     # y ~ .,
          form2 = u1 ~ x + z1 + z2 + z3 + u2,         # u1 ~ . - y,
          form3 = u2 ~ x + z1 + z2 + z3,              # u2 ~ . - y - u1,
          family1 = gaussian(),
          family2 = gaussian(),
          family3 = binomial(),
          priors = c("lambda[u1]" = "dnorm(.5, 1)"),
          n.iter = 10000, n.adapt = 4000, thin = 1,
          data = df)

## ----eval=FALSE---------------------------------------------------------------
#  unm_mod_ext <-
#    unm_glm(form1 = y ~ x + z1 + z2 + u1 + u2,     # y ~ . - z3,
#            form2 = u1 ~ x + z1 + z2 + u2,         # u1 ~ . - y - z3,
#            form3 = u2 ~ x + z1 + z2,              # u2 ~ . - y - u1 - z3,
#            family1 = binomial(),
#            family2 = gaussian(),
#            family3 = gaussian(),
#            priors = c("gamma[x]" = "dnorm(1.1, 0.9)",
#                       "delta[x]" = "dnorm(1.1, 4.5)"),
#            n.iter = 4000, n.adapt = 2000, thin = 1,
#            data = df_ext)

## ----eval=FALSE---------------------------------------------------------------
#  unm_glm(..., code_only = TRUE)
#  jags_code(unm_mod)

## ----fig.align='center', fig.height = 4, fig.width = 6, fig.cap="Histogram of the MCMC draws for the internal validation example. Smooth histogram is an indication of good convergence."----
bayesplot::mcmc_hist(unm_mod, pars = "beta[x]")

## ----fig.align='center', fig.height = 4, fig.width = 6, fig.cap="Trace plot of the MCMC draws for the internal validation example. No patterns or diverging chains in the trace plot is an indication of good convergence."----
bayesplot::mcmc_trace(unm_mod, pars = "beta[x]")

## ----echo=FALSE---------------------------------------------------------------
# df2 <-
#   runm_extended(n = 100,
#                 covariate_fam_list = list("norm", "bin", "norm"),
#                 covariate_param_list = list(c(mean = 0, sd = 1), c(.3), c(0, 2)),
#                 unmeasured_fam_list = list("norm", "bin"),
#                 unmeasured_param_list = list(c(mean = 0, sd = 1), c(.3)),
#                 treatment_model_coefs =
#                   c("int" = -1, "z1" = .4, "z2" = .5, "z3" = .4,
#                     "u1" = .75, "u2" = .75),
#                 response_model_coefs =
#                   c("int" = -1, "z1" = .4, "z2" = .5, "z3" = .4,
#                     "u1" = .75, "u2" = .75, "x" = .75),
#                 response = "norm",
#                 response_param = c("si_y" = 1),
#                 type = "int",
#                 missing_prop = .99)
# 
# unm_mod2 <-
#   unm_glm(y ~ x + z1 + z2 + z3 + u1 + u2,     # y ~ .,
#           u1 ~ x + z1 + z2 + z3 + u2,         # u1 ~ . - y,
#           u2 ~ x + z1 + z2 + z3,              # u2 ~ . - y - u1,
#           family1 = gaussian(),
#           family2 = gaussian(),
#           family3 = binomial(),
#           #priors = c("lambda[u1]" = "dnorm(0, 4)"),
#           n.iter = 4000, n.adapt = 2000, thin = 1,
#           data = df2)
# 
# write_rds(unm_mod2, "/Users/ryanhebdon/Graduate School/Research/unmconf_personal/Diagnostics/bad_convergence.rds")

#bad_mod <- readRDS("/Users/ryanhebdon/Graduate School/Research/unmconf_personal/Diagnostics/bad_convergence.rds")

## -----------------------------------------------------------------------------
unm_summary(unm_mod, df) |>
  head(10)

## -----------------------------------------------------------------------------
unm_backfill(df, unm_mod)[16:25, ]

## ----message=FALSE, warning=FALSE, fig.align='center', fig.height = 4, fig.width = 6, fig.cap= "A credible interval plot for the internal validation example. The light blue circle indicates the posterior median for each parameter, with the bold and thin blue lines displaying the 50% and 95% credible interval, respectively. Red circles are the true parameter values used in the simulation study."----
bayesplot::mcmc_intervals(unm_mod, prob_outer = .95, 
                          regex_pars = "(beta|lambda|gamma|delta|zeta).+") +
  geom_point(
    aes(value, name), data = tibble::enframe(attr(df, "params")) |>
      dplyr::mutate(name = gsub("int", "1", name)),
    color = "red", fill = "pink", size = 4, shape = 21
  )

