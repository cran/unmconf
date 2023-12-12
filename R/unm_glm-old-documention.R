if (FALSE) {

# a normal-normal model - external validation
(df <- runm(c(10, 10), type = "ext", response = "norm"))

unm_glm(
  y ~ x + z1 + z2 + z3 + u1,  family1 = gaussian(),
  u1 ~ x + z1 + z2 + z3,      family2 = gaussian(),
  data = df
)



# setting custom priors
unm_glm(
  y ~ .,      family1 = gaussian(),
  u1 ~ . - y, family2 = gaussian(),
  data = df,
  code_only = TRUE
)

unm_glm(
  y ~ .,      family1 = gaussian(),
  u1 ~ . - y, family2 = gaussian(),
  data = df,
  code_only = FALSE,
  priors = c("lambda[u1]" = "dnorm(1, 10)"),
  response_nuisance_priors = "tau_{y} <- sigma_{y}^-2; sigma_{y} ~ dunif(0, 100)",
  response_params_to_track = "sigma_{y}",
  confounder1_nuisance_priors = "tau_{u1} <- sigma_{u1}^-2; sigma_{u1} ~ dunif(0, 100)",
  confounder1_params_to_track = "sigma_{u1}"
)


# turn progress tracking on
options("unm_progress.bar" = "text")



# more complex functional forms _for non-confounder predictors only_
# zero-intercept model
unm_glm(
  y ~ . - 1,
  u1 ~ . - y,
  family1 = gaussian(),
  family2 = gaussian(),
  data = df
)
glm(y ~ . - 1, data = df)

# polynomial model
unm_glm(
  y ~ x + poly(z1, 2) + u1,
  u1 ~ x + z1,
  family1 = gaussian(),
  family2 = gaussian(),
  data = df
)
glm(y ~ x + poly(z1, 2), data = df)

# interaction model
unm_glm(
  y ~ x*z1 + u1, family1 = gaussian(),
  u1 ~ x*z1,     family2 = gaussian(),
  data = df
)
glm(y ~ x*z1, data = df)



# a binomial-binomial model
(df <- runm(
  50,
  missing_prop = .75,
  response = "bin",
  unmeasured_fam_list = list("bin"),
  unmeasured_param_list = list(.5)
))
(unm_mod <- unm_glm(
  y ~ .,
  u1 ~ . - y,
  family1 = binomial(),
  family2 = binomial(),
  data = df
))
glm(y ~ . - u1, family = binomial(), data = df)



# a poisson-normal model
(df <- runm(
  25,
  response = "pois",
  response_model_coefs = c("int" = -1, "z" = .5, "u1" = .5, "x" = .5),
  treatment_model_coefs = c("int" = -1, "z" = .5, "u1" = .5),
  covariate_fam_list = list("norm"),
  covariate_param_list = list(c(mean = 0, sd = 1)),
  unmeasured_fam_list = list("norm"),
  unmeasured_param_list = list(c(0, 1))
))

(unm_mod <- unm_glm(
  y ~ x + z + u1 + offset(log(t)),
  u1 ~ x + z,
  family1 = poisson(),
  family2 = gaussian(),
  data = df
))
glm(y ~ x + z + offset(log(t)), family = poisson(), data = df)



# a poisson-binomial model
(df <- runm(
  25,
  response = "pois",
  response_model_coefs = c("int" = -1, "z" = .5, "u1" = .5, "x" = .5),
  treatment_model_coefs = c("int" = -1, "z" = .5, "u1" = .5),
  covariate_fam_list = list("norm"),
  covariate_param_list = list(c(mean = 0, sd = 1)),
  unmeasured_fam_list = list("bin"),
  unmeasured_param_list = list(.5)
))

(unm_mod <- unm_glm(
  y ~ x + z + u1 + offset(log(t)), family1 = poisson(),
  u1 ~ x + z,                      family2 = binomial(),
  data = df
))

glm(y ~ x + z + offset(log(t)), family = poisson(), data = df)



# a gamma-normal model
(df <- runm(
  25,
  response = "gam",
  response_model_coefs = c("int" = -1, "z" = .5, "u1" = .5, "x" = .5),
  treatment_model_coefs = c("int" = -1, "z" = .5, "u1" = .5),
  covariate_fam_list = list("norm"),
  covariate_param_list = list(c(mean = 0, sd = 1)),
  unmeasured_fam_list = list("norm"),
  unmeasured_param_list = list(c(0, 1))
))

(unm_mod <- unm_glm(
  y ~ x + z + u1, family1 = Gamma(),
  u1 ~ x + z,     family2 = gaussian(),
  data = df
))

glm(y ~ x + z, family = Gamma(link = "log"), data = df)



# a gamma-binomial model
(df <- runm(
  25,
  response = "gam",
  response_model_coefs = c("int" = -1, "z" = .5, "u1" = .5, "x" = .5),
  treatment_model_coefs = c("int" = -1, "z" = .5, "u1" = .5),
  covariate_fam_list = list("norm"),
  covariate_param_list = list(c(mean = 0, sd = 1)),
  unmeasured_fam_list = list("bin"),
  unmeasured_param_list = list(.5)
))

(unm_mod <- unm_glm(
  y ~ x + z + u1, family1 = Gamma(),
  u1 ~ x + z,     family2 = binomial(),
  data = df
))
print(df, n = 25)

glm(y ~ x + z, family = Gamma(link = "log"), data = df)



# the output of unm_glm() is classed jags output

(df <- runm(20, response = "norm"))
(unm_mod <- unm_glm(
  y ~ .,
  u1 ~ . - y,
  family1 = gaussian(),
  family2 = gaussian(),
  data = df)
)
class(unm_mod)
jags_code(unm_mod)
unm_glm(y ~ ., u1 ~ . - y, data = df, code_only = TRUE)





# visualizing output
library("ggplot2")
library("bayesplot"); bayesplot_theme_set(ggplot2::theme_minimal())
mcmc_hist(unm_mod, facet_args = list(labeller = label_parsed))
mcmc_hist(unm_mod)
mcmc_trace(unm_mod, facet_args = list(labeller = label_parsed))

# more extensive visualization with the tidyverse
mcmc_intervals(unm_mod, prob = .90) +
  geom_point(
    aes(value, name), data = tibble::enframe(attr(df, "params")),
    color = "red", fill = "pink", size = 4, shape = 21
  )


library("dplyr")
library("tidyr")
unm_mod %>%
  as.matrix() %>%
  as_tibble() %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>%
  ggplot(aes("0", val)) +
  geom_jitter() +
  geom_point(
    aes("0", value), data = tibble::enframe(attr(df, "params"), name = "var"),
    color = "red", fill = "pink", size = 4, shape = 21
  ) +
  coord_flip() +
  facet_grid(var ~ ., scales = "free_y", labeller = label_parsed) +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    strip.text.y = element_text(angle = 0)
  )


# getting draws out
(samps <- posterior::as_draws_df(unm_mod))
samps$`.chain`
samps$`.iteration`
samps$`.draw`



# implementation is variable-name independent
(df <- runm(100, response = "norm"))
df$ht <- df$y
df$age <- df$u1
df$biom <- df$x
(unm_mod <- unm_glm(
  ht ~ x + biom + age,
  age ~ x + biom,
  data = df,
  family1 = gaussian(),
  family2 = gaussian()
))
jags_code(unm_mod)

# ~~ Two Unmeasured Confounders Examples (III-Stage Model) ~~
# a normal-normal-normal model - internal validation
(df <- runm(
  50,
  missing_prop = .75,
  response = "norm",
  response_model_coefs = c("int" = -1, "z" = .5,
                           "u1" = .5, "u2" = .5, "x" = .5),
  treatment_model_coefs = c("int" = -1, "z" = .5,
                            "u1" = .5, "u2" = .5),
  covariate_fam_list = list("norm"),
  covariate_param_list = list(c(mean = 0, sd = 1)),
  unmeasured_fam_list = list("norm", "norm"),
  unmeasured_param_list = list(c(0, 1), c(0, 1))
))
(unm_mod <- unm_glm(
  y ~ x + z + u1 + u2,
  u1 ~ x + z + u2,
  u2 ~ x + z,
  family1 = gaussian(),
  family2 = gaussian(),
  family3 = gaussian(),
  data = df
))

glm(y ~ x + z, data = df)
coef(unm_mod)

unm_glm(
  ~ x + z + u1 + u2, family1 = gaussian(),
  u1 ~ x + z + u2,   family2 = gaussian(),
  u2 ~ x + z,        family3 = gaussian(),
  data = df,
  code_only = TRUE
)



# a normal-normal-normal model - external validation
(df <- runm(
  c(20, 20),
  type = "ext",
  response = "norm",
  response_model_coefs = c("int" = -1, "z" = .5,
                           "u1" = .5, "u2" = .5, "x" = .5),
  treatment_model_coefs = c("int" = -1, "z" = .5, "u1" = .5, "u2" = .5),
  covariate_fam_list = list("norm"),
  covariate_param_list = list(c(mean = 0, sd = 1)),
  unmeasured_fam_list = list("norm", "norm"),
  unmeasured_param_list = list(c(0, 1), c(0, 1))
))
(unm_mod <- unm_glm(
  y ~ x + z + u1 + u2,  family1 = gaussian(),
  u1 ~ x + z + u2,      family2 = gaussian(),
  u2 ~ x + z,           family3 = gaussian(),
  data = df
))



# a binomial-binomial-binomial model - internal validation
(df <- runm(
  25,
  response = "bin",
  response_model_coefs = c("int" = -1, "z" = .5,
                           "u1" = .5, "u2" = .5, "x" = .5),
  treatment_model_coefs = c("int" = -1, "z" = .5, "u1" = .5, "u2" = .5),
  covariate_fam_list = list("norm"),
  covariate_param_list = list(c(mean = 0, sd = 1)),
  unmeasured_fam_list = list("bin", "bin"),
  unmeasured_param_list = list(.5, .75)
))
unm_glm(y ~ x + z + u1 + u2, family1 = binomial(),
        u1 ~ x + z + u2,     family2 = binomial(),
        u2 ~ x + z,          family3 = binomial(),
        data = df,
        code_only = TRUE
)






}
