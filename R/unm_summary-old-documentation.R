if (FALSE) {



# a normal-normal model - external validation
(df <- runm(c(10, 10), type = "ext", response = "norm"))

(unm_mod <- unm_glm(
  y ~ x + z1 + z2 + z3 + u1,
  u1 ~ x + z1 + z2 + z3,
  family1 = gaussian(),
  family2 = gaussian(),
  data = df
))

unm_backfill(df, unm_mod)


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
unm_backfill(df, unm_mod)
unm_summary(unm_mod, df)

# computing the dic. penalty = effective number of parameters
unm_dic(unm_mod)
coef(unm_mod)
unm_backfill(df, unm_mod)


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
  y ~ x + z + u1 + u2, family1 = gaussian(),
  u1 ~ x + z + u2, family2 = gaussian(),
  u2 ~ x + z, family3 = gaussian(),
  data = df
))
glm(y ~ x + z, data = df)
coef(unm_mod)
unm_summary(unm_mod)
unm_summary(unm_mod, df) # true values known df
unm_backfill(df, unm_mod)

# a normal-normal-normal model - external validation
(df <- runm(
  c(20, 20),
  type = "ext",
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
  y ~ x + z + u1 + u2, family1 = gaussian(),
  u1 ~ x + z + u2, family2 = gaussian(),
  u2 ~ x + z, family3 = gaussian(),
  data = df
))
unm_backfill(df, unm_mod)





}
