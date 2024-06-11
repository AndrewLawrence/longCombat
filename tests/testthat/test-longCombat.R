

# set random seed
set.seed(1)
# simulate the covariates
simdata <- data.frame(
  subid = rep(1:100, each = 5),
  age = rep(sample(c(20:60), 100, replace = TRUE), each = 5),
  diagnosis = rep(c(0, 1), each = 250),
  time = rep(0:4, times = 100)
)
# define 4 batch patterns
# each column of this matrix represents a batch pattern over time
batch.patterns <- matrix(c(1, 1, 1, 2, 2,
                           3, 3, 4, 4, 5,
                           6, 6, 6, 6, 6,
                           7, 7, 8, 8, 8), ncol = 4)

# inspect the batch patterns:
batch.patterns

# randomly sample 100 batch patterns
batch.pattern.sample <- sample(1:4, 100, replace = TRUE)
simdata$batch <- as.vector(batch.patterns[, batch.pattern.sample])
# simulate the brain features
features <- matrix(rnorm(100 * 5 * 20), nrow = 500)
# simulate additive batch effects (normally distributed)
gamma <- runif(n = 8, min = -5, max = 5)
tau <- runif(n = 8, min = 0.1, max = 0.3)
batch.add <- matrix(c(
  rnorm(mean = gamma[1], sd = tau[1], n = 20),
  rnorm(mean = gamma[2], sd = tau[2], n = 20),
  rnorm(mean = gamma[3], sd = tau[3], n = 20),
  rnorm(mean = gamma[4], sd = tau[4], n = 20),
  rnorm(mean = gamma[5], sd = tau[5], n = 20),
  rnorm(mean = gamma[6], sd = tau[6], n = 20),
  rnorm(mean = gamma[7], sd = tau[7], n = 20),
  rnorm(mean = gamma[8], sd = tau[8], n = 20)
),
ncol = 8)
# simulate multiplicative batch effects (inverse gamma distributed)
lambda <- sample(c(2, 3), 8, replace = TRUE)
theta <- sample(c(0.5, 1), 8, replace = TRUE)
batch.mult <- matrix(
  c(
    rinvgamma(n = 20, shape = lambda[1], scale = theta[1]),
    rinvgamma(n = 20, shape = lambda[2], scale = theta[2]),
    rinvgamma(n = 20, shape = lambda[3], scale = theta[3]),
    rinvgamma(n = 20, shape = lambda[4], scale = theta[4]),
    rinvgamma(n = 20, shape = lambda[5], scale = theta[5]),
    rinvgamma(n = 20, shape = lambda[6], scale = theta[6]),
    rinvgamma(n = 20, shape = lambda[7], scale = theta[7]),
    rinvgamma(n = 20, shape = lambda[8], scale = theta[8])
  ),
  ncol = 8
)
# add / multiply batch effects to the features
for (i in 1:500) {
  features[i, ] <- features[i, ] * batch.mult[, simdata$batch[i]] +
    batch.add[, simdata$batch[i]]
}
# add covariate effects to the features
features <- features -
  0.1 * simdata$age + simdata$diagnosis -
  0.5 * simdata$time -
  2.0 * simdata$diagnosis * simdata$time
# add subject random effect to the features
#   (will be the same across features in this case)
features <- features + rep(rnorm(n = 100), each = 5)
# save feature names
featurenames <- paste0("feature", 1:20)
colnames(features) <- featurenames
# combine into one data frame
simdata <- data.frame(simdata, features)

simdata_with_missing <- simdata
simdata_with_missing[27, "feature6"] <- NA


test_that("missing data produces error", {
  testthat::expect_error({
    simdata_combat <- longCombat(
      idvar = "subid",
      timevar = "time",
      batchvar = "batch",
      features = featurenames,
      formula = "age + diagnosis*time",
      ranef = "(1|subid)",
      data = simdata_with_missing
    )
  })
})
