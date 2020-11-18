con <- DBI::dbConnect(RSQLite::SQLite(), "westernjackman1994bicr/westernjackman1994bicr.db")


library(tidyverse)
library(stevemisc)
library(stevedata)
library(brms)
# library(kableExtra)

library(broom)
library(tidybayes)

# Wallerstein's priors
# left: 3(1.5)
# size: -5(2.5) // This is what he's arguing
# concen: 0(10^6) // diffuse/"ignorance" prior
# Intercept: 0(10^6) // diffuse/"ignorance" prior
wall_priors <- c(set_prior("normal(3,1.5)", class = "b", coef= "left"),
                 set_prior("normal(-5,2.5)", class = "b", coef="size"),
                 set_prior("normal(0,10^6)", class="b", coef="concen"),
                 set_prior("normal(0,10^6)", class="Intercept"))

# Stephens priors
# left: 3(1.5) // they both agree about left governments
# size: 0(10^6) // diffuse/"ignorance" prior
# concen: 10(5) // This is what Stephens thinks it is.
# Intercept: 0(10^6) // diffuse/"ignorance" prior
stephens_priors <- c(set_prior("normal(3,1.5)", class = "b", coef= "left"),
                     set_prior("normal(0,10^6)", class = "b", coef="size"),
                     set_prior("normal(10,5)", class="b", coef="concen"),
                     set_prior("normal(0,10^6)", class="Intercept"))


M1 <- lm(union ~ left + size + concen, data=uniondensity)

copy_to(con, broom::tidy(M1), "M1",
        temporary=FALSE, overwrite = TRUE)

# Uninformative priors
B0 <- brm(union ~ left + size + concen,
          data=uniondensity,
          seed = 8675309,
          chains = 4, cores = 4,
          family="gaussian")


B0 %>% gather_draws(b_Intercept, sigma, b_left, b_concen, b_size) -> tidyB0

copy_to(con, tidyB0, "B0",
        temporary=FALSE, overwrite = TRUE)


tribble(
  ~term, ~estimate, ~`std.error`, ~lwr, ~upr,
  "Intercept", 97.59, 57.48, 3.04, 192.14,
  "Left Government", .27, .08, .15, .39,
  "Labor Force Size (logged)", -6.46, 3.79, -12.70, -.22,
  "Industrial Concentration", .35, 19.25,  -31.32, 32.02
) -> WJM1 

copy_to(con, WJM1, "WJM1",
        temporary=FALSE, overwrite = TRUE)


# Wallerstein's priors
B1 <- brm(union ~ left + size + concen,
          data = uniondensity,
          prior=wall_priors,
          seed = 8675309,
          chains = 4, cores = 4,
          family="gaussian")

B1 %>% gather_draws(b_Intercept, sigma, b_left, b_concen, b_size) -> tidyB1


# Stephens' priors
B2 <- brm(union ~ left + size + concen,
          data = uniondensity,
          prior=stephens_priors,
          seed = 8675309,
          chains = 4, cores = 4,
          family="gaussian")


B2 %>% gather_draws(b_Intercept, sigma, b_left, b_concen, b_size) -> tidyB2

copy_to(con, tidyB1, "B1",
        temporary=FALSE, overwrite = TRUE)

copy_to(con, tidyB2, "B2",
        temporary=FALSE, overwrite = TRUE)


# The sensitivity analyses are:
# 1) keep the informative priors, but exclude Italy
# 2) multiply the variances by 10, and exclude Italy.
#    Notice: variances. Take sd to power 2, then multiply by 10, then sqrt()



copy_to(con, tidyB3, "B3",
        temporary=FALSE, overwrite = TRUE)

copy_to(con, tidyB4, "B4",
        temporary=FALSE, overwrite = TRUE)

copy_to(con, tidyB5, "B5",
        temporary=FALSE, overwrite = TRUE)

copy_to(con, tidyB6, "B6",
        temporary=FALSE, overwrite = TRUE)
 
DBI::dbDisconnect(con)
rm(con)
