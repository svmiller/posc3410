library(tidyverse)
library(stevemisc) 


Abortion <- readRDS(url("http://posc3410.svmiller.com/toy-data/wvs-usa-abortion.rds"))

# Install necessary packages first (if you don't have them).
# I tried to keep this as minimal as possible, but you'll need some additional packages.
# That's my bad. Sorry.

list.of.packages <- c("broom","arm","lme4","modelr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Of note: modelr will be the only one I ask you to directly load right now
# broom and arm will be loaded quietly

library(modelr)


# First, let's get a histogram to see what we're dealing with here...

Abortion %>%
  select(aj) %>% na.omit %>%
  group_by(aj) %>%
  tally() %>%
  ggplot(.,aes(as.factor(aj), n)) + geom_bar(stat="identity") +
  geom_text(aes(label=n), vjust = -.5) +
  labs(x = "Justifiability of Abortion",
       y = "Number of Observations in Each Unique Response Type",
       caption = "Data: World Values Survey, 1981-2011")


# Let's go ahead and treat it as interval...

M1 <- lm(aj ~ age + I(age^2)  + female + ideology + satisfinancial + cai  + godimportant, data=Abortion)
summary(M1)
# I like broom::tidy summaries better than straight up summary()
broom::tidy(M1)


# Let's standardize the coefficients now...

Abortion %>%
  mutate_at(vars("age", "ideology", "satisfinancial",
                 "cai", "godimportant"), list(z = ~r2sd(.))) %>%
  rename_at(vars(contains("_z")),
            ~paste("z", gsub("_z", "", .), sep = "_") ) -> Abortion


# Let's get Model 2

M2 <- lm(aj ~ z_age + I(z_age^2)  + female + z_ideology + z_satisfinancial + z_cai + z_godimportant, data=Abortion)
summary(M2)
broom::tidy(M2)

# Let's keep it easy and look at the effect of age with not standardizing it.
# To be clear: quantities of interest are going to be a case of six-to-one when standardizing or not standardizing it
# However, regression coefficients *will* change. Quantities of interest like these (simulated values of y) *won't*
# But, this will be a little more straightforward for students in seeing the underlying code. So let's roll with it.

M3 <- lm(aj ~ age + I(age^2)  + female + z_ideology + z_satisfinancial + z_cai + z_godimportant, data=Abortion)
summary(M3)

# Let's create a hypothetical row of new data
# Herein, the respondent is a typical woman of average (technically: median) ideology, financial situation, authoritarian views, and religiosity.
# The only thing that varies is age, which goes from the minimum (17) to maximum (96).

Abortion %>%
  data_grid(.model = M3, age=seq(min(age, na.rm=T), max(age, na.rm=T), by =1),
            aj = 0) -> newdatM3


# Let's get some simulations from the multivariate normal:
simsM3 <- get_sims(M3, newdatM3, 1000, 8675309) 

# Let's add in some identifiers so we better know what we're looking at:
newdatM3 %>%
  dplyr::slice(rep(row_number(), 1000)) %>%
  bind_cols(simsM3, .) -> simsM3


# And let's plot it...
# Grouping by 1000 simulations for each age, let's get the median and 95% bounds around it.
simsM3 %>%
  group_by(age) %>%
  summarize(meany = mean(y),
            lwr = quantile(y, .025),
            upr = quantile(y, .975)) %>%
  ggplot(.,aes(age, meany, ymin=lwr, ymax=upr)) +
  geom_ribbon(alpha=0.3, color="black") +
  geom_line(color="blue") +
  scale_x_continuous(breaks = seq(20, 100, by=5)) +
  labs(x = "Age",
       y = "Simulated Attitude Toward the Jusifiability of Abortion (with 95% Intervals)")

# Extra credit
# If you choose to do the extra credit, you'll need this package:
library(lme4)

M4 <- lmer(aj ~ z_age + I(z_age^2)  + female + z_ideology + z_satisfinancial + z_cai + z_godimportant + (1 | year), 
           data = Abortion)
summary(M4)

show_ranef(M4, "year")
