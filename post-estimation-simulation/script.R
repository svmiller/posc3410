library(tidyverse)
library(stevemisc)

VSG <- haven::read_dta("~/Dropbox/data/voter-study-group/2019-11/voter_2019Nov.dta")

VSG %>%
  select(systems_leader_2019Nov, systems_army_2019Nov, systems_democracy_2019Nov, 
         birthyr_2019Nov, gender_2019Nov, race_2019Nov, race_t_2019Nov, 
         ideo5_2019Nov, educ_2019Nov, bornagain_2019Nov, faminc_2019Nov, employment_2019Nov,
         ft_white_2019Nov, ft_asian_2019Nov, ft_black_2019Nov, ft_asian_2019Nov, ft_latino_2019Nov, ft_immig_2019Nov, ft_muslim_2019Nov,
         pid3_2019Nov, pid7_2019Nov) -> Data


Data %>% 
  mutate(armyruled = case_when(
    systems_army_2019Nov %in% c(1, 2) ~ 1,
    systems_army_2019Nov %in% c(3, 4) ~ 0
  ),
  armyrule = case_when(
    systems_army_2019Nov %in% c(1, 2, 3, 4) ~ (systems_army_2019Nov*-1)+5
  ),
  strongleaderd = case_when(
    systems_leader_2019Nov %in% c(1, 2) ~ 1,
    systems_leader_2019Nov %in% c(3, 4) ~ 0
  ),
  strongleader = case_when(
    systems_leader_2019Nov %in% c(1, 2, 3, 4) ~ (systems_leader_2019Nov*-1)+5
  ),
  havedemd = case_when(
    systems_democracy_2019Nov %in% c(1,2) ~ 0,
    systems_democracy_2019Nov %in% c(3,4) ~ 1
  ),
  havedem = ifelse(systems_democracy_2019Nov > 4, NA, systems_democracy_2019Nov)) -> Data

Data %>%
  mutate(white = ifelse(race_2019Nov == 1, 1, 0)) %>%
  filter(white == 1) -> Data

Data %>%
  mutate(therm_white = ft_white_2019Nov,
         therm_black = ft_black_2019Nov,
         therm_asian = ft_asian_2019Nov,
         therm_latino = ft_latino_2019Nov,
         therm_muslim = ft_muslim_2019Nov,
         therm_immig = ft_immig_2019Nov
  ) -> Data

Data %>%
  mutate_at(vars(contains("therm_")), ~ifelse(. > 100, NA, .)) -> Data

Data %>%
  mutate(prej_black = therm_white - therm_black,
         prejudice = therm_white - (((therm_black + therm_asian + therm_latino + therm_muslim))/4)) -> Data


Data %>% 
  mutate(age = 2019 - birthyr_2019Nov,
         female = case_when(gender_2019Nov == 1 ~ 0, gender_2019Nov == 2 ~ 1),
         collegeed = case_when(educ_2019Nov %in% c(1, 2, 3, 4) ~ 0,
                               educ_2019Nov %in% c(5,6) ~ 1),
         ideo = ifelse(ideo5_2019Nov > 5, NA, ideo5_2019Nov),
         unemployed = case_when(
           employment_2019Nov %in% c(3,4) ~ 1,
           employment_2019Nov %in% c(1,2, 5,6,7,8,9) ~ 0,
         ),
         pidcat = ifelse(pid3_2019Nov > 4, NA, pid3_2019Nov),
         dem = ifelse(pidcat == 1, 1, 0),
         gop = ifelse(pidcat == 2, 1, 0),
         pid7 = ifelse(pid7_2019Nov > 7, NA, pid7_2019Nov)) -> Data


M1 <- glm(strongleaderd ~ age + female + collegeed + ideo + unemployed + dem + gop + prejudice,
          data = subset(Data, prejudice >= 0), family = binomial(link = "logit"))

M2 <- glm(armyruled ~ age + female + collegeed + ideo + unemployed + dem + gop + prejudice,
          data = subset(Data, prejudice >= 0), family = binomial(link = "logit"))

M3 <- glm(havedemd ~ age + female + collegeed + ideo + unemployed + dem + gop +  prejudice,
          data = subset(Data, prejudice >= 0), family = binomial(link = "logit"))


library(modelr)

Data %>%
  data_grid(.model = M1, prejudice = c(0, 100),
            strongleaderd = 0, armyruled = 0, havedemd = 0) -> newdat_prejudice

Data %>%
  data_grid(.model = M1, therm_immig = c(0, 100)) -> newdat_immig


library(stevemisc)
Sims <- list()

Sims[[1]] <- get_sims(M1, newdat_prejudice, 1000, 8675309) %>% mutate(cat = "Strong Leader", prejudice = rep(c(0,1), 1000))
Sims[[2]] <- get_sims(M2, newdat_prejudice, 1000, 8675309) %>% mutate(cat = "Army Rule", prejudice = rep(c(0,1), 1000))
Sims[[3]] <- get_sims(M3, newdat_prejudice, 1000, 8675309) %>% mutate(cat = "Oppose Democracy", prejudice = rep(c(0,1), 1000))

do.call("rbind", Sims) 
  mutate(y = plogis(y),
         xlab = ifelse(prejudice == 0, "No White\nSocial Prejudice", "White\nSocial Prejudice")) %>%
  group_by(cat, xlab) %>%
  summarize(mean = mean(y),
            lwr = quantile(y, .025),
            upr = quantile(y, .975)) %>%
  ggplot(.,aes(xlab, y=mean, ymin=lwr, ymax=upr)) +
  facet_wrap(~cat) +
  geom_pointrange() +
  coord_flip() +
  theme_steve_web() +
  theme(axis.text = element_text(size = 11))  +
  labs(x = "", y = "Simulated Probability (with 95% Intervals)",
       title = "The Effect of White Social Prejudice on Attitudes about Democracy, Nov. 2019",
       subtitle = "In all cases, a min-to-max effect of white social prejudice results in a percentage change in the probability of an anti-democratic attitude by at least 200%.",
       caption = "Data: Nov. 2019 Voter Study Group")


Data %>%
  select( therm_asian, therm_black, therm_latino, therm_muslim) %>%
  principal(.)
