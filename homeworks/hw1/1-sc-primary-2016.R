library(tidyverse)

SCP16 <- readRDS(url("http://posc3410.svmiller.com/toy-data/SCP16.rds"))

# Get mean and median of Trump's vote share

SCP16 %>%
  summarize(meanvotetrump = mean(trump),
            medianvotetrump = median(trump)) %>% data.frame

# Get a density plot of the distribution of Trump's vote share, county-wide

SCP16 %>%
  ggplot(.,aes(trump)) + geom_density() +
  scale_x_continuous(limits=c(20,60))


# Get mean and median of county population size

SCP16 %>%
  summarize(meanpopsize = mean(population),
            medianpopsize = median(population)) %>% data.frame

# Get a density plot of the county population size

SCP16 %>%
  ggplot(.,aes(population)) + geom_density() +
  scale_x_continuous(labels = scales::comma)

# Get top five most populous counties and bottom five for comparison sake (if that helps you)

SCP16 %>%
  # Reshuffle the data such that the most populous counties come first
  arrange(-population) %>%
  # Create a rank variable for context
  mutate(rank = 1:n()) %>%
  # Get top 5 and bottom 5 by population
  # I had to cheat a little bit, knowing SC has 46 counties
  slice(1:5, 42:46) %>%
  # Select just what we want to look at
  select(rank, county, population)

# Create natural logarithmic transformations of county population size

SCP16 %>%
  mutate(logpop = log(population)) -> SCP16

# Get and compare mean and medians for population and log(population)
SCP16 %>%
  summarize(meanpop = mean(population),
            medianpop = median(population),
            meanlogpop = mean(logpop),
            medianlogpop = median(logpop)) %>%
  gather(var, val) %>%
  data.frame

# Get a density plot of natural log of county population size

SCP16 %>%
  ggplot(.,aes(logpop)) + geom_density() +
  scale_x_continuous(limits=c(8,14))

# ...and compare with the other one

SCP16 %>%
  ggplot(.,aes(population)) + geom_density() +
  scale_x_continuous(labels = scales::comma)

# Extra Credit

summary(M1 <- lm(trump ~ illiteracy +  unemployment + perblack, SCP16))
