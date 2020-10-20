# See blog post for inspiration for this exercise.
#  http://svmiller.com/blog/2020/03/normal-distribution-central-limit-theorem-inference/

library(tidyverse)
# library(stevemisc) # optional for this exercise

# RUN PREVIOUSLY:
# Population <- rbnorm(250000, mean =40.01578, sd = 40.24403,
#                      lowerbound = 0, 
#                      upperbound = 100,
#                      round = TRUE,
#                      seed = 8675309) # Jenny, I got your number...
# 
# 
# 
# Population %>% as_tibble() %>%
#   mutate(uid = 1:n()) %>%
#   rename(therm = value) %>%
#   select(uid, therm) -> Population

Population <- readRDS(url("http://posc3410.svmiller.com/toy-data/population.rds"))


Population %>%
  group_by(therm) %>%
  tally() %>%
  ggplot(.,aes(therm, n)) + geom_bar(stat="identity", fill="#619cff",color="black")
  

# Get five samples of size 10
# don't forget your seed!
set.seed(8675309) # Jenny, I got your number...

# Get the actual samples and conver them to a data frame.
Samp5_10 <- sapply(1:5, function(i){ x <-sample(Population$therm, 10, replace = TRUE) }) %>% 
  # coerce matrix to data frame, then tibble
  as.data.frame %>% as_tibble() %>%
  gather(sample, therm)

# Get sample mean of each of the five samples
Samp5_10 %>% group_by(sample) %>% summarize(meantherm = mean(therm))

# Get a million samples of size 10
# don't forget your seed!
set.seed(8675309) # Jenny, I got your number...

# Get 100,000 samples of size 10.
# Btw, this might take a while and it's fine that it does.
# There are faster sampling approaches in R, but they require more add-on packages.
Samp100k10 <- sapply(1:1e5, function(i){ x <- mean(sample(Population$therm, 10, replace = TRUE)) })  %>% 
  as_tibble() %>% mutate(iter = 1:n()) %>% select(iter, value) %>% rename(sampmean = value)


Samp100k10 %>%
  ggplot(.,aes(sampmean)) + 
  geom_density(linetype="dashed") +
  geom_vline(xintercept = mean(Population$therm), linetype="dotted") +
  stat_function(fun=dnorm,
                color="black", size=1.5, 
                args=list(mean=mean(Samp100k10$sampmean), 
                          sd=sd(Samp100k10$sampmean))) +
  labs(caption = "Density plot in dashed lines. Normal function with sample mean and standard deviation overlayed in thick, solid line.")

# Extra credit
sample_sizes <- c(10, 25, 100, 400, 2000, 4000, 10000)
Samps = list() 
# don't forget your seed!
set.seed(8675309) # Jenny, I got your number...
for (j in sample_sizes) {
  Samps[[paste0("Sample size: ", j)]] = data.frame(sampsize=j, samp=sapply(1:10, function(i){ x <- sample(Population$therm, j, replace = FALSE) }))
}

Samps %>%
  map_df(as_tibble) %>%
  gather(samp, value, samp.1:samp.10) -> Samps


Samps %>%
  group_by(sampsize, samp) %>%
  mutate(sampmean = mean(value),
         se = sd(Population$therm)/sqrt((sampsize)),
         lb95 = sampmean - 1.96*se,
         ub95 = sampmean + 1.96*se) %>%
  distinct(sampsize, samp, sampmean, se, lb95, ub95) %>%
  ungroup() %>%
  mutate(sampsize = fct_inorder(paste0("Sample Size: ", sampsize)),
         samp = as.numeric(str_replace(samp, "samp.", ""))) %>%
  ggplot(.,aes(as.factor(samp), sampmean, ymax=ub95, ymin=lb95)) +
  facet_wrap(~sampsize) +
  geom_hline(yintercept = mean(Population$therm), linetype="dashed") +
  geom_pointrange() + coord_flip() +
  labs(y = "Sample Mean (with 95% Intervals)",
       x = "Sample Number [1:10]",
       title = "Ten Sample Means of Varying Sizes (with 95% Intervals) from a Population",
       caption = "Simulated data for Homework #3",
       subtitle = "This is extra credit, so the students should interpret this plot for themselves. :P")

