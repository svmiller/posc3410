## Attendance Policy

```{r, include=FALSE}
read_csv("~/Dropbox/teaching/attendance-grades-relationship.csv") %>%
  select(name:class,-id) %>%
  mutate(name = anonymize(name, .seed = 8675309, .algo = "crc32"),
         class = as_factor(class),
         term = forcats::fct_relevel(term,
                                     "Fall 2014", "Spring 2015",
                                     "Fall 2015", "Spring 2016",
                                     "Fall 2016", "Spring 2017",
                                     "Fall 2017", "Spring 2018",
                                     "Fall 2018", "Spring 2019",
                                     "Fall 2019")) %>%
  group_by(term, class) %>%
  mutate(maxgrade = max(grade, na.rm=T),
         grade = ifelse(maxgrade > 100, grade - (maxgrade-100), grade)) %>%
  ungroup() %>%
  mutate(perattend = (attendance/max)*100) -> Attend

# library(ggpmisc)

M1 <- lm(grade ~ perattend, Attend)
M2 <- lm(grade ~ perattend, data=subset(Attend, perattend >= 75))
my.formula <- y ~ scale(x, center=T, scale=F)

M1df <- tidy(M1)
M2df <- tidy(M2)
# library(ggplot2)

```

> *Showing up is 80 percent of life* -- Woody Allen, [via Marshall Brickman](http://quoteinvestigator.com/2013/06/10/showing-up/#note-6553-1)

Students should be wary of skipping class. I deduct *all* participation points for a class after five unexcused absences and this can have important implications for a student's overall grade in the class. There is already a strong positive correlation between the percentage of classes a student has attended in the course and the student's final grade for the semester (*r* = `r round(cor(Attend$perattend, Attend$grade, use="complete.obs"), 3)`) for all `r nrow(Attend)` students I have taught since Fall 2014.

A simple linear regression of a student's final grade on percentage of classes attended for the semester for all classes I have taught since Fall 2014 suggests an increase of one percent in attendance for the semester leads to an estimated increase of `r round(M1$coefficients[2], 3)` in the student's final grade. Whereas one missed classes constitutes about a five-percent decrease in percentage attendance for the semester, one missed class means an estimated decrease of `r round(M1$coefficients[2], 3)*5` in the overall grade. The effect of attendance on the final grade for the class is precise (*t* = `r round(M1df$statistic[2], 3)`) and the likelihood that there is no actual relationship between attendance and final grade for the semester is almost zero. This simple linear model with just one predictor (attendance) provides a good fit as well (R$^2$ = `r round(summary(M1)$r.squared, 3)`). See `r figr("attendplot", TRUE, type="Figure")` in this document.

```{r attendplot, echo = FALSE, results="asis", cache=FALSE, fig.cap="The Relationship between Class Attendance and Final Grade, by Class Type", fig.height=7, fig.width=13, fig.pos="H"}


Attend %>%
  mutate(class = forcats::fct_recode(class,
                                     "Intro to International Relations" = "1020",
                                     "Quantitative Methods" = "3410",
                                     "International Conflict" = "3610",
                                     "U.S. Foreign Policy" = "3630")) %>%
  ggplot(., aes(x = perattend, y = grade)) +
  theme_steve() +
  geom_point() +
  facet_wrap(~class) +
  #scale_x_continuous(labels=scales::percent) +
  labs(x="Percent Attendance in Class", y="Final Grade in Class (0-100)") +
  geom_smooth(method = "lm", se=TRUE, formula = my.formula) +
  stat_poly_eq(formula = my.formula,
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., "~~~~~(",..rr.label..,")", sep = "")),
               geom = "label_npc", alpha = 0.33, fill="#619cff",
               size=3,
               parse = TRUE) +
  labs(caption = "Data: My attendance records. Note: values are centered to the mean and coefficients communicate effect of a one percent increase in attendance.\nMultiply it by five to get a rough estimate of the effect of skipping/attending one class from the mean attendance rate.")

```



A student might object that attendance is partly endogenous to a grade since past classes deducted all participation points after five unexcused absences. This is true, but the findings hold even when I subset the data to cases where attendance is greater than 75% (i.e. roughly the threshold below which I deduct all participation points). Students who just meet the threshold for full participation points nevertheless get an estimated decrease of `r round(M2$coefficients[2], 3)*5` in their overall grade for each missed class. This effect is also precise (*t* = `r round(M2df$statistic[2], 3)`). Put another way, we would have observed this effect in my data if there were no *true* effect of attendance on grades about `r round(summary(M2)$coefficients[2,4]*1e19)` times in `r prettyNum(1e19, big.mark=",")` "trials" (i.e. *p* = `r round(summary(M2)$coefficients[2,4], 19)`), on average. That probability is effectively zero. *Attend class*.

## Participation Policy

I want to reward each student in the class with all the participation and attendance points. This, however, is contingent on students demonstrating in class that they have read the material and understand its basic ideas. Students routinely fail to demonstrate this en masse to the professor.

I have two tactics to coerce students to do the reading. One, I will cold-call students from a list to answer a particular question. Failure to answer a question posed by me when cold-called (or failure to be in class that day) will result in a one-point deduction of the overall grade for the semester.

I may also assign a pop quiz for the lecture and cancel the remainder of my presentation for the class period. Failure to be in class for a pop quiz will result in a zero for that quiz. I will grade each quiz and weight the percentage of correct responses against the participation grade for the semester. This means a failure to be in class for what might be the only pop quiz of the semester would result in a *zero* for the participation grade for the semester. Skip class at your own peril.

Finally, prove to me you have read the syllabus by sending me an email titled "Subdivisions." In that email, copy and paste the lyrics to "Subdivisions" by Rush from their woefully underappreciated 1982 album *Signals*. Also send me a YouTube link for the music video so that I may enjoy it (after you have already enjoyed it). You will get one point of extra credit for this exercise. This extra credit offer is valid until 9 a.m. (my Gmail time) of Aug. 28, 2019.
