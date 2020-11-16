library(tidyverse)

WVS <- readRDS(url("http://posc3410.svmiller.com/toy-data/wvs-trumpism.rds"))


M1 <- glm(sldummy ~ z_age + I(z_age^2) + female +
            hsedorless + z_ideo + z_incscale + gop + unemployed +
            + z_lemanc + gop*hsedorless,
          data=subset(WVS), family=binomial(link = "logit"))

summary(M1)

M2 <- glm(sldummy ~ z_age + I(z_age^2) + female +
            hsedorless + z_ideo + z_incscale + gop + unemployed +
            + z_lemanc + gop*hsedorless,
          data=subset(WVS, year == 2011), family=binomial(link = "logit"))

summary(M2)

M3 <- glm(hddummy ~ z_age + I(z_age^2) + female +
            hsedorless + z_ideo + z_incscale + gop + unemployed +
            + z_lemanc + gop*hsedorless,
          data=subset(WVS), family=binomial(link = "logit"))

summary(M3)
