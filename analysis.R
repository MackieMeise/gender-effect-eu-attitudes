# load packages ####
library(sampleSelection)
library(lme4)
library(sjPlot)
library(ggplot2)
library(ggpubr)

# run model without selection, trust as DV ####
# filter DKs
df_2 <- filter(df, trust.eu.dk==1)

# run model
m1 <- glm(trust.eu ~ trust.nat+gender+age.grouped+pol.place+educ.grouped + country, data=df_2, family=binomial(link="probit"))

# model results
summary(m1)

# run model with selection ####
m2 <- heckit(trust.eu.dk ~ trust.nat+gender+age.grouped+pol.place+educ.grouped+country,
            trust.eu ~ trust.nat+gender+age.grouped+pol.place+educ.grouped+country,
            data=df)

#model results
summary(m2)

# run model without selection, eu.image as DV ####
# filter DKs
df_2 <- filter(df, eu.image.dk==1)

# run model
m1 <- lm(eu.image ~ gender+age.grouped+pol.place+educ.grouped + country, data=df_2)

# model results
summary(m1)

# run model with selection ####
m2 <- heckit(eu.image.dk ~ gender+age.grouped+pol.place+educ.grouped+country,
             eu.image ~ gender+age.grouped+pol.place+educ.grouped+country,
             data=df)

#model results
summary(m2)

# run model without selection, democracy satisfaction as DV ####
# filter DKs
df_2 <- filter(df, sat.demo.eu.dk==1)

# run model
m1 <- lm(sat.demo.eu ~ gender+age.grouped+pol.place+educ.grouped + country, data=df_2)

# model results
summary(m1)

# run model with selection ####
m2 <- heckit(sat.demo.eu.dk ~ gender+age.grouped+pol.place+educ.grouped+country,
             sat.demo.eu ~ gender+age.grouped+pol.place+educ.grouped+country,
             data=df)

#model results
summary(m2)

# run model without selection, eu.enlar as DV ####
# filter DKs
df_2 <- filter(df, eu.enlar.dk==1)

# run model
m1 <- glm(eu.enlar ~ gender+age.grouped+pol.place+educ.grouped + country, data=df_2, family=binomial(link="probit"))

# model results
summary(m1)

# run model with selection ####
m2 <- heckit(eu.enlar.dk ~ gender+age.grouped+pol.place+educ.grouped+country,
             eu.enlar ~ gender+age.grouped+pol.place+educ.grouped+country,
             data=df)

#model results
summary(m2)

m1 <- glm(trust.eu.dk ~ gender+age.grouped+pol.place+educ.grouped + Gender.Equality.Index, data=df, family=binomial(link="probit"))
m2 <- glm(trust.eu.dk ~ gender+age.grouped+pol.place+educ.grouped + gender*Gender.Equality.Index, data=df, family=binomial(link="probit"))

p1 <- plot_model(m1, type="eff", terms="gender")
p2 <- plot_model(m2, type="eff", terms=c("Gender.Equality.Index", "gender"))
ggarrange(p1,p2)
