#install.packages("here")
#install.packages("skimr")
#install.packages("janitor")
#install.packages("rmarkdown")

library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)
library(broom)

rawfile <- read_csv("glmer.csv")

my_data <- rawfile

clean <- my_data %>%
  select(ResponseId, grep(pattern=".Q\\d", x=colnames(my_data)))

cleaner <- clean[-c(1, 2), ]

cleaner <- tibble::rowid_to_column(cleaner, "ID")



test <- cleaner %>% 
  pivot_longer(
    cols = starts_with("N"), 
    names_to = "questions", 
    values_to = "Judgment",
    values_drop_na = TRUE
  )


test1 <- test %>%
  separate(questions, c("NV","LV","question","transitivity","protoroles"), sep = ("_"))


write.csv(test1,"\\main.csv")

# mixed effects logistic regression

library(lme4)
library(optimx)
library(emmeans)
library(sjPlot)
library(jtools)
library(car)




test1_transitive <- test1[test1$transitivity == "tr",]

test1_transitive <- test1_transitive[test1_transitive$protoroles == 1,]
test1_transitive


test1_transitive$Judgment <- as.factor(test1_transitive$Judgment)
test1_transitive$Judgment

test1_transitive$NV <- as.factor(test1_transitive$NV)
test1_transitive$LV <- as.factor(test1_transitive$LV)


#contrasts(test1_transitive$NV) <- contr.Sum(levels(test1_transitive$NV))
#contrasts(test1_transitive$LV) <- contr.Sum(levels(test1_transitive$LV))
#contrasts(test1_transitive$LV) <- as.factor(test1_transitive$LV)
#contrasts(test1_transitive$protoroles) <- contr.Sum(levels(test1_transitive$protoroles))

test1_transitive$ID <- as.numeric(as.factor(test1_transitive$ID))
test1_transitive$question <- as.numeric(as.factor(test1_transitive$question))

test1_transitive$question

contrasts(test1_transitive$NV)

test1_transitive$NV

mmmodel <- glmer(Judgment ~ 
                   NV*LV+
                   (1|ID) + (1|question), 
                 data = test1_transitive, family = binomial,
                 control=glmerControl(optimizer="bobyqa",
                                      optCtrl=list(maxfun=2e5)))



allFit(mmmodel)
help('convergence')
summary(mmmodel)
summ(mmmodel, model.info=F, model.fit=F)
# output to report in the paper
tab_model(mmmodel)

graph <-plot_model(mmmodel, 
                   type = "eff", 
                   terms= c( "LV", "NV"))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



graph

summ(mmmodel, model.info=F, model.fit=F)


levels(test1_agents$protoroles)


test1$Judgment <- as.factor(test1$Judgment)
test1$NV <- as.factor(test1$NV)
test1$LV <- as.factor(test1$LV)
test1$protoroles <- as.factor(test1$protoroles)
test1$transitivity <- as.factor(test1$transitivity)



contrasts(test1$protoroles)

mmmodel <- glmer(Judgment ~ 
                  NV*LV*protoroles+
                   (1|ID) + (1|question), 
                 data = test1, family = binomial,
                 control = glmerControl(optimizer ='optimx', 
                                        optCtrl=list(method='bobyqa')))

drop1(mmmodel,scope=.~., test = "LR")

summary(mmmodel)

emm <- emmeans(mmmodel, ~ LV*NV*protoroles)
pairs(emm, by = c("protoroles","NV"), adjust = "tukey")

graph <-plot_model(mmmodel, 
                   type = "eff", 
                   terms= c( "LV", "protoroles", "NV"))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



graph+scale_color_discrete(labels = c("Proto-Agent", "Proto-Patient"))


#PowerTest

library(mixedpower)

model <- mmmodel # which model do we want to simulate power for?
data <- test1 # data used to fit the model
fixed_effects <- c('NV * protoroles + LV * protoroles') # all fixed effects specified in FLPmodel
simvar <- "ID" # which random effect do we want to vary in the simulation?

steps <- c(50, 60, 70) # which sample sizes do we want to look at?
critical_value <- 2 # which t/z value do we want to use to test for significance?
n_sim <- 1000 # how many single simulations should be used to estimate power?

power_FLP <- mixedpower(model = mmmodel, data = test1,
                        fixed_effects = c('NV', 'protoroles', 'LV'),
                        simvar = 'ID', steps = c(50,60,70),
                        critical_value = 2, n_sim = 1000)


multiplotPower(power_FLP)


summary(mmmodel)

library(visreg)
visreg(mmmodel, "LV", by = 'protoroles')
visreg(mmmodel, "NV", by = 'protoroles')
visreg(mmmodel, "LV", by = "NV")

#library(car)
#vif(mmmodel)

library(emmeans)
library(pander)
m.emm <- emmeans(mmmodel, "LV")
m.emm


emtrends(mmmodel, pairwise ~ protoroles, var = "LV")


m.emm.df <-
  m.emm %>%
  broom::tidy()
m.emm.df

m.emm.df %>%
  ggplot(aes(LV, estimate)) +
  geom_point() +
  ylab("Estimate")

contrast(m.emm, 'tukey') %>%
  broom::tidy() %>%
  head(7)

contrast(m.emm, 'trt.vs.ctrl') %>%
  broom::tidy() %>%
  head %>%
  pander

emmip(mmmodel, LV*NV ~ protoroles) + facet_wrap(~LV)

#emmip(mmmodel, NV ~ protoroles)

#library(effects)
#e <- allEffects(m)
#print(e)

#ggplot(data = mmmodel, aes(x = LV, y = Judgments, fill = Judgments)) +
 # geom_bar(stat='identity')
