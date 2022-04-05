#install.packages("here")
#install.packages("skimr")
#install.packages("janitor")
#install.packages("rmarkdown")

library(janitor)
library(skimr)
library(here)
library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)
library(broom)

rawfile <- read_csv("Proto-roles- test_April 5, 2022_10.47.csv")

my_data <- rawfile
#colnames(my_data) <- sub("\\...*", "", colnames(my_data))
#view(my_data)
view(my_data)

#skim(my_data)

#skim_without_charts(my_data)
#glimpse(my_data)
#head(my_data)

clean <- my_data %>%
  select(ResponseId, grep(pattern=".Q\\d", x=colnames(my_data)))
#view(clean) 
#head(clean)

cleaner <- clean[-c(1, 2), ]
#skim(my_data)

#wanted <- (grep(pattern=".Q\\d", x=colnames(my_data)))

test <- cleaner %>% 
  pivot_longer(
    cols = starts_with("N"), 
    names_to = "questions", 
    values_to = "Judgment",
    values_drop_na = TRUE
  )

view(test)


test1 <- test %>%
  separate(questions, c("NV","LV","question","transitivity","protoroles"), sep = ("_"))
view(test1)

#test1$protoroles <- sub("\\...*", "", test1$protoroles)
#view(test1)

write.csv(test1,"\\data.csv")


#test2 <- unite(test1, Judgments,protoroles:Judgment, sep = "/", remove = TRUE, na.rm = FALSE)
#view(test2)


summary(test1)

# logistic regression

lvmodel <- glm(as.numeric(Judgment) ~ NV * protoroles + LV * protoroles, 
              data = test1, family = 'binomial')


summary(lvmodel)


# mixed effects logistic regression

library(lme4)

mmmodel <- glmer(as.numeric(Judgment) ~ NV * protoroles + transitivity * protoroles + LV * protoroles +
                   (1|ResponseId) + (1|question), 
               data = test1, family = binomial)

summary(mmmodel)

library(effects)
e <- allEffects(mmmodel)
print(e)

#ggplot(data = mmmodel, aes(x = LV, y = Judgments, fill = Judgments)) +
 # geom_bar(stat='identity')
