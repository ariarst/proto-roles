library(janitor)
library(skimr)
library(here)
library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)
library(broom)

frequency_test <- read_csv("frequency-test.csv")
freq_data <- frequency_test

view(freq_data)
clean_freq <- freq_data %>%
  select(ResponseId, grep(pattern=".Q\\d", x=colnames(freq_data)))

cleaner_freq <- clean_freq[-c(1, 2), ]

test_freq <- cleaner_freq %>% 
  pivot_longer(
    cols = starts_with("N"), 
    names_to = "questions", 
    values_to = "Judgment",
    values_drop_na = TRUE
  )


test1_freq <- test_freq %>%
  separate(questions, c("NV","LV","question","transitivity"), sep = ("_"))
view(test1_freq)

test1_freq$transitivity <- sub("\\...*", "", test1_freq$transitivity)

frequency()

plot <- ggplot(data = test1_freq, aes(x = LV, y = frequency(Judgment), fill = Judgment)) +
  geom_bar(position = position_fill(), stat = "identity")+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot + facet_wrap(~NV)

plot+ labs(x = 'Light Verbs', y = 'Total Number of Answers') + 
  facet_wrap(~NV) +   
  scale_y_continuous(labels = scales::percent_format())

