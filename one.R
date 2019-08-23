library(knitr)
library(tidyverse)
library(plotly)
library(DBI)

data <- read.csv("datasets/adult_data.csv")
names(data) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "gender", "capital_gain", "capital_loss", "hours_per_week", "native_country", "predictive_variable")
head(data)

data$is_rich <- if_else(data$predictive_variable == "<=50K", 0, 1)
education_summary <- data %>% group_by(education) %>% summarise(number_of_rich_people = sum(is_rich), number_of_poor_people = n() - number_of_rich_people, total_people = n())
education_summary
print(unique(data$predictive_variable))
print(unique(as.character(data$predictive_variable)))

salary <- unique(as.character(data$predictive_variable))
str_sub(salary, 2, str_length(salary))
gsub(" ", "", salary)
trimws(salary)
salary

library(microbenchmark)

microbenchmark(str_sub(salary, 2, str_length(salary)), gsub(" ", "", salary), trimws(salary))
salary <- str_sub(salary, 2, str_length(salary))
salary

data$predictive_variable <- str_sub(data$predictive_variable, 2, str_length(data$predictive_variable))

data$is_rich <- if_else(data$predictive_variable == "<=50K", 0, 1)
education_summary <- data %>% group_by(education) %>% summarise(number_of_rich_people = sum(is_rich), number_of_poor_people = n() - number_of_rich_people, total_people = n())
education_summary

education_data <- distinct(data %>% select(education, education_num)) %>% arrange(education_num)
education_data

data$is_rich <- if_else(data$predictive_variable == "<=50K", 0, 1)




education_summary <- data %>%
	group_by(education, education_num) %>%
	summarise(percentage_of_rich_people = sum(is_rich) / n() * 100) %>%
	arrange(education_num)
education_summary
ggplotly(ggplot(education_summary, aes(x = reorder(education, education_num), y = percentage_of_rich_people)) + geom_bar(stat = "identity") + coord_flip())

ggplotly(ggplot(education_summary, aes(x = education_num, y = percentage_of_rich_people, color = education)) + geom_point())





occupation_summary <- data %>%
	group_by(occupation) %>%
	summarise(percentage_of_rich_people = sum(is_rich) / n() * 100) %>%
	arrange(education_num)
occupation_summary
ggplotly(ggplot(occupation_summary, aes(x = occupation, y = percentage_of_rich_people)) + geom_bar(stat = "identity") + coord_flip())



