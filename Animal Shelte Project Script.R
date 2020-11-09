#Animal Shelter Project
#Group 6: Dingsheng Kuang, Mengyuan Guo, Saverio Hans, Yezi Zhou

#Pre-processing data
library(tidyverse)
shelter <- read_csv("/Users/kurasame/Documents/Fall 2019/shelterdata.csv")
shelter <- subset(shelter, select = -AnimalID)
#Transfer name and breed into dummy vars
shelter <- shelter %>%
  mutate(if_name = case_when(
    Name != 'NA' ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(if_breed = case_when(
    grepl("Mix", shelter$Breed) == TRUE ~ 1,
    grepl("/", shelter$Breed) == TRUE ~ 1,
    TRUE ~ 0
  ))
shelter <- subset(shelter, select = -c(Name, Breed))
#Extract days, months and seasons from DateTime
shelter <- shelter %>%
  separate(DateTime, into = c("Date", "Time"), sep = " ") %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  mutate(month = as.numeric(format(x = Date, "%m")),
       year = format(x = Date, "%Y"),
       day = as.numeric(format(x = Date, "%d")))
shelter <- shelter %>%
  mutate(season = case_when(
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    month %in% 9:11 ~ "Fall",
    TRUE ~ "Winter"
  )) 
#Unify the unit of age
shelter <- shelter %>%
  mutate(Age_ctg = case_when(
    grepl("year", shelter$AgeuponOutcome) == TRUE ~ "Over 1-year-old",
    grepl("month", shelter$AgeuponOutcome) == TRUE ~ "1-year younger",
    grepl("week", shelter$AgeuponOutcome) == TRUE ~ "1-month younger",
    grepl("day", shelter$AgeuponOutcome) == TRUE ~ "1-month younger"
  ))
shelter <- shelter %>%
  separate(AgeuponOutcome, into = c("num", "unit"), sep = " ") 
shelter <- shelter %>%
  mutate(Age_num = case_when(
    grepl("year", shelter$unit) == TRUE ~ as.numeric(shelter$num)*12,
    grepl("month", shelter$unit) == TRUE ~ as.numeric(shelter$num),
    grepl("week", shelter$unit) == TRUE ~ as.numeric(shelter$num)/4,
    grepl("day", shelter$unit) == TRUE ~ as.numeric(shelter$num)/30
  ))
shelter <- subset(shelter, select = -c(num, unit))
shelter <- shelter %>% 
  mutate(Age_ctg = factor(Age_ctg, levels = c("1-month younger", "1-year younger", "Over 1-year-old")),
         season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")))
#Re-classify colors less than 30 counts to "Other"
length(unique(shelter$Color))
counts <- 1
for (i in shelter$Color){
  if(sum(shelter$Color == i) < 15){
    shelter$Color[counts] <- "Other"
  }
  counts <- counts+1
}
counts <- 1
length(unique(shelter$Color))
#shelter %>% group_by(Color) %>% tally(sort = TRUE)
#Classify sex into female/male
shelter <- shelter %>%
  separate(SexuponOutcome, into = c("Sterility", "Gender"), sep = " ", remove = FALSE)
shelter$Gender<-replace(shelter$Gender, is.na(shelter$Gender), "Unknown")
shelter <- shelter %>%
  mutate(Sterility = if_else(Sterility %in% c("Spayed", "Neutered"), 1, 0)) %>%
  mutate(Sterility = factor(Sterility, levels = c(0, 1), labels = c("Intact or Unknown", "Spayed or Neutered")))
shelter <- na.omit(shelter)

#Overview of data -- plots
library(ggplot2)
#If have a name
plot.name <- shelter %>%
  mutate(if_name = factor(if_name, levels = c(0,1), labels = c("Without a name", "With a name"))) %>%
  group_by(if_name, OutcomeType) %>%
  tally(sort = TRUE)
ggplot(data = plot.name, aes(x = if_name, y = n, fill = OutcomeType)) +
  labs(x = "Name", y = "Count", title = "If Animal Has a Name")+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=20))+
  geom_col()
#Mixed/pure breed
plot.breed <- shelter %>%
  mutate(if_breed = factor(if_breed, levels = c(0, 1), labels = c("Pure", "Mixed"))) %>%
  group_by(if_breed, OutcomeType) %>%
  tally(sort = TRUE)
ggplot(data = plot.breed, aes(x = if_breed, y = n, fill = OutcomeType)) +
  labs(x = "Breed", y = "Count", title = "If Animal is Pure Breed or Mixed Breed")+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=20))+
  geom_col()
#Color
plot.color <- shelter %>%
  group_by(Color, OutcomeType) %>%
  tally(sort = TRUE)
ggplot(data = plot.color, aes(x = reorder(Color, -n), y = n))+
  geom_bar(stat = "identity", aes(fill = OutcomeType)) +
  labs(x = "Color", y = "Count", title = "Count Animals of Each Color")+
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA),
        text = element_text(size=20))
#Ratio by Color
plot.color_wide <- plot.color %>% 
  spread(key = OutcomeType, value = n) %>%
  mutate(ratio = Positive/(Positive + Negative)) %>%
  arrange(desc(ratio))
#Top 10 color in ratio
ggplot(data = head(plot.color_wide, 10), aes(x = reorder(Color, -ratio), y = ratio)) +
  geom_col(width = 0.8, aes(fill = "orange"))+
  labs(x = "Color", y = "Adopted Ratio", title = "Top 10 Popular Colors in Adopted Ratio")+
  theme(text = element_text(size=20),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")
#Last 10 color in ratio
ggplot(data = tail(plot.color_wide, 10), aes(x = reorder(Color, -ratio), y = ratio)) +
  geom_col(width = 0.8, fill = "#33cccc")+
  labs(x = "Color", y = "Adopted Ratio", title = "Last 10 Colors in Adopted Ratio")+
  theme(text = element_text(size=20),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")
#Dog/cats
plot.type <- shelter %>%
  group_by(AnimalType, OutcomeType) %>%
  tally(sort = TRUE)
ggplot(data = plot.type, aes(x = AnimalType, y = n))+
  geom_col(aes(fill = OutcomeType)) +
  labs(x = "Animal Type", y = "Count", title = "Count of Animals by Type")+
  theme(text = element_text(size=20),
        plot.title = element_text(hjust = 0.5))
#Sex
plot.gender <- shelter %>%
  group_by(Gender, OutcomeType) %>%
  tally(sort = TRUE)
ggplot(data = plot.gender, aes(x = reorder(Gender, -n), y = n))+
  geom_bar(stat = "identity", aes(fill = OutcomeType)) +
  coord_polar() + 
  theme(panel.background = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "grey"),
        text = element_text(size=20))+
  labs(x = "Gender", y = "Count", title = "Count of Animals by Gender")
plot.sex <- shelter %>%
  group_by(Sterility, OutcomeType) %>%
  tally(sort = TRUE)
ggplot(data = plot.sex, aes(x = reorder(Sterility, -n), y = n))+
  geom_bar(stat = "identity", aes(fill = OutcomeType)) +
  theme(panel.background = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "grey"),
        text = element_text(size=20))+
  labs(x = "Sex Condition", y = "Count", title = "Count of Animals by Sex Condition")

#Age
ggplot(data = shelter, aes(x = Age_num, fill = OutcomeType), alpha = .8) +
  geom_area(stat = "bin", binwidth = 30)+
  labs(x = "Age in Months", y = "Count", title = "Count of Animals by Age in Months")+
  theme(text = element_text(size=20),
        plot.title = element_text(hjust = 0.5))
plot.age <- shelter %>%
  group_by(Age_ctg, OutcomeType) %>%
  tally()
ggplot(data = plot.age, aes(x = Age_ctg, y = n, fill = OutcomeType))+
  geom_col()+
  labs(x = "Age", y = "Count", title = "Count of Animals by Age")+
  theme(text = element_text(size=20),
        plot.title = element_text(hjust = 0.5))
#Date - season/day
plot.season <- shelter %>%
  group_by(season, OutcomeType) %>%
  tally()
ggplot(data = plot.season, aes(x = season, y = n, fill = OutcomeType)) +
  geom_col()+
  labs(x = "Season", y = "Count", title = "Count of Animals by Seasons")+
  theme(text = element_text(size=20),
        plot.title = element_text(hjust = 0.5))
ggplot(data = shelter, aes(x = day)) +
  geom_density(alpha = .4, aes(fill = OutcomeType))+
  labs(x = "Days in month", y = "Density", title = "Density upon Days in Month")+
  theme(text = element_text(size=20),
        plot.title = element_text(hjust = 0.5))

#Modeling
library(caret)
#Build a model without objective factors (name&date)
set.seed(1930)
shelter.glm.cv01 <- train(OutcomeType ~ AnimalType + Sterility + Color + Age_ctg, data = shelter,
                         method = "glm", family = "binomial",
                         trControl = trainControl(method = "repeatedcv",
                                                  repeats = 5, number = 10,
                                                  savePredictions = T))
shelter.glm.cv01$results
#Accuracy: 0.7993
summary(shelter.glm.cv01)
#Build another model include objective factors as variables
shelter.glm.cv02 <- train(OutcomeType ~ AnimalType + if_name + Sterility + Color + day + season + Age_ctg, data = shelter,
                         method = "glm", family = "binomial",
                         trControl = trainControl(method = "repeatedcv",
                                                  repeats = 5, number = 10,
                                                  savePredictions = T))
shelter.glm.cv02$results
#Accuracy: 0.8071
summary(shelter.glm.cv02)
#Distribution of accuracy
ggplot(data = shelter.glm.cv02$resample, aes(x = Accuracy)) +
  geom_density(alpha = .2, fill="orange")+
  labs(title = "Distribution of Accuracy")+
  theme(text = element_text(size=20),
        plot.title = element_text(hjust = 0.5))
#ROC curve
library(pROC)
shelter.glm.prob <- predict(shelter.glm.cv02, type = "prob")
roc(response = factor(shelter$OutcomeType),
    predictor = shelter.glm.prob$Positive, plot = T)

#Test of Multicollinearity
library(car)
vif(shelter.glm.cv02$finalModel)
