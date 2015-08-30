rm(list=ls(all=TRUE))

setwd("/Users/tylerfrazier/workspace/work_life/WM/Teaching/COLL_100/labs/lab1_whyWM")

data <- read.csv("lab1.csv")

names(data)

data_names <- c("id", "name", "year", "tuition10_11", 
"tuition11_12", "tuition12_13", "tuition13_14", "state", 
"core_exp", "instructional_exp", "research_exp", 
"publicservice_exp")
 
names(data) <- data_names

str(data)

table(data$state)

summary(data$tuition13_14)

va_data <- subset(data, state == "Virginia")
summary(va_data$tuition13_14)

us_mean <- mean(data$tuition13_14, na.rm = TRUE)
va_above_avg <- subset(va_data, tuition13_14 > us_mean)

subset(data, name == "College of William and Mary")

min(data$tuition13_14, na.rm = TRUE)
min_cost <- min(data$tuition13_14, na.rm = TRUE)
min_obs <- subset(data, tuition13_14 == min_cost)
min_obs$name
class(min_obs$name)
min_name <- as.character(min_obs$name)
min_name
class(min_name)


data[which.min(data$tuition13_14),]
data[which.max(data$tuition13_14),]

data[which.min(data$tuition13_14),]$name
data[which.max(data$tuition13_14),]$name


wm_tuition1314 <- data[which(data$name == "College of William and Mary"), ]$tuition13_14
wm_tuition1213 <- data[which(data$name == "College of William and Mary"), ]$tuition12_13
wm_tuition1112 <- data[which(data$name == "College of William and Mary"), ]$tuition11_12
wm_tuition1011 <- data[which(data$name == "College of William and Mary"), ]$tuition10_11

wm_change14 <- wm_tuition1314 / wm_tuition1213
wm_change13 <- wm_tuition1213 / wm_tuition1112
wm_change12 <- wm_tuition1112 / wm_tuition1011

wm_change <- mean(c(wm_change14, wm_change13, wm_change12))

wm_change




data$us_change14 <- data$tuition13_14 / data$tuition12_13
data$us_change13 <- data$tuition12_13 / data$tuition11_12
data$us_change12 <- data$tuition11_12 / data$tuition10_11

data$us_change <- (data$us_change14 + data$us_change13 + data$us_change12) / 3

data$us_change

mean(data$us_change, na.rm = TRUE)

summary(data$us_change)

wm <- subset(data, name == "College of William and Mary")
wm$instructional_exp

summary(data$instructional_exp)


plot(data$tuition13_14, data$instructional_exp)

plot(va_data$instructional_exp, va_data$tuition13_14)

wm_plot <- data[which(data$name=="College of William and Mary"),]
points(wm_plot$instructional_exp, wm_plot$tuition13_14, col="green")
