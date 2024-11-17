setwd("D:/PRAK ANDAT")
getwd()

#read in your data 
library(readr)
data <- read_csv2("depression_anxiety_data.csv",
                  col_types = "ccccinnccccccncnncccccc")
names(data) <-make.names(names(data))
names(data)

#check the packaging
nrow(data)
ncol(data)
str(data)

#Look at the top and the bottom of your data
head(data[, c(11,13)])
tail(data[, c(11,13)])

#	Check your “n”s
head(table(data$depression_diagnosis))
head(table(data$gad_score))
library(dplyr)
data %>%
  filter(is.na(depression_diagnosis)) %>%
  select(depression_diagnosis, gad_score)
data_clean <- data %>%
  filter(!is.na(depression_diagnosis))
unique(data_clean$depression_diagnosis)

#Validate with at least one external data source
data_clean$depression_diagnosis <- tolower(data_clean$depression_diagnosis) == "true"
summary(data_clean$depression_diagnosis)
data_clean$gad_score <- as.numeric(data_clean$gad_score)
summary(data_clean$gad_score)
sd(data_clean$gad_score)
quantile(as.numeric(data_clean$gad_score, seq(0, 1, 0.1)))

#make a plot
data_clean$depression_diagnosis <- ifelse(data_clean$depression_diagnosis == TRUE, 1, 0)
par(las = 2, mar = c(10, 4, 2, 2), cex.axis = 0.8)
boxplot(gad_score ~ depression_diagnosis, data = data_clean, range = 0, 
        ylab = "GAD Score", xlab = "Depression Diagnosis (0 = No, 1 = Yes)")

#Try the easy solution first
data_clean%>%
  group_by(depression_diagnosis) %>%
  summarize(mean_gad = mean(gad_score, na.rm = TRUE),
            median_gad = median(gad_score, na.rm = TRUE))

#Model as expetation 
# Membuat model regresi logistik
data_clean$depression_diagnosis <- as.factor(data_clean$depression_diagnosis)
data_clean$gad_score <- as.numeric(data_clean$gad_score)
logistic_model <- glm(depression_diagnosis ~ gad_score, data = data_clean, family = binomial)
summary(logistic_model)

#Comparing Model Expectations to Reality
# Menampilkan plot
library(ggplot2)
histogram <- ggplot(data_clean, aes(x = gad_score, fill = factor(depression_diagnosis))) +
  geom_histogram(aes(y = after_stat(density)), 
                 position = "identity", 
                 binwidth = 1, 
                 alpha = 0.5, 
                 color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data_clean$gad_score, na.rm = TRUE), 
                            sd = sd(data_clean$gad_score, na.rm = TRUE)), 
                color = "red", linewidth = 1) +
  scale_fill_manual(values = c("0" = "blue", "1" = "green")) +
  labs(title = "Distribusi GAD Score dengan Kurva Normal dan Diagnosis Depresi",
       x = "GAD Score",
       y = "Kepadatan",
       fill = "Diagnosis Depresi") +
  theme_minimal()
print(histogram)

#Pendeteksian Normalitas 
library(nortest)
lillie.test(model.resid)
#Normalitas dengan plot
qqnorm(model.resid,main='Normal QQ plot')
qqline(model.resid)
