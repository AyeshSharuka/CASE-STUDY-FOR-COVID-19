#Question2(b)

all_data <- read.csv("mocy.csv")
my_data <- all_data[c("sex","year","week","age_start","age_width","country_name","deaths","personweeks")]

library(dplyr)

Italy <- filter(my_data, country_name == 'Italy')
Italy_sex01 <- filter(Italy, year>2010)
Italy_sex_2 <- filter(Italy_sex01, year<2021)

Italy_sex_2$age_start <- cut(Italy_sex_2$age_start, 
                             breaks = c(-Inf
                                        ,15,65,75,85
                                        , Inf), 
                             labels = c("0-14","15-64","65-74","75-84","85+"),
                             right = FALSE)

i <- Italy_sex_2 %>% #Group observations
  group_by(year,week,sex,age_start,personweeks) %>%
  summarise(deaths = sum(deaths))

Italy_observed <- i[c(1:4770), ]#1:4770
Italy_predicted <- i[c(4771:5200), ]#4771:5200

library(ggplot2)
ggplot(Italy_observed, aes(x = week, y = deaths)) +
  geom_point() +
  labs(title = "Observed deaths")

#ggplot(Italy_predicted, aes(x = week, y = deaths)) +
#  geom_point() +
#  labs(title = "Predict deaths")

#library(mgcv)
#M3 <- gam(Italy_observed$deaths ~ s(Italy_observed$week))
#plot(M3, se=TRUE)

#M4 <- gam(Italy_predicted$deaths ~ s(Italy_predicted$week))
#plot(M4, se=TRUE)

#Observed <- summary(M3)
#Predicted <- summary(M4)

#library(MASS)
#library(magrittr)

#mean <- mean(Italy_observed$deaths)%>%round(4)
#var <- var(Italy_observed$deaths)%>%round(4)
#x <- (var/mean)%>%round(4)
#my_model <- glm.nb(week ~ deaths, data = Italy_observed)
#summary(my_model) 

#install.packages("mgcv")
#library(mgcv)

library(mgcv)
gam_mod <- gam(deaths ~ s(week, k=4), data = Italy_observed, family = nb, method = "REML")
summary(gam_mod)
gam.check(gam_mod, rep = 500)
plot.gam(gam_mod)

#Extract expected deaths up to week 51 of 2021 
predict_values <- predict.gam(gam_mod, Italy_predicted)
li <- list(print(predict_values))
li <- list()
li <- c(li, predict_values)
preducted_values_using_GAM <- as.data.frame(do.call(rbind,li))
Italy_predicted['Obs_num'] <- c(1:430)
preducted_values_using_GAM['Obs_num'] <- c(1:430)

Final_df <- merge(Italy_predicted,preducted_values_using_GAM, by="Obs_num")
colnames(Final_df)[colnames(Final_df)=="V1"] <- "Predicted_val"

#define proportion
p <- 0.00523
#define significance level
a <- .05
#calculate 95% prediction interval
p + c(-qnorm(1-a/2), qnorm(1-a/2))*sqrt((1/430)*p*(1-p))

Italy_plot_01 <- filter(Italy, year>2014)
Italy_plot_02 <- filter(Italy_plot_01, year<2021)

Italy_plot_02$age_start <- cut(Italy_plot_02$age_start, 
                             breaks = c(-Inf
                                        ,15,65,75,85
                                        , Inf), 
                             labels = c("0-14","15-64","65-74","75-84","85+"),
                             right = FALSE)

colnames(Italy_plot_02)[colnames(Italy_plot_02)=="age_start"] <- "Age_group"

Italy_plot_female <- filter(Italy_plot_02, sex=="Female")
Italy_plot_male <- filter(Italy_plot_02, sex=="Male")

library(ggplot2)
ggplot(Italy_plot_female, aes(x = week, y = deaths)) +
  geom_point() +
  facet_wrap(~ Age_group)+
  labs(title = "Female")

ggplot(Italy_plot_male, aes(x = week, y = deaths)) +
  geom_point() +
  facet_wrap(~ Age_group)+
  labs(title = "Male")

#Question2(C)

Final_df['Excess_deaths'] <- Final_df["deaths"]-Final_df["Predicted_val"]
cumulative_excess_deaths <- sum(Final_df$Excess_deaths)%>%round(4)

gam_mod_02 <- gam(Predicted_val ~ s(week, k=4), data = Final_df, family = nb, method = "REML")
summary(gam_mod_02)

#define proportion
p <- 0.00685
#define significance level
a <- .05
#calculate 95% prediction interval
p + c(-qnorm(1-a/2), qnorm(1-a/2))*sqrt((1/430)*p*(1-p))

female_Predicted_val <- filter(Final_df, sex=="Female")
colnames(female_Predicted_val)[colnames(female_Predicted_val)=="age_start"] <- "Age_group"
male_Predicted_val <- filter(Final_df, sex=="Male")
colnames(male_Predicted_val)[colnames(male_Predicted_val)=="age_start"] <- "Age_group"

library(ggplot2)
ggplot(female_Predicted_val, aes(x = week, y = Predicted_val)) +
  geom_point() +
  facet_wrap(~ Age_group)+
  labs(title = "Female")

ggplot(male_Predicted_val, aes(x = week, y = Predicted_val)) +
  geom_point() +
  facet_wrap(~ Age_group)+
  labs(title = "Male")

summary(male_Predicted_val)
summary(female_Predicted_val)
summary(Final_df)





