all_data <- read.csv("mocy.csv")
my_data<- all_data[c("sex","year","week","age_start","age_width","country_name","personweeks")]


Italy <- my_data[c(263121:275080), ] #263121:275080
female_italy<-my_data[c(263121:269100), ] #263121:269100

female_italy$age_start <- cut(female_italy$age_start, 
               breaks = c(-Inf
                          ,15,65,75,85
                          , Inf), 
               labels = c("0-14","15-64","65-74","75-84","85+"),
               right = FALSE)
            
n1 <- female_italy[female_italy$year !="2000",]
n2 <- n1[n1$year !="2001",]
n3 <- n2[n2$year !="2002",]
n4 <- n3[n3$year !="2003",]
n5 <- n4[n4$year !="2004",]
n6 <- n5[n5$year !="2005",]
n7 <- n6[n6$year !="2006",]
n8 <- n7[n7$year !="2007",]
n9 <- n8[n8$year !="2008",]
n10 <- n9[n9$year !="2009",]
n11 <- n10[n10$year !="2010",]
n12 <- n11[n11$year !="2011",]
n13 <- n12[n12$year !="2012",]
n14 <- n13[n13$year !="2013",]
female_final <- n14[n14$year !="2014",]
colnames(female_final)[colnames(female_final)=="age_start"] <- "age_group"

new_female_final<-female_final %>% 
  group_by(year,age_group) %>%
  summarise(Total = sum(personweeks))

library(ggplot2)
ggplot(new_female_final, aes(x = year, y = Total)) +
  geom_point() +
  facet_wrap(~ age_group)+
  labs(title = "Italy")



