uk_death <- read.table("japan_Deaths.txt",head=TRUE)
japan_death <- read.table("newZealand_Deaths.txt",head=TRUE)
newZealand_death <- read.table("sweden_Deaths.txt",head=TRUE)
sweden_death <- read.table("uk_Deaths.txt",head=TRUE)
usa_death <- read.table("usa_Deaths.txt",head=TRUE)

uk_death01 <- uk_death[c("Year","Age","Male")]
japan_death01 <- japan_death[c("Year","Age","Male")]
newZealand_death01 <- newZealand_death[c("Year","Age","Male")]
sweden_death01 <- sweden_death[c("Year","Age","Male")]
usa_death01 <- usa_death[c("Year","Age","Male")]

uk_new_df<-uk_death01[c(5995:8214), ]
japan_new_df<-japan_death01[c(5884:8214), ]
newZealand_new_df<-newZealand_death01[c(27751:30081), ]
sweden_new_df<-sweden_death01[c(8770:10989), ]
usa_new_df<-usa_death01[c(7549:9768), ]
 
uk1 <- uk_new_df
uk2 <- uk1[!(uk1$Age=="110+"), ]
uk2$Age <- as.numeric(as.character(uk2$Age))
uk2$Age <- cut(uk2$Age, 
               breaks = c(-Inf
                          ,5 ,10 ,15,20,25,30,35,40,45,50,55,60 ,65,70,75,80,85
                          , Inf), 
               labels = c("0-4 years"
                         ,"5-9 years","10-14 years","15-19 years","20-24 years"
                         ,"25-29 years","30-34 years","35-39 years","40-44 years"
                         ,"45-49 years","50-54 years","55-59 years","60-64 years"
                         ,"65-69 years","70-74 years","75-79 years","80-84 years"
                          ,"85+ years"),
               right = FALSE)
japan1 <- japan_new_df
japan2 <- japan1[!(japan1$Age=="110+"), ]
japan2$Age <- as.numeric(as.character(japan2$Age))
japan2$Age <- cut(japan2$Age, 
               breaks = c(-Inf
                          ,5 ,10 ,15,20,25,30,35,40,45,50,55,60 ,65,70,75,80,85
                          , Inf), 
               labels = c("0-4 years"
                          ,"5-9 years","10-14 years","15-19 years","20-24 years"
                          ,"25-29 years","30-34 years","35-39 years","40-44 years"
                          ,"45-49 years","50-54 years","55-59 years","60-64 years"
                          ,"65-69 years","70-74 years","75-79 years","80-84 years"
                          ,"85+ years"),
               right = FALSE)
newZealand1 <- newZealand_new_df
newZealand2 <- newZealand1[!(newZealand1$Age=="110+"), ]
newZealand2$Age <- as.numeric(as.character(newZealand2$Age))
newZealand2$Age <- cut(newZealand2$Age, 
               breaks = c(-Inf
                          ,5 ,10 ,15,20,25,30,35,40,45,50,55,60 ,65,70,75,80,85
                          , Inf), 
               labels = c("0-4 years"
                          ,"5-9 years","10-14 years","15-19 years","20-24 years"
                          ,"25-29 years","30-34 years","35-39 years","40-44 years"
                          ,"45-49 years","50-54 years","55-59 years","60-64 years"
                          ,"65-69 years","70-74 years","75-79 years","80-84 years"
                          ,"85+ years"),
               right = FALSE)
sweden1 <- sweden_new_df
sweden2 <- sweden1[!(sweden1$Age=="110+"), ]
sweden2$Age <- as.numeric(as.character(sweden2$Age))
sweden2$Age <- cut(sweden2$Age, 
               breaks = c(-Inf
                          ,5 ,10 ,15,20,25,30,35,40,45,50,55,60 ,65,70,75,80,85
                          , Inf), 
               labels = c("0-4 years"
                          ,"5-9 years","10-14 years","15-19 years","20-24 years"
                          ,"25-29 years","30-34 years","35-39 years","40-44 years"
                          ,"45-49 years","50-54 years","55-59 years","60-64 years"
                          ,"65-69 years","70-74 years","75-79 years","80-84 years"
                          ,"85+ years"),
               right = FALSE)
usa1 <- usa_new_df
usa2 <- usa1[!(usa1$Age=="110+"), ]
usa2$Age <- as.numeric(as.character(usa2$Age))
usa2$Age <- cut(usa2$Age, 
               breaks = c(-Inf
                          ,5 ,10 ,15,20,25,30,35,40,45,50,55,60 ,65,70,75,80,85
                          , Inf), 
               labels = c("0-4 years"
                          ,"5-9 years","10-14 years","15-19 years","20-24 years"
                          ,"25-29 years","30-34 years","35-39 years","40-44 years"
                          ,"45-49 years","50-54 years","55-59 years","60-64 years"
                          ,"65-69 years","70-74 years","75-79 years","80-84 years"
                          ,"85+ years"),
               right = FALSE)

usa_n <- usa2[usa2$Age !="5-9 years"
              & usa2$Age !="10-14 years"
              & usa2$Age !="15-19 years"
              & usa2$Age !="15-19 years"
              & usa2$Age !="25-29 years"
              & usa2$Age !="30-34 years"
              & usa2$Age !="35-39 years"
              & usa2$Age !="45-49 years"
              & usa2$Age !="50-54 years"
              & usa2$Age !="55-59 years"
              & usa2$Age !="65-69 years"
              & usa2$Age !="70-74 years"
              & usa2$Age !="75-79 years"
              & usa2$Age !="80-84 years"
              & usa2$Age !="85+ years",]
uk_n <- uk2[uk2$Age !="5-9 years"
              & uk2$Age !="10-14 years"
              & uk2$Age !="15-19 years"
              & uk2$Age !="15-19 years"
              & uk2$Age !="25-29 years"
              & uk2$Age !="30-34 years"
              & uk2$Age !="35-39 years"
              & uk2$Age !="45-49 years"
              & uk2$Age !="50-54 years"
              & uk2$Age !="55-59 years"
              & uk2$Age !="65-69 years"
              & uk2$Age !="70-74 years"
              & uk2$Age !="75-79 years"
              & uk2$Age !="80-84 years"
              & uk2$Age !="85+ years",]
japan_n <- japan2[usa2$Age !="5-9 years"
                  & japan2$Age !="10-14 years"
                  & japan2$Age !="15-19 years"
                  & japan2$Age !="15-19 years"
                  & japan2$Age !="25-29 years"
                  & japan2$Age !="30-34 years"
                  & japan2$Age !="35-39 years"
                  & japan2$Age !="45-49 years"
                  & japan2$Age !="50-54 years"
                  & japan2$Age !="55-59 years"
                  & japan2$Age !="65-69 years"
                  & japan2$Age !="70-74 years"
                  & japan2$Age !="75-79 years"
                  & japan2$Age !="80-84 years"
                  & japan2$Age !="85+ years",]

sweden_n <- sweden2[sweden2$Age !="5-9 years"
                  & sweden2$Age !="10-14 years"
                  & sweden2$Age !="15-19 years"
                  & sweden2$Age !="15-19 years"
                  & sweden2$Age !="25-29 years"
                  & sweden2$Age !="30-34 years"
                  & sweden2$Age !="35-39 years"
                  & sweden2$Age !="45-49 years"
                  & sweden2$Age !="50-54 years"
                  & sweden2$Age !="55-59 years"
                  & sweden2$Age !="65-69 years"
                  & sweden2$Age !="70-74 years"
                  & sweden2$Age !="75-79 years"
                  & sweden2$Age !="80-84 years"
                  & sweden2$Age !="85+ years",]

newZealand_n <- newZealand2[newZealand2$Age !="5-9 years"
                  & newZealand2$Age !="10-14 years"
                  & newZealand2$Age !="15-19 years"
                  & newZealand2$Age !="15-19 years"
                  & newZealand2$Age !="25-29 years"
                  & newZealand2$Age !="30-34 years"
                  & newZealand2$Age !="35-39 years"
                  & newZealand2$Age !="45-49 years"
                  & newZealand2$Age !="50-54 years"
                  & newZealand2$Age !="55-59 years"
                  & newZealand2$Age !="65-69 years"
                  & newZealand2$Age !="70-74 years"
                  & newZealand2$Age !="75-79 years"
                  & newZealand2$Age !="80-84 years"
                  & newZealand2$Age !="85+ years",]


uk_f<-uk_n %>% 
  group_by(Year,Age) %>%
  summarise(Male = sum(Male))
usa_f<- usa_n %>% 
  group_by(Year,Age) %>%
  summarise(Male = sum(Male))
japan_f<-japan_n %>% 
  group_by(Year,Age) %>%
  summarise(Male = sum(Male))
newZealand_f<-newZealand_n %>% 
  group_by(Year,Age) %>%
  summarise(Male = sum(Male))
sweden_f<-sweden_n %>% 
  group_by(Year,Age) %>%
  summarise(Male = sum(Male))

ggplot(uk_f, aes(x = Year, y = Male)) +
  geom_point() +
  facet_wrap(~ Age)+
  labs(title = "UK")
ggplot(usa_f, aes(x = Year, y = Male)) +
  geom_point() +
  facet_wrap(~ Age)+
  labs(title = "USA")
ggplot(japan_f, aes(x = Year, y = Male)) +
  geom_point() +
  facet_wrap(~ Age)+
  labs(title = "JAPAN")
ggplot(newZealand_f, aes(x = Year, y = Male)) +
  geom_point() +
  facet_wrap(~ Age)+
  labs(title = "NEW ZEALAND")
ggplot(sweden_f, aes(x = Year, y = Male)) +
  geom_point() +
  facet_wrap(~ Age)+
  labs(title = "SWEDEN")



