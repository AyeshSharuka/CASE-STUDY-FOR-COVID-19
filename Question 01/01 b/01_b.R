#UK
uk_population <- read.table("uk_Population5.txt",head=TRUE)
uk_death <- read.table("uk_Deaths_lexis.txt",head=TRUE)
#japan
japan_population <- read.table("Japan_Population5.txt",head=TRUE)
japan_death <- read.table("Japan_Deaths_lexis.txt",head=TRUE)
#new_Zealand
newZealand_population <- read.table("New Zealand_Population5.txt",head=TRUE)
newZealand_death <- read.table("New Zealand_Deaths_lexis.txt",head=TRUE)
#sweden
sweden_population <- read.table("Sweden_Population5.txt",head=TRUE)
sweden_death <- read.table("Sweden_Deaths_lexis.txt",head=TRUE)
#usa
usa_population <- read.table("USA_Population5.txt",head=TRUE)
usa_death <- read.table("USA_Deaths_lexis.txt",head=TRUE)

uk_population01 <- uk_population[c("Year","Total")]
japan_population01 <- japan_population[c("Year","Total")]
newZealand_population01 <- newZealand_population[c("Year","Total")]
sweden_population01 <- sweden_population[c("Year","Total")]
usa_population01 <- usa_population[c("Year","Total")]

uk_death01 <- uk_death[c("Year","Total")]
japan_death01 <- japan_death[c("Year","Total")]
newZealand_death01 <- newZealand_death[c("Year","Total")]
sweden_death01 <- sweden_death[c("Year","Total")]
usa_death01 <- usa_death[c("Year","Total")]

colnames(uk_population01)[colnames(uk_population01)=="Total"] <- "Total_Population"
colnames(uk_death01)[colnames(uk_death01)=="Total"] <- "Total_death"
colnames(japan_population01)[colnames(japan_population01)=="Total"] <- "Total_Population"
colnames(japan_death01)[colnames(japan_death01)=="Total"] <- "Total_death"
colnames(newZealand_population01)[colnames(newZealand_population01)=="Total"] <- "Total_Population"
colnames(newZealand_death01)[colnames(newZealand_death01)=="Total"] <- "Total_death"
colnames(sweden_population01)[colnames(sweden_population01)=="Total"] <- "Total_Population"
colnames(sweden_death01)[colnames(sweden_death01)=="Total"] <- "Total_death"
colnames(usa_population01)[colnames(usa_population01)=="Total"] <- "Total_Population"
colnames(usa_death01)[colnames(usa_death01)=="Total"] <- "Total_death"

uk_merged_pop_id <- merge(uk_population01,uk_death01, by="Year")
japan_merged_pop_id <- merge(japan_population01,japan_death01, by="Year")
newZealand_merged_pop_id <- merge(newZealand_population01,newZealand_death01, by="Year")
sweden_merged_pop_id <- merge(sweden_population01,sweden_death01, by="Year")
usa_merged_pop_id <- merge(usa_population01,usa_death01, by="Year")

uk_new_df<-uk_merged_pop_id[c(419330:525096), ]
japan_new_df<-japan_merged_pop_id[c(281113:387192), ]
newZealand_new_df<-newZealand_merged_pop_id[c(275809:387192), ]
sweden_new_df<-sweden_merged_pop_id[c(1326001:1437384), ]
usa_new_df<-usa_merged_pop_id[c(355369:461448), ]



#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
#library(magrittr) # needs to be run every time you start R and want to use %>%
#library(dplyr)    # alternatively, this also loads %>%

uk_pdf<-uk_new_df %>% 
  group_by(Year) %>% 
  summarise(Total_Population = sum(Total_Population))
uk_ddf<-uk_new_df %>% 
  group_by(Year) %>% 
  summarise(Total_death = sum(Total_death))

japan_pdf<-japan_new_df %>% 
  group_by(Year) %>% 
  summarise(Total_Population = sum(Total_Population))
japan_ddf<-japan_new_df %>% 
  group_by(Year) %>% 
  summarise(Total_death = sum(Total_death))

newZealand_pdf<-newZealand_new_df %>% 
  group_by(Year) %>% 
  summarise(Total_Population = sum(Total_Population))
newZealand_ddf<-newZealand_new_df %>% 
  group_by(Year) %>% 
  summarise(Total_death = sum(Total_death))

sweden_pdf<-sweden_new_df %>% 
  group_by(Year) %>% 
  summarise(Total_Population = sum(Total_Population))
sweden_ddf<-sweden_new_df %>% 
  group_by(Year) %>% 
  summarise(Total_death = sum(Total_death))

usa_pdf<-usa_new_df %>% 
  group_by(Year) %>% 
  summarise(Total_Population = sum(Total_Population))
usa_ddf<-usa_new_df %>% 
  group_by(Year) %>% 
  summarise(Total_death = sum(Total_death))

uk_merged_pop_id_year <- merge(uk_pdf,uk_ddf, by="Year")
japan_merged_pop_id_year <- merge(japan_pdf,japan_ddf, by="Year")
newZealand_merged_pop_id_year <- merge(newZealand_pdf,newZealand_ddf, by="Year")
sweden_merged_pop_id_year <- merge(sweden_pdf,sweden_ddf, by="Year")
usa_merged_pop_id_year <- merge(usa_pdf,usa_ddf, by="Year")

uk_merged_pop_id_year['Mortality'] <- uk_merged_pop_id_year["Total_death"]/uk_merged_pop_id_year["Total_Population"]
japan_merged_pop_id_year['Mortality'] <- japan_merged_pop_id_year["Total_death"]/japan_merged_pop_id_year["Total_Population"]
newZealand_merged_pop_id_year['Mortality'] <- newZealand_merged_pop_id_year["Total_death"]/newZealand_merged_pop_id_year["Total_Population"]
sweden_merged_pop_id_year['Mortality'] <- sweden_merged_pop_id_year["Total_death"]/sweden_merged_pop_id_year["Total_Population"]
usa_merged_pop_id_year['Mortality'] <- usa_merged_pop_id_year["Total_death"]/usa_merged_pop_id_year["Total_Population"]

plot(uk_merged_pop_id_year$Year,uk_merged_pop_id_year$Total_Population, type = "l",col="red")
plot(japan_merged_pop_id_year$Year,japan_merged_pop_id_year$Total_Population, type = "l",col="red")
plot(newZealand_merged_pop_id_year$Year,newZealand_merged_pop_id_year$Total_Population, type = "l",col="red")
plot(sweden_merged_pop_id_year$Year,sweden_merged_pop_id_year$Total_Population, type = "l",col="red")
plot(usa_merged_pop_id_year$Year,usa_merged_pop_id_year$Total_Population, type = "l",col="red")

plot(uk_merged_pop_id_year$Year,uk_merged_pop_id_year$Total_death, type = "l",col="blue")
plot(japan_merged_pop_id_year$Year,japan_merged_pop_id_year$Total_death, type = "l",col="blue")
plot(newZealand_merged_pop_id_year$Year,newZealand_merged_pop_id_year$Total_death, type = "l",col="blue")
plot(sweden_merged_pop_id_year$Year,sweden_merged_pop_id_year$Total_death, type = "l",col="blue")
plot(usa_merged_pop_id_year$Year,usa_merged_pop_id_year$Total_death, type = "l",col="blue")

plot(uk_merged_pop_id_year$Year,uk_merged_pop_id_year$Mortality, type = "l",col="green")
plot(japan_merged_pop_id_year$Year,japan_merged_pop_id_year$Mortality, type = "l",col="green")
plot(newZealand_merged_pop_id_year$Year,newZealand_merged_pop_id_year$Mortality, type = "l",col="green")
plot(sweden_merged_pop_id_year$Year,sweden_merged_pop_id_year$Mortality, type = "l",col="green")
plot(usa_merged_pop_id_year$Year,usa_merged_pop_id_year$Mortality, type = "l",col="green")





