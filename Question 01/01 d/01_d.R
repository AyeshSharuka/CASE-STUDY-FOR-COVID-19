#UK
uk_death <- read.table("uk_Deaths_lexis.txt",head=TRUE)
#japan
japan_death <- read.table("Japan_Deaths_lexis.txt",head=TRUE)
#new_Zealand
newZealand_death <- read.table("New Zealand_Deaths_lexis.txt",head=TRUE)
#sweden
sweden_death <- read.table("Sweden_Deaths_lexis.txt",head=TRUE)
#usa
usa_death <- read.table("USA_Deaths_lexis.txt",head=TRUE)

uk_n <- uk_death[c("Year","Total")]
japan_n <- japan_death[c("Year","Total")]
newZealand_n <- newZealand_death[c("Year","Total")]
sweden_n <- sweden_death[c("Year","Total")]
usa_n <- usa_death[c("Year","Total")]

uk_f<-uk_n %>% 
  group_by(Year) %>%
  summarise(Total = sum(Total))
japan_f<-japan_n %>% 
  group_by(Year) %>%
  summarise(Total = sum(Total))
newZealand_f<-newZealand_n %>% 
  group_by(Year) %>%
  summarise(Total = sum(Total))
sweden_f<-sweden_n %>% 
  group_by(Year) %>%
  summarise(Total = sum(Total))
usa_f<-usa_n %>% 
  group_by(Year) %>%
  summarise(Total = sum(Total))

#uk
A<- list()
for (i in 2001:2020){
  avarage <- (uk_f$Total[which(uk_f$Year==i-5)]+uk_f$Total[which(uk_f$Year==i-4)]+uk_f$Total[which(uk_f$Year==i-3)]+uk_f$Total[which(uk_f$Year==i-2)]+uk_f$Total[which(uk_f$Year==i-1)]
  )/5
  excess <- uk_f$Total[which(uk_f$Year==i)]-avarage
  A <- c(A, excess)
  print(uk_f$Total[which(uk_f$Year==i)])
}
uk_my_data <- as.data.frame.table(do.call(cbind,A))
colnames(uk_my_data)[colnames(uk_my_data)=="Freq"] <- "Excess_Deaths"
uk <- uk_my_data[c("Excess_Deaths")]
uk['Year'] <- c(2001:2020)

#japan
A<- list()
for (i in 2001:2020){
  avarage <- (japan_f$Total[which(japan_f$Year==i-5)]+japan_f$Total[which(japan_f$Year==i-4)]+japan_f$Total[which(japan_f$Year==i-3)]+japan_f$Total[which(japan_f$Year==i-2)]+japan_f$Total[which(japan_f$Year==i-1)]
  )/5
  excess <- japan_f$Total[which(japan_f$Year==i)]-avarage
  A <- c(A, excess)
  print(japan_f$Total[which(japan_f$Year==i)])
}
japan_my_data <- as.data.frame.table(do.call(cbind,A))
colnames(japan_my_data)[colnames(japan_my_data)=="Freq"] <- "Excess_Deaths"
japan <- japan_my_data[c("Excess_Deaths")]
japan['Year'] <- c(2001:2020)

#newZealand
A<- list()
for (i in 2001:2021){
  avarage <- (newZealand_f$Total[which(newZealand_f$Year==i-5)]+newZealand_f$Total[which(newZealand_f$Year==i-4)]+newZealand_f$Total[which(newZealand_f$Year==i-3)]+newZealand_f$Total[which(newZealand_f$Year==i-2)]+newZealand_f$Total[which(newZealand_f$Year==i-1)]
  )/5
  excess <- newZealand_f$Total[which(newZealand_f$Year==i)]-avarage
  A <- c(A, excess)
  print(newZealand_f$Total[which(newZealand_f$Year==i)])
}
newZealand_my_data <- as.data.frame.table(do.call(cbind,A))
colnames(newZealand_my_data)[colnames(newZealand_my_data)=="Freq"] <- "Excess_Deaths"
newZealand <- newZealand_my_data[c("Excess_Deaths")]
newZealand['Year'] <- c(2001:2021)

#sweden
A<- list()
for (i in 2001:2021){
  avarage <- (sweden_f$Total[which(sweden_f$Year==i-5)]+sweden_f$Total[which(sweden_f$Year==i-4)]+sweden_f$Total[which(sweden_f$Year==i-3)]+sweden_f$Total[which(sweden_f$Year==i-2)]+sweden_f$Total[which(sweden_f$Year==i-1)]
  )/5
  excess <- sweden_f$Total[which(sweden_f$Year==i)]-avarage
  A <- c(A, excess)
  print(sweden_f$Total[which(sweden_f$Year==i)])
}
sweden_my_data <- as.data.frame.table(do.call(cbind,A))
colnames(sweden_my_data)[colnames(sweden_my_data)=="Freq"] <- "Excess_Deaths"
sweden <- sweden_my_data[c("Excess_Deaths")]
sweden['Year'] <- c(2001:2021)

#usa
A<- list()
for (i in 2001:2020){
  avarage <- (usa_f$Total[which(usa_f$Year==i-5)]+usa_f$Total[which(usa_f$Year==i-4)]+usa_f$Total[which(usa_f$Year==i-3)]+usa_f$Total[which(usa_f$Year==i-2)]+usa_f$Total[which(usa_f$Year==i-1)]
  )/5
  excess <- usa_f$Total[which(usa_f$Year==i)]-avarage
  A <- c(A, excess)
  print(usa_f$Total[which(usa_f$Year==i)])
}
usa_my_data <- as.data.frame.table(do.call(cbind,A))
colnames(usa_my_data)[colnames(usa_my_data)=="Freq"] <- "Excess_Deaths"
usa <- usa_my_data[c("Excess_Deaths")]
usa['Year'] <- c(2001:2020)


uk$Excess_Deaths <-scale(uk$Excess_Deaths)
japan$Excess_Deaths <-scale(japan$Excess_Deaths)
newZealand$Excess_Deaths <-scale(newZealand$Excess_Deaths)
sweden$Excess_Deaths <-scale(sweden$Excess_Deaths)
usa$Excess_Deaths <-scale(usa$Excess_Deaths)

plot(uk$Year,uk$Excess_Deaths, type = "l",col="red",title("UK"))
plot(japan$Year,japan$Excess_Deaths, type = "l",col="red",title("JAPAN"))
plot(newZealand$Year,newZealand$Excess_Deaths, type = "l",col="red",title("NEW ZEALAND"))
plot(sweden$Year,sweden$Excess_Deaths, type = "l",col="red",title("SWEDEN"))
plot(usa$Year,usa$Excess_Deaths, type = "l",col="red",title("USA"))

