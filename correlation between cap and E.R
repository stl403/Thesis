#Gode pakker fra Mads
library(magrittr) # pipes
library(dplyr) # filters and more
library(ggplot2) # plot
library(readxl) #read excel file
library(tidyr) #nest
library(purrr) #map 
library(openxlsx)
library("RColorBrewer")
library(lmtest)
library(sandwich)

setwd("C:/Users/leneb/Desktop/Speciale/Data")

#Start by reading all files manually 
SPXMEMBERS <- read_excel("SPXMEMBERS.xlsx")
Dates <- read_excel("Clean/Dates.xlsx", col_names = FALSE)
dates_0 <- as.vector(t(Dates))

################ USED TO GET DATA #######################
#bob <- unique(na.omit(as.vector(as.matrix(SPXMEMBERS))))
#write.xlsx(bob, file = "unikkeVirk.xlsx")


#####################################


########## PF ##################

ngroups <- 5
ndates <- length(dates_0)

###############Set up################
#Load data, general setup 
Return <- as_tibble(read_excel("Clean/Return.xlsx", col_names = FALSE)) #I clean mappen, har jeg slettet første dato fro return, saa det matcher med de andre
E <-  as_tibble(read_excel("Clean/E.xlsx", col_names = FALSE))
Cap <-  as_tibble(read_excel("Clean/Cap.xlsx", col_names = FALSE))

colnames(E) <- c("Ticker", as.character(dates_0))
colnames(Return) <- c("Ticker", as.character(dates_0))
colnames(Cap) <- c("Ticker", as.character(dates_0))



Cap
E

Cap1 <- Cap %>% 
  gather(key = "Date", value = "value", -Ticker) %>% 
  filter(value != '#N/A N/A') %>% 
  mutate(value = as.numeric(value))


E1 <- E %>% 
  gather(key = "Date", value = "escore", -Ticker) %>% 
  filter(escore != '#N/A N/A') %>% 
  mutate(escore = as.numeric(escore))

samlet1 <- Cap1 %>% left_join(E1, by = c("Ticker", "Date")) %>% na.omit()

samlet <- samlet %>% 
  #sample_n(40000) %>% 
  ggplot(aes(x = log(value), y = escore)) +
  geom_point(alpha = 1, size = 0.1) +
  geom_smooth(method = "lm")+
  xlab("log(market capitalization)") + ylab("E score") +
  theme_classic()



png(filename="C:/Users/leneb/Desktop/Speciale/Data/New_pictures/correlation_E_Cap.png",
    res=500, units="in", width = 7.5, height = 3.5)
samlet
dev.off()





