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
Bank_invest <- read_excel("Clean/Bank_invest_global.xlsx", col_names = TRUE)


Bank_invest_1 <- Bank_invest %>% as_tibble



Bank_invest_2 <- Bank_invest_1 %>%
  as_tibble %>%
  gather(key = "col", value = "penge", -Dates)

Bank_invest_3 <- Bank_invest_2 %>% mutate(year_month = substring(Dates, 1, 7))



Bank_invest_global <- ggplot(Bank_invest_3, aes(x = year_month, y = penge), col = col) + 
  #geom_point(aes(color = col))
  geom_line(aes(group = col, color = col)) +
  xlab("Month") + ylab("Investment") +
  theme_classic() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_x_discrete(breaks = unique(Bank_invest_3$year_month)[c(T, rep(F,5))])+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))





png(filename="C:/Users/leneb/Desktop/Speciale/Data/New_pictures/investmentstrategies_Bank_invest_global.png",
    res=500, units="in", width = 7.5, height = 3.5)
Bank_invest_global 
dev.off()
