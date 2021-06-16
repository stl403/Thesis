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
SPXMEMBERS <- read_excel("SPXMEMBERS6m_bagud.xlsx")
Dates <- read_excel("Clean/Dates6m_bagud.xlsx", col_names = FALSE)
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
Return <- as_tibble(read_excel("Clean/Return6m_bagud.xlsx", col_names = FALSE)) #I clean mappen, har jeg slettet første dato fro return, saa det matcher med de andre
E <-  as_tibble(read_excel("Clean/E6m_bagud.xlsx", col_names = FALSE))
Cap <-  as_tibble(read_excel("Clean/Cap6m_bagud.xlsx", col_names = FALSE))

colnames(E) <- c("Ticker", as.character(dates_0))
colnames(Return) <- c("Ticker", as.character(dates_0))
colnames(Cap) <- c("Ticker", as.character(dates_0))

#Create dataframe for monthly returns 
monthly_return <- matrix(0, nrow =ngroups, ncol = ndates)
colnames(monthly_return) <- as.character(dates_0)
monthly_return<- as.data.frame(monthly_return)



#Create control dataframe
control_filtering_companies <- matrix(NA, nrow=4, ncol=ndates) #En raekke pr. information, der filtreres paa + en raekke til antal ved start
rownames(control_filtering_companies) <- c("Start", "E", "Return", "Cap")
colnames(control_filtering_companies) <- c(as.character(dates_0))



##############ASSUMPTIONS ABOUT THE DATA #####################
#E, Cap, Return runs from 01-03-2014 to 01-02-2021 
#I assume: E and market cap known at time 1 fits with return from time 2
#Since monthly return is the last months return (backward, not forward)
#E, Cap, Return er 85 lang, da tickers er den forste 


##########################The loop###################################

for (i in 1:(ndates)){
  list <- na.omit(as.vector(SPXMEMBERS[,i]))
  control_filtering_companies[1,i]<-nrow(list)
  
  E_temp <- E %>%
    dplyr::filter(E$Ticker %in% list[[1]])
  E_temp_fil <- E_temp %>%
    dplyr::filter(E_temp[,i+1] !="N/A")
    #dplyr::filter(E_temp[,i+1] !="#N/A N/A") #First column is Ticker name (+1)
  control_filtering_companies[2,i]<-nrow(E_temp)-nrow(E_temp_fil)
   
  Return_temp <- Return %>%
    dplyr::filter(Return$Ticker %in% E_temp_fil$Ticker)
  Return_temp_fil <- Return_temp %>%
    dplyr::filter(Return_temp[,i+1] !="#N/A N/A")
  Return_temp_fil <- Return_temp_fil %>% #first column is Ticker name (+1)
    dplyr::filter(Return_temp_fil[,i+1] !="#N/A boost::bad_any_cast: failed conversion using boost::any_cast") 
  control_filtering_companies[3,i]<-nrow(Return_temp)-nrow(Return_temp_fil)
  
  Cap_temp <- Cap %>%
    dplyr::filter(Cap$Ticker %in% Return_temp_fil$Ticker)
  Cap_temp_fil <- Cap_temp %>%
    dplyr::filter(Cap_temp[,i+1] !="#N/A N/A") #First column is Ticker name (+1)
  control_filtering_companies[4,i]<-nrow(Cap_temp)-nrow(Cap_temp_fil)
  
  E_temp_fil <- E_temp_fil %>% 
    dplyr::filter(E_temp_fil$Ticker %in% Cap_temp_fil$Ticker)
  Return_temp_fil <- Return_temp_fil %>% 
    dplyr::filter(Return_temp_fil$Ticker %in% Cap_temp_fil$Ticker)
  
  
  ##### Now ordering   
  
  #First column is ticker names, and return is backward calculated
  joined_temp <- cbind(E_temp_fil[,1],E_temp_fil[,i+1],Cap_temp_fil[,i+1],Return_temp_fil[,i+1])
  colnames(joined_temp) <- c("Ticker", "E", "Cap", "Return")
  
  joined_temp[,2]=as.numeric(as.character(joined_temp[,2]))  
  joined_temp[,3]=as.numeric(as.character(joined_temp[,3]))
  joined_temp[,4]=as.numeric(as.character(joined_temp[,4]))
  
  joined_temp <- joined_temp[order(joined_temp[,2]),] #Ordered after E-score, low score first
  
  #To make sure market cap is approximately the same in each pf
  #This is done to prevent small cap bias
  #just a way to divided the pf', instead number of firms for example
  joined_temp <- joined_temp %>%  #Creates a new variabel: cummulative sum 
    as_tibble() %>% 
    dplyr::mutate(Scap=ave(Cap, FUN=cumsum))
  
  frac <- (sum(joined_temp$Cap)/ngroups) #fraction for the next calculation
  q <- c(0, frac, frac*2, frac*3, frac*4, frac*5) #this is for 5 groups
      
  #For each i, I create returns and amounts for 10 portfolios 
  return_i <- vector()
  #amount_i <- vector()
  
    for (j in 1:ngroups){
        #Select a 10th share of the total market cap
        pf <- joined_temp %>%
          dplyr::filter(between(joined_temp$Scap, q[j], q[j+1])) #approx same market cap in each
        
        #Calculate the return
        return_i[j] <- sum(pf$Return*pf$Cap)/sum(pf$Cap) #OBS:Market cap -1 er taget med ind ved konstruktion af joined_temp, som pf er lavet af
        #return_day[j] <- sum(pf$Cap)
    }
  
  monthly_return[,i] <- return_i 
  #For 2020-12-01 betyder monthly return, at det er det maanedlige afkast paa
  #den investering, som du sammensatte d.1 december 2020. Dvs. 1 december beslutter du dig
  #for at investerer så meget i de til gengængelige virksomheder og den naeste maanedstid 
  #indkasserer du saa det overskud, der staar ved 2020-12-01, selvom det faktisk forst er penge, 
  #som er tjent d. 1 januar 2021.
  print(i)
}


######################## Use of returns##############################
#Omregner fra procent til decimal, vigtig for de næste udregninger giver mening
monthly_return <- monthly_return / 100


lene2 <- cbind(1:84, t(monthly_return))
colnames(lene2) <- c("index", "pf1", "pf2","pf3", "pf4", "pf5")

lene3 <- lene2 %>%
  as_tibble %>%
  gather(key = "col", value = "penge", -index)


ggplot(lene3, aes(x = index, y = penge), col=col) + 
  #geom_point(aes(color=lene3$col)) +
  geom_line(aes(color = lene3$col)) + 
  scale_color_brewer(palette = "RdYlGn")+
  scale_fill_brewer(palette = "RdYlGn") + 
  xlab("Month") + ylab("Return") +
  theme_classic()+
  theme(legend.title = element_blank()) 



######################## SD (risk) vs. return #########################
pf_1 <- t(monthly_return[1,])
pf_2 <- t(monthly_return[2,])
pf_3 <- t(monthly_return[3,])
pf_4 <- t(monthly_return[4,])
pf_5 <- t(monthly_return[5,])

####Annulasize the returns 
#2014 (10m), 2015, 2016, 2017, 2018, 2019, 2020, 2021 (2m)
years <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
years_n_months <- c(10, 12, 12, 12, 12, 12, 12, 2) #number of months in each year
y <- c(0, 10, 22, 34, 46, 58, 70, 82) #cumulative sum used in the next loop to get the right return from the monthly_return 

n_years <- length(years) #number of years
an_return <- matrix(NA, nrow=ngroups, ncol=n_years) #annulizaed return matrix
colnames(an_return) <- years 
rownames(an_return) <- c("pf1", "pf2","pf3", "pf4", "pf5")

for (i in 1:ngroups){ #For each portfolio the annualized return are calculated
  for (k in 1:n_years){ #Year for year
    n_months <- years_n_months[k] #how many months in this year
    return_m=(1+monthly_return[i, y[k]+1]) #first month calculated to start the loop
    for (j in 2:n_months) {#rest of the months returns are multiplied on 
      return_m=(1+monthly_return[i,y[k]+j])*return_m
    }
    an_return[i,k]<-(return_m^(12/n_months)-1) #formula
  }
}





##### Bar chart of the annulizaed returns 
leif <- t(an_return)*100
leif2 <- cbind(years,leif)
leif3 <- leif2 %>%
  as_tibble %>%
  gather(key = "col", value = "anreturn", -years)


ggplot(data=leif3, aes(x=years, y=as.numeric(anreturn), fill=col)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_color_brewer(palette = "RdYlGn")+
  scale_fill_brewer(palette = "RdYlGn") + 
  xlab("Years") + ylab("Annual return") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme_classic()+
  theme(legend.title = element_blank()) 

####Average annualized return over the whole data period
av_an_return <- c(NA, ngroups)

for (i in 1:ngroups){
  av_an_return[i] <- mean(an_return[i,])
}

#### Removing weird year 2020 and less observation 2021
av_an_return_wo_202021 <- c(NA, ngroups)
for (i in 1:ngroups){
  av_an_return_wo_202021[i] <- mean(an_return[i,1:6])
}

### Standard deviation for the whole data period
sd_groups <- c(NA, ngroups)
for (i in 1:ngroups){
  sd_groups[i] <- sd(monthly_return[i,])*sqrt(12) #12 for annualized standard deviation
}


pf_names <- c("pf1", "pf2","pf3", "pf4", "pf5")


sd_an <- data.frame("pf" = pf_names,
                    "sd" = sd_groups*100,
                    "an" = CAGR*100)


ggplot(sd_an, aes(x=sd, y=an, color=pf)) +
  geom_point(size=5)+
  scale_color_brewer(palette = "RdYlGn")+
  scale_fill_brewer(palette = "RdYlGn") + 
  xlab("Annualized standard deviation") + ylab("Compound annual growth rate") +
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))







### Removing weird year 2020 and less observations 2021
sd_groups_wo_202021 <- c(NA, ngroups)
for (i in 1:ngroups){
  sd_groups_wo_202021[i] <- sd(monthly_return[i,1:69])*sqrt(69)
}

##### Scatter plot: Whole dataperiod 
pf_names <- c("pf1", "pf2","pf3", "pf4", "pf5")


sd_an <- data.frame("pf" = pf_names,
                    "sd" = sd_groups*100,
                    "an" = av_an_return*100)


ggplot(sd_an, aes(x=sd, y=an, color=pf)) +
  geom_point(size=5)+
  scale_color_brewer(palette = "RdYlGn")+
  scale_fill_brewer(palette = "RdYlGn") + 
  xlab("Standard deviation") + ylab("Average annual return") +
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))



##### Scatter plot:w.o. 2020+21

sd_an_wo_202021 <- data.frame("pf" = pf_names,
                              "sd" = sd_groups_wo_202021*100,
                              "an" = av_an_return_wo_202021*100)

ggplot(sd_an_wo_202021, aes(x=sd, y=an, color=pf)) +
  geom_point(size=5)+
  scale_color_brewer(palette = "RdYlGn")+
  scale_fill_brewer(palette = "RdYlGn") + 
  xlab("Standard deviation") + ylab("Average annual return") +
  theme_classic()+
  theme(legend.title = element_blank())+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))







######################### Immun: Cleansing out risk #########################

FF <-  read_excel("Clean/FFMOM.xlsx", col_names = TRUE)
FF <- FF/100 #fra procent til decimal 
#FF <- FF[1:83,]#sidste observation har jeg ikke return fra 

#pf_test <- pf_2
#table_test <- cbind(pf_test,FF)
#model_test <- lm((pf_test-RF)~MktRF+SMB+HML+RMW+CMA+MOM, data=table_test)

#summary(model_test)
#coeftest(model_test, vcov = NeweyWest)



#Go long in pf_10 (green) and short in pf_1 (black)
pf_df <-pf_5-pf_1


FF <-  read_excel("Clean/FFMOM.xlsx", col_names = TRUE)
FF <- FF/100 
table <- cbind(pf_df,FF)
#model <- lm((pf_df - RF)~1, data=table) #For average exess return
model <- lm((pf_df - RF)~MktRF+SMB+HML+RMW+CMA+MOM, data=table)
#model <- lm((pf_df - RF)~MktRF+SMB+HML+RMW+CMA, data=table)
#model <- lm((pf_df - RF)~MktRF+SMB+HML, data=table)
#model <- lm((pf_df - RF)~MktRF, data=table)
coeftest(model, vcov = NeweyWest)



#Immunizere by using the coefficent results from the summary above 
#pf_df <-pf_10-pf_1-0.178337*FF$MktRF-0.128950*FF$SMB+0.388592*FF$HML+0.279752*FF$RMW-0.682428*FF$CMA
pf_immun <- pf_df-coef(model)[2]*FF$MktRF-coef(model)[3]*FF$SMB-coef(model)[4]*FF$HML
                              
table_immun <- cbind(pf_immun,FF)
model_immun <- lm((pf_immun - RF)~ MktRF + SMB + HML, data=table_immun)
coeftest(model_immun, vcov=NeweyWest)

summary(model_immun) #Check that there is almost zero loading on all risk factors


#Create dataframe for imum strategy 
monthly_immun_amount <- matrix(0, nrow =1, ncol = ndates)
colnames(monthly_immun_amount) <- as.character(dates_0)
monthly_immun_amount<- as.data.frame(monthly_immun_amount)


#Starter strategien med 1 unit
monthly_immun_amount[1] <- (pf_immun[1]+1)*1

for (i in 2:(ndates)){
  monthly_immun_amount[i] <-
    (pf_immun[i]+1)*monthly_immun_amount[,i-1]
}


monthly_immun_amount_t <- t(monthly_immun_amount)
plot(monthly_immun_amount_t)


######Same for pf_df with risk included to compare 
#Create dataframe for pf_df
monthly_df_amount <- matrix(0, nrow =1, ncol = ndates)
colnames(monthly_df_amount) <- as.character(dates_0)
monthly_df_amount<- as.data.frame(monthly_df_amount)

#Starter strategien med 1 unit
monthly_df_amount[1] <- (pf_df[1]+1)*1

for (i in 2:(ndates)){
  monthly_df_amount[i] <-
    (pf_df[i]+1)*monthly_df_amount[,i-1]
}


monthly_df_amount_t <- t(monthly_df_amount)



anna2 <- cbind(1:84, monthly_immun_amount_t, monthly_df_amount_t)
colnames(anna2) <- c("index", "pf_immun", "pf_df")

anna3 <- anna2 %>%
  as_tibble %>%
  gather(key = "col", value = "penge", -index)

coltt <- scale_color_manual(values = c("black", "navy"))

ggplot(anna3, aes(x = index, y = penge), col=col) + 
  #geom_point(aes(color=anna3$col)) +
  geom_line(aes(color = anna3$col)) + coltt+
  #scale_color_brewer(palette = "Blues")+
  #scale_fill_brewer(palette = "Blues") + 
  xlab("Month") + ylab("Investment") +
  theme_classic()+
  theme(legend.title = element_blank()) 




##### Notes on Newey West standard error 

library(lmtest)
library(sandwich)

NeweyWest(model)

## Newey & West (1994) compute this type of estimator
NeweyWest(model)

## The Newey & West (1987) estimator requires specification
## of the lag and suppression of prewhitening
NeweyWest(model, lag = 4, prewhite = FALSE)

#install.packages("lmtest")
coeftest(model_test, vcov = NeweyWest)
?NeweyWest
?coeftest
#install.packages("sos")
#library(sos)
#findFn("coeftest")

plot(fitted(model), rstandard(model))
plot(rstandard(model))
qqnorm(rstandard(model));abline(0,1)




####################### Investment strategy ########################
#Create dataframe for total amounts 
monthly_total_amount <- matrix(0, nrow =ngroups, ncol = ndates)
colnames(monthly_total_amount) <- as.character(dates_0)
monthly_total_amount<- as.data.frame(monthly_total_amount)

#Starter strategien med 1 unit
monthly_total_amount[,1] <- (monthly_return[,1]+1)*1

for (i in 2:(ndates)){
  monthly_total_amount[,i] <-
    (monthly_return[,i]+1)*monthly_total_amount[,i-1]
}

CAGR <- vector()

#The investment is 1 unit in the begining for all portfolios, and then it involves
#in the 84 periods, that I have data for 
for (i in 1:ngroups){
  CAGR[i]<-(monthly_total_amount[i,84]/1)^(1/(85/12))-1
}







monthly_total_amount_t <- t(monthly_total_amount)




mads <-  #relocate(is.character) %>%
 monthly_total_amount_t

mads2 <- cbind(1:83, mads)
colnames(mads2) <- c("index", "pf1","pf2", "pf3", "pf4", "pf5")

mads3 <- mads2 %>%
  as_tibble %>%
  #relocate(pf10, how="last")%>%
  gather(key = "col", value = "penge", -index)
  #relocate("pf10", .after="pf9")


#ggplot(mads3, aes(x = index, y = penge)) + geom_point() + facet_wrap(~col)


ggplot(mads3, aes(x = index, y = penge), col=col) + 
  geom_point(aes(color=mads3$col)) +
  geom_line(aes(color = mads3$col)) + 
  scale_color_brewer(palette = "RdYlGn")+
  scale_fill_brewer(palette = "RdYlGn") + 
  xlab("Month") + ylab("Investment") +
  theme_classic()+
  theme(legend.title = element_blank()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  #scale_color_viridis(discrete = TRUE, option = "D" )+
  #scale_fill_viridis(discrete=TRUE, option="D")



###################COLOUR###############################
display.brewer.all()
brewer.pal(n=10, name="RdYlGn")
display.brewer.pal(n=10, name="RdYlGn")




####################### Investment strategi + Farma French 5 ##########


FF5 <-  read_excel("Clean/FF5.xlsx", col_names = TRUE)
FF5 <- FF5/100 #fra procent til decimal 
FF5_t <- t(FF5)


#Create dataframe for total amounts FF
monthly_total_amount_FF5 <- matrix(0, nrow =nrow(FF5_t), ncol = ncol(FF5_t))
colnames(monthly_total_amount_FF5) <- as.character(dates_0)
rownames(monthly_total_amount_FF5) <- rownames(FF5_t)
monthly_total_amount_FF5<- as.data.frame(monthly_total_amount_FF5)

monthly_total_amount_FF5[,1] <- (FF5_t[,1]+1)*1

for (j in 2:(ndates)){
    monthly_total_amount_FF5[,j] <-
      (FF5_t[,j]+1)*monthly_total_amount_FF5[,j-1]
  }

monthly_total_amount_FF5_t <- t(monthly_total_amount_FF5)
plot(monthly_total_amount_FF5_t[,1])

#Make plots with all these factors in 



################################ My own market portfolio ######################

#Here I calculated the market weighted average return for my whole univers 


monthly_return_mkt <- matrix(0,1, ncol = ndates) #only one group now
colnames(monthly_return_mkt) <- as.character(dates_0)
monthly_return_mkt<- as.data.frame(monthly_return_mkt)



#Create control dataframe
control_filtering_companies <- matrix(NA, nrow=4, ncol=ndates) #En raekke pr. information, der filtreres paa + en raekke til antal ved start
rownames(control_filtering_companies) <- c("Start", "E", "Return", "Cap")
colnames(control_filtering_companies) <- c(as.character(dates_0))




for (i in 1:(ndates)){
  list <- na.omit(as.vector(SPXMEMBERS[,i]))
  control_filtering_companies[1,i]<-nrow(list)
  
  E_temp <- E %>%
    dplyr::filter(E$Ticker %in% list[[1]])
  E_temp_fil <- E_temp %>%
    filter(E_temp[,i+1] !="#N/A N/A") #First column is Ticker name (+1)
  control_filtering_companies[2,i]<-nrow(E_temp)-nrow(E_temp_fil)
  
  Return_temp <- Return %>%
    filter(Return$Ticker %in% E_temp_fil$Ticker)
  Return_temp_fil <- Return_temp %>%
    filter(Return_temp[,i+1] !="#N/A N/A") #first column is Ticker name (+1) and return is backward calculated so +1 more
  Return_temp_fil <- Return_temp_fil %>% #first column is Ticker name (+1)
    filter(Return_temp_fil[,i+1] !="#N/A boost::bad_any_cast: failed conversion using boost::any_cast") 
  control_filtering_companies[3,i]<-nrow(Return_temp)-nrow(Return_temp_fil)
  
  Cap_temp <- Cap %>%
    filter(Cap$Ticker %in% Return_temp_fil$Ticker)
  Cap_temp_fil <- Cap_temp %>%
    filter(Cap_temp[,i+1] !="#N/A N/A") #First column is Ticker name (+1)
   control_filtering_companies[4,i]<-nrow(Cap_temp)-nrow(Cap_temp_fil)
  
  E_temp_fil <- E_temp_fil %>% 
    filter(E_temp_fil$Ticker %in% Cap_temp_fil$Ticker)
  Return_temp_fil <- Return_temp_fil %>% 
    filter(Return_temp_fil$Ticker %in% Cap_temp_fil$Ticker)
  
  
  ##### Now ordering   
  
  #First column is ticker names, and return is backward calculated
  joined_temp <- cbind(E_temp_fil[,1],E_temp_fil[,i+1],Cap_temp_fil[,i+1],Return_temp_fil[,i+1])
  colnames(joined_temp) <- c("Ticker", "E", "Cap", "Return")
  
  joined_temp[,2]=as.numeric(joined_temp[,2])
  joined_temp[,3]=as.numeric(joined_temp[,3])
  joined_temp[,4]=as.numeric(joined_temp[,4])
  
  pf <- joined_temp
    
  
  return <- sum(pf$Return*pf$Cap)/sum(pf$Cap)
  monthly_return_mkt[i] <-return
  #For 2020-12-01 betyder monthly return, at det er det maanedlige afkast paa
  #den investering, som du sammensatte d.1 december 2020. Dvs. 1 december beslutter du dig
  #for at investerer så meget i de til gengængelige virksomheder og den naeste maanedstid 
  #indkasserer du saa det overskud, der staar ved 2020-12-01, selvom det faktisk forst er penge, 
  #som er tjent d. 1 januar 2021. 
  
}

monthly_return_mkt <- monthly_return_mkt/100
monthly_return_mkt <- monthly_return_mkt-FF5[,6]


monthly_mkt_amount <- matrix(0, nrow =1, ncol = ndates)
colnames(monthly_mkt_amount) <- as.character(dates_0)
monthly_mkt_amount<- as.data.frame(monthly_mkt_amount)




#Starter strategien med 1 unit
monthly_mkt_amount[1] <- (monthly_return_mkt[1]+1)*1

for (i in 2:(ndates)){
  monthly_mkt_amount[i] <-
    (monthly_return_mkt[i]+1)*monthly_mkt_amount[,i-1]
}


monthly_mkt_amount_t <- t(monthly_mkt_amount)
plot(monthly_mkt_amount_t)
#should be almost identical to 
plot(monthly_total_amount_FF_t[,1])

louise <- cbind(monthly_total_amount_FF5_t, monthly_mkt_amount_t)

louise2 <- cbind(1:84, louise)
colnames(louise2) <- c("index", "MktRF", "SMB","HML", "RMW", "CMA", "RF", "MyMktRF")

louise3 <- louise2 %>%
  as_tibble %>%
  #relocate(pf10, how="last")%>%
  gather(key = "col", value = "penge", -index)
#relocate("pf10", .after="pf9")


#ggplot(mads3, aes(x = index, y = penge)) + geom_point() + facet_wrap(~col)

louise4 <- louise3 %>%
  filter(col=='MktRF'|col=='MyMktRF')

ggplot(louise4, aes(x = index, y = penge), col=col) + 
  geom_point(aes(color=louise4$col)) +
  geom_line(aes(color = louise4$col)) + 
  #scale_color_brewer(palette = "RdYlGn")+
  #scale_fill_brewer(palette = "RdYlGn") + 
  xlab("Month") + ylab("Investment") +
  coltt +
  theme_classic()+
  theme(legend.title = element_blank()) 
#scale_color_viridis(discrete = TRUE, option = "D" )+
#scale_fill_viridis(discrete=TRUE, option="D")




