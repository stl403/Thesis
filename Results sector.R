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
library(xtable)
library(glue)

result_generator <- function(monthly_return, 
                             ff3 = "Clean/FF3.xlsx", 
                             ffmom = "Clean/FFMOM.xlsx"){
  #sørg for ikke at dividere for mange gange med 100 
  monthly_return_fct <- monthly_return / 100
  
  pf_1 <- t(monthly_return_fct[1,])
  pf_2 <- t(monthly_return_fct[2,])
  pf_3 <- t(monthly_return_fct[3,])
  pf_4 <- t(monthly_return_fct[4,])
  pf_5 <- t(monthly_return_fct[5,])
  
  
  FF3 <-  read_excel(ff3, col_names = TRUE)
  FFMOM <-  read_excel(ffmom, col_names = TRUE)
  
  FF3 <- FF3/100 #fra procent til decimal 
  FFMOM <- FFMOM/100 #fra procent til decimal 
  
  #Go long in pf_10 (green) and short in pf_1 (black)
  pf_df <- pf_5-pf_1
  
  coef_df <- matrix(nrow = 5, ncol = 4)
  
  model <- lm((pf_df - RF)~1, data=cbind(pf_df,FF3)) #For average exess return
  coef_df[1,1] <- ((1+coef(model)[1])^12-1)*100
  coef_df[1,2] <- ((1+coeftest(model, vcov = NeweyWest)[1,2])^12-1)*100
  coef_df[1,3] <- coeftest(model, vcov = NeweyWest)[1,4]
  
  model <- lm((pf_df - RF)~MktRF, data=cbind(pf_df,FF3))
  coef_df[2,1] <- ((1+coef(model)[1])^12-1)*100
  coef_df[2,2] <- ((1+coeftest(model, vcov = NeweyWest)[1,2])^12-1)*100
  coef_df[2,3] <- coeftest(model, vcov = NeweyWest)[1,4]
  
  model <- lm((pf_df - RF)~MktRF+SMB+HML, data=cbind(pf_df,FF3))
  coef_df[3,1] <- ((1+coef(model)[1])^12-1)*100
  coef_df[3,2] <- ((1+coeftest(model, vcov = NeweyWest)[1,2])^12-1)*100
  coef_df[3,3] <- coeftest(model, vcov = NeweyWest)[1,4]
  
  model <- lm((pf_df - RF)~MktRF+SMB+HML+RMW+CMA, data=cbind(pf_df,FFMOM))
  coef_df[4,1] <- ((1+coef(model)[1])^12-1)*100
  coef_df[4,2] <- ((1+coeftest(model, vcov = NeweyWest)[1,2])^12-1)*100
  coef_df[4,3] <- coeftest(model, vcov = NeweyWest)[1,4]
  
  model <- lm((pf_df - RF)~MktRF+SMB+HML+RMW+CMA+MOM, data=cbind(pf_df,FFMOM))
  coef_df[5,1] <- ((1+coef(model)[1])^12-1)*100
  coef_df[5,2] <- ((1+coeftest(model, vcov = NeweyWest)[1,2])^12-1)*100
  coef_df[5,3] <- coeftest(model, vcov = NeweyWest)[1,4]
  
  
  colnames(coef_df) <- c("intercept", "SE", "pvalue", "stars")
  coef_tibble <- coef_df %>% as_tibble %>% mutate(stars = "")
  coef_tibble <- coef_tibble %>% mutate(stars = case_when(
    pvalue > 0.1 ~ "",
    0.1 >= pvalue & pvalue > 0.05 ~ ".",
    0.05 >= pvalue & pvalue > 0.01 ~ "*",
    0.01 >= pvalue & pvalue > 0.001 ~ "**",
    0.001 >= pvalue ~ "***"))
  
  return(coef_tibble)
}



get_latex_table <- function(reslist){
  m <- length(reslist)
  n <- dim(reslist[[1]])[1]
  output <- matrix(nrow = n*2, ncol = m+1)
  output[,1] <- c("Average excess return", "", "$\\alpha_{CAPM}$", "", "$\\alpha_{FF3}$", "", "$\\alpha_{FF5}$","", "$\\alpha_{FF6}$", "")
  for(i in 1:m){
    bobby <- reslist[[i]] %>% 
      mutate(first = glue("{round(intercept,2)}\\%{stars}")) %>% 
      mutate(second = signif(SE, 2)) %>% 
      select(first, second) %>% 
      mutate(second = as.character(second)) %>% 
      mutate(second = ifelse(nchar(second)==1,
                             yes = glue("({second}",".0)"),
                             no = glue("({second})")
      ))
    output[1:n*2-1, i+1] <- bobby$first
    output[1:n*2, i+1] <- bobby$second
  }
  return(output)
}

#################################################
#Create 4 results to use in one table

sector

#Value weighted  
res1 <- result_generator(monthly_return = sector_scores_value(7))

#Equal weighted 
res2 <- result_generator(monthly_return = sector_scores_equal(7))

#Value weighted 
res3 <- result_generator(monthly_return = sector_scores_value(8))

#Equal weighted 
res4 <- result_generator(monthly_return = sector_scores_equal(8))


#Value weighted 
res5 <- result_generator(monthly_return = sector_scores_value(9))

#Equal weighted 
res6 <- result_generator(monthly_return = sector_scores_equal(9))




reslist <- list("Market cap sector 1" = res1, 
           "Equal sector 1" = res2, 
           "Market cap sector 2" = res3, 
           "Equal sector 2" = res4,
           "Market cap sector 3"=res5, 
           "Equal sector 3"=res6
           )


yrsa <- get_latex_table(reslist)
yrsa
print(xtable(yrsa), type='latex', sanitize.text.function=identity, include.rownames=FALSE)










