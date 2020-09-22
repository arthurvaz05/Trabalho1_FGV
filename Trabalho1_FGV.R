library(dplyr)
library(zoo)
library(tidyverse)


comp_prices <- read.csv2('/Users/arthurvaz/Downloads/test_prc/comp_prices.csv', sep = ',', dec = '.')

sales <- read.csv2('/Users/arthurvaz/Downloads/test_prc/sales.csv', sep = ',', dec = '.')

#Verificando as classes das variáveis
sapply(comp_prices, class)
sapply(sales, class)

comp_prices$DATE_EXTRACTION <- comp_prices$DATE_EXTRACTION%>%as.Date()
sales$DATE_ORDER <- sales$DATE_ORDER%>%as.Date()

sales$MONTH_YEAR <- as.yearmon(sales$DATE_ORDER, "%m/%Y")
comp_prices$MONTH_YEAR <- as.yearmon(comp_prices$DATE_EXTRACTION, "%m/%Y")

sales$DATE_ORDER <- NULL
comp_prices$DATE_EXTRACTION <- NULL

sales <-  aggregate(sales[,c('QTY_ORDER','REVENUE')],
                    by = list(PROD_ID = sales$PROD_ID, MONTH_YEAR = sales$MONTH_YEAR),
                    FUN = sum)

sales$PRICE  <- sales$REVENUE/sales$QTY_ORDER


library(plotly)
fig <- plot_ly(sales, x = ~MONTH_YEAR, y = ~PRICE, type = 'scatter', mode = 'lines', color = ~PROD_ID)
fig


sales%>%
  select(-QTY_ORDER)%>%
  spread(PROD_ID,REVENUE)%>%
  select(-MONTH_YEAR)%>%
  sapply( function(x) ifelse(length(boxplot(x,plot=FALSE)['out'][[1]])==1,
                             boxplot(x,plot=FALSE)['out'],0))
  

comp_prices$COMPETITOR_PAY_TYPE <- paste(comp_prices$COMPETITOR,'_',comp_prices$PAY_TYPE,sep= '')
comp_prices$COMPETITOR <- NULL
comp_prices$PAY_TYPE <- NULL

comp_prices <-  aggregate(comp_prices[,'COMPETITOR_PRICE'],
                    by = list(PROD_ID = comp_prices$PROD_ID, MONTH_YEAR = comp_prices$MONTH_YEAR, 
                              COMPETITOR_PAY_TYPE = comp_prices$COMPETITOR_PAY_TYPE),
                    FUN = mean)

colnames(comp_prices) <- c("PROD_ID","MONTH_YEAR","COMPETITOR_PAY_TYPE","COMPETITOR_PRICE")

comp_prices <- spread(comp_prices,COMPETITOR_PAY_TYPE,COMPETITOR_PRICE)

sales_total <- left_join(sales, comp_prices, by = c('PROD_ID','MONTH_YEAR'))

sales_total$price_ind_1 <- sales_total[,grepl('1',colnames(sales_total))]%>%apply(1,function(x){
    x <- x[!is.na(x)] 
    mean(x)
})

sales_total$price_ind_2 <- sales_total[,grepl('2',colnames(sales_total))]%>%apply(1,function(x){
  x <- x[!is.na(x)] 
  mean(x)
})

sales_total[is.na(sales_total)] <- 0

sales_total$price_ind_mean <- sales_total[,c('price_ind_1','price_ind_2')]%>%apply(1,mean)

sales_total$price_index <-sales_total$PRICE/sales_total$price_ind_mean
sales_total$price_index_1 <-sales_total$PRICE/sales_total$price_ind_1
sales_total$price_index_2 <-sales_total$PRICE/sales_total$price_ind_2

sales_total[is.infinite(sales_total$price_index),'price_index'] <- 0

modelo <- list()
require(caTools) 
library(MASS)

for (i in unique(sales_total$PROD_ID)) {
  modelo_dt <- sales_total[sales_total$PROD_ID==i,c('MONTH_YEAR','QTY_ORDER','price_index')]
  modelo_dt$trend <- seq(1,NROW(modelo_dt))
  modelo_dt$MONTH_YEAR <- NULL
  train <- modelo_dt[1:NROW(modelo_dt)*0.7,]
  test <- modelo_dt[(NROW(modelo_dt)*0.7+1):NROW(modelo_dt),]
  modelo1 <- lm(QTY_ORDER~.,train)
  step.modelo1 <- stepAIC(modelo1, direction = "both", 
                        trace = FALSE)
  summary(step.modelo1)
  summary(modelo1)
  
  mape <- mean(abs((test$QTY_ORDER-(predict(modelo1, test, se.fit = TRUE)$fit))/test$QTY_ORDER))
  
  if(!exists('df_modelo')){
    df_modelo <- data.frame('PROD_ID'=i,'INTERCEPT'=modelo1$coefficients[1][[1]],'Price_index'=modelo1$coefficients[2][[1]],
               'trend'= modelo1$coefficients[3][[1]],'Mape' = mape)
  }else{
    df_modelo2 <- data.frame('PROD_ID'=i,'INTERCEPT'=modelo1$coefficients[1][[1]],'Price_index'=modelo1$coefficients[2][[1]],
                            'trend'= modelo1$coefficients[3][[1]],'Mape' = mape)
    df_modelo <- rbind(df_modelo,df_modelo2)
  }
}

