library(tidyverse)
library(tidyquant)
library(quantmod)
library(dplyr)
library(xts)
library(timetk)
library(highcharter)

company <- c("AMZN","FICO", "MSFT", "NFLX", 
             "AAPL", "NVDA", "JNJ", "M",
             "PG", "KO", "TSN", "GIS",
             "MMM", "VOX", "V","BRK-B",
             "ORCL", "COF", "BA", "AVGO",
             "GOOG", "GS", "BLK", "SBUX")
SP500 <- "^GSPC"

from_date <- "2010-03-07"
to_date <- "2025-03-07"

#Getting price data from Yahoo
stock_prices <- getSymbols(company, src= 'yahoo', from= from_date,
                           to= to_date, auto.assign = TRUE, 
                           warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-` (company)

SP500_prices <- getSymbols(SP500, src = 'yahoo',
                           from= from_date, to= to_date,
                           auto.assign = TRUE, warnings= FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-` ("SP500")

company<-str_replace_all(company, "-", ".") # renaming BRK-B to BRK.B

#Stocks Monthly Returns
stock_monthly <- to.monthly(stock_prices, indexAt = "lastof", OHLC = FALSE) 
stock_returns_xts <- Return.calculate(stock_monthly, method="log") %>%
  na.omit()

stock_returns_df <- stock_returns_xts%>%data.frame(date=index(.))%>%
  remove_rownames()%>%select("date", everything())

#SP500 Monthly Returns
SP500_monthly <- to.monthly(SP500_prices, indexAt = "lastof", OHLC = FALSE) %>%
  `colnames<-` ("SP500")
SP500_returns_xts <- Return.calculate(SP500_monthly, method="log") %>%
  na.omit()  %>%
  `colnames<-` ("SP500")

#S&P500 mean and STD
SP500_mean_return <- mean(SP500_returns_xts, na.rm = TRUE)
SP500_sd_return <- sd(SP500_returns_xts, na.rm = TRUE)
SP500_mean_return_percent  <- round(SP500_mean_return *100 ,4)
SP500_sd_return_percent <- round(SP500_sd_return *100, 4)
print(paste("Mean of S&P500 monthly returns:", SP500_mean_return))
print(paste("Standard deviation of S&P500 monthly returns:", SP500_sd_return))



#Naive Portfolio

equal_weight <- 1/ length(company)
naive_weight <- rep(equal_weight,length(company)) # making the vector of length equal to the length of the company vector, with all values 1/ length(company) to ensure sum of weights is 1 no matter the length of the company vector

tibble (naive_weight, company)
tibble (naive_weight, company) %>%
  summarise(total_weight = sum(naive_weight))


#Method 1: Returns using tidyverse
naive_returns_df_long <- stock_returns_df %>%
  gather(asset, returns, -date) %>%
  group_by(asset) %>%
  mutate(weights = equal_weight)

naive_portfolio_returns_tidyverse <- naive_returns_df_long %>%
  mutate(weighted_returns = returns * weights) %>%
  group_by(date) %>%
  summarise(Portfolio_Returns = sum(weighted_returns))

naive_portfolio_returns_tidy_mean <- mean(naive_portfolio_returns_tidyverse$Portfolio_Returns)
naive_portfolio_returns_tidy_mean_percent <- round(naive_portfolio_returns_tidy_mean *100, 4)

#Method 2: Returns using xts world
naive_portfolio_returns_xts <-
  Return.portfolio (stock_returns_xts,
                    weights = naive_weight,
                    rebalance_on="months") %>%
  `colnames<-` ("Portfolio Returns")

head (naive_portfolio_returns_xts, 3)

naive_portfolio_mean_returns_xts<-mean(naive_portfolio_returns_xts)
naive_portfolio_mean_returns_xts_percent <- round(naive_portfolio_mean_returns_xts *100, 4)


# Standard deviation
#method 1 Tidyverse
naive_portfolio_sd_tidy <- naive_portfolio_returns_tidyverse %>%
  summarise (
    sd= sd(Portfolio_Returns)) %>%
  mutate(sd_percent = round (sd, 4) *100)

# method 2 XTS world
naive_portfolio_sd_xts_bultin <- StdDev(stock_returns_xts, weights = naive_weight)
naive_portfolio_sd_xts_bultin_percent <- round(naive_portfolio_sd_xts_bultin *100 ,4)
naive_portfolio_sd_xts_bultin_percent[1,]

# visualization of Naive portfolio and S&P500
highchart(type = "stock") %>%    
  hc_title(text = "Naive Portfolio Returns vs S&P500") %>%
  hc_add_series(SP500_returns_xts$SP500, 
                name = names(SP500_returns_xts$SP500)) %>%
  hc_add_series(naive_portfolio_returns_xts$`Portfolio Returns`, 
                name = names(naive_portfolio_returns_xts$`Portfolio Returns`)) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = TRUE) %>% 
  hc_legend(enabled=TRUE) %>% 
  hc_scrollbar(enabled = TRUE)


Table2 <- data.frame(
  Asset = c("Naive Portfolio Method 1", "Naive Portfolio Method 2", "SP500"),
  Mean = c(naive_portfolio_returns_tidy_mean_percent, naive_portfolio_mean_returns_xts_percent, SP500_mean_return_percent),
  Standard_Deviation = c(naive_portfolio_sd_tidy$sd_percent,naive_portfolio_sd_xts_bultin_percent, SP500_sd_return_percent)
)
print(Table2)




#Custom Portfolio

custom_weight <- c(0.10,0.10,0.10,-0.02,
                   0.10,0.10,0.03,-0.08,
                   0.05,-0.02,0.05,0.05,
                   0.03,-0.05,0.10,0.07,
                   0.05,0.05,-0.04,0.10,
                   0.05,0.05,0.05,-0.02)
tibble (custom_weight, company)
tibble (custom_weight, company) %>%
  summarise(total_weight = sum(custom_weight))


#Method 1: Returns using tidyverse
custom_returns_df_long <- stock_returns_df %>%
  gather(asset, returns, -date) %>%
  group_by(asset) %>%   
  mutate(weights = case_when(asset==company[1] ~custom_weight[1], 
                             asset==company[2] ~custom_weight[2],
                             asset==company[3] ~custom_weight[3],
                             asset==company[4] ~custom_weight[4],
                             asset==company[5] ~custom_weight[5],
                             asset==company[6] ~custom_weight[6], 
                             asset==company[7] ~custom_weight[7],
                             asset==company[8] ~custom_weight[8],
                             asset==company[9] ~custom_weight[9],
                             asset==company[10] ~custom_weight[10],
                             asset==company[11] ~custom_weight[11], 
                             asset==company[12] ~custom_weight[12],
                             asset==company[13] ~custom_weight[13],
                             asset==company[14] ~custom_weight[14],
                             asset==company[15] ~custom_weight[15],
                             asset==company[16] ~custom_weight[16], 
                             asset==company[17] ~custom_weight[17],
                             asset==company[18] ~custom_weight[18],
                             asset==company[19] ~custom_weight[19],
                             asset==company[20] ~custom_weight[20],
                             asset==company[21] ~custom_weight[21], 
                             asset==company[22] ~custom_weight[22],
                             asset==company[23] ~custom_weight[23], 
                             asset==company[24] ~custom_weight[24]))


custom_portfolio_returns_tidyverse <- custom_returns_df_long %>%
  mutate(weighted_returns = returns * weights) %>%
  group_by(date)%>%
  summarise(Portfolio_Returns = sum(weighted_returns))

custom_portfolio_returns_tidy_mean <- mean(custom_portfolio_returns_tidyverse$Portfolio_Returns)
custom_portfolio_returns_tidy_mean_percent <- round(custom_portfolio_returns_tidy_mean *100, 4)


#Method 2: Returns using xts world
Custom_portfolio_returns_xts <-
  Return.portfolio (stock_returns_xts,
                    weights = custom_weight,
                    rebalance_on="months") %>%
  `colnames<-` ("Portfolio Returns")

head (Custom_portfolio_returns_xts, 3)

Custom_portfolio_mean_returns_xts<-mean(Custom_portfolio_returns_xts)
Custom_portfolio_mean_returns_xts_percent <- round(Custom_portfolio_mean_returns_xts *100, 4)


# Standard deviation
#method 1 Tidyverse
custom_portfolio_sd_tidy <- custom_portfolio_returns_tidyverse %>%
  summarise (
    sd= sd(Portfolio_Returns)) %>%
  mutate(sd_percent = round (sd, 4) *100)

# method 2 xts world
Custom_portfolio_sd_xts <- StdDev(stock_returns_xts, weights = custom_weight)
Custom_portfolio_sd_xts_percent <- round(Custom_portfolio_sd_xts*100 , 4)
Custom_portfolio_sd_xts_percent[1,]

#visualization of Custom portfolio & S&P500 and Table 3
highchart(type = "stock") %>%    
  hc_title(text = "Custom Portfolio Returns vs S&P500") %>%
  hc_add_series(SP500_returns_xts$SP500, 
                name = names(SP500_returns_xts$SP500)) %>%
  hc_add_series(Custom_portfolio_returns_xts$`Portfolio Returns`, 
                name = names(Custom_portfolio_returns_xts$`Portfolio Returns`)) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = TRUE) %>% 
  hc_legend(enabled=TRUE) %>% 
  hc_scrollbar(enabled = TRUE)



Table3 <- data.frame(
  Asset = c("Custom Portfolio Method 1", "Custom Portfolio Method 2", "S&P500"),
  Mean_Return = c(custom_portfolio_returns_tidy_mean_percent, Custom_portfolio_mean_returns_xts_percent, SP500_mean_return_percent),
  Standard_Deviation = c(custom_portfolio_sd_tidy$sd_percent, Custom_portfolio_sd_xts_percent[1,], SP500_sd_return_percent)
)
print(Table3)



#part 4 Sharpe Ratios
rfr <- .0003

#Method 1: for Naive Portfolio in xts world
naive_sharpe_xts <- 
  SharpeRatio(naive_portfolio_returns_xts, 
              Rf = rfr,
              FUN = "StdDev") %>% 
  `colnames<-`("Naive_Sharpe")


#Method 2: for Custom Portfolio in tidyverse world
custom_sharpe_tidyverse <- 
  custom_portfolio_returns_tidyverse %>% 
  summarise(Custom_Sharpe = mean(Portfolio_Returns - rfr)/ sd(Portfolio_Returns - rfr))


#Market Sharpe
market_sharpe <- 
  SP500_monthly %>%
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  mutate(returns = (log(SP500) - log(lag(SP500)))) %>% 
  na.omit() %>% 
  summarise(ratio = mean(returns - rfr)/sd(returns - rfr))


#Relative Sharpe Ratio

naive_relative_sharpe <- round(naive_sharpe_xts[,1]/market_sharpe$ratio,4)
custom_relative_sharpe <- round(custom_sharpe_tidyverse[[1,1]]/market_sharpe$ratio,4)

Table4 <- data.frame(
  Asset = c("Naive Portfolio", "Custom Portfolio", "Market"),
  Sharpe_Ratio = c(round(naive_sharpe_xts[,1],4), round(custom_sharpe_tidyverse[[1,1]],4), round(market_sharpe$ratio,4)),
  Relative_Sharpe = c(naive_relative_sharpe,custom_relative_sharpe,1)
)
print(Table4)


