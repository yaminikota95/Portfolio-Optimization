library(tidyquant)
library(tidyr)
library(tidyverse)
library(dbplyr)
library(quantmod) 
library(tseries)
library(timetk)
library(plotly)
library(highcharter)
library(broom)

rfr <- .0003

company <- c("AMZN","FICO", "MSFT", "NFLX", 
             "AAPL", "NVDA", "JNJ", "M",
             "PG", "KO", "TSN", "GIS",
             "MMM", "VOX", "V","BRK-B",
             "ORCL", "COF", "BA", "AVGO",
             "GOOG", "GS", "BLK", "SBUX")

from_date <- "2010-03-07"
to_date <- "2025-03-07"

#Getting price data from Yahoo
stock_prices <- getSymbols(company, src= 'yahoo', from= from_date,
                           to= to_date, auto.assign = TRUE, 
                           warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-` (company)

company<-str_replace_all(company, "-", ".") # renaming BRK-B to BRK.B

#Stocks Monthly Returns
stock_monthly <- to.monthly(stock_prices, indexAt = "lastof", OHLC = FALSE) 
stock_returns_xts <- Return.calculate(stock_monthly, method="log") %>%
  na.omit()

# S&P500 is used as proxy for market
SP500 <- "^GSPC"
SP500_prices <- getSymbols(SP500, src = 'yahoo',
                           from= from_date, to= to_date,
                           auto.assign = TRUE, warnings= FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-` ("SP500")

#SP500 Monthly Returns
SP500_monthly <- to.monthly(SP500_prices, indexAt = "lastof", OHLC = FALSE) %>%
  `colnames<-` ("SP500")
SP500_returns_xts <- Return.calculate(SP500_monthly, method="log") %>%
  na.omit()  %>%
  `colnames<-` ("SP500")

#S&P500 mean return, variance, Standard deviation, skewness, and kurtosis
SP500_mean_return <- mean(SP500_returns_xts, na.rm = TRUE)
SP500_variance <- var(SP500_returns_xts)
SP500_sd_return <- sd(SP500_returns_xts, na.rm = TRUE)
SP500_skewness <- skewness(SP500_returns_xts)
SP500_kurtosis <- kurtosis(SP500_returns_xts)

SP500_mean_return_percent  <- round(SP500_mean_return *100 ,4)
SP500_sd_return_percent <- round(SP500_sd_return *100, 4)
SP500_variance <- round(SP500_variance,4)
SP500_skewness <- round(SP500_skewness,4)
SP500_kurtosis <- round(SP500_kurtosis,4)

print(paste0("Mean Return of S&P500 monthly returns: ", SP500_mean_return_percent,"%"))
print(paste("Variance of S&P500 monthly returns:", SP500_variance))
print(paste0("Standard deviation of S&P500 monthly returns: ", SP500_sd_return_percent, "%"))
print(paste("Skewness of S&P500 monthly returns:", SP500_skewness))
print(paste("Kurtosis of S&P500 monthly returns:", SP500_kurtosis))



#Naive Portfolio
equal_weight <- 1/ length(company)
naive_weight <- rep(equal_weight,length(company)) # making the vector of length equal to the length of the company vector, with all values 1/ length(company) to ensure sum of weights is 1 no matter the length of the company vector

tibble (naive_weight, company)
tibble (naive_weight, company) %>%
  summarise(total_weight = sum(naive_weight))


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


# Portfolio Optimization
num_port <- 5000
# Creating a matrix to store the weights
all_ws <- matrix(nrow = num_port, ncol = length(company))

# Creating an empty vector to store portfolio returns
port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)


for (i in seq_along(port_returns)) { 
  w <- runif(length(company),min = -1, max = 1)
  w <- w/sum(w)
  
  # Storing weight in the matrix
  all_ws[i,] <- w
  
  # Portfolio returns
  portfolio_returns_xts <-Return.portfolio (stock_returns_xts,weights = w,rebalance_on="months") %>%
    `colnames<-` ("Returns")
  port_ret <- colMeans(portfolio_returns_xts)
  port_returns[i] <- port_ret
  
  # Creating and storing portfolio risk
  portfolio_sd_xts <- StdDev(stock_returns_xts, weights = w) %>%
    `colnames<-` ("Risk")
  port_sd <- colMeans(portfolio_sd_xts)
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  portfolio_sharpe_xts <- SharpeRatio(portfolio_returns_xts, Rf = rfr,FUN = "StdDev") %>%
    `colnames<-` ("Sharpe Ratio") %>%
    `rownames<-` ("")
  sr <- colMeans(portfolio_sharpe_xts)
  sharpe_ratio[i] <- sr
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

# Converting matrix to a tibble and changing column names
all_ws <- tk_tbl(all_ws)
colnames(all_ws) <- colnames(stock_returns_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_ws, portfolio_values))

#Identifying Optimal Portfolios
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

optimal_weights <- max_sr[1:length(company)]

optimal_weights_table <- data.frame(optimal_weights)%>%gather(.)%>%mutate(value = round(value,4))%>%`colnames<-`(c("Asset", "Weights"))
print(optimal_weights_table)

optimal_weights%>%
  summarise(total_weight = sum(optimal_weights))


# Naive Portfolio Returns 
naive_portfolio_returns_xts <-
  Return.portfolio (stock_returns_xts,
                    weights = naive_weight,
                    rebalance_on="months") %>%
  `colnames<-` ("Portfolio Returns")

head (naive_portfolio_returns_xts, 3)

# Naive Portfolio Mean Return 
naive_portfolio_mean_returns_xts<-mean(naive_portfolio_returns_xts)
naive_portfolio_mean_returns_xts_percent <- round(naive_portfolio_mean_returns_xts *100, 4)

# Naive Portfolio Variance
naive_portfolio_variance <- var(naive_portfolio_returns_xts)
naive_portfolio_variance <- round(naive_portfolio_variance,4)

# Naive portfolio Standard deviation
naive_portfolio_sd_return <- sd(naive_portfolio_returns_xts, na.rm = TRUE)
naive_portfolio_sd_return_percent <- round(naive_portfolio_sd_return *100, 4)

# Naive Portfolio Skewness
naive_portfolio_skewness <- skewness(naive_portfolio_returns_xts)
naive_portfolio_skewness <- round(naive_portfolio_skewness,4)

# Naive Portfolio Kurtosis
naive_portfolio_kurtosis <- kurtosis(naive_portfolio_returns_xts)
naive_portfolio_kurtosis <- round(naive_portfolio_kurtosis, 4)

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


# Custom Portfolio Returns 
custom_portfolio_returns_xts <-
  Return.portfolio (stock_returns_xts,
                    weights = custom_weight,
                    rebalance_on="months") %>%
  `colnames<-` ("Portfolio Returns")

head (custom_portfolio_returns_xts, 3)

# Custom Portfolio Mean Return 
custom_portfolio_mean_returns_xts<-mean(custom_portfolio_returns_xts)
custom_portfolio_mean_returns_xts_percent <- round(custom_portfolio_mean_returns_xts *100, 4)

# Custom Portfolio Variance
custom_portfolio_variance <- var(custom_portfolio_returns_xts)
custom_portfolio_variance <- round(custom_portfolio_variance, 4)

# Custom portfolio Standard deviation
custom_portfolio_sd_return <- sd(custom_portfolio_returns_xts, na.rm = TRUE)
custom_portfolio_sd_return_percent <- round(custom_portfolio_sd_return *100, 4)

# Custom Portfolio Skewness
custom_portfolio_skewness <- skewness(custom_portfolio_returns_xts)
custom_portfolio_skewness <- round(custom_portfolio_skewness,4)

# Custom Portfolio Kurtosis
custom_portfolio_kurtosis <- kurtosis(custom_portfolio_returns_xts)
custom_portfolio_kurtosis <- round(custom_portfolio_kurtosis,4)

#visualization of Custom portfolio & S&P500
highchart(type = "stock") %>%    
  hc_title(text = "Custom Portfolio Returns vs S&P500") %>%
  hc_add_series(SP500_returns_xts$SP500, 
                name = names(SP500_returns_xts$SP500)) %>%
  hc_add_series(custom_portfolio_returns_xts$`Portfolio Returns`, 
                name = names(custom_portfolio_returns_xts$`Portfolio Returns`)) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = TRUE) %>% 
  hc_legend(enabled=TRUE) %>% 
  hc_scrollbar(enabled = TRUE)


# Optimal Portfolio Returns 
optimal_portfolio_returns_xts <-
  Return.portfolio (stock_returns_xts,
                    weights = unlist(optimal_weights, use.names = FALSE),
                    rebalance_on="months") %>%
  `colnames<-` ("Portfolio Returns")

head (optimal_portfolio_returns_xts, 3)

# Optimal Portfolio Mean Return 
optimal_portfolio_mean_returns_xts<-mean(optimal_portfolio_returns_xts)
optimal_portfolio_mean_returns_xts_percent <- round(optimal_portfolio_mean_returns_xts *100, 4)

# Optimal Portfolio Variance
optimal_portfolio_variance <- var(optimal_portfolio_returns_xts)
optimal_portfolio_variance <- round(optimal_portfolio_variance, 4)

# Optimal portfolio Standard deviation
optimal_portfolio_sd_return <- sd(optimal_portfolio_returns_xts, na.rm = TRUE)
optimal_portfolio_sd_return_percent <- round(optimal_portfolio_sd_return *100, 4)

# Optimal Portfolio Skewness
optimal_portfolio_skewness <- skewness(optimal_portfolio_returns_xts)
optimal_portfolio_skewness <- round(optimal_portfolio_skewness, 4)

# Optimal Portfolio Kurtosis
optimal_portfolio_kurtosis <- kurtosis(optimal_portfolio_returns_xts)
optimal_portfolio_kurtosis <- round(optimal_portfolio_kurtosis, 4)

#visualization of Optimal portfolio & S&P500
highchart(type = "stock") %>%    
  hc_title(text = "Optimal Portfolio Returns vs S&P500") %>%
  hc_add_series(SP500_returns_xts$SP500, 
                name = names(SP500_returns_xts$SP500)) %>%
  hc_add_series(optimal_portfolio_returns_xts$`Portfolio Returns`, 
                name = names(optimal_portfolio_returns_xts$`Portfolio Returns`)) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = TRUE) %>% 
  hc_legend(enabled=TRUE) %>% 
  hc_scrollbar(enabled = TRUE)



Table1 <- data.frame(
  Asset = c("Naive Portfolio", "Custom Portfolio", "Optimal Portfolio", "S&P500"),
  Mean_Return = c(naive_portfolio_mean_returns_xts_percent, custom_portfolio_mean_returns_xts_percent, optimal_portfolio_mean_returns_xts_percent, SP500_mean_return_percent),
  Variance = c(naive_portfolio_variance, custom_portfolio_variance, optimal_portfolio_variance, SP500_variance),
  Standard_Deviation = c(naive_portfolio_sd_return_percent, custom_portfolio_sd_return_percent, optimal_portfolio_sd_return_percent, SP500_sd_return_percent),
  Skewness = c(naive_portfolio_skewness, custom_portfolio_skewness, optimal_portfolio_skewness, SP500_skewness),
  Kurtosis = c(naive_portfolio_kurtosis, custom_portfolio_kurtosis, round(optimal_portfolio_kurtosis,4), SP500_kurtosis)
)
print(Table1)

# Portfolio Outperformance
naive_market_joined <- data.frame(cbind(naive_portfolio_returns_xts,SP500_returns_xts))%>%
  `colnames<-`(c("returns","market"))%>%
  mutate(Outperformed = returns>market)%>%
  mutate(Outperformed_by = returns - market)

print(paste0("Number of months that the Naive Portfolio outperformed the market: ",sum(naive_market_joined$Outperformed), " by an average of ",round(100*mean(naive_market_joined$Outperformed_by[naive_market_joined$Outperformed==TRUE]),4),"%"))

custom_market_joined <- data.frame(cbind(custom_portfolio_returns_xts,SP500_returns_xts))%>%
  `colnames<-`(c("returns","market"))%>%
  mutate(Outperformed = returns>market)%>%
  mutate(Outperformed_by = returns - market)

print(paste0("Number of months that the Custom Portfolio outperformed the market: ",sum(custom_market_joined$Outperformed), " by an average of ",round(100*mean(custom_market_joined$Outperformed_by[custom_market_joined$Outperformed==TRUE]),4),"%"))

optimal_market_joined <- data.frame(cbind(optimal_portfolio_returns_xts,SP500_returns_xts))%>%
  `colnames<-`(c("returns","market"))%>%
  mutate(Outperformed = returns>market)%>%
  mutate(Outperformed_by = returns - market)

print(paste0("Number of months that the Optimal Portfolio outperformed the market: ",sum(optimal_market_joined$Outperformed), " by an average of ",round(100*mean(optimal_market_joined$Outperformed_by[optimal_market_joined$Outperformed==TRUE]),4),"%"))

# correlation matrix
correlation_matrix <- cor(stock_returns_xts)
print(correlation_matrix)


# CAPM
# Market returns
SP500_returns_df <- data.frame(date = index(SP500_returns_xts), market_returns = coredata(SP500_returns_xts))%>%`colnames<-`(c('date','market_returns'))

# CAPM for Naive portfolio
naive_portfolio_df <- data.frame(date = index(naive_portfolio_returns_xts), portfolio_returns = coredata(naive_portfolio_returns_xts))%>%`colnames<-`(c('date','returns'))

CAPM_naive <-
  naive_portfolio_df %>% 
  do(model = lm(returns ~ SP500_returns_df$market_returns, data = .))%>% 
  mutate(model = map(model, tidy)) %>%
  unnest(model)

# Naive portfolio CAPM Beta
naive_capm_beta <- CAPM_naive%>% filter(term != "(Intercept)")

# Naive portfolio CAPM Alpha (Jensen Alpha)
naive_capm_alpha <- CAPM_naive%>% filter(term == "(Intercept)")%>%select(estimate)
naive_capm_alpha <- round(naive_capm_alpha[[1,1]],4)


# CAPM for Custom portfolio
custom_portfolio_df <- data.frame(date = index(custom_portfolio_returns_xts), portfolio_returns = coredata(custom_portfolio_returns_xts))%>%`colnames<-`(c('date','returns'))

CAPM_custom <-
  custom_portfolio_df %>% 
  do(model = lm(returns ~ SP500_returns_df$market_returns, data = .))%>% 
  mutate(model = map(model, tidy)) %>%
  unnest(model)

# Custom portfolio CAPM Beta
custom_capm_beta <- CAPM_custom%>% filter(term != "(Intercept)")

# Custom portfolio CAPM Alpha (Jensen Alpha)
custom_capm_alpha <- CAPM_custom%>% filter(term == "(Intercept)")%>%select(estimate)
custom_capm_alpha <- round(custom_capm_alpha[[1,1]],4)


# CAPM for optimal portfolio
optimal_portfolio_df <- data.frame(date = index(optimal_portfolio_returns_xts), portfolio_returns = coredata(optimal_portfolio_returns_xts))%>%`colnames<-`(c('date','returns'))

CAPM_optimal <-
  optimal_portfolio_df %>% 
  do(model = lm(returns ~ SP500_returns_df$market_returns, data = .))%>% 
  mutate(model = map(model, tidy)) %>%
  unnest(model)

# optimal portfolio CAPM Beta
optimal_capm_beta <- CAPM_optimal%>% filter(term != "(Intercept)")

# optimal portfolio CAPM Alpha (Jensen Alpha)
optimal_capm_alpha <- CAPM_optimal%>% filter(term == "(Intercept)")%>%select(estimate)
optimal_capm_alpha <- round(optimal_capm_alpha[[1,1]],4)


# FAMA FRENCH FIVE FACTOR MODEL
# downloading Fama French 5 factor model data
factors_input <- "Global_5_Factors_Daily"
factors_address <- 
  paste("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/", 
        factors_input, 
        "_CSV.zip", 
        sep="" )

factors_csv_name <- paste(factors_input, ".csv", sep="")

temp <- tempfile()

download.file(
  # location of file to be downloaded
  factors_address,
  # where we want R to store that file
  temp)


Global_5_Factors <- 
  read_csv(unz(temp, factors_csv_name) , skip =6) %>%
  rename(date = ...1, MKT = `Mkt-RF`) %>%
  mutate(date = ymd(parse_date_time(date, "%Y%m%d")))%>%
  mutate_if(is.numeric, funs(. / 100))

Global_5_Factors_monthly <- Global_5_Factors%>%to.monthly(indexAt = "lastof", OHLC = FALSE)%>%
  mutate(date = row.names(.))%>%
  mutate(date = ymd(parse_date_time(date, "%Y%m%d")))%>%remove_rownames(.)

naive_df_joined <- left_join(naive_portfolio_df,Global_5_Factors_monthly,join_by(date))%>% 
  mutate(Returns_excess = returns - RF) %>% 
  na.omit()

# Fama French five factor model regression for Naive Portfolio
ff_naive <-
  naive_df_joined %>% 
  do(model = lm(Returns_excess ~ MKT + SMB + HML + RMW + CMA, data = .))%>% 
  mutate(model = map(model, tidy))%>%
  unnest(model) 

# Fama French five factor model estimates for Naive Portfolio
naive_five_factors <- ff_naive%>% 
  filter(term != "(Intercept)")

# Alpha of the naive portfolio
naive_ff_alpha <- ff_naive%>% 
  filter(term == "(Intercept)")%>% select(estimate)
naive_ff_alpha <- round(naive_ff_alpha[[1,1]],4)


custom_df_joined <- left_join(custom_portfolio_df,Global_5_Factors_monthly,join_by(date))%>% 
  mutate(Returns_excess = returns - RF) %>% 
  na.omit()

# Fama French five factor model regression for custom Portfolio
ff_custom <-
  custom_df_joined %>% 
  do(model = lm(Returns_excess ~ MKT + SMB + HML + RMW + CMA, data = .))%>% 
  mutate(model = map(model, tidy))%>%
  unnest(model) 

# Fama French five factor model estimates for custom Portfolio
custom_five_factors <- ff_custom%>% 
  filter(term != "(Intercept)")

# Alpha of the custom portfolio
custom_ff_alpha <- ff_custom%>% 
  filter(term == "(Intercept)")%>% select(estimate)
custom_ff_alpha <- round(custom_ff_alpha[[1,1]],4)

optimal_df_joined <- left_join(optimal_portfolio_df,Global_5_Factors_monthly,join_by(date))%>% 
  mutate(Returns_excess = returns - RF) %>% 
  na.omit()

# Fama French five factor model regression for optimal Portfolio
ff_optimal <-
  optimal_df_joined %>% 
  do(model = lm(Returns_excess ~ MKT + SMB + HML + RMW + CMA, data = .))%>% 
  mutate(model = map(model, tidy))%>%
  unnest(model) 

# Fama French five factor model estimates for optimal Portfolio
optimal_five_factors <- ff_optimal%>% 
  filter(term != "(Intercept)")

# Alpha of the optimal portfolio
optimal_ff_alpha <- ff_optimal%>% 
  filter(term == "(Intercept)")%>% select(estimate)
optimal_ff_alpha <- round(optimal_ff_alpha[[1,1]],4)


Table2 <- data.frame(
  Risk_Measure = c("CAPM Beta","FF MKT","FF SMB","FF HML","FF RMW","FF CMA"),
  Naive_Portfolio = c(naive_capm_beta$estimate,naive_five_factors$estimate),
  Custom_Portfolio = c(custom_capm_beta$estimate,custom_five_factors$estimate),
  Optimal_Portfolio = c(optimal_capm_beta$estimate,optimal_five_factors$estimate)
)
print(Table2)


# Naive Portfolio Sharpe Ratio
naive_sharpe_xts <- 
  SharpeRatio(naive_portfolio_returns_xts, 
              Rf = rfr,
              FUN = "StdDev") %>% 
  `colnames<-`("Naive_Sharpe")

# Custom Portfolio Sharpe Ratio
custom_sharpe_xts <- 
  SharpeRatio(custom_portfolio_returns_xts, 
              Rf = rfr,
              FUN = "StdDev") %>% 
  `colnames<-`("Custom_Sharpe")

# Optimal Portfolio Sharpe Ratio
optimal_sharpe <- max_sr$SharpeRatio

#Market Sharpe
market_sharpe <- 
  SP500_monthly %>%
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  mutate(returns = (log(SP500) - log(lag(SP500)))) %>% 
  na.omit() %>% 
  summarise(ratio = mean(returns - rfr)/sd(returns - rfr))


#Relative Sharpe Ratio
naive_relative_sharpe <- round(naive_sharpe_xts[,1]/market_sharpe$ratio,4)
custom_relative_sharpe <- round(custom_sharpe_xts[,1]/market_sharpe$ratio,4)
optimal_relative_sharpe <- round(optimal_sharpe/market_sharpe$ratio,4)



Table3 <- data.frame(
  Performance_Measure = c("Sharpe Ratio", "Relative Sharpe Ratio", "CAPM_Alpha", "Fama French Alpha"),
  Naive_Portfolio = c(round(naive_sharpe_xts[,1],4), naive_relative_sharpe, naive_capm_alpha, naive_ff_alpha ),
  Custom_Portfolio = c(round(custom_sharpe_xts[,1],4),custom_relative_sharpe,custom_capm_alpha,custom_ff_alpha),
  Optimal_Portfolio = c(round(optimal_sharpe,4),optimal_relative_sharpe, optimal_capm_alpha,optimal_ff_alpha),
  Market = c(round(market_sharpe$ratio,4), 1, 0, 0)
)
print(Table3)


# Identifying Minimum Variance Portfolio
min_var <- portfolio_values[which.min(portfolio_values$Risk),]

#Visualization of Efficient Frontier
efficient_frontier <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent, limits = c(-0.03,0.03)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.03,0.2)) +
  labs(x = 'Risk',y = 'Returns', title = "Efficient Frontier") +
  geom_point(aes(x = Risk, y = Return), data = min_var, color = 'red', shape = 4, size = 5, stroke = 2) +   # minimum variance portfolio
  geom_point(aes(x = Risk,y = Return), data = data.frame(Risk = c(naive_portfolio_sd_return), Return = c(naive_portfolio_mean_returns_xts)), color = 'blue', shape = 7, size = 5, stroke = 2) + # naive
  geom_point(aes(x = Risk, y = Return), data = data.frame(Risk = custom_portfolio_sd_return, Return = custom_portfolio_mean_returns_xts), color = 'purple', shape = 7, size = 5, stroke = 2) +   # custom portfolio
  geom_point(aes(x = Risk,y = Return), data = max_sr, color = 'darkgreen', shape = 8, size = 5, stroke = 2)     # optimal portfolio
efficient_frontier
# Naive Portfolio and custom portfolios lie on the inefficient portion since there is a portfolio with same risk but higher return 
