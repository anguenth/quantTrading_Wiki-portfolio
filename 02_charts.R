


source("utils.R")



# LOAD --------------------------------------------------------------------


fundsList <- readRDS(file = "./data/fundsList.rds")
fund_historic_list <- readRDS(file = "./data/fund_historic_list.rds")



# Calculate Prices and Returns --------------------------------------------

fund_returns_list <- rapply(fund_historic_list, dailyReturn, how = "replace")
fund_returns <- do.call(merge.xts, fund_returns_list) %>%
     na.fill(., list(NA, 0, 0)) %>%
     setNames(., fundsList$ISIN)

fund_prices_list <- rapply(fund_historic_list, function (x) {x$Close}, how = "replace")
fund_prices <- do.call(merge.xts, fund_prices_list) %>%
     na.fill(., list(NA, "extend", "extend")) %>%
     setNames(., fundsList$ISIN)
     # not used yet

# Plot Return Charts by Category ------------------------------------------
# Calculate Optimal Portfolio by Category ---------------------------------
# Plot Optimal Portfolios by Category -------------------------------------

set.seed(1234)
portfolio_weights_list <- list() 
for (i in 1:length(unique(fundsList$CategoryId))) {
     
     #select funds by category
     funds_category_set <- dplyr::filter(fundsList, CategoryId == unique(fundsList$CategoryId)[i])
     returns_set <- fund_returns[, funds_category_set$ISIN]
     returns_set <- returns_set[rowSums(is.na(returns_set)) == 0, ]
     
     ####### plot perfomance summaries
     a %<a-% PerformanceAnalytics::chart.CumReturns(
               fund_returns[, funds_category_set$ISIN],
               main = funds_category_set$Category[1],
               legend.loc = "topleft"
     )

     ####### calculate efficient and maxDD portfolios
     #efficient frontier
     eff <- eff.frontier(returns=returns_set, short="no",
                         max.allocation=0.8, risk.premium.up=.5, risk.increment=.001)
     #point for optimal sharpe
     eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),  ]
     #point between optimal sharpe and maximum return = eff.optimal.return
     opt.return <- (0.5*( eff[eff$sharpe==max(eff$sharpe),]$'Exp.Return' +     #return at max. sharpe
                               eff[eff$'Exp.Return'==max(eff$'Exp.Return'),]$'Exp.Return' ) )  #return at max. return
     eff.optimal.return <- head( eff[eff$'Exp.Return' >= opt.return, ], 1)
     
     #max drawdown 10%
     maxdd_10 <- portfolio.MaxDD(returns_set, MaxDD=0.10, softBudget = TRUE)
     # max drawdown 20%
     maxdd_20 <- portfolio.MaxDD(returns_set, MaxDD=0.20, softBudget = TRUE)
     
     meth_name <- c("Opt Sharpe", "High Return", "Max DD 10%", "Max DD 20%")
     portfolio_weights <- round(
          rbind.data.frame(eff.optimal.point[, 1:(ncol(eff.optimal.point)-3)], 
                           eff.optimal.return[, 1:(ncol(eff.optimal.return)-3)],
                           maxdd_10,
                           maxdd_20)
          , 2); rownames(portfolio_weights) <- meth_name
     
     portfolio_weights_list[[i]] <- portfolio_weights
     
     ####### plot efficient and maxDD portfolios
     portfolio_tmp <- cbind.xts(
          Return.portfolio(returns_set, weights = as.numeric(portfolio_weights[1, ])),
          Return.portfolio(returns_set, weights = as.numeric(portfolio_weights[2, ])),
          Return.portfolio(returns_set, weights = as.numeric(portfolio_weights[3, ])),
          Return.portfolio(returns_set, weights = as.numeric(portfolio_weights[4, ]))
     ); colnames(portfolio_tmp) <- meth_name
     b %<a-% PerformanceAnalytics::charts.PerformanceSummary(
               portfolio_tmp[-1, ], #remove first line: can be wrong
               main = funds_category_set$Category[1],
               legend.loc = "topleft")
     #plot equity distribution within portfolios
     c %<a-% barplot(as.matrix(portfolio_weights), legend.text = rownames(portfolio_weights), 
                     las = 2, cex.names = 0.6) 
     
     #save charts to pdf
     pdf( paste0("./charts/id", unique(fundsList$CategoryId)[i], "_charts.pdf") )
     print(a); print(b); print(c)
     dev.off()
     
}; names(portfolio_weights_list) <- unique(fundsList$CategoryId)



# SAVE --------------------------------------------------------------------

saveRDS(fund_returns, file = "./data/fund_returns.rds")
saveRDS(portfolio_weights_list, file = "./data/portfolio_weights_list.rds")



