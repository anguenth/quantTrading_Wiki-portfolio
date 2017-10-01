


source("utils.R")



# LOAD --------------------------------------------------------------------


wikiList <- readRDS(file = "./data/wikiList.rds")
wiki_historic_list <- readRDS(file = "./data/wiki_historic_list.rds")



# Calculate Prices and Returns --------------------------------------------

wiki_returns_list <- rapply(wiki_historic_list, dailyReturn, how = "replace")
wiki_returns <- do.call(merge.xts, wiki_returns_list) %>%
     na.fill(., list(NA, 0, 0)) %>%
     setNames(., wikiList$ISIN)

wiki_prices_list <- rapply(wiki_historic_list, function (x) {x$Cl}, how = "replace")
wiki_prices <- do.call(merge.xts, wiki_prices_list) %>%
     na.fill(., list(NA, "extend", "extend")) %>%
     setNames(., wikiList$ISIN)
     # not used yet

# Plot Return Charts by Starting Date ------------------------------------------
# Calculate Optimal Portfolio by Starting Date ---------------------------------
# Plot Optimal Portfolios by Starting Date -------------------------------------

set.seed(1234)
start_date <- c(as.Date("2015-01-02"), as.Date("2016-01-04"))
portfolio_weights_list <- list() 
for (i in 1:length(start_date)) {
     
     #select wiki by start date
     returns_set <- wiki_returns[paste0(start_date[i],"/"),]
     returns_set <- na.fill(returns_set, 0)
     #returns_set <- wiki_returns #returns_set[rowSums(is.na(returns_set)) == 0, ]
     
     ####### plot perfomance summaries
     a %<a-% PerformanceAnalytics::chart.CumReturns(
               returns_set,
               main = paste0("Wiki Returns since ", start_date[i]),
               legend.loc = NULL
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
     eff.optimal.return <- head( eff[eff$'Exp.Return' >= opt.return[1], ], 1)
     
     #max drawdown 10%
     maxdd_10 <- portfolio.MaxDD(returns_set, MaxDD=0.075, softBudget = TRUE)
     # max drawdown 20%
     maxdd_20 <- portfolio.MaxDD(returns_set, MaxDD=0.10, softBudget = TRUE)
     
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
               main = paste0("Wiki Returns since ", start_date[i]),
               legend.loc = "topleft")
     #plot equity distribution within portfolios
     c %<a-% barplot(as.matrix(portfolio_weights), legend.text = rownames(portfolio_weights), 
                     las = 2, cex.names = 0.6) 
     
     #save charts to pdf
     pdf( paste0("./charts/wiki_", start_date[i], "_charts.pdf") )
     print(a); print(b); print(c)
     dev.off()
     
}



# SAVE --------------------------------------------------------------------

saveRDS(wiki_returns, file = "./data/wiki_returns.rds")
saveRDS(portfolio_weights_list, file = "./data/portfolio_weights_list.rds")



