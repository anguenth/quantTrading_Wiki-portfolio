


source("utils.R")



# LOAD --------------------------------------------------------------------


fundsList <- readRDS(file = "./data/fundsList.rds")
fund_historic_list <- readRDS(file = "./data/fund_historic_list.rds")

fund_returns <- readRDS(file = "./data/fund_returns.rds")
portfolio_weights_list <- readRDS(file = "./data/portfolio_weights_list.rds")



# next --------------------------------------------------------------------

set.seed(1234)
#merge weights from all categories
portfolio_filtered <- do.call(cbind, portfolio_weights_list)

#filter fund returns with highest weights 
fund_returns_filt <- fund_returns[, colSums(portfolio_filtered) >= 0.4]
fund_returns_filt <- fund_returns_filt[rowSums(is.na(fund_returns_filt)) == 0, ]

PerformanceAnalytics::chart.CumReturns(
     fund_returns_filt,
     main = funds_category_set$Category[1],
     legend.loc = "topleft"
)


####### calculate efficient and maxDD portfolios
#efficient frontier
eff <- eff.frontier(returns=fund_returns_filt, short="no",
                    max.allocation=0.8, risk.premium.up=.5, risk.increment=.001)
#point for optimal sharpe
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),  ]
#point between optimal sharpe and maximum return = eff.optimal.return
opt.return <- (0.5*( eff[eff$sharpe==max(eff$sharpe),]$'Exp.Return' +     #return at max. sharpe
                          eff[eff$'Exp.Return'==max(eff$'Exp.Return'),]$'Exp.Return' ) )  #return at max. return
eff.optimal.return <- head( eff[eff$'Exp.Return' >= opt.return, ], 1)

#max drawdown 10%
maxdd_10 <- portfolio.MaxDD(fund_returns_filt, MaxDD=0.10, softBudget = TRUE)
# max drawdown 20%
maxdd_20 <- portfolio.MaxDD(fund_returns_filt, MaxDD=0.20, softBudget = TRUE)

meth_name <- c("Opt Sharpe", "High Return", "Max DD 10%", "Max DD 20%")
portfolio_weights <- round(
     rbind.data.frame(eff.optimal.point[, 1:(ncol(eff.optimal.point)-3)], 
                      eff.optimal.return[, 1:(ncol(eff.optimal.return)-3)],
                      maxdd_10,
                      maxdd_20)
     , 2); rownames(portfolio_weights) <- meth_name



####### plot efficient and maxDD portfolios
portfolio_tmp <- cbind.xts(
     Return.portfolio(fund_returns_filt, weights = as.numeric(portfolio_weights[1, ])),
     Return.portfolio(fund_returns_filt, weights = as.numeric(portfolio_weights[2, ])),
     Return.portfolio(fund_returns_filt, weights = as.numeric(portfolio_weights[3, ])),
     Return.portfolio(fund_returns_filt, weights = as.numeric(portfolio_weights[4, ]))
); colnames(portfolio_tmp) <- meth_name


pdf( paste0("./charts/id", first(fundsList$CategoryId)-1, "_optimal_portfolio.pdf") )
PerformanceAnalytics::charts.PerformanceSummary(
     portfolio_tmp[-1, ], #remove first line: can be wrong
     main = "Result - Final Portfolios",
     legend.loc = "topleft")
dev.off()

#select all weighted ISIN columns and reformat
portfolio_weights_final <- portfolio_weights[, colSums(portfolio_weights) > 0]
portfolio_weights_final <- cbind.data.frame("ISIN" = colnames(portfolio_weights_final),
                                            t(portfolio_weights_final))
fundsList_tmp <- fundsList[,-6]
fundsList_tmp <- fundsList_tmp[fundsList_tmp$ISIN %in% portfolio_weights_final$ISIN, ]
portfolio_weights_final_ext <- merge(fundsList_tmp, portfolio_weights_final, by = "ISIN")  


#save final portfolio
write.csv(portfolio_weights_final_ext, 
          file = paste0("./tables/id", first(fundsList$CategoryId)-1, "_portfolio_weights.csv")
          )


