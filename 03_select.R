


source("utils.R")



# LOAD --------------------------------------------------------------------


wikiList <- readRDS(file = "./data/wikiList.rds")
wiki_historic_list <- readRDS(file = "./data/wiki_historic_list.rds")

wiki_returns <- readRDS(file = "./data/wiki_returns.rds")
portfolio_weights_list <- readRDS(file = "./data/portfolio_weights_list.rds")

# load current portfolio first
wiki_portfolio_curr <- read.csv("./tables/wiki_portfolio_current.csv", stringsAsFactors = FALSE)


# next --------------------------------------------------------------------

set.seed(1234)
#merge weights from all categories
portfolio_weights <- do.call(rbind, portfolio_weights_list)

#filter wiki returns with highest weights 
wiki_returns_filt <- wiki_returns[, colSums(portfolio_weights) >= 0.05]
#wiki_returns_filt <- wiki_returns_filt[rowSums(is.na(wiki_returns_filt)) == 0, ]

#returns of selected wikis
PerformanceAnalytics::chart.CumReturns(
     wiki_returns_filt,
     main = "Selected Wikis",
     legend.loc = "topleft"
)



#Strategy "High Return" perfoms very good with a maxDD of below -5% in late 2015
#Strategy "Max DD 5%" has slightly better profit/risk ratio and helps to see which Wikis are driving the return


portfolio_weights <- portfolio_weights[c("High.Return", "High.Return1", 
                                          "MaxDD.5%", "MaxDD.5%1"), ]
portfolio_weights <- portfolio_weights[, colSums(portfolio_weights) > 0.03]

#ISINs remain after filtering
ISIN_select <- as.character(colnames(portfolio_weights))

#bind and calculate median values between High Return and MaxDD portfolios
portfolio_weights_med <- t(portfolio_weights)
portfolio_weights_med <- cbind(portfolio_weights_med,
                               Median = rowMeans(portfolio_weights_med[,c('High.Return', 'MaxDD.5%')]),
                               Median1 = rowMeans(portfolio_weights_med[,c('High.Return1', 'MaxDD.5%1')])
) 


#extend portfolio df with ISINs from current portfolio
portfolio_weights_ext <- cbind.data.frame(ISIN = ISIN_select, portfolio_weights_med,
                                          stringsAsFactors = FALSE)
portfolio_weights_ext <- merge(portfolio_weights_ext, wiki_portfolio_curr, 
                               by="ISIN", all.x = TRUE, all.y = TRUE)
portfolio_weights_ext[is.na(portfolio_weights_ext)] <- 0


#extend portfolio df with information from wikiList
#correct median weights with TraderValues
portfolio_weights_final <- merge(portfolio_weights_ext, wikiList, by="ISIN") %>%
     dplyr::mutate(., Median = round(Median*TraderValue, 2), 
                   Median1 = round(Median1*TraderValue, 2)) %>%
     dplyr::select(Name, ISIN, High.Return, High.Return1, `MaxDD.5%`, `MaxDD.5%1`, Median, Median1, Current) 

portfolio_weights_final <- portfolio_weights_final[order(portfolio_weights_final[,'Median1'], decreasing=TRUE), ]



#save final portfolio
saveRDS(portfolio_weights_final, file = "./data/portfolio_weights_final.rds")
write.csv(portfolio_weights_final, file = "./tables/wiki_portfolio_weights.csv")



