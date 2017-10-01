


source("utils.R")



# LOAD --------------------------------------------------------------------


wikiList <- readRDS(file = "./data/wikiList.rds")
wiki_historic_list <- readRDS(file = "./data/wiki_historic_list.rds")

wiki_returns <- readRDS(file = "./data/wiki_returns.rds")
portfolio_weights_list <- readRDS(file = "./data/portfolio_weights_list.rds")



# next --------------------------------------------------------------------

set.seed(1234)
#merge weights from all categories
portfolio_filtered <- do.call(rbind, portfolio_weights_list)

#filter wiki returns with highest weights 
wiki_returns_filt <- wiki_returns[, colSums(portfolio_filtered) >= 0.4]
wiki_returns_filt <- wiki_returns_filt[rowSums(is.na(wiki_returns_filt)) == 0, ]

PerformanceAnalytics::chart.CumReturns(
     wiki_returns_filt,
     main = "Selected Wikis",
     legend.loc = "topleft"
)

#Strategy "High Return" perfoms very good with a maxDD of -8% in late 2015

portfolio_weights <- portfolio_filtered[c("High Return", "High Return1"), ]
portfolio_weights <- portfolio_weights[, colSums(portfolio_weights) > 0]
ISIN_select <- as.character(colnames(portfolio_weights)) 
portfolio_weights <- cbind.data.frame(ISIN = ISIN_select, t(portfolio_weights),
                                      stringsAsFactors = FALSE
                                      )
portfolio_weights_ext <- dplyr::filter(wikiList, ISIN %in% portfolio_weights$ISIN)
portfolio_weights_ext <- portfolio_weights_ext[, c("Name", "ISIN", "MoneyManager")]
portfolio_weights_final_ext <- dplyr::inner_join(portfolio_weights, portfolio_weights_ext, by = "ISIN")

#save final portfolio
write.csv(portfolio_weights_final_ext, 
          file = paste0("./tables/wiki_portfolio_weights.csv")
          )


