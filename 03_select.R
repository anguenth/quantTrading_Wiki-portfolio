


source("utils.R")


# Strategy selection and capital to invest

p <- menu(c("conservative", "moderate"), 
                 graphics=TRUE, title="Select portfolio strategy to calculate:")
strategy <- c("conservative", "moderate")[p]
capital <- c(50000, 110000)[p]



# LOAD --------------------------------------------------------------------

wikiListFull <- readRDS(file = "./data/wikiListFull.rds")
wikiList <- readRDS(file = "./data/wikiList.rds")
wiki_historic_list <- readRDS(file = "./data/wiki_historic_list.rds")

wiki_returns <- readRDS(file = "./data/wiki_returns.rds")
portfolio_weights_list <- readRDS(file = "./data/portfolio_weights_list.rds")

# load current portfolio first
curr_file <- c("./tables/curr_portfolio_conservative.csv", 
               "./tables/curr_portfolio_moderate.csv")[p]
wiki_portfolio_curr <- read.csv(curr_file, stringsAsFactors = FALSE)


# next --------------------------------------------------------------------

set.seed(1234)
#merge weights from all categories
portfolio_weights <- do.call(rbind, portfolio_weights_list)

#filter wiki returns with higher weights 
wiki_returns_filt <- wiki_returns[, colSums(portfolio_weights) >= 0.05]

#returns of selected
PerformanceAnalytics::chart.CumReturns(
     wiki_returns_filt,
     main = "Selected Wikis",
     legend.loc = "topleft"
)


if (strategy == "conservative") {
     #Strategy "High Return" perfoms very good with a maxDD of below -5% in late 2015
     #Strategy "Max DD 5%" has slightly better profit/risk ratio and helps to see which Wikis are driving the return
     portfolio_weights <- portfolio_weights[c("Opt.Sharpe", "Opt.Sharpe1", 
                                              "High.Return", "High.Return1"), ]
} else if (strategy == "moderate") {
     portfolio_weights <- portfolio_weights[c("High.Return", "High.Return1", 
                                              "MaxDD.5%", "MaxDD.5%1"), ]
}

#filter wiki returns with higher weights on selected strategies
portfolio_weights <- portfolio_weights[, colSums(portfolio_weights) > 0.03]

#ISINs remain after filtering
ISIN_select <- as.character(colnames(portfolio_weights))
#strategy name grouping by starting date
strategies <- as.character(rownames(portfolio_weights))
strategies <- grepl("1$", strategies) 

#bind and calculate median values between High Return and MaxDD portfolios
portfolio_weights_med <- t(portfolio_weights)
portfolio_weights_med <- cbind(portfolio_weights_med,
                               Median = rowMeans(portfolio_weights_med[, !strategies]),
                               Median1 = rowMeans(portfolio_weights_med[, strategies])
) 


#extend portfolio df with ISINs from current portfolio
portfolio_weights_ext <- cbind.data.frame(ISIN = ISIN_select, portfolio_weights_med,
                                          stringsAsFactors = FALSE)
portfolio_weights_ext <- merge(portfolio_weights_ext, wiki_portfolio_curr, 
                               by="ISIN", all.x = TRUE, all.y = TRUE)
portfolio_weights_ext[is.na(portfolio_weights_ext)] <- 0


#extend portfolio df with information from wikiList
#correct median weights with TraderValues
portfolio_weights_final <- merge(portfolio_weights_ext, wikiListFull, by="ISIN") %>%
     dplyr::mutate(., Median = round(Median*TraderValue, 2), 
                   Median1 = round(Median1*TraderValue, 2),
                   Target = capital*(Median+Median+Median1)/3
     )
portfolio_weights_final %<>% 
     #redistributes remaining cash on all position targets
     dplyr::mutate(., Target = Target+(capital-sum(Target, na.rm=TRUE))*Target/sum(Target, na.rm=TRUE)) %>%
     #removes all position smaller than 2,000
     dplyr::mutate(., Target = signif(ifelse(Target >= 2000, Target, 0), 2)) %>%
     dplyr::select(-Punkte, -MoneyManager, -TreueAnleger, -Aktivitaet, -`Bad.Perfolio`, -InvKapital,
                   -Liquidationskennzahl, -Evaluation, -TraderValue, -Strategy, -Symbol)

portfolio_weights_final <- portfolio_weights_final[ order(portfolio_weights_final[,'Target'],
                                                          portfolio_weights_final[,'Current'], 
                                                          decreasing = TRUE), ]
View(portfolio_weights_final)

#save final portfolio
saveRDS(portfolio_weights_final, file = paste0("./data/portfolio_weights_", strategy, ".rds"))
write.csv(portfolio_weights_final, file = paste0("./tables/portfolio_weights_", strategy, ".csv"), row.names = FALSE)



