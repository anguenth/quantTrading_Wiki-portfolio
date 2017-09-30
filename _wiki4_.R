---
     title: "wiki4"

---
     

     
# Parameters --------------------------------------------------------------     

 
library(magrittr)
library(zoo)
library(quantmod)
library(ggplot2)
#library(PerformanceAnalytics)


refreshData <- TRUE
Sys.setenv(TZ='UTC')





# Functions ---------------------------------------------------------------



getWiki <- function (wikiSymbol) {
     
     require(magrittr)
     require(xts)
     
     today <- paste0(
          strsplit(as.character(Sys.Date()), "-")[[1]][3], ".",
          strsplit(as.character(Sys.Date()), "-")[[1]][2], ".",
          strsplit(as.character(Sys.Date()), "-")[[1]][1])
     
     URL <- paste0("https://www.wikifolio.com/dynamic/de/de/invest/download?type=daily&name=", 
                   wikiSymbol, "&dateFrom=01.01.2010&dateTo=", today)
     
     tmp <- tempfile()
     download.file(URL, destfile=tmp, method="libcurl")
     histData <- read.csv2(tmp, fileEncoding=c("UCS-4-INTERNAL"), skip=5, sep=";",
                           col.names = c("Date", "Interval", "Op", "Cl", "Hi", "Lo"))
     unlink(tmp)
     
     histData$Date %<>% lubridate::dmy_hms() 
     histData <- histData[,-2] 
     histData <- histData[ , c("Date", "Op", "Hi", "Lo", "Cl")]
     histData <- as.xts(histData[, -1], order.by=histData[, 1])
     
     return(histData)
}



# ----------------------------------------------------------------------------------------------
#
# Download data from Wikifolio
#
# ----------------------------------------------------------------------------------------------

if (refreshData = TRUE) {
     
     #read list containing Wikifolios (Name, ISIN, Symbol)
     setwd("/Users/home/Documents/Finanzen/R.investment/portfolio/wiki4_dach/")
     wiki4List <- read.csv2("./wiki4_wikiList.csv", 
                            sep=",", colClasses = c(rep("character", 3)))
     
     #create short Wiki symbols for downloading
     wiki4List$Symbol.short <- sapply(strsplit(wiki4List$Symbol, "WF[0]{0,4}"), "[", 2)
     
     #the only one where 0 remains in symbol name
     wiki4List$Symbol.short[grep("SWING", wiki4List$Symbol.short)] <- "0SWING"
     
     #download and save
     histDataList <- lapply(wiki4List$Symbol.short, getWiki)
     names(histDataList) <- wiki4List$Symbol
     
     saveRDS(histDataList, file = paste0("./#rawData/", Sys.Date(), "_", "histDataList.rds"))
}




# Data loading ------------------------------------------------------------


# load historical data when data was not refreshed by downloading

setwd("/Users/home/Documents/Finanzen/R.investment/portfolio/wiki4_dach/")
histDataList <- readRDS(file = paste0("./#rawData/", tail(dir("./#rawData/"), 1)))    




# Statistics --------------------------------------------------------------


i=1; wikiStats_ls <- list()

for (i in 1:length(histDataList)) {
     
     wikiReturn <- quantmod::dailyReturn(histDataList[[i]]$Cl)
     
     wikiStats_tmp <- rbind.data.frame(
          annReturn = PerformanceAnalytics::table.AnnualizedReturns(wikiReturn)[1, ],
          annSharpe = PerformanceAnalytics::table.AnnualizedReturns(wikiReturn)[3, ],
          drawdown = PerformanceAnalytics::maxDrawdown(wikiReturn), 
          sortino = PerformanceAnalytics::SortinoRatio(wikiReturn),                #sortino ratio : risk-adjusted return
          sterling = PerformanceAnalytics::table.DrawdownsRatio(wikiReturn)[1, ],  #sterling ratio : risk-adjusted return
          ulcer = PerformanceAnalytics::UlcerIndex(wikiReturn)                     #ulcer index : risk measure
     ) 
     
     #wikiStats_tmp <- as.data.frame(base::t(wikiStats_tmp))
     #wikiStats_tmp <- apply(wikiStats_tmp, 1, function (x) {round(x, digits=2)})
     #names(wikiStats_tmp) <- names(histDataList)[i]
     
     
     wikiStats_ls[[i]] <- apply(wikiStats_tmp, 1, function (x) {round(x, digits=2)})#wikiStats_tmp
     
}

wikiStats <- do.call(rbind, wikiStats_ls) %>%
     as.data.frame() 
rownames(wikiStats) <- names(histDataList)



# some clustering / plotting -----------------------------------------------------------
# using descriptive values from wikiStats

set.seed(1234)
hc <- hclust(dist(wikiStats))
plot(hc)

mycl <- cutree(hc, h=max(hc$height/3.5))
(mycl <- mycl[order(mycl)])


plot.Cluster <- function (histPricesList, cluster) {
     
     ####select subset 
     #get wiki names from selected cluster
     clSubset <- names(mycl[mycl==cluster]) #cluster 1
     cond <- names(histPricesList) %in% clSubset
     #get close prices using wiki names from list of xts objects containing Op,Hi,Lo,Cl
     histPricesClose <- histPricesList[names(histPricesList) %in% clSubset] %>%
          lapply(., function(x) (x$Cl))
     
     #change format
     histPrices <- do.call(merge.xts, histPricesClose)
     colnames(histPrices) <- names(histPricesClose)
     
     df <- as.data.frame(histPrices['2014-01-01::']) 
     df$date <- as.Date(rownames(df))
     df <- reshape2::melt(df, id.vars = c("date"))
     #df <- df[!is.na(df$value), ]
     
     library(RColorBrewer)
     n <- 60
     qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
     col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
     
     
     p <- ggplot(df, aes(x = date, y = value, color = variable)) +
          geom_line() +
          scale_color_manual(name = "Wiki Symbol", values = col_vector) +
          # Miscellaneous Formatting
          theme_bw() + ggtitle("Wiki Price Developments") +
          xlab("Date") + ylab("Price") 

     print(p)
}


plot.Cluster(histDataList, 1)
plot.Cluster(histDataList, 2)
plot.Cluster(histDataList, 3)
plot.Cluster(histDataList, 4)
plot.Cluster(histDataList, 5)
plot.Cluster(histDataList, 6)



# Eff Portfolio -----------------------------------------------------------

#calculate efficient portfolio for clusters 1-3
j=1; effPortfolio_lst <- list()
for (j in 1:3) {

     clSubset <- names(mycl[mycl==j]) #cluster j
     cond <- names(histPricesList) %in% clSubset
     histPricesClose <- histPricesList[names(histPricesList) %in% clSubset] %>%
          lapply(., function(x) (x$Cl))
     
     #change format
     histPrices <- do.call(merge.xts, histPricesClose)
     colnames(histPrices) <- names(histPricesClose)
     
     histReturns <- TTR::ROC(histPrices['2014-01-01::'], n=3, type = "discrete", na.pad = TRUE)
     colnames(histReturns) <- names(histPrices)
     histReturns <- na.fill(histReturns, fill=0)
     
     PerformanceAnalytics::chart.CumReturns(histReturns[,2])
     
     #efficient frontier
     eff <- eff.frontier(returns=histReturns, short="no", max.allocation=0.4, risk.premium.up=.5, risk.increment=.001)
     
     #point for optimal sharpe
     eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
     t(round( eff[eff$sharpe==max(eff$sharpe),], 2))
     
     
     #point between optimal sharpe and maximum return = eff.optimal.return
     opt.return <- (0.5*( eff[eff$sharpe==max(eff$sharpe),]$'Exp.Return' +     #return at max. sharpe
                               eff[eff$'Exp.Return'==max(eff$'Exp.Return'),]$'Exp.Return' ) )  #return at max. return
     
     eff.optimal.return <- head( eff[eff$'Exp.Return' >= opt.return, ], 1)
     t(round(eff.optimal.return, 2))
     
     
     #point between optimal sharpe and the optimal sharpe/max return
     
     eff.opt.return.med <- tail( eff[eff$sharpe>=(0.5*(eff.optimal.return$sharpe + eff.optimal.point$sharpe)), ], 1)
     t(round(eff.opt.return.med, 2))
     
     
     #max drawdown limited portfolio
     library(FRAPO)
     histPrices <- na.locf(histPrices)
     histPrices <- na.fill(histPrices, fill=100)
     maxdd_05 <- t(round(PMaxDD(histPrices, MaxDD = 0.15, softBudget = TRUE)@weights, 2))
     
     x <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(maxdd_05))
     PerformanceAnalytics::charts.PerformanceSummary(x)
     
     
     
     opt.point <- t(round( eff[eff$sharpe==max(eff$sharpe),], 2))
     opt.point <- head(opt.point, -3)
     y <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(opt.point))
     PerformanceAnalytics::charts.PerformanceSummary(y)
     
     
     opt.return <- t(round(eff.optimal.return, 2))
     opt.return <- head(opt.return, -3)
     z <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(opt.return))
     PerformanceAnalytics::charts.PerformanceSummary(z)
     
     opt.return.med <- t(round(eff.opt.return.med, 2))
     opt.return.med <- head(opt.return.med, -3)
     w <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(opt.return.med))
     PerformanceAnalytics::charts.PerformanceSummary(w)
     
     
     effPortfolio_lst[[j]] <- cbind(t(maxdd_05), opt.return, opt.return.med, opt.point) 

}

effPortfolioSelect <- do.call(rbind, effPortfolio_lst) %>%
                         rowSums(.)


effWiki <- base::c(names(effPortfolioSelect[effPortfolioSelect >0]),      #wikis which were considered in the eff portfolio
               names(mycl[mycl>=4]))                                        #wikis in cluster 4-6


#   -----------------------------------------------------------------------

#efficient portfolio with selected wikis
# (1) using effWiki

histPricesCl <- histPricesList[names(histPricesList) %in% effWiki] %>%
     lapply(., function(x) (x$Cl))

#change format
histPrices <- do.call(merge.xts, histPricesCl)
colnames(histPrices) <- names(histPricesCl)

histReturns <- TTR::ROC(histPrices['2014-01-01::'], n=3, type = "discrete", na.pad = TRUE)
colnames(histReturns) <- names(histPrices)
histReturns <- na.fill(histReturns, fill=0)

PerformanceAnalytics::chart.CumReturns(histReturns[,2])

#efficient frontier
eff <- eff.frontier(returns=histReturns, short="no", max.allocation=0.6, risk.premium.up=.5, risk.increment=.001)

#point for optimal sharpe
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
t(round( eff[eff$sharpe==max(eff$sharpe),], 2))


#point between optimal sharpe and maximum return = eff.optimal.return
opt.return <- (0.5*( eff[eff$sharpe==max(eff$sharpe),]$'Exp.Return' +     #return at max. sharpe
                          eff[eff$'Exp.Return'==max(eff$'Exp.Return'),]$'Exp.Return' ) )  #return at max. return

eff.optimal.return <- head( eff[eff$'Exp.Return' >= opt.return, ], 1)
t(round(eff.optimal.return, 2))


#point between optimal sharpe and the optimal sharpe/max return

eff.opt.return.med <- tail( eff[eff$sharpe>=(0.5*(eff.optimal.return$sharpe + eff.optimal.point$sharpe)), ], 1)
t(round(eff.opt.return.med, 2))


#max drawdown limited portfolio
library(FRAPO)
histPrices <- na.locf(histPrices)
histPrices <- na.fill(histPrices, fill=100)
maxdd_05 <- t(round(PMaxDD(histPrices, MaxDD = 0.09, softBudget = TRUE)@weights, 2))

x <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(maxdd_05))
PerformanceAnalytics::charts.PerformanceSummary(x)



opt.point <- t(round( eff[eff$sharpe==max(eff$sharpe),], 2))
opt.point <- head(opt.point, -3)
y <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(opt.point))
PerformanceAnalytics::charts.PerformanceSummary(y)


opt.return <- t(round(eff.optimal.return, 2))
opt.return <- head(opt.return, -3)
z <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(opt.return))
PerformanceAnalytics::charts.PerformanceSummary(z)

opt.return.med <- t(round(eff.opt.return.med, 2))
opt.return.med <- head(opt.return.med, -3)
w <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(opt.return.med))
PerformanceAnalytics::charts.PerformanceSummary(w)


effPortfolio <- cbind(t(maxdd_05), opt.return, opt.return.med, opt.point) 

effPortfolio.shrt <- data.frame(percent = row_means(effPortfolio[,c(1:2)])) %>%
                         dplyr::mutate(., rownames=rownames(.)) %>%
                         dplyr::filter(., percent > 0) %>%
                         dplyr::arrange(., desc(percent))

effPortfolio.shrt

#######################################
# (2) using selectWiki

selectWiki <- c("WF0GOLDTDG", "WFDMTRADES", "WFTZ222222", "WF00028476", "WF00ABACUS",  "WF0TSAMPEL", 
                "WF0PS02QVM", "WF23072013")

histPricesCl <- histPricesList[names(histPricesList) %in% selectWiki] %>%
     lapply(., function(x) (x$Cl))

#change format
histPrices <- do.call(merge.xts, histPricesCl)
colnames(histPrices) <- names(histPricesCl)

histReturns <- TTR::ROC(histPrices['2014-01-01::'], n=3, type = "discrete", na.pad = TRUE)
colnames(histReturns) <- names(histPrices)
histReturns <- na.fill(histReturns, fill=0)

PerformanceAnalytics::chart.CumReturns(histReturns[,2])

#efficient frontier
eff <- eff.frontier(returns=histReturns, short="no", max.allocation=0.7, risk.premium.up=.5, risk.increment=.001)

#point for optimal sharpe
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
t(round( eff[eff$sharpe==max(eff$sharpe),], 2))


#point between optimal sharpe and maximum return = eff.optimal.return
opt.return <- (0.5*( eff[eff$sharpe==max(eff$sharpe),]$'Exp.Return' +     #return at max. sharpe
                          eff[eff$'Exp.Return'==max(eff$'Exp.Return'),]$'Exp.Return' ) )  #return at max. return

eff.optimal.return <- head( eff[eff$'Exp.Return' >= opt.return, ], 1)
t(round(eff.optimal.return, 2))


#point between optimal sharpe and the optimal sharpe/max return

eff.opt.return.med <- tail( eff[eff$sharpe>=(0.5*(eff.optimal.return$sharpe + eff.optimal.point$sharpe)), ], 1)
t(round(eff.opt.return.med, 2))


#max drawdown limited portfolio
library(FRAPO)
histPrices <- na.locf(histPrices)
histPrices <- na.fill(histPrices, fill=100)
maxdd_05 <- t(round(PMaxDD(histPrices, MaxDD = 0.08, softBudget = TRUE)@weights, 2))

x <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(maxdd_05))
PerformanceAnalytics::charts.PerformanceSummary(x)



opt.point <- t(round( eff[eff$sharpe==max(eff$sharpe),], 2))
opt.point <- head(opt.point, -3)
y <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(opt.point))
PerformanceAnalytics::charts.PerformanceSummary(y)


opt.return <- t(round(eff.optimal.return, 2))
opt.return <- head(opt.return, -3)
z <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(opt.return))
PerformanceAnalytics::charts.PerformanceSummary(z)

opt.return.med <- t(round(eff.opt.return.med, 2))
opt.return.med <- head(opt.return.med, -3)
w <- PerformanceAnalytics::Return.portfolio(histReturns, weights = as.vector(opt.return.med))
PerformanceAnalytics::charts.PerformanceSummary(w)


effPortfolio <- cbind(t(maxdd_05), opt.return, opt.return.med, opt.point) 

effPortfolio.shrt <- data.frame(percent = row_means(effPortfolio[,c(1,3)])) %>%
     dplyr::mutate(., rownames=rownames(.)) %>%
     dplyr::filter(., percent > 0) %>%
     dplyr::arrange(., desc(percent))

effPortfolio.shrt






##...we are here









######
chart.CumReturns(wikiReturn)
chart.RiskReturnScatter(wikiReturn)
charts.PerformanceSummary(wikiReturn)
?chart.Scatter




dat <- data.frame(x=runif(10),y=runif(10),grp = rep(LETTERS[1:5],each = 2),stringsAsFactors = TRUE)

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(dat$grp)
colScale <- scale_colour_manual(name = "grp",values = myColors)





# Efficient Portfolio -----------------------------------------------------



#calculate efficient maximal return composition for each portfolio group

k <- 1; eff.alloc.lst <- list(); eff.maxdd.lst <- list()

while (k <= length(return_lst)) {
     
     
     #efficient portfolio at high return
     eff <- eff.frontier(returns=return_lst[[k]], short="no", max.allocation=0.2, 
                         risk.premium.up=.5, risk.increment=.001)
     
     #eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
     #t(round( eff[eff$sharpe == max(eff$sharpe),], 2))
     
     opt.return <- (0.5*( eff[eff$sharpe==max(eff$sharpe),]$'Exp.Return' +     #return at max. sharpe
                               eff[eff$'Exp.Return'==max(eff$'Exp.Return'),]$'Exp.Return' ) )  #return at max. return
     
     eff.optimal.return <- head( eff[eff$'Exp.Return' >= opt.return, ], 1)
     eff.alloc.lst[[k]] <- head( t(round(eff.optimal.return, 2)), -3)
     
     #conditional drawdown at risk constraint
     eff.maxdd.lst[[k]] <- t(t(round(PMaxDD(price_lst[[k]], MaxDD = 0.18, softBudget = TRUE)@weights, 2)))
     
     k=k+1
}   

eff.alloc.1 <- as.matrix(do.call(rbind, eff.alloc.lst))
tmp <- gregexpr("[0-9]+", rownames(eff.alloc.1))
eff.alloc.1 <- cbind( as.numeric(unique(unlist(regmatches(rownames(eff.alloc.1), tmp)))), eff.alloc.1)
colnames(eff.alloc.1) <- c("id", "eff")

eff.alloc.2 <- as.matrix(do.call(rbind, eff.maxdd.lst))
tmp <- gregexpr("[0-9]+", rownames(eff.alloc.2))
eff.alloc.2 <- cbind( as.numeric(unique(unlist(regmatches(rownames(eff.alloc.2), tmp)))), eff.alloc.2)
colnames(eff.alloc.2) <- c("id", "maxDD")

eff.alloc <- merge(as.data.frame(eff.alloc.1), as.data.frame(eff.alloc.2), by="id")


#add efficient allocation to wiki2_data
data_wiki2 <- merge(data_wiki2, eff.alloc, by="id")
#save complete wiki2_data
write.csv(data_wiki2, file = paste("./#files/", Sys.Date(), "_", "data_wiki2.csv", sep=""), row.names = FALSE)     





# Leftovers ---------------------------------------------------------------




#Use iconvlist to get all possible encodings:
codepages <- setNames(iconvlist(), iconvlist())
#read data using each of them  
lst <- lapply(codepages, function(enc) try(read.csv2("tmp.csv", skip=20,
                                                     fileEncoding=enc)))
lst <- lapply(codepages, function(enc) try(readLines("tmp.csv",
                                                     encoding=enc)))

unique(do.call(rbind, sapply(lst, dim)))

#try to guess encoding
readr::guess_encoding("tmp.csv", n_max = 1000)


```


