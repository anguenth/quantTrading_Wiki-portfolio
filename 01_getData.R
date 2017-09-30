

source("utils.R")



# Download Historical Data for Funds --------------------------------------


fundsList <- read.csv("./tables/fundsList.csv", sep=",", na.strings = c(NA, ""), encoding = "UTF-8",
                      colClasses = c("numeric", "factor", rep("character", 4)))
fundsList <- fundsList[complete.cases(fundsList[ , "SecuId"]),]
#select CategoryId to work with
#fundsList <- fundsList[fundsList$CategoryId > 100 & fundsList$CategoryId < 200, ]


i=1; fund_historic_list <- list()
for (i in 1:length(fundsList$SecuId)) {
     
     fund_secu_id <- fundsList$SecuId[i]
     #fund_ISIN <- fundsList$ISIN[i]
     
     fondscheck_url <- paste0("http://www.fondscheck.de/parts/kurse/historic.csv?secu=",
                              fund_secu_id,
                              "&boerse_id=8&min_time=1.+1.+1990&max_time=",
                              lubridate::day(Sys.Date()), ".",
                              lubridate::month(Sys.Date()), ".",
                              lubridate::year(Sys.Date()),
                              "&trenner=%3B&go=CSV-Datei+erstellen")
     
     fund_con <- RCurl::getURL(fondscheck_url)
     fund_historic <- read.csv2(textConnection(fund_con), sep=";", dec=",", na.strings = c(NA, "-"), 
                                colClasses = c("Date", rep("num.with.dots", 5))
     ); colnames(fund_historic) <- c("Date", "Open", "High", "Low", "Close", "Volume")
     
     fund_historic$Open <- ifelse(is.na(fund_historic$Open), fund_historic$Close, fund_historic$Open)
     fund_historic$High <- ifelse(is.na(fund_historic$High), round(fund_historic$Close*1.001, 2), fund_historic$High) 
     fund_historic$Low <- ifelse(is.na(fund_historic$Low), round(fund_historic$Close*0.999, 2), fund_historic$Low) 
     fund_historic$Volume <- ifelse(is.na(fund_historic$Volume), 0, fund_historic$Volume) 
     
     tail(fund_historic[1:6])
     
     fund_historic <- as.xts(fund_historic[,-1], order.by = fund_historic[,"Date"])
     fund_historic <- fund_historic[!duplicated(index(fund_historic), fromLast = TRUE), ]
     
     head((fund_historic[1:6]))
     fund_historic_list[[i]] <- fund_historic
     
     Sys.sleep(5)
     i=i+1
     
}; names(fund_historic_list) <- fundsList$ISIN



# SAVE --------------------------------------------------------------------

saveRDS(fund_historic_list, file = "./data/fund_historic_list.rds")
saveRDS(fundsList, file = "./data/fundsList.rds")
