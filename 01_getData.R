


source("utils.R")



#keep favourites only / TraderValue complete cases
favsOnly <- TRUE
#keep good money manager only
moneyOnly <- TRUE
#keep active manager only
activeOnly <- TRUE


# Download Historical Data for Wikis --------------------------------------

     
#read list containing Wikifolios (Name, ISIN, Symbol)
wikiList <- read.csv("./tables/wikiList.csv", sep=",", 
                       colClasses = c(rep("character", 3), rep("numeric", 9), rep("character", 1)))

if (favsOnly == TRUE) {
     wikiList <- wikiList[complete.cases(wikiList[, "TraderValue"]), ]
}

if (moneyOnly == TRUE) {
     wikiList <- wikiList[wikiList$MoneyManager == 1, ]
}

if (activeOnly == TRUE) {
     wikiList <- wikiList[wikiList$Aktivitaet == 1, ]
}

#create short Wiki symbols for downloading
wikiList$Symbol.short <- sapply(strsplit(wikiList$Symbol, "WF[0]{0,4}"), "[", 2)

#download and save
wiki_historic_list <- lapply(wikiList$Symbol.short, getWiki)
names(wiki_historic_list) <- wikiList$ISIN




# SAVE --------------------------------------------------------------------

saveRDS(wikiList,
        file = paste0("./data/wikiList.rds"))
saveRDS(wiki_historic_list,
        file = paste0("./data/wiki_historic_list.rds"))

     