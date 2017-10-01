

source("utils.R")



#refresh only favourites
favsOnly <- TRUE

# Download Historical Data for Wikis --------------------------------------

     
#read list containing Wikifolios (Name, ISIN, Symbol)
wikiList <- read.csv("./tables/wikiList.csv", sep=",", 
                       colClasses = c(rep("character", 3), rep("numeric", 5)))

if (favsOnly == TRUE) {
     wikiList <- wikiList[complete.cases(wikiList[, "Punkte"]), ]
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
     