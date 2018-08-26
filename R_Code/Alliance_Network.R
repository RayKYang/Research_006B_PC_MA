setwd("/Users/Ray_Mac/Documents/R Yang/~PhD Research Data/006Data/Data Cleaning")
suppressMessages(lapply(c("readr","data.table","xts","tidyr","dplyr","stringr","purrr","lubridate","maxLik"), require, character.only = TRUE))

## alliance network with all firms involved in the VC network ##
# Al_Network <- read.csv("RAW_Alliance_006B.csv", stringsAsFactors = FALSE)
# Al_Network$DealN0 <- na.locf(Al_Network$DealN0)
# all.equal(Al_Network$DealN0, Al_Network$DealN)
# Al_Network <- dplyr::select(Al_Network, c(1,4,5,9))

## alliance network with all firms ##
Al_Network <- read.csv("RAW_Alliance_ALL_006B.csv", stringsAsFactors = FALSE)
Al_Network <- dplyr::select(Al_Network, c(1,4,5))

# convert firm data to allinace pairs #
Al_Nw_splitted <- split(Al_Network, Al_Network$DealN)
extract.pairs <- function(test){
# test <- Al_Nw_splitted[[391]]
pairs <- t(combn(test$CusipAup, m = 2))
year.vec <- rep(unique(test$YearA), nrow(pairs))
result <- cbind(pairs, year.vec)
return(result)}
pair_year_list <- purrr::map(Al_Nw_splitted, safely(extract.pairs))
 which_is_wrong <- which(unlist(map(pair_year_list, function(x){!is_null(x$error)})) == TRUE)
 Al_Nw_splitted[[which_is_wrong]]
pair_year_list_OK <- purrr::map(pair_year_list[-which_is_wrong], function(list){list$result})
pair_year <- do.call(rbind, pair_year_list_OK) %>% as.data.frame()
pair_year$V1       <- as.character(pair_year$V1)
pair_year$V2       <- as.character(pair_year$V2)
pair_year$year.vec <- as.numeric(as.character(pair_year$year.vec))

# pad data n_year rolling #
pair_year_splitted <- split(pair_year, pair_year$year.vec)
pad_n_year <- function(test, n.year=5){
  # test <- pair_year_splitted[[1]]
  two.col   <- do.call(rbind, rep(list(test[,1:2]), n.year))
  third.col <- rep(test$year.vec[1]:(test$year.vec[1]+n.year-1), each=nrow(test))
  return(cbind(two.col, third.col))
}
pair_n_year <- do.call(rbind, purrr::map(pair_year_splitted, pad_n_year))
pair_n_year <- pair_n_year[!duplicated(pair_n_year),]

# centrality #
# a <- head(pair_year)[,1:2] # example #
# vec <- c(t(a))
# g <- igraph::make_graph(vec, directed = F)
# ( m <- igraph::as_adjacency_matrix(g, sparse = F) )
# plot(g)

pair_n_year_splitted <- split(pair_n_year, pair_n_year$third.col)
centrality <- function(test){
  # test <- pair_n_year_splitted[[26]]
  a <- test[,1:2]
  vec <- c(t(a))
  g <- igraph::make_graph(vec, directed = F) %>% igraph::simplify()
  return(data.frame(year=test$third.col[1],
                    cusipAup = names(igraph::constraint(g)),
                   eigen=igraph::eigen_centrality(g)$vector,
              constraint=igraph::constraint(g)))
}
centrality_alli_nwk <- do.call(rbind, purrr::map(pair_n_year_splitted, centrality))
centrality_alli_nwk <- centrality_alli_nwk[which(centrality_alli_nwk$year < 2016),]

setwd("/Volumes/RESEARCH_HD/006/network_data")
write.csv(centrality_alli_nwk, "centrality_alli_all_nwk.csv", row.names = FALSE)

# endorcement centrality #
centrality_alli_nwk <- read.csv("centrality_alli_all_nwk.csv", stringsAsFactors = FALSE)
names(pair_n_year) <- c("A.CUSIP", "B.CUSIP", "year")
pair_n_year <- left_join(pair_n_year, centrality_alli_nwk, by=c("A.CUSIP"="cusipAup", "year"="year"))
names(pair_n_year)[4:5] <- paste0("A.",names(pair_n_year)[4:5])
pair_n_year <- left_join(pair_n_year, centrality_alli_nwk, by=c("B.CUSIP"="cusipAup", "year"="year"))
names(pair_n_year)[6:7] <- paste0("B.",names(pair_n_year)[6:7])
pair_n_year <- pair_n_year %>% dplyr::select(1,3,4,5,2,6,7)
# use A to match B
endorsement.A <- pair_n_year %>% arrange(A.CUSIP, year) %>% group_by(A.CUSIP, year) %>% 
                 summarise(endorsement.eigen = max(B.eigen), endorsement.constraint = min(B.constraint))
names(endorsement.A)[1] <- "CUSIP"
# use B to match A
endorsement.B <- pair_n_year %>% arrange(B.CUSIP, year) %>% group_by(B.CUSIP, year) %>% 
                 summarise(endorsement.eigen = max(A.eigen), endorsement.constraint = min(A.constraint))
names(endorsement.B)[1] <- "CUSIP"
endorsement <- rbind(as.data.frame(endorsement.A), as.data.frame(endorsement.B))
write.csv(endorsement, "endorsement.centrality.csv", row.names = FALSE) 
