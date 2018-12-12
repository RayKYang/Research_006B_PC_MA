# last run date: 10.29.2018

################# # construct status # with the matched round data (rfmi comes from round_firm_MnA_IPO)
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr","purrr","lubridate","maxLik"))
setwd("/Volumes/RESEARCH_HD/006/network_data")
rfmi <- read.csv("rfmi.csv")
# step 1: add a col "nt.yr" to mark in which years a VC is in a network (like a snapshot)
all.year    <- function(df, wide=5){ #!!# THIS Function DETERMINES N (wide = N) year rolling, i.e. how large the gap is
  # al.year <- c(1992:1996, 2002:2003, 2007:2008)
    al.year <- unique(df$year) 
        gap <- al.year[-1] - al.year[-length(al.year)]
      gap.w <- which(gap>1)
      ends  <- al.year[gap.w]
      add   <- function(x){return( (x+1) : (x+ (wide-1) ))}
      added <- unlist(purrr::map(ends, add))
  added.all <- sort(unique(c(al.year, added)))
  return(added.all)
  } 
network.yr <- function(df){
  all.year <- all.year(df)
  net.year    <- function(year, all.year){ all.year[which(all.year >= year)] }
  yr.list  <- purrr::map(df$year, net.year, all.year=all.year) %>% sapply(paste0, collapse=",")
  return(yr.list)
}
# ## df for testing step 1
# df <- select(rfmi,company_name, fund.match,year) %>%
#   filter( company_name=="@Road, Inc.") %>%
#   slice( -(7:10) )

rfmi$nt.yr <- unlist(map(split(rfmi,rfmi$company_name),network.yr))  # split (by company) - apply - combine

# step 2: function only: extract networks in a year
extract.year <- function(string){
  strsplit(string, split=",") %>% unlist() %>% as.numeric
}
in.or.out <- function(nt.yr , test.year){
  test.year %in% extract.year(nt.yr)
}
network.in.a.year <- function(df, network.year) {
  selected.rows <- map_lgl(df$nt.yr, in.or.out, test.year=network.year)
  df[selected.rows,] %>% select(1:2,nt.yr)
}
## df for testing step 2
# df <- select(rfmi,company_name,fund.match,year) %>% 
#   filter(company_name=="@Outcome, Inc." | company_name=="@Motion, Inc." | company_name=="@Road, Inc.")
# df$nt.yr <- unlist(purrr::map(split(df,df$company_name),network.yr))
# network.df <- network.in.a.year(rfmi, network.year=2000)

# step 3: function only: find status in that year
find.status <- function(network.df, which.year, node="fund.match", link="company_name") {
  df <- data.frame(network.df[node], network.df[link])
  df[] <- lapply(df, as.character) # convert all cols into characters
  M <- table(df) %>% as.matrix() %>% tcrossprod()
  diag(M) <- 1
  g <- igraph::graph_from_adjacency_matrix(M, weighted=NULL, mode = "undirected") %>% igraph::simplify()
  return.df <- data.frame(year=which.year,
                          power=igraph::power_centrality(g,exponent = 0.9),
                          eigen=igraph::eigen_centrality(g)$vector,
                          constraint=igraph::constraint(g))
  return.df$fund.match <- rownames(return.df)
  return( return.df )
}
# # testing step 3
# network.in.2000 <- network.in.a.year(rfmi, network.year = 2000)
# find.status(network.in.2000,2000)

## finally (loop): get the df with status scores
get.whole <- function(rfmi) {
  final.result <- data.frame(year=0 , power=0 , eigen=0 , constraint=0, fund.match=0) # initialize
  for (yr in unique(rfmi$year)){
    new.result   <- network.in.a.year(rfmi, network.year=yr) %>% find.status(which.year=yr)
    final.result <- rbind(final.result, new.result)
  } 
  return(final.result)}
# get.whole(df)
start <- Sys.time()
status.df.VCs <- get.whole(rfmi) %>% filter(year!=0) # slow to run: 11.328 mins
Sys.time() - start
# write.csv(status.df.VCs, "Centrality.VCs.3Year.csv", row.names=FALSE) ### VC network positions by year
write.csv(status.df.VCs, "Centrality.VCs.5Year.csv", row.names=FALSE) ### VC network positions by year

## one more step after "finally": get the status for {companies}, change the args of find.status(... node=company )
get.whole <- function(df) {
  final.result <- data.frame(year=0 , power=0 , eigen=0 , constraint=0, fund.match=0) # initialize
  for (yr in unique(df$year)){
    new.result   <- network.in.a.year(df, yr) %>% find.status(which.year=yr, node="company_name", link="fund.match")
    final.result <- rbind(final.result, new.result)
  } 
  return(final.result)}
# get.whole(df)
start <- Sys.time()
status.df.company <- get.whole(rfmi) %>% filter(year!=0) # slow to run: 15.164 mins
colnames(status.df.company)[2:5] <- c("company_power","company_eigen","company_constraint","company_name")
Sys.time() - start
status.df.company <- arrange(status.df.company, company_name)
# write.csv(status.df.company, "Centrality.PCs.3Year.csv", row.names=FALSE) ### PCs' network positions by year
write.csv(status.df.company, "Centrality.PCs.5Year.csv", row.names=FALSE) ### PCs' network positions by year

# ### update rfmi_repu_social
# rolling.window <- 3
# rfmi_repu_social <- inner_join(rfmi, status.df.VCs, by=c("fund.match","year")) %>%
#   select(-nt.yr) %>%
#   left_join(status.df.company, by=c("company_name","year")) 
# if(nrow(rfmi)==nrow(rfmi_repu_social)){print("looks good so far!")}
# class(rfmi_repu_social$amt_000) <- "numeric"
# write.csv(rfmi_repu_social, paste0("rfmi_repu_social.",rolling.window,"Year.csv"), row.names = FALSE)