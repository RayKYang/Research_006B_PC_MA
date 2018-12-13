# last run: 12.10.2018

# setwd("/Users/Ray_Mac/Documents/R Yang/~PhD Research Data/006Data/Data Cleaning")
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr","purrr","lubridate","maxLik"))

##### Acquisition #####
# AQ <- read.csv("RAW_AQ_006B.csv", stringsAsFactors = FALSE)
#  length(unique(AQ$CusipAup))
# AQ$year <- year(mdy(AQ$Date_Ann))
#  hist(AQ$year)
# AQ <- AQ[-which(AQ$Form == "Acq. Part. Int." | AQ$Form == "Buyback" | AQ$Form == "Acq. Rem. Int." | AQ$Form == "Recapitalization"),]
# AQ$type <- ifelse(AQ$Form == "Acq. of Assets" | AQ$Form == "Acq. Cert. Asts" | AQ$Form == "Acq. Cert. Asts.", "ASSETS","EQUITY")
# AQ <- AQ[which(AQ$Acquiror_Nation == "United States"),]
# AQ <- AQ[which(AQ$Completed_or == "Completed"),]

#  AQ_count_general <- group_by(AQ, CusipAup, year) %>% summarise( n.acq.overall= n())
#  AQ_count_by.type <- group_by(AQ, CusipAup, year, type) %>% summarise( Acq.= n()) %>% spread(key=type, value=Acq.)
#  names(AQ_count_by.type)[3:4] <- c("n.acq.ASSETS", "n.acq.EQUITY")
#  AQ_count_by.type[is.na(AQ_count_by.type)] <- 0
#  AQ_count <- merge(AQ_count_general,AQ_count_by.type,c("CusipAup","year"))
#   ifelse(length(unique(AQ$CusipAup))==length(unique(AQ_count$CusipAup)),"Looks Good!","Check!")

 ### Alliance ###
# JV <- read.csv("RAW_JV_006B.csv", stringsAsFactors = FALSE)
#  length(unique(JV$CusipAup)) 
#  hist(JV$YearA)
# JV <- JV[which(JV$Status=="Completed/Signed"),]
# JV$type <- ifelse(JV$JV_flag == "Yes","JV","Alliance")
 
#  JV_count_general <- group_by(JV, CusipAup, YearA) %>% summarise( n.ali.overall= n())
#  JV_count_by.type <- group_by(JV, CusipAup, YearA, type) %>% summarise( Ali.= n()) %>% spread(key=type, value=Ali.)
#  names(JV_count_by.type)[3:4] <- c("n.ali.Alliance", "n.ali.JV")
#  JV_count_by.type[is.na(JV_count_by.type)] <- 0
#  JV_count <- merge(JV_count_general,JV_count_by.type,c("CusipAup","YearA"))
#   ifelse(length(unique(JV$CusipAup))==length(unique(JV_count$CusipAup)),"Looks Good!","Check!")
# put together #
# AQ_AL_Count <- full_join(AQ_count,JV_count,by=c("CusipAup"="CusipAup","year"="YearA")) %>% 
#               select(1,2,3,6,4,5,7,8) %>%
#               arrange(CusipAup, year)
# AQ_AL_Count[is.na(AQ_AL_Count)] <- 0
# write.csv(AQ_AL_Count,"AQ_AL_Count.csv",row.names = FALSE)
##################### Done: counting acq. and ali. Go to RESEARCH_HD to read in ##############################

setwd("/Volumes/RESEARCH_HD/006/network_data")
 AQ_AL_Count <- read.csv("AQ_AL_Count.csv") ## read directly ## [simple count by year]
 AQ_AL_Count <- arrange(AQ_AL_Count,CusipAup,year)
 
# ##### contruct AQ_AL rolling window data #####
#  # fuction for one data.block ##
#  prep.AA_rolling <- function(df){
#    ### functions for preparing data.block ###
#    sum_in_a_window <- function(nth, var.name, rolling=5){
#      selected.row <- (df$year <= df$year[nth] & df$year > (df$year-rolling)[nth])
#      selected.df <- df[selected.row,]
#      return(sum(selected.df[,var.name]))
#    }
#    df$rolling.n.acq.overall  <- sapply(seq_along(df$year), sum_in_a_window, var.name="n.acq.overall")
#    df$rolling.n.ali.overall  <- sapply(seq_along(df$year), sum_in_a_window, var.name="n.ali.overall")
#    df$rolling.n.acq.ASSETS   <- sapply(seq_along(df$year), sum_in_a_window, var.name="n.acq.ASSETS")
#    df$rolling.n.acq.EQUITY   <- sapply(seq_along(df$year), sum_in_a_window, var.name="n.acq.EQUITY")
#    df$rolling.n.ali.Alliance <- sapply(seq_along(df$year), sum_in_a_window, var.name="n.ali.Alliance")
#    df$rolling.n.ali.JV       <- sapply(seq_along(df$year), sum_in_a_window, var.name="n.ali.JV")
#    data.block <- as.data.frame(df)
#    return(data.block)
#  }
#  # test <- split(AQ_AL_Count, AQ_AL_Count$CusipAup)[[2]]
#  # prep.AA_rolling(test)
#  AA_rolling <- do.call(rbind, purrr::map(split(AQ_AL_Count, AQ_AL_Count$CusipAup), prep.AA_rolling))
#  write.csv(AA_rolling, "AA_rolling.csv", row.names = FALSE)
 
##### read in AQ_AL rolling window data #####
 AA_rolling <- read.csv("AA_rolling.csv", stringsAsFactors = FALSE) %>% dplyr::select(-c(3:8)) 
### read in VC capability data (rolling window) ###
 VC_capa.rolling <- read.csv("VC_capa.rolling.csv", stringsAsFactors = FALSE)[,-2]
### read in VC network centrality data (the gap years are alreading padded) ###
 VC_network <- read.csv("Centrality.VCs.5Year.csv", stringsAsFactors = FALSE) %>% arrange(fund.match, year) # 12.10.2018 [change ...3Year.csv to ...5Year.csv]
### read in PC network centrality data (the gap years are alreading padded) ###
 PC_network <- read.csv("Centrality.PCs.5Year.csv", stringsAsFactors = FALSE) %>% dplyr::select(-c(2)) # 12.10.2018 [change ...3Year.csv to ...5Year.csv]
    
 ### company_year row data w/ round ###
 rfmi_repu_social <- fread("rfmi_repu_social.5Year.csv") ## unit obs = company-VC-year # 12.10.2018 [change ...3Year.csv to ...5Year.csv]
 # rfmi_repu_social <- fread("rfmi_repu_social.18April.csv") # this contains [VC's success rates by distinct PCs]
 rfmi_repu_social$date <- lubridate::mdy(rfmi_repu_social$date)
 rfmi_repu_social <- arrange(rfmi_repu_social, company_name, year) %>% group_by(company_name) %>%
   mutate(round=dense_rank(year)) ## add round number ##
 ### pad data suggested by Jun Xia ###
 rfmi.split.by.PC <- split(rfmi_repu_social, rfmi_repu_social$company_name)
 # test 
 # df <- rfmi.split.by.PC[[44]]
 pad.data    <- function(df, wide=2){ # wide: N-1 year rolling # #!!# THIS Function DETERMINES THE DIFFERENCE from 006A Cleaning 
   df <- arrange(df, company_name, year, fund.match)
   al.year <- sort(unique(df$year)) # al.year <- c(1992:1996, 2002:2003, 2007:2008)
   gap <- al.year[-1] - al.year[-length(al.year)]
   gap.w <- which(gap>1)
   ends  <- al.year[gap.w]
   add   <- function(x){return( (x+1) : (x+ (wide-1) ))}
   added <- unlist(purrr::map(ends, add))
   pad.year <- added[!added %in% al.year]
   single.pad.material <- function(nth){
     block <- df[which(df$year == pad.year[nth]-1),]
     block$year  <- pad.year[nth]
     block$round <- block$round+0.5 ### mark padding
     return(block)
   }
   all.pad <- do.call(rbind, purrr::map(seq_along(pad.year), single.pad.material))
   added.df <- rbind(all.pad, df) %>% arrange(year, fund.match)
   return(added.df)
 } 
 rfmi_repu_social <- do.call(rbind, purrr::map(rfmi.split.by.PC, pad.data))
 # i <- 1
 # rfmi_repu_social[which(rfmi_repu_social$company_name == unique(rfmi_repu_social$company_name)[i]),]
 # i <- i+1
 ### adjust event variable for survival analysis ###
 adjust.Event <- function(df){
   # df <- rfmi_repu_social[which(rfmi_repu_social$company_name == unique(rfmi_repu_social$company_name)[37]),]
   first.start.year <- unique(df$year)[1] - 1
   start.year.vec <- unique(df$year)[-length(unique(df$year))]
   rep.times <- as.vector(table(df$year))[-1]
   df$start.time <- c(rep(first.start.year,as.vector(table(df$year))[1]),unlist(map2(start.year.vec,rep.times,rep)))
   df$end.time <- df$year
   df$MnA[1:(nrow(df)-1)] <- 0
   df$IPO[1:(nrow(df)-1)] <- 0
   df$event <- df$MnA + 2*df$IPO
   df$event <- factor(df$event, 0:2, labels=c("censor", "MnA", "IPO"))
   return(df)
 }
 rfmi_repu_social <- do.call(rbind,map(split(rfmi_repu_social,rfmi_repu_social$company_name),adjust.Event))
 ### merge with VC ###
 VC_network <- VC_network %>% dplyr::select(-power)
 VC_capa_r <- mutate(VC_capa.rolling, i_exit_tl=rolling.tl.IPOs, 
                                      m_exit_tl=rolling.tl.MnAs,
                                      exit_success=rolling.tl.IPOs/rolling.tl.rounds, 
                                      i_exit_success=rolling.tl.MnAs/rolling.tl.rounds,
                                      m_exit_success=rolling.tl.IPOs/rolling.tl.rounds) %>% dplyr::select(c(1,2,8:12))
 VC_nc <- merge(VC_network, VC_capa_r, by=c("year","fund.match")) %>% arrange(fund.match, year)
 rfmi_repu_social <- merge(rfmi_repu_social, VC_nc, by=c("year","fund.match"))
 ### add cumulative "match vs mismatch" overall ###
 add.match.dis.match <- function(df){
   test1 <- mutate(df, max.ownshp_cumltv = cummax(amt_000),  max.status_cumltv = cummax(eigen), max.i_exit_cumltv = cummax(i_exit_tl), max.m_exit_cumltv = cummax(m_exit_tl)) 
   #steps# 1st: identify cum max in each round, 2nd: mark if VC is the lead.xx in each row, 3rd: aggregate to round (match: at least 1 matched) 
   test2 <- group_by(test1, round) %>% mutate(max.ownshp_cumltv = max(max.ownshp_cumltv),  max.status_cumltv = max(max.status_cumltv), max.i_exit_cumltv = max(max.i_exit_cumltv),  max.m_exit_cumltv = max(max.m_exit_cumltv)) %>% 
     mutate(lead.own=ifelse(amt_000==max.ownshp_cumltv,1,0), lead.status=ifelse(eigen==max.status_cumltv,1,0), lead.ipo.exp=ifelse(i_exit_tl==max.i_exit_cumltv,1,0), lead.mna.exp=ifelse(m_exit_tl==max.m_exit_cumltv,1,0)) %>% 
     mutate(OS.match.round = ifelse(lead.own==lead.status,1,0), OI.match.round = ifelse(lead.own==lead.ipo.exp,1,0), OM.match.round = ifelse(lead.own==lead.mna.exp,1,0), 
            SI.match.round = ifelse(lead.status==lead.ipo.exp,1,0), SM.match.round = ifelse(lead.status==lead.mna.exp,1,0), IM.match.round = ifelse(lead.ipo.exp==lead.mna.exp,1,0)) %>% 
     mutate(OS.match=max(OS.match.round), OI.match=max(OI.match.round), OM.match=max(OM.match.round),
            SI.match=max(SI.match.round), SM.match=max(SM.match.round), IM.match=max(IM.match.round))
   test3 <- dplyr::select(test2, -c(max.ownshp_cumltv,max.status_cumltv,max.i_exit_cumltv,max.m_exit_cumltv,
                             lead.own,lead.status,lead.ipo.exp,lead.mna.exp,
                             OS.match.round,OI.match.round,OM.match.round,SI.match.round,SM.match.round,IM.match.round)) %>% as.data.frame
   return(test3)
 }
 rfmi_repu_social <- do.call(rbind,map(split(rfmi_repu_social,rfmi_repu_social$company_name),add.match.dis.match))
 ### add cumulative n.distinct funds ###
 add.cum.n.firms <- function(df){
   n.firms.by.round <- mutate(df, cum_unique_firms = cumsum(!duplicated(fund.match))) %>%
     group_by(round) %>%
     summarise(cum_n.firm = last(cum_unique_firms))
   result <- merge(df, n.firms.by.round, by="round")
   return(result)}
 rfvc.list <- map(split(rfmi_repu_social,rfmi_repu_social$company_name), safely(add.cum.n.firms))
 bad <- which(unlist(map(rfvc.list, function(x){!is_null(x$error)})))
 rfvc.file <- do.call(rbind, map(rfvc.list, function(x)(x$result)))
 rm(rfvc.list)
 
 rfvc.file$eigen <- rfvc.file$eigen.x
 rfvc.file$constraint <- rfvc.file$constraint.x
 rfvc.file$exit_success <- rfvc.file$exit_success.x
 rfvc.file$i_exit_success <- rfvc.file$i_exit_success.x
 rfvc.file$m_exit_success <- rfvc.file$m_exit_success.x
 ## add cumulative and non-cumulative amt, max.status, min.constraint, i_exit_success, m_exit_success ## very slow
 add.cnasim <- function(test){
   data <- test[,c("round","company_name","fund.match","amt_000","eigen","constraint","exit_success","i_exit_success","m_exit_success")]
   data.cum <- arrange(data,round) %>% 
               mutate(amt.cum=cumsum(amt_000),status.cum=cummax(eigen),constraint.cum=cummin(constraint),
               e_success.cum=cummax(exit_success),i_success.cum=cummax(i_exit_success),m_success.cum=cummax(m_exit_success)) %>%
               group_by(round) %>% filter(row_number()==n()) %>% dplyr::select(-c(2:9))
   data.round <- group_by(data, round) %>% 
                 summarise(amt.sum=sum(amt_000),status.sum=max(eigen),constraint.sum=min(constraint),
                                                     e_success.sum=max(exit_success),i_success.sum=max(i_exit_success),m_success.sum=max(m_exit_success),
                                                     sum_n_firms=n_distinct(fund.match))
   result <- reduce(list(test, data.cum, data.round), inner_join, by="round")                   
   return(result)
 }
 rfvc.file <- do.call(rbind,map(split(rfvc.file,rfvc.file$company_name),add.cnasim))
# write.csv(rfvc.file,"rfvc.file.12.31.17.csv", row.names = FALSE) ### firm-round-VC file backup
# write.csv(rfvc.file,"rfvc.file.01.31.18.csv", row.names = FALSE)
 # (test <- filter(rfvc.file, company_name=="@Road, Inc."))
 # (test <- filter(rfvc.file, company_name==unique(rfmi_repu_social.cvc$company_name[299])))
 # setwd("/Volumes/RESEARCH_HD/006/network_data")
 # rfvc.file <- read.csv("rfvc.file.01.31.18.csv")
 ### aggregate company-VC-year(round) to company-round data ###
 PC_round_008 <- rfvc.file %>% group_by(company_name, round) %>% filter(row_number()==n())
   ifelse(length(unique(PC_round_008$company_name))==length(unique(rfvc.file$company_name)) & 
            sum(duplicated(PC_round_008[,c("round","company_name")]))==0,"Looks Good","double-check")
 
 ## Add information to PC_round_008 ##
 ## market PE ##
 SnP_PE <- read.csv("S&P_PE.csv",header = FALSE)
 names(SnP_PE) <- c("year","Market_PE")
 SnP_PE$year <- lubridate::year(lubridate::mdy(SnP_PE$year)) 
 ## market hotness ##
 hot <- read.csv("all.hotness.table.csv")
 hot[is.na(hot)] <- 0
 names(hot)[2:3] <- c("IPOs_hotness_by_year","MnAs_hotness_by_year") 
 ## company info ## company age, first investment etc. ###
 company.info <- read.csv("company.info.csv")
 founding.region <- dplyr::select(company.info, company_name, founding_date, industry, Company.State.Region)
 founding.region$founding_year <- year(mdy(founding.region$founding_date))
 founding.region <- dplyr::select(founding.region, -2)
 ## merge info. with PC_round ##
 PC_round_008 <- reduce(list(PC_round_008, SnP_PE, hot),left_join,by="year")
 PC_round_008 <- merge(PC_round_008, founding.region, by="company_name")
 PC_round_008 <- arrange(PC_round_008, company_name, round) %>% as.data.frame()
# merge with PC_network #
 PC_round_008 <- merge(PC_round_008, PC_network, by=c("company_name","year"))
 ifelse(sum(duplicated(PC_round_008[,c("round","company_name")]))==0,"Looks Good","double-check")
## delete cuip==NA ##
 sum(duplicated(PC_round_008[,c("round","company_name")]))
 sum(duplicated(PC_round_008[,c("round", "cusip")]))
 sum(duplicated(PC_round_008[,c("year", "cusip")]))
 PC_round_008 <- PC_round_008[-which(is.na(PC_round_008$cusip)),]
 dup <- which(duplicated(PC_round_008[,c("cusip", "year")]) | duplicated(PC_round_008[,c("cusip", "year")], fromLast = TRUE))
 PC_round_008 <- PC_round_008[-dup,]
 dup.r <- which(duplicated(PC_round_008[,c("cusip", "round")]) | duplicated(PC_round_008[,c("cusip", "round")], fromLast = TRUE))
 # PC_round_008[dup.r,][,1:8]
 PC_round_008 <- PC_round_008[-dup.r,]

# construct the data IV=AA_roling #
 IV_data.1 <- merge(PC_round_008, AA_rolling, by.x=c("cusip","year"), by.y=c("CusipAup","year"), all.x = TRUE)
 IV_data.1[,36:41][is.na(IV_data.1[,36:41])] <- 0
 # A <- data.frame(a=seq(1, 5, 1), b=rep(NA, 5), c=rep(NA, 5))
 # A[,2:3][is.na(A[,2:3])] <- 0
 AA_IV_rolling <- IV_data.1
 ### create a lag_IV data ###
 IV_data.1_splitted <- split(IV_data.1, IV_data.1$cusip)
 prep_lag_IV <- function(test){
 # test <- IV_data.1_splitted[[3]]
 # test <- IV_data.1_splitted[[9]]
 if(nrow(test)==1){final <- test}else{
   data.1 <- test[,1:8]
   data.1$year_lag <- data.1$year - 1
   data.2 <- test[,c(2,9:ncol(test))]
   final <- merge(data.1, data.2, by.x = "year_lag", by.y = "year") %>% dplyr::select(-1)
 }
 return(test)
 }
 PC_AA_IV <- do.call(rbind, purrr::map(IV_data.1_splitted, prep_lag_IV)) ## with lagged IV

# construct the data DV=AA_roling # use network to predict the next year's (the nearest future) acquisitions and alliances only #
 DVdata_IVs <- PC_round_008
 DVdata_IVs$lead.year <- DVdata_IVs$year+1
 PC_AA_DV <- merge(DVdata_IVs, AQ_AL_Count, by.x = c("cusip", "lead.year"), by.y = c("CusipAup", "year"), all.x = TRUE)
 DV.col <- which(names(PC_AA_DV) %in% names(AQ_AL_Count)[3:8])
 PC_AA_DV[,DV.col][is.na(PC_AA_DV[,DV.col])] <- 0
 sum(duplicated(PC_AA_DV[,c("cusip", "year")]))
 
 ### final check ###
    length(unique(PC_round_008$cusip)) - length(unique(PC_AA_DV$cusip))
    length(unique(PC_round_008$cusip)) - length(unique(PC_AA_IV$cusip))
   lost.in.merge <- unique(PC_round_008$cusip)[which(!unique(PC_round_008$cusip) %in% unique(PC_AA_DV$cusip))]
   lost.in.merge <- unique(PC_round_008$cusip)[which(!unique(PC_round_008$cusip) %in% unique(PC_AA_IV$cusip))]
   # ### visually confirm ###
   #  i <- 6
   #  (test.1 <- PC_round_008[which(PC_round_008$cusip==lost.in.merge[i]),])
   #  (test.2 <- IV_data.1[which(IV_data.1$cusip==lost.in.merge[i]),])
   #  (test.3 <- AA_rolling[which(AA_rolling$CusipAup==lost.in.merge[i]),])
   #  (test.4 <- DVdata_IVs[which(DVdata_IVs$CusipAup==as.character(lost.in.merge[i])),])
   #  (test.5 <- AQ_AL_Count[which(AQ_AL_Count$CusipAup==as.character(lost.in.merge[i])),])
   #  i <- i+1
   #  PC_AA_DV[which(PC_AA_DV$cusip=="70975K"),]
   #  PC_AA_IV[which(PC_AA_DV$cusip=="70975K"),]
    # find.lost <- PC_round_008 %>% filter(cusip %in% lost.in.merge)
   PC_AA_DV <- dplyr::filter(PC_AA_DV, year>1980)
   PC_AA_IV <- dplyr::filter(PC_AA_IV, year>1980)
   cat("there are a total of", length(unique(PC_AA_DV$cusip)),"PCs in the dataset")
   cat("there are a total of", length(unique(PC_AA_IV$cusip)),"PCs in the dataset")
   ifelse(sum(duplicated(PC_AA_DV[,c("company_name","round")]))==0,"Good!","Check")
   ifelse(sum(duplicated(PC_AA_IV[,c("company_name","round")]))==0,"Good!","Check")
    # PC_AA_DV <- PC_AA_DV[-which(duplicated(PC_AA_DV[,c("company_name","round")])),]
    # PC_AA_IV <- PC_AA_IV[-which(duplicated(PC_AA_IV[,c("company_name","round")])),]
     PC_AA_IV$exit <- ifelse(PC_AA_IV$event=="censor",0,1)
     PC_AA_IV$IPO <- ifelse(PC_AA_IV$event=="IPO",1,0)
     PC_AA_IV$MnA <- ifelse(PC_AA_IV$event=="MnA",1,0)
     PC_AA_IV$amt.sum <- PC_AA_IV$amt.sum /1000
    PC_AA_DV$amt.sum <- PC_AA_DV$amt.sum /1000
    
   # write.csv(PC_AA_DV, "PC_AA_DV_1.31.2018.csv", row.names = FALSE)
   # write.csv(PC_AA_DV, "PC_AA_DV_12.10.2018.csv", row.names = FALSE)
   # write.csv(PC_AA_IV, "PC_AA_IV_1.31.2018.csv", row.names = FALSE)
   
   length(unique(AQ_AL_Count$CusipAup))
   length(unique(AA_rolling$CusipAup))
   length(unique(PC_AA_DV$cusip))
   extra <- PC_AA_DV$cusip[!unique(PC_AA_DV$cusip) %in% unique(AA_rolling$CusipAup)]
   i <- 1
   PC_AA_DV[which(PC_AA_DV$cusip==extra[i]),]
   i <- i + 1
   unique(PC_AA_DV$Company.State.Region)
   hist(data$year)

### add Alliance Experience and Alliance Network Centrality to PC_AA_DV ###
   setwd("/Volumes/RESEARCH_HD/006/network_data")
   centrality_alli_nwk <- read.csv("centrality_alli_nwk.csv")
    centrality_alli_nwk$lead.year <- centrality_alli_nwk$year + 1
    centrality_alli_nwk <- centrality_alli_nwk[,2:5]
    names(centrality_alli_nwk)[2:3] <- paste0("alliance_", names(centrality_alli_nwk)[2:3])
   AA_rolling <- read.csv("AA_rolling.csv", stringsAsFactors = FALSE) %>% dplyr::select(-c(3:8))
    AA_rolling <- AA_rolling[,c(1,2,4)]
    AA_rolling$lead.year <- AA_rolling$year + 1
    AA_rolling <- AA_rolling[,c(1,3,4)]
    names(AA_rolling)[1:2] <- c("cusipAup", "alliance_experience")
   PC_AA_DV <- PC_AA_DV[which(PC_AA_DV$lead.year > 1990 & PC_AA_DV$lead.year < 2016),]
   names(PC_AA_DV)[1] <- "cusipAup"
  PC_AA_DV <- purrr::reduce(list(PC_AA_DV, centrality_alli_nwk, AA_rolling), dplyr::left_join, by=c("cusipAup", "lead.year"))
  PC_AA_DV[is.na(PC_AA_DV)] <- 0
  # write.csv(PC_AA_DV, "PC_AA_DV_2.9.2018.csv", row.names = FALSE)
  # write.csv(PC_AA_DV, "PC_AA_DV_12.10.2018.csv", row.names = FALSE)

### add Acquisition Experience to PC_AA_DV
  setwd("/Volumes/RESEARCH_HD/006/network_data")
  AA_rolling <- read.csv("AA_rolling.csv", stringsAsFactors = FALSE) %>% dplyr::select(-c(3:8))
  AA_rolling <- AA_rolling[, c(1,2,3)]
  AA_rolling$lead.year <- AA_rolling$year + 1
  AA_rolling <- AA_rolling[, c(1,4,3)]
  names(AA_rolling)[3] <- c("acquisition_experience")
  # data.DV <- read.csv("PC_AA_DV_Jul.01.2018.csv")
  data.DV <- read.csv("PC_AA_DV_12.10.2018.csv")
  data.DV_acq.exp <- merge(data.DV, AA_rolling, by.x = c("cusipAup", "year"), by.y = c("CusipAup", "lead.year"), all.x = TRUE)
  data.DV_acq.exp[is.na(data.DV_acq.exp)] <- 0
  # write.csv(data.DV_acq.exp, "PC_AA_DV_8.28.2018.csv", row.names = FALSE)
  write.csv(data.DV_acq.exp, "PC_AA_DV_12.10.2018.csv", row.names = FALSE)
  
### add dependence variables
  ### (lead) VC dependence on PC ### relative standing ###
  ### add relative standing ###
  rfmi_repu_social <- fread("rfmi_repu_social.5Year.csv") # 12.10.2018 changed 3Year to 5Year.csv # unit obs = company-VC-year 
  PC.standig.in.VC.folio <- arrange(rfmi_repu_social, fund.match, year) %>% 
    group_by(fund.match, year) %>% 
    mutate(relative.standing = amt_000/sum(amt_000)) %>% 
    dplyr::select(company_name, fund.match, year, amt_000, relative.standing) %>% 
    ungroup() %>% 
    arrange(company_name, year, amt_000) %>% 
    group_by(company_name, year) %>% 
    filter(row_number()==n()) %>% ## select the lead VC
    arrange(company_name, year) %>% 
    as.data.frame()
  PC.standig.in.VC.folio[is.na(PC.standig.in.VC.folio)] <- 0
  names(PC.standig.in.VC.folio)[5] <- "dependence_lead_VC_on_PC"
  DEP_VC_on_PC <- PC.standig.in.VC.folio %>% dplyr::select(-fund.match, -amt_000)
  ### add All VC dependence on PC ###
  rfmi_repu_social <- fread("rfmi_repu_social.5Year.csv") # 12.10.2018 changed 3Year to 5Year.csv # unit obs = company-VC-year 
  PC.standig.in.ALL_VC <- arrange(rfmi_repu_social, fund.match, year) %>% 
    group_by(fund.match, year) %>% 
    mutate(fund_year_total_amt = sum(amt_000)) %>% 
    ungroup() %>% 
    dplyr::select(company_name, fund.match, year, amt_000, fund_year_total_amt) %>% 
    group_by(company_name, year) %>% 
    mutate(relative.standing_all_VC = sum(amt_000)/sum(fund_year_total_amt)) %>% ## All VCs' money in a PC / All VCs' money in all PC 
    filter(row_number()==n()) %>% ## select the last line
    arrange(company_name, year) %>% 
    as.data.frame()
  PC.standig.in.ALL_VC[is.na(PC.standig.in.ALL_VC)] <- 0
  names(PC.standig.in.ALL_VC)[6] <- "dependence_ALL_VC_on_PC"
  DEP_ALL_VC_on_PC <- PC.standig.in.ALL_VC %>% dplyr::select(-fund.match, -amt_000, -fund_year_total_amt)
  ### PC dependence on VC ### 
  ## HHI ###
  rfmi_repu_social <- fread("rfmi_repu_social.5Year.csv") # 12.10.2018 changed 3Year to 5Year.csv # unit obs = company-VC-year 
  add.HHI <- function(df){
    # df <- split(rfmi_repu_social.cvc,rfmi_repu_social.cvc$company_name)[[2]]
    result <- data.frame(df[,c("year","fund.match","fund_type","amt_000")])
    look_up_type <- unique(result[,c("fund.match", "fund_type")])
    pad_to_merge <- expand.grid(unique(result$fund.match), unique(result$year))
    names(pad_to_merge)[1:2] <- c("fund.match", "year")
    pad_to_merge$fund.match <- as.character(pad_to_merge$fund.match)
    result <- right_join(result, pad_to_merge, by=c("year", "fund.match"))
    result$fund_type <- NULL
    result <- left_join(result, look_up_type, by = "fund.match")
    result[is.na(result)] <- 0
    result <- result %>% 
      group_by(fund.match) %>% 
      mutate(amt_cum_by_fund = cumsum(amt_000)) %>% 
      ungroup() %>% 
      group_by(year) %>% 
      mutate(VC_Invest_HHI = sum((amt_cum_by_fund/sum(amt_cum_by_fund))^2)) %>% 
      filter(row_number()==n()) # select the last row 
    result <- dplyr::select(result, c(1,6))
    result <- merge(df, result, by="year")
    return(result)}
  rfmi_repu_social <- do.call(rbind, map(split(rfmi_repu_social,rfmi_repu_social$company_name), add.HHI))
  # lead-VC-proportion #
  add.cum.amt <- function(df){
    # split <- split(rfmi_repu_social,rfmi_repu_social$company_name) # for testing
    # df <- split[[22]] # for testing
    result <- data.frame(df[, c("year","fund.match","amt_000")]) %>% arrange(year)
    result[is.na(result)] <- 0
    result <- mutate(result, cum_max_prop_amt = cummax(amt_000)/cumsum(amt_000)) %>% 
              group_by(year) %>% 
              summarise(cum_max_prop_amt=last(cum_max_prop_amt))
    result <- merge(df, result, by="year")
    return(result)}
  rfmi_repu_social <- do.call(rbind,map(split(rfmi_repu_social,rfmi_repu_social$company_name),add.cum.amt))
 DEP_PC_on_VC <- rfmi_repu_social %>% dplyr::select(company_name, year, VC_Invest_HHI, cum_max_prop_amt) %>% 
                 group_by(company_name, year) %>% 
                 filter(row_number()==n())
 DEP_PC_on_VC[is.na(DEP_PC_on_VC)] <- 0
 names(DEP_PC_on_VC)[4] <- "dependence_PC_on_lead_VC"
  PC_AA_DV <- reduce(list(PC_AA_DV, DEP_VC_on_PC, DEP_ALL_VC_on_PC, DEP_PC_on_VC), left_join, by=c("company_name","year"))
  PC_AA_DV[is.na(PC_AA_DV)] <- 0
 # write.csv(PC_AA_DV, "PC_AA_DV_May.04.2018.csv", row.names = FALSE)
 # write.csv(PC_AA_DV, "PC_AA_DV_12.10.2018.csv", row.names = FALSE)
  
# # 2018.03.01 ### update Alliance Network Centrality to PC_AA_DV ###
#   setwd("/Volumes/RESEARCH_HD/006/network_data")
#   centrality_alli_nwk <- read.csv("centrality_alli_all_nwk.csv")
#    centrality_alli_nwk$lead.year <- centrality_alli_nwk$year + 1
#    centrality_alli_nwk <- centrality_alli_nwk[,2:5]
#    names(centrality_alli_nwk)[2:3] <- paste0("alliance_", names(centrality_alli_nwk)[2:3])
#   PC_AA_DV <- read.csv("PC_AA_DV_2.9.2018.csv", stringsAsFactors = FALSE)
#   PC_AA_DV <- PC_AA_DV[,-c(40,41)]
#   PC_AA_DV <- dplyr::left_join(PC_AA_DV, centrality_alli_nwk, by=c("cusipAup"="cusipAup","year"="lead.year"))
#   PC_AA_DV[is.na(PC_AA_DV)] <- 0
#   write.csv(PC_AA_DV, "PC_AA_DV_3.1.2018.csv", row.names = FALSE)
  
  # 07.01.2018 # add horizontal and vertical endorsement 
  setwd("/Volumes/RESEARCH_HD/006/network_data")
  data.DV <- read.csv("PC_AA_DV_May.04.2018.csv", stringsAsFactors = FALSE) %>% mutate(lag.year = year - 1)
  endorsement <- read.csv("endorsement.centrality.csv", stringsAsFactors = FALSE)
  data.DV <- left_join(data.DV, endorsement, by=c("cusipAup" = "CUSIP", "lag.year" = "year"))
  data.DV$endorsement.eigen[is.na(data.DV$endorsement.eigen)] <- 0
  data.DV$endorsement.constraint[is.na(data.DV$endorsement.constraint)] <- 1.125
   sum(duplicated(data.DV[,c("cusipAup","year","endorsement.eigen","endorsement.constraint")]))
  data.DV <- data.DV[-which(duplicated(data.DV[,c("cusipAup","year","endorsement.eigen","endorsement.constraint")])),]
   sum(duplicated(data.DV[,c("cusipAup","year")]))
  data.DV <- data.DV %>% group_by(cusipAup, year) %>% 
             mutate(endorse.eigen = max(endorsement.eigen), endorse.constraint = min(endorsement.constraint))
  data.DV <- data.DV[-which(duplicated(data.DV[,c("cusipAup","year")])),] %>% dplyr::select(-endorsement.eigen, -endorsement.constraint)
  
    centrality_alli_nwk <- read.csv("centrality_alli_all_nwk.csv", stringsAsFactors = FALSE)
     centrality_alli_nwk$lead.year <- centrality_alli_nwk$year + 1
     centrality_alli_nwk <- centrality_alli_nwk[,2:5]
     names(centrality_alli_nwk)[2:3] <- paste0("alliance_", names(centrality_alli_nwk)[2:3])
  
  data.DV <- data.DV %>% dplyr::select(-alliance_eigen, -alliance_constraint) %>% 
             dplyr::left_join(centrality_alli_nwk, by=c("cusipAup"="cusipAup","year"="lead.year"))
  data.DV$alliance_eigen[is.na(data.DV$alliance_eigen)] <- 0
  data.DV$alliance_constraint[is.na(data.DV$alliance_constraint)] <- 1.125
  # write.csv(data.DV, "PC_AA_DV_Jul.01.2018.csv", row.names = FALSE)  
  
  # # 9.17.2018 add only startup_evolved alliance centrality to PC_AA_DV_8.28.2018.csv, got insig. betas.
  # setwd("/Volumes/RESEARCH_HD/006/network_data")
  # data.DV <- read.csv("PC_AA_DV_8.28.2018.csv", stringsAsFactors = FALSE)
  # 
  # centrality_alli_nwk <- read.csv("centrality_alli_nwk.csv")
  # centrality_alli_nwk$lead.year <- centrality_alli_nwk$year + 1
  # centrality_alli_nwk <- centrality_alli_nwk[,2:5]
  # names(centrality_alli_nwk)[2:3] <- paste0("startup_alliance_", names(centrality_alli_nwk)[2:3])
  # 
  # data.DV <- dplyr::left_join(data.DV, centrality_alli_nwk, by=c("cusipAup", "lead.year"))
  
  