# last run: 12.10.2018

setwd("/Volumes/RESEARCH_HD/006/006B_raw_data")
regrrr::load.pkgs(c("readr","data.table","xts","tidyr","dplyr","stringr","purrr","lubridate","maxLik","data.table"))

# 1.1 read in clean round data #####
round <- read_csv("round.data.csv")
round.1 <- separate(round,info,c("date","amt_000","investor","else."),sep="  ")
# table(round.1$else.)
round.1[which(round.1$else. == "Ventures - Unspecified Fund"),]$investor <- "Blumberg Capital Ventures"
round.1[which(round.1$else. == "CMGI@Ventures III)"),]$investor <- "CMGI@Ventures"
round.1[which(round.1$else. == "Development Corp."),]$investor <- "Nynex Development Corp."
round.1 <- round.1[,-6]
round.2 <- separate(round.1,investor,c("investor","col6"),sep=" - ")
round.2[which(round.2$col6 == "Genzyme BioVentures"),]$investor <- "Sanofi Genzyme BioVentures"
round.2 <- round.2[-which(round.2$col6 == "Investing in UK"),]
round.2[which(round.2$col6 == "U.F."),]$investor <- "Quest Ventures"
round <- round.2[,-6]
round.3 <- separate(round,investor,c("investor","what"),sep=",")
round.3[which(round.3$what== " Communications and Entertainment"),]$investor <- "GE" 
round.3[which(round.3$what== " The (Nomura)"),]$investor <- "Nomura" 
round <- select(round.3,-what) # for 006B write.csv(unique(round$cusip),"Xpert_cusip.csv",row.names=FALSE)

# 1.2 read in and clean fund and firm data #####
fund <- read_csv("fund.data.csv")
firm <- read_csv("firm.data.csv")
## clean fund & firm names
abrr.name <- function(str){
  words.to.remove <- c("Undisclosed Fund", "- Unspecified Fund","Venture", "New Ventures","Partners","Funds", "Funding",
                       "Equity","Management","LLLP","Convergence Fund","Alternative","Private Equity","Venture Capital",
                       "Investments","Investment","Investors","Private Capital","Company LLC","Co","CO","INC","And Co","and Co",
                       "& Company","Venture Partners","Venture Funds","Corporation","PLC","Unspecified Fund","Associates","Group",
                       "Mezzanine","I","II","III","IV","V","VI","VII","VIII","IX","XI","X-A")
  pattern <- c("\\s[0-9]{4,}", "\\s([0-9]+)\\,?\\s?$", "\\s\\&\\sCo\\.?\\-?\\s?(.*)?$" , "\\s\\&\\s(Company)(\\,\\s?L\\.?\\s?P\\.?)?$", 
               "\\s(Capital)\\s?(.*)?" ,"\\s((?i)F(?i)u(?i)n(?i)d)\\s?(.*)?","\\s(Ventures)\\s?(.*)?",
               "\\s\\&\\s{0,5}\\-?\\.?\\s{0,6}$", " \\s*(\\(.*\\))" , "\\)", "\\&\\s+\\.(.*)?",
               "\\-(.*)","\\s?\\,(.*)?$","\\s\\.\\s?$",
               "(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$",
               "Co\\.?$", "Corp\\.?$", "Ltd\\.?","Inc\\.?$",
               "(\\s{0,6}\\&{0,6}\\s{0,6})?(\\sL\\.?\\s?L\\.?\\s?C\\.?\\s?)?$",
               "([0-9]+\\s?\\,?\\s?)?L?L\\.?\\s?P\\.?(.*)?$") 
  pattern <- paste0(pattern,collapse="|")
  return(tm::removeWords(str,words.to.remove) %>% str_replace_all(pattern,"") %>% str_trim() %>% str_replace_all("(\\s)+"," "))
}
fund  <- filter(fund, fund_name != "Undisclosed Fund")
fund  <- mutate(fund, fund.match = abrr.name(fund_name))
firm  <- filter(firm, firm_name != "Undisclosed Firm")
firm  <- mutate(firm, firm.match = abrr.name(firm_name))

round <- filter(round, investor != "Undisclosed Fund")
round <- mutate(round, fund.match = abrr.name(investor))

# 1.3 read in and clean company info. #####
info1 <- fread("company info..csv",na.strings="") %>% select(3,5) # industry class
colnames(info1) <- c("company_name","industry")
info2 <- fread("company info2.txt",na.strings="") %>% arrange(company_name) # dates and area
info  <- arrange(info1, company_name) %>% bind_cols(info2) %>% select(-5) # 
info  <- dplyr::filter(info, company_name != "Undisclosed Company")
sum(duplicated(info$company_name))
duplicate.names <- info[which(duplicated(info$company_name)==1 | duplicated(info$company_name,fromLast = TRUE)==1),]$company_name
info <- dplyr::filter(info, !(company_name %in% duplicate.names))
round <- dplyr::filter(round, !(company_name %in% duplicate.names))
fund  <- dplyr::filter(fund,  !(company_name %in% duplicate.names))
firm  <- dplyr::filter(firm,  !(company_name %in% duplicate.names))
info3 <- readxl::read_excel("006 more company info 0817.xls", skip=1)
names(info3) <- unlist(map(str_split(names(info3),pattern="\n"),paste,collapse=" "))
info3 <- select(info3, -9)
sum(duplicated(info3$`Company Name`) | duplicated(info3$`Company Name`,fromLast = TRUE))
duplicate.names.3 <- info3[which(duplicated(info3$`Company Name`)==1 | duplicated(info3$`Company Name`,fromLast = TRUE)==1),]$`Company Name`
info3 <- dplyr::filter(info3, !(`Company Name` %in% duplicate.names.3))
round <- dplyr::filter(round, !(company_name %in% duplicate.names.3))
fund  <- dplyr::filter(fund,  !(company_name %in% duplicate.names.3))
firm  <- dplyr::filter(firm,  !(company_name %in% duplicate.names.3))
company.info <- merge(info, info3, by.x="company_name",by.y="Company Name") # every row is a unique company
setwd("/Volumes/RESEARCH_HD/006/network_data")
write.csv(company.info, "company.info.csv", row.names=FALSE)
setwd("/Volumes/RESEARCH_HD/006/006B_raw_data")

# 1.4 combine round firm fund ### "fund.match" are the keys for matching #####
round_fund <- merge(round, fund, by.x = c("company_name","fund.match"), by.y = c("company_name","fund.match") )
round_fund <- select(round_fund,-investor,-cusip.y,-fund_name)
round_firm <- merge(round_fund, firm, by.x = c("company_name","fund.match"), by.y = c("company_name","firm.match") )
round_firm <- select(round_firm,-firm_name,-cusip.x)
round_firm <- filter(round_firm,investment_type == "Venture Capital") # VC-backed are retained
round_firm <- mutate(round_firm, year= as.numeric(substr(date,7,10)))
round_firm <- group_by(round_firm,company_name) %>% 
  mutate(rank.y = rank(year)) %>% 
  arrange(company_name,rank.y) %>%              # rank by year
  select(-rank.y, -investment_type) %>%
  group_by(company_name, fund.match, year) %>% 
  filter(row_number(company_name) == 1)         # remove duplicated rows  nrow(round_firm) = 37,602
round_firm <- filter(round_firm, fund.match!="")
# sample overview
length(unique(round_firm$company_name))
table(round_firm$fund_type)
round_firm <- round_firm %>% filter(fund_type == "PRIV" | fund_type == "CORPVEN")
table(round_firm$fund_type)

# 1.5 read in MnA and IPO data #####
## read in M&A and IPO data; then match with round data
MnA <- fread("006 M&A.txt",header="auto",na.strings="")
M.colnames <- c("target_IPO.date","target_founding.date","date_eff","date_ann","target.cusip",
                "acquirer.cusip","acquirer.status","target.status")
names(MnA) <- paste("MnA",M.colnames,sep=".")
MnA <- filter(MnA, !is.na(MnA.target.cusip)) # MnA.target.status == "Private" & 
MnA <- MnA[-which(duplicated(MnA$MnA.target.cusip) | duplicated(MnA$MnA.target.cusip, fromLast = TRUE)),]
table(MnA$MnA.acquirer.status)
round_firm_MnA <- left_join(round_firm, MnA, by = c("cusip" = "MnA.target.cusip")) %>% 
  mutate( MnA=ifelse(is.na(MnA.date_ann),0,1) )
if(nrow(round_firm_MnA)==nrow(round_firm)){print("looks good!")}else{print("check nrow(x)!")}
IPO <- fread("006 IPO.txt",header="auto",na.strings="")
IPO <- unite(IPO,V5,c(V5,V6),remove=TRUE)
colnames(IPO) <- c("IPO.date","cusip6","venture_backed","SIC","nation")
IPO <- filter(IPO, nation=="United_States" & venture_backed=="Yes") %>% select(c(1:2),4)
IPO <- IPO[-which(duplicated(IPO$cusip6) | duplicated(IPO$cusip6, fromLast = TRUE)),]
round_firm_MnA_IPO <- left_join(round_firm_MnA, IPO, by = c("cusip" = "cusip6")) %>% 
  mutate( IPO=ifelse(is.na(IPO.date),0,1) ) %>%
  mutate( exit=ifelse( MnA==1 | IPO==1, 1, 0) ) 
if(nrow(round_firm_MnA_IPO)==nrow(round_firm_MnA)){print("looks good!")}else{print("check nrow(x)!")}
# IPO and MnA market Hotness from the MnA and IPO data
IPO$IPO.date <- lubridate::mdy(IPO$IPO.date)
IPO$year <- lubridate::year(IPO$IPO.date)
IPO.table <- group_by(IPO,year) %>% summarise(IPOs_by_year=n())
MnA$MnA.date_ann <- lubridate::mdy(MnA$MnA.date_ann)
MnA$year <- lubridate::year(MnA$MnA.date_ann)
MnA.table <- group_by(MnA,year) %>% summarise(MnAs_by_year=n())
all.hotness.table <- full_join(IPO.table,MnA.table,by="year") %>% filter(!is.na(year))
# write.csv(all.hotness.table,"all.hotness.table.csv", row.names=FALSE)
# construct the DV
round_firm_MnA_IPO <- mutate(round_firm_MnA_IPO, 
                             IPOvFLIP = exit + MnA + IPO - MnA - 1 ) %>%
  mutate( IPOvFLIP = ifelse(IPOvFLIP > (-1), IPOvFLIP, NA) ) ## DV: 1=IPO 0=MnA fail.to.exit=NA
# table the outcome and clean it
table(round_firm_MnA_IPO[,c("IPO","MnA","exit")])
table(round_firm_MnA_IPO[,c("IPOvFLIP","exit")])
# as.data.frame(filter(round_firm_MnA_IPO, IPO==1 & MnA==1)[,c("MnA.date_ann","IPO.date","MnA.target_IPO.date")]) # these should be deleted
round_firm_MnA_IPO <- round_firm_MnA_IPO[-which(round_firm_MnA_IPO$IPO==1 & round_firm_MnA_IPO$MnA==1),]
table(round_firm_MnA_IPO[,c("IPO","MnA","exit")])
table(round_firm_MnA_IPO[,c("IPOvFLIP","exit")])
## sample feature
cat(paste("round data contains\n",
          "a total number of",length(unique(round_firm_MnA_IPO$company_name)),"unique companies\n",
          "a total number of",nrow(round_firm_MnA_IPO),"obervations"))

# 1.6 construct reputation with round_firm_MnA_IPO #####
dist_cum <- function(var){
  seq_long <- seq_along(var)
  dist_cum <- function(x){length(unique(head(var, x)))}
  return(sapply(seq_long, dist_cum)) }
build.capabilites <- function(df){
  # test # df <- round_firm_MnA_IPO[which(round_firm_MnA_IPO$fund.match == "Zone"),]
  df.1 <- df[,c("company_name", "year", "MnA", "IPO", "exit")] %>% as.data.frame()
  df.2 <- df.1 %>% group_by(company_name) %>% filter(row_number()==n()) %>% arrange(year) %>% ungroup() %>% # one PC one row
    dplyr::mutate(exit_success_PCs = cumsum(exit)/dist_cum(company_name),      # exit rate by distinct PCs
                  m_exit_success_PCs = cumsum(MnA)/dist_cum(company_name),     # m exit rate by distinct PCs
                  i_exit_success_PCs = cumsum(IPO)/dist_cum(company_name)) %>% # i exit rate by distinct PCs
    group_by(year) %>% 
    filter(row_number()==n()) %>% dplyr::select(-company_name, -MnA, -IPO, -exit)
  df.3 <- full_join(df.2, data.frame(year = unique(df$year[!df$year %in% df.2$year])), by = "year") %>% 
    arrange(year) %>% 
    na.locf()
  df.3[is.na(df.3)] <- 0
  result <- left_join(df, df.3, by = "year") 
  return(result)
}
rfmi.1 <- do.call(rbind, purrr::map(split(round_firm_MnA_IPO, round_firm_MnA_IPO$fund.match), build.capabilites))
rfmi.1 <- arrange(rfmi.1, fund.match, year) %>% 
  group_by(fund.match) %>%
  mutate(exit_total = cumsum(exit),                  # count successful exit 
         m_exit_tl = cumsum(MnA),                    # count successful MnAs
         i_exit_tl = cumsum(IPO),                    # count successful IPOs; 
         add.one = 1,                            # prepped for counting total# invested companie-rounds
         dist_cum = dist_cum(company_name)) %>%        # count total # distinct invested companies
  group_by(fund.match) %>%
  mutate(exit_success   = exit_total/cumsum(add.one),
         m_exit_success = m_exit_tl/cumsum(add.one),
         i_exit_success = i_exit_tl/cumsum(add.one))

# construct sequence
rfmi.2 <- arrange(rfmi.1, company_name, year) %>% 
  group_by(company_name) %>%        
  mutate(one=1) %>%
  mutate(sequence = cumsum(one), sequence.position = cumsum(one)/n()) # mark the sequence of the VCs on a company
rfmi <- select(rfmi.2, - add.one, - one) 

# make rfmi_repu_social (merge rfmi with vc and company status) #
setwd("/Volumes/RESEARCH_HD/006/network_data")
status.df.VCs <- fread("Centrality.VCs.5Year.csv")
status.df.company <- fread("Centrality.PCs.5Year.csv")

rolling.window <- 5
rfmi_repu_social <- inner_join(rfmi, status.df.VCs, by=c("fund.match","year")) %>%
                     left_join(status.df.company, by=c("company_name","year"))
if(nrow(rfmi)==nrow(rfmi_repu_social)){print("looks good so far!")}
class(rfmi_repu_social$amt_000) <- "numeric"
write.csv(rfmi_repu_social, paste0("rfmi_repu_social.", rolling.window, "Year.csv"), row.names = FALSE)