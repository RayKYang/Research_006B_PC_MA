

convert.w.rob.se <- function(model, which.part = "count"){
  coeftest(model,   sandwich::vcovCL(model,   cbind(model$model$cusipAup, factor(model$model$year)))) %>% 
    `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% 
    subset(stringr::str_detect(names(coef(model)), which.part))
}

start <- Sys.time()
re.relect <- c()
for(C in option3){
  tryCatch({
  PC <- as.character(unique(data$company_name))[C]
  
  data_TRY <- data.DV[-which(data.DV$company_name %in% PC),] 
  
  ali     <- zeroinfl(n.ali.overall ~ amt.sum + cum_n.firm + round + e_success.sum 
                    + age + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend
                    + status.cum * (acquisition_experience + alliance_experience) 
                    + alliance_eigen * (acquisition_experience + alliance_experience)
                    | amt.sum + e_success.sum + age + acquisition_experience + alliance_experience
                    , dist = "negbin", 
                    data = data.DV)
  
  ali.TRY <- zeroinfl(n.ali.overall ~ amt.sum + cum_n.firm + round + e_success.sum 
                    + age + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend
                    + status.cum * (acquisition_experience + alliance_experience) 
                    + alliance_eigen * (acquisition_experience + alliance_experience)
                    | amt.sum + e_success.sum + age + acquisition_experience + alliance_experience
                    , dist = "negbin",
                    data = data_TRY)
  
  acq     <- zeroinfl(n.acq.ASSETS ~ amt.sum + cum_n.firm + round + e_success.sum 
                    + age + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend
                    + status.cum * (acquisition_experience + alliance_experience) 
                    + alliance_eigen * (acquisition_experience + alliance_experience)
                    | amt.sum + e_success.sum + age + acquisition_experience + alliance_experience
                    , dist = "negbin", 
                    data = data.DV)
  
  acq.TRY <- zeroinfl(n.acq.ASSETS ~ amt.sum + cum_n.firm + round + e_success.sum 
                    + age + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend
                    + status.cum * (acquisition_experience + alliance_experience) 
                    + alliance_eigen * (acquisition_experience + alliance_experience)
                    | amt.sum + e_success.sum + age + acquisition_experience + alliance_experience
                    , dist = "negbin",
                    data = data_TRY)
  
  al.1_old <- convert.w.rob.se(ali,   "count") 
  al.1_new <- convert.w.rob.se(ali.TRY,   "count") 
  
  aq.1_old <- convert.w.rob.se(acq,   "count") 
  aq.1_new <- convert.w.rob.se(acq.TRY,   "count") 
  
  int1 <- 14
  int2 <- 15
  int3 <- 16
  int4 <- 17
  
  p.col <- 6
  
  wl.1 <- al.1_old[int1, p.col]
  wl.2 <- al.1_old[int2, p.col]
  wl.3 <- al.1_old[int3, p.col]
  wl.4 <- al.1_old[int4, p.col]
  
  wq.1 <- aq.1_old[int1, p.col]
  wq.2 <- aq.1_old[int2, p.col]
  wq.3 <- aq.1_old[int3, p.col]
  wq.4 <- aq.1_old[int4, p.col]
  
  improve.1a <- wl.1 - al.1_new[int1, p.col]
  improve.1b <- wl.2 - al.1_new[int2, p.col]
  improve.2a <- wl.3 - al.1_new[int3, p.col]
  improve.2b <- wl.4 - al.1_new[int4, p.col]
  
  improve.1a_Eq <- wq.1 - aq.1_new[int1, p.col]
  improve.1b_Eq <- wq.2 - aq.1_new[int2, p.col]
  improve.2a_Eq <- wq.3 - aq.1_new[int3, p.col]
  improve.2b_Eq <- wq.4 - aq.1_new[int4, p.col]
  
  impact <- data.frame(improve.1a = improve.1a,
                    improve.1b = improve.1b,
                    improve.2a = improve.2a,
                    improve.2b = improve.2b,
                    improve.1a_Eq = improve.1a_Eq,
                    improve.1b_Eq = improve.1b_Eq,
                    improve.2a_Eq = improve.2a_Eq,
                    improve.2b_Eq = improve.2b_Eq)
  
  weight <- c(wl.1, wl.2, wl.3, wl.4, wq.1, wq.2, wq.3, wq.4)
  weight[3] <- 0
  print(which(option3 == C))
  print(impact)
  if(weight %*% t(impact)>0){
    data.DV <- data_TRY
    print(al.1_new)
    print(aq.1_new)
    re.relect <- c(re.relect, C)
  }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
Sys.time() - start
