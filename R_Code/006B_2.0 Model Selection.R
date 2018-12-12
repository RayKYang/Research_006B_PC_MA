setwd("/Volumes/RESEARCH_HD/006/network_data")
data.DV <- read.csv("PC_AA_DV_8.28.2018.csv", stringsAsFactors = FALSE) # the latest verion prior to 12.10 version
# data.DV <- read.csv("PC_AA_DV_12.10.2018.csv", stringsAsFactors = FALSE) # this dataset is used 5year rolling

regrrr::load.pkgs(c("dplyr", "pscl", "Formula", "lmtest", "regrrr", "multiwayvcov", "sandwich"))

data.DV$age <- with(data.DV, year - founding_year)
data.DV$age <- ifelse(data.DV$age < 0, 0, data.DV$age)
data.DV$alliance_eigen <- regrrr::scale_01(data.DV$alliance_eigen)
data.DV$year.trend <- data.DV$year - 1989

update.zeroinfl <- function(object, new, ...) {
  call <- object$call
  call$formula <- update(as.Formula(formula(object)), new)
  eval.parent(call)
}

# delete outliers:
# option3 <- str.op3[stringr::str_detect(str.op3, "")] %>% as.numeric()
# data.DV <- data
# dim(data.DV)
# C <- option3
# PC <- as.character(unique(data$company_name))[C]
# is <- which(data$company_name %in% PC)
# data.DV <- data[-is,] 
# dim(data.DV)
# regression table 
convert.w.rob.se <- function(model, which.part = "count"){
  coeftest(model,   sandwich::vcovCL(model,   cbind(model$model$cusipAup, factor(model$model$year)))) %>% 
    `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% 
    subset(stringr::str_detect(names(coef(model)), which.part))
}
get.weight <- function(DATA){
  
  tryCatch({
    run.old.1 <- zeroinfl(n.ali.overall ~ amt.sum + cum_n.firm + round + e_success.sum 
                          + age + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend
                          + status.cum * (acquisition_experience + alliance_experience) 
                          + alliance_eigen * (acquisition_experience + alliance_experience)
                          | amt.sum + e_success.sum + age + acquisition_experience + alliance_experience
                          , dist = "negbin", 
                          data = DATA)
    
    run.old.1_Eq <- zeroinfl(n.acq.ASSETS ~ amt.sum + cum_n.firm + round + e_success.sum 
                             + age + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend
                             + status.cum * (acquisition_experience + alliance_experience) 
                             + alliance_eigen * (acquisition_experience + alliance_experience)
                             | amt.sum + e_success.sum + age + acquisition_experience + alliance_experience
                             , dist = "negbin", 
                             data = DATA)
    
    al.1_old <- convert.w.rob.se(run.old.1,   "count") 
    aq.1_old <- convert.w.rob.se(run.old.1_Eq,   "count") 
    
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
    
    weight <- c(wl.1, wl.2, wl.3, wl.4, wq.1, wq.2, wq.3, wq.4)
  }, error=function(e){cat("ERROR :", conditionMessage(e), "\n")}, 
  weight <- rep(1, 8)
  ) 
  return(weight)
}

data <- data.DV
data.DV <- data
dim(data.DV)

weight <- get.weight(data.DV)
plot(weight, type = "b")
# plot(result, type = "b")
abline(h = 0.05)

library(RColorBrewer)
# par(mar = c(0, 4, 0, 0))
# display.brewer.all()
colors <- brewer.pal(10, "Paired")

outliers.deleted <- c(4026, 6642, 3370, 7318, 5584, 6521, 1610, 5492, 5276, 2138, 2275, 5765, 5802)
for (i in 1:length(outliers.deleted)){
  PC.to.delete <- as.character(unique(data.DV$company_name))[outliers.deleted[i]]
  print(outliers.deleted[i])
  print(PC.to.delete)
  data.DV <- data.DV[-which(data.DV$company_name == PC.to.delete), ]
  print(dim(data.DV))
  # result <- get.weight(data.DV)
  # print(result)
  # lines(result, col = colors[i], type = "b")
}
result <- get.weight(data.DV)
print(result)
lines(result, col = "red", type = "b")

write.csv(data.DV, "PC_AA_DV_9.25.2018.csv", row.names = FALSE)


zouzhe <- function(model){coef(summary(model))$count %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
