setwd("/Volumes/RESEARCH_HD/006/network_data")
# data.IV <- read.csv("PC_AA_IV_1.31.2018.csv")

##### AA as IV #####
library(survival)
cox.exit.0 <- coxph(Surv(start.time, end.time, exit=="1") ~ Market_PE + IPOs_hotness_by_year + MnAs_hotness_by_year + 
                    amt.sum + cum_n.firm + round + OI.match + OM.match + SI.match +
                    + strata(Company.State.Region) 
                    + cluster(company_name)
                    , data = data.IV)
zou.cox <- function(model){result <- coef(summary(model)) %>% add.p.z %>% `[`(c(1,2,5,8)) %>% add.sig(Pr.col = 4)}
cox.exit.1 <- update(cox.exit.0, .~. + company_eigen)
cox.exit.2 <- update(cox.exit.1, .~. + status.sum)
cox.exit.3 <- update(cox.exit.2, .~. + e_success.sum)
cox.exit.4 <- update(cox.exit.3, .~. + rolling.n.acq.overall)
cox.exit.5 <- update(cox.exit.4, .~. + rolling.n.ali.overall)
cox.exit.6 <- update(cox.exit.5, .~. + rolling.n.acq.overall * rolling.n.ali.overall)
 m.exit.0 <- zou.cox(cox.exit.0) %>% format.reg.table(d=2)
 m.exit.1 <- zou.cox(cox.exit.1) %>% format.reg.table(d=2)
 m.exit.2 <- zou.cox(cox.exit.2) %>% format.reg.table(d=2)
 m.exit.3 <- zou.cox(cox.exit.3) %>% format.reg.table(d=2)
 m.exit.4 <- zou.cox(cox.exit.4) %>% format.reg.table(d=2)
 m.exit.5 <- zou.cox(cox.exit.5) %>% format.reg.table(d=2)
 m.exit.6 <- zou.cox(cox.exit.6) %>% format.reg.table(d=2)
cb.e <- combine.result(tbl_1=m.exit.0, tbl_2=m.exit.1, tbl_3=m.exit.2,tbl_4=m.exit.3,tbl_5=m.exit.4,tbl_6=m.exit.5,tbl_7=m.exit.6, n.tbl=7)
names(cb.e) <- c("Variables",paste0("Model ", 0:6))
 cox.exit.0 <- coxph(Surv(start.time, end.time, exit=="1") ~ Market_PE + IPOs_hotness_by_year + MnAs_hotness_by_year + amt.sum + cum_n.firm + round + OI.match + OM.match + SI.match + strata(Company.State.Region), data = data.IV)
 zou.cox <- function(model){result <- coef(summary(model)) %>% add.p.z %>% `[`(c(1,2,5,8)) %>% add.sig(Pr.col = 4)}
 cox.exit.1 <- update(cox.exit.0, .~. + company_eigen)
 cox.exit.2 <- update(cox.exit.1, .~. + status.sum)
 cox.exit.3 <- update(cox.exit.2, .~. + e_success.sum)
 cox.exit.4 <- update(cox.exit.3, .~. + rolling.n.acq.overall)
 cox.exit.5 <- update(cox.exit.4, .~. + rolling.n.ali.overall)
 cox.exit.6 <- update(cox.exit.5, .~. + rolling.n.acq.overall * rolling.n.ali.overall)
cp.e <- compare.models(model1=cox.exit.0, model2= cox.exit.1, model3=cox.exit.2, model4=cox.exit.3, model5=cox.exit.4, model6=cox.exit.5, model7=cox.exit.6, n=7)
names(cp.e) <- c("Variables",paste0("Model ", 0:6))
rt.e <- rbind(cb.e, cp.e)
var.names <- c("S&P Average P/E","IPOs Market Hotness","M&As Market Hotness", "Total Fund Raised (MM)", "Total # VCs Participated", "Total # Rounds Received",
               "1 = Ownership-Status Match","1 = Ownership-Capability Match","1 = Status-Capability Match", 
               "PC Social Status", "VC Social Status", "VC Investment Capability", 
               "Number of Acquisitions (5_y_rolling)", "Number of Alliances(5_y_rolling)",
               "#Acquisitions x VC #Alliance")
rt.e[seq(1,30,2),1] <- var.names
rt.e # View(rt.e)
cox.exit.7$nevent
nrow(data.IV)
length(unique(data.IV$company_name))
usdm::vif(data.IV[names(unlist(cox.exit.5$assign))])
(cor.cox.matrix <- cor.matrix(result.w.full.var=cox.exit.5, nameofdata=data.IV, number.of.IVs=14, y1.name.in.doc="1 = Exit", y2.name.in.doc="1 = M&A", y3.name.in.doc="1 = IPO",
                         y1.name.in.reg="exit",
                         y2.name.in.reg="MnA",
                         y3.name.in.reg="IPO",x.names=var.names, digits=2))
# View(cor.cox.matrix)

cox.IPO.0 <- coxph(Surv(start.time, end.time, event=="IPO") ~ Market_PE + IPOs_hotness_by_year + MnAs_hotness_by_year + 
                     amt.sum + cum_n.firm + round + OI.match + OM.match + SI.match +
                     + strata(Company.State.Region) 
                   + cluster(company_name)
                   , data = data.IV)
cox.IPO.1 <- update(cox.IPO.0, .~. + company_eigen)
cox.IPO.2 <- update(cox.IPO.1, .~. + status.sum)
cox.IPO.3 <- update(cox.IPO.2, .~. + e_success.sum)
cox.IPO.4 <- update(cox.IPO.3, .~. + rolling.n.acq.overall)
cox.IPO.5 <- update(cox.IPO.4, .~. + rolling.n.ali.overall)
cox.IPO.6 <- update(cox.IPO.5, .~. + rolling.n.acq.overall * rolling.n.ali.overall)
m.IPO.0 <- zou.cox(cox.IPO.0) %>% format.reg.table(d=2)
m.IPO.1 <- zou.cox(cox.IPO.1) %>% format.reg.table(d=2)
m.IPO.2 <- zou.cox(cox.IPO.2) %>% format.reg.table(d=2)
m.IPO.3 <- zou.cox(cox.IPO.3) %>% format.reg.table(d=2)
m.IPO.4 <- zou.cox(cox.IPO.4) %>% format.reg.table(d=2)
m.IPO.5 <- zou.cox(cox.IPO.5) %>% format.reg.table(d=2)
m.IPO.6 <- zou.cox(cox.IPO.6) %>% format.reg.table(d=2)
cb.i <- combine.result(tbl_1=m.IPO.0, tbl_2=m.IPO.1, n.tbl=7, tbl_3=m.IPO.2,tbl_4=m.IPO.3,tbl_5=m.IPO.4,tbl_6=m.IPO.5,tbl_7=m.IPO.6)
names(cb.i) <- c("Variables",paste0("Model ", 0:6))
cox.IPO.0 <- coxph(Surv(start.time, end.time, event=="IPO") ~ Market_PE + IPOs_hotness_by_year + MnAs_hotness_by_year + amt.sum + cum_n.firm + round + OI.match + OM.match + SI.match + strata(Company.State.Region), data = data.IV)
cox.IPO.1 <- update(cox.IPO.0, .~. + company_eigen)
cox.IPO.2 <- update(cox.IPO.1, .~. + status.sum)
cox.IPO.3 <- update(cox.IPO.2, .~. + e_success.sum)
cox.IPO.4 <- update(cox.IPO.3, .~. + rolling.n.acq.overall)
cox.IPO.5 <- update(cox.IPO.4, .~. + rolling.n.ali.overall)
cox.IPO.6 <- update(cox.IPO.5, .~. + rolling.n.acq.overall * rolling.n.ali.overall)
cp.i <- compare.models(model1=cox.IPO.0, model2= cox.IPO.1, model3=cox.IPO.2, model4=cox.IPO.3, model5=cox.IPO.4, model6=cox.IPO.5, model7=cox.IPO.6, n=7)
names(cp.i) <- c("Variables",paste0("Model ", 0:6))
rt.i <- rbind(cb.i, cp.i)
rt.i[seq(1,30,2),1] <- var.names
rt.i # View(rt.i)
cox.IPO.5$nevent

cox.MnA.0 <- coxph(Surv(start.time, end.time, event=="MnA") ~ Market_PE + IPOs_hotness_by_year + MnAs_hotness_by_year + 
                     amt.sum + cum_n.firm + round + OI.match + OM.match + SI.match +
                     + strata(Company.State.Region) 
                   + cluster(company_name)
                   , data = data.IV)
cox.MnA.1 <- update(cox.MnA.0, .~. + company_eigen)
cox.MnA.2 <- update(cox.MnA.1, .~. + status.sum)
cox.MnA.3 <- update(cox.MnA.2, .~. + e_success.sum)
cox.MnA.4 <- update(cox.MnA.3, .~. + rolling.n.acq.overall)
cox.MnA.5 <- update(cox.MnA.4, .~. + rolling.n.ali.overall)
cox.MnA.6 <- update(cox.MnA.5, .~. + rolling.n.acq.overall * rolling.n.ali.overall)
m.MnA.0 <- zou.cox(cox.MnA.0) %>% format.reg.table(d=2)
m.MnA.1 <- zou.cox(cox.MnA.1) %>% format.reg.table(d=2)
m.MnA.2 <- zou.cox(cox.MnA.2) %>% format.reg.table(d=2)
m.MnA.3 <- zou.cox(cox.MnA.3) %>% format.reg.table(d=2)
m.MnA.4 <- zou.cox(cox.MnA.4) %>% format.reg.table(d=2)
m.MnA.5 <- zou.cox(cox.MnA.5) %>% format.reg.table(d=2)
m.MnA.6 <- zou.cox(cox.MnA.6) %>% format.reg.table(d=2)
cb.m <- combine.result(tbl_1=m.MnA.0, tbl_2=m.MnA.1, n.tbl=7, tbl_3=m.MnA.2,tbl_4=m.MnA.3,tbl_5=m.MnA.4,tbl_6=m.MnA.5,tbl_7=m.MnA.6)
names(cb.m) <- c("Variables",paste0("Model ", 0:6))
cox.MnA.0 <- coxph(Surv(start.time, end.time, event=="MnA") ~ Market_PE + MnAs_hotness_by_year + MnAs_hotness_by_year + amt.sum + cum_n.firm + round + OI.match + OM.match + SI.match + strata(Company.State.Region), data = data.IV)
cox.MnA.1 <- update(cox.MnA.0, .~. + company_eigen)
cox.MnA.2 <- update(cox.MnA.1, .~. + e_success.sum)
cox.MnA.3 <- update(cox.MnA.2, .~. + status.sum)
cox.MnA.4 <- update(cox.MnA.3, .~. + rolling.n.acq.overall)
cox.MnA.5 <- update(cox.MnA.4, .~. + rolling.n.ali.overall)
cox.MnA.6 <- update(cox.MnA.5, .~. + rolling.n.acq.overall * rolling.n.ali.overall)
cp.m <- compare.models(model1=cox.MnA.0, model2= cox.MnA.1, model3=cox.MnA.2, model4=cox.MnA.3, model5=cox.MnA.4, model6=cox.MnA.5, model7=cox.MnA.6, n=7)
names(cp.m) <- c("Variables",paste0("Model ", 0:6))
rt.m <- rbind(cb.m, cp.m)
rt.m[seq(1,30,2),1] <- var.names
rt.m # View(rt.m)
cox.MnA.5$nevent
### AA as DV ###
# library(pscl) # zeroinfl


##### AA as DV #####
setwd("/Volumes/RESEARCH_HD/006/network_data")
# data.DV <- read.csv("PC_AA_DV_2.9.2018.csv")
# data.DV <- read.csv("PC_AA_DV_3.1.2018.csv")
# data.DV <- read.csv("PC_AA_DV_Apr.11.2018.csv")
# data.DV <- read.csv("PC_AA_DV_May.04.2018.csv")
data.DV <- read.csv("PC_AA_DV_Jul.01.2018.csv")
library(MASS)

##### DV=Acquisition #####
acq.0 <- glm.nb(n.acq.overall ~ Market_PE + IPOs_hotness_by_year + MnAs_hotness_by_year + 
               amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + factor(year), 
               data = data.DV)
library(lme4)
acq.0 <- glmer.nb(n.acq.overall ~ Market_PE + IPOs_hotness_by_year + MnAs_hotness_by_year + 
                  amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + 
                    (1|year) + (1|industry), 
                data = data.DV)

zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
acq.1 <- update(acq.0, .~. + company_eigen)
acq.2 <- update(acq.1, .~. + status.sum)
acq.3 <- update(acq.2, .~. + e_success.sum)
acq.4 <- update(acq.3, .~. + company_eigen * status.sum)
acq.5 <- update(acq.4, .~. + company_eigen * e_success.sum)
 m.acq.0 <- zouzhe(acq.0) %>% format.reg.table(d=2)
 m.acq.1 <- zouzhe(acq.1) %>% format.reg.table(d=2)
 m.acq.2 <- zouzhe(acq.2) %>% format.reg.table(d=2)
 m.acq.3 <- zouzhe(acq.3) %>% format.reg.table(d=2)
 m.acq.4 <- zouzhe(acq.4) %>% format.reg.table(d=2)
 m.acq.5 <- zouzhe(acq.5) %>% format.reg.table(d=2)
cb.acq <- combine.result(tbl_1=m.acq.0, tbl_2=m.acq.1, n.tbl=6, tbl_3=m.acq.2, tbl_4=m.acq.3, tbl_5=m.acq.4, tbl_6=m.acq.5)
names(cb.acq) <- c("Variables",paste0("Model ", 0:5)) 
cp.acq<- compare.models(model1=acq.0, model2= acq.1, model3=acq.2, model4=acq.3, model5=acq.4, model6=acq.5,  n=6)
names(cp.acq) <- c("Variables",paste0("Model ", 0:5))
rt.acq <- rbind(cb.acq, cp.acq)
var.names.aa <- c(var.names[1:12],"PC status x VC status","PC status x VC capability")
rt.acq[seq(3,30,2),1] <- var.names.aa
 rt.acq # View(rt.acq)
nobs(acq.5)

##### DV=Alliance #####
scale01 <- function(x){(x-min(x))/(max(x)-min(x))}
data.DV$company_eigen  <- scale01(DescTools::Winsorize(data.DV$company_eigen,  probs = c(0.005, 0.995), na.rm = TRUE))
data.DV$alliance_eigen <- scale01(DescTools::Winsorize(data.DV$alliance_eigen, probs = c(0.005, 0.995), na.rm = TRUE))
data.DV$amt.sum <- data.DV$amt.sum/1000

data.DV$status.sum_scale     <- scale(data.DV$status.cum)
data.DV$company_eigen_scale  <- scale(data.DV$company_eigen)
data.DV$alliance_eigen_scale <- scale(data.DV$alliance_eigen)
data.DV$multiplicative <- data.DV$status.sum_scale * data.DV$company_eigen_scale * data.DV$alliance_eigen_scale
data.DV$additive       <- data.DV$status.sum_scale + data.DV$company_eigen_scale + data.DV$alliance_eigen_scale

##### Horizontal vs. Vertical Structrual Hole #####
ali.0 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year) + company_constraint, control=glm.control(maxit=100),
                data = data.DV)
ali.1 <-  update(ali.0, .~. + constraint.sum)
ali.2 <-  update(ali.0, .~. + endorse.constraint)
ali.3 <-  update(ali.0, .~. + constraint.sum + endorse.constraint)
ali.4 <-  update(ali.0, .~. + constraint.sum * endorse.constraint)
ali.5 <-  update(ali.3, .~. + constraint.sum * company_constraint)
ali.6 <-  update(ali.0, .~. + constraint.sum * endorse.constraint + constraint.sum  * company_constraint)
# ali.7 <-  update(ali.0, .~. + constraint.cum * endorse.constraint + constraint.cum  * company_constraint)
# ali.7 <-  update(ali.0, .~. + endorse.eigen * company_eigen) # + company_eigen * alliance_eigen
# ali.8 <-  update(ali.0, .~. + status.cum    * alliance_constraint)
# ali.9 <-  update(ali.0, .~. + endorse.eigen * alliance_constraint) # + company_eigen * alliance_eigen
# ali.10 <- update(ali.0, .~. + status.cum    * company_constraint)
# ali.11 <- update(ali.0, .~. + endorse.eigen * company_constraint) # + company_eigen * alliance_eigen
# ali.12 <- update(ali.0, .~. + status.cum * endorse.eigen + status.cum * alliance_eigen + endorse.eigen * alliance_eigen + status.cum * company_eigen + endorse.eigen * company_eigen + status.cum * alliance_constraint + endorse.eigen * alliance_constraint) # + company_eigen * alliance_eigen
zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
m.ali.0  <- zouzhe(ali.0) %>% format.reg.table(d=3)
m.ali.1  <- zouzhe(ali.1) %>% format.reg.table(d=3)
m.ali.2  <- zouzhe(ali.2) %>% format.reg.table(d=3)
m.ali.3  <- zouzhe(ali.3) %>% format.reg.table(d=3)
m.ali.4  <- zouzhe(ali.4) %>% format.reg.table(d=3)
m.ali.5  <- zouzhe(ali.5) %>% format.reg.table(d=3)
m.ali.6  <- zouzhe(ali.6) %>% format.reg.table(d=3)
# m.ali.7  <- zouzhe(ali.7) %>% format.reg.table(d=3)
# m.ali.8  <- zouzhe(ali.8) %>% format.reg.table(d=3)
# m.ali.9  <- zouzhe(ali.9) %>% format.reg.table(d=3)
# m.ali.10 <- zouzhe(ali.10) %>% format.reg.table(d=3)
# m.ali.11 <- zouzhe(ali.11) %>% format.reg.table(d=3)
# m.ali.12 <- zouzhe(ali.12) %>% format.reg.table(d=3)
cb.ali <- Combine.Result(tbl_1=m.ali.0, tbl_2=m.ali.1, tbl_3=m.ali.2, tbl_4=m.ali.3, tbl_5=m.ali.4, 
                         tbl_6=m.ali.5, tbl_7=m.ali.6,
                         # tbl_8=m.ali.7, tbl_9=m.ali.8, tbl_10=m.ali.9, 
                         # tbl_11=m.ali.10, tbl_12=m.ali.11, 
                         n.tbl=7)
names(cb.ali) <- c("Variables", paste0("Model ", 0:6)) 
cb.ali <- cb.ali[-c(19:66), ]
# cp.ali <- compare.models(model1=ali.0, model2= ali.1, model3=ali.2, model4=ali.3, model5=ali.4, model6=ali.5, model7=ali.7, n=7,
#                          main.effect.only = c(3,4),
#                          intn.effect.only = c(5,6), N_main_to_intn = 2)
# names(cp.ali) <- c("Variables", paste0("Model ", 0:9))
cp.ali <- c("logLik", round(unlist(map(list(ali.0, ali.1, ali.2, ali.3, ali.4, ali.5, ali.6), logLik)), 3))
rt.ali <- rbind(cb.ali, cp.ali)
var.names.aa <- c("Total Fund Raised (MM)", "Total # VCs Participated", "Total # Rounds Received",
                  "1 = Ownership-Status Match","1 = Ownership-Capability Match","1 = Status-Capability Match", "Alliance Experience", "VC Investment Capability", "Self Prominence (PC in VC Two-mode)",
                  "Between Endorsement (VC in VC Two-mode)", "Within Endorsement (Alliance One-mode)", 
                  "Between Endorsement x Within Endorsement",
                  "Between Endorsement x Self Prominence")
rt.ali[seq(3,nrow(cb.ali),2),1] <- var.names.aa

#### two-mode vs. one-mode structural hole ####
outliners <- as.character(unique(data.DV$company_name))[c(448, 904, 1161, 2334, 5432, 7035)]
data <- data.DV[-which(data.DV$company_name %in% outliners),]
ali.0 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year) + constraint.cum, control=glm.control(maxit=100),
                data = data)
ali.1 <-  update(ali.0, .~. + company_constraint)
ali.2 <-  update(ali.0, .~. + alliance_constraint)
ali.3 <-  update(ali.0, .~. + company_constraint + alliance_constraint)
ali.4 <-  update(ali.3, .~. + company_constraint * constraint.cum)
ali.5 <-  update(ali.3, .~. + alliance_constraint * constraint.cum)
ali.6 <-  update(ali.0, .~. + company_constraint * alliance_constraint)
ali.7 <-  update(ali.0, .~. + company_constraint  * constraint.cum + alliance_constraint * constraint.cum + company_constraint * alliance_constraint) # + company_eigen * alliance_eigen
# ali.8 <-  update(ali.0, .~. + status.cum    * alliance_constraint)
# ali.9 <-  update(ali.0, .~. + endorse.eigen * alliance_constraint) # + company_eigen * alliance_eigen
# ali.10 <- update(ali.0, .~. + status.cum    * company_constraint)
# ali.11 <- update(ali.0, .~. + endorse.eigen * company_constraint) # + company_eigen * alliance_eigen
# ali.12 <- update(ali.0, .~. + status.cum * endorse.eigen + status.cum * alliance_eigen + endorse.eigen * alliance_eigen + status.cum * company_eigen + endorse.eigen * company_eigen + status.cum * alliance_constraint + endorse.eigen * alliance_constraint) # + company_eigen * alliance_eigen
zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
m.ali.0  <- zouzhe(ali.0) %>% format.reg.table(d=3)
m.ali.1  <- zouzhe(ali.1) %>% format.reg.table(d=3)
m.ali.2  <- zouzhe(ali.2) %>% format.reg.table(d=3)
m.ali.3  <- zouzhe(ali.3) %>% format.reg.table(d=3)
m.ali.4  <- zouzhe(ali.4) %>% format.reg.table(d=3)
m.ali.5  <- zouzhe(ali.5) %>% format.reg.table(d=3)
m.ali.6  <- zouzhe(ali.6) %>% format.reg.table(d=3)
m.ali.7  <- zouzhe(ali.7) %>% format.reg.table(d=3)
# m.ali.8  <- zouzhe(ali.8) %>% format.reg.table(d=3)
# m.ali.9  <- zouzhe(ali.9) %>% format.reg.table(d=3)
# m.ali.10 <- zouzhe(ali.10) %>% format.reg.table(d=3)
# m.ali.11 <- zouzhe(ali.11) %>% format.reg.table(d=3)
# m.ali.12 <- zouzhe(ali.12) %>% format.reg.table(d=3)
cb.ali.1 <- Combine.Result(tbl_1=m.ali.0, tbl_2=m.ali.1, tbl_3=m.ali.2, tbl_4=m.ali.3, tbl_5=m.ali.4, 
                           tbl_6=m.ali.5, tbl_7=m.ali.6, tbl_8=m.ali.7,
                           # tbl_8=m.ali.7, tbl_9=m.ali.8, tbl_10=m.ali.9, 
                           # tbl_11=m.ali.10, tbl_12=m.ali.11, 
                           n.tbl=8)
names(cb.ali.1) <- c("Variables", paste0("Model ", 0:7)) 
cb.ali.1 <- cb.ali.1[-c(19:66), ]
# cp.ali <- compare.models(model1=ali.0, model2= ali.1, model3=ali.2, model4=ali.3, model5=ali.4, model6=ali.5, model7=ali.7, n=7,
#                          main.effect.only = c(3,4),
#                          intn.effect.only = c(5,6), N_main_to_intn = 2)
# names(cp.ali) <- c("Variables", paste0("Model ", 0:9))
cp.ali.1 <- c("logLik", round(unlist(map(list(ali.0, ali.1, ali.2, ali.3, ali.4, ali.5, ali.6, ali.7), logLik)), 3))
rt.ali.1 <- rbind(cb.ali.1, cp.ali.1)
var.names.aa.1 <- c("Total Fund Raised (MM)", "Total # VCs Participated", "Total # Rounds Received",
                    "1 = Ownership-Status Match","1 = Ownership-Capability Match","1 = Status-Capability Match", "Alliance Experience", "VC Investment Capability", 
                    "VC Constraint in VC Network", "PC Constraint in VC Network", "PC Constraint in Alliance Network",
                    "PC Constraint in VC x VC Constraint in VC", "PC Constraint in Alliance x VC Constraint in VC", "PC Constraint in VC x PC Constraint in Alliance")
rt.ali.1[seq(3,nrow(cb.ali.1),2),1] <- var.names.aa.1
rt.ali.1

# delete outlier #
# data <- data.DV
# dim(data)
# start <- Sys.time()
# to.delete <- c()
# repeat{
#   PC.list <- as.character(unique(data$company_name))
#   candidate.pool <- purrr::map(PC.list, function(x){which(data$company_name == x)})
#   compare.impact <- do.call(rbind, purrr::map(candidate.pool, impact)) # 1:nrow(data)
#   weight <- get.weight(data)
#   delete.this.round <- PC.list[which.max(weight %*% t(compare.impact))]
#   to.delete <- c(to.delete, delete.this.round)
#   data <- if(!is.na(delete.this.round)){data[-which(data$company_name == delete.this.round),]}else{data}
#   if(!is.na(delete.this.round)){
#     print(delete.this.round)
#     print(weight)}
#   print(Sys.time() - start)
#   if(max(weight) < 0.05){break}
# }
# Sys.time() - start

paste(to.delete, collapse = ", ")
to.delete

##### Horizontal vs. Vertical Endorsement #####
ali.0 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year) + company_eigen, control=glm.control(maxit=100),
                data = data.DV)
ali.1 <-  update(ali.0, .~. + status.cum)
ali.2 <-  update(ali.0, .~. + endorse.eigen)
ali.3 <-  update(ali.0, .~. + status.cum + endorse.eigen)
ali.4 <-  update(ali.0, .~. + status.cum * endorse.eigen)
ali.5 <-  update(ali.3, .~. + status.cum * company_eigen)
ali.6 <-  update(ali.0, .~. + status.cum * endorse.eigen + status.cum  * company_eigen)
ali.7 <-  update(ali.0, .~. + status.cum * endorse.eigen + status.cum  * company_eigen)
# ali.7 <-  update(ali.0, .~. + endorse.eigen * company_eigen) # + company_eigen * alliance_eigen
# ali.8 <-  update(ali.0, .~. + status.cum    * alliance_constraint)
# ali.9 <-  update(ali.0, .~. + endorse.eigen * alliance_constraint) # + company_eigen * alliance_eigen
# ali.10 <- update(ali.0, .~. + status.cum    * company_constraint)
# ali.11 <- update(ali.0, .~. + endorse.eigen * company_constraint) # + company_eigen * alliance_eigen
# ali.12 <- update(ali.0, .~. + status.cum * endorse.eigen + status.cum * alliance_eigen + endorse.eigen * alliance_eigen + status.cum * company_eigen + endorse.eigen * company_eigen + status.cum * alliance_constraint + endorse.eigen * alliance_constraint) # + company_eigen * alliance_eigen
zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
m.ali.0  <- zouzhe(ali.0) %>% format.reg.table(d=3)
m.ali.1  <- zouzhe(ali.1) %>% format.reg.table(d=3)
m.ali.2  <- zouzhe(ali.2) %>% format.reg.table(d=3)
m.ali.3  <- zouzhe(ali.3) %>% format.reg.table(d=3)
m.ali.4  <- zouzhe(ali.4) %>% format.reg.table(d=3)
m.ali.5  <- zouzhe(ali.5) %>% format.reg.table(d=3)
m.ali.6  <- zouzhe(ali.6) %>% format.reg.table(d=3)
m.ali.7  <- zouzhe(ali.7) %>% format.reg.table(d=3)
# m.ali.8  <- zouzhe(ali.8) %>% format.reg.table(d=3)
# m.ali.9  <- zouzhe(ali.9) %>% format.reg.table(d=3)
# m.ali.10 <- zouzhe(ali.10) %>% format.reg.table(d=3)
# m.ali.11 <- zouzhe(ali.11) %>% format.reg.table(d=3)
# m.ali.12 <- zouzhe(ali.12) %>% format.reg.table(d=3)
cb.ali <- Combine.Result(tbl_1=m.ali.0, tbl_2=m.ali.1, tbl_3=m.ali.2, tbl_4=m.ali.3, tbl_5=m.ali.4, 
                         tbl_6=m.ali.5, tbl_7=m.ali.6,
                         # tbl_8=m.ali.7, tbl_9=m.ali.8, tbl_10=m.ali.9, 
                         # tbl_11=m.ali.10, tbl_12=m.ali.11, 
                         n.tbl=7)
names(cb.ali) <- c("Variables", paste0("Model ", 0:6)) 
cb.ali <- cb.ali[-c(19:66), ]
# cp.ali <- compare.models(model1=ali.0, model2= ali.1, model3=ali.2, model4=ali.3, model5=ali.4, model6=ali.5, model7=ali.7, n=7,
#                          main.effect.only = c(3,4),
#                          intn.effect.only = c(5,6), N_main_to_intn = 2)
# names(cp.ali) <- c("Variables", paste0("Model ", 0:9))
cp.ali <- c("logLik", round(unlist(map(list(ali.0, ali.1, ali.2, ali.3, ali.4, ali.5, ali.6), logLik)), 3))
rt.ali <- rbind(cb.ali, cp.ali)
var.names.aa <- c("Total Fund Raised (MM)", "Total # VCs Participated", "Total # Rounds Received",
                  "1 = Ownership-Status Match","1 = Ownership-Capability Match","1 = Status-Capability Match", "Alliance Experience", "VC Investment Capability", "Self Prominence (PC in VC Two-mode)",
                  "Between Endorsement (VC in VC Two-mode)", "Within Endorsement (Alliance One-mode)", 
                  "Between Endorsement x Within Endorsement",
                  "Between Endorsement x Self Prominence")
rt.ali[seq(3,nrow(cb.ali),2),1] <- var.names.aa

##### two-mode vs. one-mode Endorsement #####
ali.0 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year) + status.cum, control=glm.control(maxit=100),
                data = data.DV)
ali.1 <-  update(ali.0, .~. + company_eigen)
ali.2 <-  update(ali.0, .~. + alliance_eigen)
ali.3 <-  update(ali.0, .~. + company_eigen + alliance_eigen)
ali.4 <-  update(ali.3, .~. + company_eigen  * status.cum)
ali.5 <-  update(ali.3, .~. + alliance_eigen * status.cum)
ali.6 <-  update(ali.0, .~. + company_eigen * alliance_eigen)
ali.7 <-  update(ali.0, .~. + company_eigen * alliance_eigen + company_eigen  * status.cum + alliance_eigen * status.cum) # + company_eigen * alliance_eigen
# ali.8 <-  update(ali.0, .~. + status.cum    * alliance_constraint)
# ali.9 <-  update(ali.0, .~. + endorse.eigen * alliance_constraint) # + company_eigen * alliance_eigen
# ali.10 <- update(ali.0, .~. + status.cum    * company_constraint)
# ali.11 <- update(ali.0, .~. + endorse.eigen * company_constraint) # + company_eigen * alliance_eigen
# ali.12 <- update(ali.0, .~. + status.cum * endorse.eigen + status.cum * alliance_eigen + endorse.eigen * alliance_eigen + status.cum * company_eigen + endorse.eigen * company_eigen + status.cum * alliance_constraint + endorse.eigen * alliance_constraint) # + company_eigen * alliance_eigen
zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
m.ali.0  <- zouzhe(ali.0) %>% format.reg.table(d=3)
m.ali.1  <- zouzhe(ali.1) %>% format.reg.table(d=3)
m.ali.2  <- zouzhe(ali.2) %>% format.reg.table(d=3)
m.ali.3  <- zouzhe(ali.3) %>% format.reg.table(d=3)
m.ali.4  <- zouzhe(ali.4) %>% format.reg.table(d=3)
m.ali.5  <- zouzhe(ali.5) %>% format.reg.table(d=3)
m.ali.6  <- zouzhe(ali.6) %>% format.reg.table(d=3)
m.ali.7  <- zouzhe(ali.7) %>% format.reg.table(d=3)
# m.ali.8  <- zouzhe(ali.8) %>% format.reg.table(d=3)
# m.ali.9  <- zouzhe(ali.9) %>% format.reg.table(d=3)
# m.ali.10 <- zouzhe(ali.10) %>% format.reg.table(d=3)
# m.ali.11 <- zouzhe(ali.11) %>% format.reg.table(d=3)
# m.ali.12 <- zouzhe(ali.12) %>% format.reg.table(d=3)
cb.ali.1 <- Combine.Result(tbl_1=m.ali.0, tbl_2=m.ali.1, tbl_3=m.ali.2, tbl_4=m.ali.3, tbl_5=m.ali.4, 
                         tbl_6=m.ali.5, tbl_7=m.ali.6, tbl_8=m.ali.7,
                         # tbl_8=m.ali.7, tbl_9=m.ali.8, tbl_10=m.ali.9, 
                         # tbl_11=m.ali.10, tbl_12=m.ali.11, 
                         n.tbl=8)
names(cb.ali.1) <- c("Variables", paste0("Model ", 0:7)) 
cb.ali.1 <- cb.ali.1[-c(19:66), ]
# cp.ali <- compare.models(model1=ali.0, model2= ali.1, model3=ali.2, model4=ali.3, model5=ali.4, model6=ali.5, model7=ali.7, n=7,
#                          main.effect.only = c(3,4),
#                          intn.effect.only = c(5,6), N_main_to_intn = 2)
# names(cp.ali) <- c("Variables", paste0("Model ", 0:9))
cp.ali.1 <- c("logLik", round(unlist(map(list(ali.0, ali.1, ali.2, ali.3, ali.4, ali.5, ali.6, ali.7), logLik)), 3))
rt.ali.1 <- rbind(cb.ali.1, cp.ali.1)
var.names.aa.1 <- c("Total Fund Raised (MM)", "Total # VCs Participated", "Total # Rounds Received",
                  "1 = Ownership-Status Match","1 = Ownership-Capability Match","1 = Status-Capability Match", "Alliance Experience", "VC Investment Capability", 
                  "VC Endorsement in VC (Two-mode)", "PC Prominence in VC (Two-mode)", "PC Prominence in Alliance (One-mode)",
                  "PC Prominence in VC x VC Endorsement in VC", "PC Prominence in Alliance x VC Endorsement in VC", "PC Prominence in VC x PC Prominence in Alliance")
rt.ali.1[seq(3,nrow(cb.ali.1),2),1] <- var.names.aa.1

VIF <- compile.mgmt::reg.Vif(ali.12$model[,-13])
VIF$Variables <- c("Alliance Formation", var.names.aa[1:10], "PC VC-network Centrality", "PC Alliance Constraint")
cor.test(ali.12$model$company_eigen, ali.12$model$endorse.eigen)
compile.mgmt::reg.Cor(ali.12$model)

# Wald test #
v <- vcov(ali.6)
(c <- coef(ali.6))
dif <- c[34] - c[35]
se <- sqrt( v[34,34] + v[35,35] - 2*v[34,35] )
pnorm(dif/se) #This is standard normal distributed

##### Modeling #####
ali.0 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year), control=glm.control(maxit=100),
                data = data.DV)
ali.1 <- update(ali.0, .~. + status.cum)
ali.2 <- update(ali.0, .~. + company_eigen)
ali.3 <- update(ali.0, .~. + alliance_eigen)
ali.multiplicative <- update(ali.0, .~. + multiplicative)
ali.additive       <- update(ali.0, .~. + additive)
ali.4 <- update(ali.1, .~. + status.cum * company_eigen)
ali.5 <- update(ali.1, .~. + status.cum * alliance_eigen)
# ali.6 <- update(ali.1, .~. + company_eigen * alliance_eigen)
ali.7 <- update(ali.1, .~. + status.cum + company_eigen + alliance_eigen + status.cum * company_eigen + status.cum * alliance_eigen) # + company_eigen * alliance_eigen
zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
m.ali.0 <- zouzhe(ali.0) %>% format.reg.table(d=3)
m.ali.1 <- zouzhe(ali.1) %>% format.reg.table(d=3)
m.ali.2 <- zouzhe(ali.2) %>% format.reg.table(d=3)
m.ali.3 <- zouzhe(ali.3) %>% format.reg.table(d=3)
m.ali.multiplicative <- zouzhe(ali.multiplicative) %>% format.reg.table(d=3)
m.ali.additive       <- zouzhe(ali.additive)       %>% format.reg.table(d=3)
m.ali.4 <- zouzhe(ali.4) %>% format.reg.table(d=3)
m.ali.5 <- zouzhe(ali.5) %>% format.reg.table(d=3)
# m.ali.6 <- zouzhe(ali.6) %>% format.reg.table(d=3)
m.ali.7 <- zouzhe(ali.7) %>% format.reg.table(d=3)
 cb.ali <- combine.result(tbl_1=m.ali.0, tbl_2=m.ali.1, tbl_3=m.ali.2, tbl_4=m.ali.3, tbl_5=m.ali.4, tbl_6=m.ali.5, tbl_7=m.ali.7,
                         n.tbl=7,
                         main.effect.only = c(3,4),
                         intn.effect.only = c(5,6))
 names(cb.ali) <- c("Variables",paste0("Model ", 0:6)) 
 cb.ali <- cb.ali[-(19:66),]
 cp.ali <- compare.models(model1=ali.0, model2= ali.1, model3=ali.2, model4=ali.3, model5=ali.4, model6=ali.5, model7=ali.7, n=7,
                         main.effect.only = c(3,4),
                         intn.effect.only = c(5,6), N_main_to_intn = 2)
 names(cp.ali) <- c("Variables",paste0("Model ", 0:6))
rt.ali <- rbind(cb.ali, cp.ali)
var.names.aa <- c("Total Fund Raised (MM)", "Total # VCs Participated", "Total # Rounds Received",
               "1 = Ownership-Status Match","1 = Ownership-Capability Match","1 = Status-Capability Match", "Alliance Experience", "VC Investment Capability", 
               "VC Status in VC Network", "PC Status in VC Network", "PC Status in Alliance Network",
               "VC Status x PC Status in VC Network", "VC Status x PC Status in Alliance Network")
rt.ali[seq(3,nrow(cb.ali),2),1] <- var.names.aa
  rt.ali # View(rt.ali)
  
  ##### multiplicative and additive #####
  cb.ali <- combine.result(tbl_1=m.ali.0, tbl_2=m.ali.1, tbl_3=m.ali.2, tbl_4=m.ali.3, tbl_5=m.ali.multiplicative, tbl_6=m.ali.additive,
                           n.tbl=6,
                           main.effect.only = c(3,4,5,6))
  names(cb.ali) <- c("Variables",paste0("Model ", 0:5))
  cb.ali <- cb.ali[-(19:66),]
  cp.ali <- compare.models(model1=ali.0, model2= ali.1, model3=ali.2, model4=ali.3, model5=ali.multiplicative, model6=ali.additive, n=6,
                           main.effect.only = c(3,4,5,6))
  names(cp.ali) <- c("Variables",paste0("Model ", 0:5))
  rt.ali <- rbind(cb.ali, cp.ali)
  var.names.aa <- c("Total Fund Raised (MM)", "Total # VCs Participated", "Total # Rounds Received",
                    "1 = Ownership-Status Match","1 = Ownership-Capability Match","1 = Status-Capability Match", "Alliance Experience", "VC Investment Capability",
                    "VC Status in VC Network", "PC Status in VC Network", "PC Status in Alliance Network",
                    "Multiplicative Effect", "Additive Effect")
  rt.ali[seq(3,nrow(cb.ali),2),1] <- var.names.aa
  rt.ali # View(rt.ali)
  
  nobs(ali.5)
  length(unique(data.DV$company_name))
  usdm::vif(ali.5$model)
  usdm::vif(acq.5$model)
  (cor.nbi.matrix <- cor.matrix(result.w.full.var=ali.3, nameofdata=data.DV, number.of.IVs=12, y1.name.in.doc="#ACQs in Subsequent Round", y2.name.in.doc="#ALIs in Subsequent Round",
                                y1.name.in.reg="n.acq.overall",
                                y2.name.in.reg="n.ali.overall", x.names=var.names.aa, digits=2))
  
  ali.cor <- update(ali.0, .~. + status.sum + company_eigen + alliance_eigen)
  (cor.nbi.matrix <- cor.matrix(result.w.full.var=ali.cor, nameofdata=data.DV, number.of.IVs=11, y1.name.in.doc="#ALIs in Subsequent Round",
                                y1.name.in.reg="n.ali.overall", x.names=var.names.aa, digits=2))

  ##### variable selection version 1 #####
   network.v.names <- c("status.cum", "constraint.cum", "company_eigen", "company_constraint", "alliance_eigen", "alliance_constraint")
   depende.v.names <- c("dependence_lead_VC_on_PC", "VC_Invest_HHI", "dependence_PC_on_lead_VC")
   all.var.names <- c(network.v.names, depende.v.names)
   var.df <- combn(all.var.names, 2)
  bottom.x <- 3 # set output: how many bottom rows to check?
  run <- function(i){
    x1 <- eval(parse(text = paste("data.DV$", var.df[1,i], sep="")))
    x2 <- eval(parse(text = paste("data.DV$", var.df[2,i], sep="")))
    ali.0 <- zouzhe(glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + 
                      alliance_experience + e_success.sum + factor(year) + x1 * x2, 
                    control=glm.control(maxit=100),
                    data = data.DV))
    sig <- ali.0[-(1:33),c(2,4:5)]
    return(list(i,var.df[,i],sig))}
  result <- map(1:ncol(var.df),safely(run))
   res <- purrr::map(result,function(x){x[["result"]]})
   err <- purrr::map(result,function(x){x[["error"]]})
   good <- res[purrr::map_lgl(err, is_null)] 
   bad <- which(unlist(purrr::map_lgl(res, is_null))==TRUE)
  significant.main.int.1 <- which(unlist(map(good, function(x){ifelse(x[[3]][3,2] < 0.05, 1, 0)}))==1)
  good[significant.main.int.1]

  ##### PC VC_and_Alliance_Network Structure Hole x Dependence on VC #####
  ali.0 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + dependence_PC_on_lead_VC + dependence_lead_VC_on_PC
                  + factor(year), control=glm.control(maxit=100),
                  data = data.DV)
  ali.1 <- update(ali.0, .~. + I((-1)*company_constraint))
  ali.1. <- update(ali.0, .~. + I((-1)*alliance_constraint))
  ali.2 <- update(ali.0, .~. + I((-1)*company_constraint) * dependence_PC_on_lead_VC + I((-1)*alliance_constraint) )
  ali.3 <- update(ali.0, .~. + I((-1)*company_constraint) + I((-1)*alliance_constraint) * dependence_PC_on_lead_VC)
  ali.4 <- update(ali.2, .~. + (I((-1)*company_constraint) + I((-1)*alliance_constraint)) * dependence_PC_on_lead_VC)

  zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
  m.ali.0 <- zouzhe(ali.0) %>% format.reg.table(d=3)
  m.ali.1 <- zouzhe(ali.1) %>% format.reg.table(d=3)
  m.ali.1. <- zouzhe(ali.1.) %>% format.reg.table(d=3)
  m.ali.2 <- zouzhe(ali.2) %>% format.reg.table(d=3)
  m.ali.3 <- zouzhe(ali.3) %>% format.reg.table(d=3)
  m.ali.4 <- zouzhe(ali.4) %>% format.reg.table(d=3)
   cb.al <- Combine.Result(m.ali.0, m.ali.1, m.ali.1., m.ali.2, m.ali.3, m.ali.4, n.tbl = 6)[-(23:70),]
   cp.al <- compare.models(ali.0, ali.1, ali.1., ali.2, ali.3, ali.4, n=6,
                           main.effect.only = c(3),
                           intn.effect.only = c(4,5), N_main_to_intn = 2)
   names(cb.al) <- c("Variables", paste0("Model ", 0:4), "Model Full")
   names(cp.al) <- c("Variables", paste0("Model ", 0:4), "Model Full")
   rt.al <- rbind(cb.al, cp.al)
   var.names.aa <- c("Total Fund Raised (MM)", "Total # VCs Participated", "Total # Rounds Received",
                     "1 = Ownership-Status Match","1 = Ownership-Capability Match","1 = Status-Capability Match", "Alliance Experience", "VC Investment Capability",
                     "PC dependence on VC", "VC dependence on PC", "PC VC-Network Structure Hole", "PC alliance-Network Structure Hole",
                     "PC VC-Network Structure Hole x PC dependence on VC", "PC alliance-Network Structure Hole x PC dependence on VC")
   rt.al[seq(3,nrow(cb.al),2),1] <- var.names.aa
   rt.al # View(rt.ali)
   
  ##### VC Network x VC Dependence ##### 5/4/2018 #####
   data <- data.DV
   dim(data)
   # data1 <- data[-which(data$company_name == "Fisker Automotive Inc"),]
   data1 <- data[-2444, ]
   ali.0 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + VC_Invest_HHI + dependence_ALL_VC_on_PC # dependence_PC_on_lead_VC + dependence_lead_VC_on_PC
                   + factor(year), control=glm.control(maxit=100),
                   data = data1)
   ali.1  <- update(ali.0, .~. + I(status.sum))
   ali.1. <- update(ali.0, .~. + I((-1)*constraint.sum))
   ali.2 <- update(ali.0, .~. + I(status.sum) * dependence_ALL_VC_on_PC)
   ali.3 <- update(ali.0, .~. + I((-1)*constraint.sum) * dependence_ALL_VC_on_PC)  # dependence_lead_VC_on_PC
   ali.4 <- update(ali.3, .~. + I(status.sum) * dependence_ALL_VC_on_PC)
   ali.4 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + VC_Invest_HHI + dependence_ALL_VC_on_PC # dependence_PC_on_lead_VC + dependence_lead_VC_on_PC
                   + I((-1)*constraint.sum) * dependence_ALL_VC_on_PC
                   + I(status.sum) * dependence_ALL_VC_on_PC
                   + factor(year), control=glm.control(maxit=100),
                   data = data1)
   
   zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
   m.ali.0 <- zouzhe(ali.0) %>% format.reg.table(d=3)
   m.ali.1 <- zouzhe(ali.1) %>% format.reg.table(d=3)
   m.ali.1. <- zouzhe(ali.1.) %>% format.reg.table(d=3)
   m.ali.2 <- zouzhe(ali.2) %>% format.reg.table(d=3)
   m.ali.3 <- zouzhe(ali.3) %>% format.reg.table(d=3)
   m.ali.4 <- zouzhe(ali.4) %>% format.reg.table(d=3)
   # m.ali.4[77,2] <- "I(status.sum):dependence_ALL_VC_on_PC"   
   cb.al <- Combine.Result(m.ali.0, m.ali.1, m.ali.1., m.ali.2, m.ali.3, m.ali.4, n.tbl = 6)[-(23:70),]
   cp.al <- compare.models(model1 = ali.0, model2 = ali.1, model3 = ali.1., model4 = ali.2, model5 = ali.3, model6 = ali.4, n=6,
                           main.effect.only = c(3),
                           intn.effect.only = c(4,5), N_main_to_intn = 2)
   names(cb.al) <- c("Variables", paste0("Model ", 0:4), "Model Full")
   names(cp.al) <- c("Variables", paste0("Model ", 0:4), "Model Full")
   rt.al <- rbind(cb.al, cp.al)
   var.names.aa <- c("Total Fund Raised (MM)", "Total # VCs Participated", "Total # Rounds Received",
                     "1 = Ownership-Status Match","1 = Ownership-Capability Match","1 = Status-Capability Match", "Alliance Experience", "VC Investment Capability",
                     "VC Investment HHI (Concentration)", "VC dependence on PC", "VC Social Status", "VC Structure Hole",
                     "VC Social Status x VC dependence on PC", "VC Structure Hole x VC dependence on PC")
   rt.al[seq(3,nrow(cb.al),2),1] <- var.names.aa
   rt.al # View(rt.ali)
   
   
   ##########
   data1 <- data[-2444, ]
   ali.0 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + VC_Invest_HHI + dependence_ALL_VC_on_PC # dependence_PC_on_lead_VC + dependence_lead_VC_on_PC
                   + factor(year), control=glm.control(maxit=100),
                   data = data1)
   ali.1  <- update(ali.0, .~. + I(status.sum))
   ali.1. <- update(ali.0, .~. + I((-1)*constraint.sum))
   ali.2 <- update(ali.0, .~. + I(status.sum) * dependence_ALL_VC_on_PC)
   ali.3 <- update(ali.0, .~. + I((-1)*constraint.sum) * dependence_ALL_VC_on_PC)  # dependence_lead_VC_on_PC
   ali.4 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + VC_Invest_HHI + dependence_ALL_VC_on_PC # dependence_PC_on_lead_VC + dependence_lead_VC_on_PC
                   + (e_success.sum + status.sum + I((-1)*constraint.sum) + dependence_ALL_VC_on_PC) * alliance_experience
                   + factor(year), control=glm.control(maxit=100),
                   data = data1)
   ali.4 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + VC_Invest_HHI + dependence_ALL_VC_on_PC # dependence_PC_on_lead_VC + dependence_lead_VC_on_PC
                   + (e_success.sum + status.cum + I((-1)*constraint.cum)) * alliance_eigen +
                   # + alliance_eigen #+ I((-1)*alliance_constraint)) 
                   # + (company_eigen + I((-1)*company_constraint))
                   + factor(year), control=glm.control(maxit=100),
                   data = data1)
   ali.4 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + VC_Invest_HHI + dependence_ALL_VC_on_PC # dependence_PC_on_lead_VC + dependence_lead_VC_on_PC
                   + I((-1)*constraint.sum) * dependence_ALL_VC_on_PC
                   + I(status.sum) * dependence_ALL_VC_on_PC
                   + factor(year), control=glm.control(maxit=100),
                   data = data1)
   
   zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
   m.ali.0 <- zouzhe(ali.0) %>% format.reg.table(d=3)
   m.ali.1 <- zouzhe(ali.1) %>% format.reg.table(d=3)
   m.ali.1. <- zouzhe(ali.1.) %>% format.reg.table(d=3)
   m.ali.2 <- zouzhe(ali.2) %>% format.reg.table(d=3)
   m.ali.3 <- zouzhe(ali.3) %>% format.reg.table(d=3)
   m.ali.4 <- zouzhe(ali.4) %>% format.reg.table(d=3)
   # m.ali.4[77,2] <- "I(status.sum):dependence_ALL_VC_on_PC"   
   cb.al <- Combine.Result(m.ali.0, m.ali.1, m.ali.1., m.ali.2, m.ali.3, m.ali.4, n.tbl = 6)[-(23:70),]
   cp.al <- compare.models(model1 = ali.0, model2 = ali.1, model3 = ali.1., model4 = ali.2, model5 = ali.3, model6 = ali.4, n=6,
                           main.effect.only = c(3),
                           intn.effect.only = c(4,5), N_main_to_intn = 2)
   names(cb.al) <- c("Variables", paste0("Model ", 0:4), "Model Full")
   names(cp.al) <- c("Variables", paste0("Model ", 0:4), "Model Full")
   rt.al <- rbind(cb.al, cp.al)
   
   ##########
   
   
   
   data1$VC_dep_hole <- with(data1, dependence_ALL_VC_on_PC * I((-1)*constraint.sum))
   
   #####
   # model selection #
   data <- data.DV
   dim(data)
   data1 <- data[-2444,]
   impact <- function(i){
     data1 <- data[-which(unique(data$company_name) == unique(data$company_name)[i]),] 
     
     run.old <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + VC_Invest_HHI + dependence_ALL_VC_on_PC # dependence_PC_on_lead_VC + dependence_lead_VC_on_PC
                       + I((-1)*constraint.sum) * dependence_ALL_VC_on_PC
                       + I(status.sum) * dependence_ALL_VC_on_PC
                       + factor(year), control=glm.control(maxit=100),
                       data = data)
     run.new <- update(run.old, .~., data=data1)
     reg.old <- zouzhe(run.old)
     reg.new <- zouzhe(run.new)
     
     improve <- ifelse(reg.new[38,4] < 0.05, reg.old[39,4] - reg.new[39,4], 0)
     barplot( c(100*which(unique(data$company_name) == i)/length(unique(data$company_name)),100), horiz=TRUE )
     return(improve)}
   
   start <- Sys.time()
   to.delete <- c()
   repeat{
     compare.impact <- as.numeric(do.call(rbind, map(unique(data$company_name), impact)))
     ## deletion rule ###
     delete.this.round <- which.max(compare.impact)
     to.delete <- c(to.delete, delete.this.round)
     data <- if(!is.na(delete.this.round)){data[-delete.this.round,]}else{data}
     run <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + VC_Invest_HHI + dependence_ALL_VC_on_PC # dependence_PC_on_lead_VC + dependence_lead_VC_on_PC
                   + I((-1)*constraint.sum) * dependence_ALL_VC_on_PC
                   + I(status.sum) * dependence_ALL_VC_on_PC
                   + factor(year), control=glm.control(maxit=100),
                   data = data)
     reg <- zouzhe(run)
     if(!is.na(delete.this.round)){print(c(this.round=as.character(delete.this.round), 
                                           PC = i,
                                           int1=round(reg[38,4], 5), 
                                           int2=round(reg[39,4], 5), 
                                           nrow=nrow(data)))
       print(reg)}
     print(Sys.time() - start)
     if((reg[38,4] < 0.01 & reg[39,4] < 0.05) | is.na(delete.this.round)){break}
   }
   Sys.time() - start
   
   setwd("/Volumes/RESEARCH_HD/006/006B_Run_On_Cluster")
   imp <- read.csv("result.csv", stringsAsFactors = FALSE) %>% arrange(improve)
   
   ##### variable selection version.2 network related #####
   # status.var     <- c("status.cum", "company_eigen", "alliance_eigen")
   # constt.var <- c("constraint.cum", "company_constraint", "alliance_constraint")
   #  SxC <- expand.grid(status.var, constraint.var)
   #  SxC.df <- t(all.var.names)
   #  SxS.df <- combn(status.var, 2)
   #  CxC.df <- combn(constt.var, 2)
   #  var.df <- cbind(SxC.df,SxS.df,CxC.df)
   # bottom.x <- 3 # set output: how many bottom rows to check?
   # run <- function(i){
   #   x1 <- eval(parse(text = paste("data.DV$", var.df[1,i], sep="")))
   #   x2 <- eval(parse(text = paste("data.DV$", var.df[2,i], sep="")))
   #   ali.0 <- zouzhe(glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + 
   #                            alliance_experience + e_success.sum + dependence_PC_on_lead_VC + dependence_lead_VC_on_PC + factor(year) + x1 * x2, 
   #                          control=glm.control(maxit=100),
   #                          data = data.DV))
   #   sig <- ali.0[-(1:35),c(2,4:5)]
   #   barplot( c(100*(i/ncol(var.df)),100), horiz=TRUE)
   #   return(list(i,var.df[,i],sig))}
   # result <- map(1:ncol(var.df),safely(run))
   # res <- purrr::map(result,function(x){x[["result"]]})
   # err <- purrr::map(result,function(x){x[["error"]]})
   # good <- res[purrr::map_lgl(err, is_null)] 
   # bad <- which(unlist(purrr::map_lgl(res, is_null))==TRUE)
   # significant.main.int.1 <- which(unlist(map(good, function(x){ifelse(x[[3]][3,2] < 0.05, 1, 0)}))==1)
   # good[significant.main.int.1]
   # map(good[significant.main.int.1], function(x) (x[[2]]))
   
   # ##### Network Centrality x Constraint ##### 
   # ali.0 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + dependence_PC_on_lead_VC + dependence_lead_VC_on_PC
   #                 + factor(year), control=glm.control(maxit=100),
   #                 data = data.DV)
   # ali.1  <- update(ali.0, .~. + status.cum)
   # ali.1. <- update(ali.0, .~. + company_eigen)
   # ali.1.. <- update(ali.0, .~. + I((-1)*constraint.cum))
   # ali.2  <- update(ali.0, .~. + status.cum * I((-1)*constraint.cum))
   # ali.2. <- update(ali.0, .~. + company_eigen * I((-1)*constraint.cum))
   # ali.3  <- update(ali.0, .~. + status.cum * company_eigen)
   # ali.4  <- update(ali.0, .~. + status.cum * I((-1)*constraint.cum) + company_eigen * I((-1)*constraint.cum) + status.cum * company_eigen)
   #  zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
   #  m.ali.0 <- zouzhe(ali.0) %>% format.reg.table(d=3)
   #  m.ali.1 <- zouzhe(ali.1) %>% format.reg.table(d=3)
   #  m.ali.1. <- zouzhe(ali.1.) %>% format.reg.table(d=3)
   #  m.ali.1.. <- zouzhe(ali.1..) %>% format.reg.table(d=3)
   #  m.ali.2 <- zouzhe(ali.2) %>% format.reg.table(d=3)
   #  m.ali.2. <- zouzhe(ali.2.) %>% format.reg.table(d=3)
   #  m.ali.3 <- zouzhe(ali.3) %>% format.reg.table(d=3)
   #  m.ali.4 <- zouzhe(ali.4) %>% format.reg.table(d=3)
   #  m.ali.4[79,2] <- "company_eigen:I((-1) * constraint.cum)"
   # cb.al <- Combine.Result(m.ali.0, m.ali.1, m.ali.1., m.ali.1.., m.ali.2, m.ali.2., m.ali.3, m.ali.4, n.tbl = 8)[-(23:70),]
   # cp.al <- compare.models(model1 = ali.0, model2 = ali.1, model3 = ali.1., model4 = ali.1.., 
   #                         model5 = ali.2, model6 = ali.2., model7 = ali.3, model8 = ali.4, n=8,
   #                         main.effect.only = c(3,4),
   #                         intn.effect.only = c(5,6,7), N_main_to_intn = 3)
   # names(cb.al) <- c("Variables", paste0("Model ", 0:6), "Model Full")
   # names(cp.al) <- c("Variables", paste0("Model ", 0:6), "Model Full")
   # rt.al <- rbind(cb.al, cp.al)
   # var.names.aa <- c("Total Fund Raised (MM)", "Total # VCs Participated", "Total # Rounds Received",
   #                   "1 = Ownership-Status Match","1 = Ownership-Capability Match","1 = Status-Capability Match", "Alliance Experience", "VC Investment Capability",
   #                   "PC dependence on VC", "VC dependence on PC", "VC Status", "PC Status", "VC Structure Hole",
   #                   "VC Status * VC Structure Hole", "PC Status * VC Structure Hole", "VC Status * PC Status")
   # rt.al[seq(3,nrow(cb.al),2),1] <- var.names.aa
   # rt.al[,-ncol(rt.al)] # View(rt.ali)
   # knitr::kable(rt.al[,-ncol(rt.al)])