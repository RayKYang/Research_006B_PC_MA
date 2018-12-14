setwd("/Volumes/RESEARCH_HD/006/network_data")
# data.DV <- read.csv("PC_AA_DV_9.25.2018.csv", stringsAsFactors = FALSE) # the lastest version prior to 12.10 （3-yr rolling on VC centrality）
data.DV <- read.csv("PC_AA_DV_12.15.2018.csv", stringsAsFactors = FALSE) # (5-yr rolling on VC centrality)

regrrr::load.pkgs(c("dplyr", "pscl", "Formula", "lmtest", "regrrr", "multiwayvcov", "sandwich"))

# test over-dispersion
library(AER)
rd <- glm(n.ali.overall ~ cum_n.firm + round + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend + 
            amt.sum + e_success.sum + age 
          + status.cum * (acquisition_experience + alliance_experience) + alliance_eigen * (acquisition_experience + alliance_experience)
          , data = data.DV, family = poisson)
dispersiontest(rd, trafo=1)
rd_nb <- MASS::glm.nb(n.ali.overall ~ cum_n.firm + round + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend + 
                        amt.sum + e_success.sum + age 
                      + status.cum * (acquisition_experience + alliance_experience) + alliance_eigen * (acquisition_experience + alliance_experience)
                      , data = data.DV)
rd_nb_Eq <- MASS::glm.nb(n.acq.ASSETS ~ cum_n.firm + round + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend + 
                        amt.sum + e_success.sum + age 
                      + status.cum * (acquisition_experience + alliance_experience) + alliance_eigen * (acquisition_experience + alliance_experience)
                      , data = data.DV)
odTest(rd_nb)

# # simple NB model 
# rd_nb_0 <- MASS::glm.nb(n.ali.overall ~ cum_n.firm + round + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend + 
#                         amt.sum + e_success.sum + age, data = data.DV)
# rd_nb_1 <- update(rd_nb_0, . ~ . + acquisition_experience + alliance_experience)
# rd_nb_2 <- update(rd_nb_1, . ~ . + status.cum + alliance_eigen)
# rd_nb_Eq_0 <- MASS::glm.nb(n.acq.ASSETS ~ cum_n.firm + round + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend + 
#                            amt.sum + e_success.sum + age, data = data.DV)
# rd_nb_Eq_1 <- update(rd_nb_Eq_0, . ~ . + acquisition_experience + alliance_experience)
# rd_nb_Eq_2 <- update(rd_nb_Eq_1, . ~ . + status.cum + alliance_eigen)
# 
# convert.nb.rob.se <- function(model){
#   coeftest(model,   sandwich::vcovCL(model,   cbind(model$model$cusipAup, factor(model$model$year)))) %>% 
#     `[`() %>% as.data.frame() %>% add.p.z %>% add.sig}
# 
# al_nb.1_c <- convert.nb.rob.se(rd_nb_0) %>% format.reg.table(d=3)
# al_nb.2_c <- convert.nb.rob.se(rd_nb_1) %>% format.reg.table(d=3)
# al_nb.2._c <- convert.nb.rob.se(rd_nb_2) %>% format.reg.table(d=3)
# al_nb.3_c <- convert.nb.rob.se(rd_nb) %>% format.reg.table(d=3) 
# 
# aq_nb.1_c <- convert.nb.rob.se(rd_nb_Eq_0) %>% format.reg.table(d=3)
# aq_nb.2_c <- convert.nb.rob.se(rd_nb_Eq_1) %>% format.reg.table(d=3)
# aq_nb.2._c <- convert.nb.rob.se(rd_nb_Eq_2) %>% format.reg.table(d=3)
# aq_nb.3_c <- convert.nb.rob.se(rd_nb_Eq) %>% format.reg.table(d=3) 
# 
# ali_nb.simple <- rbind(regrrr::reg.combine(al_nb.1_c, al_nb.2_c, al_nb.2._c, al_nb.3_c), 
#                    regrrr::mod.compare(rd_nb_0, rd_nb_1, rd_nb_2, rd_nb, likelihood.only = TRUE))
# ali_nb.simple[seq(3,nrow(ali_nb.simple)-1,2),1] <- var.names.aa
# print("DV = Future Number of Alliances")
# knitr::kable(ali_nb.simple)
# 
# acq_nb.simple <- rbind(regrrr::reg.combine(aq_nb.1_c, aq_nb.2_c, aq_nb.2._c, aq_nb.3_c), 
#                        regrrr::mod.compare(rd_nb_Eq_0, rd_nb_Eq_1, rd_nb_Eq_2, rd_nb_Eq, likelihood.only = TRUE))
# acq_nb.simple[seq(3,nrow(acq_nb.simple)-1,2),1] <- var.names.aa
# print("DV = Future Number of Acquisitions")
# knitr::kable(acq_nb.simple)

# ZINB
update.zeroinfl <- function(object, new, ...) {
  call <- object$call
  call$formula <- update(as.Formula(formula(object)), new)
  eval.parent(call)
}

# n.acq.ASSETS n.acq.EQUITY n.acq.overall n.ali.Alliance n.ali.JV
aqi.0 <- zeroinfl(n.ali.overall ~ cum_n.firm + round + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend + 
                  amt.sum + e_success.sum + age 
                  | amt.sum + e_success.sum + age # + factor(industry) + factor(year)
                  , dist = "negbin", # + OS.match + OI.match + SI.match
                  data = data.DV) # data.DV[-is,]
aqi.1 <- update(aqi.0, .~. + acquisition_experience + alliance_experience | . + acquisition_experience + alliance_experience)
aqi.2 <- update(aqi.1, .~. + status.cum + alliance_eigen | . )
aqi.all <- update(aqi.1, .~. + status.cum * (acquisition_experience + alliance_experience) + alliance_eigen * (acquisition_experience + alliance_experience) | 
                    . )

##### Acquisition Model #####
aqi.0_Eq <- zeroinfl(n.acq.ASSETS ~ cum_n.firm + round + dependence_ALL_VC_on_PC + VC_Invest_HHI + year.trend + 
                       amt.sum + e_success.sum + age 
                     | amt.sum + e_success.sum + age # + factor(industry) + factor(year)
                     , dist = "negbin", # + OS.match + OI.match + SI.match
                     data = data.DV) # data.DV[-is,]
aqi.1_Eq <- update(aqi.0_Eq, .~. + acquisition_experience + alliance_experience | . + acquisition_experience + alliance_experience)
aqi.2_Eq <- update(aqi.1_Eq, .~. + status.cum + alliance_eigen | . )
aqi.all_Eq <- update(aqi.1_Eq, .~. + status.cum * (acquisition_experience + alliance_experience) + alliance_eigen * (acquisition_experience + alliance_experience) | 
                       . )

# vuong test
vuong(rd_nb, aqi.all)
vuong(rd_nb_Eq, aqi.all_Eq)

# run 006_2.0 Model Selection.R first 
# correlation table
var.names.aa <- c("Total # VCs Participated", "Total # Rounds Received", "VC Dependence on PC", "VC Investment Concentration", "Year Trend",
                  "Total Fund Raised (MM)", "VC Investment Capability", "PC Age",  
                  "Acquisition Experience", "Alliance Experience",
                  "VC Status in VC Network", "PC Status in Alliance Network",       
                  "VC Status x Acquisition Experience", "VC Status x Alliance Experience",
                  "PC Status x Acquisition Experience", "PC Status x Alliance Experience")
X_names <- var.names.aa[!stringr::str_detect(var.names.aa, pattern = " x ")]
c.table <- regrrr::reg.Cor.Table(model_df = aqi.all$model, model_df_to_combine = aqi.all_Eq$model, 
                      all.var.names = c("Number of Alliances", "Number of Acquisitions", X_names), d = 2)
knitr::kable(c.table)

regrrr::reg.Vif(aqi.all_Eq$model)

# regression table 
convert.w.rob.se <- function(model, which.part = "count"){
  coeftest(model,   sandwich::vcovCL(model,   cbind(model$model$cusipAup, factor(model$model$year)))) %>% 
    `[`() %>% as.data.frame() %>% add.p.z %>% add.sig %>% 
    subset(stringr::str_detect(names(coef(model)), which.part))
}

al.1_c <- convert.w.rob.se(aqi.0,   "count") %>% format_reg.table(d=3)
al.2_c <- convert.w.rob.se(aqi.1,   "count") %>% format_reg.table(d=3)
al.2._c <- convert.w.rob.se(aqi.2,   "count") %>% format_reg.table(d=3)
al.3_c <- convert.w.rob.se(aqi.all, "count") %>% format_reg.table(d=3) 
al.1_z <- convert.w.rob.se(aqi.0,   "zero")  %>% format_reg.table(d=3)
al.2_z <- convert.w.rob.se(aqi.1,   "zero")  %>% format_reg.table(d=3)
al.3_z <- convert.w.rob.se(aqi.all, "zero")  %>% format_reg.table(d=3)

aq.1_c <- convert.w.rob.se(aqi.0_Eq,   "count") %>% format_reg.table(d=3)
aq.2_c <- convert.w.rob.se(aqi.1_Eq,   "count") %>% format_reg.table(d=3)
aq.2._c <- convert.w.rob.se(aqi.2_Eq,   "count") %>% format_reg.table(d=3)
aq.3_c <- convert.w.rob.se(aqi.all_Eq, "count") %>% format_reg.table(d=3)
aq.1_z <- convert.w.rob.se(aqi.0_Eq,   "zero")  %>% format_reg.table(d=3)
aq.2_z <- convert.w.rob.se(aqi.1_Eq,   "zero")  %>% format_reg.table(d=3)
aq.3_z <- convert.w.rob.se(aqi.all_Eq, "zero")  %>% format_reg.table(d=3)

# change.var.name
change.var.name <- function(a){
  a[,"var_"] <- stringr::str_replace_all(as.character(a[,"var_"]), "count_|zero_", "") %>% unlist %>% as.character()
  return(a)
}

al.1_c <- change.var.name(al.1_c)
al.2_c <- change.var.name(al.2_c)
al.2._c <- change.var.name(al.2._c)
al.3_c <- change.var.name(al.3_c)
al.1_z <- change.var.name(al.1_z)
al.2_z <- change.var.name(al.2_z)
al.3_z <- change.var.name(al.3_z)

aq.1_c <- change.var.name(aq.1_c)
aq.2_c <- change.var.name(aq.2_c)
aq.2._c <- change.var.name(aq.2._c)
aq.3_c <- change.var.name(aq.3_c)
aq.1_z <- change.var.name(aq.1_z)
aq.2_z <- change.var.name(aq.2_z)
aq.3_z <- change.var.name(aq.3_z)

ali.count <- rbind(regrrr::reg.combine(al.1_c, al.2_c, al.2._c, al.3_c), 
                   regrrr::mod.compare(aqi.0, aqi.1, aqi.2, aqi.all, likelihood.only = TRUE))
ali.count[seq(3,nrow(ali.count)-1,2),1] <- var.names.aa
print("DV = Future Number of Alliances")
knitr::kable(ali.count)

ali.table <- rbind(regrrr::reg.combine(al.1_c, al.1_z, al.2_c, al.2_z, al.3_c, al.3_z), 
                   regrrr::mod.compare(aqi.0, aqi.1, aqi.all, likelihood.only = TRUE))
ali.table[seq(3,nrow(ali.table)-1,2),1] <- var.names.aa
knitr::kable(ali.table)

aqi.count <- rbind(regrrr::reg.combine(aq.1_c, aq.2_c, aq.2._c, aq.3_c), 
                   regrrr::mod.compare(aqi.0_Eq, aqi.1_Eq, aqi.2_Eq, aqi.all_Eq, likelihood.only = TRUE))
aqi.count[seq(3,nrow(aqi.count)-1,2),1] <- var.names.aa
print("DV = Future Number of Acquisitions")
knitr::kable(aqi.count)

aqi.table <- rbind(regrrr::reg.combine(aq.1_c, aq.1_z, aq.2_c, aq.2_z, aq.3_c, aq.3_z),
                   regrrr::mod.compare(aqi.0_Eq, aqi.1_Eq, aqi.all_Eq, likelihood.only = TRUE))
aqi.table[seq(3,nrow(aqi.table)-1,2),1] <- var.names.aa
knitr::kable(aqi.table)

# regrrr::reg.combine(al.1_c, al.2_c, al.3_c)
# regrrr::reg.combine(aq.1_c, aq.2_c, aq.3_c)
# plot
change.var.name_ <- function(a){
  rownames(a) <- stringr::str_replace_all(as.character(rownames(a)), "count_|zero_", "") %>% unlist %>% as.character()
  return(a)
}
plot_m1 <- convert.w.rob.se(aqi.all,     "count") %>% change.var.name_
plot_m2 <- convert.w.rob.se(aqi.all_Eq,  "count") %>% change.var.name_

p1 <- regrrr::reg.gg.from.model(reg.result = plot_m1, df = data.DV, model.for.predict = aqi.all, by_color = FALSE,
                                x_var.name = "alliance_experience", y_var.name = "n.ali.Alliance",
                                main1.r = 11, mdrt.r = 12, mod.n.sd = 1,
                                xlab = "Alliance Experience", ylab = "# Alliances", moderator.lab = "VC Status")

p2 <- regrrr::reg.gg.from.model(reg.result = plot_m1, df = data.DV, model.for.predict = aqi.all, by_color = FALSE,
                                x_var.name = "alliance_experience", y_var.name = "n.ali.Alliance", 
                                main1.r = 11, mdrt.r = 13, mod.n.sd = 1,
                                xlab = "Alliance Experience", ylab = "# Alliances", moderator.lab = "Alliance Status")

p3 <- regrrr::reg.gg.from.model(reg.result = plot_m1, df = data.DV, model.for.predict = aqi.all, by_color = FALSE,
                                x_var.name = "acquisition_experience", y_var.name = "n.ali.Alliance", 
                                main1.r = 10, mdrt.r = 12, mod.n.sd = 1,
                                xlab = "Acquisition Experience", ylab = "# Alliances", moderator.lab = "VC Status")

p4 <- regrrr::reg.gg.from.model(reg.result = plot_m1, df = data.DV, model.for.predict = aqi.all, by_color = FALSE,
                                x_var.name = "acquisition_experience", y_var.name = "n.ali.Alliance", 
                                main1.r = 10, mdrt.r = 13, mod.n.sd = 2,
                                xlab = "Acquisition Experience", ylab = "# Alliances", moderator.lab = "Alliance Status")

p5 <- regrrr::reg.gg.from.model(reg.result = plot_m2, df = data.DV, model.for.predict = aqi.all_Eq, by_color = FALSE,
                                x_var.name = "acquisition_experience", y_var.name = "n.acq.ASSETS", 
                                main1.r = 10, mdrt.r = 12, mod.n.sd = 1,
                                xlab = "Acquisition Experience", ylab = "# Acquisitions", moderator.lab = "VC Status")

p6 <- regrrr::reg.gg.from.model(reg.result = plot_m2, df = data.DV, model.for.predict = aqi.all_Eq, by_color = FALSE,
                                x_var.name = "acquisition_experience", y_var.name = "n.acq.ASSETS", 
                                main1.r = 10, mdrt.r = 13, mod.n.sd = 1,
                                xlab = "Acquisition Experience", ylab = "# Acquisitions", moderator.lab = "Alliance Status")

p7 <- regrrr::reg.gg.from.model(reg.result = plot_m2, df = data.DV, model.for.predict = aqi.all_Eq, by_color = FALSE,
                                x_var.name = "alliance_experience", y_var.name = "n.acq.ASSETS", 
                                main1.r = 11, mdrt.r = 12, mod.n.sd = 1,
                                xlab = "Alliance Experience", ylab = "# Acquisitions", moderator.lab = "VC Status")

p8 <- regrrr::reg.gg.from.model(reg.result = plot_m2, df = data.DV, model.for.predict = aqi.all_Eq, by_color = FALSE,
                                x_var.name = "alliance_experience", y_var.name = "n.acq.ASSETS", 
                                main1.r = 11, mdrt.r = 13, mod.n.sd = 1,
                                xlab = "Alliance Experience", ylab = "# Acquisitions", moderator.lab = "Alliance Status")

margin = ggplot2::theme(plot.margin = ggplot2::unit(c(0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35), "cm")) 
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2, grobs = lapply(list(p1, p2, p3, p4, p5, p6, p7, p8), "+", margin))

