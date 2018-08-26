impact <- function(i){ # i can be a vector
  data1 <- data[-i,] 
  
  int1 <- 37
  int2 <- 38
  int3 <- 39
  
  run.old.1 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year) + constraint.cum + company_constraint + alliance_constraint +
                             company_constraint * constraint.cum, control=glm.control(maxit=100),
                             data = data)
  run.new.1 <- update(run.old.1, .~., data=data1)
  zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
  reg.old.1 <- zouzhe(run.old.1)
  reg.new.1 <- zouzhe(run.new.1)

  run.old.2 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year) + constraint.cum + company_constraint + alliance_constraint +
                        alliance_constraint * constraint.cum, control=glm.control(maxit=100),
                      data = data)
  run.new.2 <- update(run.old.2, .~., data=data1)
  reg.old.2 <- zouzhe(run.old.2)
  reg.new.2 <- zouzhe(run.new.2)
  
  run.old.3 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year) + constraint.cum + company_constraint + alliance_constraint +
                        company_constraint * constraint.cum + alliance_constraint * constraint.cum + company_constraint * alliance_constraint, control=glm.control(maxit=100),
                      data = data)
  run.new.3 <- update(run.old.3, .~., data=data1)
  reg.old.3 <- zouzhe(run.old.3)
  reg.new.3 <- zouzhe(run.new.3)
  
  #####
  
  improve.1 <- reg.old.1[int1, 4] - reg.new.1[int1, 4]
  improve.2 <- reg.old.2[int1, 4] - reg.new.2[int1, 4]
  improve.3.a <- reg.old.3[int1, 4] - reg.new.3[int1, 4]
  improve.3.b <- reg.old.3[int2, 4] - reg.new.3[int2, 4]
 
  if(i[1] %% 100 == 1){barplot( c(100*(i[1]/nrow(data)), 100), horiz=TRUE )}
  result <- c(improve.1, improve.2, improve.3.a, improve.3.b)
  names(result) <- c("int1", "int2", "int3.a", "int3.b")
  return(result)}

get.weight <- function(DAT){
  
  int1 <- 37
  int2 <- 38
  int3 <- 39
  
  run.old.1 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year) + constraint.cum + company_constraint + alliance_constraint +
                        company_constraint * constraint.cum, control=glm.control(maxit=100),
                      data = DAT)
  zouzhe <- function(model){coef(summary(model)) %>% add.p.z(z.col=3) %>% `[`(c(1,2,3,5))  %>% add.sig(Pr.col = 4)}
  reg.old.1 <- zouzhe(run.old.1)

  run.old.2 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year) + constraint.cum + company_constraint + alliance_constraint +
                        alliance_constraint * constraint.cum, control=glm.control(maxit=100),
                      data = DAT)
  reg.old.2 <- zouzhe(run.old.2)

  run.old.3 <- glm.nb(n.ali.overall ~ amt.sum + cum_n.firm + round + OS.match + OI.match + SI.match + alliance_experience + e_success.sum + factor(year) + constraint.cum + company_constraint + alliance_constraint +
                        company_constraint * constraint.cum + alliance_constraint * constraint.cum + company_constraint * alliance_constraint, control=glm.control(maxit=100),
                      data = DAT)
  reg.old.3 <- zouzhe(run.old.3)

  ###
  i1   <- ifelse(reg.old.1[int1, 4] > 0.01, reg.old.1[int1, 4], reg.old.1[int1, 4]/5)
  i2   <- ifelse(reg.old.2[int1, 4] > 0.01, reg.old.2[int1, 4], reg.old.2[int1, 4]/5)
  i3.a <- ifelse(reg.old.3[int1, 4] > 0.01, reg.old.3[int1, 4], reg.old.3[int1, 4]/5)
  i3.b <- ifelse(reg.old.3[int2, 4] > 0.01, reg.old.3[int2, 4], reg.old.3[int2, 4]/5)
  
  # 
  result        <- c(i1,         i2,     i3.a,       i3.b)
  names(result) <- c("int1", "int2", "int1.fl", "int2.fl")
  result <- round(result, 5)
  return(result)
}