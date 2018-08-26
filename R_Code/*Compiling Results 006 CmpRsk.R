suppressMessages(lapply(c("dplyr","tidyr","lme4","purrr","gee","MuMIn","car"), require, character.only = TRUE))
add.p.z <- function(df,z.col=5){data.frame(n.r=1:nrow(df),df,
                                   p.z=2 * (1 - pnorm(abs(df[,z.col]))))}
add.sig <- function(df , Pr.col=5){data.frame(df,sig=ifelse(df[,Pr.col]<0.001,"***",
                                                            ifelse(df[,Pr.col]<0.01,"**",
                                                                   ifelse(df[,Pr.col]<0.05,"*",
                                                                          ifelse(df[,Pr.col]<0.1,"†","")))))}
format.reg.table <- function(df, d=3){
  df <- data.frame(n.r=1:nrow(df),df)
  test <- cbind(var=rownames(df),df[,c(1,3:4,6)])
  digits <- function(x,d){ if(class(x)=="numeric") {formatC(x, format = "f", digits = d)} else{x} }
  test <- data.frame(map(test,digits,d))
  test <- unite(test, coef,3,5,sep="")
  test[,4]  <- paste0("(",test[,4],")",sep="")
  reg.table <- arrange(gather(test,key,beta,-c(var,n.r)),n.r) %>% dplyr::select(2,1,4)
  even.row <- rep(c(FALSE,TRUE),nrow(reg.table)/2)
  reg.table$var <- as.character(reg.table$var)
  reg.table$var[even.row] <- ""
  return(reg.table)
}
suppressWarnings(warning("format.reg.table"))

compare.models<- function(model1,model2,model3=NULL,model4=NULL,model5=NULL,model6=NULL,model7=NULL,model8=NULL,n=5,
                          main.effect.only = NULL,
                          intn.effect.only = NULL, N_main_to_intn = 2){
  # main.effect.only <- c(3,4) # e.g. tbl_3 and tbl_4 add individual main effects to tbl_1
  # intn.effect.only <- c(5,6) # e.g. tbl_3 and tbl_4 add individual interaction  to tbl_1
  compare <- if(n==2){
    suppressWarnings(suppressMessages(anova(model1,model2)))}else if(n==3){
    suppressWarnings(suppressMessages(anova(model1,model2,model3)))}else if(n==4){
    suppressWarnings(suppressMessages(anova(model1,model2,model3,model4)))}else if(n==5){
    suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5)))}else if(n==6){
    suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6)))}else if(n==7){
    suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7)))}else if(n==8){
    suppressWarnings(suppressMessages(anova(model1,model2,model3,model4,model5,model6,model7,model8)))}
  
  model.list  <- list(model1, model2, model3, model4, model5, model6, model7, model8)
  model.list  <- model.list[1:sum(!unlist((map(model.list, is.null))))]
  
  if(!is.null(main.effect.only)){
    for(i in main.effect.only){
      compare[i,] <- anova(model.list[[main.effect.only[1]-2]], model.list[[i]])[2,]
    }
  }
  if(!is.null(intn.effect.only)){
    for(i in intn.effect.only){
      compare[i,] <- if(!is.null(main.effect.only)){
        anova(model.list[[i-N_main_to_intn]], model.list[[i]])[2,]}else{
        anova(model.list[[2]], model.list[[i]])[2,]
      }
    }
  }
  ch          <- function(x){as.character(x)}
  convert.sig <- function(df){ifelse(df<0.001,"***",ifelse(df<0.01,"**",ifelse(df<0.05,"*",ifelse(df<0.1,"†",""))))}
  dg          <- function(x)formatC(x, format = "f", digits = 2)

  
  compare.df <- if(class(model1)[1]=="negbin"){
               data.frame(AIC=ch(t(dg(unlist(purrr::map(model.list, MuMIn::AICc))))),
               Log_Likelihood=ch(t(dg(compare[3][[1]]))), Chisq=ch(t(dg(compare[7][[1]]))),
               sig=ch(t(convert.sig(compare[8][[1]])))) %>% unite(Chisq,Chisq,sig,sep="") %>% t()
    
  }else{
               data.frame(AIC=ch(t(dg(unlist(purrr::map(model.list, MuMIn::AICc))))),
               Log_Likelihood=ch(t(dg(compare[1][[1]]))),Chisq=ch(t(dg(compare[2][[1]]))),
               sig=ch(t(convert.sig(compare[4][[1]])))) %>% unite(Chisq,Chisq,sig,sep="") %>% t() }
  compare.df[nrow(compare.df),1] <- ""
  compare.df <- data.frame(Variables=row.names(compare.df),compare.df)
  return(compare.df)
}

combine.result <- function(tbl_1,tbl_2, tbl_3=NULL,tbl_4=NULL,tbl_5=NULL,tbl_6=NULL,tbl_7=NULL,tbl_8=NULL,tbl_9=NULL, 
                           n.tbl = 2,
                           main.effect.only = NULL,
                           intn.effect.only = NULL) {
  # main.effect.only <- c(3,4) # e.g. tbl_3 and tbl_4 add individual main effects to tbl_1
  # intn.effect.only <- c(5,6) # e.g. tbl_3 and tbl_4 add individual interaction  to tbl_1
   list_tbls <- list(tbl_1,tbl_2,tbl_3,tbl_4,tbl_5,tbl_6,tbl_7,tbl_8,tbl_9)[1:n.tbl]
  if(!is.null(main.effect.only)){
    for(i in main.effect.only){
    pad <- function(tle, i){
      pad.tail <- tle[(nrow(tle)-1):nrow(tle),]
      pad.head <- tle[1:(nrow(tle)-2),]
      pad <- data.frame(matrix("", 2*which(main.effect.only==i), 3))
      names(pad) <- names(pad.head)
      result <- do.call(rbind, list(pad.head, pad, pad.tail))
      result$n.r <- rep(1:(nrow(result)/2), each=2)
      return(result)
    }
    list_tbls[[i]]     <- pad(list_tbls[[i]],i)
    list_tbls[[i]]$var[1:(nrow(list_tbls[[i]])-2)] <- list_tbls[[i-1]]$var
    }
  }
  if(!is.null(intn.effect.only)){
     for(i in intn.effect.only){
     pad.int <- function(tle, i){
       pad.tail   <- tle[(nrow(tle)-1):(nrow(tle)),]
       pad.middle <- tle[(nrow(tle)-3):(nrow(tle)-2),]
        last.main <- main.effect.only[length(main.effect.only)]
        head.nr <- which(list_tbls[[last.main]]$var==tle[(nrow(tle)-5),]$var)
        pad.head <- tle[1:(head.nr+1),]
       pad.1 <- data.frame(matrix("", 2*(i-last.main-1), 3))
       pad.2 <- data.frame(matrix("", 2, 3))
       names(pad.1) <- names(pad.head)
       names(pad.2) <- names(pad.head)
       result <- do.call(rbind, list(pad.head, pad.1, pad.middle, pad.2, pad.tail))
       result$n.r <- rep(1:(nrow(result)/2), each=2)
       return(result)
     }
     list_tbls[[i]]     <- pad.int(list_tbls[[i]],i)
     list_tbls[[i]]$var[1:(nrow(list_tbls[[i]])-2)] <- list_tbls[[i-1]]$var
     }
  }  
  main.table <- list_tbls %>% 
    purrr::reduce(right_join,by=c("n.r","var")) %>%
    dplyr::select(-1)
  main.table[is.na(main.table)] <- ""
  return(main.table)}

### April 4th, 2018 ### this works better ###
Combine.Result <- function(tbl_1, tbl_2, tbl_3=NULL, tbl_4=NULL, tbl_5=NULL, tbl_6=NULL, tbl_7=NULL, tbl_8=NULL, tbl_9=NULL, tbl_10=NULL, tbl_11=NULL, tbl_12=NULL, tbl_13=NULL, tbl_14=NULL, tbl_15=NULL,
                           n.tbl = 2) {
  list_tbls <- list(tbl_1, tbl_2, tbl_3, tbl_4, tbl_5, tbl_6, tbl_7, tbl_8, tbl_9, tbl_10, tbl_11, tbl_12, tbl_13, tbl_14, tbl_15)[1:n.tbl]
    for(i in 1:length(list_tbls)){
      list_tbls[[i]]$var[seq(2, nrow(list_tbls[[i]]), by = 2)] <- paste0(list_tbls[[i]]$var[seq(1, nrow(list_tbls[[i]]), by = 2)],"s.e.")
      list_tbls[[i]]$n.r <- NULL
    }
  main.table <- list_tbls %>% 
    purrr::reduce(dplyr::full_join,by=c("var"))
  main.table[is.na(main.table)] <- ""
  main.table$var[seq(2, nrow(main.table), by = 2)] <- ""
  return(main.table)}

###

cor.matrix <- function(result.w.full.var, nameofdata, number.of.IVs, y1.name.in.doc, y2.name.in.doc=NULL, y3.name.in.doc=NULL, y1.name.in.reg, y2.name.in.reg=NULL, y3.name.in.reg=NULL, x.names, digits=2, exclude.factor=TRUE){
  selected.cols <- c(names(result.w.full.var$model),y1.name.in.reg,y2.name.in.reg,y3.name.in.reg)
  selected.cols <- if(exclude.factor==FALSE){selected.cols}else{selected.cols[!stringr::str_detect(selected.cols, pattern = "factor")]}
  data =  if(class(result.w.full.var)[1]=="negbin"){nameofdata[selected.cols]}else{nameofdata[c(names(unlist(result.w.full.var$assign)),y1.name.in.reg,y2.name.in.reg,y3.name.in.reg)]}
  colnames <- if(class(result.w.full.var)[1]=="negbin"){rownames(coef(summary(result.w.full.var)))[-1]}else{rownames(coef(summary(result.w.full.var)))}
  colnames <- if(exclude.factor==FALSE){colnames}else{colnames[!stringr::str_detect(colnames, pattern = "factor")]}
  data.m   <- as.matrix(data[c(colnames,y1.name.in.reg,y2.name.in.reg,y3.name.in.reg)])
  c.marix  <- apply(cor(data.m, use="complete.obs"), FUN=formatC, MARGIN=2, digits=digits, format='f')
  c.marix[upper.tri(c.marix,diag = FALSE)] <- NA
  means <- formatC(colMeans(data.m,na.rm=TRUE),digits=digits,format='f')
  sds   <- formatC(apply(data.m,FUN=sd,MARGIN=2,na.rm=TRUE),digits=digits,format='f')
  df <- data.frame(Mean=means,S.D.=sds,c.marix,stringsAsFactors = FALSE)
  colnames(df)[3:ncol(df)] <- 1:nrow(df)
  rownames(df) <- paste(seq(1:nrow(df)),c(x.names[1:number.of.IVs],y1.name.in.doc,y2.name.in.doc,y3.name.in.doc),sep=". ")
  df[is.na(df)] <- ""
  return(df)
}

plot.hazard <- function(df, nameofdata, 
                        main1.r, main2.r=NULL, mdrt.r=NULL, int1.r=NULL, int2.r=NULL, mdrt_is_dummy=FALSE,
                        no.dr.mdrt1.r=NULL, no.dr.int1.r=NULL, no.dr.mdrt2.r=NULL, no.dr.int2.r=NULL,
                        find.median=TRUE, min_x=0.001, max_x=0.999, max.y=1,
                        mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95, n.sd=1,
                        main=NULL, xlab="name", ylab="name",
                        mdrt.05.name=NULL, mdrt.50.name=NULL, mdrt.95.name="name",
                        flip.low.high=FALSE){
  df <- as.data.frame(df)
  para.r <- c(main1.r,main2.r,mdrt.r,int1.r,int2.r)
  othr.r <- (1:nrow(df))[!((1:nrow(df)) %in% c(para.r))]
  othr.exlude_interaction.r <- (1:nrow(df))[!((1:nrow(df)) %in% c(no.dr.int1.r, no.dr.int2.r, para.r))]
  beta.vec <- df[,2]
  b.b <- beta.vec[main1.r]
  b.a <- ifelse(!is.null(main2.r),beta.vec[main2.r],0)
  b.mdrt <- beta.vec[mdrt.r]
  b.int1 <- ifelse(!is.null(int1.r),beta.vec[int1.r],0)
  b.int2 <- ifelse(!is.null(int2.r),beta.vec[int2.r],0)
  b.othr.vec <- beta.vec[othr.r]
  
  otherterms <- as.matrix(as.data.frame(nameofdata)[,rownames(as.data.frame(df))[othr.exlude_interaction.r]])
  otherTermMedians <- if(find.median==TRUE){robustbase::colMedians(otherterms,na.rm = TRUE)}else{colMeans(otherterms,na.rm = TRUE)}
  constant <- as.numeric(t(otherTermMedians) %*% b.othr.vec)
  
  if(mdrt_is_dummy==TRUE){
    mdrt.05 <- 0
    mdrt.50 <- NULL
    mdrt.95 <- 1
  } else {
  mdrt.05 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_05, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)-
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.50 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_50, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.95 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_95, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)+
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  }
  min.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=min_x, rm.na=TRUE)
  max.x <- quantile(as.data.frame(nameofdata)[rownames(as.data.frame(df))[main1.r]][,1], probs=max_x, rm.na=TRUE)
  x <- seq(min.x, max.x,length=100)
  mdrt.05.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.05 + b.int1*x*mdrt.05 + b.int2*x^2*mdrt.05}else{rep(0,length(x))}
  mdrt.50.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.50 + b.int1*x*mdrt.50 + b.int2*x^2*mdrt.50}else{rep(0,length(x))}
  mdrt.95.vals <- if(!is.null(mdrt.r)){b.mdrt*mdrt.95 + b.int1*x*mdrt.95 + b.int2*x^2*mdrt.95}else{rep(0,length(x))}
  line1.linear <- constant + b.b*x + b.a*x^2 + mdrt.05.vals
  line2.linear <- constant + b.b*x + b.a*x^2 + mdrt.50.vals
  line3.linear <- constant + b.b*x + b.a*x^2 + mdrt.95.vals
  line1 <- line1.linear
  line2 <- line2.linear
  line3 <- line3.linear
  plot1 <- plot(c(min.x, max.x), c(min(line1,line2,line3), max.y*max(line1,line2,line3)), type="n",xlab=xlab, ylab=ylab,main=main)
  low  <- ifelse(flip.low.high==FALSE,1,6)
  high <- ifelse(flip.low.high==FALSE,6,1)
  lines(x, line1, lty=low)
  if(!is.null(mdrt.50.name)){lines(x, line2, lty=2)}
  lines(x, line3, lty=high)
  if(!is.null(mdrt.05.name)){if(is.null(mdrt.50.name)){legend("topleft", legend = c(mdrt.05.name, mdrt.95.name),
                                                              lty = c(low,high), xjust = 1, yjust = 1,adj = c(0, 0.5))}else{legend("topright", legend = c(mdrt.05.name, mdrt.50.name, mdrt.95.name),
                                                                                                                                   lty = c(low,2,high), xjust = 1, yjust = 1,adj = c(0, 0.5))}}
}

plot.lspline <- function(df, nameofdata, y.name.in.model=NULL, plot.main.effect = FALSE, constant=NULL,
                        main1.r, main2.r=NULL, knot=0.5, mdrt.r=NULL, int1.r=NULL, int2.r=NULL,
                        find.median=TRUE, min_x=0.001, max_x=1, max.y=1,
                        mdrt_05=.05, mdrt_50=0.5, mdrt_95=.95,n.sd=1,
                        main=NULL,xlab="name", ylab="name",
                        mdrt.05.name=NULL, mdrt.50.name=NULL, mdrt.95.name="name",moderator.name,
                        flip.low.high=FALSE,y.hi.lim=NULL,y.low.lim=NULL){
  df <- as.data.frame(df)
  para.r <- c(main1.r,main2.r,mdrt.r,int1.r,int2.r)
  othr.r <- (1:nrow(df))[!((1:nrow(df)) %in% c(para.r))]
  beta.vec <- df[,2]
  b.1 <- beta.vec[main1.r]
  b.2 <- ifelse(!is.null(main2.r),beta.vec[main2.r],0)
  b.mdrt <- beta.vec[mdrt.r]
  b.int1 <- ifelse(!is.null(int1.r),beta.vec[int1.r],0)
  b.int2 <- ifelse(!is.null(int2.r),beta.vec[int2.r],0)
  b.othr.vec <- beta.vec[othr.r]
  
  otherterms <- as.matrix(as.data.frame(nameofdata)[,rownames(as.data.frame(df))[othr.r]])
  otherTermMedians <- if(find.median==TRUE){robustbase::colMedians(otherterms,na.rm = TRUE)}else{colMeans(otherterms,na.rm = TRUE)}
  constant <- ifelse(is.null(constant), as.numeric(t(otherTermMedians) %*% b.othr.vec), constant)
  
  mdrt.05 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_05, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)-
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.50 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_50, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
  mdrt.95 <- ifelse(!is.null(mdrt.r),ifelse(find.median==TRUE,quantile(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], probs=mdrt_95, na.rm=TRUE),
                                            mean(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)+
                                              n.sd*sd(as.data.frame(nameofdata)[rownames(df)[mdrt.r]][,1], na.rm=TRUE)),0)
   x.name <- row.names(df)[main1.r] %>% strsplit(split = c(",")) %>% unlist %>% `[`(1)
   x.name <- substr(x.name, 9, nchar(x.name))
  min.x <- quantile(as.data.frame(nameofdata)[,x.name], probs=min_x, rm.na=TRUE)
  max.x <- quantile(as.data.frame(nameofdata)[,x.name], probs=max_x, rm.na=TRUE)
  
  library(ggplot2)    
  library(extrafont)
  library(ggthemes)
  
  if(plot.main.effect == TRUE){
    fit    <- function(x){ifelse(x <= knot, constant       + b.1*x, 
                                 (constant-(b.2-b.1)*knot) + b.2*x)}
    fit.extend    <- function(x){constant + b.1*x}
    ggplot(nameofdata, aes_string(x=x.name, y=y.name.in.model)) + # 
      scale_x_continuous(limits=c(min.x, max.x), xlab) +
      scale_y_continuous(limits=c(y.low.lim, y.hi.lim), ylab) + 
      scale_linetype_manual(moderator.name) + #  
      stat_function(fun=fit, linetype = "solid") + 
      stat_function(fun=fit.extend, linetype = "dashed") +
      geom_vline(xintercept = knot, linetype="dotted", size=0.5, alpha=0.5) +
      theme_linedraw(base_family = "Times New Roman", base_size = 16) 
  } else if (is.null(mdrt.50.name)){
  fit.lo <- function(x){ifelse(x <= knot, constant                                             + b.1*x + b.mdrt*mdrt.05 + b.int1*x*mdrt.05, 
                                        (constant-(b.2-b.1)*knot-(b.int2-b.int1)*knot*mdrt.05) + b.2*x + b.mdrt*mdrt.05 + b.int2*x*mdrt.05)}
  fit.hi <- function(x){ifelse(x <= knot, constant                                             + b.1*x + b.mdrt*mdrt.95 + b.int1*x*mdrt.95, 
                                        (constant-(b.2-b.1)*knot-(b.int2-b.int1)*knot*mdrt.95) + b.2*x + b.mdrt*mdrt.95 + b.int2*x*mdrt.95)}
  ggplot(nameofdata, aes_string(x=x.name, y=y.name.in.model)) + # 
    scale_x_continuous(limits=c(min.x, max.x), xlab) +
    scale_y_continuous(limits=c(y.low.lim, y.hi.lim), ylab) + 
    scale_linetype_manual(moderator.name, values=c("solid", "dashed", "twodash")) + #  
    stat_function(fun=fit.lo, aes(linetype = mdrt.05.name)) +
    stat_function(fun=fit.hi, aes(linetype = mdrt.95.name)) + 
    geom_vline(xintercept = knot, linetype="dotted", size=0.5, alpha=0.5) +
    theme_linedraw(base_family = "Times New Roman", base_size = 16) +
    theme(legend.position = "bottom") +
    scale_colour_tableau()
  } else {
    fit.lo <- function(x){ifelse(x <= knot, constant                                             + b.1*x + b.mdrt*mdrt.05 + b.int1*x*mdrt.05, 
                                 (constant-(b.2-b.1)*knot-(b.int2-b.int1)*knot*mdrt.05) + b.2*x + b.mdrt*mdrt.05 + b.int2*x*mdrt.05)}
    fit.mi <- function(x){ifelse(x <= knot, constant                                             + b.1*x + b.mdrt*mdrt.50 + b.int1*x*mdrt.50, 
                                 (constant-(b.2-b.1)*knot-(b.int2-b.int1)*knot*mdrt.50) + b.2*x + b.mdrt*mdrt.50 + b.int2*x*mdrt.50)}
    fit.hi <- function(x){ifelse(x <= knot, constant                                             + b.1*x + b.mdrt*mdrt.95 + b.int1*x*mdrt.95, 
                                 (constant-(b.2-b.1)*knot-(b.int2-b.int1)*knot*mdrt.95) + b.2*x + b.mdrt*mdrt.95 + b.int2*x*mdrt.95)}
    ggplot(nameofdata, aes_string(x=x.name, y=y.name.in.model)) + # 
      scale_x_continuous(limits=c(min.x, max.x), xlab) +
      scale_y_continuous(limits=c(y.low.lim, y.hi.lim), ylab) + 
      scale_linetype_manual(moderator.name, values=c("solid", "dashed", "twodash")) +
      stat_function(fun=fit.lo, aes(linetype = mdrt.05.name)) +
      stat_function(fun=fit.mi, aes(linetype = mdrt.50.name)) +
      stat_function(fun=fit.hi, aes(linetype = mdrt.95.name)) +
      geom_vline(xintercept = knot, linetype="dotted", size=0.5, alpha=0.5) +
      theme_linedraw(base_family = "Times New Roman", base_size = 16) +
      theme(legend.position = "bottom") +
      scale_colour_tableau()
  }
}
