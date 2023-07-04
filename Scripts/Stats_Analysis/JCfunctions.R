library(stringr)
library(eulerr)
library(rcompanion)
library(tictoc)
options("scipen"=100)

# Run chunks of R code in arbitrary working directory
# From Hadley Wickham
# https://github.com/yihui/knitr/issues/38
in_dir <- function(dir, code) {
  cur <- getwd()
  setwd(dir)
  on.exit(setwd(cur))
  
  force(code)
}

# How many percentage one would lose by doing complete case
comp_remove_perc<-function(data,dp=1,pct=TRUE){
  
  perc<-(nrow(data)-data%>%na.omit()%>%nrow)/nrow(data)
  
  perc_pretty<-pretty_dp(perc,dp=dp,pct=pct)
  
  return(perc_pretty)
  
}

Percentile <- function(x,right=FALSE,
                       probs=c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),
                       labels=c("<1%","1-5%","5-10%","10-20%","20-40%","40-60%","60-80%","80-90%","90-95%","95-99%",">99%")){
  cut(x,breaks=c(quantile(x,probs=probs,na.rm=TRUE)),
      include.lowest=TRUE,
      right=right,
      labels=labels)
  
}


pretty_dp <- function(x, dp=0, pct=FALSE, comma=FALSE){
  if(pct){x <- 100*x}
  if(comma){
    format(round(x, dp), digits=dp, nsmall=dp, big.mark=",") %>% trimws
  } else {
    format(round(x, dp), digits=dp, nsmall=dp) %>% trimws
  }
}

pretty_confint <- function(lci, uci, dp, pct=FALSE){
  paste0("(", pretty_dp(x=lci, dp=dp, pct=pct), ", ", pretty_dp(x=uci, dp=dp, pct=pct), ")")
}

pretty_pval <- function(p, cutoff=0.001, string="<0.001", dp=3){
  ifelse(p<cutoff, string, pretty_dp(p, dp))
}

lower <- function(x){
  paste0(tolower(substring(x, 1,1)), substring(x, 2))
}

upper <- function(x){
  paste0(toupper(substring(x, 1,1)), substring(x, 2))
}

prettyfunc <- function(x, pnames=list(), upper=FALSE, bold=FALSE, flist=c()){
  out <- x
  if(x %in% names(pnames)){
    out <- pnames[[x]]
    if(upper){
      out <- upper(out)
    }
    if(bold){
      out <- paste0("**", out, "**")
    }
  }
  if(x %in% flist){
    if(!exists("footnote_no")){
      footnote_no <<- 1
    }
    out <- paste0(out, "^", footnote_no, "^")
    footnote_no <<- footnote_no + 1
  }
  return(out)
}

diagcollist <- function(colstring, sep="", ncols) {
  x <- 0:ncols
  colstr <- paste0("`",paste0(colstring, sep, x, collapse="`, `"),"`")
  return(colstr)
}

# Print numbers and proportions for factors, median and IQR or mean and 95% CI for continuous variables
# Optionally provide p-values from chi-squared (categorical) and t-test (continuous)
descriptivetable <- function(df, varlist, contavg='mean', assocvar=NULL, pretty_names=list(), footnote_list=c()){
  if(!exists("footnote_no")){footnote_no <<- 1} # Note use of <<- instead of <- to assign this globally
  outtable <- c()
  for(var in varlist){
    if(is.factor(df[[var]])){ # Categorical variables (factors) need a row per level, n's and %'s
      n <- table(df[[var]], useNA='ifany')
      pct <- pretty_dp(prop.table(n), dp=1, pct=TRUE)
      variable <- c(prettyfunc(var, pnames=pretty_names, upper=TRUE, flist=footnote_list), rep(NA, dim(n)-1))
      levels <- names(n)
      if(!is.null(assocvar)){
        tab <- table(df[[assocvar]], df[[var]])
        chi <- chisq.test(tab)
        pval <- c(ifelse(chi$p.value<0.001, "<0.001", round(chi$p.value,3)))
        outtable <- rbind(outtable, 
                          c(var, paste0("**", variable, "**"), "", "", pval), 
                          cbind(paste0(var, levels), levels, n, pct, ""))
      } else{
        outtable<- rbind(outtable, 
                         c(var, paste0("**", variable, "**"), "", ""), 
                         cbind(paste0(var, levels), levels, n, pct))
      }
    } else { # Continuous variables need the mean (and SD) or median (and IQR)
      if(contavg=="mean"){
        n <- pretty_dp(mean(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        pct <- pretty_dp(sd(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        variable <- paste0("Mean ", prettyfunc(var, pretty_names, upper=FALSE, flist=footnote_list), " (SD)")
      } else if (contavg=="median"){
        n <- pretty_dp(median(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        IQR <- pretty_dp(quantile(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        pct <- paste0("(", IQR[2], "-", IQR[4], ")")
        variable <- paste0("Median ", prettyfunc(var, pnames=pretty_names, upper=FALSE, flist=footnote_list), " (IQR)")
      } else if(contavg=="n"){
        n <- nrow(df[!is.na(df[[var]]),])
        pct <- NA
        variable <- prettyfunc(var, pnames=pretty_names, upper=TRUE, flist=footnote_list)
      }
      if(!is.null(assocvar)){
        tt <- t.test(df[[var]][df[[assocvar]]==TRUE], df[[var]][df[[assocvar]]==FALSE])
        pval <- pretty_pval(tt$p.value)
        outtable <- rbind(outtable, cbind(var, paste0("**", variable, "**"), n, pct, pval))
      } else {
        outtable<- rbind(outtable, cbind(var, paste0("**", variable, "**"), n, pct))
      }
    }
  }
  rownames(outtable) <- c()
  if(!is.null(assocvar)){
    colnames(outtable) <- c("IDcol", "Variable", "n", "%", "p")
  } else {
    colnames(outtable) <- c("IDcol", "Variable", "n", "%")
    }
  outdf <- as.data.frame(outtable, stringsAsFactors=FALSE)
  return(outdf)
}

# Print models under MI
printMIresults <- function(df, varlist, modeloutput, expon=TRUE, pretty_names=list(), onecol=FALSE, IDcol=FALSE, forplot=FALSE){
  require(dplyr)
  
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names, onecol=onecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  modeloutput <- modeloutput %>% 
    mutate(
      LCI = estimate - 1.96*std.error,
      UCI = estimate + 1.96*std.error
    ) %>% 
    mutate(., across(.cols=c(estimate, LCI, UCI), .fns=if(expon) exp else NULL))
  
  regression <- data.frame(
    IDcol=modeloutput$term,
    HR=pretty_dp(modeloutput$estimate,dp=2),
    HR_num=modeloutput$estimate,
    CI=pretty_confint(modeloutput$LCI, modeloutput$UCI, dp=2),
    LCI=modeloutput$LCI,
    UCI=modeloutput$UCI,
    p=pretty_pval(modeloutput$p.value),
    stringsAsFactors=FALSE
  )
  
  results <- left_join(coeffnames, regression, by="IDcol")
  if(onecol){
    results$HR[is.na(results$HR) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "1"
    results$HR_num[is.na(results$HR_num) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- 1
  } else {
    results$HR[is.na(results$HR) & !is.na(results$Coefficient)] <- "1"
    results$HR_num[is.na(results$HR_num) & !is.na(results$Coefficient)] <- 1
  }
  
  coeffcols <- colnames(coeffnames)
  if(IDcol==FALSE){
    coeffcols <- coeffcols[coeffcols != "IDcol"]
  }
  
  if(forplot==FALSE){
    results <- results[,c(coeffcols, "HR", "CI", "p")]
    names(results) <- c(coeffcols, "HR", "95% CI", "p")
  } else {
    results <- results[,c(coeffcols, "HR_num", "LCI", "UCI","p")]
  }
  
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
}


# Prettyprint the results from a Cox model
# To use this, 
# model <- coxph(Surv(time_to_dementia, dementia_status) ~ age, data=data)
# kable(printcoxresults(model), caption="")
printcoxresults <- function(df, varlist, modeloutput, pretty_names=list(), onecol=FALSE, IDcol=FALSE,forplot=FALSE,print_beta=FALSE){
  require(dplyr)
  
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names, onecol=onecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  summ <- summary(modeloutput)
  coeff <- summ$coefficients
  conf <- summ$conf.int
  
  regression <- data.frame(
    IDcol=rownames(coeff),
    beta=pretty_dp(coeff[,1], dp=2),
    HR=pretty_dp(coeff[,2], dp=2), # HR
    HR_num=coeff[,2],
    LCI=conf[,3],
    UCI=conf[,4],
    CI=pretty_confint(conf[,3], conf[,4], dp=2), # 95% CI
    beta_CI=pretty_confint(coeff[,1]-(1.96*coeff[,3]),
                           coeff[,1]+(1.96*coeff[,3]),
                           dp=2),
    p=pretty_pval(coeff[,5]), # p-value
    stringsAsFactors=FALSE
  )
  
  
  results <- left_join(coeffnames, regression, by="IDcol")
  if(onecol){
    results$beta[is.na(results$beta) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "Reference"
    results$HR[is.na(results$HR) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "1"
    results$HR_num[is.na(results$HR_num) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- 1
  } else {
    results$beta[is.na(results$beta) & !is.na(results$Coefficient)] <- "Reference"
    results$HR[is.na(results$HR) & !is.na(results$Coefficient)] <- "1"
    results$HR_num[is.na(results$HR_num) & !is.na(results$Coefficient)] <- 1
  }
  
  coeffcols <- colnames(coeffnames)
  if(IDcol==FALSE){
    coeffcols <- coeffcols[coeffcols != "IDcol"]
  }
  
  if(forplot==FALSE){
    if(print_beta==FALSE){
    results <- results[,c(coeffcols, "HR", "CI", "p")]
    names(results) <- c(coeffcols, "HR", "95% CI", "p")
    } else {
      results <- results[,c(coeffcols, "beta", "beta_CI", "p")]
      names(results) <- c(coeffcols, "Beta", "95% CI", "p")
    }
  } else {
    results <- results[,c(coeffcols, "HR_num", "LCI", "UCI","p")]
  }
  
  
  
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
}


# Pretty print the results from a logistic regression model
printlogresults <- function(df, varlist, modeloutput, pretty_names=list(), onecol=FALSE, IDcol=FALSE,forplot=FALSE,print_beta=FALSE){
  require(dplyr)
  
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names, onecol=onecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  summ <- summary(modeloutput)
  coeff <- summ$coefficients
  
  regression <- data.frame(
    IDcol=rownames(coeff),
    beta=pretty_dp(coeff[,1], dp=2),
    OR=pretty_dp(exp(coeff[,1]), dp=2), # OR
    OR_num=exp(coeff[,1]),
    LCI=exp(coeff[,1]-(1.96*coeff[,2])),
    UCI=exp(coeff[,1]+(1.96*coeff[,2])),
    beta_CI=pretty_confint(coeff[,1]-(1.96*coeff[,2]),
                           coeff[,1]+(1.96*coeff[,2]),
                           dp=2),
    CI=pretty_confint(exp(coeff[,1]-(1.96*coeff[,2])),
                      exp(coeff[,1]+(1.96*coeff[,2])),
                      dp=2), # 95% CI
    p=pretty_pval(coeff[,4]), # p-value
    stringsAsFactors=FALSE
  )
  
  results <- left_join(coeffnames, regression, by="IDcol")
  if(onecol){
    results$beta[is.na(results$beta) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "Reference"
    results$OR[is.na(results$OR) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "1"
    results$OR_num[is.na(results$OR_num) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- 1
  } else {
    results$beta[is.na(results$beta) & !is.na(results$Coefficient)] <- "Reference"
    results$OR[is.na(results$OR) & !is.na(results$Coefficient)] <- "1"
    results$OR_num[is.na(results$OR_num) & !is.na(results$Coefficient)] <- 1
  }
  
  coeffcols <- colnames(coeffnames)
  if(IDcol==FALSE){
    coeffcols <- coeffcols[coeffcols != "IDcol"]
  }
  
  if(forplot==FALSE){
    if(print_beta==FALSE){
      results <- results[,c(coeffcols, "OR", "CI", "p")]
      names(results) <- c(coeffcols, "OR", "95% CI", "p")
    }else{
      results <- results[,c(coeffcols, "beta", "beta_CI", "p")]
      names(results) <- c(coeffcols, "Beta", "95% CI", "p")
    }
    
  } else {
    results <- results[,c(coeffcols, "OR_num", "LCI", "UCI")]
  }
  
  
  
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
  
}


# Round to the nearest m
mround <- function(x, base){
  base*round(x/base)
}


# Make a pretty proportion table
# To use this,
# tab <- table(data$VIhyp, data$prevHBP, useNA='ifany')
# kable(propped(tab), caption="")
propped <- function(table, margin=NULL) {
  prop <- round(100*prop.table(table, margin=margin),2)
  tabsums <- addmargins(table)
  dimt <- dim(table)
  for(i in c(1:dimt[1])) {
    if(length(dimt)>1){
      for(j in c(1:dimt[2])) {
        tabsums[i,j] <- paste0(table[i,j], " (", prop[i,j], "%)")
      }
    }
    else{
      tabsums[i] <- paste0(table[i], " (", prop[i], "%)") 
    }
  }
  return(tabsums)
}

preparecoefflist_1col <- function(df, varname, pretty_names=list()){
  # Detect whether has 2-way interaction first
  if(str_detect(varname,":")){
    
    var1=strsplit(varname,":")[[1]][1];var2=strsplit(varname,":")[[1]][2]
    
    pretty_varname1 <- prettyfunc(var1, pnames=pretty_names, bold=FALSE, upper=TRUE)
    pretty_varname2 <- prettyfunc(var2, pnames=pretty_names, bold=FALSE, upper=TRUE)
    
    int_name <-paste0("**",pretty_varname1,"*",pretty_varname2,"**")
    # detect variable type before and after :
    
    if(is.factor(df[[var1]])&is.factor(df[[var2]])){
      # Levels would be combinations of levels of each var (except ref level)
      
      ref_level<-paste(levels(df[[var1]])[1],levels(df[[var2]])[1])
      rest_level<-expand.grid(levels(df[[var1]])[-1],levels(df[[var2]])[-1])
      
      levels<-c(ref_level,paste(rest_level$Var1,rest_level$Var2))  
      variable<-c(int_name,levels)
      
      
      coeffname<-c(int_name,paste0(var1,c(levels(df[[var1]])[1],as.character(rest_level$Var1)),
                                   ":",var2,c(levels(df[[var2]])[1],as.character(rest_level$Var2))))
      
    }else if(is.numeric(df[[var1]])&is.numeric(df[[var2]])){
      variable<-int_name
      coeffname<-varname
      
    }else if(is.factor(df[[var1]])&is.numeric(df[[var2]])){
      levels<-levels(df[[var1]])
      variable <- c(int_name, levels)
      coeffname <- c(int_name, paste0(var1, levels,":",var2))
      
    }else if(is.numeric(df[[var1]])&is.factor(df[[var2]])){
      levels<-levels(df[[var2]])
      variable <- c(int_name, levels)
      coeffname <- c(int_name, paste0(var1,":",var2,levels))
      
    }else{warning("Please check type of variables in interaction")}
    
  }else{
    pretty_varname <- prettyfunc(varname, pnames=pretty_names, bold=TRUE, upper=TRUE)
    if(is.factor(df[[varname]])){
      if(is.ordered(df[[varname]])){
        poly <- length(levels(df[[varname]]))
        levels <- c("Ref", ".L", ".Q", ".C")
        if(poly > 4){
          powers <- c(4:(poly-1))
          levels <- c(levels, paste0("^", powers))
        }
        levels <- levels[1:poly]
      } else {
        levels <- levels(df[[varname]])
      }
      variable <- c(pretty_varname, levels)
      coeffname <- c(pretty_varname, paste0(varname, levels))
    } else {
      variable <- pretty_varname
      coeffname <- varname
    }
    
  }
  output <- data.frame(coeffname, variable, stringsAsFactors=FALSE)
  colnames(output) <- c("IDcol", "Coefficient")
  rownames(output) <- NULL
  
  return(output)
}

preparecoefflist_2col <- function(df, varname, pretty_names=list()){
  # Detect whether has 2-way interaction first
  if(str_detect(varname,":")){
    
    var1=strsplit(varname,":")[[1]][1];var2=strsplit(varname,":")[[1]][2]
    
    pretty_varname1 <- prettyfunc(var1, pnames=pretty_names, bold=FALSE, upper=TRUE)
    pretty_varname2 <- prettyfunc(var2, pnames=pretty_names, bold=FALSE, upper=TRUE)
    
    int_name <-paste0(pretty_varname1,"*",pretty_varname2)
    # detect variable type before and after :
    
    if(is.factor(df[[var1]])&is.factor(df[[var2]])){
      # Levels would be combinations of levels of each var (except ref level)
      
      ref_level<-paste(levels(df[[var1]])[1],levels(df[[var2]])[1])
      rest_level<-expand.grid(levels(df[[var1]])[-1],levels(df[[var2]])[-1])
      
      levels<-c(ref_level,paste(rest_level$Var1,rest_level$Var2))  
      variable<-c(int_name,rep(NA,length(levels)-1))
      
      
      coeffname<-paste0(var1,c(levels(df[[var1]])[1],as.character(rest_level$Var1)),
                        ":",var2,c(levels(df[[var2]])[1],as.character(rest_level$Var2)))
      
    }else if(is.numeric(df[[var1]])&is.numeric(df[[var2]])){
      levels<-NA
      variable<-int_name
      coeffname<-varname
      
    }else if(is.factor(df[[var1]])&is.numeric(df[[var2]])){
      levels<-levels(df[[var1]])
      variable <- c(int_name, rep(NA,length(levels)-1))
      coeffname <- paste0(var1, levels,":",var2)
      
    }else if(is.numeric(df[[var1]])&is.factor(df[[var2]])){
      levels<-levels(df[[var2]])
      variable <- c(int_name, rep(NA,length(levels)-1))
      coeffname <- paste0(var1,":",var2,levels)
      
    }else{warning("Please check type of variables in interaction")}
    
  }else{
    pretty_varname <- prettyfunc(varname, pnames=pretty_names, upper=TRUE)
    if(is.factor(df[[varname]])){
      if(is.ordered(df[[varname]])){
        poly <- length(levels(df[[varname]]))
        levels <- c("Ref", ".L", ".Q", ".C")
        if(poly > 4){
          powers <- c(4:(poly-1))
          levels <- c(levels, paste0("^", powers))
        }
        levels <- levels[1:poly]
      } else {
        levels <- levels(df[[varname]])
      }
      variable <- c(pretty_varname,
                    rep(NA, length(levels)-1))
      coeffname <- paste0(varname,levels)
    } else {
      levels <- NA
      variable <- pretty_varname
      coeffname <- varname
    }
  }
  
  output <- data.frame(coeffname, variable, levels, stringsAsFactors=FALSE)
  colnames(output) <- c("IDcol", "Coefficient", "Levels")
  rownames(output) <- NULL
  return(output)
}



preparecoefflist <- function(onecol=FALSE, ...){
  if(onecol) {
    preparecoefflist_1col(...)
  } else {
    preparecoefflist_2col(...)
  }
}


regressiontable <- function(df, outcome, varlist, regresstype, adjvarlist=c("agegrp", "gender"), pretty_names=list(), IDcol=TRUE){
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names)
  }
  
  if(regresstype=="univariable"){
    modellist <- list()
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome, " ~ ", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    # Vertically concatenate all the pretty outputs into one output table
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="adjusted"){
    modellist <- list()
    
    # Run the regressions for age and gender separately to go on top of the table
    for(adjvar in adjvarlist){
      coeffnames <- preparecoefflist(df=df, varname=adjvar)
      
      formula <- paste0(outcome, " ~ ", paste(adjvarlist, collapse="+"))
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[adjvar]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    
    # Putting age or gender in the regression twice would confuse it, so make sure they're not in the varlist
    varlist <- varlist[!varlist %in% adjvarlist]
    
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome, " ~ ", paste(adjvarlist, collapse="+"), "+", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="multivariable"){
    coeffnames <- do.call(rbind, coefflist)
    formula <- paste0(outcome, " ~ ", paste(varlist, collapse=" + "))
    model <- glm(formula, data=df, family="binomial")
    outdf <- printlogresults(model, coeffnames, IDcol=IDcol)
  }
  
  rownames(outdf) <- NULL
  return(outdf)
}


# Convert output of etmCIF to ggplot acceptable input
#https://stackoverflow.com/questions/56784714/write-a-summary-to-as-data-frame-for-use-in-ggplot-r
etm_to_df <- function(object, ci.fun = "cloglog", level = 0.95, ...) {
  l.X <- ncol(object$X)
  l.trans <- nrow(object[[1]]$trans)
  res <- list()
  for (i in seq_len(l.X)) {
    temp <- summary(object[[i]], ci.fun = ci.fun, level = level)
    res[[i]] <- data.table::rbindlist(
      temp[object$failcode + 1], idcol = "CIF"
    )[, CIF := names(object)[i]]
  }
  do.call(rbind, res)
}

# A function to create labels xxx (N= xxx) (xxx cases, xxx CR cases)
N_cases_CR<-function(data=HESonly_diab,label_name="HES only",status="HESDth_T2D_status",cr_status="cr_status"){
  
  label<-paste0(label_name," (N= ",nrow(data),") \n (",sum(data[[status]])," cases, ",(sum(data[[cr_status]])-sum(data[[status]]))/2," CR cases)")
  
  return(label)
  
}

# Return a dataframe that contains the cumulative incidence (part of the cuminc_plot)
cuminc_df<-function(data,group=1,start="TEU_BaC_AgeAtRec",
                    end="TEU_BrCa_age",cr_status="cr_status",
                    status="TEU_BrCa_status",failcode=1){
  # Start time: TEU_BaC_AgeAtRec; Stop time: TEU_BrCa_age; What counts as events: cr_status!=0
  if(!is.null(start)){
    formula<-as.formula(paste0("Surv(",start,",",end,",", cr_status,"!= 0)~",group))
  }else{formula<-as.formula(paste0("Surv(",end,",", cr_status,"!= 0)~",group))}
  
  
  # etype specifies the type of events; failcode=1 means we are only interested in plotting cr_status==1 which is BrCa incident case
  fit<-etmCIF(formula,data=data,etype = cr_status,failcode=failcode)
  
  # Transform to ggplot acceptable format
  res<-etm_to_df(fit)
  
  # If group variable is not 1, relabel the CIF levels
  if(group!=1){
    res<-res%>%
      mutate(CIF=factor(CIF,levels=paste0(group,"=",levels(data[[group]])),labels = levels(data[[group]])))
  }
  
  return(res)
}

# Plot cumulative incidence function (with competing risk)
cuminc_plot<-function(data,group,start="TEU_BaC_AgeAtRec",end="TEU_BrCa_age",cr_status="cr_status",status="TEU_BrCa_status",failcode=1,smooth=FALSE,
                      title,legend.title,xlim=c(40,79),xlab="Age in years", ylim,
                      caption="Note: Age was truncated at 79 years old;\n CR cases stands for non-BrCa death cases"){
  
  #' Create cumulative incidence plot with competing risk (can adapt left-truncation, smoothing)
  #' Basically a wrapper of etm::etmCIF
  #' https://cran.r-project.org/web/packages/etm/vignettes/etmCIF_tutorial.pdf
  #'
  #' @param data The data
  #' @param group The group of interest, specify 1 if not interested plots by groups
  #' @param start Entry time if left-truncation exists, specify NULL if no left-truncation
  #' @param end Follow up time 
  #' @param cr_status competing risk event indicator (0=censored,1=event of interest,2=competing risk event)
  #' @param status follow up status (i.e.no competing risk indicator) (0=censored,1=event of interest)
  #' @param failcode Indicates the failure type of interest for plotting (default 1)
  #' @param title Title of the plot
  #' @param legend.title Legend title
  #' @param xlim limit of x
  #' @param xlab Label of x-axis
  #' @param ylim limit of y
  #' @param smooth Show smoothed curve using cubic spline technique with 10 knots (default FALSE)
  #' 
  #' @return Cumulative incidence plot
  
  
  # Start time: TEU_BaC_AgeAtRec; Stop time: TEU_BrCa_age; What counts as events: cr_status!=0
  if(!is.null(start)){
    formula<-as.formula(paste0("Surv(",start,",",end,",", cr_status,"!= 0)~",group))
  }else{formula<-as.formula(paste0("Surv(",end,",", cr_status,"!= 0)~",group))}
  
  
  # etype specifies the type of events; failcode=1 means we are only interested in plotting cr_status==1 which is BrCa incident case
  fit<-etmCIF(formula,data=data,etype = cr_status,failcode=failcode)
  
  # Transform to ggplot acceptable format
  res<-etm_to_df(fit)%>%mutate(CIF=factor(CIF,levels=paste0(group,"=",levels(data[[group]])),labels = levels(data[[group]])))
  
  # Number of brca cases and non-brca dth cases within each PRS category
  labels=data%>%group_by(!!sym(group))%>%summarise(n=sum(!!sym(status)),nonbr=(sum(!!sym(cr_status))-sum(!!sym(status)))/2)%>%na.omit()
  
  if(smooth==FALSE){
    plot<-ggplot(res, aes(time, P)) +
      #geom_ribbon(aes(ymin = lower, ymax = upper, fill = CIF), alpha = 0.2) +
      geom_line(aes(color = CIF),size=1) +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(limits = ylim) +
      labs(x=xlab,y="Cumulative incidence",title = title,caption = caption)+
      scale_color_discrete(name=legend.title,
                           breaks=unique(res$CIF),
                           labels=paste(labels[[group]],"(",labels$n,"events,", labels$nonbr,"CR cases)"))+
      theme_bw()
    
  }else{
    
    # Smoothing using spline
    plot<-ggplot(res)+geom_spline(aes(x=time,y=P,colour=CIF),nknots = 10) +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(limits = ylim) +
      labs(x=xlab,y="Cumulative incidence",title = title,caption = caption)+
      scale_color_discrete(name=legend.title,
                           breaks=unique(res$CIF),
                           labels=paste(labels[[group]],"(",labels$n,"events,", labels$nonbr,"CR cases)"))+
      theme_bw()
    
  }
  
  return(plot)
}

# Plot cumulative incidence function (without competing risk)
cuminc_nocr_plot<-function(data,start,end,status,group,title,legend.title,xlab="Age in years",xlim=c(40,80)){
  
  if(!is.null(start)){
    formula<-as.formula(paste0("Surv(",start,",",end,",", status,")~",group))
  }else{formula<-as.formula(paste0("Surv(",end,",", status,")~",group))}
  
  
  fit<-surv_fit(formula,data=data)
  
  labels=data%>%group_by(!!sym(group))%>%summarise(n=sum(!!sym(status)))%>%na.omit()
  
  ggsurv<-ggsurvplot(fit,data=data,fun="event",title=title,
                     xlab=xlab,ylab="Cumulative incidence",#risk.table = TRUE,
                     xlim=xlim,break.x.by=5,
                     censor.size=1,size=1,ggtheme = theme_bw(),
                     legend.labs=paste(labels[[group]],"(",labels$n,"events)"),legend.title=legend.title)
  
  plot=ggsurv$plot+theme(legend.position="left")
  
  return(plot)
}


# Process analysis table of PRSonDiab for pretty presentation
# The main thing is to pivot the original printlogresults with PRS (Q1,..Q5) as the header
diab_tab<-function(df=data,varlist,modeloutput,interaction_var=NULL,analysis_name,pvalue){
  
  if(is.null(interaction_var)){
    # If the model doesn't have interaction term, we use our internal function printxxxresults
    if(class(modeloutput)[1]=="coxph"){
      tab<-printcoxresults(df,varlist ,pretty_names = pretty_names,modeloutput)
      # Hacky: Temporarily rename the column HR to OR to avoid breaking the code below
      tab=tab%>%rename(OR=HR)
      
    }else{
      # After model fitting, get the pretty table out
      tab<-printlogresults(df,varlist ,pretty_names = pretty_names,modeloutput)
    }
    
  }else{
    # If the model has interaction term, we use Publish package
    int_tab<-publish(modeloutput,print = FALSE,ci.format="(l,u)")
    
    if(class(modeloutput)[1]=="coxph"){
      # Hacky: Temporarily rename the column HR to OR to avoid breaking the code below
      int_tab$regressionTable=int_tab$regressionTable%>%rename(OddsRatio=HazardRatio)
    }
    
    # Process table output to match our printxxxresults format
    
    tab<-int_tab$regressionTable%>%
      select(!Units)%>%
      # Only keep the comparisons of interest (eg. interaction_var=TEU_BSM_BMI)
      # (e.g. HR of PRS within each category of BMI)
      filter(grepl(paste0('^',interaction_var),Variable))%>%
      # Separate out the Variable column
      # https://tidyr.tidyverse.org/reference/separate.html
      separate(Variable,c("Strata","Levels"),":",extra = "merge")%>%
      # Keep only content within the bracket
      mutate_at(c("Strata","Levels"),~gsub("[\\(\\)]", "", regmatches(., gregexpr("\\(.*?\\)", .))))%>%
      # Remove "vs Q1: lowest"
      mutate(Levels=str_remove(Levels," vs Q1: lowest"))%>%
      rename(OR=OddsRatio,`95% CI`=CI.95)
    
  }

  
  # Some processing
  tab_aft<-tab%>%
    mutate(`95% CI`=replace_na(`95% CI`," "))%>%
    mutate(OR_CI=paste0(OR," ",`95% CI`))%>%
    select(any_of(c("Strata","Levels","OR_CI")))%>%
    # transpose
    spread(Levels,OR_CI)%>%
    # Add analysis name at the beginning
    add_column(Analysis=analysis_name,.before = 1)%>%
    add_column(p=pvalue)
  
  if(is.null(interaction_var)){
    tab_aft<-tab_aft%>%
      add_column(Strata="",.after = "Analysis")
  }else{ # If there is interaction term 
    tab_aft<-tab_aft%>%
      add_column(`Q1: lowest`="1",.after="Strata")
    
    # Display interaction according to levels in the data
    tab_aft=tab_aft[match(levels(df[[interaction_var]]),tab_aft$Strata),]
  }
  
  return(tab_aft)
  
}

# Leicester risk score system
# https://pubmed.ncbi.nlm.nih.gov/20653746/
var_to_score <- function(x) {
  recode(x,
         "40-49" = 0,
         "50-59" = 5,
         "60-69" = 9,
         "Male"  = 1,
         "Female"= 0,
         "White"=0,
         "Other"=6,
         "No family history of Diab" = 0,
         "Family history of Diab" = 5,
         "<90" = 0,
         "90-99" = 4,
         "100-109" = 6,
         ">=110" = 9,
         "<25" = 0,
         "25-29" = 3,
         "30-34" = 5,
         ">=35" = 8,
         "No" = 0,
         "Yes" = 5,
         .default = NaN
  )
}

# Leicester score betas to compute probability
var_to_beta <- function(x) {
  recode(x,
         "40-49" = 0,
         "50-59" = 0.5346095,
         "60-69" = 0.9403614,
         "Male"  = 0,
         "Female"= 0.0883173,
         "White"=0,
         "Other"=0.569434,
         "No family history of Diab" = 0,
         "Family history of Diab" = 0.465857,
         "<90" = 0,
         "90-99" = 0.4261962,
         "100-109" = 0.5632965,
         ">=110" = 0.8590326,
         "<25" = 0,
         "25-29" = 0.260582,
         "30-34" = 0.4528574,
         ">=35" = 0.7528332,
         "No" = 0,
         "Yes" = 0.458373,
         .default = NaN
  )
}

Sens_Spec<-function(threshold,data=rs_train,risk_score,name,outcome="TEU_T2DM_status"){
  
  df_total=data.frame()
  
  for (i in threshold) {
    
    predicted_values<-ifelse(data[[risk_score]]>=i,1,0)
    actual_values<-data[[outcome]]
    conf_matrix<-table(predicted_values,actual_values)
    
    Sens<-sensitivity(conf_matrix,positive = "1")
    Spec<-specificity(conf_matrix,negative="0")
    
    df<-data.frame(score=name,threshold=paste0(">= ",i),sensitivity=pretty_dp(Sens,2),specificity=pretty_dp(Spec,2))
    
    df_total <- rbind(df_total,df)
    
  }
  
  return(df_total)
}

AUC_CI<-function(df,status="TEU_T2DM_status",score,method="delong"){
  
  roccurve<-pROC::roc(df[[status]]~df[[score]],quiet=TRUE)
  
  AUCs_train<-pROC::ci.auc(roccurve,quiet=TRUE,method=method)
  
  result<-paste0(pretty_dp(AUCs_train[2],3),pretty_confint(AUCs_train[1],AUCs_train[3],dp=3))
  
  return(result)
  
}

Reclassification_bar<-function(data,risk_cat=diab_rs_binary,name="Leicester risk score"){
  
  tab<-data%>%
    mutate(`Risk category`=factor(ifelse({{risk_cat}}==1,"Low","High"),levels = c("Low","High")))%>%
    group_by(`Risk category`)%>%
    summarise(n_cases=sum(TEU_T2DM_status),N=n())%>%
    mutate(perc_cases=(n_cases/N)*100,
           n_controls=N-n_cases,
           perc_controls=(n_controls/N)*100)%>%
    mutate(score=name)
  
  return(tab)
}


# Produce dataframe for calibration plot
Calibration_df<-function(data=rs_train,predRisk="Leicrs_prob",groups=10,times=10,format="wide"){
  
  #' @param data The data
  #' @param predRisk The column name that records the predicted probability
  #' @param groups Categorise ppl into how many groups (usually 10 because decile)
  #' @param times Observed risk at what time (default is 10-year risk) We use survival outcome here
  #' @param format Produce output table in wide or long format. Use wide to get the standard calibration plot. Use long to plot RP's format. 
  
  # Potential improvement: Below is only for T2D survival outcomes, one can make it into a argument in the future.
  
  # Categorise participants into different decile groups
  # For internal consistency, Do not use cut2 function.
  data<-data%>%mutate(
    pred_deciles=Percentile((!!sym(predRisk)),probs=seq(0, 1, 1/groups),labels = c(1:groups),right=TRUE))
  
  # If we use the actual probability from the logistic regression model
  pred<-data%>%group_by(pred_deciles)%>%summarise(value=mean((!!sym(predRisk))))%>%
    mutate(group=as.numeric(pred_deciles),variable="Pred")%>%
    select(group, variable, value)
  
  # Observed risk in each decile
  fit<-survfit(Surv(TEU_T2DM_time,TEU_T2DM_status)~pred_deciles,data=data)
  
  sum10<-summary(fit,times = times)
  
  sum10_df <- data.frame(group=sum10$strata,
                         surv = sum10$surv
  )
  
  obs<-sum10_df%>%
    mutate(group=as.numeric(str_remove(group,"pred_deciles=")),
           value=1-surv,
           variable="Obs")%>%
    select(group,variable,value)
  
  if(format=="wide"){
    pred_obs_wide=inner_join(pred,obs,by="group",suffix=c(".pred",".obs"))
    return(pred_obs_wide)
    
  }else{
    
    pred_obs_long<-rbind(pred,obs)
    return(pred_obs_long)
  }
}
  
  
# HES vs GP timeline & diagram
HESvsGP<-function(data,outcome_label,GP_censoring,HESDth_censoring,GP_eventdate,HESDth_eventdate){
  
  cat("For ",outcome_label," N= ",nrow(data),": \n")
  
  # Rename columns 
  data<-data%>%
    rename(GP_censoring=!!sym(GP_censoring),
           HESDth_censoring=!!sym(HESDth_censoring),
           GP_eventdate=!!sym(GP_eventdate),
           HESDth_eventdate=!!sym(HESDth_eventdate))
  
  # Q1: How many HES/Dth cases captured between baseline and end of PC
  HESDth_endPC=data%>%filter(!is.na(HESDth_eventdate) & 
                                 HESDth_eventdate<=GP_censoring)%>%nrow()
  
  cat("How many HES/Dth cases captured between baseline and GP censoring: ",HESDth_endPC,"\n")
  
  # Q2: Overlap, Within baseline and end of PC, how many cases had both HES/Dth and PC? 
  overlap_data=data%>%filter(!is.na(HESDth_eventdate) & HESDth_eventdate<=GP_censoring &
                                    !is.na(GP_eventdate) & GP_eventdate<=GP_censoring)
  
  cat("Within baseline and GP censoring, how many cases were identified in both HES/Dth and GP:",
      nrow(overlap_data),"(",round((nrow(overlap_data)/HESDth_endPC)*100,1),"%) \n")
  
  # Q3: How many GP cases captured between baseline and end of PC
  GP_endPC=data%>%filter(!is.na(GP_eventdate) & GP_eventdate<=GP_censoring)%>%nrow()
  
  # Venn diagram
  fit <- euler(c("HES/Dth" = HESDth_endPC-nrow(overlap_data), 
                 "GP" = GP_endPC-nrow(overlap_data), "HES/Dth&GP" = nrow(overlap_data)))
  
  Venn_diag<-plot(fit,quantities = list(col = "black", font = 17,fontsize=25),
                  fills = list(fill = c("steelblue2", "purple"), alpha = 0.5),
                  labels = list(col = "black", font = 17,fontsize=25))
  
  # Create histogram of differences in dx time
  diffs<-as.numeric((overlap_data$HESDth_eventdate-overlap_data$GP_eventdate)/365.25)
  
  # Plot histogram
  #Hist<-hist(diffs,main = paste0("HES/Dth dx date-GP dx date among overlaping ",nrow(overlap_data) ," incident ",outcome_label," cases"),
       #xlab = "Time difference in years")
  
  # Q4: How many HES/Dth cases happened after GP censoring
  Extra_HESDth<-data%>%filter(!is.na(HESDth_eventdate) & 
                                HESDth_eventdate>GP_censoring & 
                                HESDth_eventdate<=HESDth_censoring)%>%nrow()
  cat("How many HES/Dth cases captured after GP censoring: ",Extra_HESDth,"\n")
  
  # Create a venn diagram but with HES/Dth only after GP censoring
  fit_aft <- euler(c("HES/Dth" = Extra_HESDth))
  
  Venn_diag_aft<-plot(fit_aft,quantities = list(col = "black", font = 17,fontsize=25),
                  fills = list(fill = c("steelblue2"), alpha = 0.5),
                  labels = list(col = "black", font = 17,fontsize=25))
  
  
  # Q4: How many of the extra PC cases ended up in HES/Dth at a later point
  Extra_GP<-data%>%filter(!is.na(GP_eventdate) & GP_eventdate<=GP_censoring &
                            !is.na(HESDth_eventdate) & HESDth_eventdate>GP_censoring & 
                            HESDth_eventdate<=HESDth_censoring)%>%nrow()
  
  cat("How many of the extra GP cases ended up in HES/Dth at a later point: ",Extra_GP,"\n")
  
  # Q5: Time difference in dx time but go beyond GP censoring among those who had a GP dx
  GP_HESdx<-data%>%
    # filter people with a GP dx
    filter(!is.na(GP_eventdate) & GP_eventdate<=GP_censoring)%>%
    # create time diff in years HES dx - GP dx for those with a HES dx before full fu
    mutate(timediff=ifelse(!is.na(HESDth_eventdate) & HESDth_eventdate<=HESDth_censoring,
                           (HESDth_eventdate-GP_eventdate)/365.25,NA))

  return(list(Venn_diag,diffs,Venn_diag_aft,GP_HESdx$timediff))
  
  }
  
  
  
# Visualise timeline of GP registrations and diagnoses 
# Note: can only run on small number of participants (e.g. 10-15)!

cust_labeller <- function(x) paste0("ID=", x) #Pretty label for ID

GP_regdx_plot<-function(reg_data,clinical_data,seed=1,n=5){
  #' @param reg_data GP registration data
  #' @param clinical_data GP clinical data (the one with dx)
  #' @param seed For reproducibility 
  #' @param n Number of participants to plot
  
  # pick random people from this cohort
  set.seed(seed)
  ID=sample(reg_data$eid,n)
  one_reg <- reg_data[eid %in% ID]
  
  one_clinical<-clinical_data[eid %in% ID]
  
  # Plot timeline of registration and dx
  p<-ggplot() +
    geom_rect(data = one_reg,
              aes(xmin = reg_date, xmax = deduct_date,fill = "Registered with GP"),
              ymin = 0, ymax = 1)+
    facet_wrap(~eid, ncol=1,labeller = as_labeller(cust_labeller))+
    geom_linerange(data = one_clinical,
                   aes(x = event_dt, y = 0, ymin = 0, ymax =1,colour="event"))+
    scale_x_date(date_breaks = "5 years",date_labels = "%Y") +
    scale_fill_manual(name = NULL,
                      limits = "Registered with GP",
                      values = "grey") +
    scale_colour_manual(name = NULL,
                        limits = c("event"),
                        labels = c("Diagnosis/event"),
                        values = c("#0077bb")) +
    labs(title=paste0("Timeline of GP registrations and diagnoses of ", n," random participants"),y="",
         caption="Each row represents the timeline for one participant.")+
    #theme_minimal() +
    theme(text = element_text(size = 14),
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),legend.position = "bottom",
          legend.margin = margin(t = 0, l = 1, r = 1, b = 0)
    ) 
  
  return(p)
}

# Expression of stack one on top of the other
sfrac <- function(top,bottom,data=NULL){
  with(data,lapply(paste0("atop(",top,",",bottom,")"),str2expression))
}

# Expression of stack one on top of the other but keeping trailing 0 (for plotting forestplot with multiple groups)
# atop is for stacking
sfrac_XL <- function(top,bottom,data=NULL){
  with(data,lapply(paste0("atop('",top,"','",bottom,"')"),function(i) parse(text=i)))
}

# Below is for less space between two HR
sfrac2 <- function(top,bottom,data=NULL){
  with(data,lapply(paste0("atop(NULL,atop('",top,"','",bottom,"'))"),str2expression))
}
  
# Create function for forestplot showing HR of RFs obtained using HES only and HES+GP populations
HESvsGP_forestplot<-function(modeloutput1=HESonly_forplot,modeloutput2=HESGP_forplot,Bootstrap_ci_df=NULL,
                             clip=c(0,7),xticks=seq(0.0, 7, 0.5)){
  # modeloutput 1 and 2 should be outputs of printcoxresults()
  # Specify Bootstrap_ci_df if want bootstrap CI of ratio of HR
  
  # Wide format
  forplot<-full_join(modeloutput1%>%select(IDcol,Coefficient,HR_num,LCI,UCI,HR_CI),
                     modeloutput2%>%select(IDcol,HR_num,LCI,UCI,HR_CI),
                     by="IDcol",suffix=c(".HES",".HESGP"))
  
  # Fix for plotting: Remove HR_CI of 1 to blank 
  forplot$HR_CI.HES[is.na(forplot$LCI.HES)] <- ""
  # We want to show HR=1 for HES+GP pop
  forplot$HR_CI.HESGP[is.na(forplot$HR_CI.HESGP)]<-""
  
  ##### Delete below if don't want to plot HR=1 in plot
  
  # Replace LCI and UCI for reference level to 1 for plotting
  forplot$LCI.HES[is.na(forplot$LCI.HES) & forplot$IDcol!=forplot$Coefficient]<-1
  forplot$UCI.HES[is.na(forplot$UCI.HES) & forplot$IDcol!=forplot$Coefficient]<-1
  
  forplot$LCI.HESGP[is.na(forplot$LCI.HESGP) & forplot$IDcol!=forplot$Coefficient]<-1
  forplot$UCI.HESGP[is.na(forplot$UCI.HESGP) & forplot$IDcol!=forplot$Coefficient]<-1
  #####
  
  forplot<-forplot%>%
    # Add space for levels of categorical vars
    mutate(Coefficient=ifelse(IDcol!=Coefficient,paste0("       ",Coefficient),Coefficient))%>%
    # Change variables to pretty names
    rowwise()%>%
    mutate(Coefficient=pretty_func(Coefficient))
  
  # Produce the forest plot
  
  # https://github.com/gforge/forestplot/issues/54
  # Apparently for group_by, labeltext have to be identical between groups.
  if(is.null(Bootstrap_ci_df)){
    
    # Display text
    tabletextdummy<- list(
      c("Covariates",forplot$Coefficient),
      c("HR (95% CI)",sfrac_XL(forplot$HR_CI.HES,forplot$HR_CI.HESGP,data=forplot))
    )
    
  }else{
    
    # Merge with RHR results
    forplot_RHR<-left_join(forplot,Bootstrap_ci_df,by="IDcol")
    # Plot fix: Replace RHR NA to blank 
    forplot_RHR$RHR_CI[is.na(forplot_RHR$RHR_CI)] <- ""
    
    # Display text
    tabletextdummy<- list(
      c("Covariates",forplot_RHR$Coefficient),
      c("HR (95% CI)",sfrac_XL(forplot_RHR$HR_CI.HES,forplot_RHR$HR_CI.HESGP,data=forplot_RHR)),
      c("RHR (95% CI)",forplot_RHR$RHR_CI)
    )
    
  }
  
  # Plot
  forestplot<-forestplot(tabletextdummy, 
                         mean= cbind(c(NA, forplot$HR_num.HES), c(NA,forplot$HR_num.HESGP)),
                         lower = cbind (c(NA,forplot$LCI.HES), c(NA,forplot$LCI.HESGP)), 
                         upper = cbind(c(NA,forplot$UCI.HES), c(NA, forplot$UCI.HESGP)),
                         new_page = TRUE,
                         clip=clip,
                         xticks=xticks,
                         #lineheight = unit(50,"mm"), # contradict with legend below
                         line.margin = .015, # Add this to avoid crowding (higher value more crowded)
                         xlog = FALSE, zero=1, 
                         col = fpColors(box = c("skyblue3","red4"), 
                                        lines = c("skyblue2","red3")),
                         fn.ci_norm = c(fpDrawNormalCI, fpDrawDiamondCI),
                         is.summary = c(TRUE,rep(FALSE,nrow(forplot))), 
                         graph.pos = 2,
                         boxsize = 0.3, 
                         legend = c("HES only", "HES+GP"),
                         vertices = TRUE,
                         colgap=unit(3, 'mm'),
                         graphwidth=unit(80, 'mm'), # width of the plot
                         lwd.ci=2,
                         lwd.xaxis=2,
                         txt_gp = fpTxtGp(ticks=gpar(cex=1),
                         label=gpar(cex=1.3),legend=gpar(cex=1)))%>%
    fp_set_zebra_style("#F5F9F9")%>%
    fp_add_lines("black")
  
  return(forestplot)
}  
  

# Create function that compute the ratio of HR that could be used for bootstrap later
RHR<-function(data,i,surv_outcome="Surv(TEU_PD_time, TEU_PD_status)",vars){
  # argument i doesn't do anything, it's required by boot function. It will basically use all data.
  
  # Cox model formula
  formula<-as.formula(paste0(surv_outcome,"~ ", paste(vars, collapse="+")))
  
  # HES only
  HESonly_data<-data[i,]%>%filter(ind=="HESonly")
  HESonly_Cox<-coxph(formula,data=HESonly_data)
  
  # HES + GP
  HESGP_data<-data[i,]%>%filter(ind=="HES+GP")
  HESGP_Cox<-coxph(formula,data=HESGP_data)
  
  # Get HR from two models
  HR_HESonly<-exp(coef(HESonly_Cox))
  HR_HESGP<-exp(coef(HESGP_Cox)) 
  
  hr_ratio<-HR_HESonly/HR_HESGP
  
  return(hr_ratio)
}

# Below is not compatible for MI data
BootCI_df<-function(BootResult_object,conf=0.95,i,type="perc"){
  # i indicates the index (numeric) of which variable HR corresponds to
  
  bootci<-boot.ci(BootResult_object, type=type,conf=conf,index=i)
  # Create a df for each index
  df<-data.frame(x=pretty_dp(bootci$t0,dp=2),
                 CI=pretty_confint(bootci$percent[4],
                                   bootci$percent[5],dp=2))%>%
    rownames_to_column(., "IDcol")%>%
    mutate(RHR_CI=paste0(x," ",CI))%>%
    select(IDcol,RHR_CI)
  
  return(df)
  
}

# Below are compatible for MI data
# Below bootstraps RHR within each imputed dataset and return the observed, bootstrap RHR, and their difference in each imputation 

BootMI_RHR<-function(data_MI,surv_outcome="Surv(TEU_PD_time, TEU_PD_status)",
                     m=10,R=5,vars=vars,seed=10,label="PD"){
  
  #' @param data_MI Multiple imputed dataset in long format (do not include original data that has missing data)
  #' @param surv_outcome Survival outcome for computing RHR
  #' @param m Number of imputations used
  #' @param R Number of bootstrap 
  #' @param var Vector of covariates used in Cox models
  #' @param seed seed for reproduction
  #' @param label label used in saving data
  
  # Store observed, bootstrap RHR and its differences computed using each imputed dataset
  RHR_imp_list<-list()
  
  for (i in 1:m) {
    
    tic()
    # Within each imputation
    imp1<-data_MI%>%filter(.imp==i)
    
    # Run bootstrap
    
    set.seed(seed)
    
    Bootstrap_result = boot(data=imp1,
                            statistic=RHR,
                            stype = "i",
                            strata = factor(imp1$ind),
                            R=R,
                            vars=vars,
                            surv_outcome=surv_outcome,
                            parallel = "snow")
    
    saveRDS(Bootstrap_result,file = paste0(config$data$derived,"/Boot_MI/Bootstrap_",label,"_imp",i,".rds"))
    
    # Rbind t0 R times to compute theta (i.e. difference)
    rep_t0<-do.call("rbind", replicate(R, Bootstrap_result$t0, simplify = FALSE))
    
    # theta = observed - bootstrap values (for percentiles later)
    theta<-rep_t0-Bootstrap_result$t
    
    name<-paste0("Imp",i)
    
    # return t,t0, theta for each imputation
    tmp<-list(t0=Bootstrap_result$t0,t=Bootstrap_result$t,theta=theta)
    
    RHR_imp_list[[name]]<-tmp
    
    cat("Imputation ",i," finished!")
    
    toc()
    
  }
  return(RHR_imp_list)
}

## After imputation, compute the mean of t0 across imputations and CI based on percentile of theta

# Below pools the RHR across imputations by taking the mean and computes 
# the 95% CI of RHR using MI boot (method 1 in Schomaker2018) with empirical bootstrap.

BootMI_RHR_pool<-function(BootMI_RHR_outputs){
  
  #' @param BootMI_RHR_outputs Output obtained from running BootMI_RHR function 
  
  ## First compute the pooled estimate (e.g. mean of RHR from all imputations)
  
  # rbind t0 from each imputed dataset
  t0_all<-do.call(rbind, lapply(BootMI_RHR_outputs, `[[`, "t0"))
  
  # compute pooled estimate (i.e. mean of each column)
  pool_t0<-setNames(stack(colMeans(t0_all)),c("values","IDcol"))
  
  ## Second compute the CI 
  
  # rbind theta from each imputed dataset
  theta_all<-do.call(rbind, lapply(BootMI_RHR_outputs, `[[`, "theta"))
  
  # Compute the percentiles of each column
  theta_percentiles=lapply(1:ncol(theta_all),
                           function(i) quantile(theta_all[,i], 
                                                probs = c(.025, .975), type = 6))
  names(theta_percentiles)<-colnames(theta_all)
  
  theta_percentiles_df=bind_rows(theta_percentiles,.id="IDcol")
  
  # Construct 95% CI for the estimate (obs-theta percentile 0.975,obs-theta percentile 0.02s5)
  df<-inner_join(pool_t0,theta_percentiles_df,by="IDcol")%>%
    mutate(x=pretty_dp(values,dp=2),
           lower_CI=values-`97.5%`,
           upper_CI=values-`2.5%`)%>%
    mutate(CI=pretty_confint(lower_CI,upper_CI,dp=2))%>%
    mutate(RHR_CI=paste0(x," ",CI))%>%
    select(IDcol,RHR_CI)
  
  return(df)
}


# Correlation matrix for mixed data type

# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}


#Compare summary stats between observed and imputed (or completed) data
MI_compare<-function(MI_data=HESonlyMI_Males,imp1=Imp_males,missing_var){
  
  # Extract completed data after MI
  long1 <- complete(imp1,"long") 
  long1$.imp <- as.factor(long1$.imp)
  
  # Produce density plots for continuous variables and frequency table for categorical variables
  
  num_plot <- list() #Save the density plots
  factor_tb <- list() # Save the freq tables 
  
  for (var in missing_var){
    
    if (is.numeric(MI_data[[var]])){
      # If numeric, plot density between observed and imputed 
      
      #long1 <- cbind(long1, ind.na=is.na(imp1$data[[var]]))
      long1 <- long1 %>%
        mutate(ind.na=rep(is.na(imp1$data[[var]]),10))
      
      p<-densityplot(~long1[[var]]|.imp, data=long1, group=ind.na, plot.points=FALSE,
                     ref=TRUE, xlab=paste0(var),scales=list(y=list(draw=F)),
                     par.settings=simpleTheme(col.line=rep(c("blue","red"))), 
                     auto.key = list(columns=2,text=c("Observed","Imputed")))
      
      num_plot[[var]]=p
      
    }else{
      # If factor, produce freq table of each level between observed and imputed 
      long1 <- long1 %>%
        mutate(ind.na=rep(ifelse(is.na(imp1$data[[var]]),'Imputed','Observed'),10))
      
      factor_tb[[var]] <- lapply(unique(long1$.imp), function(i) 
        prop.table(table(long1%>%filter(.imp==i)%>%pull(ind.na),
                         long1%>%filter(.imp==i)%>%pull(var)), margin = 1)*100)
      
    }
  }
  
  return(c(num_plot,factor_tb))
}
