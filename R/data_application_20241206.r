########################
########################
##### DATA EXAMPLE #####
########################
########################

#############,
# LOAD PKGS #
#############

install.packages(c("haven","dplyr","lubridate","splines"))
library(haven)
library(dplyr)
library(lubridate)
library(splines)

#############
# SET PATHS #
#############

indata_path  <- "S:/SUEDEHAT/YJHsu/RE cohort/data/"
outdata_path <- "S:/SUEDEHAT/John/target_trial_model/data/"
output_path  <- "S:/SUEDEHAT/John/target_trial_model/output/"

#################
# DIRECT OUTPUT #
#################

sink(file=paste0(output_path,"output_application_2024_12_06.txt"),append=FALSE,type="output")

#############
# LOAD DATA #
#############

df_raw<-read_dta(paste0(indata_path,"09_RE cohort.dta"))

######################
# DESCRIBE RAW DATA  #
######################

# CREATE VARIABLES INDICATING INDEX IN 2014 AND 2015 #

pyr_table<-table(df_raw$PATID,year(df_raw$MEASURE_DATE_INDX))
id_names<-rownames(pyr_table)
pyr_each_year<-apply(pyr_table,2,function(x) ifelse(x>0,1,0))
pyr_both_year<-apply(pyr_each_year,1,function(x) x[1]*x[2])
pyr_years<-data.frame(id_names,pyr_each_year,pyr_both_year)
names(pyr_years)<-c("PATID","HAD_VISIT_IN_2014","HAD_VISIT_IN_2015","HAD_VISIT_IN_2014_AND_2015")
df_raw<-left_join(df_raw,pyr_years,"PATID")

# VARIABLES FOR USE #

names(df_raw)
vars_id<-c("PATID","ENCOUNTERID_INDX","ENC_TYPE_INDX","SITE_ID_INDX","STATE")
vars_date<-c("MEASURE_DATE_INDX","DIAG_HYPERTENSION","START_DATE_DIAG_HYPERTENSION","DX_DATE_DIAG_HYPERTENSION","START_DATE_DIAG_PREGNANCY","DX_DATE_DIAG_PREGNANCY","START_DATE_DIAG_ESRD","DX_DATE_DIAG_ESRD")
vars_factor<-c("SEX","RACEG","PAYER_PRIMARY_INDX","EMPLOYMENT_STATUS","DIABETES","CKD","HLD","MYCHART_INDX","HAD_VISIT_IN_2014","HAD_VISIT_IN_2015","HAD_VISIT_IN_2014_AND_2015")
vars_continuous<-c("SBP_INDX","DBP_INDX","AGE","ANTI_HTN_NO","RPL_THEMES","PREVISIT","PREVISIT_SAMED")
vars_numeric<-c("DIAG_PREGNANCY","DIAG_ESRD","PREVISIT")
all(vars_id %in% names(df_raw))
all(vars_date %in% names(df_raw))
all(vars_factor %in% names(df_raw))
all(vars_continuous %in% names(df_raw))

# SUBSET TO VARIABLES FOR USE #

df_use <-df_raw[,c(vars_id,vars_date,vars_factor,vars_continuous,vars_numeric)]

# CREATE COMORBIDITY PRESENCE #

Ibeforeindex   <- function(x,y) {ifelse(x>=y,1,0)}
dx_date        <- df_raw %>% select(starts_with("DX_DATE_DIAG"),-DX_DATE_DIAG_HYPERTENSION)
dx_date_col_yn <- sapply(dx_date,Ibeforeindex,df_use$MEASURE_DATE_INDX)
dx_date_any    <- apply(dx_date_col_yn,1,sum,na.rm=TRUE)

dx_start        <- df_raw %>% select(starts_with("START_DATE_DIAG"),-DX_DATE_DIAG_HYPERTENSION)
dx_start_col_yn <- sapply(dx_start,Ibeforeindex,df_use$MEASURE_DATE_INDX)
dx_start_any    <- apply(dx_start_col_yn,1,sum,na.rm=TRUE)
df_use$ANY_COMORBIDITY <- ifelse(dx_date_any>0 | dx_start_any>0 | df_use$CKD | df_use$DIABETES | df_use$HLD,1,0) #edit 7/19/2022 added inequality operators

#######################
# INITIAL DATA CHECKS #
#######################

lapply(df_use[,vars_id[c(3,4,5)]],table)
lapply(df_use[,vars_factor],table)
lapply(df_use[,vars_continuous],fivenum)

nrow(df_use) #108483 starting records
table(df_use$ANTI_HTN_NO>=2) #74555 with at least two prior scripts
table(is.na(df_use$MEASURE_DATE_INDX)) #108483 (everyone) has an encounter index date
table(is.na(df_use$DX_DATE_DIAG_HYPERTENSION)) #72473 have a diagnosis date for hypertension
table(df_use$MEASURE_DATE_INDX-df_use$DX_DATE_DIAG_HYPERTENSION>=180) #of those, 10421 have a diagnosis date at least 6 months before index
table(!is.na(df_use$START_DATE_DIAG_HYPERTENSION)) #36010 have a start date for hypertension
table(df_use$MEASURE_DATE_INDX-df_use$START_DATE_DIAG_HYPERTENSION>=180) #of those, 22578 have a start date at least 6 months before index date.
table(df_use$STATE=="MD") #of all records, 102204 live in Maryland.
table(df_use$RACEG==1 | df_use$RACEG==2) #99109 have Black or White race recorded. 
table(df_use$ENC_TYPE_INDX) #108310 are office visits; 167 are clinical support visits; 6 are follow-up visits.
table(df_use$PREVISIT>=2) #108460 (everyone) has at least two PCP visits
table(df_use$PREVISIT_SAMED>=2) #107400 (most) have at least two PCP visits at same department/cite
table(df_use$ANY_COMORBIDITY) #76631 have at least one comorbidity besides hypertension (based on problem list)
table(df_use$HAD_VISIT_IN_2014) #89193 had visit in 2015
table(df_use$HAD_VISIT_IN_2015) #95823 had visit in 2015
table(df_use$HAD_VISIT_IN_2014_AND_2015) #76533 had visit in 2014 and 2015

##################
# CREATE DATASET #
##################

#Source Population
#Age 18-85
#No diagnosis of ESRD in 2013
#No record of current pregnancy in 2013

#Eligibility variables
#W2 = W++, racial group, residence in state of Maryland, evidence of prior hypertension (at least 6 mo.), 2+ PCP visits in prior year, No ESRD, no pregnancy in prior year, visit in 2014 (FOR DESIGNS 1,2,3) or visit in 2014 and 2015 (for Design 4)
#W1 = W+, enrollment/non-enrollment in electronic portal   
#W0 = W~  visit in 2015 (FOR DESIGN 4 ONLY; NOT DEFINED FOR OTHER DESIGNS)

#Eligibility
#Q2 = Q++ = I(W2==w2') as in text 
#Q1 = Q+  = I(W1==w2') as in text 
#Q0 = Q~  = I(W0==w2') as in text (ONLY DEFINED FOR DESIGN 4)

#Allowables
#A1, age
#A2, sex
#WEEK, calendar time in weeks

#Non-allwables
#N1 type of insurance
#N2 CDC index of social vulnerability at county-level (categorized)
#N3 any comorbid chronic condition

#Standard Population
#TT Black persons who are eligible

#Time scale for trial enrollment
#calendar time in weeks

## DATA FOR OVERALL DISPARITY ##

df_use[,"PID"]<-df_use$PATID
df_use[,"EID"]<-df_use$ENCOUNTERID_INDX
df_use[,"INDEX"]<-df_use$MEASURE_DATE_INDX
df_use[,"Y"]  <-ifelse(df_use$SBP_INDX>=140 | df_use$DBP_INDX>=90,1,0)
df_use$W2_VISIT <-ifelse(df_use$PREVISIT>=2,1,0)
df_use$W2_ESRD<-ifelse((!is.na(df_use$DX_DATE_DIAG_ESRD) & df_use$MEASURE_DATE_INDX>df_use$DX_DATE_DIAG_ESRD) | (!is.na(df_use$START_DATE_DIAG_ESRD) & df_use$MEASURE_DATE_INDX>df_use$START_DATE_DIAG_ESRD),1,0)
df_use$W2_PREG<-ifelse((!is.na(df_use$DX_DATE_DIAG_PREGNANCY) & df_use$MEASURE_DATE_INDX-df_use$DX_DATE_DIAG_PREGNANCY<=360) | (!is.na(df_use$START_DATE_DIAG_PREGNANCY) & df_use$MEASURE_DATE_INDX-df_use$START_DATE_DIAG_PREGNANCY<=360),1,0)
df_use$W2_STATE<-factor(df_use$STATE)
df_use$W2_RACE<-ifelse(df_use$RACEG==3,2,ifelse(df_use$RACEG==2,1,0)) #2=Other, #1=NH Black, #0=NH White
df_use$W2_HTN<-ifelse((!is.na(df_use$DX_DATE_DIAG_HYPERTENSION) & df_use$MEASURE_DATE_INDX-df_use$DX_DATE_DIAG_HYPERTENSION>=180) | (!is.na(df_use$START_DATE_DIAG_HYPERTENSION) & df_use$MEASURE_DATE_INDX-df_use$START_DATE_DIAG_HYPERTENSION>=180),1,0)
df_use$W2_20142015<-df_use$HAD_VISIT_IN_2014_AND_2015 #for use in designs 1, 2, and 3
df_use$W2_2014<-df_use$HAD_VISIT_IN_2014 #for use in design 4
df_use$W0_2015<-df_use$HAD_VISIT_IN_2015 #for use in design 4
df_use$W1_MYCHART<-factor(df_use$MYCHART_INDX)
df_use[,"R"] <-df_use$W2_RACE
df_use[,"TT"]<-ifelse(df_use$R==1,1,0)
df_use[,"A1"]<-df_use$AGE
df_use[,"A2"]<-factor(df_use$SEX)
df_use[,"N1"]<-factor(ifelse(df_use$PAYER_PRIMARY_INDX==1,"MEDICARE",
                             ifelse(df_use$PAYER_PRIMARY_INDX==2,"MEDICAID",
                                    ifelse(df_use$PAYER_PRIMARY_INDX==3,"OTHER GOVT","PRIVATE/OTHER"))))
df_use[,"N2"]<-df_use$RPL_THEMES
df_use[,"N2"]<-factor(cut(x=df_use$N2,breaks=c(0,.1,.2,.3,.4,.6,.8,1),include.lowest=TRUE))
df_use[,"N3"]<-df_use$ANY_COMORBIDITY
df_use[,"N2_MISS"]<-is.na(df_use$N2)
df_use[,"N2"]<-ifelse(df_use$N2_MISS,0,df_use$N2)
df_use[,"YEAR"]<-factor(year(df_use$MEASURE_DATE_INDX))
df_use[,"WEEK"]<-I(df_use$YEAR=="2014")*week(df_use$MEASURE_DATE_INDX)+I(df_use$YEAR=="2015")*(52+week(df_use$MEASURE_DATE_INDX))

eligibility_vars<-c("W2_STATE","W2_RACE","W2_HTN","W1_MYCHART","W2_20142015","W2_2014","W0_2015","W2_PREG","W2_VISIT","W2_ESRD","YEAR")
df_var<-df_use[,c("PID","EID","INDEX","SITE_ID_INDX","Y","R","TT","A1","A2","N1","N2","N3","N2_MISS","WEEK","SBP_INDX","DBP_INDX",eligibility_vars)]

############################################
## FUNCTIONS TO PREPARE AND DESCRIBE DATA ##
############################################

select_one_per_week <- function (x) {
  df<-x
  dx <- df %>%
    arrange(PID,WEEK,EID) %>%
    group_by(PID,WEEK) %>%
    filter(row_number()==1L) %>%
    ungroup()    
    print(nrow(x))
    print(nrow(dx))
  return(dx)
  }

remove_bad_weeks <- function (x) {
  df <- x
  weeks<-table(df$WEEK,df$R)
  representation<-apply(weeks,1,function(x) ifelse(x[1]>9 & x[2]>9,1,0))
  good_weeks <- as.numeric(names(representation)[which(representation==1)])
  df_out<-df[which(df$WEEK %in% good_weeks),] 
  return(df_out)
  }

summarize_records <- function (x) {
  df <- x
  n_EID <- length(unique(df$EID))
  n_PID <- length(unique(df$PID))
  df_PID <- split(df,df$PID)
  n_PID_EID_list <- lapply(df_PID,nrow)
  n_EID_PID <- fivenum(do.call("c",n_PID_EID_list)) 
  output_list <- list(n_EID,n_PID,n_EID_PID)
  names(output_list) <- c("n visits","n persons","n visits per person")
  return(output_list)
  }

###############################
## FUNCTIONS TO ANALYZE DATA ##
###############################

wgt1 <- function (df,outcome,check=FALSE) {
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  #Step 1: fit models
  fit_r  <- glm(R~1,binomial("logit"),df[which(df$Q==1),])
  fit_rx  <- glm(R~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q==1),]) #fit_rx <- glm(R~A+k,binomial("logit"),df[which(df$Q==1),])
  fit_t  <- glm(TT~1,binomial("logit"),df[which(df$Q==1),])
  fit_tx <- glm(TT~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q==1),])  #fit_tx <- glm(TT~A+k,binomial("logit"),df[which(df$Q==1),])  
  #Step 2: obtain predicted values
  df[,"p_r"]  <- predict(fit_r,df,"response")
  df[,"p_rx"] <- predict(fit_rx,df,"response")
  df[,"p_t"]  <- predict(fit_t,df,"response")
  df[,"p_tx"] <- predict(fit_tx,df,"response")
  #Step 3: calculate predicted values for R=r
  df[,"p_r"]  <- df$R*df$p_r+(1-df$R)*(1-df$p_r)
  df[,"p_rx"] <- df$R*df$p_rx+(1-df$R)*(1-df$p_rx)
  #Step 4: create weights
  df[,"W"] <- df$Q*(df$p_r*df$p_tx)/(df$p_t*df$p_rx)
  check_weight <- function (x) c(mean(x),min(x),max(x))
  if (isTRUE(check)) print(check_weight(df$W[which(df$Q==1)]))
  #step 5: estimate disparity by contrasting weighted averages among Q=1
  dfQR1 <- df[which(df$Q==1 & df$R==1),]
  dfQR0 <- df[which(df$Q==1 & df$R==0),]
  EY1 <- weighted.mean(dfQR1$Y,dfQR1$W)
  EY0 <- weighted.mean(dfQR0$Y,dfQR0$W)
  RD <- EY1-EY0
  RR <- EY1/EY0
  output<-c(EY1,EY0,RD,RR)
  names(output)<-c("EY1","EY0","RD","RR")
  return(output)
  }

gcm1 <- function (df,outcome) {
  R_mat<-c(1,0)
  EP1_list<-list()
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  for (iter in 1:2) {
    rval<-R_mat[iter]
    #Step 1: fit model for Y among R=r
    fit_step1<-glm(Y~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q==1 & df$R==rval),])
    #Step 2: obtain predicted values
    df[,"P1"] <- predict(fit_step1,df,"response")
    #Step 3: obtain average in standard population TT=1 & Q==1
    dfTT <- df[which(df$Q==1 & df$TT==1),]
    EP1_list[[iter]]<-mean(dfTT$P1)
  }      
  EY1 <- EP1_list[[1]]
  EY0 <- EP1_list[[2]]
  RD <- EY1-EY0
  RR <- EY1/EY0
  output<-c(EY1,EY0,RD,RR)
  names(output)<-c("EY1","EY0","RD","RR")
  return(output)
  }

wgt2 <- function (df,outcome,phi=NULL,theta=NULL,density_ratio=NULL,check=TRUE) {
  #brings in phi and theta for use with Design 4 by weighting function wgt4
  if (!is.null(phi)) {
    df$phi <- phi
    } else {
      df$phi<-1
      }
  if (!is.null(theta)) {
    df$theta <- theta
    } else {
      df$theta<-1
      }
  if (!is.null(density_ratio)) {
    df$density_ratio <- density_ratio
    } else {
      df$density_ratio<-1
      }
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  #Step 1: fit models
  R_mat<-c(1,0)
  fit_q1_list  <- list()
  fit_q1x_list <- list()
  for (iter in 1:2) {
    rval<-R_mat[iter]
    fit_q1_list[[iter]]  <- glm(Q1~1,binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
    fit_q1x_list[[iter]] <- glm(Q1~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
    }
  fit_r   <- glm(R~1,binomial("logit"),df[which(df$Q2==1),])
  fit_rx  <- glm(R~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q2==1),])
  fit_t   <- glm(TT~1,binomial("logit"),df[which(df$Q2==1),])
  fit_tx  <- glm(TT~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q2==1),])  
  #Step 2: obtain predicted values
  df[,"p_q1_R1"]  <- predict(fit_q1_list[[1]],df,"response")
  df[,"p_q1_R0"]  <- predict(fit_q1_list[[2]],df,"response")
  df[,"p_q1x_R1"] <- predict(fit_q1x_list[[1]],df,"response")
  df[,"p_q1x_R0"] <- predict(fit_q1x_list[[2]],df,"response")
  df[,"p_r"]   <- predict(fit_r,df,"response")
  df[,"p_rx"]  <- predict(fit_rx,df,"response")
  df[,"p_t"]   <- predict(fit_t,df,"response")
  df[,"p_tx"]  <- predict(fit_tx,df,"response")
  #Step 3: calculate predicted values for R=r
  df[,"p_q1"] <- df$R*df$p_q1_R1+(1-df$R)*df$p_q1_R0
  df[,"p_q1x"] <- df$R*df$p_q1x_R1+(1-df$R)*df$p_q1x_R0
  df[,"p_r"]  <- df$R*df$p_r+(1-df$R)*(1-df$p_r)
  df[,"p_rx"] <- df$R*df$p_rx+(1-df$R)*(1-df$p_rx)
  #Step 4: create weights
  df[,"W"] <- df$Q*(df$p_q1/df$p_q1x)*(df$p_r*df$p_tx)/(df$p_t*df$p_rx)*df$phi*df$theta*df$density_ratio
  check_weight <- function (x) c(mean(x),min(x),max(x))
  if (isTRUE(check)) print(check_weight(df$W[which(df$Q==1)]))
  #step 5: estimate disparity by contrasting weighted averages among Q=1
  dfQR1 <- df[which(df$Q==1 & df$R==1),]
  dfQR0 <- df[which(df$Q==1 & df$R==0),]
  EY1 <- weighted.mean(dfQR1$Y,dfQR1$W)
  EY0 <- weighted.mean(dfQR0$Y,dfQR0$W)
  RD <- EY1-EY0
  RR <- EY1/EY0
  output<-c(EY1,EY0,RD,RR)
  names(output)<-c("EY1","EY0","RD","RR")
  return(output)
  }

gcm2 <- function (df,outcome,phi=NULL,theta=NULL) {
  #brings in phi and theta for use with Design 4 by G-computation function gcm4
  if (!is.null(phi)) {
    df$phi <- phi
    } else {
      df$phi<-1
      }
  if (!is.null(theta)) {
    df$theta <- theta
    } else {
      df$theta<-1
      }
  R_mat<-c(1,0)
  EP2_list<-list()
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  for (iter in 1:2) {
    rval<-R_mat[iter]
    #Step 1: fit model for Y among R=r
    fit_step1<-glm(Y~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,binomial("logit"),df[which(df$Q==1 & df$R==rval),])
    #Step 2: obtain predicted values P1
    df[,"P1"] <- predict(fit_step1,df,"response")
    #Step 3: fit model for P1 among R=r
    fit_step3<-glm(P1~ns(A1,3)+A2+ns(WEEK,3),quasibinomial("logit"),df[which(df$Q2==1 & df$R==rval),],df$phi[which(df$Q2==1 & df$R==rval)])
    #Step 4: obtain predicted values P2
    df[,"P2"] <- predict(fit_step3,df,"response")
    #Step 5: obtain average of P2 in standard population TT=1 & Q2==1
    dfTT <- df[which(df$Q2==1 & df$TT==1),]
    EP2_list[[iter]]<-weighted.mean(dfTT$P2,dfTT$theta)
    }      
  EY1 <- EP2_list[[1]]
  EY0 <- EP2_list[[2]]
  RD <- EY1-EY0
  RR <- EY1/EY0
  output<-c(EY1,EY0,RD,RR)
  names(output)<-c("EY1","EY0","RD","RR")
  return(output)
  }

wgt3 <- function (df,outcome,check=TRUE) {
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  #Step 1: fit models
  R_mat<-c(1,0)
  fit_q1_list  <- list()
  fit_q1x_list <- list()
  for (iter in 1:2) {
    rval<-R_mat[iter]
    fit_q1_list[[iter]]  <- glm(Q1~1,binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
    fit_q1x_list[[iter]] <- glm(Q1~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
    }
  fit_r   <- glm(R~1,binomial("logit"),df[which(df$Q2==1 & df$Q1==0),])
  fit_rx  <- glm(R~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q2==1 & df$Q1==0),])
  fit_t   <- glm(TT~1,binomial("logit"),df[which(df$Q2==1 & df$Q1==0),])
  fit_tx  <- glm(TT~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q2==1 & df$Q1==0),])  
  #Step 2: obtain predicted values
  df[,"p_q1_R1"]  <- predict(fit_q1_list[[1]],df,"response")
  df[,"p_q1_R0"]  <- predict(fit_q1_list[[2]],df,"response")
  df[,"p_q1x_R1"] <- predict(fit_q1x_list[[1]],df,"response")
  df[,"p_q1x_R0"] <- predict(fit_q1x_list[[2]],df,"response")
  df[,"p_r"]   <- predict(fit_r,df,"response")
  df[,"p_rx"]  <- predict(fit_rx,df,"response")
  df[,"p_t"]   <- predict(fit_t,df,"response")
  df[,"p_tx"]  <- predict(fit_tx,df,"response")
  #Step 3: calculate predicted values for R=r
  df[,"p_q1"] <- df$R*df$p_q1_R1+(1-df$R)*df$p_q1_R0
  df[,"p_q1x"] <- df$R*df$p_q1x_R1+(1-df$R)*df$p_q1x_R0
  df[,"p_r"]  <- df$R*df$p_r+(1-df$R)*(1-df$p_r)
  df[,"p_rx"] <- df$R*df$p_rx+(1-df$R)*(1-df$p_rx)
  #Step 4: create weights
  df[,"W"] <- df$Q*((1-df$p_q1x)/df$p_q1x)*(df$p_q1/(1-df$p_q1))*(df$p_r*df$p_tx)/(df$p_t*df$p_rx)
  check_weight <- function (x) c(mean(x),min(x),max(x))
  if (isTRUE(check)) print(check_weight(df$W[which(df$Q==1)]))
  #step 5: estimate disparity by contrasting weighted averages among Q=1
  dfQR1 <- df[which(df$Q==1 & df$R==1),]
  dfQR0 <- df[which(df$Q==1 & df$R==0),]
  EY1 <- weighted.mean(dfQR1$Y,dfQR1$W)
  EY0 <- weighted.mean(dfQR0$Y,dfQR0$W)
  RD <- EY1-EY0
  RR <- EY1/EY0
  output<-c(EY1,EY0,RD,RR)
  names(output)<-c("EY1","EY0","RD","RR")
  return(output)
  }

gcm3 <- function (df,outcome) {
  R_mat<-c(1,0)
  EP2_list<-list()
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  for (iter in 1:2) {
    rval<-R_mat[iter]
    #Step 1: fit model for Y among R=r
    fit_step1<-glm(Y~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,binomial("logit"),df[which(df$Q==1 & df$R==rval),])
    #Step 2: obtain predicted values P1
    df[,"P1"] <- predict(fit_step1,df,"response")
    #Step 3: fit model for P1 among R=r
    fit_step3<-glm(P1~ns(A1,3)+A2+ns(WEEK,3),quasibinomial("logit"),df[which(df$Q2==1 & df$Q1==0 & df$R==rval),])
    #Step 4: obtain predicted values P2
    df[,"P2"] <- predict(fit_step3,df,"response")
    #Step 5: obtain average of P2 in standard population TT=1 & Q2==1 & Q1==0
    dfTT <- df[which(df$Q2==1 & df$Q1==0 & df$TT==1),]
    EP2_list[[iter]]<-mean(dfTT$P2)
  }      
  EY1 <- EP2_list[[1]]
  EY0 <- EP2_list[[2]]
  RD <- EY1-EY0
  RR <- EY1/EY0
  output<-c(EY1,EY0,RD,RR)
  names(output)<-c("EY1","EY0","RD","RR")
  return(output)
  }

q1_distribution <- function (df,subdesign) {
  if (subdesign=="a") {
    fit_a <- glm(Q1~ns(WEEK,3),binomial("logit"),df[which(df$Q2==1),])
    pq1 <- predict(fit_a,df,"response")
  } else if (subdesign=="b") {
    fit_b_R1 <- glm(Q1~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q2==1 & df$R==1),])
    fit_b_R0 <- glm(Q1~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q2==1 & df$R==0),])
    pq1_R1 <- predict(fit_b_R1,df,"response")
    pq1_R0 <- predict(fit_b_R0,df,"response")
    pq1 <- df$R*pq1_R1 + (1-df$R)*pq1_R0
  } else if (subdesign=="c") {
    fit_c <- glm(Q1~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,binomial("logit"),df[which(df$Q2==1 & df$TT==1),])
    pq1 <- predict(fit_c,df,"response")
  }
  return(pq1)
  }

estimate_phi <- function (df,subdesign) {
  df$pq1<-q1_distribution(df,subdesign)
  R_mat<-c(1,0)
  pq0_i_list<-list()
  pq0_ii_list<-list()
  for (iter in 1:2) {
    rval<-R_mat[iter]
    #Step 1: fit model for Q~ among R=r
    fit_step1<-glm(Q0~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,binomial("logit"),df[which(df$Q2==1 & df$Q1==1 & df$R==rval),])
    #Step 2: obtain predicted values pq0_i
    pq0_i_list[[iter]] <- predict(fit_step1,df,"response")
    df[,"pq0_i_temp"]<-pq0_i_list[[iter]]
    #Step 3: fit model for pq1*pq0_i among R=r
    fit_step3<-glm(pq1*pq0_i_temp~ns(A1,3)+A2+ns(WEEK,3),quasibinomial("logit"),df[which(df$Q2==1 & df$R==rval),])
    #Step 4: obtain predicted values pq0_ii
    pq0_ii_list[[iter]] <- predict(fit_step3,df,"response")
    }        
  df[,"pq0_i"]<-df$R*pq0_i_list[[1]]+(1-df$R)*pq0_i_list[[2]]
  df[,"pq0_ii"]<-df$R*pq0_ii_list[[1]]+(1-df$R)*pq0_ii_list[[2]]
  #step 5: calculate phi
  output<-df$pq1*df$pq0_i/df$pq0_ii
  return(output)
  }

estimate_theta <- function (df,subdesign) {
  df$pq1<-q1_distribution(df,subdesign)
  #Step 1: fit model for pq1 among TT=1
  fit_step1<-glm(pq1~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,quasibinomial("logit"),df[which(df$Q2==1 & df$TT==1),])
  #Step 2: obtain predicted values Epq1 
  df[,"Epq1"] <- predict(fit_step1,df,"response")
  #Step 3: fit model for Q~ among TT=1
  fit_step3<-glm(Q0~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,binomial("logit"),df[which(df$Q2==1 & df$Q1==1 & df$TT==1),])
  #Step 4: obtain predicted values pq0_i
  df[,"pq0_i"] <- predict(fit_step3,df,"response")
  #Step 5: fit model for Epq1*pq0 among TT=1
  fit_step5<-glm(Epq1*pq0_i~ns(A1,3)+A2+ns(WEEK,3),quasibinomial("logit"),df[which(df$Q2==1 & df$TT==1),])
  #step 6: obtain predicted values
  df[,"pq0_ii"] <- predict(fit_step5,df,"response")
  #Step 7: fit model for Epq0*pq0_i among TT=1
  fit_step7<-glm(Epq1*pq0_i~1,quasibinomial("logit"),df[which(df$Q2==1 & df$TT==1),])
  #step 8: obtain predicted values
  df[,"pq0_iii"] <- predict(fit_step7,df,"response")
  #step 9: calculate theta
  output<-df$pq0_ii/df$pq0_iii
  return(output)
  }

wgt4 <- function (df,outcome,subdesign,check=FALSE) {
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  #step 1: fit models
  R_mat<-c(1,0)
  fit_q0_list  <- list()
  fit_q0x_list <- list()
  for (iter in 1:2) {
    rval<-R_mat[iter]
    fit_q0_list[[iter]]  <- glm(Q0~1,binomial("logit"),df[which(df$Q2==1 & df$Q1==1 & df$R==rval),])
    fit_q0x_list[[iter]] <- glm(Q0~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,binomial("logit"),df[which(df$Q2==1 & df$Q1==1 & df$R==rval),])
    }
  #step 2: obtain predicted values  
  df[,"p_q0_R1"]  <- predict(fit_q0_list[[1]],df,"response")
  df[,"p_q0_R0"]  <- predict(fit_q0_list[[2]],df,"response")
  df[,"p_q0x_R1"] <- predict(fit_q0x_list[[1]],df,"response")
  df[,"p_q0x_R0"] <- predict(fit_q0x_list[[2]],df,"response")
  #Step 3: calculate predicted values for R=r
  df[,"p_q0"] <- df$R*df$p_q0_R1+(1-df$R)*df$p_q0_R0
  df[,"p_q0x"] <- df$R*df$p_q0x_R1+(1-df$R)*df$p_q0x_R0
  #step 3: calculate weight for Q~
  df[,"density_ratio"] <- df$p_q0/df$p_q0x
  #step 4: estiamte phi and theta
  df$phi <- estimate_phi(df,subdesign)
  df$theta <- estimate_theta(df,subdesign)
  #step 5: estimate disparity
  output <- wgt2(df,"Y",phi=df$phi,theta=df$theta,density_ratio=df$density_ratio,check=check)
  return(output)
  }

gcm4 <- function (df,outcome,subdesign) {
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  #step 1: estiamte phi and theta
  df$phi <- estimate_phi(df,subdesign)
  df$theta <- estimate_theta(df,subdesign)
  #step 2: estimate disparity
  output <- gcm2(df,"Y",phi=df$phi,theta=df$theta)
  return(output)
  }

#########################
## DESIGN 4 SIMPLIFIED ##
#########################

estimate_phi_s <- function (df,subdesign) {
  if (subdesign=="b") output<-rep(1,nrow(df))
  else {
    df$pq1<-q1_distribution(df,subdesign)
    R_mat<-c(1,0)
    Epq1_list<-list()
    for (iter in 1:2) {
      rval<-R_mat[iter]
      #Step 1: fit model for pq1 among R=r
      fit_step1<-glm(pq1~ns(A1,3)+A2+ns(WEEK,3),quasibinomial("logit"),df[which(df$Q2==1 & df$R==rval),])
      #Step 2: obtain predicted values Eqp1
      Epq1_list[[iter]] <- predict(fit_step1,df,"response")
      }      
    df$Epq1<-df$R*Epq1_list[[1]]+(1-df$R)*Epq1_list[[2]]
    #step 5: calculate phi
    output<-df$pq1/df$Epq1
    }
  return(output)
  }

wgt4_s <- function (df,outcome,subdesign) {
  if (subdesign=="a") output<-wgt2(df,outcome)
  else {  
    df$phi <- estimate_phi_s(df,subdesign)
    #Step 0: pull outcome
    df[,"Y"] <- unlist(df[,outcome])
    #Step 1: fit models
    R_mat<-c(1,0)
    fit_q1_list  <- list()
    fit_q1x_list <- list()
    for (iter in 1:2) {
      rval<-R_mat[iter]
      fit_q1_list[[iter]]  <- glm(Q1~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
      fit_q1x_list[[iter]] <- glm(Q1~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
      }
    fit_r   <- glm(R~1,binomial("logit"),df[which(df$Q==1),])
    fit_rx  <- glm(R~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q==1),])
    fit_t   <- glm(TT~1,binomial("logit"),df[which(df$Q==1),])
    fit_tx  <- glm(TT~ns(A1,3)+A2+ns(WEEK,3),binomial("logit"),df[which(df$Q==1),])  
    #Step 2: obtain predicted values
    df[,"p_q1_R1"]  <- predict(fit_q1_list[[1]],df,"response")
    df[,"p_q1_R0"]  <- predict(fit_q1_list[[2]],df,"response")
    df[,"p_q1x_R1"] <- predict(fit_q1x_list[[1]],df,"response")
    df[,"p_q1x_R0"] <- predict(fit_q1x_list[[2]],df,"response")
    df[,"p_r"]   <- predict(fit_r,df,"response")
    df[,"p_rx"]  <- predict(fit_rx,df,"response")
    df[,"p_t"]   <- predict(fit_t,df,"response")
    df[,"p_tx"]  <- predict(fit_tx,df,"response")
    #Step 3: calculate predicted values for R=r
    df[,"p_q1"] <- df$R*df$p_q1_R1+(1-df$R)*df$p_q1_R0
    df[,"p_q1x"] <- df$R*df$p_q1x_R1+(1-df$R)*df$p_q1x_R0
    df[,"p_r"]  <- df$R*df$p_r+(1-df$R)*(1-df$p_r)
    df[,"p_rx"] <- df$R*df$p_rx+(1-df$R)*(1-df$p_rx)
    #Step 4: create weights
    df[,"W"] <- df$Q*(df$p_q1/df$p_q1x)*(df$p_r*df$p_tx)/(df$p_t*df$p_rx)*df$phi
    #step 5: estimate disparity by contrasting weighted averages among Q=1
    dfQR1 <- df[which(df$Q==1 & df$R==1),]
    dfQR0 <- df[which(df$Q==1 & df$R==0),]
    EY1 <- weighted.mean(dfQR1$Y,dfQR1$W)
    EY0 <- weighted.mean(dfQR0$Y,dfQR0$W)
    RD <- EY1-EY0
    RR <- EY1/EY0
    output<-c(EY1,EY0,RD,RR)
    names(output)<-c("EY1","EY0","RD","RR")
    }
  return(output)
  }

gcm4_s <- function (df,outcome,subdesign) {
  if (subdesign=="a") output<-gcm2(df,outcome)
  else {
    df$phi <- estimate_phi_s(df,subdesign)
    R_mat<-c(1,0)
    EP2_list<-list()
    #Step 0: pull outcome
    df[,"Y"] <- unlist(df[,outcome])
    for (iter in 1:2) {
      rval<-R_mat[iter]
      #Step 1: fit model for Y among R=r
      fit_step1<-glm(Y~ns(A1,3)+A2+ns(WEEK,3)+N1+N2+N3,binomial("logit"),df[which(df$Q==1 & df$R==rval),])
      #Step 2: obtain predicted values P1
      df[,"P1"] <- predict(fit_step1,df,"response")
      #Step 3: fit model for P1 among R=r
      fit_step3<-glm(P1~ns(A1,3)+A2+ns(WEEK,3),quasibinomial("logit"),df[which(df$Q2==1 & df$R==rval),],df$phi[which(df$Q2==1 & df$R==rval)])
      #Step 4: obtain predicted values P2
      df[,"P2"] <- predict(fit_step3,df,"response")
      #Step 5; obtain average of P2 in standard population TT=1 & Q2==1
      dfTT <- df[which(df$Q==1 & df$TT==1),]
      EP2_list[[iter]]<-mean(dfTT$P2)
      }      
    EY1 <- EP2_list[[1]]
    EY0 <- EP2_list[[2]]
    RD <- EY1-EY0
    RR <- EY1/EY0
    output<-c(EY1,EY0,RD,RR)
    names(output)<-c("EY1","EY0","RD","RR")
    }
  return(output)
  }

######################################################
##### FUNCTION TO MAKE BOOTSTRAP SAMPLING MATRIX #####
######################################################

make_boot_id_matrix <-function (df,n_boot_samples,boot_type) { 
  ctable<-table(df$PID,df$R) #persons (row) by race (column)
  cnames<-rownames(ctable)
  cB<-cnames[which(ctable[,2]>0)] #persons with R=1
  cW<-cnames[which(ctable[,1]>0)] #persons with R=0
  length(c(cB,cW))
  c_list<-list(cB,cW)
  m_list<-list()
  J<-n_boot_samples
  c_length<-lapply(c_list,length)
  c_list<-c_list[which(c_length>0)]
  if (boot_type=="balanced") {
    for (iter in 1:length(c_list)) { #For each type, create balanced matrix of cluster ids for bootstrap samples
      cXX<-c_list[[iter]]
      vXX<-rep(cXX,J) #vector
      nXX<-runif(length(vXX),0,1) #random number
      mXX<-data.frame(vXX,nXX) #dataframe vector & random number
      sXX<-mXX[order(nXX),] #sorted dataframe by random number
      m_list[[iter]]<-matrix(sXX$vXX,length(cXX),J)  
    }
  } else if (boot_type=="sampled") {
    for (iter in 1:length(c_list)) { #for each type, create matrix of cluster ids via sampling with replacement
      cXX<-c_list[[iter]]
      mXX<-matrix(rep(cXX,J),length(cXX),J)  
      sXX<-apply(mXX,2,function (x) sample(x,length(x),replace=TRUE)) #randomly sample with replacement
      m_list[[iter]]<-sXX
    }
  }
  boot_clusters<-do.call("rbind",m_list) #combine the matrices for each of the two types into a single matrix 
  return(boot_clusters)
  }

get_boot_sample<-function (df,boot_iteration,boot_id_matrix) {
  df_list<-split(df[,c("EID","PID")],df[,"PID"])
  boot_list<-df_list[boot_id_matrix[,boot_iteration]]
  boot_ids<-do.call("rbind",boot_list)
  boot_ids_vector<-boot_ids$EID
  boot_sample<-df[match(boot_ids_vector,df$EID),]
  return(boot_sample)
  }

####################################################
##### FUNCTION TO CARRY OUT BOOTSTRAP ANALYSIS #####
####################################################

bootstrap<-function(df,fxn,outcome,subdesign=NULL,n_boot_samples,boot_type="sampled"){#,round_digit=2) {
  fxn_wrapper<-function(df,fxn,outcome,subdesign) {
  if (is.null(subdesign)) {
    estimate<-fxn(df,outcome)
    } else {
      estimate<-fxn(df,outcome,subdesign)
      }
    return(estimate)
    }  
  point_estimate<-fxn_wrapper(df,fxn,outcome,subdesign)
  boot_id_matrix<-make_boot_id_matrix(df,n_boot_samples,boot_type=boot_type)
  result_list<-list()
  for (ith_sample in 1:n_boot_samples) {
    db<-get_boot_sample(df,ith_sample,boot_id_matrix)
    result<-fxn_wrapper(db,fxn,outcome,subdesign)
    result_list[[ith_sample]]<-result
  }  
  result_matrix<-do.call("rbind",result_list)
  confint_lower<-apply(result_matrix,2,quantile,probs=.025)
  confint_upper<-apply(result_matrix,2,quantile,probs=.975)
  std_error<-apply(result_matrix,2,sd)
  if (ncol(result_matrix)>1) {
    confint_lower[4]<-exp(quantile(log(result_matrix[,4]),probs=.025))
    confint_upper[4]<-exp(quantile(log(result_matrix[,4]),probs=.975))
    std_error[4]<-exp(sd(log(result_matrix[,4])))
  }
  fmt_point_estimate<-point_estimate #round(point_estimate,round_digit)
  fmt_confint_lower <-confint_lower #round(confint_lower,round_digit)
  fmt_confint_upper <-confint_upper #round(confint_upper,round_digit)
  fmt_std_error<-std_error #round(std_error,round_digit)
  output<-data.frame(t(t(names(point_estimate))),t(t(fmt_point_estimate)),t(t(fmt_confint_lower)),t(t(fmt_confint_upper)),t(t(fmt_std_error)))
  colnames(output)<-c("par","est","l95","u95","se")
  return(output)
  }

#######################################################
## SET ELIGIBILITY INDICATORS FOR DESIGNS 1,2, AND 3 ##
#######################################################

df_src<-df_var

df_src[,"Q2"]<-ifelse(df_src$W2_VISIT==1 & df_src$W2_ESRD!=1 & df_src$W2_PREG!=1 & df_src$W2_HTN==1 & df_src$W2_RACE!=2 & df_src$W2_STATE=="MD" & df_src$W2_20142015==1,1,0)
df_src[,"Q1"]<-ifelse(df_src$W1_MYCHART==1,1,0)
df_src[,"Q0"]<-1
df_src[,"Q"] <-df_src$Q2*df_src$Q1*df_src$Q0

#############################################
## SET ELIGIBILITY INDICATORS FOR DESIGN 4 ##
#############################################

df_src4<-df_var
df_src4[,"Q2"]<-ifelse(df_src4$W2_VISIT==1 & df_src4$W2_ESRD!=1 & df_src4$W2_PREG!=1 & df_src4$W2_HTN==1 & df_src4$W2_RACE!=2 & df_src4$W2_STATE=="MD" & df_src4$W2_2014==1,1,0)
df_src4[,"Q1"]<-ifelse(df_src4$W1_MYCHART==1,1,0)
df_src4[,"Q0"]<-ifelse(df_src4$W0_2015==1,1,0)
df_src4[,"Q"] <-df_src4$Q2*df_src4$Q1*df_src4$Q0

######################
## CREATE FLOWCHART ##
######################

##### DESIGNS 1, 2, AND 3 #####

## BOX 1 ##

df_src_race<-split(df_src,df_src$R)
lapply(df_src_race,summarize_records)

## BOX 2 ##

df_src_Q2 <- df_src[which(df_src$Q2==1),]
df_src_race_Q2<-split(df_src_Q2,df_src_Q2$R)
lapply(df_src_race_Q2,summarize_records)

## BOX 3 ##

df_src_Q <- df_src[which(df_src$Q==1),]
df_src_race_Q<-split(df_src_Q,df_src_Q$R)
lapply(df_src_race_Q,summarize_records)

## BOX 4 ##

df_src_Q <- df_src[which(df_src$Q==1),]
df_src_Q <- select_one_per_week(df_src_Q)
df_src_Q <- remove_bad_weeks(df_src_Q)
df_src_race_Q<-split(df_src_Q,df_src_Q$R)
lapply(df_src_race_Q,summarize_records)

##### DESIGN 4 #####

## BOX 1 ##

df_src4_race<-split(df_src4,df_src4$R)
lapply(df_src4_race,summarize_records)

## BOX 2 ##

df_src4_Q2 <- df_src4[which(df_src4$Q2==1),]
df_src4_race_Q2<-split(df_src4_Q2,df_src4_Q2$R)
lapply(df_src4_race_Q2,summarize_records)

## BOX 3 ##

df_src4_Q2Q1 <- df_src4[which(df_src4$Q2==1 & df_src4$Q1==1),]
df_src4_race_Q2Q1<-split(df_src4_Q2Q1,df_src4_Q2Q1$R)
lapply(df_src4_race_Q2Q1,summarize_records)

## BOX 4 ##

df_src4_Q <- df_src4[which(df_src4$Q==1),]
df_src4_race_Q<-split(df_src4_Q,df_src4_Q$R)
lapply(df_src4_race_Q,summarize_records)

## BOX 5 ##

df_src4_Q <- df_src4[which(df_src4$Q==1),]
df_src4_Q <- select_one_per_week(df_src4_Q)
df_src4_Q <- remove_bad_weeks(df_src4_Q)
df_src4_race_Q<-split(df_src4_Q,df_src4_Q$R)
lapply(df_src4_race_Q,summarize_records)

## BOX 6 #

df_src4_Q_NM <- df_src4_Q[which(df_src4_Q$N2_MISS!=1),]
df_src4_race_Q_NM<-split(df_src4_Q_NM,df_src4_Q_NM$R)
lapply(df_src4_race_Q_NM,summarize_records)

#################
# DESCRIBE DATA # 
#################

## DESCRIPTIVE STATISTICS OVERALL COHORT##

# number of eligible visits
table(df_src$Q)
# number of eligible persons
length(unique(ifelse(df_src$Q==0,NA,df_src$PID)))
# number and percent of eligible visits by race
table(df_src$R,df_src$Q)
table(df_src$R,df_src$Q)/apply(table(df_src$R,df_src$Q),1,sum)
length(unique(ifelse(df_src$Q==0 | df_src$R!=0,NA,df_src$PID)))
length(unique(ifelse(df_src$Q==0 | df_src$R!=1,NA,df_src$PID)))
#mean, min, max number of visits per week
df_src %>% arrange(PID,WEEK) %>% group_by(PID) %>% 
  mutate(nth_visit=1,nvisits=cumsum(nth_visit)) %>% 
  ungroup() %>%
  summarise(min_visits=mean(nvisits),min(nvisits),max_visits=max(nvisits))
#statistics for age, sex, insurance, social vulnerability by race, and comorbidity
df_src %>% filter(Q==1) %>% group_by(R) %>% summarise(mean_age=mean(A1))
df_src %>% filter(Q==1) %>% group_by(R) %>% summarise(p_female=mean(I(A2=="F")))
df_src %>% filter(Q==1) %>% group_by(R) %>% summarise(p_medicaid=mean(I(N1=="MEDICAID")))
df_src %>% filter(Q==1) %>% group_by(R) %>% summarise(p_medicaid=mean(I(N2==7)))
df_src %>% filter(Q==1) %>% group_by(R) %>% summarise(p_highSVI=mean(N3))
#number of visits by electronic portal enrollment
table(df_src$Q2,df_src$Q1)
#percent enrolled by race
df_src %>% filter(Q2==1) %>% group_by(R) %>% summarise(p_enrolled=mean(Q1))
#percent enrolled by insurance
df_src %>% filter(Q2==1) %>% group_by(N1) %>% summarise(p_enrolled=mean(Q1))

##################
## RUN ANALYSES ##
##################

n_bootstraps<-1000

### SELECT APPROPRIATE RECORDS ###

df_SRC_1 <- select_one_per_week(remove_bad_weeks(df_src[which(df_src$Q==1),]))
df_SRC_0 <- select_one_per_week(remove_bad_weeks(df_src[which(df_src$Q==0),]))
df_SRC   <- rbind(df_SRC_1,df_SRC_0)

df_SRC4_1 <- select_one_per_week(remove_bad_weeks(df_src4[which(df_src4$Q==1),]))
df_SRC4_0 <- select_one_per_week(remove_bad_weeks(df_src4[which(df_src4$Q==0),]))
df_SRC4   <- rbind(df_SRC4_1,df_SRC4_0)

### CHECK WEIGHTS ###

wgt1(df_SRC,"Y",TRUE)
wgt2(df_SRC,"Y",TRUE)
wgt3(df_SRC,"Y",TRUE)
wgt4(df_SRC4,"Y","a",TRUE)
wgt4(df_SRC4,"Y","b",TRUE)
wgt4(df_SRC4,"Y","c",TRUE)

### DESIGN 1 G-COMPUTATION ###
out1_g<-bootstrap(df_SRC[which(df_SRC$Q==1),],gcm1,"Y",NULL,n_bootstraps,"balanced")
### DESIGN 1 WEIGHTING ###
out1_w<-bootstrap(df_SRC[which(df_SRC$Q==1),],wgt1,"Y",NULL,n_bootstraps,"balanced")

print("Design 1 G-computation")
print(out1_g)
print("Design 1 Weighting")
print(out1_w)

### DESIGN 2 G-COMPUTATION ###
out2_g<-bootstrap(df_SRC[which(df_SRC$Q2==1 & df_SRC$N2_MISS!=1),],gcm2,"Y",NULL,n_bootstraps,"balanced")
### DESIGN 2 WEIGHTING ###
out2_w<-bootstrap(df_SRC[which(df_SRC$Q2==1 & df_SRC$N2_MISS!=1),],wgt2,"Y",NULL,n_bootstraps,"balanced")

print("Design 2 G-computation")
print(out2_g)
print("Design 2 Weighting")
print(out2_w)

### DESIGN 3 G-COMPUTATION ###
out3_g<-bootstrap(df_SRC[which(df_SRC$Q2==1 & df_SRC$N2_MISS!=1),],gcm3,"Y",NULL,n_bootstraps,"balanced")
### DESIGN 3 WEIGHTING ###
out3_w<-bootstrap(df_SRC[which(df_SRC$Q2==1 & df_SRC$N2_MISS!=1),],wgt3,"Y",NULL,n_bootstraps,"balanced")

print("Design 3 G-computation")
print(out3_g)
print("Design 3 Weighting")
print(out3_w)

### DESIGN 4a G-COMPUTATION ###
out4a_g<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],gcm4,"Y","a",n_bootstraps,"balanced")
### DESIGN 4a WEIGHTING ###
out4a_w<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],wgt4,"Y","a",n_bootstraps,"balanced")

print("Design 4a G-computation")
print(out4a_g)
print("Design 4a Weighting")
print(out4a_w)

### DESIGN 4b G-COMPUTATION ###
out4b_g<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],gcm4,"Y","b",n_bootstraps,"balanced")
### DESIGN 4b WEIGHTING ###
out4b_w<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],wgt4,"Y","b",n_bootstraps,"balanced")

print("Design 4b G-computation")
print(out4b_g)
print("Design 4b Weighting")
print(out4b_w)

### DESIGN 4c G-COMPUTATION ###
out4c_g<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],gcm4,"Y","c",n_bootstraps,"balanced")
### DESIGN 4c WEIGHTING ###
out4c_w<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],wgt4,"Y","c",n_bootstraps,"balanced")

print("Design 4c G-computation")
print(out4c_g)
print("Design 4c Weighting")
print(out4c_w)

### DESIGN 4a G-COMPUTATION - SIMPLIFIED ###
out4as_g<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],gcm4_s,"Y","a",n_bootstraps,"balanced")
### DESIGN 4a WEIGHTING - SIMPLIFIED ###
out4as_w<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],wgt4_s,"Y","a",n_bootstraps,"balanced")

print("Design 4a G-computation - Simplified")
print(out4as_g)
print("Design 4a Weighting - Simplified")
print(out4as_w)

### DESIGN 4b G-COMPUTATION - SIMPLIFIED ###
out4bs_g<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],gcm4_s,"Y","b",n_bootstraps,"balanced")
### DESIGN 4b WEIGHTING - SIMPLIFIED ###
out4bs_w<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],wgt4_s,"Y","b",n_bootstraps,"balanced")

print("Design 4b G-computation - Simplified")
print(out4bs_g)
print("Design 4b Weighting - Simplified")
print(out4bs_w)

### DESIGN 4c G-COMPUTATION - SIMPLIFIED ###
out4cs_g<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],gcm4_s,"Y","c",n_bootstraps,"balanced")
### DESIGN 4c WEIGHTING - SIMPLIFIED ###
out4cs_w<-bootstrap(df_SRC4[which(df_SRC4$Q2==1 & df_SRC4$N2_MISS!=1),],wgt4_s,"Y","c",n_bootstraps,"balanced")

print("Design 4c G-computation - Simplified")
print(out4cs_g)
print("Design 4c Weighting - Simplified")
print(out4cs_w)

#######################
## SUMMARIZE RESULTS ##
#######################

output_list_g<-list(out1_g,out2_g,out3_g,out4a_g,out4b_g,out4c_g,out4as_g,out4bs_g,out4cs_g)
output_list_w<-list(out1_w,out2_w,out3_w,out4a_w,out4b_w,out4c_w,out4as_w,out4bs_w,out4cs_w)
fxn_round<-function (x) apply(x[,2:4],2,round,2)
r_output_list_g<-lapply(output_list_g,fxn_round)
r_output_list_w<-lapply(output_list_w,fxn_round)
fxn_format_row <- function (x) paste0(x[1]," (",x[2],", ",x[3],")")
fxn_table <- function (x) apply(x,1,fxn_format_row)
f_output_list_g<-lapply(r_output_list_g,fxn_table)
f_output_list_w<-lapply(r_output_list_w,fxn_table)
formatted_table_g<-do.call("rbind",f_output_list_g)
formatted_table_w<-do.call("rbind",f_output_list_w)
rownames(formatted_table_g) <- c("1","2","3","4a","4b","4c","4a-S","4b-S","4c-S")
rownames(formatted_table_w) <- c("1","2","3","4a","4b","4c","4a-S","4b-S","4c-S")
print("G-Computation Results")
print(formatted_table_g)
print("Weighting Results")
print(formatted_table_w)


##################
## SAVE RESULTS ##
##################

write.csv(formatted_table_w,paste0(output_path,"output_weighting_2024_12_06.csv"))
write.csv(formatted_table_g,paste0(output_path,"output_gcomputation_2024_12_06.csv"))

#############################
## CHECK EMPIRICAL RESULTS ##
#############################

## RESULTS REGARDLESS OF EPPP ENROLLMENT ##

df_SRC2<-df_SRC
df_SRC2[,"Q2"]<-ifelse(df_SRC2$W2_VISIT==1 & df_SRC2$W2_ESRD!=1 & df_SRC2$W2_PREG!=1 & df_SRC2$W2_HTN==1 & df_SRC2$W2_RACE!=2 & df_SRC2$W2_STATE=="MD" & df_SRC2$W2_2014==1,1,0)
df_SRC2[,"Q1"]<-1
df_SRC2[,"Q0"]<-ifelse(df_SRC2$W0_2015==1,1,0)
df_SRC2[,"Q"] <-df_SRC2$Q2*df_SRC2$Q1*df_SRC2$Q0
table(df_SRC$Q) #14037
table(df_SRC2$Q) #14037

bootstrap(df_SRC2[which(df_SRC$Q==1 & df_SRC$N2_MISS!=1),],gcm1,"Y",NULL,n_bootstraps,"balanced")
bootstrap(df_SRC2[which(df_SRC$Q==1 & df_SRC$N2_MISS!=1),],wgt1,"Y",NULL,n_bootstraps,"balanced")

## RESULTS AMONG THOSE NOT ENROLLED IN EPPP

df_SRC3<-df_SRC
df_SRC3[,"Q2"]<-ifelse(df_SRC3$W2_VISIT==1 & df_SRC3$W2_ESRD!=1 & df_SRC3$W2_PREG!=1 & df_SRC3$W2_HTN==1 & df_SRC3$W2_RACE!=2 & df_SRC3$W2_STATE=="MD" & df_SRC3$W2_2014==1,1,0)
df_SRC3[,"Q1"]<-ifelse(df_SRC3$W1_MYCHART==0,1,0)
df_SRC3[,"Q0"]<-ifelse(df_SRC3$W0_2015==1,1,0)
df_SRC3[,"Q"] <-df_SRC3$Q2*df_SRC3$Q1*df_SRC3$Q0
table(df_SRC$Q) #14037
table(df_SRC3$Q) #9109

bootstrap(df_SRC3[which(df_SRC3$Q==1 & df_SRC$N2_MISS!=1),],gcm1,"Y",NULL,n_bootstraps,"balanced")
bootstrap(df_SRC3[which(df_SRC3$Q==1 & df_SRC$N2_MISS!=1),],wgt1,"Y",NULL,n_bootstraps,"balanced")

###############
## CLOSE LOG ##
###############

sink()

########################
## ESTIMATED RUN TIME ##
########################

#@ 5 bootstraps -> 150 seconds total
#Thus, @ 500 bootstraps. 15000 seconds total
#thus, 15000sec/60min/sec/60hr/min = 4.17 hours