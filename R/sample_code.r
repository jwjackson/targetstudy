#########################################
#########################################
##### SAMPLE R CODE FOR ALL DESIGNS #####
#########################################
#########################################

#############
# VARIABLES #
#############

#Q0 - partial eligibility (1=yes,0=no) based on eligibility-related variable W~ (i.e., superscript is a vertical wave with no horizontal cross)
#Q1 - partial eligibility (1=yes,0=no) based on eligibility-related variable W+ (i.e., superscript is a vertical dagger with a single horizontal cross)
#Q2 - partial eligibility (1=yes,0=no) based on eligibility-related variable W++ (i.e., superscript is a vertical dagger with two horizontal crosses)
#Q  - full eligibility (1=yes,0=no) based on eligibility variables W~,W+,W++ (i.e., no superscript)
#TT - membership in standard population (1=yes,0=no) (i.e., this is T in the text)
#R  - group membership (1=marginalized,0=privileged)
#A  - allowable covariate(s)
#N  - non-allowable covariate(s)
#k_time - calendar time k

##########################
# DESIGN 1 VIA WEIGHTING #
##########################

wgt1 <- function (df,outcome) {
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  #Step 1: fit models
  fit_r  <- glm(R~1,binomial("logit"),df[which(df$Q==1),])
  fit_rx <- glm(R~A+k_time,binomial("logit"),df[which(df$Q==1),])
  fit_t  <- glm(TT~1,binomial("logit"),df[which(df$Q==1),])
  fit_tx <- glm(TT~A+k_time,binomial("logit"),df[which(df$Q==1),])  
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

##############################
# DESIGN 1 VIA G-COMPUTATION #
##############################

gcm1 <- function (df,outcome) {
  R_mat<-c(1,0)
  EP1_list<-list()
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  for (iter in 1:2) {
    rval<-R_mat[iter]
    #Step 1: fit model for Y among R=r
    fit_step1<-glm(Y~A+k_time,binomial("logit"),df[which(df$Q==1 & df$R==rval),])
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
  
##########################
# DESIGN 2 VIA WEIGHTING #
##########################

wgt2 <- function (df,outcome,phi=NULL,theta=NULL,density_ratio=NULL) {
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
    fit_q1x_list[[iter]] <- glm(Q1~N+A+k_time,binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
    }
  fit_r   <- glm(R~1,binomial("logit"),df[which(df$Q2==1),])
  fit_rx  <- glm(R~A+k_time,binomial("logit"),df[which(df$Q2==1),])
  fit_t   <- glm(TT~1,binomial("logit"),df[which(df$Q2==1),])
  fit_tx  <- glm(TT~A+k_time,binomial("logit"),df[which(df$Q2==1),])  
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

##############################
# DESIGN 2 VIA G-COMPUTATION #
##############################

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
    fit_step1<-glm(Y~N+A+k_time,binomial("logit"),df[which(df$Q==1 & df$R==rval),])
    #Step 2: obtain predicted values P1
    df[,"P1"] <- predict(fit_step1,df,"response")
    #Step 3: fit model for P1 among R=r
    fit_step3<-glm(P1~A+k_time,quasibinomial("logit"),df[which(df$Q2==1 & df$R==rval),],df$phi[I(df$Q2==1)*I(df$R==rval)])
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

##########################
# DESIGN 3 VIA WEIGHTING #
##########################

wgt3 <- function (df,outcome) {
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  #Step 1: fit models
  R_mat<-c(1,0)
  fit_q1_list  <- list()
  fit_q1x_list <- list()
  for (iter in 1:2) {
    rval<-R_mat[iter]
    fit_q1_list[[iter]]  <- glm(Q1~1,binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
    fit_q1x_list[[iter]] <- glm(Q1~N+A+k_time,binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
    }
  fit_r   <- glm(R~1,binomial("logit"),df[which(df$Q2==1 & df$Q1==0),])
  fit_rx  <- glm(R~A+k_time,binomial("logit"),df[which(df$Q2==1 & df$Q1==0),])
  fit_t   <- glm(TT~1,binomial("logit"),df[which(df$Q2==1 & df$Q1==0),])
  fit_tx  <- glm(TT~A+k_time,binomial("logit"),df[which(df$Q2==1 & df$Q1==0),])  
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

##############################
# DESIGN 3 VIA G-COMPUTATION #
##############################

gcm3 <- function (df,outcome) {
  R_mat<-c(1,0)
  EP2_list<-list()
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  for (iter in 1:2) {
    rval<-R_mat[iter]
    #Step 1: fit model for Y among R=r
    fit_step1<-glm(Y~N+A+k_time,binomial("logit"),df[which(df$Q==1 & df$R==rval),])
    #Step 2: obtain predicted values P1
    df[,"P1"] <- predict(fit_step1,df,"response")
    #Step 3: fit model for P1 among R=r
    fit_step3<-glm(P1~A+k_time,quasibinomial("logit"),df[which(df$Q2==1 & df$Q1==0 & df$R==rval),])
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

###################################################
# OBTAIN DISTRIBUTION OF Q+ UNDER INTERVENTION G+ #
###################################################

q1_distribution <- function (df,subdesign) {
  if (subdesign=="a") {
    fit_a <- glm(Q1~k_time,binomial("logit"),df[which(df$Q2==1),])
    pq1 <- predict(fit_a,df,"response")
    } else if (subdesign=="b") {
      fit_b_R1 <- glm(Q1~A+k_time,binomial("logit"),df[which(df$Q2==1 & df$R==1),])
      fit_b_R0 <- glm(Q1~A+k_time,binomial("logit"),df[which(df$Q2==1 & df$R==0),])
      pq1_R1 <- predict(fit_b_R1,df,"response")
      pq1_R0 <- predict(fit_b_R0,df,"response")
      pq1 <- df$R*pq1_R1 + (1-df$R)*pq1_R0
      } else if (subdesign=="c") {
        fit_c <- glm(Q1~N+A+k_time,binomial("logit"),df[which(df$Q2==1 & df$TT==1),])
        pq1 <- predict(fit_c,df,"response")
        }
  return(pq1)
  }
  
################
# ESTIMATE PHI #
################

estimate_phi <- function (df,subdesign) {
  df$pq1<-q1_distribution(df,subdesign)
  R_mat<-c(1,0)
  pq0_i_list<-list()
  pq0_ii_list<-list()
  for (iter in 1:2) {
    rval<-R_mat[iter]
    #Step 1: fit model for Q~ among R=r
    fit_step1<-glm(Q0~N+A+k_time,binomial("logit"),df[which(df$Q2==1 & df$Q1==1 & df$R==rval),])
    #Step 2: obtain predicted values pq0_i
    pq0_i_list[[iter]] <- predict(fit_step1,df,"response")
    df[,"pq0_i_temp"]<-pq0_i_list[[iter]]
    #Step 3: fit model for pq1*pq0_i among R=r
    fit_step3<-glm(pq1*pq0_i_temp~A+k_time,quasibinomial("logit"),df[which(df$Q2==1 & df$R==rval),])
    #Step 4: obtain predicted values pq0_ii
    pq0_ii_list[[iter]] <- predict(fit_step3,df,"response")
    }      
  df[,"pq0_i"]<-df$R*pq0_i_list[[1]]+(1-df$R)*pq0_i_list[[2]]
  df[,"pq0_ii"]<-df$R*pq0_ii_list[[1]]+(1-df$R)*pq0_ii_list[[2]]
  #step 5: calculate phi
  output<-df$pq1*df$pq0_i/df$pq0_ii
  return(output)
  }

##################
# ESTIMATE THETA #
##################

estimate_theta <- function (df,subdesign) {
  df$pq1<-q1_distribution(df,subdesign)
  #Step 1: fit model for pq1 among TT=1
  fit_step1<-glm(pq1~N+A+k_time,quasibinomial("logit"),df[which(df$Q2==1 & df$TT==1),])
  #Step 2: obtain predicted values Epq1 
  df[,"Epq1"] <- predict(fit_step1,df,"response")
  #Step 3: fit model for Q~ among TT=1
  fit_step3<-glm(Q0~N+A+k_time,binomial("logit"),df[which(df$Q2==1 & df$Q1==1 & df$TT==1),])
  #Step 4: obtain predicted values pq0_i
  df[,"pq0_i"] <- predict(fit_step3,df,"response")
  #Step 5: fit model for Epq1*pq0 among TT=1
  fit_step5<-glm(Epq1*pq0_i~A+k_time,quasibinomial("logit"),df[which(df$Q2==1 & df$TT==1),])
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

##########################
# DESIGN 4 VIA WEIGHTING #
##########################

wgt4 <- function (df,outcome,subdesign) {
  #Step 0: pull outcome
  df[,"Y"] <- unlist(df[,outcome])
  #step 1: fit models
  R_mat<-c(1,0)
  fit_q0_list  <- list()
  fit_q0x_list <- list()
  for (iter in 1:2) {
    rval<-R_mat[iter]
    fit_q0_list[[iter]]  <- glm(Q0~1,binomial("logit"),df[which(df$Q2==1 & df$Q1==1 & df$R==rval),])
    fit_q0x_list[[iter]] <- glm(Q0~N+A+k_time,binomial("logit"),df[which(df$Q2==1 & df$Q1==1 & df$R==rval),])
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
  output <- wgt2(df,"Y",phi=df$phi,theta=df$theta,density_ratio=df$density_ratio)
  return(output)
  }

##############################
# DESIGN 4 VIA G-COMPUTATION #
##############################

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

########################################
# ESTIMATE PHI (SIMPLIFIED i.e. no Q~) #
########################################

estimate_phi_s <- function (df,subdesign) {
  if (subdesign=="b") output<-rep(1,nrow(df))
  else {
    df$pq1<-q1_distribution(df,subdesign)
    R_mat<-c(1,0)
    Epq1_list<-list()
    for (iter in 1:2) {
      rval<-R_mat[iter]
      #Step 1: fit model for pq1 among R=r
      fit_step1<-glm(pq1~A+k_time,quasibinomial("logit"),df[which(df$Q2==1 & df$R==rval),])
      #Step 2: obtain predicted values Eqp1
      Epq1_list[[iter]] <- predict(fit_step1,df,"response")
      }      
    df$Epq1<-df$R*Epq1_list[[1]]+(1-df$R)*Epq1_list[[2]]
    #step 5: calculate phi
    output<-df$pq1/df$Epq1
    }
  return(output)
  }

##################################################
# DESIGN 4 (SIMPLIFIED i.e. no Q~) VIA WEIGHTING #
##################################################

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
      fit_q1_list[[iter]]  <- glm(Q1~A+k_time,binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
      fit_q1x_list[[iter]] <- glm(Q1~N+A+k_time,binomial("logit"),df[which(df$Q2==1 & df$R==rval),])
    }
    fit_r   <- glm(R~1,binomial("logit"),df[which(df$Q==1),])
    fit_rx  <- glm(R~A+k_time,binomial("logit"),df[which(df$Q==1),])
    fit_t   <- glm(TT~1,binomial("logit"),df[which(df$Q==1),])
    fit_tx  <- glm(TT~A+k_time,binomial("logit"),df[which(df$Q==1),])  
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

######################################################
# DESIGN 4 (SIMPLIFIED i.e. no Q~) VIA G-COMPUTATION #
######################################################

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
      fit_step1<-glm(Y~N+A+k_time,binomial("logit"),df[which(df$Q==1 & df$R==rval),])
      #Step 2: obtain predicted values P1
      df[,"P1"] <- predict(fit_step1,df,"response")
      #Step 3: fit model for P1 among R=r
      fit_step3<-glm(P1~A+k_time,quasibinomial("logit"),df[which(df$Q2==1 & df$R==rval),],df$phi[which(df$Q2==1 & df$R==rval)]) 
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
