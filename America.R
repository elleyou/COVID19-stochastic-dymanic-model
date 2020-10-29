
set.seed(22334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("data_hw.xlsx") %>%
  select(date, cases_rb	,recover_rb,cases_hg	,recover_hg,cases_yg	,recover_yg,cases_ydl	,recover_ydl,cases_dg	,recover_dg,
         cases_mg	,recover_mg,cases_fg	,recover_fg)-> data_new
plot(data_new$cases_rb,type="o",xlab="date",ylab="confirmed_Japan")
plot(data_new$cases_hg,type="o",xlab="date",ylab="confirmed_Korea") 
plot(data_new$cases_mg,type="o",xlab="date",ylab="confirmed_America")
plot(data_new$cases_dg,type="o",xlab="date",ylab="confirmed_Germany")
plot(data_new$cases_fg,type="o",xlab="date",ylab="confirmed_France")
plot(data_new$cases_yg,type="o",xlab="date",ylab="confirmed_England")
plot(data_new$cases_ydl,type="o",xlab="date",ylab="confirmed_Italy")


read.xlsx("mg.xlsx") %>%
  select(date,cases_mg)-> data_mg
p=nrow(data_mg)
while(p>1){data_mg[p,-1]=data_mg[p,-1]-data_mg[p-1,-1]
p=p-1}
data_mg=data_mg[-1,]
read.xlsx("mg.xlsx") %>%
  select(date,cases_mg)-> data_mg1

sir_step <- Csnippet("
  double N_mg=327000000;
  
  
  double r_q=0;
  double r_s=1/5.3;
  
  double lambda_IN_mg1=lambda_IN_mg;
  double lambda_E_mg=lambda_IN_mg1*theta;
  
  double dN_SE_mg =S_mg*(lambda_E_mg*E_mg+lambda_IN_mg1*IN_mg)/N_mg*dt;
  double dN_EEq_mg = E_mg*rho*q_mg*r_q*dt;
  double dN_EIN_mg = E_mg*rho*r_s*dt;
  double dN_EqIH_mg = Eq_mg*r_s*dt; 
  double dN_INIH_mg = IN_mg*((1-q_mg)*r_H+q_mg*(r_H+r_q))*dt;
  double dN_ER_mg = E_mg*((1-rho)*(1-q_mg)*gamma_E+(1-rho)*q_mg*(r_q+gamma_E))*dt;
  
  S_mg += -dN_SE_mg;
  E_mg +=  dN_SE_mg-dN_EEq_mg-dN_EIN_mg-dN_ER_mg;
  Eq_mg += dN_EEq_mg-dN_EqIH_mg;
  IN_mg += -dN_INIH_mg+dN_EIN_mg;
  IH_mg += dN_EqIH_mg+dN_INIH_mg;
  R_mg += dN_ER_mg;
  N_IH_mg += dN_EqIH_mg+dN_INIH_mg;
  
  
  
  time += dt;
")

sir_init <- Csnippet("

  
  
  S_mg = 327000000;
  E_mg = E0_mg;
  Eq_mg = 0;
  IN_mg = IN0_mg;
  IH_mg = 35;
  R_mg = 0;

  

  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_mg),N_IH_mg,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_mg = rpois(N_IH_mg);
    
  ")


dp <- function (lambda_IN_mg,r_H,IN0_mg,E0_mg,log=TRUE,...) {
  f=dunif(lambda_IN_mg,0,0.7,log=log)+
    dunif(IN0_mg,0,32,log=log)+dunif(E0_mg,0,32*exp(1),log=log)+dunif(r_H,0,1,log=log)
  return(f)
}

pars <- c(rho=0.71,gamma_E=0.1,theta=0.48,
          lambda_IN_mg=0.5,q_mg=0,E0_mg=32,IN0_mg=16,r_H=0.2)



data_mg %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_mg"),
    statenames=c("time","S_mg","E_mg","Eq_mg","IN_mg","IH_mg","R_mg","N_IH_mg"),
    paramnames=c("rho","gamma_E","theta","r_H",
                 "lambda_IN_mg","q_mg","E0_mg","IN0_mg"),
    params=pars
  ) -> measSIR


K_mg<-pmcmc(measSIR, start=pars,
          Nmcmc = 1000000, Np = 2, tol=0,
          proposal=mvn.diag.rw(c(lambda_IN_mg=0.01,r_H=0.01,
                                 IN0_mg=2,E0_mg=2
          )))


plot(K_mg,pars=c("E0_mg","IN0_mg"))
plot(K_mg,pars=c("lambda_IN_mg","r_H"))
par_mg=apply(as.matrix(traces(K_mg)),2,mean)

measSIR %>%
  simulate(params=par_mg,nsim=1,format="data.frame") -> y_mg
plot(y_mg$IH_mg,type="o",xlab="date",ylab="cases")
lines(data_mg1$cases_mg,col="red")










read.xlsx("fg.xlsx") %>%
  select(date,cases_fg)-> data_fg
p=nrow(data_fg)
while(p>1){data_fg[p,-1]=data_fg[p,-1]-data_fg[p-1,-1]
p=p-1}
data_fg=data_fg[-1,]

read.xlsx("fg.xlsx") %>%
  select(date,cases_fg)-> data_fg1

sir_step <- Csnippet("
  double N_fg=66990000;
  
  
  double r_q=0;
  double r_s=1/5.3;
  
  double lambda_IN_fg1=lambda_IN_fg;
  double lambda_E_fg=lambda_IN_fg1*theta;
  
  double dN_SE_fg =S_fg*(lambda_E_fg*E_fg+lambda_IN_fg1*IN_fg)/N_fg*dt;
  double dN_EEq_fg = E_fg*rho*q_fg*r_q*dt;
  double dN_EIN_fg = E_fg*rho*r_s*dt;
  double dN_EqIH_fg = Eq_fg*r_s*dt; 
  double dN_INIH_fg = IN_fg*((1-q_fg)*r_H+q_fg*(r_H+r_q))*dt;
  double dN_ER_fg = E_fg*((1-rho)*(1-q_fg)*gamma_E+(1-rho)*q_fg*(r_q+gamma_E))*dt;
  
  S_fg += -dN_SE_fg;
  E_fg +=  dN_SE_fg-dN_EEq_fg-dN_EIN_fg-dN_ER_fg;
  Eq_fg += dN_EEq_fg-dN_EqIH_fg;
  IN_fg += -dN_INIH_fg+dN_EIN_fg;
  IH_fg += dN_EqIH_fg+dN_INIH_fg;
  R_fg += dN_ER_fg;
  N_IH_fg += dN_EqIH_fg+dN_INIH_fg;
  
  
  
  time += dt;
")

sir_init <- Csnippet("

  S_fg = 66990000;
  E_fg = E0_fg;
  Eq_fg = 0;
  IN_fg = IN0_fg;
  IH_fg = 18;
  R_fg = 0;

  

  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_fg),N_IH_fg,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_fg = rpois(N_IH_fg);
    
  ")


dp <- function (lambda_IN_fg,r_H,IN0_fg,E0_fg,log=TRUE,...) {
  f=dunif(lambda_IN_fg,0,1,log=log)+
    dunif(IN0_fg,0,173,log=log)+dunif(E0_fg,0,173*exp(1),log=log)+dunif(r_H,0,1,log=log)
  return(f)
}

pars <- c(rho=0.71,gamma_E=0.1,theta=0.48,
          lambda_IN_fg=0.5,q_fg=0,E0_fg=173,IN0_fg=80,r_H=0.2)



data_fg %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_fg"),
    statenames=c("time","S_fg","E_fg","Eq_fg","IN_fg","IH_fg","R_fg","N_IH_fg"),
    paramnames=c("rho","gamma_E","theta","r_H",
                 "lambda_IN_fg","q_fg","E0_fg","IN0_fg"),
    params=pars
  ) -> measSIR


K_fg<-pmcmc(measSIR, start=pars,
            Nmcmc = 1000000, Np = 2, tol=0,
            proposal=mvn.diag.rw(c(lambda_IN_fg=0.01,r_H=0.01,
                                   IN0_fg=5,E0_fg=5
            )))


plot(K_fg,pars=c("E0_fg","IN0_fg"))
plot(K_fg,pars=c("lambda_IN_fg","r_H"))
par_fg=apply(as.matrix(traces(K_fg)),2,mean)

measSIR %>%
  simulate(params=par_fg,nsim=1,format="data.frame") -> y_fg
plot(y_fg$IH_fg,type="o",xlab="date",ylab="cases_france")
lines(data_fg1$cases_fg,col="red")

























read.xlsx("dg.xlsx") %>%
  select(date,cases_dg)-> data_dg
p=nrow(data_dg)
while(p>1){data_dg[p,-1]=data_dg[p,-1]-data_dg[p-1,-1]
p=p-1}
data_dg=data_dg[-1,]

read.xlsx("dg.xlsx") %>%
  select(date,cases_dg)-> data_dg1

sir_step <- Csnippet("
  double N_dg=82930000;
  
  
  double r_q=0;
  double r_s=1/5.3;
  
  double lambda_IN_dg1=lambda_IN_dg;
  double lambda_E_dg=lambda_IN_dg1*theta;
  
  double dN_SE_dg =S_dg*(lambda_E_dg*E_dg+lambda_IN_dg1*IN_dg)/N_dg*dt;
  double dN_EEq_dg = E_dg*rho*q_dg*r_q*dt;
  double dN_EIN_dg = E_dg*rho*r_s*dt;
  double dN_EqIH_dg = Eq_dg*r_s*dt; 
  double dN_INIH_dg = IN_dg*((1-q_dg)*r_H+q_dg*(r_H+r_q))*dt;
  double dN_ER_dg = E_dg*((1-rho)*(1-q_dg)*gamma_E+(1-rho)*q_dg*(r_q+gamma_E))*dt;
  
  S_dg += -dN_SE_dg;
  E_dg +=  dN_SE_dg-dN_EEq_dg-dN_EIN_dg-dN_ER_dg;
  Eq_dg += dN_EEq_dg-dN_EqIH_dg;
  IN_dg += -dN_INIH_dg+dN_EIN_dg;
  IH_dg += dN_EqIH_dg+dN_INIH_dg;
  R_dg += dN_ER_dg;
  N_IH_dg += dN_EqIH_dg+dN_INIH_dg;
  
  
  
  time += dt;
")

sir_init <- Csnippet("

  S_dg =82930000;
  E_dg = E0_dg;
  Eq_dg = 0;
  IN_dg = IN0_dg;
  IH_dg = 18;
  R_dg = 0;

  

  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_dg),N_IH_dg,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_dg = rpois(N_IH_dg);
    
  ")


dp <- function (lambda_IN_dg,r_H,IN0_dg,E0_dg,log=TRUE,...) {
  f=dunif(lambda_IN_dg,0,1,log=log)+
    dunif(IN0_dg,0,122,log=log)+dunif(E0_dg,0,122*exp(1),log=log)+dunif(r_H,0,1,log=log)
  return(f)
}

pars <- c(rho=0.71,gamma_E=0.1,theta=0.48,
          lambda_IN_dg=0.5,q_dg=0,E0_dg=122,IN0_dg=61,r_H=0.2)



data_dg %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_dg"),
    statenames=c("time","S_dg","E_dg","Eq_dg","IN_dg","IH_dg","R_dg","N_IH_dg"),
    paramnames=c("rho","gamma_E","theta","r_H",
                 "lambda_IN_dg","q_dg","E0_dg","IN0_dg"),
    params=pars
  ) -> measSIR


K_dg<-pmcmc(measSIR, start=pars,
            Nmcmc = 1000000, Np = 2, tol=0,
            proposal=mvn.diag.rw(c(lambda_IN_dg=0.01,r_H=0.01,
                                   IN0_dg=5,E0_dg=5
            )))


plot(K_dg,pars=c("E0_dg","IN0_dg"))
plot(K_dg,pars=c("lambda_IN_dg","r_H"))
par_dg=apply(as.matrix(traces(K_dg)),2,mean)

measSIR %>%
  simulate(params=par_dg,nsim=1,format="data.frame") -> y_dg
plot(y_dg$IH_dg,type="o",xlab="date",ylab="cases_germany")
lines(data_dg1$cases_dg,col="red")











read.xlsx("hg.xlsx") %>%
  select(date,cases_hg)-> data_hg
p=nrow(data_hg)
while(p>1){data_hg[p,-1]=data_hg[p,-1]-data_hg[p-1,-1]
p=p-1}
data_hg=data_hg[-1,]

read.xlsx("hg.xlsx") %>%
  select(date,cases_hg)-> data_hg1

sir_step <- Csnippet("
  double N_hg=51640000;
  
  double r_q=1/(10.9-4.3);
  double r_s=1/5.3;
  double r_H=1/(10.9-5.3);
  
  if(time>=7){r_q=1/3.6;}
  
  double lambda_IN_hg1=lambda_IN_hg;
  if(time>=10){lambda_IN_hg1=lambda_IN_hg*a_hg;}
  double lambda_E_hg=lambda_IN_hg1*theta;
  
  double dN_SE_hg =S_hg*(lambda_E_hg*E_hg+lambda_IN_hg1*IN_hg)/N_hg*dt;
  double dN_EEq_hg = E_hg*rho*q_hg*r_q*dt;
  double dN_EIN_hg = E_hg*rho*r_s*dt;
  double dN_EqIH_hg = Eq_hg*r_s*dt; 
  double dN_INIH_hg = IN_hg*((1-q_hg)*r_H+q_hg*(r_H+r_q))*dt;
  double dN_ER_hg = E_hg*((1-rho)*(1-q_hg)*gamma_E+(1-rho)*q_hg*(r_q+gamma_E))*dt;
  
  S_hg += -dN_SE_hg;
  E_hg +=  dN_SE_hg-dN_EEq_hg-dN_EIN_hg-dN_ER_hg;
  Eq_hg += dN_EEq_hg-dN_EqIH_hg;
  IN_hg += -dN_INIH_hg+dN_EIN_hg;
  IH_hg += dN_EqIH_hg+dN_INIH_hg;
  R_hg += dN_ER_hg;
  N_IH_hg += dN_EqIH_hg+dN_INIH_hg;
  
  
  
  time += dt;
")

sir_init <- Csnippet("

  S_hg =51640000;
  E_hg = E0_hg;
  Eq_hg = 0;
  IN_hg = IN0_hg;
  IH_hg = 31;
  R_hg = 0;

  

  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_hg),N_IH_hg,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_hg = rpois(N_IH_hg);
    
  ")


dp <- function (lambda_IN_hg,IN0_hg,E0_hg,q_hg,a_hg,log=TRUE,...) {
  f=dunif(lambda_IN_hg,0,1,log=log)+dunif(a_hg,0,1,log=log)+dunif(q_hg,0,1,log=log)+
    dunif(IN0_hg,0,571,log=log)+dunif(E0_hg,0,571*exp(1),log=log)
  return(f)
}

pars <- c(rho=0.71,gamma_E=0.1,theta=0.48,a_hg=0.5,
          lambda_IN_hg=0.5,q_hg=0.5,E0_hg=571,IN0_hg=280)



data_hg %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_hg"),
    statenames=c("time","S_hg","E_hg","Eq_hg","IN_hg","IH_hg","R_hg","N_IH_hg"),
    paramnames=c("rho","gamma_E","theta","a_hg",
                 "lambda_IN_hg","q_hg","E0_hg","IN0_hg"),
    params=pars
  ) -> measSIR


K_hg<-pmcmc(measSIR, start=pars,
            Nmcmc = 1000000, Np = 2, tol=0,
            proposal=mvn.diag.rw(c(lambda_IN_hg=0.01,q_hg=0.01,a_hg=0.01,
                                   IN0_hg=5,E0_hg=5
            )))


plot(K_hg,pars=c("E0_hg","IN0_hg"))
plot(K_hg,pars=c("lambda_IN_hg","a_hg","q_hg"))
par_hg=apply(as.matrix(traces(K_hg)),2,mean)

measSIR %>%
  simulate(params=par_hg,nsim=1,format="data.frame") -> y_hg
plot(y_hg$IH_hg,type="o",xlab="date",ylab="cases_korea")
lines(data_hg1$cases_hg,col="red")















read.xlsx("yg.xlsx") %>%
  select(date,cases_yg)-> data_yg
p=nrow(data_yg)
while(p>1){data_yg[p,-1]=data_yg[p,-1]-data_yg[p-1,-1]
p=p-1}
data_yg=data_yg[-1,]

read.xlsx("yg.xlsx") %>%
  select(date,cases_yg)-> data_yg1

sir_step <- Csnippet("
  double N_yg=66490000;
  
  
  double r_q=0;
  double r_s=1/5.3;
  
  double lambda_IN_yg1=lambda_IN_yg;
  double lambda_E_yg=lambda_IN_yg1*theta;
  
  double dN_SE_yg =S_yg*(lambda_E_yg*E_yg+lambda_IN_yg1*IN_yg)/N_yg*dt;
  double dN_EEq_yg = E_yg*rho*q_yg*r_q*dt;
  double dN_EIN_yg = E_yg*rho*r_s*dt;
  double dN_EqIH_yg = Eq_yg*r_s*dt; 
  double dN_INIH_yg = IN_yg*((1-q_yg)*r_H+q_yg*(r_H+r_q))*dt;
  double dN_ER_yg = E_yg*((1-rho)*(1-q_yg)*gamma_E+(1-rho)*q_yg*(r_q+gamma_E))*dt;
  
  S_yg += -dN_SE_yg;
  E_yg +=  dN_SE_yg-dN_EEq_yg-dN_EIN_yg-dN_ER_yg;
  Eq_yg += dN_EEq_yg-dN_EqIH_yg;
  IN_yg += -dN_INIH_yg+dN_EIN_yg;
  IH_yg += dN_EqIH_yg+dN_INIH_yg;
  R_yg += dN_ER_yg;
  N_IH_yg += dN_EqIH_yg+dN_INIH_yg;
  
  
  
  time += dt;
")

sir_init <- Csnippet("

  S_yg =66490000;
  E_yg = E0_yg;
  Eq_yg = 0;
  IN_yg = IN0_yg;
  IH_yg = 23;
  R_yg = 0;

  

  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_yg),N_IH_yg,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_yg = rpois(N_IH_yg);
    
  ")


dp <- function (lambda_IN_yg,r_H,IN0_yg,E0_yg,log=TRUE,...) {
  f=dunif(lambda_IN_yg,0,1,log=log)+
    dunif(IN0_yg,0,93,log=log)+dunif(E0_yg,0,93*exp(1),log=log)+dunif(r_H,0,1,log=log)
  return(f)
}

pars <- c(rho=0.71,gamma_E=0.1,theta=0.48,
          lambda_IN_yg=0.5,q_yg=0,E0_yg=93,IN0_yg=46,r_H=0.2)



data_yg %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_yg"),
    statenames=c("time","S_yg","E_yg","Eq_yg","IN_yg","IH_yg","R_yg","N_IH_yg"),
    paramnames=c("rho","gamma_E","theta","r_H",
                 "lambda_IN_yg","q_yg","E0_yg","IN0_yg"),
    params=pars
  ) -> measSIR


K_yg<-pmcmc(measSIR, start=pars,
            Nmcmc = 1000000, Np = 2, tol=0,
            proposal=mvn.diag.rw(c(lambda_IN_yg=0.01,r_H=0.01,
                                   IN0_yg=5,E0_yg=5
            )))


plot(K_yg,pars=c("E0_yg","IN0_yg"))
plot(K_yg,pars=c("lambda_IN_yg","r_H"))
par_yg=apply(as.matrix(traces(K_yg)),2,mean)

measSIR %>%
  simulate(params=par_yg,nsim=1,format="data.frame") -> y_yg
data_yg1=data_yg1[-c(1),]
plot(y_yg$IH_yg,type="o",xlab="date",ylab="cases_England")
lines(data_yg1$cases_yg,col="red",type="o")



























read.xlsx("xby.xlsx") %>%
  select(date,cases_xby)-> data_xby
p=nrow(data_xby)
while(p>1){data_xby[p,-1]=data_xby[p,-1]-data_xby[p-1,-1]
p=p-1}
data_xby=data_xby[-1,]

read.xlsx("xby.xlsx") %>%
  select(date,cases_xby)-> data_xby1

sir_step <- Csnippet("
  double N_xby=46720000;
  
  
  double r_q=0;
  double r_s=1/5.3;
  
  double lambda_IN_xby1=lambda_IN_xby;
  double lambda_E_xby=lambda_IN_xby1*theta;
  
  double dN_SE_xby =S_xby*(lambda_E_xby*E_xby+lambda_IN_xby1*IN_xby)/N_xby*dt;
  double dN_EEq_xby = E_xby*rho*q_xby*r_q*dt;
  double dN_EIN_xby = E_xby*rho*r_s*dt;
  double dN_EqIH_xby = Eq_xby*r_s*dt; 
  double dN_INIH_xby = IN_xby*((1-q_xby)*r_H+q_xby*(r_H+r_q))*dt;
  double dN_ER_xby = E_xby*((1-rho)*(1-q_xby)*gamma_E+(1-rho)*q_xby*(r_q+gamma_E))*dt;
  
  S_xby += -dN_SE_xby;
  E_xby +=  dN_SE_xby-dN_EEq_xby-dN_EIN_xby-dN_ER_xby;
  Eq_xby += dN_EEq_xby-dN_EqIH_xby;
  IN_xby += -dN_INIH_xby+dN_EIN_xby;
  IH_xby += dN_EqIH_xby+dN_INIH_xby;
  R_xby += dN_ER_xby;
  N_IH_xby += dN_EqIH_xby+dN_INIH_xby;
  
  
  
  time += dt;
")

sir_init <- Csnippet("

  S_xby =46720000;
  E_xby = E0_xby;
  Eq_xby = 0;
  IN_xby = IN0_xby;
  IH_xby = 25;
  R_xby = 0;

  

  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_xby),N_IH_xby,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_xby = rpois(N_IH_xby);
    
  ")


dp <- function (lambda_IN_xby,r_H,IN0_xby,E0_xby,log=TRUE,...) {
  f=dunif(lambda_IN_xby,0,1,log=log)+
    dunif(IN0_xby,0,140,log=log)+dunif(E0_xby,0,140*exp(1),log=log)+dunif(r_H,0,1,log=log)
  return(f)
}

pars <- c(rho=0.71,gamma_E=0.1,theta=0.48,
          lambda_IN_xby=0.7,q_xby=0,E0_xby=140,IN0_xby=70,r_H=0.2)



data_xby %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_xby"),
    statenames=c("time","S_xby","E_xby","Eq_xby","IN_xby","IH_xby","R_xby","N_IH_xby"),
    paramnames=c("rho","gamma_E","theta","r_H",
                 "lambda_IN_xby","q_xby","E0_xby","IN0_xby"),
    params=pars
  ) -> measSIR


K_xby<-pmcmc(measSIR, start=pars,
             Nmcmc = 1000, Np = 2, tol=0,
             proposal=mvn.diag.rw(c(lambda_IN_xby=0.01,r_H=0.01,
                                    IN0_xby=5,E0_xby=5
             )))


plot(K_xby,pars=c("E0_xby","IN0_xby"))
plot(K_xby,pars=c("lambda_IN_xby","r_H"))
par_xby=apply(as.matrix(traces(K_xby)),2,mean)

measSIR %>%
  simulate(params=par_xby,nsim=1,format="data.frame") -> y_xby
data_xby1=data_xby1[-c(1),]
plot(y_xby$IH_xby,type="o",xlab="date",ylab="cases_spain")
lines(data_xby1$cases_xby,col="red",type="o")
