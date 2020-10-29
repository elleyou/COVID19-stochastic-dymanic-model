####fit gamma and b
## anhui
set.seed(1122334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("anhui.xlsx") %>%
  select(date, cases_ah	,recover_ah)-> data_new
read.xlsx("anhui.xlsx") %>%
  select(date, cases_ah	,recover_ah)-> data_new1
p=nrow(data_new)
while(p>1){data_new[p,-1]=data_new[p,-1]-data_new[p-1,-1]
p=p-1}
data_new=data_new[-1,]
data_new1=data_new1[-1,]
data_new1[,2]=data_new1[,2]-data_new1[,3]
data_new[,2]=data_new1[,2]



sir_step <- Csnippet("
  
gamma_ah=gamma_IH_ah;

if(time<24){gamma_ah=gamma_ah*b_ah;}
  time += dt;
")

sir_init <- Csnippet("
  time=0;
")


dmeas <-  Csnippet("
  double f;
  f = dpois(nearbyint(recover_ah),cases_ah*gamma_ah,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    recover_ah = rpois(cases_ah*gamma_ah);
  ")


dp <- function (
  gamma_IH_ah,b_ah,log=TRUE,...) {
  f=dunif(gamma_IH_ah,1/30,1,log=log)+dunif(b_ah,0,1,log=log)
  return(f)
}

pars <- c(gamma_IH_ah=0.047,b_ah=0.17)



data_new %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    statenames=c("time","gamma_ah"),
    paramnames=c("gamma_IH_ah","b_ah"),
    params=pars
  ) -> measSIR


K1<-pmcmc(measSIR, start=pars,
          Nmcmc = 100000, Np = 2, tol=0,
          proposal=mvn.diag.rw(c(gamma_IH_ah=0.01,b_ah=0.01)))


##result


> p_ah
loglik     log.prior         nfail   gamma_IH_ah          b_ah 
-139.10866968    0.03390155    0.00000000    0.07942372    0.24877493 









##hebei
set.seed(1122334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("hebei.xlsx") %>%
  select(date, cases_hb	,recover_hb)-> data_new
read.xlsx("hebei.xlsx") %>%
  select(date, cases_hb	,recover_hb)-> data_new1
p=nrow(data_new)
while(p>1){data_new[p,-1]=data_new[p,-1]-data_new[p-1,-1]
p=p-1}
data_new=data_new[-1,]
data_new1=data_new1[-1,]
data_new1[,2]=data_new1[,2]-data_new1[,3]
data_new[,2]=data_new1[,2]



sir_step <- Csnippet("
  
gamma_hb=gamma_IH_hb;

if(time<22){gamma_hb=gamma_hb*b_hb;}
  time += dt;
")

sir_init <- Csnippet("
  time=0;
")


dmeas <-  Csnippet("
  double f;
  f = dpois(nearbyint(recover_hb),cases_hb*gamma_hb,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    recover_hb = rpois(cases_hb*gamma_hb);
  ")


dp <- function (
  gamma_IH_hb,b_hb,log=TRUE,...) {
  f=dunif(gamma_IH_hb,1/30,1,log=log)+dunif(b_hb,0,1,log=log)
  return(f)
}

pars <- c(gamma_IH_hb=0.047,b_hb=0.17)



data_new %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    statenames=c("time","gamma_hb"),
    paramnames=c("gamma_IH_hb","b_hb"),
    params=pars
  ) -> measSIR


K2<-pmcmc(measSIR, start=pars,
          Nmcmc = 100000, Np = 2, tol=0,
          proposal=mvn.diag.rw(c(gamma_IH_hb=0.01,b_hb=0.01)))


##result
> p_2
loglik    log.prior        nfail  gamma_IH_hb         b_hb 
-69.61696865   0.03390155   0.00000000   0.08422052   0.33775630 











###Shandong

set.seed(1122334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("shandong.xlsx") %>%
  select(date, cases_sd	,recover_sd)-> data_new
read.xlsx("shandong.xlsx") %>%
  select(date, cases_sd	,recover_sd)-> data_new1
p=nrow(data_new)
while(p>1){data_new[p,-1]=data_new[p,-1]-data_new[p-1,-1]
p=p-1}
data_new=data_new[-1,]
data_new1=data_new1[-1,]
data_new1[,2]=data_new1[,2]-data_new1[,3]
data_new[,2]=data_new1[,2]



sir_step <- Csnippet("
  
gamma_sd=gamma_IH_sd;

if(time<21){gamma_sd=gamma_sd*b_sd;}
  time += dt;
")

sir_init <- Csnippet("
  time=0;
")


dmeas <-  Csnippet("
  double f;
  f = dpois(nearbyint(recover_sd),cases_sd*gamma_sd,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    recover_sd = rpois(cases_sd*gamma_sd);
  ")


dp <- function (
  gamma_IH_sd,b_sd,log=TRUE,...) {
  f=dunif(gamma_IH_sd,1/30,1,log=log)+dunif(b_sd,0,1,log=log)
  return(f)
}

pars <- c(gamma_IH_sd=0.047,b_sd=0.17)



data_new %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    statenames=c("time","gamma_sd"),
    paramnames=c("gamma_IH_sd","b_sd"),
    params=pars
  ) -> measSIR


K3<-pmcmc(measSIR, start=pars,
          Nmcmc = 100000, Np = 2, tol=0,
          proposal=mvn.diag.rw(c(gamma_IH_sd=0.01,b_sd=0.01)))



> par_sd
loglik    log.prior        nfail  gamma_IH_sd         b_sd 
-82.42102411   0.03390155   0.00000000   0.05643712   0.35909461 




###Jiangsu

set.seed(1122334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("jiangsu.xlsx") %>%
  select(date, cases_js	,recover_js)-> data_new
read.xlsx("jiangsu.xlsx") %>%
  select(date, cases_js	,recover_js)-> data_new1
p=nrow(data_new)
while(p>1){data_new[p,-1]=data_new[p,-1]-data_new[p-1,-1]
p=p-1}
data_new=data_new[-1,]
data_new1=data_new1[-1,]
data_new1[,2]=data_new1[,2]-data_new1[,3]
data_new[,2]=data_new1[,2]



sir_step <- Csnippet("
  
gamma_js=gamma_IH_js;

if(time<24){gamma_js=gamma_js*b_js;}
  time += dt;
")

sir_init <- Csnippet("
  time=0;
")


dmeas <-  Csnippet("
  double f;
  f = dpois(nearbyint(recover_js),cases_js*gamma_js,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    recover_js = rpois(cases_js*gamma_js);
  ")


dp <- function (
  gamma_IH_js,b_js,log=TRUE,...) {
  f=dunif(gamma_IH_js,1/30,1,log=log)+dunif(b_js,0,1,log=log)
  return(f)
}

pars <- c(gamma_IH_js=0.047,b_js=0.17)



data_new %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    statenames=c("time","gamma_js"),
    paramnames=c("gamma_IH_js","b_js"),
    params=pars
  ) -> measSIR


K4<-pmcmc(measSIR, start=pars,
          Nmcmc = 100000, Np = 2, tol=0,
          proposal=mvn.diag.rw(c(gamma_IH_js=0.01,b_js=0.01)))



> par_js
loglik     log.prior         nfail   gamma_IH_js          b_js 
-117.83019255    0.03390155    0.00000000    0.10202064    0.29046404 









##MCMC part









set.seed(1122334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("anhui.xlsx") %>%
  select(date, cases_ah	,recover_ah )-> data_new
p=nrow(data_new)
while(p>1){data_new[p,-1]=data_new[p,-1]-data_new[p-1,-1]
p=p-1}
data_new=data_new[-1,]


sir_step <- Csnippet("
    double N_ah=63659000;
  
  
  double r_q=1/(10.9-4.3);
  double r_s=1/5.3;
  double r_H=1/(10.9-5.3);
  
  if(time>=7){r_q=1/3.6;}
    
  double gamma_IH_ah1=gamma_IH_ah;
  if(time<24){gamma_IH_ah1=gamma_IH_ah1*b_ah;}
  
  double lambda_IN_ah1=lambda_IN_ah;
  if(time>=7){lambda_IN_ah1=lambda_IN_ah*a_ah;}
  double lambda_E_ah=lambda_IN_ah1*theta;
  
  double dN_SE_ah =S_ah*(lambda_E_ah*E_ah+lambda_IN_ah1*IN_ah)/N_ah*dt;
  double dN_EEq_ah = E_ah*rho*q_ah*r_q*dt;
  double dN_EIN_ah = E_ah*rho*r_s*dt;
  double dN_EqIH_ah = Eq_ah*r_s*dt;
  double dN_INIH_ah = IN_ah*((1-q_ah)*r_H+q_ah*(r_H+r_q))*dt;
  double dN_ER_ah = E_ah*((1-rho)*(1-q_ah)*gamma_E+(1-rho)*q_ah*(r_q+gamma_E))*dt;
  double dN_IHLRH_ah=IHL_ah*gamma_IH_ah1*dt;
  
  S_ah += -dN_SE_ah;
  E_ah +=  dN_SE_ah-dN_EEq_ah-dN_EIN_ah-dN_ER_ah;
  Eq_ah += dN_EEq_ah-dN_EqIH_ah;
  IN_ah += -dN_INIH_ah+dN_EIN_ah;
  IH_ah += dN_EqIH_ah+dN_INIH_ah;
  R_ah += dN_ER_ah;
  N_IH_ah += dN_EqIH_ah+dN_INIH_ah;
  IHL_ah += (dN_EqIH_ah+dN_INIH_ah)-dN_IHLRH_ah;
  RH_ah += dN_IHLRH_ah;
  N_RH_ah += dN_IHLRH_ah;
  
  
  time += dt;
")

sir_init <- Csnippet("

  
  S_ah = 63659000;
  E_ah = E0_ah;
  Eq_ah = 0;
  IN_ah = IN0_ah;
  IH_ah = 0;
  R_ah = 0;
  IHL_ah=9;
  RH_ah=0;
  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_ah),N_IH_ah,1)+dpois(nearbyint(recover_ah),N_RH_ah,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_ah = rpois(N_IH_ah);
    recover_ah = rpois(N_RH_ah);
  ")


dp <- function (a_ah,lambda_IN_ah,q_ah,IN0_ah,E0_ah,log=TRUE,...) {
  f=dunif(a_ah,0,0.2,log=log)+dunif(lambda_IN_ah,0,1,log=log)+
    dunif(q_ah,0,1,log=log)+
    dunif(IN0_ah,0,70,log=log)+dunif(E0_ah,0,70*exp(1),log=log)
  
  return(f)
}

pars <- c(rho=0.68,gamma_E=0.1,theta=0.47,
          lambda_IN_ah=0.54,q_ah=0.45,a_ah=0.06,E0_ah=100,IN0_ah=50,gamma_IH_ah=0.079,b_ah=0.249)



data_new %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_ah","N_RH_ah"),
    statenames=c("time","S_ah","E_ah","Eq_ah","IN_ah","IH_ah","R_ah","N_IH_ah","IHL_ah","RH_ah","N_RH_ah"),
    paramnames=c("rho","gamma_E","theta",
                 "lambda_IN_ah","q_ah","a_ah","E0_ah","IN0_ah","gamma_IH_ah","b_ah"),
    params=pars
  ) -> measSIR


K_ah<-pmcmc(measSIR, start=pars,
         Nmcmc = 1000000, Np = 2, tol=0,
         proposal=mvn.diag.rw(c(
                      lambda_IN_ah=0.01,q_ah=0.01,a_ah=0.01,
                                IN0_ah=5,E0_ah=5
         )))











#HEBEI






set.seed(1122334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("hebei.xlsx") %>%
  select(date, cases_hb	,recover_hb )-> data_new
p=nrow(data_new)
while(p>1){data_new[p,-1]=data_new[p,-1]-data_new[p-1,-1]
p=p-1}
data_new=data_new[-1,]


sir_step <- Csnippet("
  double N_hb=75919700;
  
  
  double r_q=1/(10.9-4.3);
  double r_s=1/5.3;
  double r_H=1/(10.9-5.3);
  
  if(time>=7){r_q=1/3.6;}
    
  double gamma_IH_hb1=gamma_IH_hb;
  if(time<24){gamma_IH_hb1=gamma_IH_hb1*b_hb;}
  
  double lambda_IN_hb1=lambda_IN_hb;
  if(time>=7){lambda_IN_hb1=lambda_IN_hb*a_hb;}
  double lambda_E_hb=lambda_IN_hb1*theta;
  
  double dN_SE_hb =S_hb*(lambda_E_hb*E_hb+lambda_IN_hb1*IN_hb)/N_hb*dt;
  double dN_EEq_hb = E_hb*rho*q_hb*r_q*dt;
  double dN_EIN_hb = E_hb*rho*r_s*dt;
  double dN_EqIH_hb = Eq_hb*r_s*dt;
  double dN_INIH_hb = IN_hb*((1-q_hb)*r_H+q_hb*(r_H+r_q))*dt;
  double dN_ER_hb = E_hb*((1-rho)*(1-q_hb)*gamma_E+(1-rho)*q_hb*(r_q+gamma_E))*dt;
  double dN_IHLRH_hb=IHL_hb*gamma_IH_hb1*dt;
  
  S_hb += -dN_SE_hb;
  E_hb +=  dN_SE_hb-dN_EEq_hb-dN_EIN_hb-dN_ER_hb;
  Eq_hb += dN_EEq_hb-dN_EqIH_hb;
  IN_hb += -dN_INIH_hb+dN_EIN_hb;
  IH_hb += dN_EqIH_hb+dN_INIH_hb;
  R_hb += dN_ER_hb;
  N_IH_hb += dN_EqIH_hb+dN_INIH_hb;
  IHL_hb += (dN_EqIH_hb+dN_INIH_hb)-dN_IHLRH_hb;
  RH_hb += dN_IHLRH_hb;
  N_RH_hb += dN_IHLRH_hb;
  
  
  time += dt;
")

sir_init <- Csnippet("

  
  S_hb = 75919700;
  E_hb = E0_hb;
  Eq_hb = 0;
  IN_hb = IN0_hb;
  IH_hb = 0;
  R_hb = 0;
  IHL_hb=2;
  RH_hb=0;
  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_hb),N_IH_hb,1)+dpois(nearbyint(recover_hb),N_RH_hb,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_hb = rpois(N_IH_hb);
    recover_hb = rpois(N_RH_hb);
  ")


dp <- function (a_hb,lambda_IN_hb,q_hb,IN0_hb,E0_hb,log=TRUE,...) {
  f=dunif(a_hb,0,0.2,log=log)+dunif(lambda_IN_hb,0,1,log=log)+
    dunif(q_hb,0,1,log=log)+
    dunif(IN0_hb,0,33,log=log)+dunif(E0_hb,0,33*exp(1),log=log)
  
  return(f)
}

pars <- c(rho=0.68,gamma_E=0.1,theta=0.47,
          lambda_IN_hb=0.54,q_hb=0.45,a_hb=0.06,E0_hb=50,IN0_hb=25,gamma_IH_hb=0.084,b_hb=0.338)



data_new %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_hb","N_RH_hb"),
    statenames=c("time","S_hb","E_hb","Eq_hb","IN_hb","IH_hb","R_hb","N_IH_hb","IHL_hb","RH_hb","N_RH_hb"),
    paramnames=c("rho","gamma_E","theta",
                 "lambda_IN_hb","q_hb","a_hb","E0_hb","IN0_hb","gamma_IH_hb","b_hb"),
    params=pars
  ) -> measSIR


K_hb<-pmcmc(measSIR, start=pars,
            Nmcmc = 1000000, Np = 2, tol=0,
            proposal=mvn.diag.rw(c(
              lambda_IN_hb=0.01,q_hb=0.01,a_hb=0.01,
              IN0_hb=5,E0_hb=5
            )))







> p_ah
loglik     log.prior         nfail           rho       gamma_E         theta  lambda_IN_ah 
-261.81558903   -7.88755257    0.00000000    0.68000000    0.10000000    0.47000000    0.67864857 
q_ah          a_ah         E0_ah        IN0_ah   gamma_IH_ah          b_ah 
0.10339846    0.01156224   77.28959006   40.69142503    0.07900000    0.24900000 
> p_hb
loglik     log.prior         nfail           rho       gamma_E         theta  lambda_IN_hb 
-173.31083790   -6.38357721    0.00000000    0.68000000    0.10000000    0.47000000    0.45141328 
q_hb          a_hb         E0_hb        IN0_hb   gamma_IH_hb          b_hb 
0.07704629    0.06887901   66.77739428   12.74964057    0.08400000    0.33800000 





###########Jiangsu

set.seed(1122334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("jiangsu.xlsx") %>%
  select(date, cases_js	,recover_js )-> data_new
p=nrow(data_new)
while(p>1){data_new[p,-1]=data_new[p,-1]-data_new[p-1,-1]
p=p-1}
data_new=data_new[-1,]


sir_step <- Csnippet("
  double N_js=80700000;
  
  
  double r_q=1/(10.9-4.3);
  double r_s=1/5.3;
  double r_H=1/(10.9-5.3);
  
  if(time>=7){r_q=1/3.6;}
    
  double gamma_IH_js1=gamma_IH_js;
  if(time<24){gamma_IH_js1=gamma_IH_js1*b_js;}
  
  double lambda_IN_js1=lambda_IN_js;
  if(time>=7){lambda_IN_js1=lambda_IN_js*a_js;}
  double lambda_E_js=lambda_IN_js1*theta;
  
  double dN_SE_js =S_js*(lambda_E_js*E_js+lambda_IN_js1*IN_js)/N_js*dt;
  double dN_EEq_js = E_js*rho*q_js*r_q*dt;
  double dN_EIN_js = E_js*rho*r_s*dt;
  double dN_EqIH_js = Eq_js*r_s*dt;
  double dN_INIH_js = IN_js*((1-q_js)*r_H+q_js*(r_H+r_q))*dt;
  double dN_ER_js = E_js*((1-rho)*(1-q_js)*gamma_E+(1-rho)*q_js*(r_q+gamma_E))*dt;
  double dN_IHLRH_js=IHL_js*gamma_IH_js1*dt;
  
  S_js += -dN_SE_js;
  E_js +=  dN_SE_js-dN_EEq_js-dN_EIN_js-dN_ER_js;
  Eq_js += dN_EEq_js-dN_EqIH_js;
  IN_js += -dN_INIH_js+dN_EIN_js;
  IH_js += dN_EqIH_js+dN_INIH_js;
  R_js += dN_ER_js;
  N_IH_js += dN_EqIH_js+dN_INIH_js;
  IHL_js += (dN_EqIH_js+dN_INIH_js)-dN_IHLRH_js;
  RH_js += dN_IHLRH_js;
  N_RH_js += dN_IHLRH_js;
  
  
  time += dt;
")

sir_init <- Csnippet("

  
  S_js = 80700000;
  E_js = E0_js;
  Eq_js = 0;
  IN_js = IN0_js;
  IH_js = 0;
  R_js = 0;
  IHL_js=1;
  RH_js=0;
  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_js),N_IH_js,1)+dpois(nearbyint(recover_js),N_RH_js,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_js = rpois(N_IH_js);
    recover_js = rpois(N_RH_js);
  ")


dp <- function (a_js,lambda_IN_js,q_js,IN0_js,E0_js,log=TRUE,...) {
  f=dunif(a_js,0,0.2,log=log)+dunif(lambda_IN_js,0,1,log=log)+
    dunif(q_js,0,1,log=log)+
    dunif(IN0_js,0,69,log=log)+dunif(E0_js,0,69*exp(1),log=log)
  
  return(f)
}

pars <- c(rho=0.68,gamma_E=0.1,theta=0.47,
          
          
          lambda_IN_js=0.54,q_js=0.45,a_js=0.06,E0_js=50,IN0_js=25,gamma_IH_js=0.102,b_js=0.290)



data_new %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_js","N_RH_js"),
    statenames=c
    
    ("time","S_js","E_js","Eq_js","IN_js","IH_js","R_js","N_IH_js","IHL_js","RH_js","N_RH_js")
    ,
    paramnames=c("rho","gamma_E","theta",
                 "lambda_IN_js","q_js","a_js","E0_js","IN0_js","gamma_IH_js","b_js"),
    params=pars
  ) -> measSIR


K_js<-pmcmc(measSIR, start=pars,
            Nmcmc = 1000000, Np = 2, tol=0,
            proposal=mvn.diag.rw(c(
              lambda_IN_js=0.01,q_js=0.01,a_js=0.01,
              IN0_js=5,E0_js=5
            )))



> p_js
loglik     log.prior         nfail           rho       gamma_E         theta  lambda_IN_js 
-209.48678910   -7.85877510    0.00000000    0.68000000    0.10000000    0.47000000    0.50898535 
q_js          a_js         E0_js        IN0_js   gamma_IH_js          b_js 
0.15725639    0.03499557  109.53582953   30.91628948    0.10200000    0.29000000 


###########Shandong


set.seed(1122334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("shandong.xlsx") %>%
  select(date, cases_sd	,recover_sd )-> data_new
p=nrow(data_new)
while(p>1){data_new[p,-1]=data_new[p,-1]-data_new[p-1,-1]
p=p-1}
data_new=data_new[-1,]


sir_step <- Csnippet("
  double N_sd=100702100;
  
  
  double r_q=1/(10.9-4.3);
  double r_s=1/5.3;
  double r_H=1/(10.9-5.3);
  
  if(time>=7){r_q=1/3.6;}
    
  double gamma_IH_sd1=gamma_IH_sd;
  if(time<21){gamma_IH_sd1=gamma_IH_sd1*b_sd;}
  
  double lambda_IN_sd1=lambda_IN_sd;
  if(time>=7){lambda_IN_sd1=lambda_IN_sd*a_sd;}
  double lambda_E_sd=lambda_IN_sd1*theta;
  
  double dN_SE_sd =S_sd*(lambda_E_sd*E_sd+lambda_IN_sd1*IN_sd)/N_sd*dt;
  double dN_EEq_sd = E_sd*rho*q_sd*r_q*dt;
  double dN_EIN_sd = E_sd*rho*r_s*dt;
  double dN_EqIH_sd = Eq_sd*r_s*dt;
  double dN_INIH_sd = IN_sd*((1-q_sd)*r_H+q_sd*(r_H+r_q))*dt;
  double dN_ER_sd = E_sd*((1-rho)*(1-q_sd)*gamma_E+(1-rho)*q_sd*(r_q+gamma_E))*dt;
  double dN_IHLRH_sd=IHL_sd*gamma_IH_sd1*dt;
  
  S_sd += -dN_SE_sd;
  E_sd +=  dN_SE_sd-dN_EEq_sd-dN_EIN_sd-dN_ER_sd;
  Eq_sd += dN_EEq_sd-dN_EqIH_sd;
  IN_sd += -dN_INIH_sd+dN_EIN_sd;
  IH_sd += dN_EqIH_sd+dN_INIH_sd;
  R_sd += dN_ER_sd;
  N_IH_sd += dN_EqIH_sd+dN_INIH_sd;
  IHL_sd += (dN_EqIH_sd+dN_INIH_sd)-dN_IHLRH_sd;
  RH_sd += dN_IHLRH_sd;
  N_RH_sd += dN_IHLRH_sd;
  
  
  time += dt;
")

sir_init <- Csnippet("

  
  S_sd = 100702100;
  E_sd = E0_sd;
  Eq_sd = 0;
  IN_sd = IN0_sd;
  IH_sd = 0;
  R_sd = 0;
  IHL_sd=6;
  RH_sd=0;
  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_sd),N_IH_sd,1)+dpois(nearbyint(recover_sd),N_RH_sd,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_sd = rpois(N_IH_sd);
    recover_sd = rpois(N_RH_sd);
  ")


dp <- function (a_sd,lambda_IN_sd,q_sd,IN0_sd,E0_sd,log=TRUE,...) {
  f=dunif(a_sd,0,0.2,log=log)+dunif(lambda_IN_sd,0,1,log=log)+
    dunif(q_sd,0,1,log=log)+
    dunif(IN0_sd,0,81,log=log)+dunif(E0_sd,0,81*exp(1),log=log)
  
  return(f)
}

pars <- c(rho=0.68,gamma_E=0.1,theta=0.47,
          
          lambda_IN_sd=0.54,q_sd=0.45,a_sd=0.06,E0_sd=100,IN0_sd=50,gamma_IH_sd=0.056,b_sd=0.359)



data_new %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_sd","N_RH_sd"),
    statenames=c
    ("time","S_sd","E_sd","Eq_sd","IN_sd","IH_sd","R_sd","N_IH_sd","IHL_sd","RH_sd","N_RH_sd")
    ,
    paramnames=c("rho","gamma_E","theta",
                 "lambda_IN_sd","q_sd","a_sd","E0_sd","IN0_sd","gamma_IH_sd","b_sd"),
    params=pars
  ) -> measSIR


K_sd<-pmcmc(measSIR, start=pars,
            Nmcmc = 1000000, Np = 2, tol=0,
            proposal=mvn.diag.rw(c(
              lambda_IN_sd=0.01,q_sd=0.01,a_sd=0.01,
              IN0_sd=5,E0_sd=5
            )))



> par_sd
loglik     log.prior         nfail           rho       gamma_E         theta  lambda_IN_sd 
-5.930068e+02 -8.179460e+00  0.000000e+00  6.800000e-01  1.000000e-01  4.700000e-01  4.831085e-01 
q_sd          a_sd         E0_sd        IN0_sd   gamma_IH_sd          b_sd 
6.849965e-03  1.951101e-01  3.962574e+01  6.128632e+01  5.600000e-02  3.590000e-01 

















  

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
  
  if(time>27){lambda_IN_dg1=lambda_IN_dg1*a;}
  
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
  R_dg = 3;

  

  
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


dp <- function (lambda_IN_dg,r_H,IN0_dg,E0_dg,a,log=TRUE,...) {
  f=dunif(lambda_IN_dg,0,1,log=log)+
    dunif(IN0_dg,0,122,log=log)+dunif(E0_dg,0,122*exp(1),log=log)+dunif(r_H,0,1,log=log)+dunif(a,0,1,log=log)
  return(f)
}

pars <- c(rho=0.71,gamma_E=0.1,theta=0.48,
          lambda_IN_dg=0.5,q_dg=0,E0_dg=122,IN0_dg=61,r_H=0.2,a=0.5)



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
    paramnames=c("rho","gamma_E","theta","r_H","a",
                 "lambda_IN_dg","q_dg","E0_dg","IN0_dg"),
    params=pars
  ) -> measSIR


K_dg<-pmcmc(measSIR, start=pars,
            Nmcmc = 1000000, Np = 2, tol=0,
            proposal=mvn.diag.rw(c(lambda_IN_dg=0.01,r_H=0.01,
                                   IN0_dg=5,E0_dg=5,a=0.01
            )))

> par_dg
loglik     log.prior         nfail           rho       gamma_E         theta  lambda_IN_dg 
-2191.8297936   -10.6080421     0.0000000     0.7100000     0.1000000     0.4800000     0.5152883 
q_dg         E0_dg        IN0_dg           r_H             a 
0.0000000   324.7395238     3.1190270     0.4760051     0.3761552 


plot(K_dg,pars=c("E0_dg","IN0_dg"))
plot(K_dg,pars=c("lambda_IN_dg","r_H"))
par_dg=apply(as.matrix(traces(K_dg)),2,mean)

measSIR %>%
  simulate(params=par_dg,nsim=1,format="data.frame") -> y_dg
plot(y_dg$IH_dg,type="o",xlab="date",ylab="cases_germany")
lines(data_dg1$cases_dg,col="red")


> p_dg
loglik     log.prior         nfail           rho       gamma_E         theta  lambda_IN_dg 
-4224.8344217   -10.6080421     0.0000000     0.7100000     0.1000000     0.4800000     0.5171379 
q_dg         E0_dg        IN0_dg           r_H             a 
0.0000000   330.5563851   108.3368133     0.5082419     0.3214383 



## fix E0 and IN_0
> p_dg
loglik     log.prior         nfail           rho       gamma_E         theta  lambda_IN_dg 
-4097.1040032   -11.5966347     0.0000000     0.7100000     0.1000000     0.4800000     0.4719712 
q_dg         E0_dg        IN0_dg           r_H             a 
0.0000000   540.5111449    10.4969941     0.3709594     0.2861426 





















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
  
    double r_s=1/5.3;
  
  double lambda_IN_dg1=lambda_IN_dg;
  
  if(time>27){lambda_IN_dg1=lambda_IN_dg1*a;}
  
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
  R_dg = 3;

  

  
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


dp <- function (lambda_IN_dg,r_H,IN0_dg,E0_dg,a,q_dg,r_q,log=TRUE,...) {
  f=dunif(lambda_IN_dg,0,1,log=log)+dunif(r_q,0,1,log=log)+dunif(q_dg,0,1,log=log)+
    dunif(IN0_dg,0,122,log=log)+dunif(E0_dg,0,122*exp(1),log=log)+dunif(r_H,0,1,log=log)+dunif(a,0,1,log=log)
  return(f)
}

pars <- c(rho=0.71,gamma_E=0.1,theta=0.48,r_q=0.5,
          lambda_IN_dg=0.6,q_dg=0.1,E0_dg=202,IN0_dg=20,r_H=0.4,a=0.3)



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
    paramnames=c("rho","gamma_E","theta","r_H","a","r_q",
                 "lambda_IN_dg","q_dg","E0_dg","IN0_dg"),
    params=pars
  ) -> measSIR


K_dg<-pmcmc(measSIR, start=pars,
            Nmcmc = 1000000, Np = 2, tol=0,
            proposal=mvn.diag.rw(c(lambda_IN_dg=0.01,r_H=0.01,
                                   IN0_dg=5,E0_dg=5,a=0.01,q_dg=0.01,r_q=0.01
            )))
