

set.seed(1122334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("data_new.xlsx") %>%
  select(date, cases_bj	,recover_bj	,cases_sh,	recover_sh	,cases_gd	,recover_gd	,cases_zj,	recover_zj,	cases_cq,	recover_cq	,cases_hunan,	recover_hunan
  )-> data_new
read.xlsx("data_new.xlsx") %>%
  select(date, cases_bj	,recover_bj	,cases_sh,	recover_sh	,cases_gd	,recover_gd	,cases_zj,	recover_zj,	cases_cq,	recover_cq	,cases_hunan,	recover_hunan
  )-> data_new1
p=nrow(data_new)
while(p>1){data_new[p,-1]=data_new[p,-1]-data_new[p-1,-1]
p=p-1}
data_new=data_new[-1,]
data_new1=data_new1[-1,]
data_new1[,2]=data_new1[,2]-data_new1[,3]
data_new1[,4]=data_new1[,4]-data_new1[,5]
data_new1[,6]=data_new1[,6]-data_new1[,7]
data_new1[,8]=data_new1[,8]-data_new1[,9]
data_new1[,10]=data_new1[,10]-data_new1[,11]
data_new1[,12]=data_new1[,12]-data_new1[,13]
data_new[,c(2,4,6,8,10,12)]=data_new1[,c(2,4,6,8,10,12)]

sir_step <- Csnippet("
  
gamma_gd=gamma_IH_gd;
gamma_sh=gamma_IH_sh;
gamma_bj=gamma_IH_bj;
gamma_cq=gamma_IH_cq;
gamma_zj=gamma_IH_zj;
gamma_hunan=gamma_IH_hunan;

if(time<15){gamma_gd=gamma_gd*b_gd;}
if(time<15){gamma_bj=gamma_bj*b_bj;}
if(time<15){gamma_zj=gamma_zj*b_zj;}
if(time<22){gamma_sh=gamma_sh*b_sh;}
if(time<20){gamma_cq=gamma_cq*b_cq;}
if(time<20){gamma_hunan=gamma_hunan*b_hunan;}

  time += dt;
")

sir_init <- Csnippet("
  time=0;
")


dmeas <-  Csnippet("
  double f;
  f = dpois(nearbyint(recover_gd),cases_gd*gamma_gd,1)+
  dpois(nearbyint(recover_bj),cases_bj*gamma_bj,1)+
  dpois(nearbyint(recover_sh),cases_sh*gamma_sh,1)+
  dpois(nearbyint(recover_cq),cases_cq*gamma_cq,1)+
  dpois(nearbyint(recover_zj),cases_zj*gamma_zj,1)+
  dpois(nearbyint(recover_hunan),cases_hunan*gamma_hunan,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    recover_gd = rpois(cases_gd*gamma_gd);
    recover_bj = rpois(cases_bj*gamma_bj);
    recover_sh = rpois(cases_sh*gamma_sh);
    recover_zj = rpois(cases_zj*gamma_zj);
    recover_cq = rpois(cases_cq*gamma_cq);
    recover_hunan = rpois(cases_hunan*gamma_hunan);
  ")


dp <- function (
              gamma_IH_bj,b_bj,gamma_IH_zj,b_zj,
              gamma_IH_gd,b_gd,gamma_IH_cq,b_cq,
              gamma_IH_sh,b_sh,gamma_IH_hunan,b_hunan,log=TRUE,...) {
  f=dunif(gamma_IH_gd,1/30,1,log=log)+dunif(b_gd,0,1,log=log)+
    dunif(gamma_IH_bj,1/30,1,log=log)+dunif(b_bj,0,1,log=log)+
    dunif(gamma_IH_sh,1/30,1,log=log)+dunif(b_sh,0,1,log=log)+
    dunif(gamma_IH_zj,1/30,1,log=log)+dunif(b_zj,0,1,log=log)+
    dunif(gamma_IH_cq,1/30,1,log=log)+dunif(b_cq,0,1,log=log)+
    dunif(gamma_IH_hunan,1/30,1,log=log)+dunif(b_hunan,0,1,log=log)
  return(f)
}

pars <- c(gamma_IH_gd=0.047,b_gd=0.17,gamma_IH_bj=0.037,b_bj=0.5,gamma_IH_sh=0.11,b_sh=0.15,
          gamma_IH_zj=0.048,b_zj=0.3,gamma_IH_cq=0.067,b_cq=0.24,gamma_IH_hunan=0.071,b_hunan=0.44)



data_new %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    statenames=c("time","gamma_gd","gamma_bj","gamma_sh","gamma_cq","gamma_zj","gamma_hunan"),
    paramnames=c("gamma_IH_gd","b_gd","gamma_IH_bj","b_bj",
                 "gamma_IH_sh","b_sh","gamma_IH_zj","b_zj",
                 "gamma_IH_cq","b_cq","gamma_IH_hunan","b_hunan"),
    params=pars
  ) -> measSIR


K1<-pmcmc(measSIR, start=pars,
         Nmcmc = 500000, Np = 2, tol=0,
         proposal=mvn.diag.rw(c(gamma_IH_gd=0.01,b_gd=0.01,gamma_IH_sh=0.01,b_sh=0.01,
                                gamma_IH_zj=0.01,b_zj=0.01,gamma_IH_bj=0.01,b_bj=0.01,
                                gamma_IH_cq=0.01,b_cq=0.01,gamma_IH_hunan=0.01,b_hunan=0.01
         )))

