

set.seed(1122334455)
library(tidyverse)
library(pomp)
library(doRNG)

library(openxlsx)
read.xlsx("data_new.xlsx") %>%
  select(date, cases_bj	,recover_bj	,cases_sh,	recover_sh	,cases_gd	,recover_gd	,cases_zj,	recover_zj,	cases_cq,	recover_cq	,cases_hunan,	recover_hunan
  )-> data_new
p=nrow(data_new)
while(p>1){data_new[p,-1]=data_new[p,-1]-data_new[p-1,-1]
p=p-1}
data_new=data_new[-1,]


sir_step <- Csnippet("
  double N_gd=113460000;
  double N_bj=21540000;
  double N_sh=24240000;
  double N_zj=57370000;
  double N_cq=31020000;
  double N_hunan=68990000;
  
  
  double r_q=1/(10.9-4.3);
  double r_s=1/5.3;
  double r_H=1/(10.9-5.3);
  
  if(time>=7){r_q=1/3.6;}
  
  double gamma_IH_gd1=gamma_IH_gd;
  if(time<15){gamma_IH_gd1=gamma_IH_gd1*b_gd;}
  
  double lambda_IN_gd1=lambda_IN_gd;
  if(time>=7){lambda_IN_gd1=lambda_IN_gd*a_gd;}
  double lambda_E_gd=lambda_IN_gd1*theta;
  
  double dN_SE_gd =S_gd*(lambda_E_gd*E_gd+lambda_IN_gd1*IN_gd)/N_gd*dt;
  double dN_EEq_gd = E_gd*rho*q_gd*r_q*dt;
  double dN_EIN_gd = E_gd*rho*r_s*dt;
  double dN_EqIH_gd = Eq_gd*r_s*dt;
  double dN_INIH_gd = IN_gd*((1-q_gd)*r_H+q_gd*(r_H+r_q))*dt;
  double dN_ER_gd = E_gd*((1-rho)*(1-q_gd)*gamma_E+(1-rho)*q_gd*(r_q+gamma_E))*dt;
  double dN_IHLRH_gd=IHL_gd*gamma_IH_gd1*dt;
  
  S_gd += -dN_SE_gd;
  E_gd +=  dN_SE_gd-dN_EEq_gd-dN_EIN_gd-dN_ER_gd;
  Eq_gd += dN_EEq_gd-dN_EqIH_gd;
  IN_gd += -dN_INIH_gd+dN_EIN_gd;
  IH_gd += dN_EqIH_gd+dN_INIH_gd;
  R_gd += dN_ER_gd;
  N_IH_gd += dN_EqIH_gd+dN_INIH_gd;
  IHL_gd += (dN_EqIH_gd+dN_INIH_gd)-dN_IHLRH_gd;
  RH_gd += dN_IHLRH_gd;
  N_RH_gd += dN_IHLRH_gd;
  
  
  
  double gamma_IH_bj1=gamma_IH_bj;
  if(time<15){gamma_IH_bj1=gamma_IH_bj1*b_bj;}
  
  double lambda_IN_bj1=lambda_IN_bj;
  if(time>=7){lambda_IN_bj1=lambda_IN_bj*a_bj;}
  double lambda_E_bj=lambda_IN_bj1*theta;
  
  double dN_SE_bj =S_bj*(lambda_E_bj*E_bj+lambda_IN_bj1*IN_bj)/N_bj*dt;
  double dN_EEq_bj = E_bj*rho*q_bj*r_q*dt;
  double dN_EIN_bj = E_bj*rho*r_s*dt;
  double dN_EqIH_bj = Eq_bj*r_s*dt;
  double dN_INIH_bj = IN_bj*((1-q_bj)*r_H+q_bj*(r_H+r_q))*dt;
  double dN_ER_bj = E_bj*((1-rho)*(1-q_bj)*gamma_E+(1-rho)*q_bj*(r_q+gamma_E))*dt;
  double dN_IHLRH_bj=IHL_bj*gamma_IH_bj1*dt;
  
  S_bj += -dN_SE_bj;
  E_bj +=  dN_SE_bj-dN_EEq_bj-dN_EIN_bj-dN_ER_bj;
  Eq_bj += dN_EEq_bj-dN_EqIH_bj;
  IN_bj += -dN_INIH_bj+dN_EIN_bj;
  IH_bj += dN_EqIH_bj+dN_INIH_bj;
  R_bj += dN_ER_bj;
  N_IH_bj += dN_EqIH_bj+dN_INIH_bj;
  IHL_bj += (dN_EqIH_bj+dN_INIH_bj)-dN_IHLRH_bj;
  RH_bj += dN_IHLRH_bj;
  N_RH_bj += dN_IHLRH_bj;
  
  
  double gamma_IH_sh1=gamma_IH_sh;
  if(time<22){gamma_IH_sh1=gamma_IH_sh1*b_sh;}
  
  double lambda_IN_sh1=lambda_IN_sh;
  if(time>=7){lambda_IN_sh1=lambda_IN_sh*a_sh;}
  double lambda_E_sh=lambda_IN_sh1*theta;
  
  double dN_SE_sh =S_sh*(lambda_E_sh*E_sh+lambda_IN_sh1*IN_sh)/N_sh*dt;
  double dN_EEq_sh = E_sh*rho*q_sh*r_q*dt;
  double dN_EIN_sh = E_sh*rho*r_s*dt;
  double dN_EqIH_sh = Eq_sh*r_s*dt;
  double dN_INIH_sh = IN_sh*((1-q_sh)*r_H+q_sh*(r_H+r_q))*dt;
  double dN_ER_sh = E_sh*((1-rho)*(1-q_sh)*gamma_E+(1-rho)*q_sh*(r_q+gamma_E))*dt;
  double dN_IHLRH_sh=IHL_sh*gamma_IH_sh1*dt;
  
  S_sh += -dN_SE_sh;
  E_sh +=  dN_SE_sh-dN_EEq_sh-dN_EIN_sh-dN_ER_sh;
  Eq_sh += dN_EEq_sh-dN_EqIH_sh;
  IN_sh += -dN_INIH_sh+dN_EIN_sh;
  IH_sh += dN_EqIH_sh+dN_INIH_sh;
  R_sh += dN_ER_sh;
  N_IH_sh += dN_EqIH_sh+dN_INIH_sh;
  IHL_sh += (dN_EqIH_sh+dN_INIH_sh)-dN_IHLRH_sh;
  RH_sh += dN_IHLRH_sh;
  N_RH_sh += dN_IHLRH_sh;
  
  
  
  double gamma_IH_zj1=gamma_IH_zj;
  if(time<15){gamma_IH_zj1=gamma_IH_zj1*b_zj;}
  
  double lambda_IN_zj1=lambda_IN_zj;
  if(time>=7){lambda_IN_zj1=lambda_IN_zj*a_zj;}
  double lambda_E_zj=lambda_IN_zj1*theta;
  
  double dN_SE_zj =S_zj*(lambda_E_zj*E_zj+lambda_IN_zj1*IN_zj)/N_zj*dt;
  double dN_EEq_zj = E_zj*rho*q_zj*r_q*dt;
  double dN_EIN_zj = E_zj*rho*r_s*dt;
  double dN_EqIH_zj = Eq_zj*r_s*dt;
  double dN_INIH_zj = IN_zj*((1-q_zj)*r_H+q_zj*(r_H+r_q))*dt;
  double dN_ER_zj = E_zj*((1-rho)*(1-q_zj)*gamma_E+(1-rho)*q_zj*(r_q+gamma_E))*dt;
  double dN_IHLRH_zj=IHL_zj*gamma_IH_zj1*dt;
  
  S_zj += -dN_SE_zj;
  E_zj +=  dN_SE_zj-dN_EEq_zj-dN_EIN_zj-dN_ER_zj;
  Eq_zj += dN_EEq_zj-dN_EqIH_zj;
  IN_zj += -dN_INIH_zj+dN_EIN_zj;
  IH_zj += dN_EqIH_zj+dN_INIH_zj;
  R_zj += dN_ER_zj;
  N_IH_zj += dN_EqIH_zj+dN_INIH_zj;
  IHL_zj += (dN_EqIH_zj+dN_INIH_zj)-dN_IHLRH_zj;
  RH_zj += dN_IHLRH_zj;
  N_RH_zj += dN_IHLRH_zj;
  
  
  
  double gamma_IH_cq1=gamma_IH_cq;
  if(time<20){gamma_IH_cq1=gamma_IH_cq1*b_cq;}
  
  double lambda_IN_cq1=lambda_IN_cq;
  if(time>=7){lambda_IN_cq1=lambda_IN_cq*a_cq;}
  double lambda_E_cq=lambda_IN_cq1*theta;
  
  double dN_SE_cq =S_cq*(lambda_E_cq*E_cq+lambda_IN_cq1*IN_cq)/N_cq*dt;
  double dN_EEq_cq = E_cq*rho*q_cq*r_q*dt;
  double dN_EIN_cq = E_cq*rho*r_s*dt;
  double dN_EqIH_cq = Eq_cq*r_s*dt;
  double dN_INIH_cq = IN_cq*((1-q_cq)*r_H+q_cq*(r_H+r_q))*dt;
  double dN_ER_cq = E_cq*((1-rho)*(1-q_cq)*gamma_E+(1-rho)*q_cq*(r_q+gamma_E))*dt;
  double dN_IHLRH_cq=IHL_cq*gamma_IH_cq1*dt;
  
  S_cq += -dN_SE_cq;
  E_cq +=  dN_SE_cq-dN_EEq_cq-dN_EIN_cq-dN_ER_cq;
  Eq_cq += dN_EEq_cq-dN_EqIH_cq;
  IN_cq += -dN_INIH_cq+dN_EIN_cq;
  IH_cq += dN_EqIH_cq+dN_INIH_cq;
  R_cq += dN_ER_cq;
  N_IH_cq += dN_EqIH_cq+dN_INIH_cq;
  IHL_cq += (dN_EqIH_cq+dN_INIH_cq)-dN_IHLRH_cq;
  RH_cq += dN_IHLRH_cq;
  N_RH_cq += dN_IHLRH_cq;
  
  
  
  double gamma_IH_hunan1=gamma_IH_hunan;
  if(time<20){gamma_IH_hunan1=gamma_IH_hunan1*b_hunan;}
  
  double lambda_IN_hunan1=lambda_IN_hunan;
  if(time>=7){lambda_IN_hunan1=lambda_IN_hunan*a_hunan;}
  double lambda_E_hunan=lambda_IN_hunan1*theta;
  
  double dN_SE_hunan =S_hunan*(lambda_E_hunan*E_hunan+lambda_IN_hunan1*IN_hunan)/N_hunan*dt;
  double dN_EEq_hunan = E_hunan*rho*q_hunan*r_q*dt;
  double dN_EIN_hunan = E_hunan*rho*r_s*dt;
  double dN_EqIH_hunan = Eq_hunan*r_s*dt;
  double dN_INIH_hunan = IN_hunan*((1-q_hunan)*r_H+q_hunan*(r_H+r_q))*dt;
  double dN_ER_hunan = E_hunan*((1-rho)*(1-q_hunan)*gamma_E+(1-rho)*q_hunan*(r_q+gamma_E))*dt;
  double dN_IHLRH_hunan=IHL_hunan*gamma_IH_hunan1*dt;
  
  S_hunan += -dN_SE_hunan;
  E_hunan +=  dN_SE_hunan-dN_EEq_hunan-dN_EIN_hunan-dN_ER_hunan;
  Eq_hunan += dN_EEq_hunan-dN_EqIH_hunan;
  IN_hunan += -dN_INIH_hunan+dN_EIN_hunan;
  IH_hunan += dN_EqIH_hunan+dN_INIH_hunan;
  R_hunan += dN_ER_hunan;
  N_IH_hunan += dN_EqIH_hunan+dN_INIH_hunan;
  IHL_hunan += (dN_EqIH_hunan+dN_INIH_hunan)-dN_IHLRH_hunan;
  RH_hunan += dN_IHLRH_hunan;
  N_RH_hunan += dN_IHLRH_hunan;
  
  
  time += dt;
")

sir_init <- Csnippet("

  
  S_gd = 113460000;
  E_gd = E0_gd;
  Eq_gd = 0;
  IN_gd = IN0_gd;
  IH_gd = 0;
  R_gd = 0;
  IHL_gd=32;
  RH_gd=0;
  
  S_bj = 21540000;
  E_bj = E0_bj;
  Eq_bj = 0;
  IN_bj = IN0_bj;
  IH_bj = 0;
  R_bj = 0;
  IHL_bj=14;
  RH_bj=0;
  
  S_sh = 24240000;
  E_sh = E0_sh;
  Eq_sh = 0;
  IN_sh = IN0_sh;
  IH_sh = 0;
  R_sh = 0;
  IHL_sh=16;
  RH_sh=0;
  
  S_zj = 57370000;
  E_zj = E0_zj;
  Eq_zj = 0;
  IN_zj = IN0_zj;
  IH_zj = 0;
  R_zj = 0;
  IHL_zj=9;
  RH_zj=0;
  

  S_cq = 31020000;
  E_cq = E0_cq;
  Eq_cq = 0;
  IN_cq = IN0_cq;
  IH_cq = 0;
  R_cq = 0;
  IHL_cq=9;
  RH_cq=0;
  
  
  
  S_hunan =68990000;
  E_hunan = E0_hunan;
  Eq_hunan = 0;
  IN_hunan = IN0_hunan;
  IH_hunan = 0;
  R_hunan = 0;
  IHL_hunan=9;
  RH_hunan=0;
  
  time=0;
")


dmeas <-  Csnippet("
  double f;
    f = dpois(nearbyint(cases_gd),N_IH_gd,1)+dpois(nearbyint(recover_gd),N_RH_gd,1)+
    dpois(nearbyint(cases_bj),N_IH_bj,1)+dpois(nearbyint(recover_bj),N_RH_bj,1)+
    dpois(nearbyint(cases_sh),N_IH_sh,1)+dpois(nearbyint(recover_sh),N_RH_sh,1)+
    dpois(nearbyint(cases_zj),N_IH_zj,1)+dpois(nearbyint(recover_zj),N_RH_zj,1)+
    dpois(nearbyint(cases_cq),N_IH_cq,1)+dpois(nearbyint(recover_cq),N_RH_cq,1)+
    dpois(nearbyint(cases_hunan),N_IH_hunan,1)+dpois(nearbyint(recover_hunan),N_RH_hunan,1);
  lik = (give_log) ? f : exp(f);

")

rmeas <- Csnippet("
    cases_gd = rpois(N_IH_gd);
    recover_gd = rpois(N_RH_gd);
    cases_bj = rpois(N_IH_bj);
    recover_bj = rpois(N_RH_bj);
    cases_sh = rpois(N_IH_sh);
    recover_sh = rpois(N_RH_sh);
    cases_zj = rpois(N_IH_zj);
    recover_zj = rpois(N_RH_zj);
    cases_cq = rpois(N_IH_cq);
    recover_cq = rpois(N_RH_cq);
    cases_hunan = rpois(N_IH_hunan);
    recover_hunan = rpois(N_RH_hunan);
  ")


dp <- function (rho,theta,
                a_gd,lambda_IN_gd,q_gd,gamma_IH_gd,b_gd,a_bj,lambda_IN_bj,q_bj,gamma_IH_bj,b_bj,
                a_sh,lambda_IN_sh,q_sh,gamma_IH_sh,b_sh,a_zj,lambda_IN_zj,q_zj,gamma_IH_zj,b_zj,
                a_cq,lambda_IN_cq,q_cq,gamma_IH_cq,b_cq,
                a_hunan,lambda_IN_hunan,q_hunan,gamma_IH_hunan,b_hunan,
                IN0_hunan,E0_hunan,IN0_cq,E0_cq,IN0_zj,E0_zj,IN0_sh,E0_sh,IN0_bj,E0_bj,IN0_gd,E0_gd,log=TRUE,...) {
  f=dunif(rho,0,1,log=log)+dunif(theta,0,1,log=log)+
    dunif(a_gd,0,0.2,log=log)+dunif(lambda_IN_gd,0,0.7,log=log)+
    dunif(q_gd,0,1,log=log)+
    dunif(a_bj,0,0.2,log=log)+dunif(lambda_IN_bj,0,0.7,log=log)+
    dunif(q_bj,0,1,log=log)+
    dunif(a_sh,0,0.2,log=log)+dunif(lambda_IN_sh,0,0.7,log=log)+
    dunif(q_sh,0,1,log=log)+
    dunif(a_zj,0,0.2,log=log)+dunif(lambda_IN_zj,0,0.7,log=log)+
    dunif(q_zj,0,1,log=log)+
    dunif(a_cq,0,0.2,log=log)+dunif(lambda_IN_cq,0,0.7,log=log)+
    dunif(q_cq,0,1,log=log)+
    dunif(a_hunan,0,0.2,log=log)+dunif(lambda_IN_hunan,0,0.7,log=log)+
    dunif(q_hunan,0,1,log=log)+
    dunif(IN0_hunan,0,134,log=log)+dunif(E0_hunan,0,134*exp(1),log=log)+
    dunif(IN0_cq,0,101,log=log)+dunif(E0_cq,0, 101*exp(1),log=log)+
    dunif(IN0_zj,0,163,log=log)+dunif(E0_zj,0, 163*exp(1),log=log)+
    dunif(IN0_sh,0,50,log=log)+dunif(E0_sh,0, 50*exp(1),log=log)+
    dunif(IN0_bj,0,66,log=log)+dunif(E0_bj,0, 66*exp(1),log=log)+
    dunif(IN0_gd,0,156,log=log)+dunif(E0_gd,0, 156*exp(1),log=log)
  
  return(f)
}

pars <- c(rho=0.75,gamma_E=1.0/7.0,theta=0.53,
          lambda_IN_gd=0.54,q_gd=0.45,a_gd=0.06,E0_gd=160,IN0_gd=77,gamma_IH_gd=0.0511,b_gd=0.1542,
          lambda_IN_bj=0.43,q_bj=0.37,a_bj=0.1,E0_bj=45,IN0_bj=45,gamma_IH_bj=0.0367,b_bj=0.4433,
          lambda_IN_sh=0.4,q_sh=0.48,a_sh=0.1,E0_sh=75,IN0_sh=22,gamma_IH_sh= 0.1174,b_sh=0.1492,
          lambda_IN_zj=0.37,q_zj=0.52,a_zj=0.07,E0_zj=360,IN0_zj=78,gamma_IH_zj= 0.0535,b_zj=0.2625,
          lambda_IN_cq=0.39,q_cq=0.35,a_cq=0.11,E0_cq=70,IN0_cq=80,gamma_IH_cq=0.0709,b_cq=0.2193,
          lambda_IN_hunan=0.45,q_hunan=0.39,a_hunan=0.024,E0_hunan=240,IN0_hunan=33,gamma_IH_hunan=0.0787,b_hunan=0.3995
)



data_new %>%
  pomp(
    times="date",t0=0,
    rprocess=euler(sir_step,delta.t=1/10),
    rinit=sir_init,
    rmeasure=rmeas,
    dmeasure=dmeas,
    dprior=dp,
    accumvars=c("N_IH_gd","N_RH_gd","N_IH_bj","N_RH_bj","N_IH_sh","N_RH_sh","N_IH_zj","N_RH_zj","N_IH_cq","N_RH_cq","N_IH_hunan","N_RH_hunan"),
    statenames=c("time","S_gd","E_gd","Eq_gd","IN_gd","IH_gd","R_gd","N_IH_gd","IHL_gd","RH_gd","N_RH_gd",
                 "S_bj","E_bj","Eq_bj","IN_bj","IH_bj","R_bj","N_IH_bj","IHL_bj","RH_bj","N_RH_bj",
                 "S_sh","E_sh","Eq_sh","IN_sh","IH_sh","R_sh","N_IH_sh","IHL_sh","RH_sh","N_RH_sh",
                 "S_zj","E_zj","Eq_zj","IN_zj","IH_zj","R_zj","N_IH_zj","IHL_zj","RH_zj","N_RH_zj",
                 "S_cq","E_cq","Eq_cq","IN_cq","IH_cq","R_cq","N_IH_cq","IHL_cq","RH_cq","N_RH_cq",
                 "S_hunan","E_hunan","Eq_hunan","IN_hunan","IH_hunan","R_hunan","N_IH_hunan","IHL_hunan","RH_hunan","N_RH_hunan"),
    paramnames=c("rho","gamma_E","theta",
                 "lambda_IN_gd","q_gd","a_gd","E0_gd","IN0_gd","gamma_IH_gd","b_gd",
                 "lambda_IN_bj","q_bj","a_bj","E0_bj","IN0_bj","gamma_IH_bj","b_bj",
                 "lambda_IN_sh","q_sh","a_sh","E0_sh","IN0_sh","gamma_IH_sh","b_sh",
                 "lambda_IN_zj","q_zj","a_zj","E0_zj","IN0_zj","gamma_IH_zj","b_zj",
                 "lambda_IN_cq","q_cq","a_cq","E0_cq","IN0_cq","gamma_IH_cq","b_cq",
                 "lambda_IN_hunan","q_hunan","a_hunan","E0_hunan","IN0_hunan","gamma_IH_hunan","b_hunan"),
    params=pars
  ) -> measSIR


K<-pmcmc(measSIR, start=pars,
         Nmcmc = 3000000, Np = 2, tol=0,
         proposal=mvn.diag.rw(c(rho=0.01,theta=0.01,
                                lambda_IN_gd=0.01,q_gd=0.01,a_gd=0.01,
                                lambda_IN_bj=0.01,q_bj=0.01,a_bj=0.01,
                                lambda_IN_sh=0.01,q_sh=0.01,a_sh=0.01,
                                lambda_IN_zj=0.01,q_zj=0.01,a_zj=0.01,
                                lambda_IN_cq=0.01,q_cq=0.01,a_cq=0.01,
                                lambda_IN_hunan=0.01,q_hunan=0.01,a_hunan=0.01,
                                IN0_gd=5,IN0_bj=5,IN0_sh=5,IN0_cq=5,IN0_hunan=5,IN0_zj=5,
                                E0_gd=5,E0_bj=5,E0_sh=5,E0_zj=5,E0_hunan=5,E0_cq=5
         )))


