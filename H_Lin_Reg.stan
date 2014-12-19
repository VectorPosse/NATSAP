//This is the stan model for heirarchical linear regression
data {
  int<lower=0> nSubj;          //number of individuals
  int<lower=0> nGroup;         //number of groups
  int<lower=0> nIPred;         //number of individual level predictors
  int<lower=0> nGPred;         //number of group level predictors
  int groupId[nSubj];          //group per individual
  matrix[nSubj,nIPred] IPred;  //individual level predictors
  matrix[nGroup,nGPred] GPred; //group level predictors
  real diff[nSubj];            //treatment effects
}

parameters {
  corr_matrix[nIPred] Omega;    //prior covariance 
  vector<lower=0>[nIPred] tau;  //prior scale
  matrix[nGPred, nIPred] gamma; //group coefficients
  vector[nIPred] beta[nGroup];  //individual coefficients by group
  real<lower=0> sigma;          //prediction error scale
}
//transformed parameters{
//  matrix[nIPred, nIPred] Sigma_beta;
//  Sigma_beta <- diag_matrix(tau)*Omega*diag_matrix(tau);
//}
model {
  matrix[nIPred, nIPred] Sigma_beta;
  Sigma_beta <- diag_matrix(tau)*Omega*diag_matrix(tau);

  tau ~ cauchy(0,2.5);
  Omega ~ lkj_corr(2);

  for (gPred in 1:nGPred)
    gamma[gPred] ~ normal(0,1000);

  for (group in 1:nGroup)
    beta[group] ~ multi_normal((GPred[group]*gamma)', Sigma_beta);
  {
  vector[nSubj] IPred_beta_groupId;
  for(subj in 1:nSubj)
    IPred_beta_groupId[subj] <- IPred[subj]*beta[groupId[subj]];
  diff ~ normal(IPred_beta_groupId, sigma);
  }
}