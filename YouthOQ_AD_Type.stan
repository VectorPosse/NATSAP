//This is the stan model for the heirarchical t-tests by Type
data {
  int<lower=0> nSubj; //number of subjects
  int<lower=0> nType; //number of programs
  real diff[nSubj];   // treatment effect
  int TypeId[nSubj];  //program per subject
}
transformed data {
  real muG;
  real<lower=0> sigmaG;
  muG <- 0;
  sigmaG <- 1000;
}
parameters {
  vector[nType] mu; 
  real<lower=0> sigma;
}
model {
  mu ~ normal(muG, sigmaG);
  for(subjIdx in 1:nSubj)
  {
    diff[subjIdx] ~ normal( mu[TypeId[subjIdx]] , sigma );
  }
}