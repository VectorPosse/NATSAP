//This is the stan model for the heirarchical t-tests by Gender
data {
  int<lower=0> nSubj; //number of subjects
  int<lower=0> nGender; //number of programs
  real diff[nSubj];   // treatment effect
  int GenderId[nSubj];  //program per subject
}
transformed data {
  real muG;
  real<lower=0> sigmaG;
  muG <- 0;
  sigmaG <- 1000;
}
parameters {
  vector[nGender] mu; 
  real<lower=0> sigma;
}
model {
  mu ~ normal(muG, sigmaG);
  for(subjIdx in 1:nSubj)
  {
    diff[subjIdx] ~ normal( mu[GenderId[subjIdx]] , sigma );
  }
}