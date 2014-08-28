data {
    int<lower=0> nSubj; //number of subjects
    int<lower=0> nProg; //number of programs
    real diff[nSubj];   // treatment effect
    int ProgId[nSubj];    //program per subject
}
transformed data {
    real muG;
    real<lower=0> sigmaG;
    muG <- 0;
    sigmaG <- 50;
}
parameters {
    vector[nProg] mu; 
    real<lower=0> sigma;
}
model {
    mu ~ normal(muG, sigmaG);
  for(subjIdx in 1:nSubj)
  {
    // ERROR: The Prog vector returns the id of the program
    // but the vector mu[] can only handle inputs from 1-30
    // so it fails when Prog[subjIdx] = 110, for example
      diff[subjIdx] ~ normal( mu[ProgId[subjIdx]] , sigma );
  }
}