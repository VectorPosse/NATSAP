//This is the stan model for non-hierarchical t-test
data {
    int<lower=0> nSubj; //number of subjects
    real diff[nSubj];   //treatment effect
}
parameters {
    real mu; 
    real<lower=0> sigma;
}
model {
    diff ~ normal(mu, sigma);
}