data {
    int<lower=0> N;   // number of youth
    real diff[N];        // treatment effect
}
parameters {
    real mu; 
    real<lower=0> sigma;
}
model {
    diff ~ normal(mu, sigma);
}