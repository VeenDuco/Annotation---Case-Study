data {
  int<lower=0> I;               // # items (reviews)
  int<lower=0> N;               // # annotations
  int<lower=0> D;               // # of predictors for each item
  matrix[I,D] x;                // [n,d] predictors d for item n
  int<lower=1> ii[N];           // indicator to which item the n-th annotation belongs
}

generated quantities {
  vector[D] w;   // logistic regression coeffs
  real w0 = normal_rng(0, 5);          // intercept
  int<lower=0, upper=1> y[N];
  
  for(d in 1:D)
    w[d] = normal_rng(0, 2);       
  
  for(n in 1:N)
      y[n] = bernoulli_logit_rng(w0 + x[ii[n]] * w);
    
}
