data {
  int<lower=0> I;               // # items (reviews)
  int<lower=0> I_mis;           // # items that are not annotated
  int<lower=0> J;               // # annotators (raters / heuristics)
  int<lower=0> N;               // # annotations
  int<lower=0> D;               // # of predictors for each item
  matrix[I,D] x;                // [n,d] predictors d for item n
  //int<lower=0,upper=1> y[N];    // observed annotations
  int<lower=1> ii[N];           // indicator to which item the n-th annotation belongs
  int<lower=1> ii_mis[I_mis];   // indicator of missing items
  int<lower=1> jj[N];           // indicator to which annotator the n-th annotation belongs 
}


generated quantities {
  vector[D] w;   // logistic regression coeffs
  real w0 = normal_rng(0, 5);          // intercept
  vector<lower=0.1,upper=1>[J] alpha;   // sensitivity
  vector<lower=0.1,upper=1>[J] beta;    // specificity
  int<lower=0, upper=1> z[N];
  int<lower=0, upper=1> y[N];
  
  for(j in 1:J){
    alpha[j] = beta_rng(10, 1);
    beta[j] = beta_rng(10, 1);
  }
  
  for(d in 1:D)
    w[d] = normal_rng(0, 2);       
  
    for(n in 1:N)
      z[n] = bernoulli_logit_rng(w0 + x[ii[n]] * w);
    
    for(n in 1:N){
      if(z[n] == 1){
        y[n] = bernoulli_rng(alpha[jj[n]]);
      } else {
        y[n] = bernoulli_rng((1-beta[jj[n]]));
      }
    }
    
}
