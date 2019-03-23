/**
 * Binary Raykar et al. notation translation
 *    Raykar et al  =  this model
 *    D_i == (x[i], y[i,])
 *    y^r_n == y[r,n]
 *    y_i == z[i]
 *
 * Raykar et al. specify priors:
 *    alpha[j] ~ beta(a1[j], a2[j]);
 *    beta[j] ~ beta(b1[j], b2[j]);
 *    w ~ multi_normal(rep_vector(0,D), inverse(Gamma));
 * but fail to mention values of a1, a2, b1, b2, Gamma
 *
 * applied by Poesio et al. to reviews on Amazon
 *   items are reviews
 *   heuristics act as "annotators"
 *
 */
data {
  int<lower=0> I;               // # items (reviews)
  int<lower=0> I_mis;           // # items that are not annotated
  int<lower=0> J;               // # annotators (raters / heuristics)
  //int<lower=0> J_mis;           // # annotators that did not rate imes
  int<lower=0> N;               // # annotations
  int<lower=0> D;               // # of predictors for each item
  matrix[I,D] x;                // [n,d] predictors d for item n
  int<lower=0,upper=1> y[N];    // observed annotations
  int<lower=1> ii[N];           // indicator to which item the n-th annotation belongs
  int<lower=1> ii_mis[I_mis];   // indicator of missing items
  int<lower=1> jj[N];           // indicator to which annotator the n-th annotation belongs 
  //int<lower=1> jj_mis[J_mis];   // indicator of missing annotator
}
parameters {
  // constrain sensitivity and specificity to be non-adversarial
  vector[D] w;                          // logistic regression coeffs
  real w0;                              // intercept

  vector<lower=0.1,upper=1>[J] alpha;   // sensitivity
  vector<lower=0.1,upper=1>[J] beta;    // specificity
}

model {
  // ITEM Side
  vector[I] logit_z_hat;
  vector[I] log_z_hat;   // log_z[n] = log Pr[z[n] == 1]
  vector[I] log1m_z_hat; // log_z[n] = log Pr[z[n] == 0]

  vector[J] log_alpha;
  vector[J] log1m_alpha;
  vector[J] log_beta;
  vector[J] log1m_beta;

  if(D == 0){
    logit_z_hat = rep_vector(w0, I);
  } else {
    logit_z_hat = w0 + x * w;
  }
  
  for (i in 1:I) {
    log_z_hat[i] = log_inv_logit(logit_z_hat[i]);
    log1m_z_hat[i] = log1m_inv_logit(logit_z_hat[i]);
  }

  // Annotator side
  for (j in 1:J) {
    log_alpha[j] = log(alpha[j]);
    log1m_alpha[j] = log1m(alpha[j]);
    log_beta[j] = log(beta[j]);
    log1m_beta[j] = log1m(beta[j]);
  }

  // priors
  w ~ normal(0,2);
  w0 ~ normal(0,5);
  alpha ~ beta(10,1);
  beta ~ beta(10,1);

  for (n in 1:N){
    real pos_sum;
    real neg_sum;
    pos_sum = log_z_hat[ii[n]];
    neg_sum = log1m_z_hat[ii[n]];
    if (y[n] == 1) {
        pos_sum = pos_sum + log_alpha[jj[n]];
        neg_sum = neg_sum + log1m_beta[jj[n]];
      } else {
        pos_sum = pos_sum + log1m_alpha[jj[n]];
        neg_sum = neg_sum + log_beta[jj[n]];
      }
      target += log_sum_exp(pos_sum, neg_sum);
  }

}

generated quantities {
  vector[I] E_z;
  {
    vector[I] logit_z_hat;
    vector[I] log_z_hat;   // posterior mean: log Pr[z[n] == 1]
    vector[I] log1m_z_hat; //                 log Pr[z[n] == 0]

    vector[J] log_alpha;
    vector[J] log1m_alpha;
    vector[J] log_beta;
    vector[J] log1m_beta;

    if(D == 0){
        logit_z_hat = rep_vector(w0, I);
      } else {
        logit_z_hat = w0 + x * w;
      }
    
    for (i in 1:I) {
      log_z_hat[i] = log_inv_logit(logit_z_hat[i]);
      log1m_z_hat[i] = log1m_inv_logit(logit_z_hat[i]);
    }

    for (j in 1:J) {
      log_alpha[j] = log(alpha[j]);
      log1m_alpha[j] = log1m(alpha[j]);
      log_beta[j] = log(beta[j]);
      log1m_beta[j] = log1m(beta[j]);
    }

   for (n in 1:N){
    real pos_sum;
    real neg_sum;
    real maxpn;
    pos_sum = log_z_hat[ii[n]];
    neg_sum = log1m_z_hat[ii[n]];
    if (y[n] == 1) {
        pos_sum = pos_sum + log_alpha[jj[n]];
        neg_sum = neg_sum + log1m_beta[jj[n]];
      } else {
        pos_sum = pos_sum + log1m_alpha[jj[n]];
        neg_sum = neg_sum + log_beta[jj[n]];
      }
    maxpn = fmax(pos_sum, neg_sum);
      pos_sum = pos_sum - maxpn;
      neg_sum = neg_sum - maxpn;
      E_z[ii[n]] = exp(pos_sum) / (exp(pos_sum) + exp(neg_sum));
  }
  for (m in 1:I_mis){
    real pos_sum;
    real neg_sum;
    real maxpn;
    pos_sum = log_z_hat[ii_mis[m]];
    neg_sum = log1m_z_hat[ii_mis[m]];
    maxpn = fmax(pos_sum, neg_sum);
      pos_sum = pos_sum - maxpn;
      neg_sum = neg_sum - maxpn;
      E_z[ii_mis[m]] = exp(pos_sum) / (exp(pos_sum) + exp(neg_sum));
  }
  
  
  }
}
