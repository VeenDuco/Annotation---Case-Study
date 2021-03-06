# logistic regression SBC:
library(rstan)
library(foreach)
library(parallel)
library(doParallel)
library(bayesplot)
library(tcltk)
model <- stan_model("SIMULATIONs/MODELS/estimation_logistic_step.stan")
gen_model <- stan_model("SIMULATIONs/MODELS/generation_logistic_step.stan")
detectCores()
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
# define functions
data_structure <- function(I, J, D, missing){
  x <- matrix(rnorm(I * D), I, D)
  item <- rep(1:I, J)
  annotator <- rep(1:J, each = I)
  
  sim.data <- data.frame(item, annotator)
  sim.data.missing <- sim.data[-sample(1:nrow(sim.data), nrow(sim.data) * missing), ]
  N <- nrow(sim.data.missing)
  
  return(list(I = I, missing = missing, J = J, N = N, x = x, D = D,
              I_mis = length((1:I)[-unique(sim.data.missing$item)]),
              J_mis = length((1:J)[-unique(sim.data.missing$annotator)]),
              ii = sim.data.missing$item,
              ii_missing = as.array((1:I)[-unique(sim.data.missing$item)]),
              jj = sim.data.missing$annotator,
              jj_missing = as.array((1:J)[-unique(sim.data.missing$annotator)])
  ))
}

sbc_rank <- function(ranks, reps, bins, title = NULL){
  
  rank_df = data.frame(ranks = ranks)
  
  lb <- qbinom(.005, reps, (bins+1)^-1)
  mean <- qbinom(.5, reps, (bins+1)^-1)
  ub <- qbinom(.995, reps, (bins+1)^-1)
  
  ggplot(rank_df, aes(x = ranks))  + 
    geom_segment(aes(x = -0.5, y = mean, xend = bins - 0.5, yend = mean), colour = "grey25") + 
    geom_polygon(data = data.frame(x = c(-1, -0.5, -1, bins, bins - 0.5, bins),
                                   y=c(lb, mean, ub, ub, mean, lb)), 
                 aes(x = x, y = y), fill="grey45", color="grey25", alpha=0.5) +
    geom_histogram(binwidth = 1, fill="#A25050",colour="black") + 
    theme_default() + xlab("Rank Statistic") + ylab("") +
    if(!is.null(title)) ggtitle(title) else ggtitle("")
  
}

# get data (structure)
set.seed(23032019)
sim.data <- data_structure(J = 10,
                           I = 100,
                           D = 10,
                           missing = .5)

warm.init <- 1000
reps <- 3000
bins <- 19

prior.sample <- sampling(gen_model, data = list(x = sim.data$x,
                                                I = sim.data$I,
                                                I_mis = sim.data$I_mis,
                                                N = sim.data$N,
                                                D = sim.data$D,
                                                ii = sim.data$ii,
                                                ii_mis = sim.data$ii_mis),
                         algorithm = "Fixed_param",
                         seed = 25032019, chains = 1,
                         warmup = 0, iter = reps)

prior.w0 <- extract(prior.sample, pars = "w0")$'w0'
prior.w <- extract(prior.sample, pars = "w")$'w'
prior.data <- extract(prior.sample, pars = "y")$'y'

finalMatrix  <- foreach(i = 1:reps, .packages = c("rstan", "tcltk"), 
                        .export = c("sim.data", "bins", "warm.init", 
                                    "prior.w0", "prior.w", "prior.data", "reps"),
                        .combine = rbind,
                        .verbose = TRUE) %dopar% {
                          
                          if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=reps)
                          setTkProgressBar(pb, i)  
                          
                          w_init <- rnorm(sim.data$D)
                          w0_init <- rnorm(1)
                          
                          init_fun <- function(n) {
                            list(w = w_init,
                                 w0 = w0_init)
                          }
                          
                          thin <- 1
                          draws <- bins
                          warm <- warm.init
                          fit <- sampling(model, chains = 1, warmup = warm, iter = warm + draws, 
                                          data = list(J = sim.data$J,
                                                      I = sim.data$I,
                                                      I_mis = sim.data$I_mis,
                                                      D = sim.data$D,
                                                      N = sim.data$N,
                                                      x = sim.data$x,
                                                      ii = sim.data$ii,
                                                      ii_mis = sim.data$ii_mis,
                                                      jj = sim.data$jj,
                                                      y = prior.data[i, ]))
                          
                          
                          while(min(summary(fit)$summary[, "n_eff"]) < bins) {
                            thin <- thin * 2
                            draws <- draws * 2
                            warm <- warm * 1.5
                            fit <- sampling(model, chains = 1, warmup = warm, iter = warm + draws, 
                                            data = list(J = sim.data$J,
                                                        I = sim.data$I,
                                                        I_mis = sim.data$I_mis,
                                                        D = sim.data$D,
                                                        N = sim.data$N,
                                                        x = sim.data$x,
                                                        ii = sim.data$ii,
                                                        ii_mis = sim.data$ii_mis,
                                                        jj = sim.data$jj,
                                                        y = prior.data[i, ]))
                          }
                          
                          
                          fit.w0 <- extract(fit, pars = "w0")$'w0'
                          fit.w <- extract(fit, pars = "w")$'w'
                          
                          fit.w0 <- fit.w0[(1:bins) * thin]
                          fit.w <- fit.w[(1:bins) * thin, ]
                          
                          tempMatrix <- matrix(NA, ncol = (1 + sim.data$D), nrow = 1)
                          
                          tempMatrix[1, 1] <- sum(fit.w0 < prior.w0[i])
                          
                          if(sim.data$D > 0){
                            for(d in 1:sim.data$D) {
                              tempMatrix[1, 1 + d] <- sum(fit.w[, d] < prior.w[i, d])
                            }
                          }
                          
                          tempMatrix
                          
                        }

rank.w0 <- finalMatrix[, 1]
rank.w <- finalMatrix[, 2:(1 + sim.data$D)]


pdf(paste0("SIMULATIONS/RESULTS/FIGURES/logistic_regression", 
           "I", sim.data$I, "J", sim.data$J, "D", sim.data$D, 
           "missing", sim.data$missing * 100, 
           "Bins", bins, "replications", reps, "_ggplot.pdf"))

par(mfrow = c(1, 1))
plot(NA, xlim = c(0, 5), ylim = c(0, 6), bty = 'n',
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
text(1, 5, "Simulation Based Calibration Based On:", pos = 4)
text(1, 4, paste0("J = ", sim.data$J), pos = 4)
text(1, 3, paste0("I = ", sim.data$I), pos = 4)
text(1, 2, paste0("D = ", sim.data$D), pos = 4)
text(1, 1, paste0("missing = ", sim.data$missing * 100, "%"), pos = 4)
points(rep(1,4),1:4, pch=15)

sbc_rank(rank.w0, reps = reps, bins = bins + 1, title = "Intercept")

for(d in 1:sim.data$D){
  print(sbc_rank(rank.w[, d], reps = reps, bins = bins + 1, title = paste0("Predictor ", d)))
}

dev.off()

save.image(paste0("SIMULATIONS/RESULTS/IMAGES/logistic_regression", 
                  "I", sim.data$I, "J", sim.data$J, "D", sim.data$D, 
                  "missing", sim.data$missing * 100, 
                  "Bins", bins, "replications", reps, ".rdata"))

saveRDS(finalMatrix, paste0("SIMULATIONS/RESULTS/finalMatrixRDS/logistic_regression", 
                            "I", sim.data$I, "J", sim.data$J, "D", sim.data$D, 
                            "missing", sim.data$missing * 100, 
                            "Bins", bins, "replications", reps, ".rds"))


writeLines(capture.output(sessionInfo()), 
           paste0("SIMULATIONS/RESULTS/SESSIONINFO/logistic_regression", 
                  "I", sim.data$I, "J", sim.data$J, "D", sim.data$D, 
                  "missing", sim.data$missing * 100, 
                  "Bins", bins, "replications", reps, ".txt"))

stopCluster(cl)
