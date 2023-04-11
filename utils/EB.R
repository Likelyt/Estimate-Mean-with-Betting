EB <- function(n, x){
  # confidence interval
  CI_EB = array(0, dim=c(n, 2))
  CI_EB[1,] = c(0,1)
  lambda = rep(0, n)
  sigma_hat = rep(0, n)
  mu_hat = rep(0, n)
  width = rep(0, n)
  phi = rep(0, n)
  v = rep(0, n)
  
  est_mean = rep(0, n)
  S = 0
  C = 1/2
  
  for(t in 1:n){
    S = S + x[t]
    if (t==1){
      mu_hat[t] = (0.5+x[t])/(t+1) 
      sigma_hat[t] = 1/8
      lambda[t] =  min(sqrt( (2 * log(2/alpha) )/(sigma_hat[t] * t * log(t+1))), C)
      v[t] = x[t]^2
      est_mean[t] = x[t] 
    }else{
      mu_hat[t] = (0.5 + S)/(t+1)
      sigma_hat[t] = (0.25 + sum((x[1:t] - mu_hat[1:t])^2))/(t+1)
      lambda[t] = min(sqrt((2 * log(2/alpha) )/(sigma_hat[t-1]^2 * t * log(t+1))),C)
      # calculate v, phi, and width 
      v[t] = (x[t] - mu_hat[t-1])^2
      phi[t] = -log(1-lambda[t]) - lambda[t]
      width[t] = (log(2/alpha) + (v[1:t]%*%phi[1:t])[1])/sum(lambda[1:t])
      # mean
      est_mean[t] = (sum(lambda[1:t]%*%x[1:t])[1])/sum(lambda[1:t])
      # CI
      CI_EB[t,] = est_mean[t] + c(-1,1)* width[t] 
    }
    # One steps more is the intercept
    CI_EB[t,1] = max(CI_EB[1:t, 1]) #lower bound
    CI_EB[t,2] = min(CI_EB[1:t, 2]) #upper bound
  }
  low_EB = CI_EB[,1]
  up_EB = CI_EB[,2]
  
  return(list(low_eb = low_EB, up_eb = up_EB))
}

