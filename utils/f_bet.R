f_bet <- function(data, alpha, g=500){
  theta = 0.5
  M = seq(0,1, by = 1/g)
  x = data
  n = length(x)
  C = 1/2
  
  K_plus = matrix(1, nrow = g+1, ncol=n)
  K_mins = matrix(1, nrow = g+1, ncol=n)
  lambda_b = rep(0, n)
  k_plus = matrix(1, nrow = g+1, ncol=n)
  k_mins = matrix(1, nrow = g+1, ncol=n)
  lambda_b_plus = matrix(1, nrow = g+1, ncol=n)
  lambda_b_mins = matrix(1, nrow = g+1, ncol=n)
  
  S=0
  mu_hat_b=rep(0,n)
  sigma_hat_b=rep(0,n)
  lambda_b=rep(0,n)
  K_pm = matrix(1, nrow = g+1, ncol=n)
  CI_b = matrix(0, nrow = 2, ncol = n)
  CI_b[,1] = c(0,1)
  
  for(t in 1:n){
    S = S + x[t]
    if (t==1){
      mu_hat_b[t] = (0.5+x[t])/(t+1) 
      sigma_hat_b[t] = 1/8
      lambda_b[t] =  sqrt((2 * log(2/alpha) )/(sigma_hat_b[t] * t * log(t+1)))
      for(i in 1:(g+1)){
        m = M[i]
        if (m == 0){
          lambda_b_plus[i,t] = lambda_b[t]
          lambda_b_mins[i,t] = min(lambda_b[t], C/(1-m))
        }
        if (m == 1){
          lambda_b_plus[i,t] = min(lambda_b[t], C/m)
          lambda_b_mins[i,t] = lambda_b[t]
        }else{
          lambda_b_plus[i,t] = min(lambda_b[t], C/m)
          lambda_b_mins[i,t] = min(lambda_b[t], C/(1-m))
        }
        
        k_plus[i,t] = 1 + lambda_b_plus[i,t]*(x[t] - m)
        k_mins[i,t] = 1 - lambda_b_mins[i,t]*(x[t] - m)
        
        K_plus[i,t] = k_plus[i,t]
        K_mins[i,t] = k_mins[i,t]
        
        #K_pm[i, t] = max(theta * K_plus[i,t], (1-theta) * K_mins[i,t])
        K_pm[i, t] = theta * K_plus[i,t]+ (1-theta) * K_mins[i,t]
      }
    }else{
      mu_hat_b[t] = (0.5 + S)/(t+1)
      sigma_hat_b[t] = (0.25 + sum((x[1:t] - mu_hat_b[1:t])^2))/(t+1)
      lambda_b[t] = sqrt((2 * log(2/alpha) )/(sigma_hat_b[t-1]^2 * t * log(t+1)))
      for(i in 1:(g+1)){
        m = M[i]
        if (m == 0){
          lambda_b_plus[i,t] = lambda_b[t]
          lambda_b_mins[i,t] = min(lambda_b[t], C/(1-m))
        }
        if (m == 1){
          lambda_b_plus[i,t] = min(lambda_b[t], C/m)
          lambda_b_mins[i,t] = lambda_b[t]
        }else{
          lambda_b_plus[i,t] = min(lambda_b[t], C/m)
          lambda_b_mins[i,t] = min(lambda_b[t], C/(1-m))
        }
        
        k_plus[i,t] = 1 + lambda_b_plus[i,t] * (x[t] - m)
        k_mins[i,t] = 1 - lambda_b_mins[i,t] * (x[t] - m)
        
        K_plus[i,t] = K_plus[i,t-1] * k_plus[i,t]
        K_mins[i,t] = K_mins[i,t-1] * k_mins[i,t]
        
        #K_pm[i, t] = max(theta * K_plus[i,t], (1-theta) * K_mins[i,t])
        K_pm[i, t] = theta * K_plus[i,t] + (1-theta) * K_mins[i,t]
      }
    }
    # construct the confidence interval
    ms = M[which(K_pm[,t] < 1/alpha)]
    CI_b[,t] = c(min(ms), max(ms)) #lower bound and upper bound
    
    # One steps more is the intercept
    CI_b[1, t] = max(CI_b[1,1:t]) #lower bound
    CI_b[2, t] = min(CI_b[2,1:t]) #upper bound
  }
  
  low_b_bet = CI_b[1,]
  up_b_bet = CI_b[2,]
  return(list(l=low_b_bet, u=up_b_bet))
}