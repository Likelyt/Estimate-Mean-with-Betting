library(boot)
n = 10000
dist = 'beta' #ber
# Bernoulli
mean = 0.25
alpha = 0.05
# Beta
beta1=10
beta2=30
dup = 100

EB = source("EB.R")$value
f_bet = source("f_bet.R")$value
boots_ci = source("bootstrap.R")$value
g = 5000
B = 200

low_EBs = array(0, dim=c(dup,n))
up_EBs = array(0, dim=c(dup,n))

low_bets = array(0, dim=c(dup,n))
up_bets = array(0, dim=c(dup,n))

low_boots = array(0, dim=c(dup,n))
up_boots = array(0, dim=c(dup,n))

prob_eb = rep(0,dup)
prob_bet = rep(0,dup)
prob_boot = rep(0,dup)

cover_prob <- function(data, true_mean){
  n = dim(data)[1]
  prob = length(which(data[,1] < true_mean & data[,2]>true_mean))/n
  return(prob)
}
cover_len <- function(data){
  n = dim(data)[1]
  len = data[,2]-data[,1]
  return(len)
}

split = 10
BS_seq = rep(0, split)
for(i in 1:split){
  BS_seq[i] = 2^i
}

for(t in 1:dup){
  set.seed(12345+t)
  x = rbeta(n, beta1, beta2)
  # 1. EB Method
  eb_res = EB(n, x)
  low_EB = eb_res$low_eb
  up_EB = eb_res$up_eb
    
  low_EBs[t, ] = low_EB
  up_EBs[t, ] = up_EB
  
  prob_eb[t] = cover_prob(cbind(low_EB, up_EB), mean)
  
  # 2. Betting Method
  CI_bet = f_bet(x, alpha, g)
  CI_bet = cbind(CI_bet$l, CI_bet$u)
  low_bet = CI_bet[,1]
  up_bet = CI_bet[,2]
  
  low_bets[t, ] = low_bet
  up_bets[t, ] = up_bet
  
  prob_bet[t] = cover_prob(CI_bet, mean)
  
  # 3. Bootstrap Method
  CI_boots = array(0, dim=c(n,2))
  CI_boots[1,] = c(0,1)
  for(i in 2:n){
    if(i %in% BS_seq){
      CI_boots[i,] = boots_ci(x[1:i], alpha/split, B)
    }else{
      CI_boots[i,] = CI_boots[i-1,]
    }
  }
  low_boot = CI_boots[,1]
  up_boot = CI_boots[,2]
  
  low_boots[t, ] = low_boot
  up_boots[t, ] = up_boot
  
  prob_boot[t] = cover_prob(CI_boots, mean)
}

# Coverage Probability
cat("The coverage probability for EB (CI):",mean(prob_eb), sd(prob_eb), 
    ",bet (CI):", mean(prob_bet), sd(prob_bet),
    ",bootstrap (CI):", mean(prob_boot), sd(prob_boot))

# Coverage Length
len_eb = cover_len(cbind(apply(low_EBs, 2, mean), apply(up_EBs, 2, mean)))
len_bet = cover_len(cbind(apply(low_bets, 2, mean), apply(up_bets, 2, mean)))
len_boot = cover_len(cbind(apply(low_boots, 2, mean), apply(up_boots, 2, mean)))


selected = seq(1, n, 10)
log_selected = log(seq(1, n, 10), base=10)

pdf("fig/1-sim-CI.pdf", width = 7, height = 5)
plot(selected, apply(low_EBs, 2, mean)[selected], type = 'l', lty=1, lwd=2, col = "blue",ylim = c(0.22,0.28),
     xlab = "Time", ylab = "CIs", main = expression(paste(mu==0.25)), cex=1.4, cex.lab=1.4)
lines(selected, apply(up_EBs, 2, mean)[selected], type = "l", lty=1, lwd=2, col = "blue")
abline(h=mean, lty = 2, col = "black")

lines(selected, apply(low_bets, 2, mean)[selected], type = "l", lty=2, lwd=2,col = "red")
lines(selected, apply(up_bets, 2, mean)[selected], type = "l", lty=2, lwd=2,col = "red")

lines(selected, apply(low_boots, 2, mean)[selected], type = "l", lty=3, lwd=2,col = "green")
lines(selected, apply(up_boots, 2, mean)[selected], type = "l", lty=3, lwd=2,col = "green")

legend("topright", legend = c("Pr-EB", "Betting", "Boostrap"), cex=1.4, col = c("blue", "red", "green"), 
      lty = 1,lwd=2)
dev.off()


# Plot
pdf("fig/1-sim-CL.pdf", width = 7, height = 5)
plot(selected, len_eb[selected], type = "l", lty=1, lwd=2, col = "blue",ylim = c(0,0.1),
     xlab = "Time", ylab = "CL", main = expression(paste(mu == 0.25)), cex=1.4, cex.lab=1.4)
lines(selected, len_bet[selected], type = "l",lty=2, lwd=2,  col = "red")
lines(selected, len_boot[selected], type = "l",lty=3, lwd=2,  col = "green")
legend("topright", legend = c("Pr-EB", "Betting", "Boostrap"), lwd=2, cex=1.4, col = c("blue", "red", "green"), lty = 1)
dev.off()
 





