# Estimate-mean-with-betting

In this project, we implemented two simulations to show the performance of betting method
compared with the emperical Berstein method and boostrap method to construct the confidence sequence. The code is written in R langauge. Two main files are `1-synthetic.R` and `2-baseball.Rmd`. The detailed implementation of betting, boostrap, and emprical Berstein methods are in the `utils` folder.  All figure results are in the `fig` folder.

## 1. Synthetic data with Beta(10,30) distribution
_Data Generation_: $1\times 10^4$ data points are randomly sampled from a Beta distribution $\text{Beta}(10,30)$ to construct a $95\%$ confidence sequece.

```R
R 1-synthetic.R
```

## 2. Baseball batting data
_Data Description_: We created the first 45 random batting sequences $S_{i}$ based on the first given 45 bats MLE where 
$$S_{i} = [x_{i}^{s}| \sum_{s=1}^{45} x_{i}^{s}=p_{i}, x_{i}^{s}=\{0,1\}], \text{for } i = 1,2,..., n$$

Here $n =18$ represents the 18 players and $p_{i}$ represents the batting MLE. Our goal is to construct confidence interval for the true batting level $p_{i}^{*}$ of each player.
