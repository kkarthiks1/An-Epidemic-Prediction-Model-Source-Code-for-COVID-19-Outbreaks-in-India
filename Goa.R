onset <- as.Date(c("2020-03-26", "2020-03-26","2020-03-26", "2020-03-29", "2020-03-29","2020-04-03", "2020-04-04"))
 
library(incidence)
i <- incidence(onset)
i
plot(i, border = "white")
mu <- 4.7 # mean in days days
sigma <- 2.9 # standard deviation in days
library(earlyR)

res <- get_R(i, si_mean = mu, si_sd = sigma)
res
plot(res)
plot(res, "lambdas", scale = length(onset) + 1)
abline(v = onset, lwd = 3, col = "grey")
abline(v = today, col = "blue", lty = 2, lwd = 2)
points(onset, seq_along(onset), pch = 20, cex = 3)
R_val <- sample_R(res, 1000)
summary(R_val)
quantile(R_val)
quantile(R_val, c(0.025, 0.975))
hist(R_val, border = "grey", col = "navy",
     xlab = "Values of R",
     main = "Sample of likely R values")
si <- res$si
si
library(projections)

future_i <- project(i, R = R_val, n_sim = 1000, si = res$si, n_days = 30)
future_i
mean(future_i)
plot(future_i)
predicted_n <- colSums(future_i)
summary(predicted_n)
