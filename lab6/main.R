library(L1pack)

# prepare datatset
x <- seq(from=-1.8, to=2, by=0.2)
y_good <- 2 + 2 * x + rnorm(length(x))

# prepare bad dataset
y_bad <- y_good
y_bad[1] <- y_bad[1] + 10
y_bad[20] <- y_bad[20] - 10

# counting coefs by least squares
lm_good <- lm(y_good ~ x, data.frame(x=x, y=y_good))$coefficients
lm_bad <- lm(y_bad ~ x, data.frame(x=x, y=y_bad))$coefficients

# counting coefs by least absolute deviations
lad_good <- lad(y_good ~ x, data.frame(x=x, y=y_good))$coefficients
lad_bad <- lad(y_bad ~ x, data.frame(x=x, y=y_bad))$coefficients

# print result coefs
print(lm_good)
print(lm_bad)
print(lad_good)
print(lad_bad)

# plot result funcs
plot(x, y_good, main="No errors, least squares")
lines(c(-1.8, 2), c(2 - 2 * 1.8, 2 + 2 * 2), col="red")
lines(c(-1.8, 2), c(lm_good[1] - lm_good[2] * 1.8, lm_good[1] + lm_good[2] * 2), col="blue")
lines(c(-1.8, 2), c(lad_good[1] - lad_good[2] * 1.8, lad_good[1] + lad_good[2] * 2), col="purple")
legend(x=-1.8, y=max(abs(y_good)), legend=c("Model", "LS", "LAD"), col=c("red", "blue", "purple"), lty=1)

plot(x, y_bad, main="Errors, least squares")
lines(c(-1.8, 2), c(2 - 2 * 1.8, 2 + 2 * 2))
lines(c(-1.8, 2), c(lm_bad[1] - lm_bad[2] * 1.8, lm_bad[1] + lm_bad[2] * 2), col="blue")
lines(c(-1.8, 2), c(lad_bad[1] - lad_bad[2] * 1.8, lad_bad[1] + lad_bad[2] * 2), col="purple")
legend(x=-1.8, y=max(abs(y_bad)), legend=c("Model", "LS", "LAD"), col=c("red", "blue", "purple"), lty=1)