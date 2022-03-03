
# remove the objects from global environment
rm(list = ls())

library(MASS)
library(boot)
library(splines)
attach(Boston)
set.seed(1)

############### use cubic polynomial regression to predict nox using dis

lm.fit = lm(nox ~ poly(dis, 3), data = Boston)

summary(lm.fit)
# Summary shows that all polynomial terms are significant while predicting nox using dis

# plot the resulting data and polynomial fits
dis.lim = range(dis)
dis.grid = seq(from = dis.lim[1], to = dis.lim[2], by = 0.1)

lm.pred = predict(lm.fit, list(dis = dis.grid))

plot(nox ~ dis, data = Boston, col = "darkgrey")

lines(dis.grid, lm.pred, col = "red", lwd = 2)
#Plot shows a smooth curve fitting the data fairly well







# calculate RSS for a range of polynomial degrees from 1 to 10

all.rss = rep(NA, 10)

for (i in 1:10) {
  lm.fit = lm(nox ~ poly(dis, i), data = Boston)
  all.rss[i] = sum(lm.fit$residuals^2)
}

# print the result

all.rss
# train RSS monotonically decreases with degree of polynomial,the higher the order of the polynomial model, the better the fit is going to be to the data points in the training set. 








# 10-fold cross validation to pick the best polynomial degree

all.deltas = rep(NA, 10)

for (i in 1:10) {
  glm.fit = glm(nox ~ poly(dis, i), data = Boston)
  
  all.deltas[i] = cv.glm(Boston, glm.fit, K = 10)$delta[2]
  
}

plot(1:10, all.deltas, xlab = "Degree", ylab = "CV error", type = "l", pch = 20, lwd = 2)

# the CV error reduces as we increase degree from 1 to 4, and the starts increasing for higher degrees. We pick 4 as the best polynomial degree







# fit a regression spline to predict nox using dis with 4 df

# dis range from 1 to 13, We split this range in roughly equal 4 intervals and establish knots at [4,7,11]

sp.fit = lm(nox ~ bs(dis, df = 4, knots = c(4, 7, 11)), data = Boston)

summary(sp.fit)
# The summary shows that all terms in spline fit are significant

sp.pred = predict(sp.fit, list(dis = dis.grid))

plot(nox ~ dis, data = Boston, col = "darkgrey")

lines(dis.grid, sp.pred, col = "red", lwd = 2)

# Plot shows that the spline fits data well except at the extreme values of dis






# calculate RSS for a range of df from 3 to 16

all.cv = rep(NA, 16)

for (i in 3:16) {
  lm.fit = lm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = sum(lm.fit$residuals^2)
}

all.cv[-c(1, 2)]
# As expected, train RSS monotonically decreases with degree of polynomial






# 10-fold cross validation to pick the best polynomial degree

all.cv = rep(NA, 16)

for (i in 3:16) {
  lm.fit = glm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = cv.glm(Boston, lm.fit, K = 10)$delta[2]
}

plot(3:16, all.cv[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")

# CV error attains minimum at df=13. We pick 12 as the optimal degrees of freedom
