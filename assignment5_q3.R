rm(list = ls())
air <- read.csv('airquality.csv', header = T)
air$cub_ozone <- air$Ozone^(1/3)
air <- subset(air, select = -c(X,Ozone))
air <- na.omit(air)

# a
Solar.R <- air$Solar.R
cubozone <- air$cub_ozone
solarlims <- range(Solar.R)
plot(Solar.R, cubozone, xlim = solarlims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(Solar.R, cubozone, cv = TRUE)
fit$lambda
fit$df
lines(fit, col = "blue", lwd = 2)

Wind <- air$Wind
cubozone <- air$cub_ozone
windlims <- range(Wind)
plot(Wind, cubozone, xlim = windlims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(Wind, cubozone, cv = TRUE)
fit$lambda
fit$df
lines(fit, col = "blue", lwd = 2)

Temp <- air$Temp
cubozone <- air$cub_ozone
solarlims <- range(Temp)
plot(Temp, cubozone, xlim = solarlims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(Temp, cubozone, cv = TRUE)
fit$lambda
fit$df
lines(fit, col = "blue", lwd = 2)

Month <- air$Month
cubozone <- air$cub_ozone
solarlims <- range(Month)
plot(Month, cubozone, xlim = solarlims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(Month, cubozone, cv = TRUE)
fit$lambda
fit$df
lines(fit, col = "blue", lwd = 2)

Day <- air$Day
cubozone <- air$cub_ozone
solarlims <- range(Day)
plot(Day, cubozone, xlim = solarlims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(Day, cubozone)
fit$lambda
fit$df
lines(fit, col = "blue", lwd = 2)

# b
library(gam)
gam1 <- gam(cub_ozone ~ s(Solar.R, 3.99798) + s(Wind, 3.588671) + s(Temp, 5.821091) + s(Month, 4.999999) + s(Day, 8.483312), data = air)
plot(gam1, se = TRUE, col = "blue")

# c
preds <- predict(gam1, newdata = air)
res <- air$cub_ozone - preds

solarlims <- range(Solar.R)
plot(air$Solar.R, res, xlim = solarlims, cex = .5, col = "darkgrey")

solarlims <- range(Wind)
plot(air$Wind, res, xlim = solarlims, cex = .5, col = "darkgrey")

solarlims <- range(Temp)
plot(air$Temp, res, xlim = solarlims, cex = .5, col = "darkgrey")

solarlims <- range(Month)
plot(air$Month, res, xlim = solarlims, cex = .5, col = "darkgrey")

solarlims <- range(Day)
plot(air$Day, res, xlim = solarlims, cex = .5, col = "darkgrey")