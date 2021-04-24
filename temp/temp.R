## ----exploratory analysis-----------------------------------------------------
summary(ames.full)

# count NA values and get column types
# nac <- sapply(ames.full, function(x) sum(is.na(x)))
# for (i in 1:length(nac)) {
#   cat(names(nac[i]), "\t", nac[[i]], "\t", class(ames.full[1,i]), "\n")
# }

# hist(ames.full$SalePrice)
# hist(ames.full$Gr.Liv.Area)

## ----model 1------------------------------------------------------------------

model1 <- lm(SalePrice ~ Gr.Liv.Area, data=ames.full)
summary(model1)

par(mfrow=c(2,2))
plot(model1)

plot(ames.full$Gr.Liv.Area, ames.full$SalePrice, col=point.col)
abline(reg=model1)

# Residual plots
# plot(fitted(model1), ames.full$SalePrice, xlab = "Fitted Values", col=point.col)
plot(residuals(model1), fitted(model1), xlab = "Residuals", ylab = "Fitted Values", col=point.col)

## ----model 2------------------------------------------------------------------

model2 <- lm(
  SalePrice ~ Gr.Liv.Area + Year.Built,
  data=ames.full
  )

model <- model2
summary(model)

par(mfrow=c(2,2))
plot(model2)


# Residual plots
plot(residuals(model), fitted(model), xlab = "Residuals", ylab = "Fitted Values", col=point.col)


## ----model 3------------------------------------------------------------------

model3 <- lm(
  SalePrice ~ Gr.Liv.Area + Year.Built,
  data=ames.full
)

model <- model3
summary(model)

par(mfrow=c(2,2))
plot(model3)

# Residual plots
plot(residuals(model), fitted(model), xlab = "Residuals", ylab = "Fitted Values", col=point.col)


## ----model 4------------------------------------------------------------------

model4 <- lm(
  SalePrice ~ Gr.Liv.Area + Year.Built + 
    Lot.Area + Overall.Qual + Overall.Cond + 
    Garage.Area + Total.Bsmt.SF,
  data=ames.full
)

model <- model4
summary(model)

par(mfrow=c(2,2))
plot(model4)

# Residual plots
plot(residuals(model), fitted(model), xlab = "Residuals", ylab = "Fitted Values", col=point.col)

## ----model 5------------------------------------------------------------------

model5 <- lm(
  SalePrice ~ .,
  data=na.omit(ames.full)
)

model <- model4
summary(model)

par(mfrow=c(2,2))
plot(model4)

# Residual plots
plot(residuals(model), fitted(model), xlab = "Residuals", ylab = "Fitted Values", col=point.col)



## ----pca----------------------------------------------------------------------
ames.numeric <- na.omit(subset(ames.full, select=c(
  SalePrice,
  Gr.Liv.Area,
  Year.Built,
    Lot.Area, Overall.Qual, Overall.Cond, 
    Garage.Area, Total.Bsmt.SF
)))

ames.pca <- prcomp(ames.numeric, center = TRUE, scale. = TRUE)

library("rgl")
plot3d(ames.pca$x[,1], ames.pca$x[,2], ames.pca$x[,3], type="s", size=1, lit=TRUE,)
rglwidget()

# compute the distance from the origin given the first three PCs
r2 <- ames.pca$x[,1]^2 + ames.pca$x[,2]^2 + ames.pca$x[,3]^2
# get the indices of the points sorted in decreasing distance from the origin
r2 <- order(r2, decreasing=TRUE)

# output the information
cat("indices of outlier points: ")
idx = r2[1:3]
cat(idx)
cat("\n")
# print their total area
cat("area of outlier points: ")
cat(ames.numeric$Gr.Liv.Area[idx])
cat("\n")

dev.new(width=5, height=5)
plot(ames.pca$x[,1], ames.pca$x[,2], asp=1, col=point.col)
points(ames.pca$x[idx,1], ames.pca$x[idx,2], col='red', pch=5)

hist(ames.full$Gr.Liv.Area)
abline(v=ames.numeric$Gr.Liv.Area[idx], col='red')

hist(ames.full$Year.Built)
abline(v=ames.numeric$Year.Built[idx], col='red')

hist(ames.full$Lot.Area)
abline(v=ames.numeric$Lot.Area[idx], col='red')

hist(ames.full$Overall.Qual)
abline(v=ames.numeric$Overall.Qual[idx], col='red')

hist(ames.full$Overall.Cond)
abline(v=ames.numeric$Overall.Cond[idx], col='red')

hist(ames.full$Garage.Area)
abline(v=ames.numeric$Garage.Area[idx], col='red')

hist(ames.full$Total.Bsmt.SF)
abline(v=ames.numeric$Total.Bsmt.SF[idx], col='red')
