# Assignment11.1

Use the below given data set

Data Set

Perform the below given activities:

a. Apply PCA to the dataset and show proportion of variance

b. Perform PCA using SVD approach

c. Show the graphs of PCA components

Ans 2 a. ->

public scala.Tuple2<Matrix,Vector> computePrincipalComponentsAndExplainedVariance(int k) Ans 2 b. ->

library(dagdata)

data(tissuesGeneExpression)

library(rafalib)

group <- as.fumeric(tab$Tissue)

x <- t(e)

pc <- prcomp(x)

names(pc)

plot(pc$x[, 1], pc$x[, 2], col = group, main = "PCA", xlab = "PC1", ylab = "PC2")

cx <- sweep(x, 2, colMeans(x), "-")

sv <- svd(cx)

names(sv)

plot(sv$u[, 1], sv$u[, 2], col = group, main = "SVD", xlab = "U1", ylab = "U2")

sv$v[1:5, 1:5]

pc$rotation[1:5, 1:5]

head(sv$d^2)

head(pc$sdev^2)

head(sv$d^2/(ncol(e) - 1))

plot(sv$d^2/sum(sv$d^2), xlim = c(0, 15), type = "b", pch = 16, xlab = "principal components", ylab = "variance explained")

plot(sv$d^2/sum(sv$d^2), type = "b", pch = 16, xlab = "principal components", ylab = "variance explained")

svNoCenter <- svd(x)

plot(pc$x[, 1], pc$x[, 2], col = group, main = "PCA", xlab = "PC1", ylab = "PC2") points(0, 0, pch = 3, cex = 4, lwd = 4)

plot(svNoCenter$u[, 1], svNoCenter$u[, 2], col = group, main = "SVD not centered", xlab = "U1", ylab = "U2") sv2 <- svd(t(e))

plot(sv2$u[, 1], sv2$u[, 2], col = group, main = "samples vs genes (typical PCA)", xlab = "U1", ylab = "U2")

sv1 <- svd(e) plot(sv1$v[, 1], sv1$v[, 2], col = group, main = "genes vs samples (SVA)", xlab = "V1", ylab = "V2")

Ans 2 c. ->

PCACharts <- function(abc)

{

abc.var <- abc$sdev ^ 2 abc.pvar <- abc.var/sum(abc.var)

print("Proportion of Variance :")

print(abc.pvar)

par(mfrow = c(4,4))

plot(abc.pvar,xlab="Principal Component", ylab="Proportion of Variance explained", ylim=c(0,1), type='b')

plot(cumsum(abc.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')

screenplot(abc)

screenplot(abc,type="l")

par(mfrow=c(1,1))

}
