#Unsupervised Self-Organizing Maps 
library(kohonen)

#Data
data <- read.csv("breast cancer wiconsin data.csv")
data <- data[c(2:12)]
str(data)
X <- scale(data[, -1])
summary(X)

#SOM
set.seed(123)
g <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")
map <- som(X,
           grid = g,
           alpha = c(0.05, 0.01),
           radius = 1,
           rlen = 2000)
plot(map, type = "changes")
plot(map,
     type = "codes",
     palette.name = rainbow,
     main = "10 by 10 Mapping of Iris Data")
plot(map, type = "count")
plot(map, type = "mapping")
plot(map, type = "dist.neighbours")

#Supervised Self-Organizing Maps

#Data Split
set.seed(555)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

#Normalization
trainX <- scale(train[, -1])
testX <- scale(test[, -1],
               center = attr(trainX, "scaled:center"),
               scale = attr(trainX, "scaled:scale"))
trainY <- factor(train[, 1])
Y <- factor(test[, 1])
test[, 1] <- 0
testXY <- list(independent = testX, dependent = test[, 1])

#Classification & Prediction Model
set.seed(222)
map1 <- xyf(trainX,
            classvec2classmat(factor(trainY)),
            grid = somgrid(10, 10, "hexagonal"),
            rlen = 2000)
plot(map1, type = "changes")
plot(map1)
plot(map1, type = "count")

#Prediction
pred <- predict(map1, newdata = testXY)
conf_mat <- table(Predicted = pred$predictions[[2]], Actual = Y)
sum(diag(conf_mat))/sum(conf_mat)

#Cluster Boundries
par(mfrow = c(1, 1))
plot(map1,
     type = "codes",
     main = c("Codes X", "Codes Y"))
map1.hc <- cutree(hclust(dist(map1$codes[[2]], p=2)), k=2)
add.cluster.boundaries(map1, map1.hc)
