
setwd("D:/2nd term material/Datascience/labs/Data Science project 2022")


# step 1: Retrieve and Clean Up Data 
zetaFile <- file.path("zeta.csv")
df1 <- read.csv(zetaFile)
df1
duplicated(df1)
sum(duplicated(df1))

file.create("zeta_nodeups.csv")
write.csv(df1,"zeta_nodeups.csv")



# step 2 : Data Analysis
incomeFile <- file.path("zipIncome.txt")
df2 <- read.csv(incomeFile)
df2
colnames(df2) <- c("zipcode","income")

plot(df2, xlab="zipcode", ylab="income", pch=19,main=" Scater Plot")

summary(df2)
df2 <- subset(df2 , df2$income >= 7000 & df2$income <= 200000)
summary(df2)


# step 3: Data Visualization
boxplot(df2$income,xlab='Income')
# Scaled by log10(income)
new_income<- log10(df2$income)

boxplot(new_income,xlab='X',ylab='Scaled Income')


# Advanced Analytics/Methods (K-means) 
library(cluster)
# install.packages("factoextra")
# library(factoextra)
income_elec <- file.path("income_elec_state.csv")
df3 <- read.csv(income_elec)
df3

state <-df3$X
income<-df3$income
Elec<-df3$elec
t<-data.frame(state,income,Elec)

colnames(t)<-c("state","meanhouseholdincome","meanelectricityusage")
t
#sort
income <- sort (t$meanhouseholdincome)
k <- kmeans(t$meanhouseholdincome, 10, iter.max = 20, nstart = 2)
str(k)


# Cluster identification for each observation
k$cluster

# Confusion Matrix
Confusion_Matrix <- table(t$meanhouseholdincome, k$cluster)
Confusion_Matrix

#plot
plot(t$meanhouseholdincome, col =  k$cluster)
points(k$centers, col =k$cluster, pch = 8, cex = 2)



# sum of squares
sum_of_squares <- function(x) sum(scale(x, scale = FALSE)^2)
sum_of_squares


#Repeat above step several times, we get different plots
k <- kmeans(t$meanhouseholdincome, 10)
plot(t$meanhouseholdincome, col = k$cluster)
points(k$centers, col=k$cluster, pch=8)

# Cluster centeroids "fitted" to each obs.:
# fitted.x <- fitted(k);  head(fitted.x)
# resid.x <- x - fitted(k)
# help (fitted)

#determine a reasonable value of k
k_range<-numeric(10)
for (i in 1:10) k_range[i] <- sum(kmeans(t$meanhouseholdincome, centers=i, nstart = 100, iter.max = 50)$tot.withinss)
plot(1:10, k_range, type="b", xlab="Number of Clusters", ylab="Total within-clusters sum of squares")

# "elbow"= 3 ,With k=3
k <- kmeans(t$meanhouseholdincome, 3, nstart=100, iter.max = 50)
plot(t$meanhouseholdincome, col = k$cluster)
points(k$centers, col= 1:10, pch=8) # col 1:10

# Visualization clustering results
kmean.clusters<-k$cluster
kmean.clusters

# log10 scale , cluster
New_k <- log10(t$meanhouseholdincome)
k <- kmeans(New_k, 10, nstart=100, iter.max = 50)
plot(New_k, col = k$cluster)
points(k$centers, col=1:10, pch=8)

# K-means clustering is not scale-invariant,
# Any changes made to the units of the data may impact on the clustering.

# Re-evaluation of k
k_range <-numeric(10)
for (i in 1:10) k_range[i] = sum(kmeans(New_k, centers=i, nstart = 100, iter.max = 50)$tot.withinss)
plot(1:10, k_range, type="b", xlab="Number of Clusters", ylab="Total within-clusters sum of squares")


# Elbow in the different position: k=3

# Outliers
k <- kmeans(t$meanhouseholdincome, 3 , 15)

#Plot the clusters ,centers
plot(t$meanhouseholdincome , col = k$cluster)

points(k$centers , col = 1:10 ,pch = 8)

summary(Income)
# Removing the Outliers
Income <- subset(t$meanhouseholdincome , t$meanhouseholdincome > 39000)
summary(Income)

# Re-evaluation of k
K1 <- kmeans(Income, 3 , 15)

#Plot the clusters , Centers
plot(Income , col = K1$cluster)

points(K1$centers , col = 1:10 ,pch = 8,cex = 2)
DATA <- cbind(t$meanhouseholdincome, cluster = k$cluster)
head(DATA)

# Means of each cluster
total_means <- aggregate(t$meanhouseholdincome, by=list(cluster=k$cluster), mean)
total_means
# calculate gap statistic based on the number of clusters
# library(factoextra)
# gap_stat <- clusGap(t$meanhouseholdincome,
#                  #  FUN = pam,
#                    K.max = 10, #max clusters to consider
#                    B = 50) #total bootstrapped iterations
# fviz_gap_stat(gap_stat)

#fviz_cluster(kmed, t$meanhouseholdincome)#plot cluster
#Date in whice cluster
