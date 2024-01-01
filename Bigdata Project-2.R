install.packages("psych")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("factoextra")
install.packages("class")
install.packages("gmodels")
install.packages("caret")

library(gmodels)

library("class")
library("psych")
library("corrplot")
library("caret")

dataset<-read.table("/Users/rashitiwary/Documents/George Washington University/Masters Subject/CSCI 6444 Big data and Analytics/Project 2/abalone.data",stringsAsFactors = FALSE, sep=',')
dataset[1:12,1:9]

dataset.names<-c("Sex","Length","Diameter","Height","Whole weight","Shucked weight","Viscera weight","Shell weight","Rings")
dataset.names

str(dataset)

Sex<-dataset[,1]
Sex

names(dataset)<-dataset.names
dataset[1:12,1:9]

is.numeric(dataset)

df2<-dataset
df2


summary(dataset)

nadata<-na.omit(dataset)
str(nadata)

describe(dataset)

plot(dataset)


# Convert Sex column to factor
df2$Sex <- factor(df2$Sex)


# Create a new column with 0 or 1 values based on "Sex" column
df2$Sex_num <- ifelse(df2$Sex == "M", 1, ifelse(df2$Sex == "F", 2, 3))
df2<-df2[,-1]
df2

#the correlation matrix
corr_matrix <- cor(df2[,1:9])
corr_matrix


# Visualize the correlation matrix
corrplot(corr_matrix, type = "lower", order = "hclust", tl.col = "Black",tl.srt = 45)

#From the correlation matrix, we can see that "Diameter", "Length", "Height", "Whole weight",
#"Viscera weight", "Shucked weight" and "Shell weight" have the strongest correlations with "Rings".
#It's important to note that "SEX" has a weaker correlation with
#"Rings" compared to the other attributes hence removing it

df2<-df2[,-9]
df2[1:10,1:8]

#MIN-MAX normalization
normalize<-function(x){((x-min(x))/(max(x)-min(x)))}
normalize

df2.norm<-as.data.frame(lapply(df2[,1:8], normalize))
         
df2.norm[250:260,]  
df2[250:260,]


#Z-Score Normalization
df2.znorm<-as.data.frame(lapply(df2[,1:8], scale))
df2.znorm[1:10,]
df2[1:10,]

#K-means clustering with K=2
df2.norm_kmeans_k2=kmeans(df2.norm, centers = 2)
str(df2.norm_kmeans_k2)
df2.norm_kmeans_k2

factoextra::fviz_cluster(df2.norm_kmeans_k2,df2.norm,title = "K-means clustering (k=2) of Abalone dataset")

#K-means clustering with K=3
df2.norm_kmeans_k3=kmeans(df2.norm, centers = 3 )
str(df2.norm_kmeans_k3)
df2.norm_kmeans_k3

factoextra::fviz_cluster(df2.norm_kmeans_k3,df2.norm,title = "K-means clustering (k=3) of Abalone dataset")

#K-means clustering with K=4
df2.norm_kmeans_k4=kmeans(df2.norm, centers = 4 )
str(df2.norm_kmeans_k4)
df2.norm_kmeans_k4

factoextra::fviz_cluster(df2.norm_kmeans_k4,df2.norm,title = "K-means clustering (k=4) of Abalone dataset")


#K-means clustering with K=5
df2.norm_kmeans_k5=kmeans(df2.norm, centers = 5 )
str(df2.norm_kmeans_k5)
df2.norm_kmeans_k5

factoextra::fviz_cluster(df2.norm_kmeans_k5,df2.norm,title = "K-means clustering (k=5) of Abalone dataset")

#K-means clustering with K=6
df2.norm_kmeans_k6=kmeans(df2.norm, centers = 6 )
str(df2.norm_kmeans_k6)
df2.norm_kmeans_k6

factoextra::fviz_cluster(df2.norm_kmeans_k6,df2.norm,title = "K-means clustering (k=6) of Abalone dataset")

#K-means clustering with K=7
df2.norm_kmeans_k7=kmeans(df2.norm, centers = 7 )
str(df2.norm_kmeans_k7)
df2.norm_kmeans_k7

factoextra::fviz_cluster(df2.norm_kmeans_k7,df2.norm,title = "K-means clustering (k=7) of Abalone dataset")

#K-means clustering with K=8
df2.norm_kmeans_k8=kmeans(df2.norm, centers = 8 )
str(df2.norm_kmeans_k8)
df2.norm_kmeans_k8

factoextra::fviz_cluster(df2.norm_kmeans_k8,df2.norm,title = "K-means clustering (k=8) of Abalone dataset")

#K-means clustering with K=9
df2.norm_kmeans_k9=kmeans(df2.norm, centers = 9 )
str(df2.norm_kmeans_k9)
df2.norm_kmeans_k9

factoextra::fviz_cluster(df2.norm_kmeans_k9,df2.norm,title = "K-means clustering (k=9) of Abalone dataset")

#K-means clustering with K=10
df2.norm_kmeans_k10=kmeans(df2.norm, centers = 10 )
str(df2.norm_kmeans_k10)
df2.norm_kmeans_k10
df2.norm_kmeans_k10$centers

factoextra::fviz_cluster(df2.norm_kmeans_k10,df2.norm,title = "K-means clustering (k=10) of Abalone dataset")

#Finding a Good Number of Clusters
wssplot <- function(data, nc= 15, seed = 1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <-sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b",
       xlab="Number of clusters K", ylab=" within-clusters sum of squares")
}
wssplot(df2.norm,nc=10,seed=12324)
  

factoextra::fviz_nbclust(df2.norm,FUNcluster = kmeans,method="wss",k.max=10,verbose=TRUE) 
factoextra::fviz_nbclust(df2.norm,FUNcluster = kmeans,method="silhouette",k.max=10,verbose=TRUE) 
factoextra::fviz_nbclust(df2.norm,FUNcluster = kmeans,method="gap_stat",k.max=10,verbose=TRUE) 

#KNN IMPLEMENTATION

#Splitting data into 70-30
df2.norm.rows= nrow(df2.norm)
df2.norm.sample=0.7
df2.rows=df2.norm.rows*df2.norm.sample
df2.rows

df2.train.index=sample(df2.norm.rows,df2.rows)
length(df2.train.index)

df2.train_70<-df2.norm[df2.train.index,]
df2.train_70[1:15,]

df2.test_70<-df2.norm[-df2.train.index,]
df2.train_70[1:15,]
dim(df2.test_70)


#Splitting data into 60-40
df2.norm.rows_60= nrow(df2.norm)
df2.norm.sample_60=0.6
df2.rows_60=df2.norm.rows_60*df2.norm.sample_60
df2.rows_60

df2.train.index_60=sample(df2.norm.rows_60,df2.rows_60)
length(df2.train.index_60)

df2.train_60<-df2.norm[df2.train.index_60,]
df2.train_60[1:15,]

df2.test_60<-df2.norm[-df2.train.index_60,]
df2.train_60[1:15,]
dim(df2.test_60)


#Splitting data into 50-50
df2.norm.rows_50= nrow(df2.norm)
df2.norm.sample_50=0.5
df2.rows_50=df2.norm.rows_50*df2.norm.sample_50
df2.rows_50

df2.train.index_50=sample(df2.norm.rows_50,df2.rows_50)
length(df2.train.index_50)

df2.train_50<-df2.norm[df2.train.index_50,]
df2.train_50[1:15,]

df2.test_50<-df2.norm[-df2.train.index_50,]
df2.train_50[1:15,]
dim(df2.test_50)


######################KNN with K=5 and 70-30 split##########################
df2.train_70_k5<-kmeans(df2.train_70, centers=5)
df2.train_70_k5

df2.test_70_k5= knn(df2.train_70, df2.test_70, df2.train_70_k5$cluster,k=5)
df2.test_70_k5 

#apply kmeans test data to generate labels for the test records
df2.test_70_kmeans_k5<-kmeans(df2.test_70, centers=5)
df2.test_70_kmeans_k5

df2.test_70_k5.labels<-df2.test_70_kmeans_k5$cluster
length(df2.test_70_k5.labels) 

df2.test_70_k5.labels 


#Linear modelling using GLM
df2.train_70.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight+Viscera.weight+
                        Shell.weight,family=gaussian, data=df2.train_70)

summary(df2.train_70.glm)

# anova – Analysis of Variance on the model result
df2.train_70.glm.anova<-anova(df2.train_70.glm,test="Chisq")
df2.train_70.glm.anova

plot(df2.train_70.glm)

#Predict
df2.test_70.predict<-predict(df2.train_70.glm, newdata = df2.test_70)
length(df2.test_70.predict)
summary(df2.test_70.predict)

#confidence intervals
confint(df2.train_70.glm)

#Comparing Actual vs. Prediction:
df2.test_70.predict.k5=kmeans(df2.test_70.predict, centers=5)
df2.test_70.predict.k5
length(df2.test_70.predict.k5)

df2.test_70.ct.k5=CrossTable(df2.test_70.predict.k5$cluster, df2.test_70_kmeans_k5$cluster, prop.chisq=TRUE)
df2.test_70.ct.k5

length(df2.test_70_kmeans_k5$cluster)

length(df2.test_70.predict.k5$cluster)


conf_mat=df2.test_70.ct.k5$t
conf_mat


# Calculate the metrics
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- diag(conf_mat) / colSums(conf_mat)
recall <- diag(conf_mat) / rowSums(conf_mat)
specificity <- sapply(1:nrow(conf_mat), function(i) {
  TN <- sum(conf_mat[-i, -i])
  FP <- sum(conf_mat[-i, i])
  TN / (TN + FP)
})
error <- 1 - accuracy

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("Error:", error, "\n")
###########################KNN with K=5 and 70-30 split ends###########################



###########################KNN with K=7 and 70-30 split########################
df2.train_70_k7<-kmeans(df2.train_70, centers=7)
df2.train_70_k7

df2.test_70_k7= knn(df2.train_70, df2.test_70, df2.train_70_k7$cluster,k=7)
df2.test_70_k7

#apply kmeans test data to generate labels for the test records
df2.test_70_kmeans_k7<-kmeans(df2.test_70, centers=7)
df2.test_70_kmeans_k7

df2.test_70_k7.labels<-df2.test_70_kmeans_k7$cluster
length(df2.test_70_k7.labels)

df2.test_70_k7.labels


#Linear modelling using GLM
df2.train_70.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight+Viscera.weight+
                        Shell.weight,family=gaussian, data=df2.train_70)

summary(df2.train_70.glm)

# anova – Analysis of Variance on the model result
df2.train_70.glm.anova<-anova(df2.train_70.glm,test="Chisq")
df2.train_70.glm.anova

plot(df2.train_70.glm)

#Predict
df2.test_70.predict<-predict(df2.train_70.glm, newdata = df2.test_70)
df2.test_70.predict
summary(df2.test_70.predict)

#confidence intervals
confint(df2.train_70.glm)

#Comparing Actual vs. Prediction:
df2.test_70.predict.k7=kmeans(df2.test_70.predict, centers=7)
df2.test_70.predict.k7

df2.test_70.ct.k7<-CrossTable(df2.test_70.predict.k7$cluster, df2.test_70_kmeans_k7$cluster, prop.chisq=TRUE)
df2.test_70.ct.k7

length(df2.test_70_kmeans_k7$cluster)

length(df2.test_70.predict.k7$cluster)


conf_matrix=df2.test_70.ct.k7$t
conf_matrix


# Calculate the metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- diag(conf_matrix) / colSums(conf_matrix)
recall <- diag(conf_matrix) / rowSums(conf_matrix)
specificity <- sapply(1:nrow(conf_matrix), function(i) {
  TN <- sum(conf_matrix[-i, -i])
  FP <- sum(conf_matrix[-i, i])
  TN / (TN + FP)
})
error <- 1 - accuracy

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("Error:", error, "\n")
##########################Knn with 70-30, 7 clusters end#############################



###########################KNN with K=9 and 70-30 split###########################
df2.train_70_k9<-kmeans(df2.train_70, centers=9)
df2.train_70_k9

df2.test_70_k9= knn(df2.train_70, df2.test_70, df2.train_70_k9$cluster,k=9)
df2.test_70_k9

#apply kmeans test data to generate labels for the test records
df2.test_70_kmeans_k9<-kmeans(df2.test_70, centers=9)
df2.test_70_kmeans_k9

df2.test_70_k9.labels<-df2.test_70_kmeans_k9$cluster
length(df2.test_70_k9.labels)

df2.test_70_k9.labels

#Linear modelling using GLM
df2.train_70.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight+Viscera.weight+
                        Shell.weight,family=gaussian, data=df2.train_70)

summary(df2.train_70.glm)

# anova – Analysis of Variance on the model result
df2.train_70.glm.anova<-anova(df2.train_70.glm,test="Chisq")
df2.train_70.glm.anova

plot(df2.train_70.glm)

#Predict
df2.test_70.predict<-predict(df2.train_70.glm, newdata = df2.test_70)
df2.test_70.predict
summary(df2.test_70.predict)

#confidence intervals
confint(df2.train_70.glm)

#Comparing Actual vs. Prediction:
df2.test_70.predict.k9=kmeans(df2.test_70.predict, centers=9)
df2.test_70.predict.k9

df2.test_70.ct.k9<-CrossTable(df2.test_70.predict.k9$cluster, df2.test_70_kmeans_k9$cluster, prop.chisq=TRUE)
df2.test_70.ct.k9

confusion_matrix=df2.test_70.ct.k9$t
confusion_matrix


length(df2.test_70_kmeans_k9$cluster)

length(df2.test_70.predict.k9$cluster)

df2.test_70.ct.k9

# Calculating the metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
specificity <- sapply(1:nrow(confusion_matrix), function(i) {
  TN <- sum(confusion_matrix[-i, -i])
  FP <- sum(confusion_matrix[-i, i])
  TN / (TN + FP)
})
error <- 1 - accuracy


# Printing the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("Error:", error, "\n")
###########################KNN with K=9 and 70-30 split ends###########################


###########################KNN with K=5 and 60-40 split###########################
df2.train_60_k5<-kmeans(df2.train_60, centers=5)
df2.train_60_k5

df2.test_60_k5= knn(df2.train_60, df2.test_60, df2.train_60_k5$cluster,k=5)
df2.test_60_k5

#apply kmeans test data to generate labels for the test records
df2.test_60_kmeans_k5<-kmeans(df2.test_60, centers=5)
df2.test_60_kmeans_k5

df2.test_60_k5.labels<-df2.test_60_kmeans_k5$cluster
length(df2.test_60_k5.labels)

df2.test_60_k5.labels

#Linear modelling using GLM
df2.train_60.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight+Viscera.weight+
                        Shell.weight,family=gaussian, data=df2.train_60)

summary(df2.train_60.glm)

# anova – Analysis of Variance on the model result
df2.train_60.glm.anova<-anova(df2.train_60.glm,test="Chisq")
df2.train_60.glm.anova

plot(df2.train_60.glm)

#Predict
df2.test_60.predict<-predict(df2.train_60.glm, newdata = df2.test_60)
df2.test_60.predict

summary(df2.test_60.predict)

#confidence intervals
confint(df2.train_60.glm)

#Comparing Actual vs. Prediction:
df2.test_60.predict.k5=kmeans(df2.test_60.predict, centers=5)
df2.test_60.predict.k5

df2.test_60.ct.k5<-CrossTable(df2.test_60.predict.k5$cluster, df2.test_60_kmeans_k5$cluster, prop.chisq=TRUE)
df2.test_60.ct.k5

confusion_matrix=df2.test_60.ct.k5$t
confusion_matrix


length(df2.test_60_kmeans_k5$cluster)

length(df2.test_60.predict.k5$cluster)

df2.test_60.ct.k5

# Calculating the metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
specificity <- sapply(1:nrow(confusion_matrix), function(i) {
  TN <- sum(confusion_matrix[-i, -i])
  FP <- sum(confusion_matrix[-i, i])
  TN / (TN + FP)
})
error <- 1 - accuracy


# Printing the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("Error:", error, "\n")
###########################KNN with K=5 and 60-40 split ends###########################

###########################KNN with K=7 and 60-40 split###########################
df2.train_60_k7<-kmeans(df2.train_60, centers=7)
df2.train_60_k7

df2.test_60_k7= knn(df2.train_60, df2.test_60, df2.train_60_k7$cluster,k=7)
df2.test_60_k7

#apply kmeans test data to generate labels for the test records
df2.test_60_kmeans_k7<-kmeans(df2.test_60, centers=7)
df2.test_60_kmeans_k7

df2.test_60_k7.labels<-df2.test_60_kmeans_k7$cluster
length(df2.test_60_k7.labels)

df2.test_60_k7.labels

#Linear modelling using GLM
df2.train_60.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight+Viscera.weight+
                        Shell.weight,family=gaussian, data=df2.train_60)

summary(df2.train_60.glm)

# anova – Analysis of Variance on the model result
df2.train_60.glm.anova<-anova(df2.train_60.glm,test="Chisq")
df2.train_60.glm.anova

plot(df2.train_60.glm)


#Predict
df2.test_60.predict<-predict(df2.train_60.glm, newdata = df2.test_60)
df2.test_60.predict

summary(df2.test_60.predict)

#confidence intervals
confint(df2.train_60.glm)

#Comparing Actual vs. Prediction:
df2.test_60.predict.k7=kmeans(df2.test_60.predict, centers=7)
df2.test_60.predict.k7

df2.test_60.ct.k7<-CrossTable(df2.test_60.predict.k7$cluster, df2.test_60_kmeans_k7$cluster, prop.chisq=TRUE)
df2.test_60.ct.k7

confusion_matrix=df2.test_60.ct.k7$t
confusion_matrix


length(df2.test_60_kmeans_k7$cluster)

length(df2.test_60.predict.k7$cluster)

df2.test_60.ct.k7

# Calculating the metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
specificity <- sapply(1:nrow(confusion_matrix), function(i) {
  TN <- sum(confusion_matrix[-i, -i])
  FP <- sum(confusion_matrix[-i, i])
  TN / (TN + FP)
})
error <- 1 - accuracy


# Printing the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("Error:", error, "\n")
###########################KNN with K=7 and 60-40 split ends###########################

###########################KNN with K=9 and 60-40 split###########################
df2.train_60_k9<-kmeans(df2.train_60, centers=9)
df2.train_60_k9

df2.test_60_k9= knn(df2.train_60, df2.test_60, df2.train_60_k9$cluster,k=9)
df2.test_60_k9

#apply kmeans test data to generate labels for the test records
df2.test_60_kmeans_k9<-kmeans(df2.test_60, centers=9)
df2.test_60_kmeans_k9

df2.test_60_k9.labels<-df2.test_60_kmeans_k9$cluster
length(df2.test_60_k9.labels)

df2.test_60_k9.labels

#Linear modelling using GLM
df2.train_60.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight+Viscera.weight+
                        Shell.weight,family=gaussian, data=df2.train_60)

summary(df2.train_60.glm)

# anova – Analysis of Variance on the model result
df2.train_60.glm.anova<-anova(df2.train_60.glm,test="Chisq")
df2.train_60.glm.anova

plot(df2.train_60.glm)


#Predict
df2.test_60.predict<-predict(df2.train_60.glm, newdata = df2.test_60)
df2.test_60.predict

summary(df2.test_60.predict)

#confidence intervals
confint(df2.train_60.glm)

#Comparing Actual vs. Prediction:
df2.test_60.predict.k9=kmeans(df2.test_60.predict, centers=9)
df2.test_60.predict.k9

df2.test_60.ct.k9<-CrossTable(df2.test_60.predict.k9$cluster, df2.test_60_kmeans_k9$cluster, prop.chisq=TRUE)
df2.test_60.ct.k9

confusion_matrix=df2.test_60.ct.k9$t
confusion_matrix


length(df2.test_60_kmeans_k9$cluster)

length(df2.test_60.predict.k9$cluster)

df2.test_60.ct.k9

# Calculating the metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
specificity <- sapply(1:nrow(confusion_matrix), function(i) {
  TN <- sum(confusion_matrix[-i, -i])
  FP <- sum(confusion_matrix[-i, i])
  TN / (TN + FP)
})
error <- 1 - accuracy


# Printing the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("Error:", error, "\n")
###########################KNN with K=9 and 60-40 split ends###########################

###########################KNN with K=5 and 50-50 split###########################
df2.train_50_k5<-kmeans(df2.train_50, centers=5)
df2.train_50_k5

df2.test_50_k5= knn(df2.train_50, df2.test_50, df2.train_50_k5$cluster,k=5)
df2.test_50_k5

#apply kmeans test data to generate labels for the test records
df2.test_50_kmeans_k5<-kmeans(df2.test_50, centers=5)
df2.test_50_kmeans_k5

df2.test_50_k5.labels<-df2.test_50_kmeans_k5$cluster
length(df2.test_50_k5.labels)

df2.test_50_k5.labels

#Linear modelling using GLM
df2.train_50.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight+Viscera.weight+
                        Shell.weight,family=gaussian, data=df2.train_50)

summary(df2.train_50.glm)

# anova – Analysis of Variance on the model result
df2.train_50.glm.anova<-anova(df2.train_50.glm,test="Chisq")
df2.train_50.glm.anova

plot(df2.train_50.glm)


#Predict
df2.test_50.predict<-predict(df2.train_50.glm, newdata = df2.test_50)
df2.test_50.predict

summary(df2.test_50.predict)

#confidence intervals
confint(df2.train_50.glm)

#Comparing Actual vs. Prediction:
df2.test_50.predict.k5=kmeans(df2.test_50.predict, centers=5)
df2.test_50.predict.k5

df2.test_50.ct.k5<-CrossTable(df2.test_50.predict.k5$cluster, df2.test_50_kmeans_k5$cluster, prop.chisq=TRUE)
df2.test_50.ct.k5

confusion_matrix=df2.test_50.ct.k5$t
confusion_matrix


length(df2.test_50_kmeans_k5$cluster)

length(df2.test_50.predict.k5$cluster)

df2.test_50.ct.k5

# Calculating the metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
specificity <- sapply(1:nrow(confusion_matrix), function(i) {
  TN <- sum(confusion_matrix[-i, -i])
  FP <- sum(confusion_matrix[-i, i])
  TN / (TN + FP)
})
error <- 1 - accuracy


# Printing the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("Error:", error, "\n")
###########################KNN with K=5 and 50-50 split ends###########################



###########################KNN with K=7 and 50-50 split###########################
df2.train_50_k7<-kmeans(df2.train_50, centers=7)
df2.train_50_k7

df2.test_50_k7= knn(df2.train_50, df2.test_50, df2.train_50_k7$cluster,k=7)
df2.test_50_k7

#apply kmeans test data to generate labels for the test records
df2.test_50_kmeans_k7<-kmeans(df2.test_50, centers=7)
df2.test_50_kmeans_k7

df2.test_50_k7.labels<-df2.test_50_kmeans_k7$cluster
length(df2.test_50_k7.labels)

df2.test_50_k7.labels

#Linear modelling using GLM
df2.train_50.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight+Viscera.weight+
                        Shell.weight,family=gaussian, data=df2.train_50)

summary(df2.train_50.glm)

# anova – Analysis of Variance on the model result
df2.train_50.glm.anova<-anova(df2.train_50.glm,test="Chisq")
df2.train_50.glm.anova

plot(df2.train_50.glm)


#Predict
df2.test_50.predict<-predict(df2.train_50.glm, newdata = df2.test_50)
df2.test_50.predict

summary(df2.test_50.predict)

#confidence intervals
confint(df2.train_50.glm)

#Comparing Actual vs. Prediction:
df2.test_50.predict.k7=kmeans(df2.test_50.predict, centers=7)
df2.test_50.predict.k7

df2.test_50.ct.k7<-CrossTable(df2.test_50.predict.k7$cluster, df2.test_50_kmeans_k7$cluster, prop.chisq=TRUE)
df2.test_50.ct.k7

confusion_matrix=df2.test_50.ct.k7$t
confusion_matrix


length(df2.test_50_kmeans_k7$cluster)

length(df2.test_50.predict.k7$cluster)

df2.test_50.ct.k7

# Calculating the metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
specificity <- sapply(1:nrow(confusion_matrix), function(i) {
  TN <- sum(confusion_matrix[-i, -i])
  FP <- sum(confusion_matrix[-i, i])
  TN / (TN + FP)
})
error <- 1 - accuracy


# Printing the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("Error:", error, "\n")
###########################KNN with K=7 and 50-50 split ends###########################




###########################KNN with K=9 and 50-50 split###########################
df2.train_50_k9<-kmeans(df2.train_50, centers=9)
df2.train_50_k9

df2.test_50_k9= knn(df2.train_50, df2.test_50, df2.train_50_k9$cluster,k=9)
df2.test_50_k9

#apply kmeans test data to generate labels for the test records
df2.test_50_kmeans_k9<-kmeans(df2.test_50, centers=9)
df2.test_50_kmeans_k9

df2.test_50_k9.labels<-df2.test_50_kmeans_k9$cluster
length(df2.test_50_k9.labels)

df2.test_50_k9.labels

#Linear modelling using GLM
df2.train_50.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight + Viscera.weight + Shell.weight,family=gaussian, data=df2.train_50)

summary(df2.train_50.glm)

# anova – Analysis of Variance on the model result
df2.train_50.glm.anova<-anova(df2.train_50.glm,test="Chisq")
df2.train_50.glm.anova

plot(df2.train_50.glm)


#Predict
df2.test_50.predict<-predict(df2.train_50.glm, newdata = df2.test_50)
df2.test_50.predict

summary(df2.test_50.predict)

#confidence intervals
confint(df2.train_60.glm)

#Comparing Actual vs. Prediction:
df2.test_50.predict.k9=kmeans(df2.test_50.predict, centers=9)
df2.test_50.predict.k9

df2.test_50.ct.k9<-CrossTable(df2.test_50.predict.k9$cluster, df2.test_50_kmeans_k9$cluster, prop.chisq=TRUE)
df2.test_50.ct.k9

confusion_matrix=df2.test_50.ct.k9$t
confusion_matrix


length(df2.test_50_kmeans_k9$cluster)

length(df2.test_50.predict.k9$cluster)

df2.test_60.ct.k9

# Calculating the metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
specificity <- sapply(1:nrow(confusion_matrix), function(i) {
  TN <- sum(confusion_matrix[-i, -i])
  FP <- sum(confusion_matrix[-i, i])
  TN / (TN + FP)
})
error <- 1 - accuracy


# Printing the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("Error:", error, "\n")
###########################KNN with K=9 and 50-50 split ends###########################




######################Analysis for 70-30 split#################
#Linear modeling
df2.train_70.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight + 
                        Viscera.weight + Shell.weight,family=gaussian, data=df2.train_70)
summary(df2.train_70.glm)
plot(df2.train_70.glm)


#Predict functions
df2.test_70.predict<-predict(df2.train_70.glm, newdata = df2.test_70)
df2.test_70.predict
summary(df2.test_70.predict)
summary(df2.test_70$Rings)

df2.train_70.predict<-predict(df2.train_70.glm, newdata = df2.train_70)
df2.train_70.predict
summary(df2.train_70.predict)
summary(df2.train_70$Rings)

# anova – Analysis of Variance on the model result
df2.train_70.glm.anova<-anova(df2.train_70.glm,test="Chisq")
df2.train_70.glm.anova

#confidence intervals
confint(df2.train_70.glm)
######################Analysis for 70-30 split#################


######################Analysis for 60-40 split#################
#Linear modeling
df2.train_60.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight + 
                        Viscera.weight + Shell.weight,family=gaussian, data=df2.train_60)
summary(df2.train_60.glm)
plot(df2.train_60.glm)

#Predict functions
df2.test_60.predict<-predict(df2.train_60.glm, newdata = df2.test_60)
df2.test_60.predict
summary(df2.test_60.predict)
summary(df2.test_60$Rings)

df2.train_60.predict<-predict(df2.train_60.glm, newdata = df2.train_60)
df2.train_60.predict
summary(df2.train_60.predict)
summary(df2.train_60$Rings)

# anova – Analysis of Variance on the model result
df2.train_60.glm.anova<-anova(df2.train_60.glm,test="Chisq")
df2.train_60.glm.anova

#confidence intervals
confint(df2.train_60.glm)
######################Analysis for 60-40 split#################


######################Analysis for 50-50 split#################
#Linear modeling
df2.train_50.glm= glm(formula = Rings ~ Length + Diameter + Height+ Whole.weight + 
                        Viscera.weight + Shell.weight,family=gaussian, data=df2.train_50)
summary(df2.train_50.glm)
plot(df2.train_50.glm)

#Predict functions
df2.test_50.predict<-predict(df2.train_50.glm, newdata = df2.test_50)
df2.test_50.predict
summary(df2.test_50.predict)
summary(df2.test_50$Rings)


df2.train_50.predict<-predict(df2.train_50.glm, newdata = df2.train_50)
df2.train_50.predict
summary(df2.train_50.predict)
summary(df2.train_50$Rings)

# anova – Analysis of Variance on the model result
df2.train_50.glm.anova<-anova(df2.train_50.glm,test="Chisq")
df2.train_50.glm.anova

#confidence intervals
confint(df2.train_50.glm)
######################Analysis for 50-50 split#################



