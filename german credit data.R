install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
install.packages("factoextra")

library(ggplot2)
library(dplyr)
library(corrplot)
library(factoextra)



#Loading in file
data=read.csv("C:/Users/Tejaswini yadav/Desktop/bank data/german_credit_data.csv")

#Dimensions of data
dim(data)
    
#Features of data
head(data)


#structure of data
str(data)


#Renaming credit amount,Checking account and Saving account
data<-rename(data,CreditAmount=Credit.amount)
data<-rename(data,Savingaccount=Saving.accounts)
data<-rename(data,CheckingAccount=Checking.account)


#Removing the first column which shows index 
data <- data[, -1]
head(data)


#Checking for null values
any(is.na(data))

# Print the number of missing values in each column 
colSums(is.na(data))

# Print the data types of each column 
print(sapply(data, class))

#Exploratory data analysis


# Plot Themes
bold_axis <- element_text(face = "bold", color = "black", size = 20)
axis_text <- element_text(face = "bold", size = 14)

#Gender Breakdown
# Create a bar plot for a categorical variable
ggplot(data, aes(x = Sex)) +
  geom_bar() +
  xlab("Sex") +
  ylab("Count") +
  ggtitle("Distribution of Sex")

#count of males and females
gender_counts <- table(data$Sex)
gender_counts

# Plot Customers by Age
Plotage <- ggplot(data,aes(x=Age))
Plotage + geom_histogram(fill="blue", alpha = 0.7) + theme(axis.text = axis_text) + theme(title = bold_axis) + ggtitle("Histogram of Customer Age")


#Plot by CreditAmount
PlotCredit <- ggplot(data, aes(x = CreditAmount))
PlotCredit + geom_histogram(fill="pink", alpha = 0.8) + theme(axis.text = axis_text) + theme(title = bold_axis) + ggtitle("Histogram of Credit Amount")
mean(data$CreditAmount)
sd(data$CreditAmount)


#Plot by Duration
PlotDuration <- ggplot(data, aes(x = Duration))
PlotDuration + geom_histogram(fill="Purple", alpha = 0.8) + theme(axis.text = axis_text) + theme(title = bold_axis) + ggtitle("Histogram of Duration")
mean(data$Duration)
sd(data$Duration)


#InDepth Data Exploration(Relationship between variables)

# Correlation between variables
cor_matrix <- cor(data[c("Age", "CreditAmount", "Duration")])

cor_matrix
# Plot the correlogram
corrplot(cor_matrix, method = "number")

#Scattered plot of CreditAmount and Duration
ggplot(data,aes(x = CreditAmount, y = Duration, col=Sex))+geom_point()

#Scattered plot of Age and Duration
ggplot(data,aes(x =Age, y = Duration, col=Sex))+geom_point()

#Scattered plot of CreditAmount and Age
ggplot(data,aes(x = Age, y = CreditAmount,col=Sex))+geom_point()

#Checking the linear relationship between credit amount and duration
scatter2 <- ggplot(data, aes(x = CreditAmount, y = Duration))  + geom_point(aes(color = factor(Sex)))
scatter2 +geom_smooth(method = "lm", color ="black") + theme(axis.text = axis_text) + theme(title = bold_axis)

##histogram of Gender and Spending Score
#Clustering with KMeans

# Select numerical columns for K-means clustering
numerical_cols <- data[c("Age", "CreditAmount", "Duration")]



# Create histograms for the columns
ggplot(data, aes(x = Age)) +
  geom_histogram( fill = "blue", color = "white") +
  xlab("Age") +
  ylab("Frequency") +
  ggtitle("Histogram of Age")


ggplot(data, aes(x = CreditAmount)) +
  geom_histogram( fill = "green", color = "white") +
  xlab("Credit Amount") +
  ylab("Frequency") +
  ggtitle("Histogram of Credit Amount")

ggplot(data, aes(x = Duration)) +
  geom_histogram( fill = "orange", color = "white") +
  xlab("Duration") +
  ylab("Frequency") +
  ggtitle("Histogram of Duration")


#Standardizing the Variables
scaled_data <- select(data, c(Age, CreditAmount, Duration))
scaled_data <- as.data.frame(scale(scaled_data))
scaled_data

# k-means clustering
##K Cluster Model
set.seed(101)
Cluster1 <- kmeans(scaled_data[,1:3],2,nstart=100)
print(Cluster1)
# deciding optimal number of cluster
# Elbow method
fviz_nbclust(scaled_data, kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)+labs(subtitle = "Elbow method")

#Average Silhouette Method
set.seed(101)
fviz_nbclust(scaled_data, kmeans, method = "silhouette")

#Gap Statistic Method
set.seed(101)
fviz_nbclust(scaled_data, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")




#Adjusting Kmeans Model
set.seed(101)
kmeans_result <- kmeans(scaled_data[,1:3],3,iter.max=100, nstart=100)
kmeans_result

plot(scaled_data[,1:3], col=kmeans_result$cluster)

kmeans_result$centers

# Visualize the clusters
fviz_cluster(kmeans_result, data = scaled_data, geom = "point",
             frame = FALSE, stand = FALSE, pointsize = 2) +
  ggtitle("K-means Clustering (k = 3)")

# making cluster as factor
kmeans_result$cluster <- as.factor(kmeans_result$cluster)
# assgining cluster to the original  data set
data.clust <- cbind(numerical_cols, cluster = kmeans_result$cluster)

# Aggregate the clustered data by cluster and calculate the means
cluster_means <- aggregate(data.clust[, 1:ncol(data.clust) - 1], 
                           by = list(cluster = data.clust$cluster), 
                           mean)

# Print the cluster means
print(cluster_means)

