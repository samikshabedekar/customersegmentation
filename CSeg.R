library(readxl)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(corrgram)
library(corrplot)
library(cluster) 
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)
library(ggcorrplot)


sales <- read_csv("C:/Users/Samiksha/Desktop/Mall_Customers.csv")
#View(sales)
glimpse(sales)


#Cleaning the Data
any(is.na(sales))
is.na(sales)
colSums(is.na(sales))
sales$`Annual Income (k$)`[sales$`Annual Income (k$)`==""] <- NA
sales$`Spending Score (1-100)`[sales$`Spending Score (1-100)`==""] <- NA
colSums(is.na(sales))
which(is.na(sales$`Annual Income (k$)`))
which(is.na(sales$`Spending Score (1-100)`))
sales$`Annual Income (k$)`<- ifelse(is.na(sales$`Annual Income (k$)`), mean(sales$`Annual Income (k$)`, na.rm=TRUE), sales$`Annual Income (k$)`)
sales$`Spending Score (1-100)`<- ifelse(is.na(sales$`Spending Score (1-100)`), mean(sales$`Spending Score (1-100)`, na.rm=TRUE),sales$`Spending Score (1-100)`)
colSums(is.na(sales))






#Visualizations
a=table(sales$Gender)
barplot(a,main="Count Of Male And Female Vistors ",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2))

b=table(sales$Age)
barplot(b,main="Customer Age Comparision",
        ylab="Count",
        xlab="Age",
        col="grey")

ggplot(sales, aes(Gender, 'AnnualIncome (k$)', col = Gender)) +
  geom_point(size = 2) + 
  scale_y_continuous(name = "Annual income per year") +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = 6 )


ggplot(sales,aes(Gender)) + geom_density(fill ="blue")

head(sales)

New=table(sales$Gender)
barplot(New,main=" BarPlot For Gender",
        ylab="Count",
        xlab="Gender",
        col=rainbow(10),
        legend=rownames(New))
pct=round(New/sum(New)*100)
lbs=paste(c("Male","Female")," ",pct,"%",sep=" ")



summary(sales$`Spending Score (1-100)`)

hist(sales$`Spending Score (1-100)`,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="gold",
     labels=TRUE)



pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,col = heat.colors(2),
      main="Customer Bifercation On basis of Gender")

c=table(sales$CustomerCity)
pct=round(c/sum(a)*100)
lbs=paste(c("Delhi","Mumbai","Kolkata","Bengaluru","Chennai")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(c,labels=lbs,
      main="Pie Chart Depicting City of Branches")


#Categorizing Data
sales$Gender = factor(sales$Gender, levels = c('Male', 'Female'), labels = c(0,1))
head(sales)
summary(sales)
sales$Gender<-as.numeric(sales$Gender)


#Corelation
glimpse(sales)

cor.matrix <- cor(sales[,c(2,3,5,4)], method = "pearson", use = "complete.obs")
ggcorrplot(cor.matrix, hc.order = TRUE, lab = TRUE)



#determining the number of clusters
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(sales[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

#from the above we derive that 4 is the appropriate amount amount of cluster from Elbow method, as the graph bends at 4^^

#From Silhoutte method
k2<-kmeans(sales[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(sales[,3:5],"euclidean")))

k3<-kmeans(sales[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(sales[,3:5],"euclidean")))

k4<-kmeans(sales[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(sales[,3:5],"euclidean")))

k5<-kmeans(sales[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(sales[,3:5],"euclidean")))

k6<-kmeans(sales[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(sales[,3:5],"euclidean")))

k7<-kmeans(sales[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(sales[,3:5],"euclidean")))

k8<-kmeans(sales[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(sales[,3:5],"euclidean")))

k9<-kmeans(sales[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(sales[,3:5],"euclidean")))

k10<-kmeans(sales[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(sales[,3:5],"euclidean")))

#Finding the optimal number of clusters using the above method
fviz_nbclust(sales[,3:5], kmeans, method = "silhouette")


#optimal number of clusters using Sihouette method is 7


#Gap Statistics method

set.seed(1)
stat_gap <- clusGap(sales[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

#Deciding the clusters
c<-(1+4+7)/3
print(c)
k4<-kmeans(sales[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
k4


#Plotting
set.seed(1)
ggplot(sales, aes(x =sales$`Annual Income (k$)`, y =sales$`Spending Score (1-100)`))+ 
  geom_point(stat = "identity", aes(color = as.factor(k4$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4","5"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4","Cluster 5")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")





#glimpse(sales)
#sales$Gender = factor(sales$Gender, levels = c('Male', 'Female'), labels = c(0,1))
#glimpse(sales)

k7<-kmeans(sales[,c(3,5)],6,iter.max=100,nstart=50,algorithm="Lloyd")
k7
#Plotting
set.seed(1)
ggplot(sales, aes(x =sales$Age, y = sales$`Spending Score (1-100)`)) + 
  geom_point(stat = "identity", aes(color = as.factor(k7$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4","5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4","Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")









































#hist(sales$Rating,
#     col="gold",
 #    main="Histogram to Show Count of Ratings",
  #   xlab="Rating",
   #  ylab="Frequency",
    # labels=TRUE)

qplot(x=sales$`Tax 5%`, y=sales$`Unit price`, data=sales, geom = "point")

qplot(x=sales$`Unit price`, y=sales$Quantity, data=sales, geom = "point")

qplot(x=sales$Age, y=sales$`Spending Score (1-100)`, data=sales, geom = "point")

qplot(sales$Quantity,y=sales$`Unit price`, data = sales, colour = sales$Gender, shape = sales$Gender)

qplot(sales$`Unit price` ,sales$Quantity,data = sales,geom= "boxplot", fill = sales$Gender)

qplot(sales$`Product line` ,sales$`Customer type`,data = sales,geom= "boxplot", fill = sales$`Customer type`)

qplot(sales$Quantity,sales$`Product line` , data = sales,geom= "boxplot", fill = sales$`Product line`)

qplot(sales$`Tax 5%`,sales$`Product line` , data = sales,geom= "boxplot", fill = sales$`Product line`)

qplot(sales$`Unit price`,sales$`Product line` , data = sales,geom= "boxplot", fill = sales$`Product line`)

qplot(sales$City ,sales$`Tax 5%` , data = sales,geom= "boxplot", fill = sales$`Tax 5%` )

ggplot(sales, aes(x = sales$`Unit price`, y = sales$`Tax 5%`)) +
  geom_point()



#determining the number of clusters








head(sales)
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(sales[,5:6],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

k4<-kmeans(
  sales[, (names(sales) %in% c("AnnualIncome (k$)","Spending Score(1-100)"))],4,iter.max=100,nstart=50,algorithm="Lloyd")
k4



#Plotting
set.seed(1)
ggplot(sales[, (names(sales) %in% c("AnnualIncome (k$)","Spending Score (1-100)"))], aes(x ='AnnualIncome (k$)', y = 'Spending Score (1-100)')) + 
  geom_point(stat = "identity", aes(color = as.factor(k4$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4","5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4","Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")



#model1<-lm(sales$`Annual Income (k$)`~ sales$`Spending Score (1-100)`+sales$Age, data=sales)
#summary(model1)


