library(ggplot2);theme_set(theme_bw())
library(dplyr)
library(skmeans)
library(gridExtra)
customers <- read.csv("C:/Users/ADMIN/Downloads/Mall_Customers.csv")
attach(customers)
dim(customers)
str(customers)
n_distinct(CustomerID)
class(Gender)
table(Gender)
as.data.frame(Gender)
as.vector(table(Gender))
as.data.frame(table(Gender))  %>% 
  ggplot(aes(x = Gender, y = Freq))  +
  geom_bar(stat = "identity", fill = "#F8766D")+
  geom_text(y = as.vector(table(Gender)), 
          label = paste0((as.vector(table(Gender))/sum(as.vector(table(Gender))))*100, "%"))
summary(Age)
ggplot(as.data.frame(Age), aes(y = Age)) + geom_boxplot(fill='#F8766D')
ggplot(customers, aes( x = Age,stat(count), fill = Gender)) + geom_density(alpha = 0.4)
p1 <- ggplot(as.data.frame(Annual.Income..k..), aes(y = Annual.Income..k..))+
             geom_boxplot(fill='#F8766D') + ylim(c(1,150))
p1
p2 <- ggplot(as.data.frame(Spending.Score..1.100.), aes(y = Spending.Score..1.100.)) + 
             geom_boxplot(fill='#00BFC4') + ylim(c(1,150))
p2
grid.arrange(p1, p2, ncol = 2)
ggplot(customers, aes( x = Spending.Score..1.100.,y = stat(count), fill = Gender)) +
  geom_density(alpha = 0.4)
ggplot(customers, aes( x = Annual.Income..k..,y = stat(count), fill = Gender)) +
  geom_density(alpha = 0.9)
Kdata <- customers[,c(4,5)]
tot.withinss <- vector("numeric", length = 10)
for (i in 1:10){
  kDet <- kmeans(Kdata, i)
  tot.withinss[i] <- kDet$tot.withinss
}
print(kDet)
print(kDet$betweenss)
print(kDet$tot.withinss)
summary(kDet)
ggplot(as.data.frame(tot.withinss), aes(x = seq(1,10), y = tot.withinss)) + 
  geom_point(col = "#F8766D") +    
  geom_line(col = "#F8766D") + 
  theme(axis.title.x.bottom = element_blank()) +
  ylab("Within-cluster Sum of Squares") +
  xlab("Number of Clusters") +
  ggtitle("Elbow K Estimation")
customerClusters <- kmeans(Kdata, 5)
customerClusters
ggplot(Kdata, aes(x = Annual.Income..k.., y = Spending.Score..1.100.)) + 
          geom_point(stat = "identity", aes(color = as.factor(customerClusters$cluster))) +
                           scale_color_discrete(name=" ",
                           breaks=c("1", "2", "3", "4", "5"),
                labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")) +
                    ggtitle("Mall Customer Segmens", subtitle = "K-means Clustering")
