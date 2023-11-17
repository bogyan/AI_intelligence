library(factoextra)
library(stats)
library(ggplot2)
library(ggfortify)
library(caret)
library(corrplot)
# df = read.csv('C:/Users/bogda/Downloads/wine.data')
df = wine

View(df)

# Change dependent variable to factor type
df$V1<- as.factor(df$V1)

Wine_class = df$V1
table(Wine_class)

# Separating independent variables from the dependent variable
df_clust <- df[2:14]
sapply(df_clust, class)

# All variables are either numeric or integer type which is good for K-means clustering

# Countplot

wine_DA <- data.frame(Class = Wine_class)

p <- ggplot(wine_DA, aes(x = Class)) +
  geom_bar(fill = c("#e1812c", "#3274a1", "#3a923a")) +
  labs(title = "Wine Class Count")

print(p)

# Pie chart

wine_percentage = round(table(Wine_class)/178*100,2)

pie(wine_percentage, 
    main = "Class Percentages",
    col = c("#e1812c", "#3274a1", "#3a923a"),  # Define colors
    labels = paste(names(wine_percentage), ": ", wine_percentage, "%"),
    border = "white",  # Add a white border
    clockwise = TRUE)

# Correlation matrix of independent variables

cor(df_clust)
corrplot(cor(df_clust), method = 'number')

# Scaling variables in order to increase the effectiveness of the k-means algorithm
df_clust_scale <- scale(df_clust)
df_clust_scale

# Setting the seed and determining the optimal number of clusters
set.seed(42)
fviz_nbclust(df_clust_scale,kmeans,method="wss") + geom_vline(xintercept = 3, linetype = 2) + 
  labs(subtitle = "Elbow Method N1")
fviz_nbclust(df_clust_scale,kmeans,method="silhouette") + labs(subtitle = "Silhouette Method N2")

# K-means clustering
kmeans_clust <- kmeans(df_clust_scale,centers = 3, nstart = 150 , iter.max = 150)

# Visualization
km.clust <- kmeans_clust$cluster
row.names(df_clust_scale) <- paste(df$V1,1:dim(df)[1],sep = "_")
fviz_cluster(list(data = df_clust_scale, cluster = km.clust))

table(km.clust,df$V1) #Wine class 1 is in cluster 2, wine class 2 is in cluster 3 and wine class 3 is 
# in cluster 1, let's rearrange:

km.clust_upd <- ifelse(km.clust == 2, 1, ifelse(km.clust == 3, 2, 3))
table(km.clust_upd,df$V1)

# Create a confusion matrix
conf_mat <- table(km.clust_upd, df$V1)
conf_mat

# Extract the confusion matrix values and calculate basic statistics
TP1 <- conf_mat[1, 1]
FN1 <- conf_mat[1, 2] + conf_mat[1, 3]
FP1 <- conf_mat[2, 1] + conf_mat[3, 1]
TN1 <- conf_mat[2, 2] + conf_mat[2, 3] + conf_mat[3, 2] + conf_mat[3, 3]

TP2 <- conf_mat[2, 2]
FN2 <- conf_mat[2, 1] + conf_mat[2, 3]
FP2 <- conf_mat[1, 2] + conf_mat[3, 2]
TN2 <- conf_mat[1, 1] + conf_mat[1, 3] + conf_mat[3, 1] + conf_mat[3, 3]
  
TP3 <- conf_mat[3, 3]
FN3 <- conf_mat[3, 1] + conf_mat[3, 2]
FP3 <- conf_mat[1, 3] + conf_mat[2, 3]
TN3 <- conf_mat[1, 1] + conf_mat[2, 2] + conf_mat[1, 2] + conf_mat[2, 1]

# Accuracy
accuracy1 = (TP1 + TN1) / (TP1 + TN1 + FP1 + FN1)
accuracy2 = (TP2 + TN2) / (TP2 + TN2 + FP2 + FN2)
accuracy3 = (TP3 + TN3) / (TP3 + TN3 + FP3 + FN3)

# Precision
precision1 <- TP1 / (TP1 + FP1)
precision2 <- TP2 / (TP2 + FP2)
precision3 <- TP3 / (TP3 + FP3)

# Recall
recall1 <- TP1 / (TP1 + FN1)
recall2 <- TP2 / (TP2 + FN2)
recall3 <- TP3 / (TP3 + FN3)

# F1 score
f1_score1 <- 2 * (precision1 * recall1) / (precision1 + recall1)
f1_score2 <- 2 * (precision2 * recall2) / (precision2 + recall2)
f1_score3 <- 2 * (precision3 * recall3) / (precision3 + recall3)

# Specificity 
specificity1 <- TN1 / (TN1 + FP1)
specificity2 <- TN2 / (TN2 + FP2)
specificity3 <- TN3 / (TN3 + FP3)


# Table of values
Wine_classes <- c("Class_1", "Class_2", "Class_3")
accuracy <- c(accuracy1, accuracy2, accuracy3)
precision <- c(precision1, precision2, precision3)
recall <- c(recall1, recall2, recall3)
f1_score <- c(f1_score1, f1_score2, f1_score3)
specificity <- c(specificity1, specificity2, specificity3)

model_performance <- data.frame(Model = Wine_classes, Accuracy = accuracy, Precision = precision, Recall = recall,
                                F1_score = f1_score, Specificity = specificity)

View(model_performance)

