# Wine Dataset Analysis

# Business Problem: We have a dataset with thousands of wines, and the client wants answers
# to specific questions. Answers must be presented in the most visual format possible, i.e., graphs!

# Note: Ensure the dataset files exist at the specified paths or update paths accordingly.
# Required files: "winequality2.csv" and "dados/winequality-types.csv"



# Setting up the working directory
setwd("C:/Users/omjai/BDA/Wine_Quality_Predictions/first part")
getwd()

# Loading required packages
library(plotly)  # For interactive visualizations
library(dplyr)   # For data manipulation
library(shiny)   # For interactive web applications
library(DMwR)    # For data mining tasks
library(hopkins) # For Hopkins statistic (replacing deprecated clustertend)
library(NbClust) # For determining number of clusters
library(corrplot)# For correlation plots
library(caret)   # For predictive modeling

# Loading the dataset
df <- read.csv("C:/Users/omjai/BDA/Wine_Quality_Predictions/winequality2.csv")

# Viewing the dataset
View(df)

# Remove the 'ID' column for better analysis
df2 <- df[,-1]
View(df2)

# Checking the data types of each column
str(df2)
# Getting the first insights with a statistical summary of the dataset
summary(df2)

# Storing the column names in a variable
colunas <- names(df2)

# Creating a histogram for each column
for (col in seq(1, length(names(df2)), 1)) {
  hist(df2[,col], col = 'darkred', main = paste('Histogram of', colunas[col]), xlab = colunas[col])
}

# Client Questions:

# 1. In which pH range are the best wines?
# Assumption: Best wines have ratings between 6 and 9. Create a subset with wines in this range.
df_best <- df2[df2$quality > 5, c(9,12)]  # pH (column 9) and quality (column 12)
# Place the pH values into bins for easier visualization
min(df_best$pH)
max(df_best$pH)
df_best$pH_group <- cut(df_best$pH, seq(2.7, 4.1, 0.1))
View(df_best)

# Group the data by pH bins and quality
df_best_notas <- as.data.frame(with(df_best, table(df_best$pH_group, df_best$quality)))
colnames(df_best_notas) <- c('pH', 'nota', 'freq')
View(df_best_notas)
# Separate the dataset by ratings
df_best_nota6 <- df_best_notas[df_best_notas$nota == 6,]
df_best_nota7 <- df_best_notas[df_best_notas$nota == 7,]
df_best_nota8 <- df_best_notas[df_best_notas$nota == 8,]
df_best_nota9 <- df_best_notas[df_best_notas$nota == 9,]

# Plotting the result
fig <- plot_ly(data = df_best_nota6, x = ~pH, y = ~freq, type = 'bar', name = 'nota6', alpha = 0.5)
fig <- fig %>% add_trace(data = df_best_nota7, y = ~freq, name = 'nota7', alpha = 0.6)
fig <- fig %>% add_trace(data = df_best_nota8, y = ~freq, name = 'nota8', alpha = 0.7)
fig <- fig %>% add_trace(data = df_best_nota9, y = ~freq, name = 'nota9', alpha = 0.9)
fig <- fig %>% layout(title = 'Ratings by pH Range', barmode = 'overlay')
fig

# Showing an overview of ratings by pH with a boxplot
boxplot(pH ~ quality, data = df2, main = "Boxplot Rating vs pH", xlab = "Rating", ylab = "pH", col = 'darkred')

# Conclusion: The ideal pH for white wines is 3.1–3.4, and for red wines, 3.3–3.6. The client's wines align with these ranges, with the best ratings in these pH levels.

# 2. Does higher alcohol content correlate with better or worse wine ratings?
# Create a subset with wines rated 6–9 and alcohol content
df_alcool <- df2[df2$quality > 5, c(11,12)]  # Alcohol (column 11) and quality (column 12)
# Group the data by bins
min(df_alcool$alcohol)
max(df_alcool$alcohol)
df_alcool$groups <- cut(df_alcool$alcohol, seq(8.0, 14.2, 1))
View(df_alcool)

df_alcool_notas <- as.data.frame(with(df_alcool, table(df_alcool$groups, df_alcool$quality)))
colnames(df_alcool_notas) <- c('alcohol', 'nota', 'freq')

# Separate the dataset by ratings
df_alcool_nota6 <- df_alcool_notas[df_alcool_notas$nota == 6,]
df_alcool_nota7 <- df_alcool_notas[df_alcool_notas$nota == 7,]
df_alcool_nota8 <- df_alcool_notas[df_alcool_notas$nota == 8,]
df_alcool_nota9 <- df_alcool_notas[df_alcool_notas$nota == 9,]

# Plotting the result
fig2 <- plot_ly(data = df_alcool_nota6, x = ~alcohol, y = ~freq, type = 'bar', name = 'nota6', alpha = 0.5)
fig2 <- fig2 %>% add_trace(data = df_alcool_nota7, y = ~freq, name = 'nota7', alpha = 0.7)
fig2 <- fig2 %>% add_trace(data = df_alcool_nota8, y = ~freq, name = 'nota8', alpha = 0.7)
fig2 <- fig2 %>% add_trace(data = df_alcool_nota9, y = ~freq, name = 'nota9', alpha = 0.9)
fig2 <- fig2 %>% layout(title = 'Ratings by Alcohol Content', barmode = 'overlay')
fig2

# Boxplot to check variation by rating
boxplot(alcohol ~ quality, data = df2, main = "Boxplot Rating vs Alcohol Content", xlab = "Rating", ylab = "Alcohol", col = 'darkred')
cor.test(df2$alcohol, df2$quality)

# Conclusion: The best ratings were given to wines with alcohol content between 11% and 13%.

# 3. What is the best range for total sulfur dioxide?
# Create a subset with wines rated 6–9 and total sulfur dioxide
df_dioxide <- df2[df2$quality > 5, c(7,12)]  # Total sulfur dioxide (column 7) and quality (column 12)
# Group the data by bins
min(df_dioxide$total.sulfur.dioxide)
max(df_dioxide$total.sulfur.dioxide)
table(df_dioxide$total.sulfur.dioxide, df_dioxide$quality)
df_dioxide$groups <- cut(df_dioxide$total.sulfur.dioxide, seq(5, 295, 10))
View(df_dioxide)

df_dioxide_notas <- as.data.frame(with(df_dioxide, table(df_dioxide$groups, df_dioxide$quality)))
colnames(df_dioxide_notas) <- c('total_dioxide', 'nota', 'freq')

# Separate the dataset by ratings
df_dioxide_nota6 <- df_dioxide_notas[df_dioxide_notas$nota == 6,]
df_dioxide_nota7 <- df_dioxide_notas[df_dioxide_notas$nota == 7,]
df_dioxide_nota8 <- df_dioxide_notas[df_dioxide_notas$nota == 8,]
df_dioxide_nota9 <- df_dioxide_notas[df_dioxide_notas$nota == 9,]

# Plotting the result (corrected to use correct datasets for nota8 and nota9)
fig3 <- plot_ly(data = df_dioxide_nota6, x = ~total_dioxide, y = ~freq, type = 'bar', name = 'nota6', alpha = 0.6)
fig3 <- fig3 %>% add_trace(data = df_dioxide_nota7, y = ~freq, name = 'nota7', alpha = 0.7)
fig3 <- fig3 %>% add_trace(data = df_dioxide_nota8, y = ~freq, name = 'nota8', alpha = 0.7)
fig3 <- fig3 %>% add_trace(data = df_dioxide_nota9, y = ~freq, name = 'nota9', alpha = 0.9)
fig3 <- fig3 %>% layout(title = 'Ratings by Total Sulfur Dioxide', barmode = 'overlay')
fig3

# Boxplot (corrected y-axis label)
boxplot(total.sulfur.dioxide ~ quality, data = df2, main = "Boxplot Rating vs Total Sulfur Dioxide", xlab = "Rating", ylab = "Total Sulfur Dioxide", col = 'darkred')

# Conclusion: Best ratings were for wines with total sulfur dioxide between 95 and 135 mg/L.
# EU limits are 160 mg/L for reds and 210 mg/L for whites. Only 8 wines exceed 210 mg/L.
# Some highly rated wines have near-zero sulfur dioxide.

# 4. Is there a correlation between sulfate level and wine rating?
# Create a subset with wines rated 6–9 and sulfates
df_sulphate <- df2[df2$quality > 5, c(10,12)]  # Sulfates (column 10) and quality (column 12)
# Group the data by bins
min(df_sulphate$sulphates)
max(df_sulphate$sulphates)
df_sulphate$groups <- cut(df_sulphate$sulphates, seq(0.22, 1.95, 0.2))
View(df_sulphate)

df_sulphate_notas <- as.data.frame(with(df_sulphate, table(df_sulphate$groups, df_sulphate$quality)))
colnames(df_sulphate_notas) <- c('sulphates', 'nota', 'freq')

# Separate the dataset by ratings
df_sulphate_nota6 <- df_sulphate_notas[df_sulphate_notas$nota == 6,]
df_sulphate_nota7 <- df_sulphate_notas[df_sulphate_notas$nota == 7,]
df_sulphate_nota8 <- df_sulphate_notas[df_sulphate_notas$nota == 8,]
df_sulphate_nota9 <- df_sulphate_notas[df_sulphate_notas$nota == 9,]

# Plotting the result
fig4 <- plot_ly(data = df_sulphate_nota6, x = ~sulphates, y = ~freq, type = 'bar', name = 'nota6', alpha = 0.6)
fig4 <- fig4 %>% add_trace(data = df_sulphate_nota7, y = ~freq, name = 'nota7', alpha = 0.7)
fig4 <- fig4 %>% add_trace(data = df_sulphate_nota8, y = ~freq, name = 'nota8', alpha = 0.7)
fig4 <- fig4 %>% add_trace(data = df_sulphate_nota9, y = ~freq, name = 'nota9', alpha = 0.9)
fig4 <- fig4 %>% layout(title = 'Ratings by Sulfate Levels', barmode = 'overlay')
fig4

# Boxplot (corrected y-axis label)
boxplot(sulphates ~ quality, data = df2, main = "Boxplot Rating vs Sulfates", xlab = "Rating", ylab = "Sulfates", col = 'darkred')

# Conclusion: Best ratings were for wines with sulfate levels between 0.42 and 0.62.
# Lower sulfite levels are preferred, as research suggests reducing sulfites in the future.

# 5. What are the strongest correlations between variables and quality?
# Compute the correlation between all variables
correlacao <- cor(df2)

# Plotting the result
corrplot(correlacao, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot.mixed(correlacao, lower.col = "black")

# Findings: Strongest correlations with quality are alcohol, volatile acidity, and density.
# Plot additional graphs for these variables
boxplot(density ~ quality, data = df2, main = "Boxplot Rating vs Density", xlab = "Rating", ylab = "Density", col = 'darkred')
boxplot(volatile.acidity ~ quality, data = df2, main = "Boxplot Rating vs Volatile Acidity", xlab = "Rating", ylab = "Volatile Acidity", col = 'darkred')

# Note: Volatile acidity, when high, gives a vinegar-like aroma, often due to poor winemaking practices.

# Clustering: Separating Red and White Wines
# Problem: The dataset contains both white and red wines, but the client does not know which is which.
# Use KMeans to separate into two clusters.
sum(is.na(df2))  # Check for missing data

# Version 1: Clustering using quality
df_tocluster <- df2[,13, drop = FALSE]  # Quality column (column 13), keep as data frame
# Calculate how clusterable the dataframe is. > 0.5 = No | < 0.5 = Yes
hopkins(df_tocluster, n = nrow(df_tocluster) - 1)

# Calculate the ideal number of clusters
num_clust <- NbClust(df_tocluster, distance = 'euclidean',
                     min.nc = 2, max.nc = 5,
                     method = 'kmeans',
                     index = 'silhouette')
num_clust$Best.nc  # Suggests 4 clusters, but use 2 for red/white

# KMeans clustering
set.seed(123)  # For reproducibility
modelo <- kmeans(df_tocluster, 2)
print(modelo)
df$cluster <- modelo$cluster
View(df)

# Load reference dataset with wine types
key_df <- read.csv('dados/winequality-types.csv')
key_df <- key_df[,c(1,15)]
key_df$cluster <- ifelse(key_df$Type == 'White', 1, 2)

# Check model accuracy
df$check <- ifelse(key_df$cluster == df$cluster, 1, 0)
acuracia <- sum(df$check) / nrow(df)
cat('Model accuracy: ', acuracia * 100, '%')  # 98%

# Version 2: Clustering using pH, alcohol, and quality
df_tocluster2 <- df2[,c(9,11,13)]  # pH, alcohol, quality
hopkins(df_tocluster2, n = nrow(df_tocluster2) - 1)  # 0.17 (clusterable)

# Calculate the ideal number of clusters
num_clust <- NbClust(df_tocluster2, distance = 'euclidean',
                     min.nc = 2, max.nc = 5,
                     method = 'kmeans',
                     index = 'silhouette')
num_clust$Best.nc  # Suggests 2 clusters

# KMeans clustering
set.seed(123)  # For reproducibility
modelo2 <- kmeans(df_tocluster2, 2)
df$cluster <- modelo2$cluster

# Load reference dataset
key_df2 <- read.csv('dados/winequality-types.csv')
key_df2 <- key_df2[,c(1,15)]
View(key_df2)
key_df2$cluster <- ifelse(key_df2$Type == 'White', 1, 2)

# Check model accuracy
df$check <- ifelse(key_df2$cluster == df$cluster, 1, 0)
acuracia <- sum(df$check) / nrow(df)
cat('Model accuracy: ', acuracia * 100, '%')  # 98%
View(df[df$check == 0,])

# Dimensionality Reduction
df_pca <- prcomp(df[,c(2:14)], center = TRUE, scale = TRUE)
# Plot PCA results
plot(df_pca$x[,1], df_pca$x[,2], col = df$cluster)

# Predictive Modeling for White Wines
# Goal: Create predictions for white wine ratings (Low: 3–6, High: 7–9)
# Separate white wines based on cluster
df_whites <- df[df$cluster == 1,]
df_whites <- df_whites[,-c(1,14,15,16)]  # Remove ID, cluster, and other columns

# Transform quality into binary classes
df_whites$qualityBin <- ifelse(df_whites$quality >= 7, 'High', 'Low')
df_whites$qualityBin <- as.factor(df_whites$qualityBin)
View(df_whites)

# Check for missing data
sum(is.na(df_whites))  # No missing data

# Balance dataset using SMOTE
smote_data <- SMOTE(qualityBin ~ ., df_whites, perc.over = 650, k = 5, perc.under = 115)
prop.table(table(smote_data$qualityBin))
View(smote_data)

# Split data into training (90%) and test sets
set.seed(123)  # For reproducibility
index <- sample(x = nrow(smote_data), size = 0.9 * nrow(smote_data), replace = FALSE)
df_train <- as.data.frame(smote_data[index, -12])
df_test <- as.data.frame(smote_data[-index, -12])
View(df_train)

# Classification
TrainingParameters <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

# Random Forest model
model_rf <- train(qualityBin ~ alcohol + volatile.acidity + density + chlorides + pH, data = df_train,
                  method = 'rf', metric = 'Accuracy',
                  trControl = TrainingParameters)
print(model_rf)  # 95% accuracy

# AdaBoost model (note: potential error in original code with 'Class'; corrected to 'qualityBin')
model_ada <- train(qualityBin ~ alcohol + volatile.acidity + density + chlorides + pH, data = df_train,
                   method = 'adaboost')
print(model_ada)  # 85% accuracy

# Evaluate Random Forest on test set
y_test <- as.factor(df_test[,12])
X_test <- df_test[,-12]
predictions <- predict(model_rf, X_test)
confusionMatrix(predictions, y_test)  # 96% accuracy

# Save the Random Forest model for use in a Shiny app
saveRDS(model_rf, "model_rf_wines.rds")