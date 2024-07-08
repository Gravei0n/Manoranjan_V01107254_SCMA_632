# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))


# Load necessary packages
install.packages(c("psych", "factoextra", "cluster"))
library(psych)
library(factoextra)
library(cluster)

# Set the working directory and read the data
file_path <- "E:/VCU Bridge course/Survey.csv"  # Adjust the path if necessary
data <- read.csv(file_path)

# View the structure of the data
str(data)

# Convert necessary columns to numeric if possible, or exclude non-numeric columns
# Extract only the numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Handle missing values if any (e.g., by removing rows with NA values or imputing them)
numeric_data <- na.omit(numeric_data)

# Ensure the data is suitable for PCA and Factor Analysis (e.g., numeric data)
# Perform Principal Component Analysis (PCA)
pca_result <- prcomp(numeric_data, scale. = TRUE)
summary(pca_result)
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_var(pca_result, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Perform Factor Analysis
fa_result <- fa(numeric_data, nfactors = 2, rotate = "varimax")
print(fa_result)
fa.diagram(fa_result)

# Conduct Cluster Analysis
# Scale the data
data_scaled <- scale(numeric_data)

# Determine the optimal number of clusters
fviz_nbclust(data_scaled, kmeans, method = "wss")
fviz_nbclust(data_scaled, kmeans, method = "silhouette")

# Perform k-means clustering
set.seed(123)
kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)
print(kmeans_result)

# Visualize the clustering result
fviz_cluster(kmeans_result, data = data_scaled)

# Additional Cluster Analysis - Hierarchical Clustering
dist_matrix <- dist(data_scaled, method = "euclidean")
hclust_result <- hclust(dist_matrix, method = "ward.D2")
plot(hclust_result)
rect.hclust(hclust_result, k = 3, border = "red")
