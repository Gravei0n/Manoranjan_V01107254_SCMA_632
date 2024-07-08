# Set a CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))


install.packages("MASS")  # For MDS function
install.packages("ggplot2")  # For visualization
library(MASS)
library(ggplot2)

icecream_data <- read.csv("E:/VCU Bridge course/icecream.csv")

numeric_data <- icecream_data[, -1]  # Assuming the first column is non-numeric (Brand names)
scaled_data <- scale(numeric_data)

distance_matrix <- dist(scaled_data)

mds_result <- isoMDS(distance_matrix)

mds_points <- as.data.frame(mds_result$points)
colnames(mds_points) <- c("Dim1", "Dim2")
mds_points$Brand <- icecream_data$Brand

ggplot(mds_points, aes(x = Dim1, y = Dim2, label = Brand)) +
  geom_point() +
  geom_text(vjust = 1, hjust = 1) +
  ggtitle("MDS of Ice Cream Brands") +
  xlab("Dimension 1") +
  ylab("Dimension 2")
