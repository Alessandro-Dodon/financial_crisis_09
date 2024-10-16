################################################################################
#Load Libraries and suppress warnings
suppressWarnings(suppressMessages({
  library(reshape2)
  library(car)
  library(stats)
  library(ggfortify)
  library(knitr)
  library(kableExtra)
  library(scales)
  library(ggrepel)
  library(Rtsne)
  library(tidyverse)  # This will load readr, dplyr, tidyr, ggplot2 and some others
}))

# Reading the CSV files suppressing warnings
suppressWarnings(suppressMessages({
  file_path <- "/Users/aledo/Desktop/R_PROJECT_3 YEAR_PRO_2/Unzipped_data/"
  files <- c(
    "API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_5871639.csv",  # GDP growth (annual %)
    "API_CM.MKT.LCAP.GD.ZS_DS2_en_csv_v2_5873240.csv",  # Market capitalization of listed domestic companies (% of GDP)
    "API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_5871597.csv",     # Inflation, consumer prices (annual %)
    "API_GC.DOD.TOTL.GD.ZS_DS2_en_csv_v2_5872587.csv",  # Central government debt, total (% of GDP)
    "API_BX.KLT.DINV.WD.GD.ZS_DS2_en_csv_v2_5994659.csv",  # Foreign direct investment, net inflows (% of GDP)
    "API_SL.UEM.TOTL.NE.ZS_DS2_en_csv_v2_5996644.csv",   # Unemployment, total (% of total labor force) (national estimate)
    "API_BN.CAB.XOKA.CD_DS2_en_csv_v2_5873206.csv",
    "API_NE.GDI.TOTL.ZS_DS2_en_csv_v2_5873774.csv",
    "API_FM.LBL.BMNY.GD.ZS_DS2_en_csv_v2_5873493.csv",
    "API_GC.REV.XGRT.GD.ZS_DS2_en_csv_v2_5873527.csv",
    "API_NY.GNS.ICTR.ZS_DS2_en_csv_v2_5873909.csv",
    "API_FR.INR.LEND_DS2_en_csv_v2_5873491.csv",
    "API_NE.CON.TOTL.ZS_DS2_en_csv_v2_5995951.csv"
  )
  
  data_list <- lapply(files, function(file) {
    read_csv(paste0(file_path, file), skip = 3)
  })
}))

################################################################################
# Correlation plot
# Define country of interest and years of interest
country_of_interest <- "United States"
years_of_interest <- 1990:2020

# Filter, reshape, and combine the data list into one wide-format dataframe
combined_data_correlation_plot <- bind_rows(lapply(data_list, function(df) {
  df %>%
    filter(`Country Name` == country_of_interest) %>%
    select(`Country Name`, `Country Code`, `Indicator Name`, all_of(as.character(years_of_interest)))
})) %>%
  pivot_longer(cols = as.character(years_of_interest), names_to = "Year", values_to = "Value") %>%
  pivot_wider(names_from = `Indicator Name`, values_from = "Value", id_cols = "Year") %>%
  mutate(Year = as.numeric(Year))  # Convert Year to numeric if it's not already

# Calculate correlations
cor_matrix <- cor(combined_data_correlation_plot[-1])  # Exclude the 'Year' column

# Melt the correlation matrix
cor_melted <- melt(cor_matrix)

# Create ggplot heatmap
heatmap_plot_correlation <- ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = round(value, 2)), size = 2) +  # Adjust text size
  scale_fill_gradient2(low = "blue", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),  # Perpendicular text on x-axis
        axis.text.y = element_text(angle = 0, vjust = 0.5, size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        aspect.ratio = 1,  # Ensure the plot is square
        plot.margin = margin(10, 10, 10, 10)) +  # Adjust plot margins
  labs(x = "Indicator", y = "Indicator", fill = "Correlation")

# Print the heatmap
print(heatmap_plot_correlation) #Enlarge the plot with the zoom function for interpretation

################################################################################
# Heatmap business cycles
# Re-load the data
gdp_growth_data <- data_list[[1]]

# List of countries for which we want to plot the heatmap (adjust accordingly)
heatmap_business_cycles_countries <- c("United States", "Japan", "Germany", "China", "Greece", "Iceland", "Spain", 
                       "Ireland", "Portugal", "Italy", "France", "United Kingdom", "Russian Federation", 
                       "India", "Brazil", "Ukraine", "Australia", "Canada", "Mexico", 
                       "Indonesia", "Turkey", "Estonia", "Saudi Arabia", "Sweden", 
                       "Netherlands", "Switzerland", "Argentina", "Nigeria", "Poland", "Egypt")

# Select the data for the chosen countries and years 1960-2022
heatmap_data_selected <- data_list[[1]] %>%
  filter(`Country Name` %in% heatmap_business_cycles_countries) %>%
  select(`Country Name`, `1960`:`2022`) # Include years 2021 and 2022

# Reshape the data for plotting
heatmap_data_melted <- melt(heatmap_data_selected, id.vars = "Country Name")

# Create the heatmap with square tiles and white borders, and a distinct color for NA values
gdp_growth_heatmap <- ggplot(heatmap_data_melted, aes(x = variable, y = `Country Name`, fill = value)) +
  geom_tile(color = "white") + # White borders and square tiles
  scale_fill_gradientn(
    colors = c("red", "orange", "white", "lightblue", "blue"), # Adjusted color scheme to blue shades
    values = rescale(c(-10, 0, 15)), # Set the limits for the color gradient
    limits = c(-10, 15), # Set the limits for the data
    na.value = "grey50", # Color for NA values
    name="GDP Growth (%)"
  ) +
  scale_x_discrete(breaks = c("1960", "1970", "1980", "1990", "2000", "2010", "2020")) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(size = 7), # Smaller text size for country names
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank(), # Remove y-axis title
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold") # Bolder, more prominent title
  ) +
  labs(x = "", y = "", title = "Visualization of Business Cycles (1960-2022)") # Updated title

# Display the heatmap
print(gdp_growth_heatmap) #Enlarge the plot with the zoom function for interpretation

################################################################################
# Create dataframe for 2009
# Extract data for 2009 from the first six datasets
data_crises <- lapply(data_list[1:6], function(df) {
  df %>%
    select(`Country Name`, `2009`)
})

# Naming the datasets for clarity, corresponding to the first six datasets
names(data_crises) <- c("GDP Growth", "Market Capitalization", "Inflation", "Government Debt", "Foreign Investment", "Unemployment")

# Merging the datasets on Country Name
data_merged_crises <- Reduce(function(x, y) merge(x, y, by = "Country Name", all = TRUE), data_crises)

# Renaming columns for clarity
colnames(data_merged_crises) <- c("Country Name", "GDP Growth (2009)", "Market Capitalization (2009)", "Inflation (2009)", "Government Debt (2009)", "Foreign Investment (2009)", "Unemployment (2009)")

# Remove rows with any NA values
data_cleaned_crises <- na.omit(data_merged_crises)

# Create a copy of the data before standardization
data_cleaned_crises_original <- data_cleaned_crises

# Standardize the numeric variables
data_cleaned_crises[, -1] <- scale(data_cleaned_crises[, -1])

# View the standardized data frame
View(data_cleaned_crises)

################################################################################
# K-means clustering (with original standardized data)
# Create a copy of the data to preserve the original dataframe
data_for_clustering <- data_cleaned_crises

# Elbow Method to determine the optimal number of clusters
set.seed(123) # For reproducibility
wss <- numeric(15)
for (i in 1:15) {
  kmeans_result <- kmeans(data_for_clustering[, -1], centers = i, nstart = 20)
  wss[i] <- kmeans_result$tot.withinss
}
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# Choose the optimal number of clusters (adjust based on the elbow plot)
optimal_k <- 3 # Based on the elbow plot

# K-means clustering using the optimal number of clusters
kmeans_result <- kmeans(data_for_clustering[, -1], centers = optimal_k, nstart = 20)

# Create a table with Country Name and Cluster assignment without altering the original data
cluster_table <- data_cleaned_crises[, c("Country Name"), drop = FALSE]
cluster_table$Cluster <- as.factor(kmeans_result$cluster)

# Order the table by cluster number
cluster_table_ordered <- cluster_table[order(cluster_table$Cluster), ]

# View the ordered table
View(cluster_table_ordered)

################################################################################
# PCA 
# Perform PCA on the cleaned data for the crises year
pca_result_crises <- prcomp(data_cleaned_crises[, -1], center = TRUE, scale. = TRUE)
pca_result_crises$rotation 
pca_summary_crises <- summary(pca_result_crises)
print(pca_summary_crises) # Check the importance of components

# Choose the first 2 principal components for k-means clustering
data_pca_crises <- as.data.frame(pca_result_crises$x[, 1:2])

################################################################################
# PCA plot interpretation
# Extract the loadings (coefficients for each principal component)
loadings <- pca_result_crises$rotation[, 1:2]

# Create a dataframe from the loadings for easier use with ggplot2
loadings_df <- as.data.frame(loadings)
loadings_df$variable <- rownames(loadings)

# Plot the loadings as arrows, starting from the origin
plot_PCA_interpretation <- ggplot() +
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
               color = "black") +  # Black arrows with smaller tips
  geom_text(data = loadings_df, aes(x = PC1, y = PC2, label = variable), 
            nudge_x = 0.05, nudge_y = 0.05, color = "black") +  # Add labels for each variable
  theme_minimal() +
  labs(title = "Plot of PCA with Macroeconomic Indicators",
       x = "Principal Component 1 (29%)",
       y = "Principal Component 2 (24%)")

print(plot_PCA_interpretation) #Enlarge the plot with the zoom function for interpretation

################################################################################
# K-means clustering (with PCA)
# Clustering with k-means on the 2 PCA components
set.seed(123) # Setting seed for reproducibility
wss <- numeric(15)
for (i in 1:15) {
  kmeans_result_crises <- kmeans(data_pca_crises, centers = i, nstart = 20) # Use the correct variable
  wss[i] <- kmeans_result_crises$tot.withinss
}
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# Determine the optimal number of clusters from the elbow plot
optimal_k <- 3 # Adjust based on the elbow plot

# K-means clustering on the two PCA components
kmeans_result_crises <- kmeans(data_pca_crises, centers = optimal_k, nstart = 20) # Use the correct variable

# Add the cluster assignment to the original data frame
data_cleaned_crises$Cluster <- as.factor(kmeans_result_crises$cluster)

# Add the first two principal components to the data frame for plotting
data_cleaned_crises$PCA1 <- pca_result_crises$x[, 1] # Correct pca_result_crises
data_cleaned_crises$PCA2 <- pca_result_crises$x[, 2] # Correct pca_result_crises

# Improved color scheme for clusters
# Make sure the number of colors corresponds to the number of clusters
color_spectrum <- scale_color_manual(values = c("red", "orange", "blue")[1:optimal_k])

# Create a ggplot using the first two principal components for visualization
plot_PCA_k_means <- ggplot(data_cleaned_crises, aes(x = PCA1, y = PCA2, color = Cluster)) + # Use the correct data frame
  geom_point(alpha = 0.6, size = 2, shape = 16) +
  color_spectrum +
  labs(title = "K-Means Clustering on Principal Components (PC1 and PC2)",
       x = "Principal Component 1 (29%)",
       y = "Principal Component 2 (24%)") +
  theme_minimal() +
  theme(legend.position = "right")

# Use ggrepel to avoid overlapping text
plot_PCA_k_means <- plot_PCA_k_means + geom_text_repel(aes(label = `Country Name`), size = 3, max.overlaps = Inf)

# Print the plot
print(plot_PCA_k_means) #Enlarge the plot with the zoom function for interpretation

# Create a table with Country Name and Cluster assignment
cluster_table_pca <- data_cleaned_crises %>%
  select(`Country Name`) %>%
  mutate(Cluster = as.factor(kmeans_result_crises$cluster))

# Order the table by cluster number and view it
cluster_table_ordered_pca <- cluster_table_pca %>%
  arrange(Cluster)
View(cluster_table_ordered_pca)

################################################################################
# Hierarchical clustering (without PCA)
# Hierarchical Clustering on the original data
dist_matrix <- dist(data_for_clustering[, -1], method = "euclidean")
hc_crises <- hclust(dist_matrix, method = "complete")

# Plot the dendrogram with adjusted text size
par(cex = 0.5)  # Adjusts the text size
plot(hc_crises, labels = data_for_clustering$`Country Name`, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "") #Enlarge the plot with the zoom function for interpretation

# Reset par settings to default (if necessary for subsequent plots)
par(cex = 1)

################################################################################
# Hierarchical clustering (without PCA) and cutting into 3 clusters
# Hierarchical Clustering on the original data
dist_matrix <- dist(data_for_clustering[, -1], method = "euclidean")
hc_crises <- hclust(dist_matrix, method = "complete")

# Cut the dendrogram to create 3 clusters
cutree_clusters <- cutree(hc_crises, k = 3)

# Add cluster assignments to the original data
data_for_clustering$Cluster <- as.factor(cutree_clusters)

# Plot the dendrogram with adjusted text size and color branches by cluster
par(cex = 0.5)  # Adjusts the text size
plot(hc_crises, labels = data_for_clustering$`Country Name`, main = "Hierarchical Clustering Dendrogram - 2009", xlab = "") #Enlarge the plot with the zoom function for interpretation

# Color branches by cluster assignment
rect.hclust(hc_crises, k = 3, border = 2:4)

# Reset par settings to default (if necessary for subsequent plots)
par(cex = 1)

################################################################################
# Table to interpret the results (for k-means without PCA)
# Extract Country Name and Cluster from the non-PCA-based cluster assignments
cluster_assignments_non_pca <- cluster_table_ordered %>%
  select(`Country Name`, Cluster)

# Merge the non-PCA-based cluster assignments with the original 2009 data
data_merged_with_clusters_non_pca <- data_cleaned_crises_original %>%
  left_join(cluster_assignments_non_pca, by = "Country Name")

# Check for any NA values in Cluster and remove those rows
data_merged_with_clusters_non_pca <- na.omit(data_merged_with_clusters_non_pca)

# Order by Cluster
data_merged_with_clusters_non_pca <- data_merged_with_clusters_non_pca %>%
  arrange(Cluster)

# View the data frame with clusters and original values
View(data_merged_with_clusters_non_pca)

# Group by Cluster and calculate means for the non-standardized data (k-means without PCA)
cluster_summary_non_pca <- data_merged_with_clusters_non_pca %>%
  group_by(Cluster) %>%
  summarise(
    Mean_GDP_Growth = mean(`GDP Growth (2009)`, na.rm = TRUE),
    Mean_Market_Capitalization = mean(`Market Capitalization (2009)`, na.rm = TRUE),
    Mean_Inflation = mean(`Inflation (2009)`, na.rm = TRUE),
    Mean_Government_Debt = mean(`Government Debt (2009)`, na.rm = TRUE),
    Mean_Foreign_Investment = mean(`Foreign Investment (2009)`, na.rm = TRUE),
    Mean_Unemployment = mean(`Unemployment (2009)`, na.rm = TRUE)
  )

# View the summary table with means for non-standardized data (k-means without PCA)
View(cluster_summary_non_pca)

################################################################################
# Table to interpret the results (for k-means with PCA)
# Extract Country Name and Cluster from the PCA-based cluster assignments
cluster_assignments_pca <- cluster_table_ordered_pca %>%
  select(`Country Name`, Cluster)

# Merge the PCA-based cluster assignments with the original 2009 data
data_merged_with_clusters_pca <- data_cleaned_crises_original %>%
  left_join(cluster_assignments_pca, by = "Country Name")

# Check for any NA values in Cluster and remove those rows
data_merged_with_clusters_pca <- na.omit(data_merged_with_clusters_pca)

# Order by Cluster
data_merged_with_clusters_pca <- data_merged_with_clusters_pca %>%
  arrange(Cluster)

# View the data frame with clusters and original values
View(data_merged_with_clusters_pca)

# Group by Cluster and calculate means for the non-standardized data
cluster_summary_pca <- data_merged_with_clusters_pca %>%
  group_by(Cluster) %>%
  summarise(
    Mean_GDP_Growth = mean(`GDP Growth (2009)`, na.rm = TRUE),
    Mean_Market_Capitalization = mean(`Market Capitalization (2009)`, na.rm = TRUE),
    Mean_Inflation = mean(`Inflation (2009)`, na.rm = TRUE),
    Mean_Government_Debt = mean(`Government Debt (2009)`, na.rm = TRUE),
    Mean_Foreign_Investment = mean(`Foreign Investment (2009)`, na.rm = TRUE),
    Mean_Unemployment = mean(`Unemployment (2009)`, na.rm = TRUE)
  )

# View the summary table with means for non-standardized data
View(cluster_summary_pca)

