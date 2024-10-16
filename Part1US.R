# Loading libraries and data
# Load Libraries and suppress warnings
suppressWarnings(suppressMessages({
  library(forecast)
  library(reshape2)
  library(chromote)
  library(car)
  library(tseries)
  library(ggfortify)
  library(plotly)
  library(DT)
  library(knitr)
  library(kableExtra)
  library(ggrepel)
  library(Rtsne)
  library(tidyverse)  # This will load readr, dplyr, tidyr, ggplot2 and some others
}))

# Reading the CSV files suppressing warnings
suppressWarnings(suppressMessages({
  file_path <- "C:/Users/aledo/OneDrive/Desktop/R_PROJECT_3 YEAR_PRO_2/Unzipped_data/"
  files <- c(
    "API_BN.CAB.XOKA.CD_DS2_en_csv_v2_5873206.csv",
    "API_CM.MKT.LCAP.GD.ZS_DS2_en_csv_v2_5873240.csv",
    "API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_5871597.csv",
    "API_GC.DOD.TOTL.GD.ZS_DS2_en_csv_v2_5872587.csv",
    "API_NE.GDI.TOTL.ZS_DS2_en_csv_v2_5873774.csv",
    "API_NY.GDP.MKTP.CD_DS2_en_csv_v2_5871885.csv",
    "API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_5871639.csv",
    "API_CM.MKT.INDX.ZG_DS2_en_csv_v2_5871629.csv",
    "API_FM.LBL.BMNY.GD.ZS_DS2_en_csv_v2_5873493.csv",
    "API_GC.REV.XGRT.GD.ZS_DS2_en_csv_v2_5873527.csv",
    "API_NY.GNS.ICTR.ZS_DS2_en_csv_v2_5873909.csv",
    "API_FR.INR.LEND_DS2_en_csv_v2_5873491.csv",
    "API_NE.EXP.GNFS.ZS_DS2_en_csv_v2_5995190.csv",
    "API_NE.IMP.GNFS.ZS_DS2_en_csv_v2_5994778.csv",
    "API_NY.ADJ.SVNG.GN.ZS_DS2_en_csv_v2_5996051.csv",
    "API_BX.KLT.DINV.CD.WD_DS2_en_csv_v2_5995288.csv",
    "API_NE.CON.TOTL.ZS_DS2_en_csv_v2_5995951.csv"
  )
  
  data_list <- lapply(files, function(file) {
    read_csv(paste0(file_path, file), skip = 3)
  })
}))

################################################################################
# Extracting GDP growth details from World Bank with web scraping
# # Start a browser session
b <- ChromoteSession$new()

# Navigate to the World Bank page
b$Page$navigate("https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG?view=chart")

# Give the page some time to load
Sys.sleep(5)  

# Click the "Details" button using the selector
b$Runtime$evaluate('document.querySelector("button.button.secondary.openinnew").click()')

# Wait for the content to load after clicking the button
Sys.sleep(5) 

# Extract the details using the provided selector
details_info <- b$Runtime$evaluate('document.querySelector("#modal > div > div > div > div").innerText')$result$value

# Close the browser session
b$close()

# Formatting the output for Quarto
# Split the content into sections
sections <- unlist(strsplit(details_info, "\n\n"))

cat("## Extracted Details\n\n")

# Iterate over sections and print without title formatting
for (section in sections) {
  # Wrap the text to a width of 80 characters
  wrapped_text <- paste(strwrap(section, width = 80), collapse="\n")
  
  # Print the wrapped text
  cat(wrapped_text, "\n\n---\n\n")
}

################################################################################
# Cluster analysis of GDP growth (2007-2009)
# Extract and filter GDP growth data
gdp_growth_data <- data_list[[7]] %>%
  select(`Country Name`, `2007`, `2008`, `2009`) %>%
  na.omit() # Removing NA values

# K-means clustering
set.seed(123)
clustering_data <- as.matrix(gdp_growth_data[, 2:4])
kmeans_result <- kmeans(clustering_data, centers = 3)

# Attach clusters to data
gdp_growth_data$cluster <- kmeans_result$cluster
gdp_growth_data$cluster_label <- factor(gdp_growth_data$cluster, 
                                        labels = c("Severely Impacted", "Moderately Impacted", "Minimally Impacted"))

# Run PCA for dimensionality reduction
pca_result <- prcomp(clustering_data, center = TRUE, scale. = TRUE)

# Attach PCA dimensions to the data
gdp_growth_data$pca1 <- pca_result$x[, 1]
gdp_growth_data$pca2 <- pca_result$x[, 2]

# Create custom hover text
gdp_growth_data$hover_info <- with(gdp_growth_data, paste("Country: ", `Country Name`, 
                                                          "<br>Cluster: ", cluster_label,
                                                          "<br>2007 GDP Growth: ", `2007`, "%",
                                                          "<br>2008 GDP Growth: ", `2008`, "%",
                                                          "<br>2009 GDP Growth: ", `2009`, "%"))

# Create ggplot object with refined labels and hover info
p <- ggplot(gdp_growth_data, aes(x = pca1, y = pca2, color = cluster_label, text = hover_info)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "orange", "green"), name = "Impact Level") +
  labs(title = "PCA of Countries based on GDP Growth (2007-2009)",
       caption = "Cluster Analysis with PCA",
       x = "PCA Dimension 1",
       y = "PCA Dimension 2") +
  theme(plot.caption = element_text(hjust = 0.5))

# Convert to an interactive plotly object with specified size
interactive_plot <- ggplotly(p, tooltip = "text", width = 1500, height = 1500) %>%
  config(displayModeBar = FALSE) # Remove the default toolbar

# Display the plot
interactive_plot

################################################################################
# Data filtering and reshaping
# Define country of interest and years of interest
country_of_interest <- "United States"
years_of_interest <- 1990:2020

# Filter, reshape, and combine the data list into one wide-format dataframe
combined_data <- bind_rows(lapply(data_list, function(df) {
  df %>%
    filter(`Country Name` == country_of_interest) %>%
    select(`Country Name`, `Country Code`, `Indicator Name`, all_of(as.character(years_of_interest)))
})) %>%
  pivot_longer(cols = as.character(years_of_interest), names_to = "Year", values_to = "Value") %>%
  pivot_wider(names_from = `Indicator Name`, values_from = "Value", id_cols = "Year") %>%
  mutate(Year = as.numeric(Year))  # Convert Year to numeric if it's not already

# Create Interactive Data Table with DT
datatable(combined_data, 
          options = list(pageLength = 10, autoWidth = TRUE),
          class = 'cell-border stripe',
          rownames = FALSE,
          filter = 'top', 
          caption = 'Data for Financial Indicators: 1990-2020')

################################################################################
# Data quality and pre-processing
# Initialize dataframe for detailed outlier information
outlier_details_df <- tibble(
  Indicator = character(),
  Year = integer(),
  Value = numeric(),
  Outlier = logical()
)

# Initialize a list to store the number of outliers for each indicator
num_outliers_list <- list()

# Loop through each indicator to identify outliers
for (indicator_name in names(combined_data)[-1]) {
  indicator_data <- combined_data %>%
    select(Year, Value = all_of(indicator_name)) %>%
    na.omit()
  
  # Calculate the IQR and identify outliers
  Q1_value <- quantile(indicator_data$Value, 0.25, na.rm = TRUE)
  Q3_value <- quantile(indicator_data$Value, 0.75, na.rm = TRUE)
  IQR_value <- Q3_value - Q1_value
  
  # Identify outliers
  outlier_data <- indicator_data %>%
    filter(Value < Q1_value - 1.5 * IQR_value | Value > Q3_value + 1.5 * IQR_value) %>%
    mutate(Outlier = TRUE) %>%
    select(Year, Value, Outlier)
  
  # Add to the outlier details dataframe
  if (nrow(outlier_data) > 0) {
    outlier_data$Indicator <- indicator_name
    outlier_details_df <- bind_rows(outlier_details_df, outlier_data)
  }
  
  # Store the number of outliers for each indicator
  num_outliers_list[[indicator_name]] <- nrow(outlier_data)
}

# Check Missing Values per indicator
missing_values_per_column <- combined_data %>%
  select(-Year) %>%
  summarise_all(~ sum(is.na(.)))

# Check for duplicates based on Year
duplicates_per_year <- combined_data %>%
  group_by(Year) %>%
  summarise(Count = n()) %>%
  filter(Count > 1)

# Create a summary table after the loop
data_quality_summary <- names(combined_data)[-1] %>%
  map_df(~ tibble(
    Indicator = .x,
    Num_Of_Missing = missing_values_per_column[[.x]],
    Num_Of_Duplicates = if (.x == "Year") nrow(duplicates_per_year) else NA_integer_,
    Num_Of_Outliers = num_outliers_list[[.x]]
  ))

# Apply kable styling to data quality summary table
summary_table <- kable(data_quality_summary, 
                       caption = "Data Quality Summary (Excluding Outliers)", 
                       escape = F, 
                       format = "html",
                       col.names = c("Indicator", "Num. of Missing", "Num. of Duplicates", "Num. of Outliers"), 
                       align = c('l', 'r', 'r', 'r')) %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "hover"),
                full_width = F, 
                position = "center", 
                font_size = 12) %>%
  scroll_box(width = "100%", height = "300px")

# Display the summary table
summary_table

# Apply kable styling to outlier details table, only if there are outliers
if (nrow(outlier_details_df) > 0) {
  outlier_details_table <- kable(outlier_details_df, 
                                 caption = "Outlier Details", 
                                 escape = F, 
                                 format = "html",
                                 col.names = c("Indicator", "Year", "Value", "Outlier"), 
                                 align = c('l', 'r', 'r', 'r')) %>%
    kable_styling(bootstrap_options = c("striped", "bordered", "hover"),
                  full_width = F, 
                  position = "center", 
                  font_size = 12)
  
  # Display the outlier details table
  outlier_details_table
}

################################################################################
# Summary statistics
# Function to calculate summary statistics for a column
calculate_stats <- function(data, column_name) {
  data %>%
    summarize(
      Indicator = column_name,
      Mean = mean(.data[[column_name]], na.rm = TRUE),
      Median = median(.data[[column_name]], na.rm = TRUE),
      Max = max(.data[[column_name]], na.rm = TRUE),
      Min = min(.data[[column_name]], na.rm = TRUE),
      Std_Dev = sd(.data[[column_name]], na.rm = TRUE)
    )
}

# Calculate summary statistics for each indicator column
summary_stats <- lapply(names(combined_data)[-1], function(column_name) {
  calculate_stats(combined_data, column_name)
}) %>%
  bind_rows()

# Render the table as an interactive DataTable
datatable(summary_stats, 
          options = list(pageLength = 5, autoWidth = TRUE),
          class = 'cell-border stripe', 
          rownames = FALSE) %>%
  formatRound(columns = 2:ncol(summary_stats), digits = 2)

################################################################################
# Visualizations of indicators
# We first start with the standardization of the data 
indicator_names <- colnames(combined_data)[-1]  # Exclude the first column which is 'Year'

# Create a copy of the data for standardized values
standardized_data <- combined_data
standardized_data[indicator_names] <- as.data.frame(lapply(standardized_data[indicator_names], scale))

# Create the interactive plot with a dropdown menu
interactive_plot <- plot_ly(data = combined_data, x = ~Year)

# Add traces for each indicator
for(indicator in indicator_names) {
  # Set visibility: only the first indicator's trace is visible initially
  is_visible <- indicator == indicator_names[1]
  
  # Generate the hover text
  hover_text <- paste("Year: ", combined_data$Year,
                      "<br>Original Value: ", combined_data[[indicator]],
                      "<br>Standardized Value: ", standardized_data[[indicator]])
  
  interactive_plot <- interactive_plot %>%
    add_trace(y = standardized_data[[indicator]],  # Use standardized data here
              name = indicator,
              visible = is_visible,
              type = 'scatter',
              mode = 'lines+markers',
              text = hover_text,  # Set the hover text
              hoverinfo = 'text+name')
}

# Create dropdown menu entries
buttons <- list()
for(i in seq_along(indicator_names)) {
  visibility_vals <- rep(FALSE, length(indicator_names))
  visibility_vals[i] <- TRUE
  buttons[[i]] <- list(
    method = "restyle",
    args = list("visible", visibility_vals),
    label = indicator_names[i]
  )
}

# Update layout with dropdown menu and fixed y-axis range
interactive_plot <- interactive_plot %>%
  layout(
    xaxis = list(title = "Year", range = c(1990, 2020)),
    yaxis = list(title = "Standardized Indicator Value", range = c(3, 3)),  # Fixed y-axis range
    showlegend = FALSE,
    updatemenus = list(
      list(
        y = 1.15,
        x = 0.5,
        xanchor = "center",
        yanchor = "top",
        buttons = buttons
      )
    ),
    dragmode = "pan",
    hovermode = "closest"
  )

# To highlight the financial crisis period (2007-2009) in red
highlight_crisis <- list(
  type = "rect",
  xref = "x",
  yref = "paper",
  x0 = 2007,
  x1 = 2009,
  y0 = 0,
  y1 = 1,
  fillcolor = "red",
  opacity = 0.2,
  line = list(width = 0)
)

# To highlight the recovery period (2010-2012) in green
highlight_recovery <- list(
  type = "rect",
  xref = "x",
  yref = "paper",
  x0 = 2010,
  x1 = 2014,
  y0 = 0,
  y1 = 1,
  fillcolor = "green",
  opacity = 0.2,
  line = list(width = 0)
)

# Add the highlights to the plot and remove default toolbar
interactive_plot <- interactive_plot %>%
  layout(shapes = list(highlight_crisis, highlight_recovery)) %>%
  config(displayModeBar = FALSE)  # Remove default toolbar

# Display the plot
interactive_plot

################################################################################
# Visualizing correlations among economic indicators
# Calculate correlations
cor_matrix <- cor(combined_data[-1])  # Exclude the 'Year' column

# Melt the correlation matrix
cor_melted <- melt(cor_matrix)

# Create ggplot heatmap
heatmap_plot <- ggplot(data = cor_melted, aes(x=Var1, y=Var2)) +
  geom_tile(aes(fill=value), color='white') +
  geom_text(aes(label=round(value, 2)), size=2) +  # Adjust text size if necessary
  scale_fill_gradient2(low="blue", high="red", midpoint=0) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(fill="Correlation")

# Convert to plotly object for interactivity and specify dimensions
interactive_heatmap <- ggplotly(heatmap_plot, width = 800, height = 800) %>%
  config(displayModeBar = FALSE)  # Remove the default toolbar

# Print or render the interactive heatmap
interactive_heatmap

################################################################################
# Showing significant correlations
# Filter out self-correlations
significant_correlations <- subset(cor_melted, abs(value) > 0.5 & Var1 != Var2)

# Create a unique identifier for each pair and remove duplicates
significant_correlations$pair <- apply(significant_correlations[, c("Var1", "Var2")], 1, function(x) paste(sort(x), collapse = "-"))
significant_correlations <- significant_correlations[!duplicated(significant_correlations$pair), ]

# Remove the 'pair' column right after removing duplicates
significant_correlations$pair <- NULL

# Order by correlation values like a thermometer (positive to negative)
significant_correlations <- significant_correlations[order(significant_correlations$value, decreasing = TRUE), ]

# Create a kable table with styling, without row names
significant_cor_table <- kable(significant_correlations, 
                               caption = "Significant Correlations (above 0.5 or below -0.5, excluding self-correlations and duplicates)", 
                               format = "html", 
                               booktabs = TRUE,
                               row.names = FALSE) %>%  # Set row.names to FALSE
  kable_styling(bootstrap_options = c("striped", "bordered", "hover"),
                full_width = FALSE, 
                position = "center", 
                font_size = 12) %>%
  scroll_box(width = "100%", height = "300px")

# Display the table
significant_cor_table

################################################################################
# Stationarity check for economic indicators
# Stationarity Check using Augmented Dickey-Fuller Test
adf_test_results <- lapply(names(combined_data)[-1], function(column_name) {
  test_result <- adf.test(combined_data[[column_name]], alternative = "stationary")
  data.frame(Indicator = column_name, 
             Test_Statistic = test_result$statistic, 
             P_Value = test_result$p.value)
})

# Combine results into a single data frame
adf_test_df <- do.call(rbind, adf_test_results)

# Display the results using kable and style with kable_styling
kable(adf_test_df, format = "html", table.attr = "class='table'") %>%
  kable_styling()

################################################################################
# Applying first differencing and re-checking stationarity
# Apply first differencing to make series stationary
time_series_diff <- as.data.frame(lapply(combined_data[-1], diff))
time_series_diff$Year <- combined_data$Year[-1]  # Adjusting the year column after differencing

# Re-checking Stationarity using Augmented Dickey-Fuller Test
adf_test_results_diff <- lapply(names(time_series_diff)[-1], function(column_name) {
  test_result <- adf.test(time_series_diff[[column_name]], alternative = "stationary")
  data.frame(Indicator = column_name, 
             Test_Statistic = test_result$statistic, 
             P_Value = test_result$p.value)
})

# Combine results into a single data frame
adf_test_df_diff <- do.call(rbind, adf_test_results_diff)

# Display the results using kable and style with kable_styling
kable(adf_test_df_diff, format = "html", table.attr = "class='table'") %>%
  kable_styling()

################################################################################
# Second differencing for persistent non-stationarity
# Apply second differencing for non-stationary series
columns_to_diff_again <- c("Central.government.debt..total....of.GDP.", "Current.account.balance..BoP..current.US..", 
                           "GDP..current.US..", "GDP.growth..annual...", "Gross.capital.formation....of.GDP.", 
                           "Gross.savings....of.GDP.", "Market.capitalization.of.listed.domestic.companies....of.GDP.", 
                           "S.P.Global.Equity.Indices..annual...change.")

time_series_diff_2 <- as.data.frame(lapply(time_series_diff[columns_to_diff_again], diff))
time_series_diff_2$Year <- time_series_diff$Year[-1]  # Adjusting the year column after second differencing

# Re-checking Stationarity using Augmented Dickey-Fuller Test after second differencing
adf_test_results_diff_2 <- lapply(names(time_series_diff_2)[-1], function(column_name) {
  test_result <- adf.test(time_series_diff_2[[column_name]], alternative = "stationary")
  data.frame(Indicator = column_name, 
             Test_Statistic = test_result$statistic, 
             P_Value = test_result$p.value)
})

# Combine results into a single data frame
adf_test_df_diff_2 <- do.call(rbind, adf_test_results_diff_2)

# Display the results using kable and style with kable_styling
kable(adf_test_df_diff_2, format = "html", table.attr = "class='table'") %>%
  kable_styling()

################################################################################
# Regression analysis on stationary data
# Drop twice-differenced columns from `time_series_diff`
time_series_diff <- time_series_diff[, !(names(time_series_diff) %in% columns_to_diff_again)]

# Merge the data set
merged_diff_data <- left_join(time_series_diff, time_series_diff_2, by="Year")

# Move 'Year' to the first column
merged_diff_data <- merged_diff_data[, c("Year", setdiff(names(merged_diff_data), "Year"))]

# Remove rows with NA values
merged_diff_data <- na.omit(merged_diff_data)

# Regression Analysis using stationary predictors
model <- lm(GDP.growth..annual... ~ Inflation..consumer.prices..annual... + 
              Lending.interest.rate.... + 
              Revenue..excluding.grants....of.GDP. +
              Gross.capital.formation....of.GDP. + # Note: We're using the twice differenced column
              Gross.savings....of.GDP., # Note: We're using the twice differenced column
            data = merged_diff_data)

summary(model)

################################################################################
# Residual analysis and model diagnostics
# Residual Analysis
par(mfrow = c(2, 2))
plot(model)

# Check for Autocorrelation in Residuals
acf(resid(model))

# Check for Multicollinearity using VIF
vif(model)

