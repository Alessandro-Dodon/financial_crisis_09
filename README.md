# Financial Crises Analysis

This project explores the economic impact of financial crises on both the U.S. economy and the world economy. The analysis is split into two parts, each using different methodologies to assess the effects of financial crises. Data came from the World Bank economic indicators.

## Part 1: U.S. Economy Impact (Part1US)

In this section, I analyze the economic impact of financial crises on the United States. The analysis includes web scraping, interactive visualizations and tables for extensive descriptive statistics and correlation matrix to analyze the relationship between macroeconomic indicators.

## Part 2: World Economy Impact (Part2W)

In this section, the analysis extends to the world economy, using basic machine learning techniques (PCA and clustering methods) to explore patterns and groupings in how different countries are affected by financial crises. The final output is a comprehensive report.

## Files

### `Part1US.qmd`
- Analyzes the US economy, including interpretations of the results. Can be rendered as an HTML file for interactive visualizations and tables.

### `Part2W.R`
- Applies PCA and clustering techniques across various countries.

### `Part2W.pdf`
- Summarizes the results and visualizations from the world economy analysis with a brief explanation.

### `Unzipped_data`
- Folder containing all datasets (already unzipped) from the World Bank used in the analysis. Also available at https://databank.worldbank.org/home for more information.

## User Guide

Ensure R and Quarto are installed. The .qmd file can be rendered to HTML, and the .R file runs automatically by pressing "Run" sequentially in RStudio. Data must be placed in the `Unzipped_data` folder, included in this repository, in the same directory as the script.

If any required packages are missing, simply remove the # at the start of the install.packages() lines in the scripts to install them. Note that the chromote package requires a compatible browser that supports the Chrome DevTools Protocol (CDP), such as Google Chrome or other Chromium-based browsers

