# Financial Crisis Analysis

This project explores the economic impact of the financial crisis on both the U.S. economy and the world economy. The analysis is split into two parts, each using different methodologies to assess the effects of the crisis. Data came from the World Bank economic indicators.

## Part 1: U.S. Economy Impact (Part1US)

In this section, I analyze the economic impact of the crisis on the United States. The analysis includes web scraping, interactive visualizations and tables for extensive descriptive statistics and correlation matrix to analyze the relationship between macroeconomic indicators.

## Part 2: World Economy Impact (Part2W)

In this section, the analysis extends to the world economy, using basic machine learning techniques (PCA and clustering methods) to explore patterns and groupings in how different countries are affected by the financial crisis. The final output is a comprehensive report.

## Files

**`Part1US.qmd`** Analyzes the US economy, including interpretations of the results. Can be rendered as an HTML file for interactive visualizations and tables.

**`Part2W.qmd`** Applies PCA and clustering techniques across various countries. Can be rendered to a pdf file containing the full explanation.

**`Part2W.pdf`** The rendered pdf, which summarizes the results and visualizations from the world economy.

**`Unzipped_data`** Folder containing all datasets (already unzipped) from the World Bank used in the analysis. 

## Results Example

Below is a visualization of the "Global" Business Cycle, which indicates 2009 as the most destructive year of the crisis. For further details and insights, please refer to the report pdf.

![Business Cycle](BusinessCycle.png)

## User Guide

1. **Setup**:
   - Ensure R and Quarto are installed.
   - Download the data from the repository and place it in the `Unzipped_data` folder within the same directory as the script (a relative path is used).
   - The dataset is also available at [World Bank Databank](https://databank.worldbank.org/home).
      - Quarto requires additional dependencies such as Pandoc and LaTeX (for PDF) and a web browser (for HTML rendering). Ensure these are installed if needed by running the following in the R console:
     ```r
     install.packages("tinytex")  # Install the TinyTeX package
     tinytex::install_tinytex()    # Install TinyTeX (LaTeX distribution)
     ```
     Quarto **already includes Pandoc**, so no separate installation is required.


2. **Execution**:
   - Run the `.qmd` files using Quarto to generate the output, or alternatively render the `.qmd` files directly.
   - The `chromote` package requires a compatible browser that supports the Chrome DevTools Protocol (CDP), such as Google Chrome or another Chromium-based browser.
   - Note that **internet connection** is required for web scraping.

3. **Packages**:
   - All required packages are loaded at the beginning of the `.qmd` files as a list for easy reference.
   - If any packages are missing, install them by running the following command in the R console or terminal:
     ```r
     install.packages("package_name")
     ```

## Contacts
For any clarifications, questions, or to report issues with the code, feel free to reach out via email at alessandro.dodon@usi.ch. You can also find my LinkedIn link in my GitHub bio.

