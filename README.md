# Financial Crisis Analysis  

This university project, completed during my Bachelor's in Economics at the University of Bologna, explores the economic impact of the financial crisis on both the U.S. economy and the world economy. The analysis is split into two parts, each using different methodologies to assess the effects of the crisis. Data came from the World Bank economic indicators.  

## Part 1: U.S. Economy Impact (Part1US)

In this section, I analyze the economic impact of the crisis on the United States. The analysis includes web scraping, interactive visualizations and tables for extensive descriptive statistics and correlation matrix to analyze the relationship between macroeconomic indicators.

## Part 2: World Economy Impact (Part2W)

In this section, the analysis extends to the world economy, using basic machine learning techniques (PCA and clustering methods) to explore patterns and groupings in how different countries are affected by the financial crisis. The final output is a comprehensive report.

## Files

**`part_1_us.qmd`** Analyzes the US economy, including interpretations of the results. Can be rendered as an HTML file for interactive visualizations and tables.

**`part_2_w.qmd`** Applies PCA and clustering techniques across various countries. Can be rendered to a pdf file containing the full explanation.

**`part_2_w.pdf`** The rendered pdf, which summarizes the results and visualizations from the world economy.

**`unzipped_data`** Folder containing all datasets (already unzipped) from the World Bank used in the analysis. 

## Results Example

Below is a visualization of the "Global" Business Cycle, which indicates 2009 as the most destructive year of the crisis. For further details and insights, please refer to the report pdf.

![Business Cycle](business_cycle.png)

## User Guide

1. **Setup**:  
   - Clone the repository (Recommended):  
     ```bash
     git clone https://github.com/Alessandro-Dodon/financial_crisis_09 
     cd financial_crisis_09
     ```
   - Download as ZIP (Alternative):  
     Click the **"Code"** button (green) at the top of the repository page, select **"Download ZIP"**, extract the files, and place them in your working directory.  
   - Make sure the data is in the same working directory as the scripts (a relative path is used).  
   - The data is also available at [World Bank Databank](https://databank.worldbank.org/home).
   - Ensure R and Quarto are installed.  
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

## Contact
For any clarifications, questions, or to report issues with the code, feel free to reach out via email at alessandro.dodon@usi.ch. You can also find my LinkedIn link in my GitHub bio.

