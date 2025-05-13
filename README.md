# Data Analytics Capstone 2025

## Project Overview

### Project Structure:

```         
├── 2024 Data Analysis
│   ├── Code
│   ├── Create Dataset
│   ├── Models
│   ├── Visualizations
│   └── Output
│       └── visualization.png
├── Up to 2023 Data Analysis
│   ├── Code
│   └── Output
└── README.md
```

## Data Sources and Preparation

NOTE: The data from the original project is not included in this repository due to confidentiality issues

**Data Sources**\
- DMARC\
- ACS API

**DMARC Data - Cleaning and Creation of Data sets**\
**Create different level data set**\
- Visit: Includes every visit - Monthly Count: Various monthly counts\
- Quarter Count: Various quarterly counts\
- Monthly Household Frequency: How often each household visits each month\
- Monthly Total Frequency: Counts on frequencies per month\
- Household: Each individual Household\
- Household First Visit Per Year: Households whose first visit was in a given year\
- Household Per Year: Every household that visited during a given year\
**Cleaning**\
- In numeric columns, replacing NA values with the median\
**API Data - Cleaning and Creation of Datasets**\
**Cleaning**\
- Making sure rows only included Iowa zipcodes that were included in the original dataset

## 2024 Data Analysis

### 1. Code

-   All scripts related to the 2024 data analysis are located in the `2024 Data Analysis/Code` folder.

### 2. Create Dataset

-   Data set creation scripts and procedures can be found in the `2024 Data Analysis/Create Dataset` folder.

### 3. Models

-   R scripts and files for model building and evaluation are in the `2024 Data Analysis/Models` folder.

### 4. Visualizations

-   Plots and visualizations generated from the 2024 data are stored in the `2024 Data Analysis/Visualizations` folder. Key visualizations include `visualization.png` located in the `Output` folder.

### 5. Output

-   Final outputs, including datasets and model results, are saved in the `2024 Data Analysis/Output` folder.

------------------------------------------------------------------------

## Up to 2023 Data Analysis

### 1. Code

-   Analysis scripts for datasets up to 2023 are stored in the `Up to 2023 Data Analysis/Code` folder.

### 2. Output

-   Outputs and results for the analysis up to 2023 are located in the `Up to 2023 Data Analysis/Output` folder.

------------------------------------------------------------------------

## Requirements

-   R version: 4.3.1 or later
-   Libraries:
    -   ggplot2
    -   dplyr
    -   tidyr
    -   caret
    -   randomForest

## Usage

1.  Clone the repository:

    ``` bash
    git clone <repository-url>
    ```

2.  Navigate to the project directory:

    ``` bash
    cd data-analysis-project
    ```

3.  Install required libraries:

    ``` r
    install.packages(c("ggplot2", "dplyr", "tidyr", "caret", "randomForest"))
    ```

4.  Run analysis scripts in the respective folders:

    ``` r
    source("2024 Data Analysis/Code/main_analysis.R")
    ```

------------------------------------------------------------------------
### Models

Knn : Final outputs and results from the model are in the models folder.

------------------------------------------------------------------------
## Authors

-   Your Name - Data Analyst

## License

This project is licensed under the MIT License.
