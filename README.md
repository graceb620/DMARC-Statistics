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

Random Forest Model:
We used a Random Forest model to predict whether a household had its first visit in 2023 or 2022. This model works by combining the results of many decision trees to make a final prediction. We tuned the model to handle class imbalance and adjusted parameters to improve accuracy. Important variables in the final model included SNAP participation, housing type, and household size.

Ridge Regression:
We used this model to determine which variables are most strongly associated with the likelihood of a household’s first visit in 2023/2022. Ridge regression is a type of logistic regression that helps reduce overfitting by shrinking the model’s coefficients. We used cross-validation to choose the best regularization strength (lambda). This model helped us predict visit outcomes while keeping the influence of less important variables in check.

Lasso Regression: 
Lasso regression is similar to Ridge but can also remove unnecessary variables by setting their coefficients to zero. We also used this model to determine which variables are most strongly associated with the likelihood of a household’s first visit in 2023/2022. We used it to identify key predictors while keeping the model simple. Like the other models, we evaluated performance using ROC curves and AUC.

Knn Model: Final outputs and results from the model are in the models folder.

------------------------------------------------------------------------
## Authors

-   Grace Bero - Data Analyst
-   Amelia Burnell - Data Analyst
-   Zofia Landowska - Data Analyst
-   Lynette Ndibalekera - Data Analyst

## License

This project is licensed under the MIT License.
