# Data Analytics Capstone 2025

## Project Overview
For our Case Studies in Data Analytics class, we worked with data provided by DMARC food pantries. They are the biggest food pantry network in Iowa. There has been an increase in the number of unique first-time visitors in 2023.

Our goal of this project was to investigate the data and provide insights for DMARC to understand their increasing number of overall visitors and first-time visitors.
Additionally, we wanted to find answers to these questions:
- What are the characteristics of first-time visitors?
- What differentiates first-time visitors from returners?
- What are some behaviors in households? Do they change based on the type of household?

Initially, we only had data on each visit from 2018 to the beginning of 2024. However, we focused on data after 2020, because the data collection in 2018 and 2019 worked a bit different. In April, we received data for all of 2024, which also included more information like veteran status. 
Because of the sensitive nature of this data, it is not accessible for the public. 

### Project Structure:

```         
├── Exploration & Analysis (2018-2023)
│   ├── Code
│       └── Create_dataset
│       └── data_exploration
│           └── annual_income
│           └── create_variable_day_of_the_week
│           └── create_variable_first_visit
│           └── family_type
│           └── first_map_2023
│           └── housing_situation_map
│           └── income_src
│           └── small zipcode exploration
│       └── models
│           └── clustering21n22n23
│           └── knn_model
│           └── random_forest_22
│           └── random_forest_23
│           └── ridge_lasso_22
│           └── ridge_lasso_23
│       └── visualization_code
│           └── API_Visualizations
│           └── Choropleth_Maps
│           └── Federal poverty level and SNAP
│           └── Lynette-visualizations
│           └── Visualizations on hh_data
│           └── Zosia-visualizations
│   └── Output
│           └── Choropleth__API_NonSNAP_HH.html
│           └── Choropleth__NewHH.html
│           └── Choropleth__ReturnerHH.html
│           └── first snap.png
│           └── last snap.png
│           └── last snap.png
│           └── SNAP at first visit.png
│           └── snap first.png
│           └── SNAP last visit.png
│           └── snap last.png
├── Exploration & Analysis (2024)
│   ├── code
│       └── DataSets2024
│       └── models
│           └── Ridge_firstvisit2024
│           └── School children households analysis
│           └── Veteran households analysis
│       └── visualizations_code
│           └── Dietary Issues and Income Visualizations
│           └── Dietary Issues and Location Visualizations
│   └── Output
│           └── schoolchild_variableimportance.png
│           └── veteran_variableimportance.png
└── README.md
```

## Data Sources and Preparation

NOTE: The data from the original project is not included in this repository due to confidentiality issues

### **Data Sources**  
- DMARC  
- ACS API

### **DMARC Data - Cleaning and Creation of Data sets**  
#### **Create different level data set**  
- Visit: Includes every visit - Monthly Count: Various monthly counts  
- Quarter Count: Various quarterly counts  
- Monthly Household Frequency: How often each household visits each month  
- Monthly Total Frequency: Counts on frequencies per month  
- Household: Each individual Household  
- Household First Visit Per Year: Households whose first visit was in a given year  
- Household Per Year: Every household that visited during a given year  
#### **Cleaning**  
- In numeric columns, replacing NA values with the median  
### **API Data - Cleaning and Creation of Datasets**  
#### **Cleaning**  
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

Knn Model: Final outputs and results from the model are in the models folder. We used the model to predict new visitors of 2023 and used the results to analyze the variable importance. Knn is a model that is used for prediction. The model was fine tuned it using different K values. We evaluated the model perfomance using a confusion matrix that showed the false positives, false negatives, true positives and true negatives. 

------------------------------------------------------------------------
## Authors

-   Grace Bero - Data Analyst
-   Amelia Burnell - Data Analyst
-   Zofia Landowska - Data Analyst
-   Lynette Ndibalekera - Data Analyst
