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

---

## 2024 Data Analysis

### 1. Code
- All scripts related to the 2024 data analysis are located in the `2024 Data Analysis/Code` folder.

### 2. Create Dataset
- Dataset creation scripts and procedures can be found in the `2024 Data Analysis/Create Dataset` folder.

### 3. Models
- R scripts and files for model building and evaluation are in the `2024 Data Analysis/Models` folder.

### 4. Visualizations
- Plots and visualizations generated from the 2024 data are stored in the `2024 Data Analysis/Visualizations` folder. Key visualizations include `visualization.png` located in the `Output` folder.

### 5. Output
- Final outputs, including datasets and model results, are saved in the `2024 Data Analysis/Output` folder.

---

## Up to 2023 Data Analysis

### 1. Code
- Analysis scripts for datasets up to 2023 are stored in the `Up to 2023 Data Analysis/Code` folder.

### 2. Output
- Outputs and results for the analysis up to 2023 are located in the `Up to 2023 Data Analysis/Output` folder.

---

## Requirements
- R version: 4.3.1 or later
- Libraries:
  - ggplot2
  - dplyr
  - tidyr
  - caret
  - randomForest
  - rpart
  - rpart.plot
  - logistf
  - glmnet
  - RColorBrewer

## Usage
1. Clone the repository:
   ```bash
   git clone <repository-url>
   ```

2. Navigate to the project directory:
   ```bash
   cd data-analysis-project
   ```

3. Install required libraries:
   ```R
   install.packages(c("ggplot2", "dplyr", "tidyr", "caret", "randomForest"))
   ```

4. Run analysis scripts in the respective folders:
   ```R
   source("2024 Data Analysis/Code/main_analysis.R")
   ```

---

## Authors
- Your Name - Data Analyst

## License
This project is licensed under the MIT License.
