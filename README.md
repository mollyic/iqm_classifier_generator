# MRI Scan Quality and Motion Prediction Pipeline

## Overview

An R-based pipeline for running machine learning models to predict MRI scan quality and motion artifacts. The pipeline dynamically generates scripts to run models using various configurations such as sequence type, rating type, principal components (PCs), weighting, and model mode (regression or classification). The models are trained and evaluated using hyperparameter tuning, and the best-performing model is selected based on the desired metric (e.g., `RMSE` for regression or `ROC_AUC` for classification).

---

## Key Features

1. **Modular structure**: The pipeline dynamically generates and runs scripts based on multiple configurations for input data.
2. **Multiple models**: Supports a range of machine learning models, including Random Forest, XGBoost, LASSO, Elastic Net, and Ridge regression.
3. **Preprocessing**: Utilizes PCA for dimensionality reduction and flexible recipe-based preprocessing for each configuration.
4. **Hyperparameter Tuning**: Automatically tunes hyperparameters for each model using grid search and cross-validation.
5. **Final Model Selection**: Selects the best-performing model based on regression (RMSE) or classification (ROC_AUC) metrics.
6. **Parallel Execution**: Supports parallel execution of model comparisons to speed up computations.

---

## Repository Structure

- **`modules/`**: Contains the core scripts for model generation and execution.
  - `generate_runs.R`: Dynamically generates scripts for different configurations.
  - `workflows.R`: Defines the workflow for preprocessing, tuning, and fitting models.
  - `models.R`: Contains model definitions (Random Forest, XGBoost, LASSO, etc.).
  - `main.R`: Executes the generated script for a specific configuration.
  
- **`work/terminal_output/`**: Stores terminal outputs for each run.
- **`models/`**: Stores the trained models in `.rds` format.

---

## How to Use

### 1. Clone the repository

```bash
git clone <repository-url>
cd <repository-directory>
```

### 2. Set up your R environment
Ensure that R and the required packages are installed. You can install the necessary R packages with:

```bash
install.packages(c("tidymodels", "furrr", "progressr", "glmnet", "ranger", "xgboost"))
```
