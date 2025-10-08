# Click-Through Rate (CTR) Prediction using Logistic Regression in R

## Project Overview

This project focuses on analyzing a dataset of user interactions with online advertisements to predict whether a user will click on an ad. The primary goal is to build and evaluate a logistic regression model that can effectively forecast the Click-Through Rate (CTR) based on user demographics and online behavior.

The analysis involves several key stages:
* **Exploratory Data Analysis (EDA):** Visually and statistically exploring the data to uncover trends and relationships.
* **Data Preprocessing:** Cleaning and preparing the data for modeling, including feature engineering.
* **Model Building:** Training a logistic regression model on various sets of features.
* **Model Evaluation:** Assessing the model's performance using metrics like Accuracy, ROC curves, and AUC.
* **Segmented Analysis:** Drilling down into specific demographic segments (e.g., by age group or geography) for more targeted insights.

## Datasets

Two main datasets were used in this analysis:
* `ad_10000records.csv`: An initial dataset for preliminary analysis.
* `Final_Ad_Topics_categorical.csv`: An enriched dataset with additional categorical features like ad topics and time of day.

Key features include:
* `Daily Time Spent on Site`: Time in minutes the user spends on the site.
* `Age`: User's age in years.
* `Area Income`: Average income of the geographical area of the user.
* `Daily Internet Usage`: Time in minutes the user spends on the internet per day.
* `Ad Topic Line`: The headline of the ad.
* `Gender`: Gender of the user.
* `Clicked on Ad`: The target variable; 0 if the user did not click, 1 if they did.

## Methodology

### 1. Exploratory Data Analysis (EDA)
Visualizations like histograms, boxplots, and bar charts were used to understand the distribution of variables and their relationship with the target variable (`Clicked on Ad`). A **Chi-square test** was also conducted to check for significant associations between categorical variables.

### 2. Modeling
The core of this project is **Logistic Regression**, a statistical model used for binary classification. The `glm()` function in R was used to train the model. To optimize the model, **stepwise variable selection** was performed using the `stepAIC()` function, and the overall model significance was validated with the **Likelihood Ratio Test**.

### 3. Model Evaluation
The model's predictive power was evaluated using a train/test split methodology. Key performance metrics calculated include:
* **Accuracy:** Overall correct predictions.
* **Confusion Matrix:** A table showing True Positives, True Negatives, False Positives, and False Negatives.
* **True Positive Rate (TPR) / Recall:** The proportion of actual clicks that were correctly identified.
* **False Positive Rate (FPR):** The proportion of non-clicks that were incorrectly identified as clicks.
* **ROC Curve & AUC:** The Receiver Operating Characteristic curve and the Area Under the Curve, which measure the model's ability to distinguish between classes.

## Scripts Overview

* `CTR DATA -1 , GRAPHS, TEST, TRAIN, ANALYSIS.R`: Initial EDA and baseline logistic regression model.
* `final code for full data.R`: Advanced modeling on the full dataset with `stepAIC` for feature selection and ROC analysis.
* `CTR UPDATED GRAPH CODE.R`: A script dedicated to comprehensive EDA and visualizations.
* `CTR COMPILED FULL DATA AND AGE GROUP 1 DATA.R`: Combines a full data model with a separate, targeted analysis on "Age Group 1".
* `1.R`: A deep-dive analysis on a specific user segment: users from Asian countries within "Age Group 1".
* `CTR CODE ASIAN COUNTRIES.R`: Implements the Likelihood Ratio Test to formally assess model and predictor significance.
* `auc roc.R`: A template script demonstrating a standard, robust workflow for model evaluation using `caret` and `pROC`.

## R Libraries Used

To run these scripts, you will need the following R libraries. You can install them using `install.packages("package_name")`.

* `MASS`: For stepwise model selection (`stepAIC`).
* `ROCit` or `pROC`: For ROC curve analysis.
* `lmtest`: For coefficient tests (`coeftest`).
* `vcd`: For creating mosaic plots.
* `caret`: For a streamlined machine learning workflow.
* `ggplot2`: For advanced data visualization.

## How to Run

1.  Clone this repository to your local machine.
2.  Make sure you have all the required R libraries installed.
3.  Open any of the `.R` scripts in RStudio or your preferred R environment.
4.  **Important:** Change the working directory path in the `setwd()` function at the beginning of the script to match the location of the dataset files on your computer.
5.  Run the script to see the analysis and results.
