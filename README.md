# imdb-score-prediction
Predicting IMDB scores of upcoming movies using historical data from the Internet Movie Database. 
## Overview
This project explores the development of a predictive model for IMDb ratings using a dataset of 1930 movies released between 1936 and 2018. The dataset includes variables such as film identifiers, characteristics, cast, and production details. The aim was to create a refined model with minimal error, providing insights into factors influencing IMDb ratings and predictions for 12 upcoming movies.

## Key Features
- **Data Cleaning:** Addressed inconsistencies, standardized distributor names, and removed low-variance predictors.
- **Feature Engineering:** Transformed skewed data, dummified categorical variables, and created interaction terms.
- **Model Development:** Used Generalized Linear Models with cross-validation to minimize mean squared error (MSE).
- **Results:** Final model achieved an MSE of 0.48 and demonstrated significant predictors like film genres, release month, and director fame. Movies and their predicted scores can be found [here](https://github.com/ThisIsMirk/imdb-score-prediction/blob/main/final_prediction.csv).
  
## Authors
- **Felix Veaux**
- **Juan David Ovalle**
- **Arturo Medina**
- **Mirza Abubacker**
- **Hazel Foo**
- **Jean Nieto**

## Report
The complete analysis and methodology can be found in the [project report](https://github.com/ThisIsMirk/imdb-score-prediction/blob/main/Project%20Overview.pdf).
