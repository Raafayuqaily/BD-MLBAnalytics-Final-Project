# MLB Game Outcome Prediction and Betting Analysis

## 1. Introduction
This project explores the prediction of Major League Baseball (MLB) game outcomes and identifies the key factors that influence a home team's victory. The analysis spans games from 2010 to 2021 and integrates historical game statistics with betting odds data. Using the New York Yankees as a case study, we applied various statistical and machine learning techniques to develop predictive models and assess their performance against betting market odds.

## 2. Methodology Overview
### Data Sources
1. **Retrosheet Database**: Comprehensive game-by-game analytics for MLB games, including detailed team performance and player statistics.
2. **Sports Book Review (SBR) Odds Data**: Historical betting odds for MLB games, providing bookmaker predictions of game outcomes.

### Data Preprocessing
- Consolidation of multiple dataframes from different seasons into a single dataset.
- Standardization of team codes and handling of missing values.
- Creation of new variables and rolling statistics for predictive modeling.

### Predictive Modeling
- **OLS Logistic Regression**: To predict game outcomes based on historical performance metrics.
- **Multivariate Regression, LASSO Regression, Random Forest Models**: To identify significant factors within a game that predict victory.
- **FDR Analysis**: To control the false discovery rate and identify robust predictors.
- **K-Means Clustering and PCA**: To explore patterns and reduce dimensionality in the dataset.

## 3. Results
### Predictive Models
- **Logistic Regression**: Demonstrated strong predictive capabilities using rolling statistics over 30 and 162 game windows, slightly lower performance compared to betting market odds.
- **Multivariate and LASSO Regression, Random Forest**: Consistently highlighted the importance of offensive metrics such as hits, walks, and hit-by-pitch over defensive measures.

### Key Findings
- Offensive performance metrics are more predictive of home team victory than defensive metrics.
- Betting odds are a reliable benchmark for predicting game outcomes, closely matching actual results.
- Enhanced predictive models show promise for future applications in sports analytics and betting strategies.

## 4. Conclusion
This study provides valuable insights into the factors driving MLB game outcomes and demonstrates the potential of data-driven approaches in sports betting analytics. By integrating statistical inference with big data techniques, we have established a solid foundation for future research and applications in sports prediction.

## 5. Future Work
- **Expand Predictive Models**: Develop models for all MLB teams and explore interaction effects for more accurate predictions.
- **Incorporate Defensive and Pitching Data**: Enhance models with comprehensive defensive metrics and pitcher-specific data.
- **Player-Level Analysis**: Model individual player contributions to improve model accuracy.
- **Online Algorithms**: Implement live data integration for real-time predictions and betting strategies.
