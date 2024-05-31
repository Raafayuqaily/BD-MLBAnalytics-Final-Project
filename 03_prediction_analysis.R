##### Changes to get prediction working
library(dplyr)
library(zoo)
library(iml)
library(ggplot2)
library(dplyr)
library(caret)

df <- read.csv("odds_and_R_data.csv")

create_team_df <- function(team) {
  df_team_v <- df %>% filter(Visiting_Team_Code == team)
  df_team_v$opponent <- df_team_v$Home_Team_Code
  df_team_v$home_game <- 0

  df_team_h <- df %>% filter(Home_Team_Code == team)
  df_team_h$opponent <- df_team_h$Visiting_Team_Code
  df_team_h$home_game <- 1

  df_team <- bind_rows(df_team_h, df_team_v) %>% arrange(as.Date(Date, format="%m/%d/%Y"))

  df_team <- df_team %>%
    mutate(
      runs = ifelse(home_game == 1, H_Final, V_Final),
      AB = ifelse(home_game == 1, HmAB, VisAB),
      H = ifelse(home_game == 1, HmH, VisH),
      `2B` = ifelse(home_game == 1, HmD, VisD),
      `3B` = ifelse(home_game == 1, HmT, VisT),
      HR = ifelse(home_game == 1, HmHR, VisHR),
      BB = ifelse(home_game == 1, HmBB, VisBB),
      SB = ifelse(home_game == 1, HmSB, VisSB),
      CS = ifelse(home_game == 1, HmCS, VisCS),
      ERR = ifelse(home_game == 1, HmE, VisE)
    )

  for (winsize in c(162, 30)) {
    suff <- as.character(winsize)
    for (raw_col in c("AB", "H", "2B", "3B", "HR", "BB", "runs", "SB", "CS", "ERR")) {
      new_col <- paste0("rollsum_", raw_col, "_", suff)
      df_team[[new_col]] <- rollsum(df_team[[raw_col]], winsize, fill = NA, align = "right")
    }

    df_team[[paste0("rollsum_BATAVG_", suff)]] <- df_team[[paste0("rollsum_H_", suff)]] / df_team[[paste0("rollsum_AB_", suff)]]
    df_team[[paste0("rollsum_OBP_", suff)]] <- (df_team[[paste0("rollsum_H_", suff)]] + df_team[[paste0("rollsum_BB_", suff)]]) /
      (df_team[[paste0("rollsum_AB_", suff)]] + df_team[[paste0("rollsum_BB_", suff)]])
    df_team[[paste0("rollsum_SLG_", suff)]] <- (df_team[[paste0("rollsum_H_", suff)]] + df_team[[paste0("rollsum_2B_", suff)]] +
                                                 2 * df_team[[paste0("rollsum_3B_", suff)]] + 3 * df_team[[paste0("rollsum_HR_", suff)]]) /
      df_team[[paste0("rollsum_AB_", suff)]]
    df_team[[paste0("rollsum_OBS_", suff)]] <- df_team[[paste0("rollsum_OBP_", suff)]] + df_team[[paste0("rollsum_SLG_", suff)]]
  }

  df_team <- df_team %>%
    arrange(season, Date) %>%
    group_by(season) %>%
    mutate(season_game = season * 1000 + row_number()) %>%
    ungroup()

  return(df_team)
}

# Ensure df and team_df are available from the previous steps
# df <- read.csv("odds_and_R_data.csv")
team_df <- create_team_df("NYA")

# Prepare the data
# Adjust home_win based on whether the team is playing at home or away
team_df <- team_df %>%
  mutate(adjusted_home_win = ifelse(home_game == 1, home_win, 1 - home_win))

# Select relevant columns for the model (rolling statistics, adjusted target variable, and implied closing odds, home_game)
model_data <- team_df %>%
  select(contains("rollsum"), contains("BATAVG"), contains("OBP"), contains("SLG"), contains("OBS"), adjusted_home_win, home_game, h_implied_closing_odds, v_implied_closing_odds)

# Remove rows with NA values
model_data <- model_data %>% drop_na()

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(model_data$adjusted_home_win, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

model <- glm(adjusted_home_win ~ . -h_implied_closing_odds -v_implied_closing_odds, data = train_data, family = binomial)

summary(model)

test_data$predicted_prob <- predict(model, newdata = test_data, type = "response")

# Add a column to distinguish home and away games for comparison
test_data <- test_data %>%
  mutate(implied_odds = ifelse(home_game == 1, h_implied_closing_odds, v_implied_closing_odds))

ggplot(test_data, aes(x = implied_odds, y = predicted_prob)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted Probabilities vs Implied Odds",
       x = "Implied Odds",
       y = "Predicted Probabilities") +
  theme_minimal()

log_loss <- function(y, pred) {
  return(-mean(y * log(pred) + (1 - y) * log(1 - pred)))
}

log_loss_predicted <- log_loss(test_data$adjusted_home_win, test_data$predicted_prob)
log_loss_implied <- log_loss(test_data$adjusted_home_win, test_data$implied_odds)
print(paste("Log Loss (Predicted):", log_loss_predicted))
print(paste("Log Loss (Implied):", log_loss_implied))




# Lasso version
# Add interactions, and reduce variable count

# create interaction model
formula <- as.formula(paste("~ .^2", collapse = "+"))
design_matrix <- model.matrix(formula, as.data.frame(select(train_data, -h_implied_closing_odds, -v_implied_closing_odds, -adjusted_home_win)))[, -1]  

# fit lasso model to training data
lasso_mod <- gamlr(design_matrix, train_data$adjusted_home_win)
plot(lasso_mod, main = "Lasso Path")

# Test performance on testing set
test_data2 = test_data %>%
  select(-h_implied_closing_odds, -v_implied_closing_odds, -predicted_prob, -implied_odds, -adjusted_home_win)
test_data_model = model.matrix(formula, as.data.frame(test_data2))[, -1]  

test_data$predicted_prob_lasso = predict(lasso_mod, newdata = test_data_model, type = "response")@x

log_loss_predicted <- log_loss(test_data$adjusted_home_win, test_data$predicted_prob_lasso)
log_loss_implied <- log_loss(test_data$adjusted_home_win, test_data$implied_odds)
print(paste("Log Loss (Predicted):", log_loss_predicted))
print(paste("Log Loss (Implied):", log_loss_implied))


# look at parameters
coefficients <- coef(lasso_mod, s = lasso_mod$lambda.aicc)[, 1]
non_zero_coefficients <- coefficients[coefficients != 0]
significant_predictors <- non_zero_coefficients[order(abs(non_zero_coefficients), decreasing = TRUE)]

cat("There are ", length(significant_predictors), " predictors of Run_diff that have non-zero coefficients. \n")

top_10_predictors <- data.frame(Predictor = names(significant_predictors), Coefficient = significant_predictors)
top_10_predictors <- head(top_10_predictors, 10)
top_10_predictors




