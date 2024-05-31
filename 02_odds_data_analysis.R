## This script analyzes the performance of
## bookmaker odds-implied probabilities
## on predicting actual match outcomes

# Load needed libraries
library(tidyverse)
library(stargazer)

rm(list=ls())

# Read CSV
games <- read.csv("odds_and_R_data.csv")

# function to calculate probability of a team winning given American odds
calculate_implied_odds <- function(odds) {
    if (is.na(odds)) {
        return(NA)
    } else if (odds > 0) {
        implied_probability <- 100 / (odds + 100)
    } else {
        implied_probability <- abs(odds) / (abs(odds) + 100)
    }
    return(implied_probability)
}


# Create new implied odds variable and probability of favorites' winning
games <- games %>% mutate(bm_prob_home_close = sapply(H_Close, calculate_implied_odds),
                          bm_prob_away_close = sapply(V_Close, calculate_implied_odds),
                          bm_prob_home_open = sapply(H_Open, calculate_implied_odds),
                          bm_prob_away_open = sapply(V_Open, calculate_implied_odds))

# normalize probabilities such that bm_home + bm_away = 1
normalization_bm_close <- (games$bm_prob_home_close + games$bm_prob_away_close)
normalization_bm_open <- (games$bm_prob_home_open + games$bm_prob_away_open)

games <- games %>% mutate(bm_prob_home_close = bm_prob_home_close / normalization_bm_close,
                          bm_prob_away_close = bm_prob_away_close / normalization_bm_close,
                          prob_margin_close = bm_prob_home_close - bm_prob_away_close,  
                          fav_team_win_bm_close = (abs(prob_margin_close) + 1) / 2,
                          bm_prob_home_open = bm_prob_home_open / normalization_bm_open,
                          bm_prob_away_open = bm_prob_away_open / normalization_bm_open,
                          prob_margin_open = bm_prob_home_open - bm_prob_away_open,  
                          fav_team_win_bm_open = (abs(prob_margin_open) + 1) / 2)

games$fav_team_win_close <- ifelse((games$prob_margin_close >= 0 & games$home_win==1) |
                                  (games$prob_margin_close < 0 & games$home_win==0), 1, 0)
games$fav_team_win_open <- ifelse((games$prob_margin_open >= 0 & games$home_win==1) |
                                    (games$prob_margin_open < 0 & games$home_win==0), 1, 0)

## Split data into bins for plotting
# fav team analysis
breaks <- c(seq(0.5, 0.75, by=0.05), 1)
games$prob_margin_bins_close <- cut(games$fav_team_win_bm_close, breaks = breaks, include.lowest = TRUE)
games$prob_margin_bins_open <- cut(games$fav_team_win_bm_open, breaks = breaks, include.lowest = TRUE)



odds_summ_close <- games %>% group_by(prob_margin_bins_close) %>%
                                      summarise(mean_fav_team_win = mean(fav_team_win_close),
                                                se = sd(fav_team_win_close) / n(),
                                                mean_fav_team_win_bm = mean(fav_team_win_bm_close),
                                                n = n())

odds_summ_open <- games %>% filter(fav_team_win_bm_open<=1) %>% group_by(prob_margin_bins_open) %>%
                                    summarise(mean_fav_team_win = mean(fav_team_win_open),
                                              se = sd(fav_team_win_open) / n(),
                                              mean_fav_team_win_bm = mean(fav_team_win_bm_open),
                                              n = n())

# home team analysis
breaks <- c(0, 0.25, 0.4, 0.5, 0.6, 0.7, 1)
games$home_win_bins_close <- cut(games$bm_prob_home_close, breaks = breaks, include.lowest = TRUE)
games$home_win_bins_open <- cut(games$bm_prob_home_open, breaks = breaks, include.lowest = TRUE)


odds_summ_close_home <- games %>% group_by(home_win_bins_close) %>%
                                          summarise(mean_home_win = mean(home_win),
                                                    se = sd(home_win) / n(),
                                                    n = n())

odds_summ_open_home <- games %>% group_by(home_win_bins_open) %>%
                                          summarise(mean_home_win = mean(home_win),
                                                    se = sd(home_win) / n(),
                                                    n = n())

## Plotting

# closing odds (fav team)
p_close <- ggplot(odds_summ_close, aes(x = prob_margin_bins_close, y = mean_fav_team_win, fill = prob_margin_bins_close )) +
                  geom_col() +
                  geom_errorbar(aes(ymin = mean_fav_team_win - se, ymax = mean_fav_team_win + se), width = 0.2, color = "black") +
                  labs(x = "Bookmaker Predicted Probability of Favorite Team Winning (Bins)", 
                       y = "Share of Favorite Team Actually Winning") +
                  scale_fill_brewer(palette = "Pastel1") +
                  theme_minimal() + theme(legend.position = "none")
ggsave("figures/bm_vs_true_prob_close.png", plot = p_close, width = 20, height = 10, units = "cm",bg="white")
plot(p_close)

# opening odds (fav team)
p_open <- ggplot(odds_summ_open, aes(x = prob_margin_bins_open, y = mean_fav_team_win, fill = prob_margin_bins_open )) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_fav_team_win - se, ymax = mean_fav_team_win + se), width = 0.2, color = "black") +
  labs(x = "Bookmaker Predicted Probability of Favorite Team Winning (Bins)", 
       y = "Share of Favorite Team Actually Winning") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() + theme(legend.position = "none")
ggsave("figures/bm_vs_true_prob_open.png", plot = p_open, width = 20, height = 10, units = "cm",bg="white")
plot(p_open)


# closing odds (home team)
p_close_home <- ggplot(odds_summ_close_home, aes(x = home_win_bins_close, y = mean_home_win, fill = home_win_bins_close )) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_home_win - se, ymax = mean_home_win + se), width = 0.2, color = "black") +
  labs(x = "Bookmaker Predicted Probability of Home Team Winning (Bins)", 
       y = "Share of Home Team Actually Winning") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() + theme(legend.position = "none")
ggsave("figures/bm_vs_true_prob_close_home.png", plot = p_close_home, width = 20, height = 10, units = "cm",bg="white")
plot(p_close)


# opening odds (home team)
p_open_home <- ggplot(odds_summ_open_home, aes(x = home_win_bins_open, y = mean_home_win, fill = home_win_bins_open )) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_home_win - se, ymax = mean_home_win + se), width = 0.2, color = "black") +
  labs(x = "Bookmaker Predicted Probability of Home Team Winning (Bins)", 
       y = "Share of Home Team Actually Winning") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() + theme(legend.position = "none")
ggsave("figures/bm_vs_true_prob_open_home.png", plot = p_open_home, width = 20, height = 10, units = "cm",bg="white")
plot(p_open)

## OLS and Logistic Regression


# LHS variable is favorite team won
odds_lm_close <- lm(fav_team_win_close ~ fav_team_win_bm_close, data=games)
odds_logm_close <- glm(fav_team_win_close ~ fav_team_win_bm_close, family = binomial(), data = games)
odds_lm_open <- lm(fav_team_win_open ~ fav_team_win_bm_open, data=games)
odds_logm_open <- glm(fav_team_win_open ~ fav_team_win_bm_open, family = binomial(), data = games)

stargazer(odds_lm_close, odds_logm_close, odds_lm_open, odds_logm_open, type="latex", 
          dep.var.labels = c("Favorite Team Won (Closing Odds)", "Favorite Team Won (Opening Odds)"), 
          covariate.labels = c("Prob. of Favorite Winning (Bookmaker Closing Odds)", 
                               "Prob. of Favorite Winning (Bookmaker Opening Odds)"),
          omit.stat = c("ser", "adj.rsq", "aic"), df = FALSE)


# LHS variable is home team won
odds_lm_close <- lm(home_win ~ bm_prob_home_close, data=games)
odds_logm_close <- glm(home_win ~ bm_prob_home_close, family = binomial(), data = games)
odds_lm_open <- lm(home_win ~ bm_prob_home_open, data=games)
odds_logm_open <- glm(home_win ~ bm_prob_home_open, family = binomial(), data = games)

stargazer(odds_lm_close, odds_logm_close, odds_lm_open, odds_logm_open, type="latex", 
          dep.var.labels = c("Home Team Won (Closing Odds)", "Home Team Won (Opening Odds)"), 
          covariate.labels = c("Prob. of Home Team Winning (Bookmaker Closing Odds)", 
                               "Prob. of Home Team Winning (Bookmaker Opening Odds)"),
          omit.stat = c("ser", "adj.rsq", "aic"), df = FALSE)








    