library(tidyverse)
library(magrittr)
library(gamlr)

dat = read.csv("odds_and_R_data.csv") 
dat2 = dat %>%
  select(-X, -Date, -ParkID, -VisMgrNm, -HmMgrNm, -WinPNm, -GWinRBINm,
         -H_Pitcher, -V_Pitcher, -Home_Team_Code, -Visiting_Team_Code,
         -h_implied_opening_odds, -v_implied_opening_odds,
         -h_implied_closing_odds, -v_implied_closing_odds, 
         -h_implied_opening_odds_mid, -h_implied_closing_odds_mid,
         -H_Opening_OU, -H_Opening_Odds, -H_Closing_OU, -H_Closing_Odds,
         -V_Opening_OU, -V_Opening_Odds, -V_Closing_OU, -V_Closing_Odds,
         -H_Open, -H_Close, -H_Run_Line, -H_Run_Line_Odds,
         -V_Open, -V_Close, -V_Run_Line, -V_Run_Line_Odds,
         -run_dif, -VisRuns, -HmRuns, -H_Final, -V_Final, -season,
         -Day, -VisTmLg, -HmTmLg, -DayNight, -Year, -Month, -Day
         ) %>%
  select(-VisRBI, -HmRBI, -VisER, -HmER, -VisHR, -HmHR, -VisTER, -HmTER,
         -H_1st, -H_2nd, -H_3rd, -H_4th, -H_5th, -H_6th, -H_7th, -H_8th, -H_9th,
         -V_1st, -V_2nd, -V_3rd, -V_4th, -V_5th, -V_6th, -V_7th, -V_8th, -V_9th,
         -VisPO, -HmPO, -NumOuts
         ) %>%
  replace_na(list(H_9th = 0)) %>%
  na.omit() %>%
  filter(Attendance > 0) 

dat3 = dat %>%
  select(-X, -Date, -ParkID, -VisMgrNm, -HmMgrNm, -WinPNm, -GWinRBINm,
         -H_Pitcher, -V_Pitcher, -Home_Team_Code, -Visiting_Team_Code,
         -h_implied_opening_odds, -v_implied_opening_odds,
         -h_implied_closing_odds, -v_implied_closing_odds, 
         -h_implied_opening_odds_mid, -h_implied_closing_odds_mid,
         -H_Opening_OU, -H_Opening_Odds, -H_Closing_OU, -H_Closing_Odds,
         -V_Opening_OU, -V_Opening_Odds, -V_Closing_OU, -V_Closing_Odds,
         -H_Open, -H_Close, -H_Run_Line, -H_Run_Line_Odds,
         -V_Open, -V_Close, -V_Run_Line, -V_Run_Line_Odds,
         -home_win, -VisRuns, -HmRuns, -H_Final, -V_Final, -season,
         -Day, -VisTmLg, -HmTmLg, -DayNight, -Year, -Month, -Day
  ) %>%
  select(-VisRBI, -HmRBI, -VisER, -HmER, -VisHR, -HmHR, -VisTER, -HmTER,
         -H_1st, -H_2nd, -H_3rd, -H_4th, -H_5th, -H_6th, -H_7th, -H_8th, -H_9th,
         -V_1st, -V_2nd, -V_3rd, -V_4th, -V_5th, -V_6th, -V_7th, -V_8th, -V_9th,
         -VisPO, -HmPO, -NumOuts
  ) %>%
  replace_na(list(H_9th = 0)) %>%
  na.omit() %>%
  filter(Attendance > 0) 


# Predict home winning odds, see what they're capturing

# Home win with stepwise regression
hw_step_null = glm(home_win ~ 1, data = dat2, family = "binomial")
hw_step_full = glm(home_win ~ ., data = dat2, family = "binomial")
hw_step_fwd = step(hw_step_null, scope = formula(hw_step_full), dir="forward")


# Home win with lasso
hw_lasso = cv.gamlr(select(dat2,-home_win), y = dat2$home_win, family = "binomial")
coef(hw_lasso)

# Score diff with stepwise regression
rd_step_null = glm(run_dif ~ 1, data = dat3, family = "gaussian")
rd_step_full = glm(run_dif ~ ., data = dat3, family = "gaussian")
rd_step_fwd = step(rd_step_null, scope = formula(rd_step_full), dir="forward")

# Score diff with lasso
rd_lasso = cv.gamlr(select(dat3,-run_dif), y = dat3$run_dif, family = "gaussian")
coef(rd_lasso)



