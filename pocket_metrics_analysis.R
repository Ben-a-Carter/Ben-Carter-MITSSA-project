
require('dplyr', "grid", "ggplot2", "gganimate", 
        'tidyverse', 'ggpubr', 'png', "apaTables", 
        "survival", "ggfortify", "survminer", 
        "pammtools", "vangogh", "tidymodels",
        "bonsai", "vip", "caret", "catboost",
        "forcats")

screens <- read.csv("player_play.csv") %>% 
  group_by(gameId, playId) %>%
  mutate(isScreen = ifelse(routeRan == "SCREEN", "yes", NA)) %>%
  fill(isScreen, .direction = "updown") %>%
  select(gameId, playId, isScreen) %>%
  distinct(.keep_all = T)

plays <- read.csv("plays.csv") %>%
  select(gameId, playId, playAction, pff_runConceptPrimary, timeToThrow, timeToSack, timeInTackleBox, dropbackType, passResult) %>%
  filter(pff_runConceptPrimary != "TRICK" | is.na(pff_runConceptPrimary)) %>%
  select(-pff_runConceptPrimary)
  
files <- list.files(pattern = "*.csv") %>%
  lapply(., read.csv) %>%
  do.call(rbind, .)

test_data <- files %>%
  filter(!is.na(los) & quarter != 5 & down != 4) %>%
  select(-c(timeToThrow, possessionTeam, los, lineToGain, 
            penTime, def_success, passResult)) %>% 
  na.omit() %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.integer), as.numeric)) %>%
  left_join(screens) %>%
  filter(is.na(isScreen)) %>%
  left_join(plays) %>%
  select(gameId, playId, event_fill, eventTime, timeToThrow, timeToSack, timeInTackleBox, dropbackType, passResult, pocketPen) %>% 
  filter(dropbackType %in% c("TRADITIONAL", "SCRAMBLE")) %>%
  group_by(gameId, playId) %>%
  filter(!is.na(event_fill) & event_fill %in% c("pass_forward", "qb_sack", "qb_strip_sack", "run")) %>%
  mutate(time = ifelse(dropbackType == "SCRAMBLE" & event_fill == "pass_forward", timeInTackleBox, eventTime), 
         passResult = ifelse(dropbackType == "SCRAMBLE" & event_fill == "run" & passResult == "", "R", passResult), 
         passResult = ifelse(passResult %in% c("I", "IN"), "IN", passResult), 
         passResult = ifelse(event_fill == "qb_sack" & passResult == "", "S", passResult)) %>%
  select(gameId, playId, event_fill, time, passResult, pocketPen)
  

surv_data <- test_data %>%
  mutate(pocketPen = ifelse(pocketPen == "yes", 1, 0))

km_fit <- survfit(Surv(time, pocketPen) ~ event_fill, surv_data)
km_fit

surv_diff <- survdiff(Surv(time, pocketPen) ~ event_fill, surv_data)
surv_diff

surv_data <- surv_summary(km_fit) %>%
  mutate(median_time = ifelse(event_fill == "pass_forward" & time == 3.1, time, 
                              ifelse(event_fill == "qb_sack"& time == 2.5, time,  
                                     ifelse(event_fill == "run" & time == 3, time, NA))))

ggplot(surv_data, aes(x = time, y = surv, ymin = lower, ymax = upper,
                      color = event_fill, fill = event_fill)) +
  geom_ribbon(stat = "stepribbon",  color = NA, alpha = 0.15) +
  geom_smooth(method = "gam", se = F) + # Add a loess smoothed line
  geom_segment(aes(x = median_time, y = 0.5, xend = median_time, yend = 0),
               color = "black", size = .5, linetype = "dashed") +
  geom_segment(aes(x = median_time, y = 0.5, xend = median_time, yend = 0),
               color = "black", size = .5, linetype = "dashed") +
  geom_segment(aes(x = median_time, y = 0.5, xend = median_time, yend = 0),
               color = "black", size = .5, linetype = "dashed") +
  geom_segment(aes(x = 1.7, y = 0.5, xend = median_time, yend = 0.5),
               color = "black", size = .5, linetype = "dashed") +
  labs(x = "Time",
       y = "Survival Probability") +
  scale_color_manual(name = "Outcome",   # custom legend title
                     labels = c("Pass attempt", "Sack", "QB Scramble"),
                     values = c("pass_forward" = "#428C5C", "qb_sack" = "#A63333", "run" = "#27418C")) +
  scale_fill_manual(name = "Outcome",   # custom legend title
                    labels = c("Pass attempt", "Sack", "QB Scramble"),
                    values = c("pass_forward" = "#428C5C", "qb_sack" = "#A63333", "run" = "#27418C")) +
  annotate("text", x = 8, y = 0.75, size = 4, color = "grey50", hjust = 1,
           label = "Each decrease in the KM-curve represents\n the difference in proportion of pockets\n that remain intact after a given\n amount of time (s) per\n pass outcome.") +
  annotate("text", x = .5, y = 0.5, size = 4, color = "black", hjust = 0,
           label = "Log-rank p<.0001") +
  theme_classic() + 
  theme(
    axis.text.y = element_text(hjust = 0, size = 10), 
    legend.position = "top", 
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "grey99", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
    strip.background = element_blank(),
    strip.text.y = element_blank())

player_play <- read.csv("player_play.csv") %>%
  select(gameId, playId, nflId, causedPressure) %>%
  distinct(.keep_all = T) %>%
  mutate(causedPressure = ifelse(causedPressure == "TRUE", 1, 0)) %>%
  select(-nflId) %>%
  group_by(gameId, playId) %>%
  mutate(causedPressure = sum(causedPressure)) %>% 
  distinct(.keep_all = T)

test_data <- files %>%
  filter(!is.na(los) & quarter != 5 & down != 4) %>%
  select(-c(timeToThrow, possessionTeam, los, lineToGain, 
            penTime, def_success, passResult)) %>% 
  na.omit() %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.integer), as.numeric)) %>%
  left_join(screens) %>%
  filter(is.na(isScreen)) %>%
  left_join(plays) %>%
  filter(dropbackType %in% c("TRADITIONAL", "SCRAMBLE")) %>%
  group_by(gameId, playId) %>%
  filter(!is.na(event_fill) & event_fill %in% c("pass_forward", "qb_sack", "qb_strip_sack", "run")) %>%
  mutate(time = ifelse(dropbackType == "SCRAMBLE" & event_fill == "pass_forward", timeInTackleBox, eventTime), 
         passResult = ifelse(dropbackType == "SCRAMBLE" & event_fill == "run" & passResult == "", "R", passResult), 
         passResult = ifelse(passResult %in% c("I", "IN"), "IN", passResult), 
         passResult = ifelse(event_fill == "qb_sack" & passResult == "", "S", passResult)) %>%
  ungroup() %>%
  left_join(player_play) %>%
  select(-c(gameId, playId, timeToThrow, timeToSack, timeInTackleBox, dropbackType, passResult, isScreen, event_fill, eventTime)) 

dt <- as.data.table(test_data)
y <- dt$pocketPen
X <- dt[, !c("pocketPen")]

n <- nrow(X)
idx <- sample(seq_len(n), 0.7*n)
train_pool <- catboost.load_pool(data = X[idx, ], label = y[idx])
test_pool  <- catboost.load_pool(data = X[-idx, ], label = y[-idx])
X_train <- X[idx, ]
y_train <- y[idx]

train_pool <- catboost.load_pool(data = X_train, label = y_train)

fit_control <- trainControl(
  method = "cv",
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary 
)

grid <- expand.grid(
  depth = c(5, 6, 7),
  learning_rate = c(0.01, 0.05, 0.1),
  l2_leaf_reg = c(2, 3, 4), 
  rsm = c(0.9, 1.0, 1.1),
  border_count = c(64),
  iterations = c(400, 500, 600)
)
model_tuned <- train(
  x = X_train,
  y = as.factor(make.names(y_train)), 
  method = catboost.caret,
  tuneGrid = grid,
  trControl = fit_control)

print(model_tuned$bestTune)

params <- list(
  loss_function = 'Logloss',
  eval_metric = 'AUC',
  iterations = 600,
  depth = 5,
  learning_rate = 0.1,
  rsm = 0.9,
  l2_leaf_reg = 4,
  border_count = 64,
  random_seed = 123,
  od_type = 'Iter',
  od_wait = 50
)

model <- catboost.train(train_pool, test_pool, params = params)

pred_prob <- catboost.predict(model, test_pool, prediction_type = "Probability")
pred <- as.integer(pred_prob > 0.5)

vImp <- as.data.frame(catboost.get_feature_importance(model, pool = test_pool, type = 'FeatureImportance')) %>%
  arrange(-V1) %>%
  rownames_to_column() %>%
  rename(Variable = rowname, 
         `Feature Importance` = V1) %>% 
  mutate(Variable = as.factor(Variable)) %>%
  slice_max(order_by = `Feature Importance`, n = 10) 

vImp %>%
  rowwise() %>%
  summarise(Variable = Variable,
            `Feature Importance` = seq(0, `Feature Importance`, by = 0.001)) %>%
  unnest(cols = `Feature Importance`) %>%
  mutate(Variable = case_when(Variable == "time" ~ "Time",
                              Variable == "causedPressure" ~ "Pressures",
                              Variable == "sd_oline_distances" ~ "sdOLCohesion", 
                              Variable != c("time", "causedPressure", "sd_oline_distances") ~ Variable),
         Variable = fct_reorder(Variable, `Feature Importance`, .fun = max, .desc = FALSE)) %>%
  ggplot() +
  geom_tile(aes(
    x = Variable,
    y = `Feature Importance`,
    fill = `Feature Importance`,
    width = 0.85, color = `Feature Importance`
  )) +
  coord_flip() +
  scale_color_gradient(low = "#819BB4", high = "#27418C") +
  theme_classic() + 
  theme(axis.text.y = element_text(hjust = 1, size = 10), 
    axis.title.y = element_blank(), 
    legend.position = "none", 
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "grey99", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
    strip.background = element_blank(),
    strip.text.y = element_blank())
  
testPen <- as.vector(testPen$pocketPen)

caret::confusionMatrix(as.factor(testPen), as.factor(pred), positive = "1")

