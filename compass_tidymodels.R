
# here
install.packages("here")
library(here)

# pacman
install.packages("pacman")
library(pacman)

# libraries
source(here::here("libraries.R"))

# get the data
loader = function() {
  df = read_csv(here::here("data/compas-scores-raw.csv")) %>% 
    clean_names() %>% 
    mutate(date_of_birth = year(mdy(date_of_birth))) %>%   # had to move all the date stuff here to avoid a factor issue in bake step
    mutate(date_of_birth = as.character(date_of_birth)) %>%
    mutate(date_of_birth = 2014 - as.numeric(str_replace(date_of_birth, "^(20)", "19"))) %>%
    mutate(date_of_birth = case_when(
      ((25 <= date_of_birth) & (date_of_birth <= 45)) ~ "25 to 45",
      (date_of_birth < 25) ~ "Younger than 25",
      (date_of_birth > 45) ~ "Older than 45"
    ))
  df
}
df_raw = loader()

# save the cols we want to keep
good_cols = c("person_id", "assessment_id", "sex_code_text", "ethnic_code_text", "date_of_birth", "custody_status", "display_text",
              "marital_status", "score_text") # exhaustive

# get distinct prediction tasks
tasks = df_raw %>% 
  select(display_text) %>% 
  distinct() %>% 
  pull()

# split our data
df_split = rsample::initial_split(df_raw, prop = 0.8)

# build and prep a recipe for our data 
prepper = function(x, good_cols = list(NA), grouping_var, category = NA) {
  x %>% 
    training() %>% 
    select(all_of(good_cols)) %>% 
    clean_names() %>% 
    group_by({{grouping_var}}) %>% 
    filter(display_text == category) %>% 
    arrange(person_id) %>% 
    mutate(rowname = {{grouping_var}}) %>% 
    column_to_rownames() %>% 
    select(-{{grouping_var}}, -display_text, -person_id) %>% 
    ungroup() %>% 
    recipe(score_text ~ .) %>%  # recipes doesn't behave well with curly curly yet, otherwise I would put this in a target arg
    step_filter(
      ethnic_code_text %in% c("African-American", "Caucasian", "Hispanic"),
      custody_status %in% c("Jail Inmate", "Probation", "Pretrial Defendant"),
      marital_status %in% c("Single", "Married")
    ) %>%
    step_mutate_at(all_nominal(), fn = factor) %>%
    step_mutate_at(all_nominal(), fn = fct_drop) %>%
    step_dummy(all_nominal(), -score_text, one_hot = TRUE) %>% 
    prep() # great, it works
}

# assign each of our unique prepped recipes for the prediction tasks to a list 
generate_train_recipe = function(x) {
  df_train_recipes = c()
  for (i in tasks) {
    df_train_recipes[[i]] = prepper(x, good_cols = good_cols, assessment_id, category = i)
  } # produces 3 multiclass classification tasks 
  names(df_train_recipes) = tasks
  df_train_recipes
}
train_recipes = generate_train_recipe(df_split)

# actually get the train data out
juicer = function(x) {
  df_train_data = c()
  for (i in 1:length(x)) {
    df_train_data[[i]] = juice(x[[i]])
  }
  names(df_train_data) = tasks
  df_train_data
}
juiced_train_data = juicer(train_recipes)

# assign each of our unique prediction tasks to a list 
generate_test_recipe = function(x) {
  df_test_recipes = c()
  for (i in 1:length(x)) {
    df_test_recipes[[i]] = x[[i]] %>% 
      bake(testing(df_split)) 
  }
  names(df_test_recipes) = tasks
  df_test_recipes
}
test_recipes = generate_test_recipe(train_recipes)

# building our models
classification_model_list = list("random_forest" = rand_forest(mode = "classification"), 
                   "decision_tree" = decision_tree(mode = "classification"),
                   "xgboost" = boost_tree(mode = "classification"),
                   "glmnet" = logistic_reg(mode = "classification"))

# ???
enginer3 = function(x, models = classification_model_list) {
  generated_models = c()
  for (i in 1:length(models)) {
    generated_models[[i]] = models[[i]] %>% 
      fit(score_text ~., data = x)
  }
  generated_models
}
fit_model_tibble = map(juiced_train_data, enginer3)

# holder = list()
# for (i in 1:length(fit_model_tibble[[1]])) {
#   holder[[i]] = fit_model_tibble[[1]][[i]] %>% 
#     predict(test_recipes[[i]]) %>%
#     bind_cols(test_recipes[[i]])
# } # need to go one level out

holder2 = list()
for (i in 1:length(fit_model_tibble)) {
  inner_model_list = fit_model_tibble[[i]] # list
  holder = list()
  for (j in 1:length(inner_model_list)) {
    holder[[j]] = inner_model_list[[j]] %>% 
      predict(test_recipes[[i]]) %>%
      bind_cols(test_recipes[[i]])
  }
  holder2 = c(holder2, holder)
} 
holder2

  
### Still to do
#### 1. Account for nested tasks (need another for loop or something)
#### 2. Develop a tuning scheme
#### 3. Add the imp/interp fxs from my pres in 
#### 4. Drake it 


rand_forest(mode = "classification") %>% 
  set_engine("ranger") %>% 
  fit(score_text ~ ., data = juiced_train_data[[1]])

learners = c("random_forest" = rand_forest(mode = "classification") %>% 
               set_engine("ranger") %>% 
               fit(score_text ~ ., data = training(df_train_recipes[["Risk of Failure to Appear"]])))
enginerator = function(x) {
  c("random_forest" = rand_forest(mode = "classification") %>% 
      set_engine("ranger") %>% 
      fit(score_text ~ ., data = df_train_recipes[["Risk of Failure to Appear"]]))
}


######################################################################## JUNK ########################################################################

# # tidymodels libraries
# library(tune)
# library(dials)
# library(tidymodels)
# library(parsnip)
# library(lubridate)

# build our first dataset
violence_data = df_raw %>% 
  select(good_cols) %>% 
  clean_names() %>% 
  group_by(assessment_id) %>% # want to end up with one prediction for each score for each individual assess instance (individual)
  slice(1) %>%  # get the relevant ScaleSet value from each group
  arrange(person_id) %>% 
  ungroup() %>% 
  mutate(date_of_birth = year(mdy(date_of_birth))) %>% 
  mutate(date_of_birth = as.character(date_of_birth)) %>% 
  mutate(date_of_birth = 2014 - as.numeric(str_replace(date_of_birth, "^(20)", "19"))) %>% 
  mutate(date_of_birth = case_when(
    ((25 <= date_of_birth) & (date_of_birth <= 45)) ~ "25 to 45",
    (date_of_birth < 25) ~ "Younger than 25",
    (date_of_birth > 45) ~ "Older than 45"
  )) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-person_id) %>% 
  # mutate_if(is.factor, function(u) lumper(u, 0.14)) # originally did this, but we lose too much info
  filter(ethnic_code_text %in% c("African-American", "Caucasian", "Hispanic")) %>% # focusing on the 3 most common ethnicities
  filter(custody_status %in% c("Jail Inmate", "Probation", "Pretrial Defendant")) %>% 
  filter(marital_status %in% c("Single", "Married")) %>% 
  mutate_if(is.factor, fct_drop) %>% 
  mutate(rowname = assessment_id) %>% 
  column_to_rownames() %>% 
  select(-assessment_id, -display_text)

# build our second dataset
recidivism_data = df_raw %>% 
  select(good_cols) %>% 
  clean_names() %>% 
  group_by(assessment_id) %>% # want to end up with one prediction for each score for each individual assess instance (individual)
  slice(2) %>%  # get the relevant ScaleSet value from each group
  arrange(person_id) %>% 
  ungroup() %>% 
  mutate(date_of_birth = year(mdy(date_of_birth))) %>% 
  mutate(date_of_birth = as.character(date_of_birth)) %>% 
  mutate(date_of_birth = 2014 - as.numeric(str_replace(date_of_birth, "^(20)", "19"))) %>% 
  mutate(date_of_birth = case_when(
    ((25 <= date_of_birth) & (date_of_birth <= 45)) ~ "25 to 45",
    (date_of_birth < 25) ~ "Younger than 25",
    (date_of_birth > 45) ~ "Older than 45"
  )) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-person_id) %>% 
  # mutate_if(is.factor, function(u) lumper(u, 0.14)) # originally did this, but we lose too much info
  filter(ethnic_code_text %in% c("African-American", "Caucasian", "Hispanic")) %>% # focusing on the 3 most common ethnicities
  filter(custody_status %in% c("Jail Inmate", "Probation", "Pretrial Defendant")) %>% 
  filter(marital_status %in% c("Single", "Married")) %>% 
  mutate_if(is.factor, fct_drop) %>% 
  mutate(rowname = assessment_id) %>% 
  column_to_rownames() %>% 
  select(-assessment_id)

# build our third dataset
appear_data = df_raw %>% 
  select(good_cols) %>% 
  clean_names() %>% 
  group_by(assessment_id) %>% # want to end up with one prediction for each score for each individual assess instance (individual)
  slice(3) %>%  # get the relevant ScaleSet value from each group
  arrange(person_id) %>% 
  ungroup() %>% 
  mutate(date_of_birth = year(mdy(date_of_birth))) %>% 
  mutate(date_of_birth = as.character(date_of_birth)) %>% 
  mutate(date_of_birth = 2014 - as.numeric(str_replace(date_of_birth, "^(20)", "19"))) %>% 
  mutate(date_of_birth = case_when(
    ((25 <= date_of_birth) & (date_of_birth <= 45)) ~ "25 to 45",
    (date_of_birth < 25) ~ "Younger than 25",
    (date_of_birth > 45) ~ "Older than 45"
  )) %>% 
  mutate_if(is.character, as.factor) %>% 
  select(-person_id) %>% 
  # mutate_if(is.factor, function(u) lumper(u, 0.14)) # originally did this, but we lose too much info
  filter(ethnic_code_text %in% c("African-American", "Caucasian", "Hispanic")) %>% # focusing on the 3 most common ethnicities
  filter(custody_status %in% c("Jail Inmate", "Probation", "Pretrial Defendant")) %>% 
  filter(marital_status %in% c("Single", "Married")) %>% 
  mutate_if(is.factor, fct_drop) %>% 
  mutate(rowname = assessment_id) %>% 
  column_to_rownames() %>% 
  select(-assessment_id)

# violence recipe raw
violence_recipe = df_raw %>% 
  select(good_cols) %>% 
  clean_names() %>% 
  group_by(assessment_id) %>% 
  filter(display_text == "Risk of Violence") %>% 
  arrange(person_id) %>% 
  mutate(rowname = assessment_id) %>% 
  column_to_rownames() %>% 
  select(-assessment_id, -display_text, -person_id) %>% 
  ungroup() %>% 
  mutate(date_of_birth = year(mdy(date_of_birth))) %>% 
  mutate(date_of_birth = as.character(date_of_birth)) %>% 
  mutate(date_of_birth = 2014 - as.numeric(str_replace(date_of_birth, "^(20)", "19"))) %>% 
  mutate(date_of_birth = case_when(
    ((25 <= date_of_birth) & (date_of_birth <= 45)) ~ "25 to 45",
    (date_of_birth < 25) ~ "Younger than 25",
    (date_of_birth > 45) ~ "Older than 45"
  )) %>% 
  recipe(score_text ~ .) %>% 
  step_filter(
    ethnic_code_text %in% c("African-American", "Caucasian", "Hispanic"),
    custody_status %in% c("Jail Inmate", "Probation", "Pretrial Defendant"),
    marital_status %in% c("Single", "Married")
  ) %>% 
  step_mutate_at(all_nominal(), fn = factor) %>% 
  step_mutate_at(all_nominal(), fn = fct_drop) %>% 
  step_dummy(all_nominal(), -score_text, one_hot = TRUE) # great, it works 


# generate our models (currently using default engines)
enginer = function(x, models = NA) {
  generated_models = c()
  for (i in 1:length(models)) {
    generated_models[[i]] = models[[i]] %>% 
      fit(score_text ~., data = x)
  }
  generated_models
}
fit_models = enginer(juiced_train_data[[1]], models = classification_model_list)
class(fit_models[[1]]) 
# generate our models (currently using default engines)
enginer2 = function(x, models = NA) {
  generated_models = c()
  for (i in 1:length(x)) {
    for (j in 1:length(models)) {
      generated_models[[i, j]] = models[[j]] %>%
        fit(score_text ~., data = x[[i]])
    }
  }
  generated_models
}
fit_models2 = enginer2(juiced_train_data, models = classification_model_list)

# generate our models (currently using default engines)
enginer3 = function(x) {
  x %>% fit(score_text ~., data = )
}

a = enframe(juiced_train_data) 
b = enframe(classification_model_list) 

map(1:nrow(juiced_train_data), ~{ 
  i <- .x
  map(classification_model_list, ~ {
    model <- .x
    fit(model, score_text ~., data = x[[i]])
  })
})


### As of now, having a very difficult time doing this R-style with dataframes. Containing that work here so that I can build a 
### temporary list implementation
fit_model_tibble = map(juiced_train_data, enginer3) %>% 
  enframe()

a = fit_model_tibble %>% 
  mutate(recipes = test_recipes) %>% 
  unnest(value)
a
map(a, function(x) x$value %>% 
      predict(x$recipes) %>% 
      bind_cols(x) %>% 
      metrics(truth = score_text, estimate = .pred_class)
)
a %>% 
  select(value)
a %>% 
  select(value) %>% 
  predict(a$recipes)

fit_models[[1]] %>% 
  predict(test_recipes[[1]]) %>% 
  bind_cols(test_recipes[[1]]) #%>% 
metrics(truth = score_text, estimate = .pred_class)


fit_model_tibble %>% 
  group_by(name) %>% 
  group_split()



a = fit_model_tibble %>% 
  pull(value) %>% 
  extract2(1) 
predict(a, test_recipes[[1]]) %>% 
  bind_cols(test_recipes[[1]]) %>% 
  metrics(truth = score_text, estimate = .pred_class) 

library(magrittr)
predictor = function(x) {
  x %>% 
    pull(2) %>% 
    extract2()
  
}


fit_model_tibble %>% 
  mutate(name = str_remove(name, "[0-9]")) %>% 
  group_by(name) %>% 
  mutate()

library(magrittr)
fit_model_tibble %>% 
  pull(value) %>% 
  extract2(1) %>% 
  class
predict(fit_model_tibble[1, 2] %>% pull(), test_recipes[[1]])

a = fit_model_tibble[1, 2] %>% 
  pull()
class(a)
predict(a, new_data = test_recipes[[1]])
class(test_recipes)
# get a tibble of predictions 
predict(fit_models[[1]], test_recipes[[1]])

# take the tibble of predictions and estimate metrics 
fit_models[[1]] %>% 
  predict(test_recipes[[1]]) %>% 
  bind_cols(test_recipes[[1]]) %>% 
  metrics(truth = score_text, estimate = .pred_class)

###



