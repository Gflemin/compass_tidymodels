
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
                                 "xgboost" = boost_tree(mode = "classification"))


###### This is where I am stuck ###### 

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

###### This is where I am stuck ###### 


