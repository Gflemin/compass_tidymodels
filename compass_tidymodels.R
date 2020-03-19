
# here
install.packages("here")
library(here)

# pacman
install.packages("pacman")
library(pacman)

# libraries
source(here::here("libraries.R"))

# get the data
loader2 = function() {
  df= read_csv(here::here("data/compas-scores-raw.csv"))
  df
}
df_raw = loader2()

# save the cols we want to keep
good_cols = c("Person_ID", "AssessmentID", "Sex_Code_Text", "Ethnic_Code_Text", "DateOfBirth", "CustodyStatus", "DisplayText",
              "MaritalStatus", "ScoreText") # exhaustive

# get distinct prediction tasks
tasks = df_raw %>% 
  select(DisplayText) %>% 
  distinct() %>% 
  pull()
  
# build and prep a recipe for our data 
prepper = function(x, good_cols = list(NA), grouping_var, category = NA) {
  x %>% 
    select(all_of(good_cols)) %>% 
    clean_names() %>% 
    group_by({{grouping_var}}) %>% 
    filter(display_text == category) %>% 
    arrange(person_id) %>% 
    mutate(rowname = {{grouping_var}}) %>% 
    column_to_rownames() %>% 
    select(-{{grouping_var}}, -display_text, -person_id) %>% 
    ungroup() %>% 
    mutate(date_of_birth = year(mdy(date_of_birth))) %>%
    mutate(date_of_birth = as.character(date_of_birth)) %>%
    mutate(date_of_birth = 2014 - as.numeric(str_replace(date_of_birth, "^(20)", "19"))) %>%
    mutate(date_of_birth = case_when(
      ((25 <= date_of_birth) & (date_of_birth <= 45)) ~ "25 to 45",
      (date_of_birth < 25) ~ "Younger than 25",
      (date_of_birth > 45) ~ "Older than 45"
    )) %>%
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

# assign each of our unique prediction tasks to a list 
nu_list = c()
for (i in tasks) {
  nu_list[[i]] = prepper(df_raw, good_cols = good_cols, assessment_id, category = i)
} # produces 3 multiclass classification tasks 
nu_list





### OK, it looks like I should be able to duplicate the data cleaning code completely in recipes. lets try!



# Classification tasker
tasker_class = function(data, id, target) { # data: df produced by combiner (dataframe), id: identifier for the new task (string), target: column in df 
  # that corresponds to the target vector of our model (vector)
  task = TaskClassif$new(id = id, backend = data, target = target)
  task
}

# Regression tasker
tasker_regr = function(data, id, target) { # data: df produced by combiner (dataframe), id: identifier for the new task (string), target: column in df 
  # that corresponds to the target vector of our model (vector)
  task = TaskRegr$new(id = id, backend = data, target = target)
  task
}

# wrap our data into a mlr3 "task" object 
violence_task = tasker_class(violence_data, id = "assessment_id", target = "score_text")

# choose our mlr3 learers
lrn_list_classif = c("classif.rpart", "classif.ranger")

# generate our mlr3 learners 
learners = learn_gener(lrn_list_classif)


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

