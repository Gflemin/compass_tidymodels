

##### Drake plan

### Modify lines in this file to change the underlying models, resampling methods, target variables, etc.

# build our drake plan object
plan = drake_plan(
  # load the raw data
  df_raw = loader(),
  # format and combine the raw data to get our cleaned features
  df = combiner(df_raw, list = c("overall_cond", "overall_qual", "mo_sold", "kitchen_abv_gr", "half_bath", 
                                 "garage_cars", "full_bath", "fireplaces", "bsmt_half_bath", "bsmt_full_bath", "bedroom_abv_gr"), # additional vars to make factors
                0.2, 0.1),
  # wrap our data into a mlr3 "task" object 
  task = tasker(df, id = "house", target = "sale_price"),
  # choose our mlr3 learers
  lrn_list = c("regr.lm", "regr.rpart", "regr.ranger"), # modify learners here
  # actually generate the learners
  learners = learn_gener(lrn_list),
  # create a train/test split for our data
  datasets = splitter(task, 0.8, 2),
  # train our learners
  trained_learners = trainer(learners, task, datasets),
  # perform resampling on our learners
  resample = resampler("cv", 4, 4),
  # benchmark our resampled learners vs. our learners which were just trained on the training data
  benchmarks = benchmarker(task, learners, resample, measures = c("regr.mse", "regr.mape")),
  # generate predictions
  preds = predictor(trained_learners, task, datasets[[2]]),
  # save our data into a list object so that we have a df of our design matrix and a vector of our targets
  data_clean = fixer(task, "sale_price"), 
  # generate variable importance
  permutation_plots = imler_permutation(trained_learners, lrn_list, data_clean[[1]], data_clean[[2]]), 
  # generate ICE, PDP+ICE, and ALE plots 
  global_plots = imler_global_plots(trained_learners, lrn_list, data_clean[[1]], data_clean[[2]], feature = "gr_liv_area"),
  # generate LIME and Shapely values
  local_plots = imler_local(trained_learners, lrn_list, data_clean[[1]], data_clean[[2]], observation = 122) # modify observation of interest here
)

# configure our drake plan into a visualizable object (see runner.R vis_drake_graph() fx call)
config = drake_config(plan)


