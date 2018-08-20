library(devtools)     # load_all()
library(stringi)      # string manipulation
library(focussearch)  # Search the surrogates
library(doParallel)   # Parallelization
library(doMC)         # Parallelization
library(doRNG)        # Parallel RNG
library(foreach)      # Parallelization
library(patchwork)    # Vizualisation
library(ggpubr)       # Vizualisation
library(drake)        # Reports
load_all()

# Create Reports -----------------------------------------------------------------------------------
lrn.par.sets = getLearnerParSets()
learner.names = stri_sub(stri_paste("mlr.", names(lrn.par.sets)), 1, -5)
update_all_results()

# --------------------------------------------------------------------------------------------------
plan = drake_plan(
  data_in = file_in("defaultLOOCV/full_results.Rds"),
  # glmnet
  learner.name_1 = paste0(learner.names[1]),
  results_1 = get_results_from_folder(1, loadd(data_in)),
  data_1 = preprocess_results(results_1 %>% filter(search.type %in% c("design", "package-default", "random"))),
  plots_1 = make_plots(results_1, data_1, 1),
  report_1 = create_report(learner.name_1, data_1, j =  1),
  # rpart
  learner.name_2 = paste0(learner.names[2]),
  results_2 = get_results_from_folder(2, data_in),
  data_2 = preprocess_results(results_2 %>% filter(search.type %in% c("design", "package-default", "random"))),
  plots_2 = make_plots(results_2, data_2, 2),
  report_2 = create_report(learner.name_2, data_2, 2),
  # svm
  learner.name_4 = paste0(learner.names[4]),
  results_4 = get_results_from_folder(4, data_in),
  data_4 = preprocess_results(results_4 %>% filter(search.type %in% c("design", "package-default", "random"))),
  plots_4 = make_plots(results_4, data_4, 4),
  report_4 = create_report(learner.name_4, data_4, 4),
  # xgboost
  learner.name_6 = paste0(learner.names[6]),
  results_6 = get_results_from_folder(6, data_in),
  data_6 = preprocess_results(results_6 %>% filter(search.type %in% c("design", "package-default", "random"))),
  plots_6 = make_plots(results_6, data_6, 6),
  report_6 = create_report(learner.name_6, data_6, 6),
  
  strings_in_dots = "literals"
)

make(plan)


# --------------------------------------------------------------------------------------------------
update_all_results()
df = readRDS("defaultLOOCV/full_results.Rds")

list.files("defaultLOOCV/save/", full.names = TRUE) %>%
  as.data.frame(x = .) %>%
  rename(file = ".") %>%
  filter(stri_detect_fixed(file, "svm"), stri_detect_fixed(file, "random_")) %>%
  pull(file) %>% as.character() %>%
  lapply(., readRDS) %>%
  do.call("bind_rows", .) %>%
  group_by(learner.id, n) %>%
  tally()
  



df$oob.perf %>%
  filter(search.type != "randomBotData") %>%
  filter(learner.id == "classif.xgboost.dummied.tuned") %>%
  filter(!(task.id %in% c("nomao", "Bioresponse"))) %>%
  group_by(learner.id, task.id) %>%
  mutate(rnk = dense_rank(auc.test.mean)) %>%
  group_by(learner.id, search.type, n) %>%
  summarize(
    rnk = mean(rnk),
    mean_auc = mean(auc.test.mean),
    median_auc = median(auc.test.mean),
    cnt = n()) %>%
  arrange(desc(cnt, n)) %>%
  print.data.frame(.)
  

rpartdf = df$oob.perf %>%
  filter(search.type != "randomBotData") %>%
  filter(learner.id == "classif.rpart.tuned") %>%
  filter(!(task.id %in% c("nomao", "Bioresponse"))) %>%
  filter(n %in% c(1, 2, 4, 6, 8, 10)) %>%
  group_by(learner.id, task.id, n) %>%
  mutate(rnk = dense_rank(desc(auc.test.mean))) %>%
  group_by(learner.id, search.type, n) %>%
  summarize(
    rnk = mean(rnk),
    mean_auc = mean(auc.test.mean),
    median_auc = median(auc.test.mean),
    cnt = n()) %>%
  arrange(cnt) %>%
  filter(search.type != "mbo" & search.type != "package-default" & search.type != "random" )

rpartdf$search.type[rpartdf$search.type == "design"] = "median"
rpartdf$search.type[rpartdf$search.type == "defaults_cycle"] = "Q0.3 Q0.7. Q0.5"


p1 = ggplot(rpartdf, aes(x = n, y = mean_auc, color = search.type)) +
  geom_point() + geom_line()
p2 = ggplot(rpartdf, aes(x = n, y = median_auc, color = search.type)) +
    geom_point() + geom_line()
ggsave("defaultLOOCV/auc_search.type_comparison.pdf", plot = p1/p2, scale = 2, height = 4, width = 4)
ggsave("../paper_2018_multiple_defaults/figures/auc_search_type_comparison.pdf",
  plot = p1/p2, scale = 1.5, height = 4, width = 5)



df$oob.perf %>%
  filter(search.type != "randomBotData") %>%
  filter(learner.id == "classif.xgboost.dummied.tuned") %>%
  filter(!(task.id %in% c("nomao", "Bioresponse"))) %>%
  filter(!(search.type == "random" & n <= 8)) %>%
  filter(!(search.type == "design" & n <= 4)) %>%
  group_by(learner.id, task.id) %>%
  mutate(rnk = dense_rank(auc.test.mean)) %>%
  group_by(learner.id, search.type, n) %>%
  summarize(
    mean_rank_auc = mean(rnk),
    mean_auc = mean(auc.test.mean),
    median_auc = median(auc.test.mean),
    cnt = n()) %>%
  arrange(desc(mean_auc)) %>%
  rename(
    mean.rank.auc = mean_rank_auc,
    mean.auc = mean_auc,
    median.auc = median_auc
  )  %>%
  ungroup() %>% 
  mutate(search.type = ifelse(search.type ==  "design", "mult.defaults", search.type)) %>%
  mutate(search.type = ifelse(search.type ==  "package-default", "package", search.type)) -> xgbdf


df$oob.perf %>%
  filter(search.type != "randomBotData") %>%
  filter(learner.id == "classif.glmnet.tuned") %>%
  filter(!(task.id %in% c("nomao", "Bioresponse"))) %>%
  filter(!(search.type == "random" & n <= 8)) %>%
  filter(!(search.type == "design" & n <= 4)) %>%
  group_by(learner.id, task.id) %>%
  mutate(rnk = dense_rank(desc(auc.test.mean))) %>%
  group_by(learner.id, search.type, n) %>%
  summarize(
    mean_rank_auc = mean(rnk),
    mean_auc = mean(auc.test.mean),
    median_auc = median(auc.test.mean),
    cnt = n()) %>%
  arrange(desc(mean_auc)) %>%
  rename(
    mean.rank.auc = mean_rank_auc,
    mean.auc = mean_auc,
    median.auc = median_auc
  )  %>%
  ungroup() %>% 
  mutate(search.type = ifelse(search.type ==  "design", "mult.defaults", search.type)) %>%
  mutate(search.type = ifelse(search.type ==  "package-default", "package", search.type)) -> glmnetdf

library(knitr)
library(kableExtra)

rpartdf = df$oob.perf %>%
  filter(search.type != "defaults_mean") %>% 
  filter(search.type != "defaults_cycle") %>%
  filter(search.type != "design") %>%
  filter(search.type != "randomBotData") %>%
  filter(learner.id == "classif.rpart.tuned") %>%
  filter(!(task.id %in% c("nomao", "Bioresponse"))) %>%
  filter(!(search.type == "random" & n <= 8)) %>%
  filter(!(search.type == "hodges-lehmann" & n <= 4)) %>%
  group_by(learner.id, task.id) %>%
  mutate(rnk = dense_rank(desc(auc.test.mean))) %>%
  group_by(learner.id, search.type, n) %>%
  summarize(
    mean_rank_auc = mean(rnk),
    mean_auc = mean(auc.test.mean),
    median_auc = median(auc.test.mean),
    cnt = n()) %>%
  arrange(desc(mean_auc)) %>%
  ungroup() %>%
  rename(
    mean.rank.auc = mean_rank_auc,
    mean.auc = mean_auc,
    median.auc = median_auc
  )  %>%
  mutate(search.type = ifelse(search.type ==  "hodges-lehmann", "mult.defaults", search.type)) %>%
  mutate(search.type = ifelse(search.type ==  "package-default", "package", search.type))

make_table = function(df) {
  df %>% select(-cnt, -learner.id) %>%
  unite(search.type_n, search.type, n, sep = ".") %>%
  gather(measure, value, mean.rank.auc:median.auc, -search.type_n) %>%
  spread(key = search.type_n, value) %>%
  rename(package = package.1) -> rpart.table
    
  if(ncol(rpart.table) == 9)
    rpart.table = rpart.table[c(2, 1, 3), c(1,6, 4, 5, 3,7, 8, 9, 2)]
  if(ncol(rpart.table) == 8)
    rpart.table = rpart.table[c(2, 1, 3), c(1,6, 4, 5, 3,7, 8, 2)]
  # Best values
  opt = c(apply(rpart.table[1, -1], 1, min), apply(rpart.table[2:3, -1], 1, max))

  make_bold = function(x) {
    if (is.numeric(x)) {
    x = round(x, 4)
    x = cell_spec(x, "latex", bold = ifelse(x == round(opt, 4), TRUE, FALSE))
    }
    return(x)
  }

  rpart.table %>%
    mutate_all(make_bold) %>%
    knitr::kable(format = "latex", digits = 3, escape = F)
}
  
make_table(rpartdf)
make_table(xgbdf)
make_table(glmnetdf)

