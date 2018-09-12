packrat::off()
library(devtools)
load_all()

df = readRDS("defaultLOOCV/full_results.Rds")$oob.perf %>%
	mutate(n = as.factor(n)) %>%
	mutate(search.type = recode(search.type, "design" = "defaults")) %>%
	mutate(learner.short = recode(learner.id,
	  "classif.glmnet.tuned" = "ElasticNet",
	  "classif.rpart.tuned" = "Decision Tree",
	  "classif.xgboost.dummied.tuned" = "Xgboost")) %>%
	filter(!(task.id %in% c("nomao", "Bioresponse"))) %>%
	filter(search.type %in% c("random", "defaults", "mbo")) %>%
	filter(n %in% c(1, 2, 4, 8, 10, 16, 32, 64)) %>%
	filter(learner.id != "classif.svm.tuned") %>%
	group_by(task.id, learner.id) %>%
 	mutate(auc.test.mean = (auc.test.mean - min(auc.test.mean, na.rm = TRUE)) /
  		(max(auc.test.mean, na.rm = TRUE) - min(auc.test.mean, na.rm = TRUE)))

df %>%
 group_by(learner.short, search.type, n) %>%
 tally(n()) %>%
 filter(nn < 36) %>%
 print(n = 44)

df %>%
 filter(learner.short == "ElasticNet") %>%
 filter(search.type == "defaults") %>%
 filter(n == 16)

f = list.files("defaultLOOCV/save", full.names = TRUE)
fs = f[stri_detect_regex(f, "design_16.*glmnet")]
lapply(fs, readRDS)

# - GLMNET -------------------------------------------------------------
glmnet = df %>%
	filter(learner.id == "classif.glmnet.tuned") %>%
	filter(search.type %in% c("random", "defaults", "mbo")) %>%
	filter(n %in% c(1, 2, 4, 8, 16, 32)) %>%
	mutate(learner.short = factor(learner.short, levels = unique(learner.short)[c(1, 2, 3)]))

p1 = ggplot(glmnet, aes(x = n, y = auc.test.mean, fill = search.type)) +
	geom_boxplot(width = 0.9, varwidth = TRUE) +
	theme_bw() +
	ylab("Normalized AUC") +
	xlab("Number of evaluations") +
	labs(fill = "Seach strategy") +
	facet_wrap(~learner.short) +
	theme(legend.position = "top")

ggsave("defaultLOOCV/boxplots_auc_glmnet.pdf", plot = p1, height = 3, width = 4, scale = 1)


# - RPART ------------------------------------------------------------
rpart = df %>%
	filter(learner.id == "classif.rpart.tuned") %>%
	filter(search.type %in% c("random", "defaults", "mbo")) %>%
	filter(n %in% c(1, 2, 4, 8, 16, 32, 64)) %>%
	mutate(learner.short = factor(learner.short, levels = unique(learner.short)[c(1, 2, 3)]))

p2 = ggplot(rpart, aes(x = n, y = auc.test.mean, fill = search.type)) +
	geom_boxplot(width = 0.9, varwidth = TRUE) +
	theme_bw() +
	ylab("Normalized AUC") +
	xlab("Number of evaluations") +
	facet_wrap(~learner.short) +
	theme(legend.position = "none")

ggsave("defaultLOOCV/boxplots_auc_rpart.pdf", plot = p2, height = 3, width = 4, scale = 1)


# - XGBOOST - ----------------------------------------------------------
xgb = df %>%
	filter(learner.id == "classif.xgboost.dummied.tuned") %>%
	filter(search.type %in% c("random", "defaults", "mbo")) %>%
	filter(n %in% c(1, 2, 4, 8, 16, 32, 64)) %>%
	mutate(learner.short = factor(learner.short, levels = unique(learner.short)[c(1, 2, 3)]))

p3 = ggplot(xgb, aes(x = n, y = auc.test.mean, fill = search.type)) +
	geom_boxplot(width = 0.9, varwidth = TRUE) +
	theme_bw() +
	ylab("Normalized AUC") +
	xlab("Number of evaluations") +
	labs(fill = "Seach strategy") +
	facet_wrap(~learner.short) +
	theme(legend.position = "none")

ggsave("defaultLOOCV/boxplots_auc_xgboost.pdf", plot = p3, height = 3, width = 4, scale = 1)


# All three ----------------------------------------------------------
df2 = df %>%
filter(search.type %in% c("random", "defaults", "mbo")) %>%
filter(n %in% c(1, 2, 4, 8, 10, 16, 32, 64)) %>%
filter(learner.id != "classif.svm.tuned") %>%
mutate(learner.short = factor(learner.short, levels = unique(learner.short)[c(1, 2, 3)]))

pfull = ggplot(df2, aes(x = n, y = auc.test.mean, fill = search.type)) +
geom_boxplot() +
theme_bw() +
facet_wrap(~learner.short, nrow = 3) +
ylab("Area under the curve") +
xlab("Number of evaluations") +
labs(fill = "Seach strategy") +
theme(legend.position = "bottom")

ggsave("defaultLOOCV/boxplots_auc_full.pdf", plot = pfull, height = 8, width = 4, scale = 1)

# Critical Differences ----------------------------------------------------------
library(patchwork)
pcd = (create_cdplot(df, "Decision Tree") + xlab("")) /
	(create_cdplot(df, "ElasticNet") + xlab("")) /
	(create_cdplot(df, "Xgboost") + xlab("Average Rank"))
ggsave("defaultLOOCV/cdplots.pdf", plot = pcd, height = 4, width = 4, scale = 1.35)


pcd1 = (create_cdplot(df, "Decision Tree") + xlab(""))
pcd2 = 	(create_cdplot(df, "ElasticNet") + xlab(""))
pcd3 = 	(create_cdplot(df, "Xgboost") + xlab("Average Rank"))
ggsave("defaultLOOCV/cdplots_rpart.pdf", plot = pcd1, height = 4/3, width = 4, scale = 1.35)
ggsave("defaultLOOCV/cdplots_glmnet.pdf", plot = pcd2, height = 4/3, width = 4, scale = 1.35)
ggsave("defaultLOOCV/cdplots_xgboost.pdf", plot = pcd3, height = 4/3, width = 4, scale = 1.35)



# mean and std
df %>%
 group_by(learner.id, n) %>%
 filter(search.type == "defaults") %>%
 summarize(avg_auc = mean(auc.test.mean), std_auc = sd(auc.test.mean)) %>%
 data.frame() %>%
writeARFF(., "defaultLOOCV/avg_std_mlr.arff", overwrite = TRUE)





# -----------------------------------------------------------------------------------
# SKLEARN_DATA
# -----------------------------------------------------------------------------------

dfsklearn = lapply(list.files("results_sklearn", full.names = TRUE), read.csv) %>%
 setNames(., stri_sub(list.files("results_sklearn"), to = -5)) %>%
 bind_rows(., .id = "learner.id") %>%
 rename(task.id = task_id) %>%
 rename(acc.test.mean = evaluation) %>%
 separate(strategy_name, c("search.type", "n"), "__") %>%
 mutate(n = as.factor(n)) %>%
 mutate(learner.id = as.factor(learner.id)) %>%
 mutate(search_n = as.factor(paste0(search.type, "_", n))) %>%
 select(-X) %>%
 mutate(search.type = ifelse(search.type == "greedy", "defaults", search.type)) %>%
 mutate(search.type = ifelse(search.type == "random_search", "random", search.type)) %>%
 mutate(learner.short = learner.id)


# - random Forest -------------------------------------------------------------
rf = dfsklearn %>%
    mutate(n = factor(n, levels = c(1, 2, 4, 8, 16, 32))) %>%
    filter(n != 64) %>%
    filter(learner.id == "random_forest") %>%
    group_by(task.id, learner.id) %>%
 	mutate(acc.test.mean = (acc.test.mean - min(acc.test.mean, na.rm = TRUE)) /
  		(max(acc.test.mean, na.rm = TRUE) - min(acc.test.mean, na.rm = TRUE)))

p1.2 = ggplot(rf, aes(x = n, y = acc.test.mean, fill = search.type)) +
	geom_boxplot(width = 0.6) +
	theme_bw() +
	ylab("Normalized Accuracy") +
	xlab("Number of evaluations") +
	labs(fill = "Seach strategy") +
	facet_wrap(~learner.short) +
	theme(legend.position = "none") +
	scale_fill_manual(values = hue_pal()(3)[c(1,2,3)])

ggsave("defaultLOOCV/boxplots_acc_rf.pdf", plot = p1.2, height = 3, width = 4, scale = 1)


# - RPART ------------------------------------------------------------
svm = dfsklearn %>%
    mutate(n = factor(n, levels = c(1, 2, 4, 8, 16, 32))) %>%
    filter(n != 64) %>%
    filter(learner.id == "libsvm_svc") %>%
    group_by(task.id, learner.id) %>%
 	mutate(acc.test.mean = (acc.test.mean - min(acc.test.mean, na.rm = TRUE)) /
  		(max(acc.test.mean, na.rm = TRUE) - min(acc.test.mean, na.rm = TRUE)))

p2.2 = ggplot(svm, aes(x = n, y = acc.test.mean, fill = search.type)) +
	geom_boxplot() +
	theme_bw() +
	ylab("Normalized Accuracy") +
	xlab("Number of evaluations") +
	facet_wrap(~learner.short) +
	theme(legend.position = "none") +
	scale_fill_manual(values = hue_pal()(3)[c(1,3)])

ggsave("defaultLOOCV/boxplots_acc_svm_svc.pdf", plot = p2.2 , height = 3, width = 4, scale = 1)



# - XGBOOST - ----------------------------------------------------------
ada = dfsklearn %>%
    mutate(n = factor(n, levels = c(1, 2, 4, 8, 16, 32))) %>%
    filter(n != 64) %>%
    filter(learner.id == "adaboost") %>%
    group_by(task.id, learner.id) %>%
 	mutate(acc.test.mean = (acc.test.mean - min(acc.test.mean, na.rm = TRUE)) /
  		(max(acc.test.mean, na.rm = TRUE) - min(acc.test.mean, na.rm = TRUE)))

p3.2 = ggplot(ada, aes(x = n, y = acc.test.mean, fill = search.type)) +
	geom_boxplot() +
	theme_bw() +
	ylab("Normalized Accuracy") +
	xlab("Number of evaluations") +
	labs(fill = "Seach strategy") +
	facet_wrap(~learner.short) +
	theme(legend.position = "none") +
	scale_fill_manual(values = hue_pal()(3)[c(1,2,3)])

ggsave("defaultLOOCV/boxplots_acc_adaboost.pdf", plot = p3.2, height = 3, width = 4, scale = 1)


# Boxplots all
psklearnfull = dfsklearn %>%
    mutate(n = factor(n, levels = c(1, 2, 4, 8, 16, 32))) %>%
    mutate(search.type = factor(search.type,  levels = c("random", "defaults"))) %>%
    filter(n != 64) %>%
	ggplot(aes(x = n, y = acc.test.mean, fill = search.type)) +
	geom_boxplot(outlier.alpha = 0.5) +
	theme_bw() +
	facet_wrap(~learner.short, nrow = 3, scales = "free_y") +
	ylab("Accuracy") +
	xlab("Number of evaluations") +
	labs(fill = "Seach strategy") +
	theme(legend.position = "bottom")
ggsave("defaultLOOCV/boxplots_sklearn_acc.pdf", plot = psklearnfull, height = 6, width = 4, scale = 1)


## CD Plots
pcd2 = (create_cdplot(dfsklearn, "adaboost", "acc.test.mean") + xlab("")) /
	   (create_cdplot(dfsklearn, "random_forest", "acc.test.mean") + xlab("")) /
	   (create_cdplot(dfsklearn, "libsvm_svc", "acc.test.mean") + xlab("Average Rank"))

ggsave("defaultLOOCV/cdplots_sklearn.pdf", plot = pcd2, height = 4, width = 4, scale = 1.35)

pcd1 =  (create_cdplot(dfsklearn, "adaboost", "acc.test.mean") + xlab(""))
pcd2 = 	(create_cdplot(dfsklearn, "random_forest", "acc.test.mean") + xlab(""))
pcd3 = 	(create_cdplot(dfsklearn, "libsvm_svc", "acc.test.mean") + xlab("Average Rank"))
ggsave("defaultLOOCV/cdplots_adaboost.pdf", plot = pcd1, height = 4/3, width = 3.8, scale = 1.3)
ggsave("defaultLOOCV/cdplots_rf.pdf", plot = pcd2, height = 4/3, width = 3.8, scale = 1.3)
ggsave("defaultLOOCV/cdplots_svm.pdf", plot = pcd3, height = 4/3, width = 3.8, scale = 1.3)


# mean and std
dfsklearn %>%
 group_by(learner.id, n) %>%
 filter(search.type == "defaults") %>%
 summarize(avg_acc = mean(acc.test.mean), std_acc = sd(acc.test.mean)) %>%
 data.frame() %>%
writeARFF(., "defaultLOOCV/avg_std_sklearn.arff", overwrite = TRUE)




#---------------------------------------------------------------------------
# Output for Jan:
readRDS("defaultLOOCV/full_results.Rds")$oob.perf %>%
	mutate(n = as.factor(n)) %>%
	mutate(search.type = recode(search.type, "design" = "defaults")) %>%
	mutate(learner.short = recode(learner.id,
	  "classif.glmnet.tuned" = "ElasticNet",
	  "classif.rpart.tuned" = "Decision Tree",
	  "classif.xgboost.dummied.tuned" = "Xgboost")) %>%
	filter(!(task.id %in% c("nomao", "Bioresponse"))) %>%
	filter(search.type %in% c("random", "defaults", "mbo")) %>%
	filter(learner.id != "classif.svm.tuned") %>%
	rename(task_id = task.id) %>%
	rename(learner_id = learner.id) %>%
	select(-data_id) %>%
	mutate(strategy_name = paste0(search.type, "__", "n")) %>%
	write.csv("results_mlr.csv")


