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
	filter(learner.id != "classif.svm.tuned")




# - GLMNET -------------------------------------------------------------
glmnet = df %>%
 filter(learner.id == "classif.glmnet.tuned") %>%
 filter(search.type %in% c("random", "defaults", "mbo")) %>% 
 filter(n %in% c(1, 2, 4, 8, 10, 16, 32))

glmnet %>%  group_by(search.type, n) %>% tally(n())

p1 = ggplot(glmnet, aes(x = n, y = auc.test.mean, fill = search.type)) +
geom_boxplot() +
ylab("Area under the curve") +
xlab("Number of evaluations") +
labs(fill = "Seach strategy")



# - RPART ------------------------------------------------------------
rpart = df %>%
filter(learner.id == "classif.rpart.tuned") %>%
filter(search.type %in% c("random", "defaults", "mbo")) %>% 
filter(n %in% c(1, 2, 4, 8, 10, 16, 32, 64))

rpart %>% group_by(search.type, n) %>% tally(n())

ggplot(rpart, aes(x = n, y = auc.test.mean, fill = search.type)) +
geom_boxplot() +
ylab("Area under the curve") +
xlab("Number of evaluations") +
labs(fill = "Seach strategy")


# - XGBOOST - ----------------------------------------------------------
xgb = df %>%
filter(learner.id == "classif.xgboost.dummied.tuned") %>%
filter(search.type %in% c("random", "defaults", "mbo")) %>% 
filter(n %in% c(1, 2, 4, 8, 10, 16, 32, 64))

xgb %>% group_by(search.type, n) %>% tally(n())

ggplot(xgb, aes(x = n, y = auc.test.mean, fill = search.type)) +
geom_boxplot() +
theme_bw() +
ylab("Area under the curve") +
xlab("Number of evaluations") +
labs(fill = "Seach strategy")


# - All three ----------------------------------------------------------
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



# Critical Differences



create_cdplot = function(learner) {
	mask = df %>% group_by(task.id) %>% summarize(n = n()) %>% arrange(desc(n)) %>% filter(n == 42)
	aggr.meas = "auc.test.mean"

	dft = df[df$task.id %in% mask$task.id, ] %>%
	  mutate(search_n = paste0(search.type, "_", n)) %>%
	  filter(learner.short == learner) %>%
	  filter(n %in% c(2, 4, 8, 16, 32)) %>%
	  mutate(search_n = paste0(search.type, "_", n)) %>%
	  filter(search_n %in% c("random_8", "random_16", "random_32", "mbo_32", "defaults_8", "defaults_4"))

	frm = as.formula(stri_paste(aggr.meas, " ~  search_n| task.id", sep = ""))
	friedman.test(frm, data = dft)
	ntst = PMCMRplus::frdAllPairsNemenyiTest(dft$auc.test.mean, dft$search_n, dft$task.id)

	n.learners = length((unique(dft$search_n)))
	n.tasks = length(unique(dft$task.id))
	q.nemenyi = qtukey(1 - 0.05, n.learners, 1e+06) / sqrt(2L)
	cd.nemenyi = q.nemenyi * sqrt(n.learners * (n.learners + 1L) / (6L * n.tasks))
	q.bd = qtukey(1L - (0.05 / (n.learners - 1L)), 2L, 1e+06) / sqrt(2L)
	cd.bd = q.bd * sqrt(n.learners * (n.learners + 1L) / (6L * n.tasks))


	dd = dft %>%
	 group_by(task.id) %>% 
	 mutate(rnk = dense_rank(desc(auc.test.mean))) %>%
	 select(rnk, task.id, search_n, auc.test.mean) %>%
	 arrange(task.id) %>%
	 group_by(search_n) %>%
	 summarize(mean_rank = mean(rnk)) %>%
	 mutate(right = mean_rank > median(mean_rank)) %>%
	 mutate(yend = min_rank(mean_rank) - 0.75 + 0.1*row_number()) %>%
	 mutate(yend = ifelse(yend < median(yend), yend, max(yend) - yend + 1)) %>%
	 mutate(xend = ifelse(!right, 0L, max(mean_rank) + 1L)) %>%
	 mutate(right = as.numeric(right))

	sub = sort(dd$mean_rank)
    # Compute a matrix of all possible bars
    mat = apply(t(outer(sub, sub, `-`)), c(1, 2),
      FUN = function(x) ifelse(x > 0 && x < cd.nemenyi, x, 0))
    # Get start and end point of all possible bars
    xstart = round(apply(mat + sub, 1, min), 3)
    xend   = round(apply(mat + sub, 1, max), 3)
    nem.df = data.table(xstart, xend, "diff" = xend - xstart)
    nem.df = nem.df[, .SD[which.max(.SD$diff)], by = "xend"]
    nem.df = nem.df[nem.df$xend - nem.df$xstart > 0, ]
    nem.df$y = seq(from = 0.1, to = 0.35, length.out = dim(nem.df)[1])

	p = ggplot(dd)
	p = p + geom_point(aes_string("mean_rank", 0, colour = "search_n"), size = 3)
	p = p + geom_segment(aes_string("mean_rank", 0, xend = "mean_rank", yend = "yend",
	                              color = "search_n"), size = 1)
	p = p + geom_segment(aes_string("mean_rank", "yend", xend = "xend",
	                          	  yend = "yend", color = "search_n"), size = 1)
	p = p + geom_text(aes_string("xend", "yend", label = "search_n",
	                             hjust = "right"), vjust = -1)
	p = p + xlab("Average Rank")
	p = p + geom_segment(aes_string("xstart", "y", xend = "xend", yend = "y"),
	                   data = nem.df, size = 2, color = "dimgrey", alpha = 0.9)
	p = p + annotate("text",
	               label = stri_paste("Critical Difference =", round(cd.nemenyi, 2), sep = " "),
	               y = max(dd$yend) + .1, x = mean(dd$mean_rank))
	p = p + annotate("segment",
	               x =  mean(dd$mean_rank) - 0.5 * cd.nemenyi,
	               xend = mean(dd$mean_rank) + 0.5 * cd.nemenyi,
	               y = max(dd$yend) + .25,
	               yend = max(dd$yend) + .25,
	               size = 2L)
	p = p + theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                legend.position = "none",
                panel.background = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(size = 1),
                axis.line.y = element_blank(),
                panel.grid.major = element_blank(),
                plot.background = element_blank())
	p = p + ggtitle(learner)

	p
}
library(patchwork)
pcd = (create_cdplot("Decision Tree") + xlab("")) /
(create_cdplot("ElasticNet") + xlab("")) /
(create_cdplot("Xgboost") + xlab("Average Rank"))
ggsave("defaultLOOCV/cdplots.pdf", plot = pcd, height = 8, width = 4, scale = 1.35)
