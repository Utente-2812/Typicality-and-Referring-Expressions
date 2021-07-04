
# Packages ----------------------------------------------------------------

# install.packages('yardstick')
# install.packages('irr')
# install.packages("jtools")
# install.packages("stat_cor")
# install.packages("sjPlot")
# install.packages("devtools")
# install.packages("stargazer")
# install.packages('ggstatsplot')
# install.packages('ggfortify')
# install.packages('InformationValue')
# install.packages('ISLR')
# install.packages('caret')
# install.packages('brant')

# Libraries ---------------------------------------------------------------
library(brant)
library(ggplot2)
library(GGally)
library(caret)
library(InformationValue)
library(ISLR)
library(ggfortify)
library(ggpubr)
library(psych)
library(ggplot2)
library(yardstick)
library(jtools)
library(irr)
library(ggiraphExtra)
library(stat_cor)
library(dplyr)
library(plyr)
library(lme4)
library(lmerTest)
library(sjPlot)
library(languageR)
library(Hmisc)
library(car)
library(readr)
library(reshape)
library(vcd)
library(brglm)
library(psy)
library(tidyverse) 
library(broom)
library(ggstatsplot)
library(ggrepel)
library(AER)
library(stargazer)
library(sandwich)
library(caret)
library(devtools)
devtools::install_github('topepo/caret/pkg/caret')

# Upload the data ---------------------------------------------------------
dataFrameFull <- read_csv("dataFrameFull2.csv")
dataFrameFull$X1 <- NULL
# Rename columns ---------------------------------------------------------
dataFrameFull$category[dataFrameFull$category=='1'] <- 'role'
dataFrameFull$category[dataFrameFull$category=='0'] <- 'taxonomic'
dataFrameFull$original_label_topname[dataFrameFull$original_label_topname=='R'] <- 'topname MN: role'
dataFrameFull$original_label_topname[dataFrameFull$original_label_topname=='T'] <- 'topname MN: taxonomic'

# Create boolean values for R and T experiment ---------------------------------------------------------
dataFrameFull$bool_category[dataFrameFull$category=='role'] <- 1
dataFrameFull$bool_category[dataFrameFull$category=='taxonomic'] <- 0
dataFrameFull$bool_category_topname[dataFrameFull$original_label_topname=='topname MN: role'] <- 1
dataFrameFull$bool_category_topname[dataFrameFull$original_label_topname=='topname MN: taxonomic'] <- 0

# Adding a column for the mean and sd ---------------------------------------------------------
dataFrameFull$mean_judgement <- rowMeans(dataFrameFull[,0:5], na.rm = TRUE)
dataFrameFull$sd <- sd(dataFrameFull$mean_judgement)

# Fileter and create new df ---------------------------------------------------------
# with category experiment == category MN 
same_topname <- dataFrameFull[dataFrameFull$same_topname == TRUE, ]
# for only R and T
only_role <- dataFrameFull[dataFrameFull$category == "role", ]
only_taxa <- dataFrameFull[dataFrameFull$category == "taxonomic", ]

reshaped <- merge(only_role, only_taxa, by=c("image", "perc_role",'perc_taxa',
                                             'weights', 'topname',
                                             'domain', 'vg_object_id',
                                              'original_label_topname',
                                             'perc_top_v2', 'bool_category_topname'))

same_topname_2 <- merge(reshaped, same_topname,  by=c("image", "perc_role",'perc_taxa',
                                                      'weights', 'topname',
                                                      'domain', 'vg_object_id',
                                                      'original_label_topname',
                                                      'perc_top_v2', 'bool_category_topname'))

# DENSITY PLOT for normality ---------------------------------------------------------
plot(density(only_role$mean_judgement), 
     main = "Density plot of mean judgments", xlab = "role names judgments")
plot(density(only_taxa$mean_judgement), 
     main = "Density plot of mean judgments", xlab = "taxonomic names judgments")
plot(density(dataFrameFull$mean_judgement), 
     main = "Density plot of mean judgments", xlab = "Mean judgments") # all data 

# Normal distribution test ---------------------------------------------------------
shapiro.test(only_role$mean_judgement) #  the distribution is non-normal
shapiro.test(only_taxa$mean_judgement) #  the distribution is normal

# Descriptive stats ---------------------------------------------------------

# Plot mean ratings by group 
p_1 <- ggplot(dataFrameFull, x = "category", y = "mean_judgement", 
          color = "category", palette = c("#00AFBB", "#E7B800"),
          ylab = "Typicality Ratings", xlab = "Category") +
          geom_jitter(aes(colour = category), size = 1) 
  
p_1 

p_2 <- ggplot(dataFrameFull, aes(x = category, y = mean_judgement,
                                 fill = category)) + 
              geom_boxplot(alpha=0.3) +
              geom_jitter(aes(colour = category), size = 1) + 
              facet_grid(~original_label_topname) +
              theme_bw() + 
              xlab("Category") + ylab("Typicality Ratings") 
p_2

# Raincloud plots ----
raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  axis.text = element_text(size = 10),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 10),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

source('R_rainclouds_template.R')

pdf("typicality_mean.pdf", width = 10)
ggplot(dataFrameFull, aes(x = name_image, y = mean_judgement, fill = category))+
  geom_flat_violin(alpha=0.3, position = position_nudge(x = .2, y = 0),adjust = 2)+
  geom_point(aes(x = name_image, y = mean_judgement), position = position_jitter(width = .15), size = .25) + 
  labs(y = "Typicality Ratings", x="Labels extracted from ManyNames") +
  theme_bw() +
  raincloud_theme
dev.off()
p_3

# Find outliers  ---------------------------------------------------------
out_t <- boxplot.stats(only_taxa$mean_judgement)$out
out_ind_t <- which(only_taxa$mean_judgement %in% c(out_t))
out_ind_t
out_r <- boxplot.stats(only_role$mean_judgement)$out
out_ind_r <- which(only_role$mean_judgement %in% c(out_r))
out_ind_r
taxa_outliers <- only_taxa[out_ind_t, ]
role_outliers <- only_role[out_ind_r, ]

# CORRELATIONS ---------------------------------------------------------
# because it is not normally distributed
kruskal.test(same_topname_2$mean_judgement.role ~
               same_topname_2$mean_judgement.taxonomic, 
             data = same_topname_2)

# Spearman correlation
# dataFrameFull$name_image <- unclass(dataFrameFull$name_image)
cor.test(dataFrameFull$mean_judgement , dataFrameFull$name_image,
         method = 'spearman')

cor_taxa <- cor.test(only_taxa$mean_judgement , only_taxa$perc_taxa,
         method = 'spearman', exact=FALSE) 

p_cor_taxa <- ggplot(only_taxa, aes(fill= perc_taxa)) +
              geom_point(aes(mean_judgement, name_image, color= perc_taxa)) +
              labs(x = "Typicality Ratings", 
                   y="Taxonomic nouns") +
                      theme_bw() + theme(
                        text = element_text(size = 12),
                        axis.text = element_text(size = 12))
p_cor_taxa 

cor_role <- cor.test(only_role$mean_judgement , only_role$perc_role,
         method = 'spearman', exact=FALSE) 
p_cor_role <- ggplot(only_role) +
                    geom_point(aes(mean_judgement, name_image, color=perc_role
                                   )) +
                    labs(x = "Typicality Ratings", 
                         y="Role nouns", fill = "Name agreement \n on Role") +
                    theme_bw() + theme(
                      text = element_text(size = 12),
                      axis.text = element_text(size = 12))

p_cor_role

x = dataFrameFull[dataFrameFull$mean_judgement, dataFrameFull$perc_role, dataFrameFull$perc_taxa]
cor(select(dataFrameFull, mean_judgement, perc_role, perc_taxa))


# Inter-annotator agreement  ---------------------------------------------------------
# The level of agreement between the 5 non-unique raters
icc(
  dataFrameFull[, 1:6], model = "twoway", 
  type = "consistency", unit = "single"
)

# REGRESSIONS ---------------------------------------------------------

# Cross-categories plots
p_4_r <- ggplot(data = only_role, aes(x = mean_judgement, y = perc_role)) +
                  geom_point(shape = 18, color='darkblue') +
                  geom_smooth(method = lm, col = "darkblue") +
                  theme_bw() +
                  xlab("Typicality Ratings for Role names") +
                  ylab("Name agreement on Role names") +
                  theme(text = element_text(size = 16),
                        axis.title.x = element_text(size = 16),
                        axis.title.y = element_text(size = 14))
p_4_r

p_4_t <- ggplot(data = only_taxa, aes(x = mean_judgement, y = perc_taxa)) +
                geom_point(shape = 18, color='#FF00CC') +
                geom_smooth(method = lm, col = "#FF00CC") +
                theme_bw() +
                xlab("Typicality Ratings for Taxonomic names") +
                ylab("Name agreement on Taxonomic names") +
                theme(text = element_text(size = 16),
                      axis.title.x = element_text(size = 16),
                      axis.title.y = element_text(size = 14))
p_4_t

p_5 <- ggplot(data = dataFrameFull, aes(x = perc_top_v2, y = mean_judgement)) +
                facet_grid(category ~ original_label_topname) +
                geom_point() +
                geom_smooth(method='lm', formula= y~x) +
                stat_cor(label.y = 8, label.x.npc = "left") +
                labs(y = "mean typicality judgement", x = "name agreement") +
                theme_bw()
p_5

# Plots for rules ---------------------------------------------------------

# create 3 different dataframe according to the rules
tie <- dataFrameFull[dataFrameFull$class_scores == "tie", ]
more_role <- dataFrameFull[dataFrameFull$class_scores == "more role", ]
more_taxa <- dataFrameFull[dataFrameFull$class_scores == "more taxa", ]

# TIE 
plot_tie <- ggplot(data = tie, aes(x = perc_top_v2, y = mean_judgement)) +
  facet_grid(category ~ original_label_topname) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  stat_cor(label.y = 8, label.x.npc = "left") +
  labs(y = "Rating", x = "name agreement") +
  theme_bw()

plot_tie

# MORE ROLE
plot_more_role <- ggplot(data = more_role, aes(x = perc_role, y = mean_judgement)) +
  facet_grid(category ~ original_label_topname) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  stat_cor(label.y = 8, label.x.npc = "left") +
  labs(y = "Rating", x = "name agreement") +
  theme_bw()

plot_more_role

# MORE TAXA
plot_more_taxa <- ggplot(data = more_taxa, aes(x = perc_taxa, y = mean_judgement)) +
  facet_grid(category ~ original_label_topname) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x) +
  stat_cor(label.y = 8, label.x.npc = "left") +
  labs(y = "Rating", x = "name agreement") +
  theme_bw() 

plot_more_taxa

# Models ---------------------------------------------------------

# MODEL 1 = LINEAR REGRESSIONS---------------------------------------------------------
# predict the category based on mean
lm_class <- lm(bool_category ~ mean_judgement,
               weights = weights,
               data=dataFrameFull)
plot(lm_class) # diagnostic plot
summary(lm_class)
# plot the data
p_6 <- plot(x = dataFrameFull$mean_judgement, 
             y = dataFrameFull$bool_category,
             xlab = "Typicality Ratings",
             ylab = "Taxonomic 0 - Role 1",
             pch = 20,
             ylim = c(-0.4, 1.4),
             cex.main = 0.8)
p_6 + abline(h = 1, lty = 2, col = "darkred")
p_6 + abline(h = 0, lty = 2, col = "darkred")
p_6 +  abline(lm_class, 
               lwd = 1.8, 
               col = "steelblue")
stargazer(lm_class) # create a table wih results for Latex
ncvTest(lm_class)

# MODEL 2 = GENERALIZED LINEAR MODEL---------------------------------------------------------
# predict the increase on name agreement based on category in our exp.+
# interaction between typ. rating and category + rules
glm_2 <- glm(perc_role ~ category + log(mean_judgement):category + 
                class_scores,
                family=binomial(),
                weights = weights,
                data = dataFrameFull)
coeftest(glm_2, vcov. = vcovHC, type = "HC1") # add robustness
summary(glm_2) 
stargazer(glm_2) # create a table wih results for Latex

# predict the increase on name agreement based on category in our exp.+
# interaction between typ. rating and category + rules

glm_2_contrast <- glm(perc_taxa ~ category + log(mean_judgement):category + 
                             class_scores,
                           family=binomial(),
                           weights = weights,
                           data = dataFrameFull)
coeftest(glm_2_contrast, vcov. = vcovHC, type = "HC1") # add robustness
summary(glm_2_contrast) 
stargazer(glm_2_contrast) # create a table wih results for Latex

# MODEL 3 = GENERALIZED LINEAR MODEL---------------------------------------------------------
# predict the increase on name agreement based on category in MN.+
# mean of typ. ratings separately for role and taxonomic
glm_3 <- glm(perc_role ~ bool_category_topname + 
                  mean_judgement.role + mean_judgement.taxonomic,
                  weights = weights,
                  data = same_topname_2)
coeftest(glm_3, vcov. = vcovHC, type = "HC1") # add robustness
summary(glm_3) 
stargazer(glm_3) # create a table wih results for Latex

# MODEL 4 - LOGISTIC REGRESSION---------------------------------------------------------
# Predict a binary outcome of category based on typicality ratings
glm_4 <- glm(bool_category ~ mean_judgement,
                family=binomial(link='logit'),
                weights = weights,
                data = dataFrameFull)
coeftest(glm_4, vcov. = vcovHC, type = "HC1") # add robustness
summary(glm_4) 
stargazer(glm_4) # create a table wih results for Latex

# K-fold cross-validation to estimate the prediction error and the accuracy of a model.
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(bool_category ~ mean_judgement, data = dataFrameFull, method = "lm",
               trControl = train.control)
print(model) # low results

# create a confusion matrix
predicted <- round(fitted(glm_3_ok))
truth <- dataFrameFull$bool_category
outcomes <- table(predicted, truth)
outcomes
confusion <- conf_mat(outcomes)
autoplot(confusion, 
         type = "heatmap")
summary(confusion, event_level ='first')

# Train and test 
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(dataFrameFull), replace=TRUE, prob=c(0.8,0.2))
train <- dataFrameFull[sample, ]
test <- dataFrameFull[!sample, ]
#use model to predict probability of default
predicted <- predict(glm_4, test, type="response")
test$bool_category <- ifelse(test$bool_category=="1", 1, 0)
optimal <- optimalCutoff(test$bool_category, predicted)[1] #find optimal cutoff probability to use to maximize accuracy
confusionMatrix(test$bool_category, predicted)
# print metrics
misClassError(test$bool_category, predicted, threshold = optimal)
sensitivity(test$bool_category, predicted) # proportion of mean that say T and the model correctly predicted to be T
specificity(test$bool_category, predicted) # proportion of mean that say R and the model correctly predicted to be R
---------------------------------------------------------
---------------------------------------------------------
# CAUTION: for the following part you need a different df with a column with all responses NOT grouped by image!
---------------------------------------------------------
---------------------------------------------------------
# Reaction time analysis and plot  ---------------------------------------------------------

lapply(dataOLRscale[, c("rate", "category_x",)], table)

ggplot(dataOLRscale, aes(x = rate, y = category_x)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# change rate to factor ---------------------------------------------------

dataOLRscale$rate <- as.factor(dataOLRscale$rate)

# ordered regression ------------------------------------------------------
## fit ordered logit model and store results 'm'
m <- polr(rate ~  bool_category + bool_category_topname + perc_taxa, 
          data = dataOLRscale, Hess=TRUE)
summary(m)
(ctable <- coef(summary(m)))

# calculate a p-value
pv <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = pv))

# get confidence intervals for the parameter estimates
exp(coef(m))
(ci <- confint(m))
confint.default(m)
exp(cbind(OR = coef(m), ci))

# Check for Proportional odds assumption ----------------------------------
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

(s <- with(dataOLRscale, 
           summary(as.numeric(rate) ~ 
                     bool_category + bool_category_topname + perc_taxa, 
                   fun=sf)))

separate_m <- glm(I(as.numeric(rate) >= 2) ~ bool_category, 
                  family="binomial", data = dataOLRscale)
(ci <- confint(separate_m))
separate_m <- glm(I(as.numeric(rate) >= 3) ~ bool_category, 
                  family="binomial", data = dataOLRscale)
(ci <- confint(separate_m))
separate_m <- glm(I(as.numeric(rate) >= 3) ~ perc_taxa, 
                  family="binomial", data = dataOLRscale)
(ci <- confint(separate_m))

s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
plot(s, which=1:4, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))


# Predict prob ------------------------------------------------------------

# obtain predicted probabilities
newdat <- data.frame(
  bool_category = rep(0:1, 2000),
  bool_category_topname = rep(0:1, each = 2000),
  perc_taxa= rep(seq(from = 0.1379, 
                     to = 0.9286 , length.out = 200), 4))
newdat <- cbind(newdat, predict(m, newdat, type = "probs"))
lnewdat <- melt(newdat, 
                id.vars = c("bool_category", "bool_category_topname",
                            "perc_taxa"),
                variable.name = "Level", value.name="Probability")
ggplot(lnewdat, aes(x = perc_taxa, y = Probability, colour = Level)) +
  geom_line() + facet_grid(bool_category_topname ~ bool_category, 
                           labeller="label_both")


olr.var <- dataOLRscale[, c('bool_category','bool_category_topname','perc_taxa')]
ggpairs(olr.var, title = "Correlation Plot between each Variable")

brant(m) # check for odd proportional ratio assumption
featurePlot(x = dataOLRscale[, perc_taxa], 
            y = dataOLRscale[, bool_category], plot = "pairs")
