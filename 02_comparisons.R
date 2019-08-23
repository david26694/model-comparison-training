
library(tidyverse)
library(tidyposterior)


load(file = 'data/aucs.RData')

# Paired t-test -------------------------------------------------------------------------------

# Paired t test finds differences
t.test(aucs$roc_auc_xgb, aucs$roc_auc_rf, paired = T)
t.test(aucs$roc_auc_xgb, aucs$roc_auc_lasso, paired = T)

# Anova ---------------------------------------------------------------------------------------

anova_df <- aucs %>% 
  gather(model, value, -id)

# Anova finds differences
anova(lm(value ~ model, anova_df))

summary(lm(value ~ model, anova_df))

anova_df %>% 
  ggplot(aes(x = model, y = value, group = id, color = id)) + 
  geom_line() + 
  theme_minimal()

# Tidyposterior -------------------------------------------------------------------------------

bayesian_lm <- perf_mod(aucs, transform = logit_trans)

bayesian_comparison <- contrast_models(bayesian_lm)

ggplot(tidy(bayesian_lm)) + theme_minimal()

summary(bayesian_comparison, size = 0.01)
summary(bayesian_comparison, size = 0.03)

ggplot(bayesian_comparison, size = 0.01) + theme_minimal()
ggplot(bayesian_comparison, size = 0.03) + theme_minimal()
