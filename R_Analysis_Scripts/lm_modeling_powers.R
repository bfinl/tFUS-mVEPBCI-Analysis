library(lme4)
library(multcomp)
library(lmeresampler)
library(readxl)

# -----------------------------------------------------
# Load in the data
# -----------------------------------------------------
data <- read_excel("/Users/jkosnoff/Library/CloudStorage/Box-Box/Joshua-Speller-Paper/Manuscript/nCommu/Source Data.xlsx", "Figure 3 IT-Alpha")

# Only take the the data to be analyzed (the data file also contains p-values, which will mess up this script)
data <- data[, c("power","condition","subject")]
data$subject <- factor(data$subject)
data$condition <- factor(data$condition)


lmm <- lmer(power ~ condition + (1|subject),
            data = data)


# # If you want to do any kind of bootstrap analysis
# # Note that you may want to change the way that resample is applied
# # depending on your set up. As it is currently set up, 
# # it ~should~ be re-sampling at the individual trial level
# # (aka a trial-level case re-sampling)
# mod1_boot <- bootstrap(lmm, .f = fixef, type = "case", B = 1000,
#                        resample = c(FALSE, TRUE))
# confint(mod1_boot, type = 'perc')

# If you want to visualize the residuals:
# qqnorm(resid(lmm))

summary(lmm)

# Get the ANOVA for factor significance
anova(lmm)

# z-test each combination
res <- glht(lmm, linfct = mcp(condition = "Tukey"),
             alternative = 'two.sided',
             test=adjusted(type="none"))

# For these comparisons, take the two-sided test, divide the p-values by 2
# to get the one-tailed (otherwise, you have to specify greater or less than),
# Then do post-hoc p-value correction on the divided p-values

# If you are interested in a two-tailed test, you can just call:
# summary(res, test = adjusted('fdr'))

# Extract p-values
p_values <- summary(res,  test = adjusted('none'))$test$pvalues

# Two tailed test --> one tailed test
p_values <- p_values / 2

# Multiple comparisons FDR correction
p.adjust(p_values, method = "fdr")
