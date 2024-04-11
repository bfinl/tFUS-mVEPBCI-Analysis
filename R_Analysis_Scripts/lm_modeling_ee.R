library(lme4)
library(multcomp)
library(BayesFactor)
library(lmeresampler)
library(svglite)
library(ggplot2)
library(readxl)

# -----------------------------------------------------
# Load in the data
# -----------------------------------------------------
data <- read_excel("/Users/jkosnoff/Library/CloudStorage/Box-Box/Joshua-Speller-Paper/Manuscript/nCommu/Source Data.xlsx", "Figure 1")

# Only take the the data to be analyzed (the data file also contains p-values, which will mess up this script)
data <- data[, c("EE","Condition","Order", "Scans", "Subj")]
data$Subj <- factor(data$Subj)
data$Condition <- factor(data$Condition)

# -----------------------------------------------------
# Define the linear mixed effect model
# -----------------------------------------------------
lmm <- lmer(EE ~ Condition + Order*Scans + (1|Subj),
            data = data)

# -----------------------------------------------------
# Run bootstrap analysis 
# -----------------------------------------------------
# mod1_boot <- bootstrap(lmm, .f = fixef, type = "case", B = 1000,
#                       resample = c(FALSE, TRUE))

# -----------------------------------------------------
# Run a Type III ANOVA to test factor significance
# -----------------------------------------------------
anova(lmm)

# -----------------------------------------------------
# Z-tests on the LMM 
# -----------------------------------------------------
res <- glht(lmm, linfct = mcp(Condition = "Tukey"), 
          alternative = 'two.sided', 
          adjusted = (type = 'none'))

# For these comparisons, take the two-sided test, divide the p-values by 2
# to get the one-tailed (otherwise, you have to specify greater or less than), 
# Then do post-plot p-value correction on the divided p-values
summary(res, test = adjusted('bonferroni'))

# Extract p-values. Note that the adjustment should be explicitly stated as 
# 'none.' Extract raw p-values, then divide by two if performing a one-tailed
# test, then perform adjustment.
p_values <- summary(res, test = adjusted('none'))$test$pvalues
# Two tailed test --> one tailed test
p_values <- p_values / 2
# p_values
# Multicomparison correction
p.adjust(p_values, method = "bonferroni")

# -----------------------------------------------------
# Bayes Factor Analysis
# -----------------------------------------------------

# Define the range of rfixedscale values
rfixedscale_values <- seq(0.01, 1, by = 0.01) 

# Number of times to compute the Bayes Factor for each rfixedscale value
num_iterations <-1000

# Container to store the Bayes Factors
bayes_factors <- matrix(nrow = length(rfixedscale_values), ncol = num_iterations)

# Compute Bayes Factors for each rfixedscale value
for (i in 1:length(rfixedscale_values)) {
  rfixedscale <- rfixedscale_values[i]
  for (j in 1:num_iterations) {
    # Compute Bayes Factor
    bf_result <- anovaBF(EE ~ Condition + Subj,
                         whichRandom = "Subj",
                         rscaleFixed = rfixedscale,
                         data = data)
    bayes_factors[i, j] <- exp(bf_result@bayesFactor$bf)  # Save Bayes Factor
  }
}

# Calculate median and confidence interval for each rfixedscale value
medians <- apply(bayes_factors, 1, median)
lower_ci <- apply(bayes_factors, 1, function(x) quantile(x, probs = 0.025))
upper_ci <- apply(bayes_factors, 1, function(x) quantile(x, probs = 0.975))

df <- data.frame(rfixedscale = rfixedscale_values,
                 median = medians,
                 lower_ci = lower_ci,
                 upper_ci = upper_ci)

# Create the plot using ggplot2
ggplot(df, aes(x = rfixedscale)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "black", alpha = 0.3) +
  geom_line(aes(y = median), color = "black") +
  
  # Add lines corresponding to BF intervals
  geom_hline(yintercept = c(3, 10), linetype = "dashed", color = c("blue", "red")) +
 
  # Add y axis points and labels. Add axes
  scale_y_continuous(breaks = c(0, 3, 10, 20, 30, 40, 50, 60)) +
  labs(x = "Fixed Effect Scaling Factor", y = "Median Bayes Factor") + 
  # geom_hline(yintercept = c(0), linetype = "solid", color = c("darkgray")) +
  # geom_vline(xintercept = c(0), linetype = "solid", color = c("darkgray")) +
  
  # 
  geom_ribbon(data = subset(df, median >= 10),
              aes(ymax = lower_ci, ymin = 0, fill = "strong evidence"), alpha = 0.3) +
  
  geom_ribbon(data = subset(df, median <= 10.5 & median > 3),
              aes(ymax = lower_ci, ymin = 0, fill = "moderate evidence"), alpha = 0.3) +

  scale_fill_manual(name = "Bayes Factor",
                    values = c("strong evidence" = "red", "moderate evidence" = "blue"),
                    labels = c("moderate evidence", "strong evidence")) +
  
  # Label the default point
  geom_point(data = df[df$rfixedscale == 0.5, ], aes(x = 0.5, y = median), size = 5, shape = 21, fill = "darkgray", color = "black") +
  geom_text(data = df[df$rfixedscale == 0.5, ], aes(x = 0.5, y = median), label = "Recommended Default", color = "black", nudge_y = 3, nudge_x = 0.18) +
  
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.95, 0.95), legend.justification = c(1, 1),
        text = element_text(family = "Arial", size = 14),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20)) # + 
  
  ggsave(file="Fig1c.svg", width=8, height=8)
