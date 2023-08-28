
# EDA ---------------------------------------------------------------------

# Mulitcollinearity - Visual Inspection

png("imgs/multicollinearity.png", width = 1080, height = 1080, units = "px", pointsize = 32)
par(family = "serif")
metrics_minmax %>%
  dplyr::select(c(popularity, degree, betweenness, eigenvector)) %>% 
  stats::cor(use = "na.or.complete") %>% 
  corrplot::corrplot.mixed(upper = "color",
                           lower = "number",
                           tl.pos = "lt",
                           tl.col = "black",
                           lower.col = "black",
                           number.cex = 1)
dev.off()




# Multiple Linear Regression ----------------------------------------------

# 1. Continuity of variables
# Create plot_data
plot_data <- metrics_minmax %>% 
  tidyr::pivot_longer(cols = -c(artist, Genre, genre2, genre3, collaboration),
                      names_to = "key", values_to = "value")
# Visualise relation distribution between variables and quality
par(family = "serif")
var_dist_freq <- plot_data %>% 
  ggplot2::ggplot(aes(x = value), na.rm = T) + 
  ggplot2::geom_density(alpha = 0.6) +
  ggplot2::facet_wrap(~ key, scales = "free")
ggsave(var_dist_freq,
       filename = "var_dist_freq.png",
       path = here::here("imgs"),
       device = "png",
       width = 6, height = 4, units = "in",
       dpi = 600
)
# Visualise relation between variables and quality in boxplots
par(family = "serif")
var_dist_box <- plot_data %>%
  ggplot2::ggplot(aes(y = value), na.rm = T)+
  ggplot2::geom_boxplot(alpha =0.6)+
  ggplot2::facet_wrap(~key, scales ="free")+
  ggplot2::theme(axis.title.x =element_blank(),
                 axis.text.x =element_blank(),
                 axis.ticks.x =element_blank())
ggsave(var_dist_box,
       filename = "var_dist_box.png",
       path = here::here("imgs"),
       device = "png",
       width = 6, height = 4, units = "in",
       dpi = 600
)

# 2. Linear relationship between variables
png("imgs/added-variable-plots.png", width = 1080, height = 960, units = "px", pointsize = 32)
par(family = "serif")
avPlots(regression, main = "")
dev.off()

# 3. Multicollinearity
#see EDA for visual inspection
vif(lm(popularity ~ degree + betweenness + eigenvector,
       data = metrics_minmax, na.action = na.exclude))

# 4. Variance homogeneity
png("imgs/residuals-vs-fitted.png", width = 1080, height = 960, units = "px", pointsize = 32)
par(family = "serif")
plot(regression, 1, main = "")
dev.off()
# Breusch-Pagan test
bptest(regression)

# 5. Strong cases
# Cook Distances determine how strongly individual values influence the regression.
png("imgs/cooks-distances.png", width = 1080, height = 960, units = "px", pointsize = 32)
par(family = "serif")
plot(regression, 4, main = "")
dev.off()

# 6. Normal distribution of residuals
# 1. Histogram of residuals
png("imgs/histogram-residuals.png", width = 1080, height = 960, units = "px", pointsize = 32)
par(family = "serif")
hist(regression$residuals, breaks= 30, freq = F,
     main = "Distribution of residuals",
     xlab = "Residuals")
curve(dnorm(x, mean(regression$residuals), sd(regression$residuals)), 
      col = "red",
      add = T)
dev.off()
# 2. Q-Q-plot
png("imgs/q-q-plot.png", width = 1080, height = 960, units = "px", pointsize = 32)
par(family = "serif")
plot(regression, 2, main = "")
dev.off()


# Penalized Regression ----------------------------------------------------

# Alpha and Lambda Visualisations

alpha <- pop_elnet_results %>% ggplot(aes(x = alpha, y = RMSE, col = lambda)) + geom_point() +
  theme(axis.text = element_text(color="black", size=12, family="serif"),
        axis.text.x = element_text(color="black", size=12, family="serif"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.line.x = element_line(linewidth=0.5, color="grey"),
        axis.line.y = element_line(linewidth=0.5, color="grey"),
        panel.grid = element_line(color = "honeydew2",
                                  size = 0.5,
                                  linetype = 1),
        panel.background = element_rect(fill="white"),
        plot.margin = margin(10,10,10,10),
        plot.title = element_text(color="black", size=16, family="serif"),
        plot.subtitle = element_text(color="grey26", size=14, family="serif"),
        legend.text = element_text(color="black", size=12, family="serif"),
        text = element_text(color="black", size=14, family="serif")
  )
ggsave(alpha,
       filename = "alpha.png",
       path = here::here("imgs"),
       device = "png",
       width = 6, height = 4, units = "in",
       dpi = 600
)

lambda <- pop_elnet_results %>% ggplot(aes(x = lambda, y = RMSE, col = alpha)) + geom_point() +
  theme(axis.text = element_text(color="black", size=12, family="serif"),
        axis.text.x = element_text(color="black", size=12, family="serif"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.line.x = element_line(linewidth=0.5, color="grey"),
        axis.line.y = element_line(linewidth=0.5, color="grey"),
        panel.grid = element_line(color = "honeydew2",
                                  size = 0.5,
                                  linetype = 1),
        panel.background = element_rect(fill="white"),
        plot.margin = margin(10,10,10,10),
        plot.title = element_text(color="black", size=16, family="serif"),
        plot.subtitle = element_text(color="grey26", size=14, family="serif"),
        legend.text = element_text(color="black", size=12, family="serif"),
        text = element_text(color="black", size=14, family="serif")
  )
ggsave(lambda,
       filename = "lambda.png",
       path = here::here("imgs"),
       device = "png",
       width = 6, height = 4, units = "in",
       dpi = 600
)

# Predicted vs Original Values
png("imgs/predicted-vs-original.png", width = 1080, height = 720, units = "px", pointsize = 32)
par(family = "serif")
plot(y, type = "o", col = "blue", ylim = range(c(y, enr_preds$pop)), 
     xlab = "Observation", ylab = "Value", main = "")
lines(enr_preds$preds, type = "o", col = "red")
legend("topright", legend = c("Original", "Predicted"), 
       col = c("blue", "red"), pch = c(1, 1))
dev.off()

# ANOVA -------------------------------------------------------------------

# Mean Distribution
anova_means <- ggplot(metrics, aes(x = collaboration, y = popularity, group = collaboration)) +
  geom_point(cex = 1.5, pch = 1.0, position = position_jitter(w = 0.1, h = 0),
             colour = "royalblue", alpha = 0.75) +
  xlab("") +
  #stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'point', colour = "black", shape = 18, size = 2) +
  theme(axis.text = element_text(color="black", size=12, family="serif"),
        axis.text.x = element_text(color="black", size=12, family="serif"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.line.x = element_line(linewidth=0.5, color="grey"),
        axis.line.y = element_line(linewidth=0.5, color="grey"),
        panel.grid = element_line(color = "honeydew2",
                                  size = 0.5,
                                  linetype = 1),
        panel.background = element_rect(fill="white"),
        plot.margin = margin(10,10,10,10),
        plot.title = element_text(color="black", size=16, family="serif"),
        plot.subtitle = element_text(color="grey26", size=14, family="serif"),
        legend.text = element_text(color="black", size=12, family="serif"),
        text = element_text(color="black", size=14, family="serif")
  )
ggsave(anova_means,
       filename = "anova-means.png",
       path = here::here("imgs"),
       device = "png",
       width = 6, height = 4, units = "in",
       dpi = 600
)

# Homoscedasticity plots
png("imgs/anova-homoscedasticity.png", width = 1080, height = 960, units = "px", pointsize = 32)
par(family = "serif")
par(mfrow = c(2,2))
plot(aov)
par(mfrow = c(1,1))
dev.off()

# Confidence Intervals
png("imgs/anova-confidence.png", width = 1080, height = 960, units = "px", pointsize = 32)
par(family = "serif")
plot(TukeyHSD(aov), las = 1, main = "")
dev.off()
