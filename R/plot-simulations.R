
rm(list = ls())

library(ggplot2)
library(dplyr)

df <- readr::read_csv("output/mon-sims.csv")

ggplot(df, aes(x = true, y = est, color = fd_z)) + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_point(shape = 1) + 
  #scale_shape(solid = FALSE) + 
  scale_color_gradient2(high = "#d95f02", mid = "#1b9e77", low = "#d95f02") + 
  labs(x = "True Effect",
       y = "Estimated Effect",
       color = "Effect of Identifying Variable") + 
  facet_grid(w1_type ~ obs) + 
  theme_bw() + theme(panel.margin = unit(.3, "cm")) + theme(legend.position = "bottom")
ggsave("doc/figs/mon-sims-scatter.pdf", height = 7, width = 9)



summarize(group_by(df, obs, w1_type), 
          bias = round(mean(abs(est - true)), 3), 
          #true = round(mean(abs(true)), 3), 
          q95 = round(quantile(abs(est - true), .95), 2),
          max = round(quantile(abs(est - true), 1), 2),
          gt_0.1 = round(100*mean(abs(est - true) > 0.1)),
          gt_0.3 = round(100*mean(abs(est - true) > 0.3)),
          gt_0.5 = round(100*mean(abs(est - true) > 0.5)),
          cor = round(cor(est, true), 2),
          sign_error = 100*mean(sign(est) != sign(true)))

m <- quantreg::rq(abs(true - est) ~ abs(fd_z), tau = 0.95, data = subset(df, obs == "partial observability"))
pred_df <- data.frame(fd_z = c(.1, .3, .5))
predict(m, newdata = pred_df)


qplot(abs(fd_z), abs(true - est), data = df) + geom_smooth(method = "lm", se = FALSE) + stat_quantile(quantiles = c(0.95))

ggplot(df, aes(x = abs(fd_z), y = abs(true - est))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_quantile(quantiles = 0.95) + 
  facet_grid(w1_type ~ obs)
  
  
with(df, cor(abs(fd_z), abs(true - est)))


  
df <- readr::read_csv("output/link-sims.csv")
df$link <- reorder(x = df$link, X = abs(df$fd1 - df$true1), FUN = sd)


ggplot(df, aes(x = true1, y = fd1, shape = w1_type, color = n_x + n_z)) + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_point() + 
  facet_grid(link ~ obs) + 
  scale_shape(solid = FALSE) + 
  theme_bw() + 
  scale_color_gradient(high = "#d95f02", low = "#1b9e77") + 
  labs(x = "True Effect",
       y = "Estimated Effect",
       color = "Number of Identifying Variables",
       shape = "Type of Key Explanatory Variable") + theme(legend.position = "bottom")
ggsave("doc/figs/link-sims-scatter.pdf", height = 10, width = 10)


summarize(group_by(df, w1_type, obs, link), 
          bias = round(mean(abs(fd1 - true1)), 3), 
          #true = round(mean(abs(true1)), 3), 
          q95 = round(quantile(abs(fd1 - true1), .95), 2),
          max = round(quantile(abs(fd1 - true1), 1), 2),
          gt_0.1 = round(100*mean(abs(fd1 - true1) > 0.1)),
          gt_0.3 = round(100*mean(abs(fd1 - true1) > 0.3)),
          gt_0.5 = round(100*mean(abs(fd1 - true1) > 0.5)),
          cor = round(cor(fd1, true1), 2),
          sign_error = 100*mean(sign(fd1) != sign(true1)))

qplot(n_x + n_z, abs(true1 - fd1), data = df) + geom_smooth(method = "lm")
s50 <- summarize(group_by(df, k = n_x + n_z, link), q = quantile(abs(fd1 - true1), .5), at = "50")
s95 <- summarize(group_by(df, k = n_x + n_z, link), q = quantile(abs(fd1 - true1), .95), at = "95")
s <- rbind(s50, s95)
qplot(k, q, color = at, facets = ~ link, data = s)

with(subset(df, link == "cauchit"), cor(n_x + n_z, abs(true1 - fd1)))
with(df, cor(n_x + n_z, abs(true1 - fd1)))

m <- quantreg::rq(abs(true1 - fd1) ~ I(n_x + n_z), tau = 0.95, 
                  data = subset(df, obs == "partial observability" & link == "cauchit"))
pred_df <- data.frame(n_x = c(0, 1, 2), n_z = c(1, 1, 2))
predict(m, newdata = pred_df)

