
library(ggplot2)

x <- -2:1
y <- plogis(x)
y_star <- y
y_star[1] <- 0.18
df2 <- data.frame(x, y, y_star)

x <- seq(-2, 1, length.out = 100)
y <- plogis(x)
df <- data.frame(x, y)

ggplot(df, aes(x = x, y = y)) + 
  geom_line(color = "#1b9e77", size = 1.5) + 
  geom_point(data = df2, aes(x = x, y = y_star), color = "#d95f02", size = 3) + 
  geom_smooth(data = df2, aes(x = x, y = y_star), span = 1, se = FALSE, linetype = 2, color = "#d95f02") + 
  labs(x = expression(x), 
       y = expression(g^-1*(x))) + 
  theme_bw() + 
  annotate(geom = "text", x = x[1], y = y[1], label = "plain(logit)^-1*(x)", parse = TRUE, vjust = 1/2, hjust = -1/1.5, color = "#1b9e77", size = 5) + 
  annotate(geom = "text", x = df2$x[1], y = df2$y_star[1], label = "h^-1*(x)", parse = TRUE, vjust = -1/2, hjust = -1/12, color = "#d95f02", size = 5)
ggsave("doc/figs/example.pdf", height = 4, width = 7)