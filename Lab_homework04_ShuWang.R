## Recreat graph from Lecture 8 Notes ##

library(ggplot2)
library(tidyverse)

x1 = seq(-3, 3, 0.01)
y1 <- dnorm(x1)
x2 = seq(0, 6, 0.01)
y2 <- dnorm(x2, mean = 3.2)
x_category <- c(rep("x1", length(x1)), rep("x2", length(x2)))
data_final <- data.frame(x_category, x = c(x1, x2), y = c(y1, y2))

p <- ggplot(data = data_final, aes(x = x, y = y, group = x_category)) +
  geom_line(size = 1, aes(col = x_category), show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red")) +
  geom_vline(aes(xintercept = qnorm(0.95)), lty=2, lwd=1)

p <- p + 
  geom_ribbon(data = filter(data_final, (x_category == "x1" & x >= qnorm(0.95)) | (x_category == "x2" & x <= qnorm(0.95))),
              aes(x = x, ymax = y, ymin = 0, fill = x_category), colour = "black", alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), labels = c("  Type I Error", "  Type II Error")) +
  annotate("text", x = 1.3, y = 0.03, label = expression(beta), 
           col = "black", size = 8) +
  annotate("text", x = 2, y = 0.03, label = expression(alpha), 
           col = "white", size = 8)

p <- p + labs(x = "", y = "") +
  scale_x_discrete(limits=c(0, 3.2), label = c(expression(theta[0]), expression(theta[a]))) +
  theme_bw() +
  theme(panel.grid =element_blank(),
        legend.title=element_blank(),
        legend.position = c(0, 1),
        legend.justification=c(0,1),
        legend.background=element_rect(fill="white",colour="black"),
        legend.margin = margin(8, 8, 8, 8),
        legend.text = element_text(size = 12, face = "bold"),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text.x = element_text(size = 20, vjust = -2),
        axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 2, 
                                   face = "bold",size = 15),
        panel.background = element_rect(colour = "black", size = 1))
p


## A function to check if a given positive integer is a prime number ##

findprime <- function (x) {
  if (x <= 1) {
    return("This is not a prime number.")
  } 
  if (x == 2) {
    return("This is a prime number.")
    } 
  for (i in 2:sqrt(x)) {
    if (x %% i == 0) {
      return("This is not a prime number.")
    } else {
      return("This is a prime number.")
    }
  }
}
