# Week2
library(UsingR); data(diamond)
plot(diamond$carat, diamond$price,
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch =21, frame = FALSE)
abline(lm(price ~ carat, data = diamond), lwd = 2)