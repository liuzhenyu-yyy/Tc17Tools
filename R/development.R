source("E:/LabWork/code/MyFunction.R")

ReadGeneInfo("hg38", classify.protein = T)
ReadGeneInfo("mm10", classify.protein = T)

usethis::use_data(gene.info.hg38)
usethis::use_data(trans.info.hg38)
usethis::use_data(protien.info.hg38)
usethis::use_data(gene.info.mm10)

x <- 1:20
y <- 5:24 + rnorm(20)

plot.data <- data.frame(x = x, y = y)
model <- lm(y~x, data = plot.data)
res1 <- paste("y = ", signif(model$coefficients[2], 4), "x",
             ifelse(model$coefficients[1] > 0, " + ", " - "),
             signif(abs(model$coefficients[1]), 4),
             sep = "")
res2 <- paste("r^2 = ", signif(summary(model)$r.squared, 4))
ggplot(plot.data) +
  geom_point(aes(x = x, y = y)) +
  theme_classic()+
  geom_smooth(aes(x = x, y = y), method = "lm") +
  geom_text(aes(x = median(x), y = max(y)),
            label = paste(res1, res2, sep = "\n"))

usethis::use_mit_license()
devtools::document()
devtools::check(args = "--no-examples")
