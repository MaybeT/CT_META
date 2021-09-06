library(plotly)
library(reshape2)

sleep = read.table("http://www.statsci.org/data/general/sleep.txt", header = T)
sleep = na.omit(sleep)
sleep = mutate(sleep, logTotalSleep = log(TotalSleep))
sleep_mod = lm(logTotalSleep ~ Gestation + Danger, data=sleep)

graph_reso = 0.5
axis_x = seq(min(sleep$Gestation), max(sleep$Gestation), by = graph_reso)
axis_y = seq(min(sleep$Danger), max(sleep$Danger), by = graph_reso)

sleep_surface = expand.grid(Gestation = axis_x,
                            Danger = axis_y,
                            KEEP.OUT.ATTRS = F)

sleep_surface$TotalSleep = exp(predict.lm(sleep_mod, newdata = sleep_surface))

sleep_surface = acast(sleep_surface, Danger ~ Gestation, value.var = "TotalSleep")

p = plot_ly(data = sleep) %>% 
  add_trace(x = ~Gestation, y = ~Danger, z = ~TotalSleep,
            type = "scatter3d", mode = "markers",
            opacity = 0.8) %>% 
  add_trace(z = sleep_surface,
            x = axis_x,
            y = axis_y,
            type = "surface", colorscale = list(c(0,1), c("green","blue")))

p

htmlwidgets::saveWidget(p, 'example.html')

# path/to/file.html

dat <- iris
# Edit from here
x <- which(names(dat) == "Species") # name of grouping variable
y <- which(names(dat) == "Sepal.Length" # names of variables to test
           | names(dat) == "Sepal.Width"
           | names(dat) == "Petal.Length"
           | names(dat) == "Petal.Width")
method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
my_comparisons <- list(c("setosa", "versicolor"), c("setosa", "virginica"), c("versicolor", "virginica")) # comparisons for post-hoc tests
# Edit until here
# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(dat,
                   x = colnames(dat[j]), y = colnames(dat[i]),
                   color = colnames(dat[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter"
    )
    print(
      p + stat_compare_means(aes(label = paste0(..method.., ", p-value = ", ..p.format..)),
                             method = method1, label.y = max(dat[, i], na.rm = TRUE)
      )
      + stat_compare_means(comparisons = my_comparisons, method = method2, label = "p.format") # remove if p-value of ANOVA or Kruskal-Wallis test >= alpha
    )
  }
}
# new code below for t-test
dat <- Stats
# remove one level to have only two groups
dat <- subset(dat, Stats != "X8km", "X12km", "X16km","Planter")
dat$Stats <- factor(dat$Rep)
# boxplots and t-tests for the 4 variables at once
for (i in 1:4) { # variables to compare are variables 1 to 4
  boxplot(dat[, i] ~ dat$Rep, # draw boxplots by group
          ylab = names(dat[i]), # rename y-axis with variable's name
          xlab = "Reps"
  )
  print(t.test(dat[, i] ~ dat$X8km)) # print results of t-test
}

#compute one-sample t-test in R
t.test(Stats$X8km, Stats$X12km = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


t.test(Stats$X8km, Stats$X12km, Stats$X16km, Stats$Planter)


