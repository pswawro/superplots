library(tidyverse)
library(ggpubr)
library(extrafontdb)
library(extrafont)
library(Cairo)
library(broom)

loadfonts()

# Create dummy dataframe
data <- tibble(condition = factor(rep(c("a", "b", "c"), times = 3)),
               rep = factor(rep(1:3, each = 3)),
               total = rep(50, times = 9),
               value = sample(0:50, size = 9)) %>%
  mutate(ratio = value/total)

data_means <- data %>%
  group_by(condition) %>%
  summarize(mean_ratio = mean(ratio),
            median_ratio = median(ratio),
            sem = sd(ratio) / sqrt(length(ratio)),
            max_ratio = max(ratio))

# Calculate stats
data_signif <- if(length(unique(data$condition)) == 2) {
  t.test(ratio ~ condition, data, paired = TRUE) %>%
    tidy() %>%
    transmute(group1 = unique(data$condition)[2],
              group2 = unique(data$condition)[1],
              p = signif(p.value, 2),
              y.position = max(data$ratio))
} else {
  TukeyHSD(aov(ratio ~ condition, data = data)) %>%
    tidy() %>%
    separate(contrast, into = c("group1", "group2")) %>%
    transmute(group1,
              group2,
              p = signif(adj.p.value, 2),
              y.position = max(data$ratio) * 1.1) %>%
    filter(group2 == "a")
}

# Plot data
theme <- theme(legend.position = "none",
               text = element_text(family = "Arial", color = "black"),
               axis.title.x = element_blank(),
               axis.title.y = element_text(size = 9),
               axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5),
               axis.text.y = element_text(size = 8, margin = unit(c(0, 2, 0, 0), "mm")),
               plot.tag = element_text(size = 8),
               plot.tag.position = c(0.3, 0.06),
               axis.ticks.length.y = unit(-1, "mm"))

ggplot(data_means, aes(x = condition, y = mean_ratio)) +
  geom_col() +
  geom_point(data = data, aes(x = condition, y = ratio, fill = rep, shape = rep), size = 5) +
  geom_errorbar(aes(ymin = mean_ratio - sem, ymax = mean_ratio + sem), width = 0.2) +
  stat_pvalue_manual(data = data_signif, step.increase = 0.1) +
  scale_shape_manual(values = c(22, 23, 24)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "xxx") +
  theme_classic() +
  theme