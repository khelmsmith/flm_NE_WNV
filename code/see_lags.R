source("code/extract_functional.r")
library(ggplot2)

test.model <- extract_functional(mod)

test.model$label <- sub(".*:", "", test.model$label)

lags <- ggplot(test.model, aes(x=-x, y=fit)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se), alpha=0.4) +
  geom_hline(yintercept=0, linetype=2) +
  facet_wrap(~label, scale = "free_y") +
  #labs(title = paste0(modname)) +
  xlab("months") +
  ylab("coefficient") +
  theme_bw()
lags

