#' See lags
#' 
#' @param mod A model object created by call.flm
#' @param results.path File path for output
#' 
see.lags = function(mod, results.path){

  test.model <- extract_functional(mod)

  test.model$label <- sub(".*:", "", test.model$label)
  
  lags <- ggplot2::ggplot(test.model, aes(x=-x, y=fit)) +
    geom_line() +
    geom_ribbon(aes(ymin=fit-se, ymax=fit+se), alpha=0.4) +
    geom_hline(yintercept=0, linetype=2) +
    facet_wrap(~label, scale = "free_y") +
    #labs(title = paste0(modname)) +
    xlab("months") +
    ylab("coefficient") +
    theme_bw()
  lags

  save1 = ggsave(sprintf("%slags.png", results.path), lags)
  
  
}