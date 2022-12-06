#' @title: titanicGlm
#' @version: 0.1 /3/27/2019

#' @author Zhenyuan Lu

#'
#' @source source("www/functions/titanicGlm.R")
#'
#' @example titanicGlm(titanic, "age", "survived", "sex", facet="sex")
#' 
#' Loard libs ----
library(tidyverse)
titanicGlm <- function(df, x, color, facet=c("survived", "sex", "age", "pclass")){
  if(is.null(df))
    stop("")
  facet <-
    match.arg(facet, c("survived", "sex", "age", "pclass"), FALSE)

  
  p <- 
    df %>%
    filter(!is.na(x)) %>%
    ggplot(aes_string(x = x, y = "survived", color = color)) +
    geom_jitter(height = 0.05, alpha = 0.35) +
    geom_smooth(method = "glm",
                method.args = list(family = "binomial"))  +
    facet_wrap(reformulate(facet)) +
    labs(x = x, y = paste("Probability of Survived",sep=" ")) +
    theme_bw()

return(p)
}




