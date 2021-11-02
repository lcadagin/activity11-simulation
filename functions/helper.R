generate_n_samples <- function(n){
  tibble(
    normal = rnorm(n = n, mean = 65, sd = 2),
    exponential = rexp(n = n, rate = 0.5),
    chisqr = rchisq(n = n, df = 23),
    uniform = runif(n = n, min = 7, max = 16)
  )
}


distribution_exploration <- function(n, rep, mean, sd, rate, df, min, max){
  
  .samps <- map(1:rep, generate_n_samples)
  
  .samp_means <- map_df(.samps, sample_mean)
  
  .restructured_means <- .samp_means %>% 
    pivot_longer(
      cols = everything(),
      names_to = "distribution",
      values_to = "means"
    )
  
  .plot <- .restructured_means %>% 
    ggplot(aes(x = means)) +
    geom_histogram(aes(y = ..count.. / sum(..count..))) +
    facet_wrap(~ distribution, ncol = 4, scales = "free") +
    labs(y = "proportion")
  
  .tab <- .restructured_means %>% 
    group_by(distribution) %>% 
    summarise(mean = mean(means),
              sd = sd(means))
  
  list(.plot, .tab)
  
}