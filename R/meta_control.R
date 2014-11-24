#' Geometric mean of control genes for normalizing
#' @description Takes a data frame and the names of control primers, can be more than one, and returns a data frame
#' that contains the geometric mean of the control primers - allowing for relative values to be calculated. Similiar to other
#' functions in this package, requires a specific data frame
#' @param df data frame containing all data, from the process_data function, containing columns, Primer, Sample, Group, mean, sd
#' @param control control genes to calculate the geometric mean
#' @export

meta_control <- function(df, control) {
  control_df <- df[df$Primer %in% control, ]
  #data should not have zeros
  gmean <- function(x) { exp(mean(log(x)))}
  metagenes <- control_df %>% group_by(Sample, Group) %>% summarise(mean=gmean(mean))
  metagenes$Primer <- rep('meta')
  metagenes$sd <- rep(0)
  full <-  rbind(df, metagenes)
  return(full)
}
