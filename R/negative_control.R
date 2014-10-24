#' Negative Control
#' @description Compare values to a negative control locus
#' @param df data frame must have columns Well, Cq, Sample, Group, Primer.
#' @param primer to compare values to
#' @export

negative_control <- function(df, primer) {
  control = df[df$Primer == primer, ]$val
  df$norm = df$val/control
  return(df)
}
