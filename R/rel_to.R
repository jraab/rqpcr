#' Relative To
#' @description Compare qPCR data to a control primer
#' @param df data frame must have columns Well, Cq, Sample, Group, Primer. 
#' @param primer What is the name of the control primer - should be in the 'Primer' column of df
#' @export

rel_to = function(df, primer) { 
  control = df[df$Primer == primer, ]$val
  df$norm = df$val/control
  return(df)
}