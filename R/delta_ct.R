#' Calculate Delta Ct
#' @description This function takes a data frame and calculates the 2^(ct_control - ct_experiment)
#' @param df data frame must have columns Well, Cq, Sample, Group, Primer.
#' @param control Group name of the control sample - defaults to control
#' @param col column number or character that contains the control group
#' @export

delta_ct = function(df, col, control='Control') {
  cntrl <- df[df[,col] == control, ]$mean
  df$val = 2^(cntrl-df$mean)
  return(df)
}

