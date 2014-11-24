#' Calculate Delta Ct For ChIP samples
#' @description This function takes a data frame and calculates the 2^(ct_control - ct_experiment)
#' @param df data frame must have columns Well, Cq, Sample, Group, Primer.
#' @param control Group name of the control sample - defaults to control
#' @param col column number or character that contains the control group


delta_ct_chip = function(df, col, control='Control') {
  cntrl <- df[df[,col] == control, ]$mean
  df$val = 2^(cntrl-df$mean)
  return(df)
}

#' Calculate Delta Ct of RT-qPCR compared to a meta gene
#' @description This function takes a data frame and calcualte  the delta ct
#' compared to a normalization gene or a meta gene gene
#' @param df data frame must have columns Well, Cq, Sample, Group, Primer
#' @param control name of control primer sample defaults to 'meta'


delta_ct_rt <- function(df, control='meta') {
  cntrl <- df[df$Primer == control, ]$mean
  df$val <- 2^(cntrl-df$mean)
  return(df)
}

