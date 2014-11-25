#' Relative To Control Primer
#' @description Compare qPCR data to a control locus
#' For example: take the normalized data from delta_ct and compare to a negative locuse.
#' Function should be used as second step in ChIP-qPCR
#' @param df data frame must have columns Well, Cq, Sample, Group, Primer.
#' @param primer What is the name of the control primer - should be in the 'Primer' column of df


rel_to_primer = function(df, primer) {
  control = df[df$Primer == primer, ]$val
  if (is.null(control)) { print (df) }
  df$norm = df$val/control
  return(df)
}

#' Relative to Control Sample
#' @description Compare qPCR data to a control sample.
#' For example: take normalized data and get relative fold enrichment
#' Function should be used as first step in qRT-PCR
#' @param df data frame should contain columns  Well, Cq, Sample, Group, Primer.
#' @param col name or integer of column used as control sample
#' @param sample name of the sample to be used as the control


rel_to_sample <- function(df, col, sample, log2=F) { 
  control <- df[df[,col] == sample, ]$val
  if (is.null(control)) { print(df) } 
  df$norm <- df$val/control
  if (log2 == T) { 
    df$norm <- log2(df$norm) 
  } 
  return(df)
} 
