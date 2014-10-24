#' Process data
#' @description Take a data frame containing qPCR data and return a data frame with normalized and relative data
#' @param df data frame must have columns Well, Cq, Sample, Group, Primer.
#' @param threshold standard deviation for replicates over which a warning will be flagged
#'     default = 0.5
#' @param control_group Sample name to compare data to
#' @param control_primer Primer to compare enrichment against
#' @export

process_data <- function(df, threshold=0.5 ) {
  df <- df[!is.na(df$Sample), ]
  df <- df[!is.null(df$Sample), ]
  df <- df[!df$Sample == '',]
  summary = df %>% group_by(Primer, Sample, Group) %>%
    summarise(mean=mean(Cq, na.rm=T), sd=sd(Cq, na.rm=T) )
  flagged <- summary[summary$sd > threshold, ]
  if (nrow(flagged) > 0) {
    print(flagged)
  }
 return(summary)
}
