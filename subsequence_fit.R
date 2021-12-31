#' subsequence_fit
#'     Model variant counts against collection date within a date range
#'
#' @param variants - data frame, Sars-CoV-2 variants of interest and concern created by plot_named_variants.
#' @param variant - character, a variant name
#' @param start - a date or a character string in the format "YYYY-MM-DD", start of data range
#' @param end - a date or a character string in the format "YYYY-MM-DD", end of data range 
#' @param plot - logical, should the data be plotted. Default = TRUE
#' @param title - character string, an optional title.
#'
#' @return a list
#'     df - the rows from variants with an added Fit column containing the fitter data.
#'     fit - the linear model, log(Count) ~ Collection.date for the date range.
#'     
#' @requires dates must be in the range [start, end].
#'     
subsequence_fit <- function(variants,
                            variant,
                            start,
                            end,
                            plot = TRUE,
                            title = NULL) {
  require(tidyverse)
  
  # convert dates if needed
  start_date <- start
  if(class(start_date) == "character") {
    start_date <- as.Date(start_date)
  }
  
  end_date <- end
  if(class(end_date) == "character") {
    end_date <- as.Date(end_date)
  }
  
  # select rows with variant and within date range
  df <- variants %>%
    filter(Variant == {{ variant }}) %>%
    filter(Collection.date >= start_date & Collection.date <= end_date)
  
  # model
  fit <- lm(log(Count) ~ Collection.date, data = df)
  df <- df %>%
    add_column(Fit = predict(fit))
  
  if(plot) {
    p <- ggplot(df) + 
      geom_point(aes(x = Collection.date, y = log(Count))) +
      geom_line(aes(x = Collection.date, y = Fit))
    
    if(! is.null(title)) {
      p <- p + ggtitle(title)
    }
    
    print(p)
  }
  
  return(list(df = df, fit = fit))
 
}