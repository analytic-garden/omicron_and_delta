#' plot_named_variants
#' Draw plots of daily counts and percentage of counts for variants of interest.
#'
#' @param meta_table   A GISAID variant table read by read.csv.
#' @param host         A string indicating host species.
#' @param country      Country of interest. If NULL, all countries are included.
#' @param plot_counts  A logical. Should daily counts be plotted?
#' @param plot_percent A logical. Should daily percent counts be plotted?
#' @param log          A logical. Should counts be plotted on log scale? Ignored if plot_counts = FALSE.
#' @param title        An optional title string for plots.
#'
#' @return A date frame containing columns: Collection.date, Variant, Count, Total, and Pct
#' 
plot_named_variants <- function(meta_table, 
                                 host = "Human", 
                                 country = "USA", 
                                 plot_counts = TRUE,
                                 plot_percent = TRUE,
                                 log = FALSE, 
                                 title = NULL ) {
  library(tidyverse)

###################################### Helper Functions ####################################  
  
#' plot_pct
#' Plot daily percent of each variant vs date.
#' 
#' @param dfp A dataframe, see below.
#'
#' @return NULL
  plot_pct <- function(dfp) {
    p <- ggplot(dfp) + 
      geom_point(aes(x = Collection.date, y = Pct, color = Variant))

    if(! is.null(title)) {
      p <- p + ggtitle(paste(title, "Percent of Daily Counts", sep = " "))
    }
    
    print(p)
    
    return(NULL)
  }
  
#' plot_cnts
#' Plot daily counts of the variants.
#'
#' @param dfp A data frame, see below
#'
#' @return NULL
  plot_cnts <- function(dfp) {
    p <- ggplot(dfp) 
    if(! log) {
      p <- p +
        geom_point(aes(x = Collection.date, y = Count, color = Variant))
    } else {
      p <- p + 
        geom_point(aes(x = Collection.date, y = log(Count), color = Variant)) 
    }
    
    if(! is.null(title)) {
      p <- p + ggtitle(title)
    }
    
    print(p)
    
    return(NULL)
  }
  
###########################################################################################
  
  # Filter by host. Remove invalid dates and empty lineages.
  # Reformat Location into Country
  df <- meta_table %>% 
    filter(Host == {{ host }}) %>%
    filter(Variant != "") %>%
    # mutate(Variant = ifelse(Variant == "", "Wuhan", Variant)) %>%
    select(Collection.date, Location, Variant) %>% 
    separate(Location, c("Region", "Country", "State"), sep = "\\s*/\\s*", extra = "drop", fill = "right") %>%
    select(-c("Region", "State")) %>%
    mutate(Collection.date = as.Date(Collection.date, format = "%Y-%m-%d")) %>%
    separate(Variant, c("V", "Name"), extra = "drop", fill = "right") %>%
    unite(Variant, V:Name, sep = " ", remove = TRUE) %>%
    mutate(Variant = as.factor(Variant))
  
  # Filter by country and lineages
  # Count by date and lineage
  if(! is.null(country)) {
    df2 <- 
      df %>%
      filter(Country == {{ country }}) %>%
      group_by(Collection.date, Variant) %>%
      count() %>%
      rename(Count = n)
  }
  else {
    df2 <- 
      df %>%
      group_by(Collection.date, Variant) %>%
      count() %>%
      rename(Count = n)
  }
  
  # get totals by date and lineage
  dfp2 <- 
    df2 %>% 
    group_by(Collection.date) %>% 
    summarise(Total = sum(Count)) 
  
  dfp3 <- data.frame(Collection.date = as.Date(integer()),
                     Pango.lineag = character(),
                     Count = integer(),
                     Total = integer(),
                     Pct = double())
  
  # construct a data frame of the counts of lineages and their percents
  for(var in unique(df2$Variant)) {
    dfp4 <- 
      df2 %>% 
      filter(Variant == {{ var }}) %>% 
      full_join(dfp2) %>% 
      mutate(Pct = Count / Total) %>% 
      drop_na()
    
    dfp3 <- rbind(dfp3, dfp4)
  }
  
  dfp3 <- ungroup(dfp3)
  
  if(plot_percent) {
    plot_pct(dfp3)
  }
  
  if(plot_counts) {
    plot_cnts(df2)  
  }
  
  return(dfp3)
}