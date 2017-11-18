compare_cols <- function(df) {
  # Fraction of non-missing rows that agree
  
  # Check data type
  df_class <- unique(unlist(sapply(df,class)))
  
  if (length(df_class)>1){
    stop("Variables not of same class")
  }
  
  # Only use non NA rows
  row_keep <- !(is.na(df[1]) | is.na(df[2]))
  
  if (any(row_keep)) {
    df_keep <- df[row_keep,]
    
    # Comparison is different depending on the class
    # In all cases diff is a number between 0 and 1
    # with smaller number indicating less difference
    
    if (df_class %in% c("integer","double")) {
      # Average absolute difference
      diff <- mean(abs(df_keep[,1] - df_keep[,2]))
      
      
    } else if(df_class %in% c("Date","POSIXct","POSIXt")) {
      # Average difference in days
      diff <- mean(as.numeric(as.period(interval(as.Date(df_keep[,1]), 
                                                 as.Date(df_keep[,2])), 
                                        "days"), "days"))
      
    } else if(df_class %in% "character") {
      # Fraction do not agree
      diff <- mean(df_keep[,1] != df_keep[,2])
      
    } else {
      stop("Variable class not recognized")
    }
  } else {
    diff <- 0
  }
  
  return(diff)
  
}


compare_df <- function(df1, df2, by){
  # Dark magic
  by <- enquo(by)
  
  # Compares two dataframes on specified columns
  out <- list()
  
  # Compare columns
  out[["cols_in1not2"]] <- setdiff(colnames(df1),colnames(df2))
  out[["cols_in2not1"]] <- setdiff(colnames(df2),colnames(df1))
  out[["cols_in1and2"]] <- intersect(colnames(df1),colnames(df2))
  
  # Subset on shared rows and select columns
  row_keep <- base::intersect(select(df1,!!by)[[1]],select(df2,!!by)[[1]])
  out[["n_rows_compare"]] <- length(row_keep)
  
  df1 <- df1 %>%
    filter((!!by) %in% row_keep) %>%
    arrange(!!by)
  
  df2 <- df2 %>%
    filter((!!by) %in% row_keep) %>%
    arrange(!!by)
  
  # Convert time objects to date only
  df1 <- df1 %>% mutate_if(is.POSIXt, funs(as.Date))
  df2 <- df2 %>% mutate_if(is.POSIXt, funs(as.Date))
  
  out[["diff"]] <- sapply(out[["cols_in1and2"]], function(x) 
    compare_cols(cbind(
      df1[x],
      df2[x]
    )))
  
  return(out)
}

days_between <- function(d1,d2) {
  as.numeric(as.period(interval(as.Date(d1), as.Date(d2)), "days"), "days")
}