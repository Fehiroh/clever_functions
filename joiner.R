
joiner <- function(x, y, quick = TRUE){
  y_rows <- as.numeric(nrow(y))
  x_rows <- as.numeric(nrow(x))
  if (isTRUE(x_row > y_row)){
    small_rows <- y_row
    small_df <- deparse(substitute(y))
  } else {
    small_rows <- x_row
    small_df <- deparse(substitute(x))
  }
  
  counter <- 1 
  cat("\n")
  for (i in names(x)){
    i_col <- x[, i]
    for(j in names(y)){
      j_col <- y[, j]
      join_check_1 <- all(i_col %in% j_col)
      join_check_2 <- all(j_col %in% i_col)
      
      if (isTRUE(join_check_1) & isTRUE(join_check_2)){
        cat(paste0(i, " from ", deparse(substitute(x)), " and ", j, 
                     " from ",  deparse(substitute(y)),
                     " are identical"), "\n")
        if (isTRUE(quick == TRUE)) {
          return()
        }
      }
      similarity_xy <- as.numeric(length(i_col %in% j_col)) / x_rows * 100 
      similarty_yx  <- as.numeric(length(j_col %in% i_col)) / y_rows * 100 
      similarity <- max(c(similarity_xy, similarty_yx))
      
      if (isTRUE(counter == 1)){
        max_similarity <- similarity
        max_sim_st_x <- i 
        max_sim_st_y <- j
  
      }
      if (isTRUE(similarity > max_similarity)){
        max_similarity <- similarity
        max_sim_st_x <- i 
        max_sim_st_y <- j
      }
      counter <- counter + 1 
    }
  }
  sim_statement_1 <- paste( max_sim_st_x, "from", deparse(substitute(x)), "and",
                         max_sim_st_y , "from",  deparse(substitute(y)),
                         "are the most similar, with a match rate of:", 
                         max_similarity, "percent.")
  sim_statement_2 <- paste("The smallest number of rows (", small_rows, 
                             ") came from:" , small_df)
  cat("\n", sim_statement_1, "\n", sim_statement_2, "\n", sep = "")
}


x <- iris
y <- iris

joiner(x, y, quick = FALSE)
