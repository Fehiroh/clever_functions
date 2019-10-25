


# make_ls_df is a function designed to assist in iterative file importation and writing. 
# it creates a data frame that stores filepaths, shortened names, and optionally, names for the files post-write. 
# the inputs for it are filepath,  
#                        pattern,
#                        recursive,
#                       and prefix 
#  filepath, pattern, and recursive all function identically to how they do in list.files()
#  for more documentation use ?list.files

make_ls_df <- function(filepath, pattern = NULL, recursive = FALSE, prefix = NULL) {
  require(dplyr)
  full_names <- as.data.frame(list.files(filepath, pattern, full.names =  TRUE, recursive =  recursive))
  part_names <- as.data.frame(list.files(filepath, pattern, full.names = FALSE, recursive =  recursive))
  ls_df <<- bind_cols(full_names, part_names) 
  names(ls_df) <- c("full_names", "part_names")
  if (!is.null(prefix)){
    ls_df <- ls_df %>% 
      mutate(new_names = paste(prefix, part_names, sep = "_"))
  }
}


# example:

df_names <- make_ls_df(filepath = "C:/Users/afehir/Documents/Documents/BITS/SSE/Fixes for version 2", pattern = ".csv", 
                       prefix = "new")


for (i in df_names$full_names){
  temp_df <- df_names %>% 
    filter()
}



