make_output_path <- function(){
    
  # define path - folder with current date
  path = paste0("derived_data/model_output/", Sys.Date())
  
  # if the directory doesn't exist, 
  if (!(exists(path))){
    
    directories_to_move = 
    # first copy all files in this directory into another: model_output_previous
    file.copy(from = list.files("derived_data/model_output", full.names = TRUE), 
              to =  "derived_data/model_output_previous/",
              overwrite = TRUE, recursive = TRUE)
    
    # delete all files which have been copied across - only if they do exist in the other path
    #exists(paste0("derived_data/model_output_previous/", list.files("derived_data/model_output", full.names = FALSE))[1])
    unlink(list.files("derived_data/model_output", full.names = TRUE), recursive = TRUE)
  
    # now create the new folder
    dir.create(path, showWarnings = FALSE)
    
  }
  
  # return the path of the created folder - used in other functions to save ouputs
  return(path)
  
}