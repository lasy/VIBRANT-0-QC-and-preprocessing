get_vibrant_data_dir <- function(){
  str_c(get_VIBRANT_Dropbox_dir(), "90_VIBRANT_consolidated_data/")
}




get_data_dir <- function(data_source = "real"){
  
  if (str_detect(getwd(), "laurasymul"))
    data_dir <- "/Users/laurasymul/OneDrive - UCL/Academia/Research/VIBRANT data UCLouvain/"
  else if (str_detect(getwd(), "vermeren"))
    data_dir <- "/Users/lvermeren/OneDrive - UCL/VIBRANT data UCLouvain/"
  else
    stop("You need to specify the path to the data directory in `R/get_data_dir.R`")
  
  if (data_source == "simulated"){
    data_dir <- str_c(data_dir, "simulated data/")
    data_dir <- fs::dir_ls(data_dir) |> sort(decreasing = TRUE) |> magrittr::extract(1) |> str_c("/")
  }
  else if (data_source == "real")
    data_dir <- str_c(data_dir, "actual data/")
  else
    stop("data_source must be either 'simulated' or 'real'")
  
  data_dir
}



get_output_dir <- function(data_source = "simulated"){
  
  if (str_detect(getwd(), "laurasymul"))
    output_dir <- "/Users/laurasymul/OneDrive - UCL/Academia/Research/VIBRANT data UCLouvain/"
  else if (str_detect(getwd(), "vermeren"))
    output_dir <- "/Users/lvermeren/OneDrive - UCL/VIBRANT data UCLouvain/"
  else
    stop("You need to specify the path to the data directory in `R/get_data_dir.R`")
  
  if (data_source == "simulated"){
    output_dir <- str_c(output_dir, "simulated data/")
    output_dir <- fs::dir_ls(output_dir) |> sort(decreasing = TRUE) |> magrittr::extract(1) |> str_c("/")
  }
  else if (data_source == "real")
    output_dir <- str_c(output_dir, "actual data/01 Preprocessed and QCed/")
  else
    stop("data_source must be either 'simulated' or 'real'")
  
  output_dir
}


