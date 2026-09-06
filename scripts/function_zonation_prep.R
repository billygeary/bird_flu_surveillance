# Modified functions from Zonation R to allow for condition layers

condition_link_file = function(condition_file, link_number= 1, rescale = 0){
  filename <- "condition_link_file.txt"
  recursive <- TRUE
  
  if (missing(condition_file)) {
    stop("condition file is required and must be specified.")
  }
  con <- file(filename, open = "wt")
  writeLines(paste(link_number, rescale, condition_file), con)
  close(con)
  message("Condition link file ", filename, " has been created.")
}

feature_list_mod = function (spp_file_dir, weight = NULL, group = NULL, threshold = NULL, condition = FALSE) 
{
  filename <- "feature_list.txt"
  recursive <- TRUE
  spp_file_pattern <- ".+\\.(tif|tiff|img|asc)$"
  target_rasters <- list.files(path = spp_file_dir, pattern = spp_file_pattern, 
                               full.names = TRUE, recursive = recursive)
  if (length(target_rasters) == 0) {
    stop("No raster files (.tif, .tiff, .img, or .asc) were found in the '", 
         spp_file_dir, "' folder.")
  }
  feature_list <- data.frame(filename = target_rasters)
  if (!is.null(weight)) 
    feature_list$weight <- weight
  if (!is.null(group)) 
    feature_list$group <- group
  if (!is.null(threshold)) 
    feature_list$threshold <- threshold
  if (condition==TRUE){ 
    feature_list$condition <- 1} else{feature_list$condition <- 0} 
  
  colnames(feature_list) <- paste0("\"", colnames(feature_list), 
                                   "\"")
  write.table(feature_list, file = filename, row.names = FALSE, 
              quote = FALSE, col.names = TRUE)
  message("Feature list ", filename, " has been created.")
}



settings_file_mod = function (feature_list_file, 
                          external_solution_file = NULL, 
                          analysis_area_mask_layer = NULL, 
                          hierarchic_mask_layer = NULL, 
                          condition_link_file = NULL,
                          cost_layer = NULL) 
{
  filename <- "settings_file.z5"
  if (missing(feature_list_file)) {
    stop("feature_list file is required and must be specified.")
  }
  con <- file(filename, open = "wt")
  writeLines(paste("feature list file =", feature_list_file), 
             con)
  if (!is.null(external_solution_file)) {
    writeLines(paste("external solution =", external_solution_file), 
               con)
  }
  if (!is.null(analysis_area_mask_layer)) {
    writeLines(paste("analysis area mask layer =", analysis_area_mask_layer), 
               con)
  }
  if (!is.null(hierarchic_mask_layer)) {
    writeLines(paste("hierarchic mask layer =", hierarchic_mask_layer), 
               con)
  }
  if (!is.null(condition_link_file)) {
    writeLines(paste("condition link file =", condition_link_file), 
               con)
  }
  if (!is.null(cost_layer)) {
    writeLines(paste("cost layer =", cost_layer), con)
  }
  close(con)
  message("Settings file ", filename, " has been created.")
}

command_file_mod = function (os = "os_detection", zonation_path, flags = "", marginal_loss_mode = "CAZ2", 
          gui_activated = FALSE, settings_file = "settings_file.z5", 
          output_dir = "output") 
{
  command_file <- "command_file"
  results_directory <- output_dir
  if (missing(zonation_path)) {
    stop("'zonation_path' must be provided.")
  }
  if (os == "os_detection") {
    os <- if (.Platform$OS.type == "windows") 
      "Windows"
    else "Linux"
  }
  allowed_modes <- c("CAZ1", "CAZ2", "ABF", "CAZMAX", "LOAD", 
                     "RAND")
  if (!marginal_loss_mode %in% allowed_modes) {
    stop("'marginal_loss_mode' must be one of: ", paste(allowed_modes, 
                                                        collapse = ", "), ".")
  }
  if (!file.exists(settings_file)) {
    stop("Settings file not found: '", settings_file, "'.")
  }
  if (flags != "") {
    allowed_flags <- c("a", "w", "g", "h", "x", "X", "t", "c")
    flag_chars <- strsplit(flags, "")[[1]]
    invalid_flags <- setdiff(flag_chars, allowed_flags)
    if (length(invalid_flags) > 0) {
      stop("Invalid analysis option flag(s): ", paste(invalid_flags, 
                                                      collapse = ", "), ". Flags activate analysis options and must be one or more of: ", 
           paste(allowed_flags, collapse = ", "), ".")
    }
  }
  if (os == "Windows" && !grepl("\\.cmd$", command_file)) {
    command_file <- paste0(command_file, ".cmd")
  }
  else if (os == "Linux" && !grepl("\\.sh$", command_file)) {
    command_file <- paste0(command_file, ".sh")
  }
  if (os == "Windows") {
    command_template <- paste0("@setlocal\n", "@PATH=", zonation_path, 
                               ";%PATH%\n", "z5")
  }
  else if (os == "Linux") {
    command_template <- paste0("#!/bin/sh\n", "export PATH=", 
                               zonation_path, ":$PATH\n", "zonation5")
  }
  else {
    stop("Unknown value for 'os'. Please use 'Windows' or 'Linux'.")
  }
  if (flags != "") {
    command_template <- paste0(command_template, " -", flags)
  }
  command_template <- paste0(command_template, " --mode=", 
                             marginal_loss_mode)
  if (gui_activated) {
    command_template <- paste0(command_template, " --gui")
  }
  command_template <- paste0(command_template, " ", settings_file, 
                             " ", results_directory)
  writeLines(command_template, command_file)
  if (os == "Linux") {
    Sys.chmod(command_file, mode = "0755")
  }
  message("Command file created: ", command_file)
}

