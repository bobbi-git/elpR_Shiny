# merge selection tables with differing column names, order, and number
# can be in one folder or subfolders
# bje37@cornell.edu
# Updated Jan 2025

# This function will
# - merge selection tables that are in a single folder or subfolders
# - selection tables can have differing columns and column order - the script will standardize across tables
# - this script will merge all selections into one table and will rename the Selection ID so they are all unique
# - This script expects Raven Sound Selection Tables with columns: Selection, Begin Time (s),
#        End Time (s), High Freq (s), Low Freq (s), File Offset (s), Begin Path
# - output file is .txt selection table

#' @name restructure_general_function
#' @title Merge Selection Tables
#' @author Bobbi J. Estabrook <bobbi.estabrook@cornell.edu>
#'
#' @description This function will merge selection tables together. They can be in sub-folders or all in one folder.
#' This function will:
#' \itemize{
#' \item Read all .txt files in the specified directory
#' \item Preserve original selection IDs
#' \item Remove duplicate entries
#' \item Standardize column formats
#' \item Add source file information
#' \item Calculate clock times using selection_datetime function
#' \item Create a new output file with timestamp in name
#' }
#'
#' @param path Character string specifying the directory containing selection table files
#' @param recursive Logical; if TRUE, searches for files recursively in subdirectories
#'
#' @return A data frame containing the merged selection tables with standardized columns:
#' \itemize{
#' \item Selection - New sequential selection numbers
#' \item Original Selection ID - Original selection numbers from source files
#' \item Source Selection Table - Name of the original file
#' \item Begin Time (s) - Start time in seconds
#' \item End Time (s) - End time in seconds
#' \item Low Freq (Hz) - Lower frequency bound
#' \item High Freq (Hz) - Upper frequency bound
#' \item File Offset (s) - Time offset in source file
#' \item Begin Path - Original file path
#' \item Begin File - Filename from Begin Path
#' \item Begin Clock Time - Calculated datetime of the selection
#' }
#'
#' @import dplyr
#' @export
#' @importFrom dplyr bind_rows distinct select everything
#'
#' @note The function expects selection table files in tab-delimited format

restructure_general_function <- function(
    path,
    recursive = TRUE
  ){

  print("Running restructure_general_function...")

  library(dplyr)
  #library(lubridate)

  # Check if path exists
  if (!dir.exists(path)) {
    stop("The specified directory does not exist")
  }

  # Determine which folders to process
  if (recursive) {
    # get immediate subdirectories only (exclude the parent path)
    folders_to_process <- list.dirs(path, full.names = TRUE, recursive = FALSE)
    # remove the parent directory itself if present
    folders_to_process <- folders_to_process[normalizePath(folders_to_process) != normalizePath(path)]
    if (length(folders_to_process) == 0) {
      # no subfolders; fall back to processing the parent directory
      folders_to_process <- path
    }
  } else {
    folders_to_process <- path
  }

  # Define priority order for columns
  priority_cols <- c(
    "Selection",
    "Original Selection ID",
    "Source Selection Table",
    "Begin Time (s)",
    "End Time (s)",
    "Low Freq (Hz)",
    "High Freq (Hz)",
    "File Offset (s)",
    "Begin Path",
    "Begin File"
  )

  output_files <- character()

  for (folder_path in folders_to_process) {
    file_names <- list.files(path = folder_path,
                           pattern = "\\.txt$",
                           recursive = FALSE,
                           full.names = TRUE)

    if (length(file_names) == 0) {
      cat("No .txt files found in", folder_path, "\n")
      next
    }

    # Create empty list for tables
    all_tables <- list()
    valid_tables <- 0

    # Process each file
    for (i in 1:length(file_names)) {
      table <- try(read.table(file_names[i],
                             header = TRUE,
                             sep = "\t",
                             check.names = FALSE,
                             quote = "",
                             fill = TRUE,
                             stringsAsFactors = FALSE))

      if (!inherits(table, "try-error") && nrow(table) > 0) {
        valid_tables <- valid_tables + 1

        # Save original Selection number
        if("Selection" %in% names(table)) {
          table$`Original Selection ID` <- table$Selection
        }

        # Add original filename
        table$`Source Selection Table` <- basename(file_names[i])

        # Convert known numeric columns
        numeric_cols <- c("Begin Time (s)", "End Time (s)",
                          "Low Freq (Hz)", "High Freq (Hz)",
                          "File Offset (s)")

        # Convert columns appropriately
        for(col in names(table)) {
          if(col %in% numeric_cols) {
            table[[col]] <- as.numeric(as.character(table[[col]]))
          } else {
            table[[col]] <- as.character(table[[col]])
          }
        }

        # Extract filename from Begin Path
        if("Begin Path" %in% names(table)) {
          table$`Begin File` <- basename(as.character(table$"Begin Path"))
        }

        # Sort columns: priority columns first, then remaining columns alphabetically
        other_cols <- setdiff(names(table), priority_cols)
        sorted_other_cols <- sort(other_cols)
        all_cols <- c(
          priority_cols[priority_cols %in% names(table)],
          sorted_other_cols
        )

        table <- table[, all_cols]
        all_tables[[valid_tables]] <- table
        cat("File:", basename(file_names[i]), "- rows:", nrow(table), "\n")
      }
    }

    if(valid_tables == 0) {
      next
    }

    # Combine tables
    merged_df <- bind_rows(all_tables)

    # Sort by Begin File and File Offset
    merged_df <- merged_df[order(merged_df$"Begin File", merged_df$"File Offset (s)"),]

    # Add new Selection numbers
    merged_df$Selection <- 1:nrow(merged_df)

    # Move Selection and Original Selection ID to first columns
    merged_df <- merged_df %>%
      select(Selection, `Original Selection ID`, `Source Selection Table`, everything())

    # Create output filename
    output_file <- file.path(folder_path,
                           paste0("combined_selection_tables_",
                                  format(Sys.time(), "%Y%m%d_%H%M%S"),
                                  ".txt"))

    # Write output
    write.table(merged_df,
                output_file,
                sep = "\t",
                row.names = FALSE,
                quote = FALSE,
                fileEncoding = "UTF-8",
                na = "")

    output_files <- c(output_files, output_file)

    # Print summary
    cat("\nOriginal number of rows:", nrow(bind_rows(all_tables)), "\n")
    cat("Final number of rows after removing duplicates:", nrow(merged_df), "\n")
    cat("Number of columns:", ncol(merged_df), "\n")
    cat("\nOutput file saved as:", basename(output_file), "\n")
  }

  if(length(output_files) == 0) {
    stop("No valid tables found to process")
  }

  print("Running restructure_general_function complete.")
  return(output_files)
}


## TO DO
# check for duplicates and remove if desired? No necessary?
# For validation purposes, is it possible to print the total number of rows (excluding headers) that were read in as well as the total number of rows combined in the final table?
# clock time doesn't work
