#' Recursively Download a Google Drive Directory
#'
#' @description
#' Navigates through a Google Drive folder hierarchy and replicates the
#' entire directory structure on the local file system. This function
#' identifies subfolders versus files and handles each accordingly by
#' calling itself recursively for nested directories.
#'
#' @param drive_item A `dribble` (Google Drive tibble) representing the
#'   starting folder. This is typically obtained via `googledrive::drive_get()`.
#' @param local_destination_path A character string specifying the local
#'   directory where the folder structure should be mirrored.
#'
#' @return Invisible `NULL`. The function's primary purpose is the
#'   creation of local directories and the downloading of files.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Obtain the folder metadata using a unique ID from a URL
#' folder_url <- "https://drive.google.com/drive/folders/1w7A_p8k9Lq0X..."
#' target_folder <- googledrive::drive_get(googledrive::as_id(folder_url))
#'
#' # Download the entire directory structure
#' download_drive_recursive(target_folder, "downloads/noaa_data")
#' }
download_drive_recursive <- function(drive_item, local_destination_path) {

  # Ensure the local destination directory exists
  fs::dir_create(local_destination_path, recurse = TRUE)

  # List all items contained within the current Google Drive folder
  folder_contents <- googledrive::drive_ls(drive_item)

  # Loop through every item in the folder contents
  for (row_index in seq_len(nrow(folder_contents))) {

    current_item <- folder_contents[row_index, ]

    # Extract metadata using bracketed indexing for stability
    # drive_resource contains the technical details like MIME type
    item_resource <- current_item[["drive_resource"]][[1]]
    item_mime_type <- item_resource[["mimeType"]]
    item_name      <- current_item[["name"]]

    # Define the standard Google Drive folder MIME type
    google_folder_mime_type <- "application/vnd.google-apps.folder"

    if (item_mime_type == google_folder_mime_type) {
      # If the item is a folder, calculate the new local path and recurse
      new_local_subdirectory <- file.path(local_destination_path, item_name)

      download_drive_recursive(
        drive_item = current_item,
        local_destination_path = new_local_subdirectory
      )
    } else {
      # If the item is a file, download it to the current local path
      googledrive::drive_download(
        file = current_item,
        path = file.path(local_destination_path, item_name),
        overwrite = TRUE
      )
    }
  }

  return(invisible(NULL))
}

# TODO: need to remove generate_sst_data() after having real data
generate_sst_data <- function(years_vector) {
  set.seed(42)
  n_years <- length(years_vector)
  months  <- 1:12
  n_total <- n_years * 12
  m_seq   <- 1:n_total

  # Logic: Mean (13) + Seasonality (8) + Warming Trend (0.02/mo) + Random Noise
  values <- 13.0 +
    8 * sin(2 * pi * (m_seq - 4) / 12) +
    0.02 * (m_seq / 12) +
    rnorm(n_total, 0, 1.1)

  df <- data.frame(
    index = "sst",
    year  = rep(years_vector, each = 12),
    month = rep(months, times = n_years),
    value = round(values, 9),
    unit  = "Celsius"
  )

  write.csv(
    df,
    file = file.path(data_destination, "simulated_sst.csv"),
    row.names = FALSE
  )

  return(df)
}
