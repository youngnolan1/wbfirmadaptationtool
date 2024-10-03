#' @title Subset master geo-linked WBES dataset based on survey, country, or region.
#'
#' @description
#' The data_subset() function subsets the raw master dataset based on the user's chosen type: 'survey', 'country', or 'region'.
#'
#' @import haven
#' @import dplyr
#'
#' @param data_file_path A string of the user's file path to the raw data.
#' @param filter_value A string that indicates the value of the country, survey (CountryYear), or region the user wants to analyze.
#' @param subset_type A string that indicates the type of subsetting: "survey", "country", or "region".
#'
#' @return The subsetted dataset as a dataframe (save to object and use as input in other wbfirmadaptation functions).
#'
#' @export
data_subset <- function(data_file_path, filter_value, subset_type){

  # Read master geo-linked WBES data
  df <- haven::read_dta(data_file_path) %>%
    dplyr::mutate(idstd = as.numeric(idstd))

  # Subset based on subset_type
  if (subset_type == "survey") {
    subset_df <- df %>%
      dplyr::filter(country == filter_value)
  } else if (subset_type == "country") {
    subset_df <- df %>%
      dplyr::filter(countryname == filter_value)
  } else if (subset_type == "region") {
    subset_df <- df %>%
      dplyr::filter(region == filter_value)
  } else {
    stop("Invalid subset_type. Please choose 'survey', 'country', or 'region'.")
  }

  # Warning message if fewer than 700 observations
  if (nrow(subset_df) < 700) {
    warning(paste("The", subset_type, "has fewer than 700 observations."))
  }

  return(subset_df)
}
