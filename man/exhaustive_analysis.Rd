% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5_exhaustive_analysis.R
\name{exhaustive_analysis}
\alias{exhaustive_analysis}
\title{Subset master geo-linked WBES dataset based on survey, country, or region.}
\usage{
exhaustive_analysis(
  data_file_path,
  filter_value,
  subset_type,
  html_or_tex,
  output_directory
)
}
\arguments{
\item{data_file_path}{A string of the user's file path to the raw data.}

\item{filter_value}{A string that indicates the value of the country, survey (CountryYear), or region the user wants to analyze.}

\item{subset_type}{A string that indicates the type of subsetting: "survey", "country", or "region".}

\item{output_directory}{A string containing the directory in which the results will be saved.}
}
\value{
All possible analysis (tables and charts from regressions) offered by the wbfirmadaptation package saved in output_directory.
}
\description{
The exhaustive_analysis() function runs all possible analysis offered by the wbfirmadaptation package, saving results in a series of folders within a specified directory.
}
