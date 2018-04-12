#' Checks whether set of given runs is complete.
#' 
#' @param list_of_directories Vector of strings containing path names. Must have "-xy" at the end of each name, with xy beinga one or two digit number.
#' @param runnumbers Vector of integers providing the number that have to be present in \code{list_of_directories}.
#' @return Logical. TRUE if set of numbers at the end of the path names is idential to \code{runnumbers}.
#' @author David Klein
#' @export


emulator_runs_complete <- function(list_of_directories,runnumbers=1:73) {
  # Extract numbers from directory names
  numbers <- gsub(".*-([0-9]{1,2}$)","\\1",list_of_directories)
  # Set of given runs is complete if all numbers extracted from the directory names are in the sequence given below
  check <- setequal(numbers,as.character(runnumbers))
  if(!check) cat("Missing runs:",setdiff(as.character(runnumbers),numbers),"\n")
  return(check)
}
