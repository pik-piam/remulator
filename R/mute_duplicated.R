#' Set duplicated samples to NA
#'
#' This function sets duplicated samples (per region, year, model, scenario) to NA. This is
#' useful to clean your data before fitting.
#'
#' @param data MAgPIE object containing the samples to remove the duplicates from.
#' @return Magpie object with duplicated samples set to NA.
#' @author David Klein
#' @importFrom magclass getNames as.magpie magpply

mute_duplicated <- function(data) {

  dupli <- unwrap(data)

  # transfer dimension names to unwrapped object
  names(dimnames(dupli)) <- getSets(data)

  dupli <- apply(dupli, c(1, 2, 3), duplicated)
  # put dimenson one (samples) to the end
  dupli <- aperm(dupli, c(2, 3, 4, 1))
  # provide names for samples
  dimnames(dupli)[[4]] <- getNames(data, dim = 2)

  dupli <- as.magpie(dupli)

  # create empty object
  dupli_data <- data + NA
  # pick duplicated data points
  dupli_data[dupli] <- data[dupli]
  # find number of TRUE elements
  dupli_count <- magpply(dupli, sum, c(1, 2, 3.1)) # as.magpie(apply(unwrap(dupli),c(1,2,3),sum))
  # Attach dupli (TRUE/FALSE) as attribute to data before TRUE/FALSE are changed to 1/0
  attr(data, "duplicated_flag") <- dupli
  # Attach dupli_count as attribute to data
  attr(data, "duplicated_count") <- dupli_count
  # Attach dupli_data as attribute to data
  attr(data, "duplicated_data") <- dupli_data

  dupli[dupli] <- NA # duplicated values: TRUE  -> NA
  dupli[!dupli] <- 1 # unique values    : FALSE -> 1

  # set duplicated values in data to NA by multiplying with dupli
  tmp <- data * dupli

  # to keep attributes (would be erased by multiplication)
  data[,,] <- tmp

  return(data)
}
