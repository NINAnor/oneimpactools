#' Extract values of raster using in parallel
#'
#' @param r Stack of `SpatRaster` objects
#' @param sl Vector of step lines, with a column representing the step id
#' (stratum) and a LINESTRING geometry for each step. Can be an `sf` or a
#' `SpatVector` object.
#' @param ws List of
#'
#' @export
parallel_extract <- function(r, sl, ws,
                             step_id = "use_ava_data_animals_id",
                             prefix = "along_"){

  # check number of summaries
  n_summaries <- sapply(ws, length)
  # repeat raster indices
  # not needed, it would require performing the extraction multiple times
  # rasts <- rep(r, n_summaries)
  # summaries
  # we add "mean" to account for the stepID
  ws <- c("mean", unlist(ws))

  # extract values
  extracted_values <- terra::extract(rasts, sl)
  # extracted_values <- terra::extract(r, terra::vect(sl), fun=mean)
  # ext_v <- raster::extract(raster::raster(r), sl)

  # repeat column for which there is more than one summary statistic
  n_summaries <- c(1, n_summaries) # add 1 for ID
  indices <- rep(1:ncol(extracted_values), times = n_summaries)
  ext_val_rep <- extracted_values[,indices]
  # split extracted values by step (column ID automatically created)
  ext_val_split <- split(ext_val_rep, extracted_values$ID)

  # #max slope
  # step_values <- data.frame(step_id = sl$use_ava_data_animals_id)
  #
  # tmp <- unlist(lapply(extracted_values, function(x){ get_summary(matrix(x[,1]), "max")}))
  # step_values$max_slope <- tmp

  #all other variables
  summaries <- as.data.frame(do.call("rbind", lapply(ext_val_split, get_summary, y = ws)))
  names(summaries) <- c(step_id,
                        paste0(prefix, unlist(lapply(strsplit(colnames(ext_val_rep)[-1], split="@"), function(x) x[1])), "_", ws[-1]))

  summaries[,1] <- sl[[step_id]]
  return(summaries)
}

# function to get summary statistics for a list of
# matrices with values extracted from a raster
get_summary <- function(x, y){

  toto <- function(a,b,c){
    if (c[a]=="mean"){ res <- mean(b[,a], na.rm=T)}
    if (c[a]=="max") { res <- max(b[,a], na.rm=T)}
    if (c[a]=="min") { res <- min(b[,a], na.rm=T)}
    return(res)
  }
  res <- unlist(lapply(c(1:ncol(x)), toto, b = x, c = y))
  return(res)
}
