
#' This function extracts slope parameters for the connection between two lakes from the requisite database.
#'
#' @param parameters_of_interest A vector containing the names of the slope parameters you wish to extract. These can be extracted using the code found in the examples.
#' @param connectivity A matrix which should include the upstream and downstream lake you wish to evalutate and the presence or absence of your target fish in the upstream lake.
#' @param con The connection parameters set to connect to your database
#' @return The connectivity table with slope parameters adjoined.
#' @examples
#' @export
#'
#' # Extract parameters using the following code:
#' parameters <- colnames(tbl(database_folder, "example_connectivity_matrix") %>%
#' head() %>%
#'  collect())
#'
#' # Check parameters you need
#' names(parameters)
#'
#' parameters_of_interest <- parameters[12:15]
#'
#' slope_table <- extract_slope_params(parameters_of_interest, connectivity)

# Define parameters of interest

# parameters_of_interest <- c("upstream_lakes", "upstream_lakes_slope_max_max","upstream_lakes_slope_max", "upstream_lakes_slope_perc_90", "upstream_lakes_slope_perc_90_max")
# con already defined

# Upload the lake connections

# connectivity_raw <- read.csv(file="./slope_analysis/Data/sweden_connections.csv",sep=',',header=T)
# connectivity <- connectivity_raw %>%
#  filter(!is.na(Present))
# head(connectivity)


extract_slope_params <- function(parameters_of_interest, connectivity, con) {
  # Create query

  upstream_slopes_con <- "SELECT * FROM (SELECT lakeid"
  for (i in parameters_of_interest) {
    upstream_slopes_con <- paste(upstream_slopes_con,", CAST(unnest(string_to_array(",i,", \', \')) AS numeric) AS ",i,sep="")
  }
  upstream_slopes_con <- paste(upstream_slopes_con," FROM temporary.connectivity_sweden_full WHERE upstream_lakes != \'\') AS x",sep="")

  # Run query and remove duplicates
  print("Running query")
  upstream_slopes <- get_postgis_query(con,upstream_slopes_con)
  print("Query complete")
  upstream_slopes <- upstream_slopes[!duplicated(upstream_slopes),]


  connectivity_addon <- as.data.frame(matrix(NA,ncol=length(parameters_of_interest)-1,nrow=nrow(connectivity)))
  colnames(connectivity_addon) <- gsub("upstream_lakes_","",parameters_of_interest[-1])

  for ( i in 1:nrow(connectivity)) {
    slope_1conx <- upstream_slopes %>%
      filter(lakeid == connectivity[i,"downstreamLakeID"] &
               upstream_lakes == connectivity[i,"lakeID"])
    connectivity_addon[i,1:ncol(connectivity_addon)] <- slope_1conx[3:(2+ncol(connectivity_addon))]
  }
  connectivity_slopes <- cbind(connectivity, connectivity_addon)



  return(connectivity_slopes)
}

# Test function 1
# upstream_slopes_test <- extract_slope_params(parameters_of_interest, connectivity)

