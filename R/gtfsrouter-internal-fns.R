# Internal gtfsrouter functions called directly here:

rcpp_traveltimes <- utils::getFromNamespace ("rcpp_traveltimes", "gtfsrouter")

convert_start_time_limits <- utils::getFromNamespace ("convert_start_time_limits", "gtfsrouter")

station_name_to_ids <- utils::getFromNamespace ("station_name_to_ids", "gtfsrouter")
