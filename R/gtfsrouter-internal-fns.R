# Internal gtfsrouter functions called directly here:

rcpp_traveltimes <- utils::getFromNamespace ("gtfsrouter", "rcpp_traveltimes")

convert_start_time_limits <- utils::getFromNamespace ("gtfsrouter", "convert_start_time_limits")

station_name_to_ids <- utils::getFromNamespace ("gtfsrouter", "station_name_to_ids")
