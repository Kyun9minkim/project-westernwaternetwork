calculate_metrics <- function(demand, shortage) {
  supplied <- demand - shortage
  list(
    max_shortage = max(shortage, na.rm = TRUE),
    volumetric_reliability = sum(pmin(demand, supplied), na.rm = TRUE) / sum(demand, na.rm = TRUE),
    time_reliability = mean(shortage < 1e-6, na.rm = TRUE)
  )
}

