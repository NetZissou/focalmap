data_hiv_testing_locations <- function(arrow = FALSE) {

  file_csv <-
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "HIV",
      "hiv_columbus_testing_locations.csv"
    )

  file_parquet <-
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "HIV",
      "hiv_columbus_testing_locations.parquet"
    )

  if (arrow) {
    return(arrow::read_parquet(
      file = file_parquet
    ))
  } else {
    return(vroom::vroom(
      file = file_csv
    ))
  }
}

data_food_pantries <- function(arrow = FALSE) {
  file_csv <-
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "Food Pantries",
      "food_pantries_columbus.csv"
    )

  file_parquet <-
    fs::path(
      opioidDashboard::ROOT_PATH,
      "other", "Food Pantries",
      "food_pantries_columbus.parquet"
    )

  if (arrow) {
    return(arrow::read_parquet(
      file = file_parquet
    ))
  } else {
    return(vroom::vroom(
      file = file_csv
    ))
  }
}

data_hepc_treatment <- function(arrow = FALSE) {

  file_csv <-
    fs::path(
      FOCALPipe::ROOT_PATH,
      "other",
      "Doctors and Clinicians",
      "doctors_columbus_hepc.csv"
    )

  file_parquet <-
    fs::path(
      FOCALPipe::ROOT_PATH,
      "other",
      "Doctors and Clinicians",
      "doctors_columbus_hepc.parquet"
    )

  if (arrow) {
    return(arrow::read_parquet(
      file = file_parquet
    ))
  } else {
    return(vroom::vroom(
      file = file_csv
    ))
  }
}
