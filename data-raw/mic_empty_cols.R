# data-raw/mic_empty_cols.R
# code to prepare `mic_empty_cols` data goes here

mic_empty_cols <- c(
  "repYear",
  "repCountry",
  "zoonosis",
  "matrix",
  "sampUnitType",
  "sampStage",
  "sampOrig",
  "sampType",
  "sampContext",
  "sampler",
  "progCode",
  "progSampStrategy",
  "labIsolCode",
  "sampY",
  "sampM",
  "sampD",
  "isolY",
  "isolM",
  "isolD",
  "analysisY",
  "analysisM",
  "analysisD",
  "totUnitsPositive",
  "totUnitsTested",
  "anMethCode",
  "lowest",
  "highest",
  "substance",
  "cutoffValue",
  "MIC"
)

usethis::use_data(mic_empty_cols, overwrite = TRUE)
