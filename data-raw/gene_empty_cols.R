# data-raw/gene_empty_cols.R
# code to prepare `gene_empty_cols` data goes here

gene_empty_cols <- c(
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
  "seqTech"
)

usethis::use_data(gene_empty_cols, overwrite = TRUE)
