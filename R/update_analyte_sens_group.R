#' Update antimicrobial codes and names
#'
#' This function updates the "analytt_sens_group" table in the NORM-VET Database. The variable "update" is by default set to FALSE, so that the user can check the output data before overwriting the existing data in the database. The output of the function is a list containing three elements; the old data, the new data, and the differences between them.
#'
#' @param server Name of the server to connect to
#' @param database Name of the database to fetch data from
#' @param user Username for the database connection
#' @param update Logical, whether or not the data will be sent to the database. Check differences within the data before updating the database by using `update = FALSE`
#'
#' @author Håkon Kaspersen, \email{hakon.kaspersen@@vetinst.no}
#'
#' @export
#' @import dplyr
#' @import tidyr
#' @importFrom DBI dbWriteTable
#' @importFrom getPass getPass
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#'
update_analyte_sens_group <- function(server,
                                      database,
                                      user,
                                      update = FALSE) {

  # Fetch password
  pw <- getPass()

  # Connect to database
  con <- dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = server,
    Database = database,
    UID = user,
    PWD = pw
  )

  old_table <- as_tibble(tbl(con, "analytt_sens_group"))

  new_codes <- data.frame(
    analyttkode_sens = c(
      "080112",
      "080118",
      "080120",
      "080123",
      "080131",
      "080140",
      "080175",
      "080181",
      "080186",
      "080187",
      "080188",
      "08011501",
      "080198",
      "080197",
      "080196"
    )
  ) %>%
    mutate(
      substans = case_when(
        analyttkode_sens == "080112" ~ "Polymyksiner, kolistin",
        analyttkode_sens == "080118" ~ "Nitrofurantoin",
        analyttkode_sens == "080120" ~ "Oksolinsyre",
        analyttkode_sens == "080123" ~ "Amoksicillin + klavulansyre",
        analyttkode_sens == "080131" ~ "Apramycin",
        analyttkode_sens == "080140" ~ "Flavomycin",
        analyttkode_sens == "080175" ~ "Amoksicillin + klavulansyre 2 : 1",
        analyttkode_sens == "080186" ~ "Cefovecin",
        analyttkode_sens == "080187" ~ "Doksysyklin",
        analyttkode_sens == "080188" ~ "Pradofloksacin",
        analyttkode_sens == "08011501" ~ "Sulfametoksasol",
        analyttkode_sens == "080198" ~ "Klaritromycin",
        analyttkode_sens == "080197" ~ "Gamitromycin",
        analyttkode_sens == "080196" ~ "Metronidazol",
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(analyttkode_gruppe = case_when(
      analyttkode_sens == "080120" ~ "Quinolones",
      analyttkode_sens == "080123" ~ "Beta-lactams/penicillins",
      analyttkode_sens == "080131" ~ "Aminoglycosides",
      analyttkode_sens == "080175" ~ "Beta-lactams/penicillins",
      analyttkode_sens == "080186" ~ "Cephalosporins (3rd gen.)",
      analyttkode_sens == "080187" ~ "Tetracyclines",
      analyttkode_sens == "080188" ~ "Quinolones",
      analyttkode_sens == "08011501" ~ "Sulfonamides and Trimethoprims",
      analyttkode_sens == "080198" ~ "Makrolider/Linkosamider",
      analyttkode_sens == "080197" ~ "Makrolider/Linkosamider",
      analyttkode_sens == "080196" ~ "Metronidazol*",
      TRUE ~ substans
    ))

  RESULT_analyttkode_sens_gruppe <- noRmvet::am_groups %>%
    rbind(new_codes) %>%
    # Set a mark on substances that are not grouped into any
    # group
    mutate(analyttkode_gruppe = ifelse(
      substans == analyttkode_gruppe, paste0(substans,"*"), analyttkode_gruppe
    )) %>%
    # Get norwegian names for all classes
    mutate(
      analyttkode_gruppe_nor = case_when(
        analyttkode_gruppe == "Aminoglycosides" ~ "Aminoglykosider",
        analyttkode_gruppe == "Beta-lactams/penicillins" ~ "Beta-laktamer/penicilliner",
        analyttkode_gruppe == "Macrolides/lincosamides" ~ "Makrolider/Linkosamider",
        analyttkode_gruppe == "Cephalosporins (1st gen.)" ~ "Cefalosporiner (1. gen)",
        analyttkode_gruppe == "Cephalosporins (2nd gen.)" ~ "Cefalosporiner (2. gen)",
        analyttkode_gruppe == "Cephalosporins (3rd gen.)" ~ "Cefalosporiner (3. gen)",
        analyttkode_gruppe == "Cephalosporins (4th gen.)" ~ "Cefalosporiner (4. gen)",
        analyttkode_gruppe == "Cephalosporins (5th gen.)" ~ "Cefalosporiner (5. gen)",
        analyttkode_gruppe == "Quinolones" ~ "Kinoloner",
        analyttkode_gruppe == "Carbapenems" ~ "Karbapenemer",
        analyttkode_gruppe == "Amphenicols" ~ "Amfenikoler",
        analyttkode_gruppe == "Steroid antibacterials" ~ "Steroider",
        analyttkode_gruppe == "Tetracyclines" ~ "Tetrasykliner",
        analyttkode_gruppe == "Polymyxins" ~ "Polymyksiner, kolistin*",
        analyttkode_gruppe == "Oxazolidinones" ~ "Oxazolidinoner",
        analyttkode_gruppe == "Ionophores" ~ "Ionoforer",
        analyttkode_gruppe == "Antimycobacterials" ~ "Antimycobakterielle",
        analyttkode_gruppe == "Sulfonamides and Trimethoprims" ~ "Sulfonamider og trimetoprim",
        analyttkode_gruppe == "Glycopeptides" ~ "Glykopeptider",
        analyttkode_gruppe == "Pleuromutilins" ~ "Pleuromutiliner",
        analyttkode_gruppe == "Streptomyciner" ~ "Streptomycin",
        TRUE ~ analyttkode_gruppe
      )
    ) %>%
    # Correct names for substances
    mutate(
      substans = case_when(
        substans == "Ampicillin+amoksicillin" ~ "Ampicillin/Amoksicillin",
        substans == "Cefotaksim+klavulansyre" ~ "Cefotaksim/Klavulansyre",
        substans == "Enrofloksacin(fluorokinolon)" ~ "Enrofloksacin",
        substans == "Neomycin+framycetin" ~ "Neomycin/Framycetin",
        substans == "Quinupristin+dalfopristin" ~ "Quinupristin/Dalfopristin",
        substans == "Sulfonamider" ~ "Sulfametoksasol",
        substans == "Streptomyciner" ~ "Streptomycin",
        substans %in% c("Sulfa+trimetoprim","Trimetoprim+sulfametoksasol") ~ "Sulfametoksasol/Trimetoprim",
        substans == "Ceftolozan+Tazobakatam" ~ "Ceftolozan/Tazobaktam",
        substans == "Piperacillin + tazobaktam" ~ "Piperacillin/Tazobaktam",
        substans == "Polymyksiner, kolistin" ~ "Kolistin",
        substans %in% c("Amoksicillin + klavulansyre","Amoksicillin + klavulansyre 2 : 1") ~ "Amoksicillin/Klavulansyre",
        substans == "Ceftazidim+klavulansyre" ~ "Ceftazidim/Klavulansyre",
        TRUE ~ substans
      )
    ) %>%
    select(analyttkode_sens, substans, analyttkode_gruppe_nor) %>%
    rename("analyttkode_gruppe" = analyttkode_gruppe_nor) %>%
    distinct()

  comp <- old_table %>%
    left_join(
      RESULT_analyttkode_sens_gruppe,
      by = c(
        "analyttkode_sens",
        "substans"
      )
    ) %>%
    filter(analyttkode_gruppe.x != analyttkode_gruppe.y)

  if (update == FALSE) {
    if (nrow(comp) == 0) {
      print("No differences detected, no update needed.")
    } else {
      print(
        "Differences detected, see output and confirm before updating server."
      )
      return(
        list(
          "old_data" = old_table,
          "new_data" = RESULT_analyttkode_sens_gruppe,
          "diff" = comp
        )
      )
    }
  } else {
    print("Updating table in database.")
    dbWriteTable(
      conn = con,
      name = "analytt_sens_group",
      RESULT_analyttkode_sens_gruppe,
      overwrite = TRUE
    )
  }
}
