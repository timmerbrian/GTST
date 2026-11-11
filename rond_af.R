#' Productie afronden
#'
#' Rondt een productiescript af. Indien een dataframe en bestandsnaam worden
#' opgegeven, wordt het resultaat naar Excel geschreven en als historisch
#' CSV-bestand opgeslagen. Daarna wordt de technische logging afgesloten.
#'
#' Het Excelbestand wordt opgeslagen in `d_Verslagen`. Als het bestand al
#' bestaat, blijven bestaande werkbladen behouden. Als het werkblad al bestaat,
#' wordt alleen dat werkblad vervangen en blijft de oorspronkelijke positie
#' behouden.
#'
#' De historische CSV-bestanden worden opgeslagen in een submap van
#' `d_Verslagen` met de naam van het actieve script. Iedere uitvoering krijgt
#' een timestamp, zodat eerdere versies behouden blijven.
#'
#' Voor het schrijven van data naar Excel wordt `openxlsx2::wb_add_data()`
#' gebruikt. Aanvullende argumenten in `...` worden rechtstreeks aan deze
#' functie doorgegeven.
#'
#' Indien `df` en `naam` niet worden opgegeven, wordt alleen de technische
#' logging afgesloten.
#'
#' @param df Optioneel data.frame dat moet worden opgeslagen.
#' @param naam Optioneel character met de naam van het Excelbestand,
#'   inclusief `.xlsx`-extensie. Verplicht wanneer `df` is opgegeven.
#' @param stap Optioneel character met de naam van het Excelwerkblad.
#'   Standaard wordt de naam van het actieve script gebruikt.
#' @param ... Aanvullende argumenten voor `openxlsx2::wb_add_data()`,
#'   bijvoorbeeld `col_names = FALSE`.
#'
#' @return Onzichtbaar een lijst met het Excelbestand, CSV-archief en
#'   afgesloten technische logbestand.
#'
#' @examples
#' \dontrun{
#' rond_af()
#'
#' rond_af(
#'   df = eindbestand,
#'   naam = "Productie.xlsx"
#' )
#'
#' rond_af(
#'   df = eindbestand,
#'   naam = "Productie.xlsx",
#'   stap = "Stap02",
#'   col_names = FALSE
#' )
#' }
#'
#' @export
rond_af <- function(
    df = NULL,
    naam = NULL,
    stap = NULL,
    ...) {

  heeft_df <- !is.null(df)
  heeft_naam <- !is.null(naam)

  if (xor(heeft_df, heeft_naam)) {
    stop(
      "'df' en 'naam' moeten samen worden opgegeven.",
      call. = FALSE
    )
  }

  if (
    !is.null(stap) &&
      (
        !is.character(stap) ||
          length(stap) != 1L ||
          is.na(stap) ||
          !nzchar(stap)
      )
  ) {
    stop(
      "'stap' moet een geldige naam zijn.",
      call. = FALSE
    )
  }

  scriptnaam <- getOption(
    "GTST.log_naam"
  )

  if (
    is.null(scriptnaam) &&
      (is.null(stap) || heeft_df)
  ) {
    scriptnaam <- vind_script_naam(
      met_extensie = FALSE
    )
  }

  if (is.null(stap)) {
    stap <- scriptnaam
  }

  excel_pad <- NULL
  csv_pad <- NULL

  if (heeft_df) {

    if (!is.data.frame(df)) {
      stop(
        "'df' moet een data.frame zijn.",
        call. = FALSE
      )
    }

    if (
      !is.character(naam) ||
        length(naam) != 1L ||
        is.na(naam) ||
        !nzchar(naam)
    ) {
      stop(
        "'naam' moet een geldige bestandsnaam zijn.",
        call. = FALSE
      )
    }

    if (tolower(tools::file_ext(naam)) != "xlsx") {
      stop(
        "'naam' moet eindigen op '.xlsx'.",
        call. = FALSE
      )
    }

    if (!exists(
      "d_Verslagen",
      envir = .GlobalEnv,
      inherits = FALSE
    )) {
      stop(
        "'d_Verslagen' is niet beschikbaar. Controleer het .Rprofile.",
        call. = FALSE
      )
    }

    pad_verslagen <- get(
      "d_Verslagen",
      envir = .GlobalEnv,
      inherits = FALSE
    )

    if (
      !is.character(pad_verslagen) ||
        length(pad_verslagen) != 1L ||
        is.na(pad_verslagen) ||
        !nzchar(pad_verslagen)
    ) {
      stop(
        "'d_Verslagen' moet een geldig pad zijn.",
        call. = FALSE
      )
    }

    if (!dir.exists(pad_verslagen)) {
      dir.create(
        pad_verslagen,
        recursive = TRUE
      )
    }

    excel_pad <- file.path(
      pad_verslagen,
      naam
    )

    wb <- if (file.exists(excel_pad)) {
      openxlsx2::wb_load(excel_pad)
    } else {
      openxlsx2::wb_workbook()
    }

    werkbladen <- wb$get_sheet_names()

    if (stap %in% werkbladen) {

      oude_positie <- match(
        stap,
        werkbladen
      )

      wb$remove_worksheet(
        sheet = stap
      )

      wb$add_worksheet(
        sheet = stap
      )

      nieuwe_volgorde <- append(
        werkbladen[
          werkbladen != stap
        ],
        stap,
        after = oude_positie - 1L
      )

      wb$set_order(
        sheets = nieuwe_volgorde
      )

    } else {
      wb$add_worksheet(
        sheet = stap
      )
    }

    wb$add_data(
      sheet = stap,
      x = df,
      ...
    )

    wb$save(
      file = excel_pad,
      overwrite = TRUE
    )

    archief_map <- file.path(
      pad_verslagen,
      scriptnaam
    )

    if (!dir.exists(archief_map)) {
      dir.create(
        archief_map,
        recursive = TRUE
      )
    }

    timestamp <- format(
      Sys.time(),
      "%Y%m%d_%H%M%S"
    )

    csv_pad <- file.path(
      archief_map,
      paste0(
        scriptnaam,
        "_",
        timestamp,
        ".csv"
      )
    )

    utils::write.csv2(
      df,
      file = csv_pad,
      row.names = FALSE
    )

    vul_verslag_aan(
      "Excelbestand", excel_pad,
      "Tabblad", stap,
      "CSV-archief", csv_pad
    )
  }

  logbestand <- getOption(
    "GTST.logbestand"
  )

  if (!is.null(logbestand)) {
    vul_verslag_aan(
      "Technische logging",
      "Afgesloten"
    )
  }

  afgesloten_log <- stop_logging()

  if (heeft_df) {
    message(
      paste0(
        "Productie afgerond.\n",
        "Excel: ",
        excel_pad,
        "\n",
        "Tabblad: ",
        stap,
        "\n",
        "CSV-archief: ",
        csv_pad,
        "\n",
        "Technische logging afgesloten."
      )
    )
  } else {
    message(
      "Productie afgerond. Technische logging afgesloten."
    )
  }

  invisible(
    list(
      excel = excel_pad,
      csv = csv_pad,
      log = afgesloten_log
    )
  )
}
