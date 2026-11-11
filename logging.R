#' Starten logging
#'
#' Open een connectie naar een logfile via sink
#'
#' Schrijft de output stream weg via sink. Standaard gebruikers informatie &
#' sessie info wordt meegelogd
#'
#' @param log_bestand locatie waar het logbestand geschreven naar kan worden.
#' @param sessio_info logical, of de sessie info gelogd moet worden. Default naar FALSE
#' @param log_bij_interactief logical, of we logging aanzetten bij interactief draaien, default is FALSE
#' @param parameters parameters bestand | r lijst die in de logging terecht kunnen komen
#'
#' @returns open logbestand waar stdout naartoe gaat
#' @keywords internal
start_logging_oud <- function(log_bestand = NULL, sessio_info = FALSE, log_bij_interactief = FALSE, parameters = NULL) {
  if (!log_bij_interactief) {
    if (interactive()) {
      return("Er wordt alleen gelogd bij niet interactief gebruik")
    }
  }

  if (is.null(log_bestand)) {
    log_bestand <- file.path(here::here(), "UNTITLED_log.txt")
  }

  if (!dir.exists(dirname(log_bestand))) {
    dir.create(dirname(log_bestand), showWarnings = FALSE)
  }

  log_bestand <- file(log_bestand, open = "wt")

  sink(log_bestand, split = T, type = "output")

  log_message_met_timestamp("Start logging")

  cat("User info\n")
  cat(paste0("Gedraaid door: ", whoami::username(), "\n"))
  cat("\n")

  if (sessio_info) {
    print(utils::sessionInfo())
    cat("\n")
  }

  if (!is.null(parameters)) {
    if (is.character(parameters) & file.exists(parameters)) {
      cat("Overzicht parameters:\n")

      params <- yaml::read_yaml(parameters)

      cat(yaml::as.yaml(params), "\n")
    }
  }
}

#' Stoppen loggen
#'
#' Stopt open sink connectie
#'
#' @param log_bij_interactief logical, of we logging aanzetten bij interactief draaien, default is FALSE
#'
#' @returns sluit de logging en schrijft txt weg
#'
#' @keywords internal
stop_logging_oud <- function(log_bij_interactief = FALSE) {
  if (!log_bij_interactief) {
    if (interactive()) {
      return("Er wordt alleen gelogd bij niet interactief gebruik")
    }
  }

  log_message_met_timestamp("Stop logging")

  sink(type = "output")
}

#' Hulp functie voor formatten loggen
#'
#' @returns formatted string
#' @keywords internal
string_log_item <- function() {
  if (options::opt("logging_aan")) {
    cat(paste0(
      # Lengte van witdh gebruiken om juiste lijn te printen
      "#", paste0(rep("-", getOption("width") - 2), collapse = ""), "#",
      collapse = ""
    ), "\n")
  }
}


#' Hulp functie voor formatten loggen
#'
#' @param message string waaraan timestamp toegevoegd moet worden
#'
#' @returns formatted string
#' @keywords internal
log_message_met_timestamp <- function(message) {
  if (options::opt("logging_aan")) {
    cat(glue::glue("[{format(Sys.time(), format = '%Y-%m-%d %H:%M:%OS4')}] {message}\n\n"))
  }
}

#' Hulp functie voor formatten loggen
#'
#' @param naam naam van de functie
#'
#' @returns formatted string
#' @keywords internal
enter_log_item <- function(naam = NULL) {
  if (options::opt("logging_aan")) {
    if (is.null(naam)) {
      call <- sys.call(-1)

      if (!is.null(call) && length(call) > 0) {
        naam <- as.character(call[[1]])
      } else {
        naam <- "<onbekende functie>"
      }
    }
    string_log_item()
    log_message_met_timestamp(glue::glue("Start draaien van {naam}"))
  }
}

#' Hulp functie voor formatten loggen
#'
#' @returns formatted string
#' @keywords internal
exit_log_item <- function() {
  if (options::opt("logging_aan")) {
    log_message_met_timestamp(glue::glue("Einde van het draaien van deze functie"))

    string_log_item()

    cat("\n")
  }
}

#' Geformateerde tabel maken voor md rendering
#'
#' @param df dataframe dat gerenderd moet worden
#' @param titel optionele titel
#'
#' @returns print een tabel in md formaat
#' @export
#'
#' @examples
#' maak_markdown_tabel(mtcars, titel = "df mtcars")
maak_markdown_tabel <- function(df, titel = NULL) {
  if (!is.null(titel)) {
    cat("\n##", titel, "\n\n")
  }

  if (!is.data.frame(df)) {
    df <- as.data.frame(df)
  }

  # Zet alle kolommen om naar characters zodat we lengtes kunnen meten
  df_karakter <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  # Bepaal de maximale breedte per kolom (kop of inhoud)
  kolom_breedtes <- pmax(nchar(names(df_karakter)), sapply(df_karakter, function(kol) max(nchar(kol), na.rm = TRUE)))

  opvul_kolom <- function(kol, breedte) {
    format(kol, width = breedte, justify = "left")
  }
  df_opgevuld <- mapply(opvul_kolom, df_karakter, kolom_breedtes, SIMPLIFY = FALSE)
  df_opgevuld <- as.data.frame(df_opgevuld, stringsAsFactors = FALSE)
  kopregel <- paste0("| ", paste(mapply(format, names(df_karakter), width = kolom_breedtes, justify = "left"), collapse = " | "), " |")
  scheiding <- paste0("|", paste(sapply(kolom_breedtes, function(b) paste(rep("-", b + 2), collapse = "")), collapse = "|"), "|")
  rijen <- apply(df_opgevuld, 1, function(rij) {
    paste0("| ", paste(rij, collapse = " | "), " |")
  })
  cat("\n")
  cat(kopregel, "\n")
  cat(scheiding, "\n")
  cat(paste(rijen, collapse = "\n"), "\n")
  cat("\n")
}


#' Gebruikte bieb paden loggen
#'
#' @returns gebruikte bieb paden
#' @export
#'
#' @examples
#' log_bieb_paden()
log_bieb_paden <- function() {
  cat(
    "We gebruiken de volgende bieb paden:\n",
    paste(seq_along(.libPaths()), .libPaths(), sep = ". ", collapse = "\n"), "\n\n"
  )
}

#' Hulp functies voor maken verslagbestand
#'
#' @param controle_df df met controles
#' @param log log bestand waaran de df wordt toegevoegd
#'
#' @returns appended log bestand
voeg_controle_toe_aan_verslag <- function(controle_df, log) {
  controle_naam <- as.character(substitute(controle_df))
  if (exists(controle_naam, envir = .GlobalEnv)) {
    controle_df <- bewerk_log(controle_df)

    log <- dplyr::bind_rows(log, controle_df)
    return(log)
  } else {
    return(log)
  }
}

#' Maken verslagbestand
#'
#' Maakt een verslagbestand op basis van het door GTST opgebouwde
#' controlebestand.
#'
#' De functie haalt `controlebestand` op uit de globale omgeving, vervangt
#' ontbrekende waarden door lege tekst en bepaalt indien nodig automatisch
#' de naam van het verslagbestand op basis van de scriptnaam.
#'
#' @param verslagbestand Optionele bestandsnaam voor het verslagbestand.
#'   Indien `NULL`, wordt de naam afgeleid van het actieve script.
#'
#' @return Het controlebestand als data.frame.
#'
#' @export
maak_verslagbestand <- function(verslagbestand = NULL) {

  if (!exists(
    "controlebestand",
    envir = .GlobalEnv
  )) {
    stop(
      "Er is geen GTST logging om een verslagbestand van te maken",
      call. = FALSE
    )
  }

  log_df <- get(
    "controlebestand",
    envir = .GlobalEnv
  )

  # In het kader van leesbaarheid
  log_df[is.na(log_df)] <- ""

  if (is.null(verslagbestand)) {

    verslagbestand <- tryCatch(
      {
        script <- vind_script_naam()

        gsub(
          "\\.r$",
          ".txt",
          script,
          ignore.case = TRUE
        )
      },
      error = function(e) {
        stop(
          "We kunnen geen scriptnaam vinden voor dit verslagbestand",
          call. = FALSE
        )
      }
    )

  } else if (
    tolower(tools::file_ext(verslagbestand)) != "txt"
  ) {

    stop(
      "Het gegeven verslagbestand is geen txt, pas dit aan",
      call. = FALSE
    )
  }

  if (!dir.exists(
    file.path(
      here::here(),
      "verslagen"
    )
  )) {
    stop(
      "Er bestaat geen verslagen dir om het verslagbestand naartoe te schrijven",
      call. = FALSE
    )
  }

  print(
    tibble::as_tibble(
      log_df
    )
  )

  return(log_df)
}

##' gtst controles bestand aanvullen
#'
#' Voeg je eigen logging toe aan het gtst controles bestand
#'
#' Hiermee kan je zelf regels toevoegen aan het verslagbestand.
#' Dit bestand wordt normaliter aangemaakt & aangevuld door bepaalde
#' GTST functies (inlezen koppelen etc). Mocht je zelf iets willen toevoegen,
#' kan dit met deze functie.
#'
#' @param ... regels in formaat 'omschrijving', 'regel1', 'waarde1'
#' @param df optioneel, naam van verslagbestand waar deze regel aan wordt toegevoegd. Default is true.
#' @param witregel logical, of je een witregel toevoegt na je regels
#'
#' @returns aangevulde logging
#' @export
#'
#' @examples
#' vul_verslag_aan(
#'   "omschrijving", "",
#'   "uitleg1", 2,
#'   "uitleg2", 4,
#'   witregel = T
#' )
vul_verslag_aan <- function(..., df = NULL, witregel = TRUE) {
  rijen <- list(...)

  if (!length(rijen) %% 2 == 0) {
    stop("Je log regels zijn oneven, we verwachten en even aantal:
         regel1, waarde1, etc")
  }

  # Juiste waardes uit de ... halen zodat we een logbestand kunnne maken in de vorm
  # Omschrijving1, waarde1
  # Omschrijving2, waarde2
  omschrijving <- unlist(rijen[c(TRUE, FALSE)])
  waarde <- unlist(rijen[c(FALSE, TRUE)])

  rij <- data.frame(
    omschrijving = omschrijving,
    aantal = waarde,
    stringsAsFactors = FALSE
  )

  if (!is.null(df)) {
    schrijf_r_logging(df, rij, witregel = witregel)
  } else {
    schrijf_r_logging("controlebestand", rij, witregel = witregel)
  }
}

#' Start technische logging
#'
#' Start een technisch logbestand voor de huidige uitvoering van een
#' productiescript.
#'
#' Binnen `map` wordt automatisch een submap aangemaakt met de opgegeven
#' `naam`. Iedere uitvoering krijgt in deze submap een afzonderlijk
#' logbestand met de naam van de productiestap, datum en tijd.
#'
#' De opgegeven hoofdmap `map` moet al bestaan. De submap voor de
#' productiestap wordt automatisch aangemaakt indien deze nog niet bestaat.
#'
#' @param map Bestaande map waarin de technische logbestanden worden
#'   opgeslagen.
#' @param naam Naam van de productiestap. Deze naam wordt gebruikt als
#'   submap, als begin van de bestandsnaam en in de header van het
#'   logbestand.
#'
#' @return Onzichtbaar het pad naar het actieve logbestand.
#'
#' @examples
#' logmap <- file.path(
#'   tempdir(),
#'   "Logging"
#' )
#'
#' if (!dir.exists(logmap)) {
#'   dir.create(logmap)
#' }
#'
#' logbestand <- start_logging(
#'   map = logmap,
#'   naam = "Stap02_Presa"
#' )
#'
#' stop_logging()
#'
#' @export
start_logging <- function(
    map,
    naam) {

  if (
    !is.character(map) ||
    length(map) != 1L ||
    is.na(map) ||
    !nzchar(map)
  ) {
    stop(
      "'map' moet een geldig pad zijn.",
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
      "'naam' moet een geldige naam zijn.",
      call. = FALSE
    )
  }

  if (!dir.exists(map)) {
    stop(
      paste0(
        "De opgegeven logmap bestaat niet:\n",
        map
      ),
      call. = FALSE
    )
  }

  starttijd <- Sys.time()

  logmap <- file.path(
    map,
    naam
  )

  if (!dir.exists(logmap)) {

    aangemaakt <- dir.create(
      logmap,
      recursive = FALSE
    )

    if (
      !aangemaakt &&
      !dir.exists(logmap)
    ) {
      stop(
        paste0(
          "Map voor logbestand kon niet worden aangemaakt:\n",
          logmap
        ),
        call. = FALSE
      )
    }
  }

  bestand <- file.path(
    logmap,
    paste0(
      naam,
      "_",
      format(
        starttijd,
        "%Y%m%d_%H%M%S"
      ),
      ".log"
    )
  )

  options(
    GTST.logbestand = bestand,
    GTST.log_starttijd = starttijd,
    GTST.log_naam = naam,
    GTST.log_teller = 0L,
    GTST.log_stack = list(),
    GTST.log_warnings = 0L,
    GTST.log_errors = 0L
  )

  cat(
    paste0(
      "================================================================================\n",
      "GTST PRODUCTIELOG\n",
      "================================================================================\n",
      "\n",
      "Naam       ",
      naam,
      "\n",
      "Start      ",
      format(
        starttijd,
        "%d-%m-%Y %H:%M:%S"
      ),
      "\n",
      "\n",
      "================================================================================\n"
    ),
    file = bestand,
    append = FALSE
  )

  invisible(bestand)
}


schrijf_log <- function(bericht) {

  bestand <- getOption(
    "GTST.logbestand"
  )

  if (
    is.null(bestand) ||
    !is.character(bestand) ||
    length(bestand) != 1L ||
    is.na(bestand) ||
    !nzchar(bestand)
  ) {
    return(
      invisible(NULL)
    )
  }

  stack <- getOption(
    "GTST.log_stack",
    list()
  )

  teller <- getOption(
    "GTST.log_teller",
    0L
  )

  warnings <- getOption(
    "GTST.log_warnings",
    0L
  )

  errors <- getOption(
    "GTST.log_errors",
    0L
  )

  tijd <- format(
    Sys.time(),
    "%H:%M:%S"
  )

  if (grepl("^START ", bericht)) {

    functie <- sub(
      "^START ",
      "",
      bericht
    )

    diepte <- length(stack)

    if (diepte == 0L) {

      teller <- teller + 1L

      nummer <- sprintf(
        "%02d",
        teller
      )

      cat(
        paste0(
          "\n",
          "[",
          nummer,
          "] ",
          functie,
          strrep(
            " ",
            max(
              1L,
              55L -
                nchar(nummer) -
                nchar(functie)
            )
          ),
          tijd,
          "\n\n"
        ),
        file = bestand,
        append = TRUE
      )

    } else {

      nummer <- NA_character_

      inspringing <- paste0(
        strrep(
          "     ",
          diepte
        )
      )

      cat(
        paste0(
          inspringing,
          "[intern] ",
          functie,
          strrep(
            " ",
            max(
              1L,
              48L -
                nchar(functie) -
                nchar(inspringing)
            )
          ),
          tijd,
          "\n\n"
        ),
        file = bestand,
        append = TRUE
      )
    }

    stack[[length(stack) + 1L]] <- list(
      functie = functie,
      starttijd = Sys.time(),
      nummer = nummer
    )

    options(
      GTST.log_stack = stack,
      GTST.log_teller = teller
    )

    return(
      invisible(NULL)
    )
  }

  if (grepl("^EINDE ", bericht)) {

    functie <- sub(
      "^EINDE ",
      "",
      bericht
    )

    diepte <- length(stack)

    if (diepte == 0L) {
      return(
        invisible(NULL)
      )
    }

    item <- stack[[diepte]]

    duur <- round(
      as.numeric(
        difftime(
          Sys.time(),
          item$starttijd,
          units = "secs"
        )
      ),
      2
    )

    if (diepte == 1L) {

      cat(
        paste0(
          "     KLAAR",
          strrep(
            " ",
            49L
          ),
          format(
            duur,
            nsmall = 2
          ),
          " sec\n",
          "\n",
          "================================================================================\n"
        ),
        file = bestand,
        append = TRUE
      )

    } else {

      inspringing <- strrep(
        "     ",
        diepte
      )

      cat(
        paste0(
          inspringing,
          "KLAAR",
          strrep(
            " ",
            15L
          ),
          format(
            duur,
            nsmall = 2
          ),
          " sec\n\n"
        ),
        file = bestand,
        append = TRUE
      )
    }

    stack <- stack[
      -length(stack)
    ]

    options(
      GTST.log_stack = stack
    )

    return(
      invisible(NULL)
    )
  }

  if (grepl("^WARNING ", bericht)) {

    warnings <- warnings + 1L

    inhoud <- sub(
      "^WARNING [^\n]*\n?",
      "",
      bericht
    )

    diepte <- length(stack)

    inspringing <- strrep(
      "     ",
      max(
        1L,
        diepte
      )
    )

    regels <- strsplit(
      inhoud,
      "\n",
      fixed = TRUE
    )[[1L]]

    cat(
      paste0(
        inspringing,
        "WARNING\n",
        inspringing,
        regels[1L],
        "\n"
      ),
      file = bestand,
      append = TRUE
    )

    if (length(regels) > 1L) {

      cat(
        paste0(
          inspringing,
          regels[-1L],
          "\n"
        ),
        file = bestand,
        append = TRUE
      )
    }

    cat(
      "\n",
      file = bestand,
      append = TRUE
    )

    options(
      GTST.log_warnings = warnings
    )

    return(
      invisible(NULL)
    )
  }

  if (grepl("^ERROR ", bericht)) {

    errors <- errors + 1L

    functie <- sub(
      "^ERROR ([^\n]*).*",
      "\\1",
      bericht
    )

    inhoud <- sub(
      "^ERROR [^\n]*\n?",
      "",
      bericht
    )

    diepte <- length(stack)

    if (diepte > 0L) {
      item <- stack[[diepte]]

      duur <- round(
        as.numeric(
          difftime(
            Sys.time(),
            item$starttijd,
            units = "secs"
          )
        ),
        2
      )
    } else {
      duur <- NA_real_
    }

    inspringing <- strrep(
      "     ",
      max(
        1L,
        diepte
      )
    )

    regels <- strsplit(
      inhoud,
      "\n",
      fixed = TRUE
    )[[1L]]

    cat(
      paste0(
        inspringing,
        "ERROR\n",
        inspringing,
        regels[1L],
        "\n"
      ),
      file = bestand,
      append = TRUE
    )

    if (length(regels) > 1L) {

      cat(
        paste0(
          inspringing,
          regels[-1L],
          "\n"
        ),
        file = bestand,
        append = TRUE
      )
    }

    cat(
      paste0(
        "\n",
        inspringing,
        "AFGEBROKEN"
      ),
      file = bestand,
      append = TRUE
    )

    if (!is.na(duur)) {
      cat(
        paste0(
          strrep(
            " ",
            44L
          ),
          format(
            duur,
            nsmall = 2
          ),
          " sec"
        ),
        file = bestand,
        append = TRUE
      )
    }

    cat(
      "\n",
      file = bestand,
      append = TRUE
    )

    if (diepte > 0L) {

      stack <- stack[
        -length(stack)
      ]
    }

    options(
      GTST.log_stack = stack,
      GTST.log_errors = errors
    )

    return(
      invisible(NULL)
    )
  }

  diepte <- length(stack)

  inspringing <- strrep(
    "     ",
    max(
      1L,
      diepte
    )
  )

  cat(
    paste0(
      inspringing,
      bericht,
      "\n"
    ),
    file = bestand,
    append = TRUE
  )

  invisible(NULL)
}


schrijf_log_controle <- function(
    omschrijving,
    waarden,
    duur = NULL) {

  bestand <- getOption(
    "GTST.logbestand"
  )

  if (
    is.null(bestand) ||
    !is.character(bestand) ||
    length(bestand) != 1L ||
    is.na(bestand) ||
    !nzchar(bestand)
  ) {
    return(
      invisible(NULL)
    )
  }

  stack <- getOption(
    "GTST.log_stack",
    list()
  )

  diepte <- length(stack)

  inspringing <- strrep(
    "     ",
    max(
      1L,
      diepte
    )
  )

  namen <- names(
    waarden
  )

  waarden <- as.character(
    waarden
  )

  is_geheel_getal <- grepl(
    "^-?[0-9]+$",
    waarden
  )

  waarden[is_geheel_getal] <- vapply(
    waarden[is_geheel_getal],
    function(x) {
      format(
        as.numeric(x),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        trim = TRUE,
        nsmall = 0
      )
    },
    character(1)
  )

  breedte_naam <- max(
    nchar(namen)
  )

  breedte_waarde <- max(
    nchar(waarden)
  )

  regels <- paste0(
    inspringing,
    format(
      namen,
      width = breedte_naam,
      justify = "left"
    ),
    "     ",
    format(
      waarden,
      width = breedte_waarde,
      justify = "right"
    )
  )

  cat(
    paste0(
      inspringing,
      omschrijving,
      "\n",
      paste(
        regels,
        collapse = "\n"
      ),
      "\n\n"
    ),
    file = bestand,
    append = TRUE
  )

  invisible(NULL)
}


#' Stop technische logging
#'
#' Sluit het actieve technische logbestand af en schrijft een samenvatting
#' van de productie naar het logbestand.
#'
#' @return Onzichtbaar het pad naar het afgesloten logbestand.
#'
#' @examples
#' logmap <- file.path(
#'   tempdir(),
#'   "Logging"
#' )
#'
#' if (!dir.exists(logmap)) {
#'   dir.create(logmap)
#' }
#'
#' start_logging(
#'   map = logmap,
#'   naam = "Test"
#' )
#'
#' stop_logging()
#'
#' @export
stop_logging <- function() {

  bestand <- getOption(
    "GTST.logbestand"
  )

  starttijd <- getOption(
    "GTST.log_starttijd"
  )

  if (
    is.null(bestand) ||
    !is.character(bestand) ||
    length(bestand) != 1L ||
    is.na(bestand) ||
    !nzchar(bestand)
  ) {
    return(
      invisible(NULL)
    )
  }

  eindtijd <- Sys.time()

  duur <- if (!is.null(starttijd)) {
    round(
      as.numeric(
        difftime(
          eindtijd,
          starttijd,
          units = "secs"
        )
      ),
      2
    )
  } else {
    NA_real_
  }

  teller <- getOption(
    "GTST.log_teller",
    0L
  )

  warnings <- getOption(
    "GTST.log_warnings",
    0L
  )

  errors <- getOption(
    "GTST.log_errors",
    0L
  )

  cat(
    paste0(
      "\n",
      "PRODUCTIE VOLTOOID\n",
      "================================================================================\n",
      "\n",
      "Einde                 ",
      format(
        eindtijd,
        "%d-%m-%Y %H:%M:%S"
      ),
      "\n",
      "Totale duur           ",
      format(
        duur,
        nsmall = 2
      ),
      " sec\n",
      "Hoofdaanroepen         ",
      teller,
      "\n",
      "Warnings               ",
      warnings,
      "\n",
      "Errors                 ",
      errors,
      "\n",
      "\n",
      "================================================================================\n"
    ),
    file = bestand,
    append = TRUE
  )

  options(
    GTST.logbestand = NULL,
    GTST.log_starttijd = NULL,
    GTST.log_naam = NULL,
    GTST.log_teller = NULL,
    GTST.log_stack = NULL,
    GTST.log_warnings = NULL,
    GTST.log_errors = NULL
  )

  invisible(bestand)
}

#' Initialiseer productiescript
#'
#' Bereidt de R-omgeving voor op het uitvoeren van een productiescript.
#'
#' Een eventueel nog actieve logging wordt eerst afgesloten. Vervolgens
#' worden objecten die niet tot de standaard startomgeving behoren verwijderd,
#' wordt garbage collection uitgevoerd en wordt een nieuwe technische logging
#' gestart voor het actieve script.
#'
#' De objecten die behouden moeten blijven worden bepaald via `start_params`.
#' De logging wordt geschreven naar `d_Monitor`.
#'
#' @return Onzichtbaar het pad naar het aangemaakte logbestand.
#' @export
#'
#' @examples
#' \dontrun{
#' initialiseer()
#' }
initialiseer <- function() {

  if (!is.null(
    getOption(
      "GTST.logbestand"
    )
  )) {
    stop_logging()
  }

  if (!exists(
    "start_params",
    envir = .GlobalEnv,
    inherits = FALSE
  )) {
    stop(
      "'start_params' is niet beschikbaar. Controleer het .Rprofile.",
      call. = FALSE
    )
  }

  start_params <- get(
    "start_params",
    envir = .GlobalEnv,
    inherits = FALSE
  )

  huidige_objecten <- ls(
    envir = .GlobalEnv,
    all.names = TRUE
  )

  verwijderen <- setdiff(
    huidige_objecten,
    start_params
  )

  if (length(verwijderen) > 0L) {
    rm(
      list = verwijderen,
      envir = .GlobalEnv
    )
  }

  gc()

  if (!exists(
    "d_Monitor",
    envir = .GlobalEnv,
    inherits = FALSE
  )) {
    stop(
      "'d_Monitor' is niet beschikbaar. Controleer het .Rprofile.",
      call. = FALSE
    )
  }

  logmap <- get(
    "d_Monitor",
    envir = .GlobalEnv,
    inherits = FALSE
  )

  logbestand <- start_logging(
    map = logmap,
    naam = vind_script_naam(
      met_extensie = FALSE
    )
  )

  invisible(logbestand)
}

#' Productie afronden
#'
#' Rondt een productiescript af. Indien een dataframe en bestandsnaam worden
#' opgegeven, wordt het resultaat naar Excel geschreven en als historisch
#' CSV-bestand gearchiveerd. Daarna wordt de technische logging afgesloten.
#'
#' Het Excelbestand wordt opgeslagen in `d_Verslagen`. Als het bestand al
#' bestaat, blijven bestaande werkbladen behouden. Het werkblad voor de
#' huidige stap wordt vervangen indien het al bestaat.
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
#' @return Onzichtbaar een lijst met de aangemaakte bestanden en het
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

  scriptnaam <- vind_script_naam(
    met_extensie = FALSE
  )

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

    if (
      tolower(tools::file_ext(naam)) != "xlsx"
    ) {
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

    if (file.exists(excel_pad)) {
      wb <- openxlsx2::wb_load(
        excel_pad
      )
    } else {
      wb <- openxlsx2::wb_workbook()
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
      "Technische logging", "Afgesloten"
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


