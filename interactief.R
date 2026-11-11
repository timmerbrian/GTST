#' Custom viewer
#'
#' Opent een custom viewer als je op een data.frame klikt in je Environment.
#'
#' De functie overschrijft `utils::View` in de globale omgeving zodat grote
#' datasets sneller bekeken kunnen worden. Data.frames met meer dan 10000
#' rijen worden beperkt tot de eerste 10000 rijen.
#'
#' @return Onzichtbaar `NULL`. De functie maakt een aangepaste `View`-functie
#'   aan in de globale omgeving.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' laad_custom_viewer()
#' }
laad_custom_viewer <- function() {
  custom_view <- function(data) {
    real_view <- get(
      "View",
      envir = as.environment("package:utils")
    )

    aantal_rij <- nrow(data)

    if (aantal_rij > 10000) {
      data <- data[
        1:10000, ,
        drop = FALSE
      ]
    }

    cli::cli_alert_info(
      paste(
        "Je gebruikt een custom view om de data te bekijken.",
        "Dataframes met meer dan 10000 rijen worden afgekapt.",
        "Van deze dataset laten we maximaal de eerste 10000 rijen zien."
      )
    )

    real_view(data)
  }

  assign(
    x = "View",
    value = custom_view,
    envir = globalenv()
  )

  invisible(NULL)
}

#' Todos laten zien in Rstudio
#'
#' Opent todos in je Rstudio markers pane
#'
#' Functie die comments die zich aan een bepaalde naamgeving houden, die je hierbebeden kan zien,
#' in een makkelijk overzicht laat zien. Je ziet per script op welke regel je nog TODOS hebt.
#' De volgende naamgevingen voor commentaar zijn nu ondersteund:
#' 'todo:'
#' 'TODO:'
#' 'REVIEW:'
#' 'REFACTOR:'
#' 'REMOVE:'
#'
#' @returns regels die gemarkeerd zijn in je markers pane
#' @export
#'
#' @examples
#' laat_todos_zien()
laat_todos_zien <- function() {
  todo_strings <- c("todo:", "TODO:", "REFACTOR:", "REVIEW:", "REMOVE:")
  options(todor_patterns = todo_strings)

  if (!exists("setups_map")) {
    bestanden <- list.files(
      here::here(),
      pattern = "\\.R$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )

    setups_map <- bestanden |>
      dirname() |>
      table() |>
      which.max() |>
      names()
  }
  todor::todor(search_path = setups_map)
}

#' Rstudio opschonen
#'
#' Openstaande tabbladen opschonen in Rstudio
#'
#' Sluit alle docs af en opent het rprofile (indien aanwezig).
#' Zo begin je altijd met een schone omgeving als je naar dit project navigeert
#'
#' @returns schoon rstudio profiel
#' @export
#'
#' @examples
#' sluit_docs_en_open_profiel()
sluit_docs_en_open_profiel <- function() {
  sluit_docs()
  open_r_rprofiel()
}

#' Rstudio opschonen
#'
#' Wrapper om rstudioapi alle tabs te sluiten
#'
#' @returns schone env
#' @export
#'
#' @examples
#' sluit_docs()
sluit_docs <- function() {
  if (is_rstudio_actief()) {
    invisible(rstudioapi::executeCommand("closeAllSourceDocs"))
  }
}

# TODO: openen op laatste regel?

#' Open r profiel
#'
#' Kijken of er een .rprofile bestand is en deze openen
#'
#' @returns geopend rprofile in rstudio
#' @export
#'
#' @examples
#' open_r_rprofiel()
open_r_rprofiel <- function() {
  if (is_rstudio_actief()) {
    if (file.exists(".rprofile")) {
      invisible(rstudioapi::navigateToFile(".rprofile", moveCursor = TRUE))
    }
  }
}

#' Opent laatst bewerkte r script
#'
#'
#' @returns opent het laatst bewerkte script in rstudio
#' @export
#'
#' @examples
#' open_laatst_bewerkt_rscript()
open_laatst_bewerkt_rscript <- function() {
  laatse_file <- fs::file_info(
    fs::dir_ls(file.path(here::here()),
      recurse = TRUE,
      regexp = "\\.R$|\\.r$"
    )
  )
  laatse_file <- laatse_file[order(laatse_file$modification_time, decreasing = TRUE), "path"][1, ]
  invisible(
    rstudioapi::navigateToFile(laatse_file$path, line = 1, moveCursor = TRUE)
  )
}

#' Vind string in project
#'
#' Zoek een speciefieke string (regex) en zie waar deze in je project voorkomen
#'
#' @param zoekopdracht te zoeken string
#' @param recursief logical, of je recursief wil zoeken
#'
#' @returns marker object indien de string voorkomt in je project
#' @export
#'
#' @examples
#' vind_string_in_project("steekproefeenheidID")
#' vind_string_in_project("(?i)hoofdletter")
vind_string_in_project <- function(zoekopdracht,
                                   recursief = !is_r_project()
                                   # Recursief kijken als we in een proj zitten, anders niet
) {
  r_bestanden <- list.files(
    path       = here::here(),
    pattern    = "\\.[Rr]$",
    recursive  = recursief,
    full.names = TRUE
  )
  markers <- list()
  for (file in r_bestanden) {
    lines <- tryCatch(readLines(file, warn = FALSE), error = function(e) character(0))
    matches <- grep(zoekopdracht, lines, perl = TRUE)
    for (line_num in matches) {
      col <- regexpr(zoekopdracht, lines[line_num], perl = TRUE)[[1]]
      markers <- c(markers, list(
        list(
          type    = "info",
          file    = file,
          line    = line_num,
          column  = as.integer(col),
          message = paste0("Match: ", trimws(lines[line_num]))
        )
      ))
    }
  }
  if (length(markers) == 0) {
    message("Bericht:  ", zoekopdracht, " komt niet voor")
    return(invisible(NULL))
  }
  rstudioapi::sourceMarkers(
    name = paste0("Resultaten voor: ", zoekopdracht),
    markers = markers,
    basePath = here::here(),
    autoSelect = "first"
  )
  invisible(markers)
}
# TODO: afkappen bij de veel bestanden | matches?

#' Open bestand
#'
#' @param bestand naam van bestand
#'
#' @returns geopend bestand met de geassocieerde applicatie
#' @export
#'
#' @examples
#' open_bestand("voorbeelddata/test.csv")
open_bestand <- function(bestand) {
  if (is_r_project()) {
    bestand <- file.path(here::here(), bestand)
  }
  shell.exec(bestand)
}

#' Vind string addin
#'
#' Start een interactieve addin waarmee binnen het huidige project
#' naar een opgegeven tekst kan worden gezocht.
#'
#' @returns Een Shiny-gadget. De functie wordt aangeroepen vanwege
#'   het interactieve effect en heeft geen relevante retourwaarde
#'   voor normaal gebruik.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' vind_string_addin()
#' }
vind_string_addin <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Zoek op string in project"),
    miniUI::miniContentPanel(
      shiny::textInput("string", "Kies een string die je wil zoeken"),
      shiny::actionButton("zoek", "zoek")
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$zoek, {
      GTST::vind_string_in_project(input$string)
      shiny::stopApp()
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  viewer <- shiny::dialogViewer(
    "Zoek op string",
    width = 400,
    height = 100
  )

  shiny::runGadget(
    ui,
    server,
    viewer = viewer
  )
}

#' Rprofiel aanmaken met GTST settings
#'
#' Maakt of vult een rprofiel aan met een optionele R-bibliotheek uit GTST_LIBRARY_PATH
#'
#' @returns .rprofile-bestand in de projectmap; zonder vaste interne paden.
#' @export
#'
#' @examples
#' maak_gtst_rprofile()
maak_gtst_rprofile <- function() {
  r_profiel <- file.path(here::here(), ".rprofile")

  # Gebruik alleen een door de gebruiker ingestelde bibliotheeklocatie.
  basis_profiel <- c(
    "# GTST: optionele bibliotheek uit GTST_LIBRARY_PATH",
    'gtst_bibliotheek <- Sys.getenv("GTST_LIBRARY_PATH", unset = "")',
    "if (nzchar(gtst_bibliotheek) && dir.exists(gtst_bibliotheek)) {",
    "  .libPaths(c(gtst_bibliotheek, .libPaths()))",
    "}",
    "rm(gtst_bibliotheek)",
    ""
  )

  if (file.exists(r_profiel)) {
    tekst <- readLines(r_profiel)

    vraag_rprofiel_toestemming("Je hebt al een rprofiel staan, deze wordt aangevuld met de GTST settings")

    writeLines(
      c(tekst, basis_profiel),
      r_profiel
    )
  } else {
    vraag_rprofiel_toestemming()

    writeLines(
      basis_profiel,
      r_profiel
    )
  }
}

#' Interne hulp functie
#'
#' @param bericht extra msg om verschil tussen bestaand en niet bestaand profiel mee te geven
#'
#' @returns stopt aanmaken rprofiel als de gebruiker dat wil
vraag_rprofiel_toestemming <- function(bericht = "") {
  cli::cli_alert(
    paste0("Let op, we maken een rprofiel aan op: ", here::here(), ".\n", bericht)
  )

  antwoord <- readline("Is dit oke? (y/n)")

  if (tolower(antwoord) == "y") {
    invisible(return(NULL))
  } else {
    stop("Profiel niet gemaakt vanwege user input")
  }
}

#' Maak kruistabel addin
#'
#' @returns geprinte kruistabel
#' @export
#'
#' @examples
#' maak_kruistabel_addin()
# package-interne state om dubbele registratie te voorkomen
maak_kruistabel_addin <- function() {
  .pkg_env <- new.env(parent = emptyenv())

  if (isTRUE(.pkg_env$actief)) {
    message("Er draait al een kruistabel-dashboard. Sluit dat eerst af (Done-knop) voordat je een nieuwe start.")
    return(invisible(NULL))
  }
  port <- httpuv::randomPort()
  env_file <- tempfile(fileext = ".RData")
  config_file <- tempfile(fileext = ".rds")
  boot_file <- tempfile(fileext = ".R")
  stop_file <- tempfile(fileext = ".stop")
  save_env <- function() {
    objs <- ls(envir = .GlobalEnv)
    if (length(objs) > 0) {
      save(list = objs, file = env_file, envir = .GlobalEnv)
    } else {
      save(list = character(0), file = env_file)
    }
  }
  save_env() # eerste snapshot, direct beschikbaar voor de app
  # zelf-herplannende timer: elke 10s de env wegschrijven, tot stop-signaal
  schedule_save <- function() {
    if (file.exists(stop_file)) {
      unlink(stop_file)
      unlink(env_file)
      unlink(config_file)
      unlink(boot_file)
      .pkg_env$actief <- FALSE
      message("Kruistabel-dashboard gestopt.")
      return(invisible(NULL))
    }
    tryCatch(save_env(), error = function(e) {
      message("Kruistabel-sync mislukt: ", conditionMessage(e))
    })
    later::later(schedule_save, delay = 10)
  }
  later::later(schedule_save, delay = 10)
  .pkg_env$actief <- TRUE
  saveRDS(
    list(port = port, env_file = env_file, stop_file = stop_file),
    config_file
  )
  app_path <- gsub("\\\\\\\\", "//", system.file("app", "app.r", package = "GTST"))
  writeLines(sprintf(
    'config <- readRDS("%s")\nsource("%s", local = TRUE)',
    gsub("\\\\", "\\\\\\\\", config_file), app_path
  ), boot_file)
  job_id <- rstudioapi::jobRunScript(
    path       = boot_file,
    name       = "Kruistabel dashboard",
    workingDir = getwd()
  )
  url <- sprintf("http://127.0.0.1:%d", port)

  Sys.sleep(10)

  rstudioapi::viewer(url)
  rstudioapi::executeCommand("activateConsole")

  invisible(job_id)
}
