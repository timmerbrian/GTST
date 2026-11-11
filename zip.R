#' Grotere bestanden unzippen
#'
#' Unzipt een zip file met gekozen bestanden naar een (tijdelijke) locatie
#'
#' Dit een een functie waar je de bestanden kan kiezen uit een bepaalde zip
#' om te unzippen en op een bepaalde plek neer te zetten. Je kan ze vanaf
#' deze plek verder verwerken (inlezen, wegschrijven e.d.). In tegenstelling tot
#' de standaard zipfunctionaliteit in R, gebruikt deze de 7zip.exe. Hierdoor
#' kunnen bestanden unzipt worden op dezelfde manier als in powershell.
#'
#' @param zip_pad locatie van het zip bestand
#' @param te_lezen_bestanden  bestanden die we over willen houden
#' @param exdir locatie waar de ungezipte bestanden heen kunnen
#' @param seven_zip_pad exe die gebruikt wordt om te unzippen
#' @param overwrite T/F, bestanden in de exdir overschrijven
#' @param verbose T/F, extra informatie printen
#'
#' @returns exdir met in te lezen bestanden
#' @export
#'
#' @examples
#' unzip_7z(
#'   zip_pad = "voorbeelddata/test.zip",
#'   te_lezen_bestanden = "help.csv",
#'   verbose = FALSE
#' )
unzip_7z <- function(zip_pad,
                     te_lezen_bestanden = NULL,
                     exdir = tempdir(),
                     seven_zip_pad = "C:/Program Files/7-Zip/7z.exe",
                     overwrite = TRUE,
                     verbose = TRUE) {
  enter_log_item()

  if (!file.exists(zip_pad)) {
    stop(":x: Zip file niet gevonden: ", zip_pad)
  }

  if (!file.exists(seven_zip_pad)) {
    stop(":x: 7z.exe bestaat niet")
  }

  aanwezig_in_zip <- zip::zip_list(zip_pad)$filename

  if (!is.null(te_lezen_bestanden)) {
    niet_in_zip <- te_lezen_bestanden[!te_lezen_bestanden %in% aanwezig_in_zip]

    if (length(niet_in_zip) >= 1) {
      if (length(niet_in_zip) == length(te_lezen_bestanden)) {
        stop("Alle opgegeven bestanden: (", paste0(te_lezen_bestanden, collapse = " en "), ") zitten niet in de zip, we unzippen niet.")
      } else {
        te_lezen_bestanden <- te_lezen_bestanden[te_lezen_bestanden %in% aanwezig_in_zip]
        warning("Let op, de volgende bestanden: ", paste0(niet_in_zip, collapse = " en "), " zitten niet in de zip file.
                We lezen: ", paste0(te_lezen_bestanden, collapse = " en "), " in.")
      }
    }
  }

  args <- c(
    "x",
    shQuote(normalizePath(zip_pad)),
    if (!is.null(te_lezen_bestanden)) shQuote(te_lezen_bestanden),
    paste0("-o", shQuote(normalizePath(exdir)))
  )

  if (overwrite) {
    args <- c(args, "-y")
  }

  # if (verbose) message("7Zip aanroepen: ", zip_pad)
  if (verbose) {
  }

  res <- system2(seven_zip_pad, args, stdout = TRUE, stderr = TRUE)

  if (verbose) {
    log_message_met_timestamp("Output 7z.exe:")
    cat(res, sep = "\n")
  }

  extracted_files <- list.files(exdir, recursive = TRUE, full.names = TRUE)
  exit_log_item()
  return(invisible(extracted_files))
}

#' Gezipte bestanden inlezen
#'
#' Pakt een bestand uit een ZIP-bestand uit naar een tijdelijke locatie en
#' leest het vervolgens direct in met [lees_bestand_in()].
#'
#' Het opgegeven bestand wordt met [unzip_7z()] uitgepakt naar `tempdir()`.
#' Daarna wordt het uitgepakte bestand ingelezen met [lees_bestand_in()].
#' Extra argumenten via `...` worden doorgegeven aan [lees_bestand_in()].
#'
#' @param bestand Character. Pad naar het ZIP-bestand.
#' @param bestand_in_zip Character. Naam of pad van het bestand binnen het
#'   ZIP-bestand dat moet worden ingelezen.
#' @param verbose Logisch. Geeft aan of aanvullende informatie tijdens het
#'   uitpakken moet worden weergegeven. Standaard `FALSE`.
#' @param ... Extra argumenten die worden doorgegeven aan [lees_bestand_in()].
#'
#' @return Het object dat door [lees_bestand_in()] wordt teruggegeven,
#'   doorgaans een `data.frame`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Personen_demo <- lees_bestand_in_uit_zip(
#'   bestand = "voorbeelddata/Invoer.zip",
#'   bestand_in_zip = "Personen.sav",
#'   verbose = FALSE,
#'   n_max = 12
#' )
#' }
lees_bestand_in_uit_zip <- function(
    bestand,
    bestand_in_zip,
    verbose = FALSE,
    ...) {
  unzip_7z(
    zip_pad = bestand,
    te_lezen_bestanden = bestand_in_zip,
    exdir = tempdir(),
    verbose = verbose
  )

  data <- lees_bestand_in(
    file.path(tempdir(), bestand_in_zip),
    ...
  )

  return(data)
}

#' Grote bestanden zippen
#'
#' Bestanden zippen via system en de 7z.exe
#'
#' Gebruikt de 7z.exe om bestanden te zippen naar een bepaald pad.
#'
#' @param bestanden char vector met volledige paden van bestanden die gezipt moeten worden
#' @param zip_bestand pad naar het te maken zip bestand
#' @param seven_zip_pad locatie van de 7z.exe
#' @param verbose logical, of de system output van 7z geprint moet worden
#' @param compression niveau van compressie
#' @param verwijder_bestanden logical, of de originele bestanden die naar de zip gaan verwijderd kunnen worden
#'
#' @returns gezipt bestand
#' @export
#'
#' @examples
#' zip_7z(
#'   bestanden = c("voorbeelddata/test.csv", "voorbeelddata/test.xlsx"),
#'   zip_bestand = "voorbeelddata/test.zip"
#' )
zip_7z <- function(bestanden,
                   zip_bestand,
                   seven_zip_pad = "C:/Program Files/7-Zip/7z.exe",
                   verbose = FALSE,
                   compression = 5,
                   verwijder_bestanden = FALSE) {
  enter_log_item()

  stopifnot(is.character(bestanden), length(bestanden) > 0)
  stopifnot(is.character(zip_bestand), length(zip_bestand) == 1)

  bestanden <- normalizePath(bestanden, winslash = "\\", mustWork = TRUE)
  zip_bestand <- normalizePath(
    dirname(zip_bestand),
    winslash = "\\",
    mustWork = TRUE
  ) |>
    file.path(basename(zip_bestand))

  if (!file.exists(seven_zip_pad)) {
    stop("7z.exe pad niet gevonden: ", seven_zip_pad)
  }

  args <- c(
    "a",
    "-tzip",
    paste0("-mx=", compression),
    shQuote(zip_bestand),
    shQuote(bestanden)
  )

  res <- system2(
    command = seven_zip_pad,
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )

  if (!file.exists(zip_bestand)) {
    stop(
      "7-Zip mislukt:\n",
      paste(res, collapse = "\n")
    )
  }

  if (verbose) {
    log_message_met_timestamp("Output 7z.exe:")
    cat(res, sep = "\n")
  }

  if (verwijder_bestanden) {
    suppressMessages(
      file.remove(bestanden)
    )
  }

  exit_log_item()

  invisible(zip_bestand)
}
