#' Verander kolomnaan
#'
#' Verander een kolomnaam in een df van oud naar nieuw
#'
#' Verandert kolomnamen in de vorm oud > nieuw. De functie is hoofdlettergevoelig.
#' In het geval dat de oude naam niet voorkomt krijg je hier een melding van.
#'
#' @param df data.frame object waar je de kolomnaam van wil veranderen
#' @param oud oude naam
#' @param nieuw nieuwe naam
#'
#' @returns data.frame object
#' @export
#'
#' @examples
#' verander_kolomnaam(mtcars, "mpg", "milespergallon")
verander_kolomnaan <- function(df, oud, nieuw) {
  if (!is.data.frame(df)) {
    stop("Geef een dataframe op als input")
  }

  if (!oud %in% colnames(df)) {
    stop("Kolom: ", oud, " zit niet in je data")
  }

  colnames(df)[colnames(df) == oud] <- nieuw
  return(df)
}

#' Sorteer kolomvolgorde
#'
#' Functie die de kolomvolgorde aanpast door voor kolommen de nieuwe index aan te geven
#'
#' Sorteert de kolomvolgorde van een data.frame. Je geeft de nieuwe posities aan
#' van kolommen waarvan je de plek wil veranderen. Van kolomnamen waar je geen nieuwe
#' plek van opgeeft, wordt de positie behouden
#'
#' @param df data.frame like object waarvan de kolomvolgorde aangepast dient te worden
#' @param volgorde kolom & positie die je wil wijzigen, e.g.: c(mpg = 2, vs = 9)
#'
#' @returns data.frame like object met gesorteerde kolomvolgorde
#' @export
#'
#' @examples
#' sorteer_kolomvolgorde(mtcars, c(mpg = 4, carb = 1))
sorteer_kolomvolgorde <- function(df, volgorde) {
  alle_kolommen <- colnames(df)
  n_kolommen <- length(alle_kolommen)
  ongeldige_namen <- setdiff(names(volgorde), alle_kolommen)
  if (length(ongeldige_namen) > 0) {
    stop("Ongeldige kolomnamen: ", paste(ongeldige_namen, collapse = ", "))
  }

  ongeldige_posities <- volgorde[volgorde < 1 | volgorde > n_kolommen]
  if (length(ongeldige_posities) > 0) {
    stop(
      "Ongeldige posities (buiten bereik 1-", n_kolommen, "): ",
      paste(ongeldige_posities, collapse = ", ")
    )
  }

  overige <- alle_kolommen[!alle_kolommen %in% names(volgorde)]
  resultaat <- overige

  for (kol in names(volgorde)[order(volgorde)]) {
    pos <- volgorde[[kol]]
    resultaat <- append(resultaat, kol, after = pos - 1)
  }

  df[, resultaat, drop = FALSE]
}

#' Namenlijstselectie toepassen
#'
#' Selecteert variabelen op basis van een namenlijst en hernoemt
#' de geselecteerde Blaisevariabelen naar de opgegeven R-namen.
#'
#' Variabelen uit de dataset die niet in de namenlijst voorkomen worden
#' niet opgenomen in het resultaat. Deze variabelen kunnen optioneel naar
#' een CSV-bestand worden geschreven.
#'
#' Na de selectie wordt, indien beschikbaar, metadata van de oorspronkelijke
#' dataset gekoppeld aan het resultaat.
#'
#' @param data Dataframe waarop de selectie wordt toegepast.
#' @param bestand Pad naar het Excelbestand met worksheet `namen`.
#' @param niet_meegenomen_bestand Optioneel CSV-pad waarin variabelen
#'   worden geschreven die niet zijn geselecteerd.
#' @param omschrijving Optionele omschrijving voor het controlebestand.
#'   Indien niet opgegeven wordt `"Namenlijstselectie toepassen"` gebruikt.
#' @param ... Eventuele extra parameters die worden doorgegeven aan
#'   `koppel_meta()`.
#'
#' @return Het geselecteerde en hernoemde dataframe.
#'
#' @examples
#' bestand <- file.path(
#'   tempdir(),
#'   "namenlijst_selectie.xlsx"
#' )
#'
#' namenlijst <- data.frame(
#'   Blaisenamen = c("A", "B"),
#'   Rnamen = c("variabele_a", "variabele_b")
#' )
#'
#' openxlsx::write.xlsx(
#'   namenlijst,
#'   bestand,
#'   sheetName = "namen"
#' )
#'
#' data <- data.frame(
#'   A = 1:3,
#'   B = 4:6,
#'   C = 7:9
#' )
#'
#' selectie <- pas_selectie_toe(
#'   data,
#'   bestand
#' )
#'
#' names(selectie)
#'
#' @export
pas_selectie_toe <- function(
    data,
    bestand,
    niet_meegenomen_bestand = NULL,
    omschrijving = NULL,
    ...) {

  starttijd <- Sys.time()

  data_naam <- deparse(
    substitute(data)
  )

  if (is.null(omschrijving)) {
    omschrijving <- paste0(
      "Namenlijst ",
      basename(bestand),
      " toepassen op ",
      data_naam
    )
  }

  schrijf_log(
    "START pas_selectie_toe"
  )

  resultaat <- tryCatch(

    withCallingHandlers(

      {

        if (!is.data.frame(data)) {
          stop(
            "'data' moet een dataframe zijn.",
            call. = FALSE
          )
        }

        selectie <- lees_namenlijst(
          bestand,
          dubbele_r_fout = TRUE,
          dubbele_uitgeklapte_r_fout = TRUE
        )

        Blaisenamen <- selectie$Blaisenamen
        Rnamen <- selectie$Rnamen

        missende_Blaisenamen <- setdiff(
          Blaisenamen,
          names(data)
        )

        if (length(missende_Blaisenamen) > 0L) {
          stop(
            paste0(
              "De volgende Blaisenamen zijn niet aanwezig in de dataset:\n",
              paste(
                paste0(
                  "- ",
                  missende_Blaisenamen
                ),
                collapse = "\n"
              )
            ),
            call. = FALSE
          )
        }

        niet_meegenomen <- setdiff(
          names(data),
          Blaisenamen
        )

        if (!is.null(niet_meegenomen_bestand)) {

          if (
            !is.character(
              niet_meegenomen_bestand
            ) ||
            length(
              niet_meegenomen_bestand
            ) != 1L ||
            is.na(
              niet_meegenomen_bestand
            ) ||
            !nzchar(
              niet_meegenomen_bestand
            )
          ) {
            stop(
              paste0(
                "niet_meegenomen_bestand moet ",
                "een geldig bestandspad zijn."
              ),
              call. = FALSE
            )
          }

          if (
            tolower(
              tools::file_ext(
                niet_meegenomen_bestand
              )
            ) != "csv"
          ) {
            stop(
              "niet_meegenomen_bestand moet een .csv-bestand zijn.",
              call. = FALSE
            )
          }

          output_map <- dirname(
            niet_meegenomen_bestand
          )

          if (!dir.exists(output_map)) {
            stop(
              paste0(
                "Map voor niet_meegenomen_bestand bestaat niet:\n",
                output_map
              ),
              call. = FALSE
            )
          }

          niet_meegenomen_df <- data.frame(
            variabele = niet_meegenomen,
            stringsAsFactors = FALSE
          )

          utils::write.csv(
            niet_meegenomen_df,
            niet_meegenomen_bestand,
            row.names = FALSE,
            na = ""
          )
        }

        data_selectie <- data[
          ,
          Blaisenamen,
          drop = FALSE
        ]

        names(
          data_selectie
        ) <- Rnamen

        data_selectie <- koppel_meta(
          dataset = data_selectie,
          verplicht = FALSE,
          dataset_naam = data_naam,
          interne_aanroep = TRUE,
          ...
        )

        meta_controle <- attr(
          data_selectie,
          "meta_controle"
        )

        attr(
          data_selectie,
          "meta_controle"
        ) <- NULL

        rij <- data.frame(
          omschrijving = c(
            omschrijving,
            "Aantal variabelen voor selectie",
            "Aantal ingeklapte namen in namenlijst",
            "Aantal uitgeklapte namen in namenlijst",
            "Aantal variabelen niet meegenomen",
            "Aantal variabelen hernoemd",
            names(meta_controle)
          ),
          aantal = c(
            "",
            ncol(data),
            nrow(selectie$compact),
            length(Blaisenamen),
            length(niet_meegenomen),
            sum(Blaisenamen != Rnamen),
            unname(meta_controle)
          ),
          stringsAsFactors = FALSE
        )

        schrijf_r_logging(
          "controlebestand",
          rij
        )

        duur <- round(
          as.numeric(
            difftime(
              Sys.time(),
              starttijd,
              units = "secs"
            )
          ),
          2
        )

        schrijf_log_controle(
          omschrijving = omschrijving,
          waarden = stats::setNames(
            rij$aantal[-1L],
            rij$omschrijving[-1L]
          ),
          duur = duur
        )

        data_selectie
      },

      warning = function(w) {

        schrijf_log(
          paste0(
            "WARNING pas_selectie_toe\n",
            conditionMessage(w)
          )
        )
      }
    ),

    error = function(e) {

      schrijf_log(
        paste0(
          "ERROR pas_selectie_toe\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE pas_selectie_toe"
  )

  resultaat
}

#' Namenlijst maken
#'
#' Maakt of controleert een Excel-namenlijst met Blaisenamen en
#' bijbehorende R-namen.
#'
#' Als het werkbestand al bestaat, wordt de bestaande namenlijst
#' gecontroleerd. Als een bestaand bronbestand is opgegeven, wordt dit
#' gekopieerd naar het werkbestand en gecontroleerd. Anders wordt een
#' nieuwe namenlijst gemaakt op basis van de opgegeven namen.
#'
#' @param werkbestand Pad naar het werkbestand dat moet worden gemaakt
#'   of gecontroleerd.
#' @param namen Optionele character-vector met Blaisenamen. Wordt gebruikt
#'   wanneer geen bestaand werkbestand of bronbestand beschikbaar is.
#' @param bronbestand Optioneel bestaand namenbestand dat als basis
#'   voor het werkbestand wordt gekopieerd.
#' @param omschrijving Optionele omschrijving voor het controlebestand.
#'   Indien niet opgegeven wordt `"Namenlijst maken"` gebruikt.
#'
#' @return Onzichtbaar het pad naar het gemaakte of gecontroleerde
#'   werkbestand.
#'
#' @examples
#' werkbestand <- file.path(
#'   tempdir(),
#'   "namenlijst.xlsx"
#' )
#'
#' namen <- c(
#'   "HHBox.HH.AantalPP",
#'   "HHBox.Persoon.Leeftijd",
#'   "HHBox.Persoon.Geslacht"
#' )
#'
#' maak_namenlijst(
#'   werkbestand = werkbestand,
#'   namen = namen
#' )
#'
#' file.exists(werkbestand)
#'
#' @export
maak_namenlijst <- function(
    werkbestand,
    namen = NULL,
    bronbestand = NULL,
    omschrijving = NULL) {

  starttijd <- Sys.time()

  if (is.null(omschrijving)) {
    omschrijving <- paste0(
      "Namenlijst ",
      basename(werkbestand),
      " verwerken"
    )
  }

  schrijf_log(
    "START maak_namenlijst"
  )

  uitvoer <- function() {

    if (
      !is.character(werkbestand) ||
      length(werkbestand) != 1L ||
      is.na(werkbestand) ||
      !nzchar(werkbestand)
    ) {
      stop(
        "'werkbestand' moet een geldig bestandspad zijn.",
        call. = FALSE
      )
    }

    schrijf_controle <- function(
    actie,
    aantal_aangeleverd = NULL,
    aantal_na_samenvoegen = NULL,
    aantal_namen = NULL) {

      berichten <- "Actie namenlijst"
      aantallen <- actie

      if (!is.null(aantal_aangeleverd)) {
        berichten <- c(
          berichten,
          "Aantal aangeleverde namen"
        )

        aantallen <- c(
          aantallen,
          aantal_aangeleverd
        )
      }

      if (!is.null(aantal_na_samenvoegen)) {
        berichten <- c(
          berichten,
          "Aantal namen na samenvoegen"
        )

        aantallen <- c(
          aantallen,
          aantal_na_samenvoegen
        )
      }

      if (!is.null(aantal_namen)) {
        berichten <- c(
          berichten,
          "Aantal namen in namenlijst"
        )

        aantallen <- c(
          aantallen,
          aantal_namen
        )
      }

      rij <- data.frame(
        omschrijving = c(
          omschrijving,
          berichten
        ),
        aantal = c(
          "",
          aantallen
        ),
        stringsAsFactors = FALSE
      )

      schrijf_r_logging(
        "controlebestand",
        rij
      )

      duur <- round(
        as.numeric(
          difftime(
            Sys.time(),
            starttijd,
            units = "secs"
          )
        ),
        2
      )

      schrijf_log_controle(
        omschrijving = omschrijving,
        waarden = stats::setNames(
          rij$aantal[-1],
          rij$omschrijving[-1]
        ),
        duur = duur
      )

      invisible(NULL)
    }

    if (file.exists(werkbestand)) {

      namenlijst <- lees_namenlijst(
        werkbestand,
        dubbele_r_fout = FALSE,
        dubbele_uitgeklapte_r_fout = FALSE
      )

      schrijf_controle(
        actie = "Bestaande namenlijst gecontroleerd",
        aantal_namen = nrow(namenlijst)
      )

      message(
        "Namenlijst bestaat al en is gecontroleerd:\n",
        werkbestand
      )

      return(
        invisible(
          werkbestand
        )
      )
    }

    output_map <- dirname(
      werkbestand
    )

    if (!dir.exists(output_map)) {
      stop(
        paste0(
          "Map voor werkbestand bestaat niet:\n",
          output_map
        ),
        call. = FALSE
      )
    }

    if (
      !is.null(bronbestand) &&
      file.exists(bronbestand)
    ) {

      gelukt <- file.copy(
        from = bronbestand,
        to = werkbestand,
        overwrite = FALSE
      )

      if (!gelukt) {
        stop(
          paste0(
            "Bronbestand kon niet worden gekopieerd naar:\n",
            werkbestand
          ),
          call. = FALSE
        )
      }

      namenlijst <- tryCatch(
        {
          lees_namenlijst(
            werkbestand,
            dubbele_r_fout = FALSE,
            dubbele_uitgeklapte_r_fout = FALSE
          )
        },
        error = function(e) {
          NULL
        }
      )

      if (is.null(namenlijst)) {

        unlink(
          werkbestand
        )

        lees_namenlijst(
          bronbestand,
          dubbele_r_fout = FALSE,
          dubbele_uitgeklapte_r_fout = FALSE
        )

        stop(
          paste0(
            "Het gekopieerde bronbestand kon niet als geldige ",
            "namenlijst worden ingelezen."
          ),
          call. = FALSE
        )
      }

      schrijf_controle(
        actie = "Namenlijst aangemaakt op basis van bestaand bestand",
        aantal_namen = nrow(namenlijst)
      )

      message(
        "Namenlijst aangemaakt op basis van bestaand bestand:\n",
        bronbestand
      )

      return(
        invisible(
          werkbestand
        )
      )
    }

    if (is.null(namen)) {
      stop(
        paste0(
          "Er is geen bestaand bronbestand gevonden ",
          "en er is geen namen-vector opgegeven."
        ),
        call. = FALSE
      )
    }

    if (!is.character(namen)) {
      stop(
        "namen moet een character-vector zijn.",
        call. = FALSE
      )
    }

    if (anyNA(namen)) {
      stop(
        "namen bevat NA-waarden.",
        call. = FALSE
      )
    }

    aantal_aangeleverd <- length(
      namen
    )

    namen <- samenvoegen_namen(
      namen
    )

    aantal_na_samenvoegen <- length(
      namen
    )

    namen_df <- namen_converteren(
      namen
    )

    wb <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(
      wb,
      "namen"
    )

    openxlsx::writeData(
      wb,
      "namen",
      namen_df
    )

    openxlsx::setColWidths(
      wb,
      "namen",
      cols = seq_len(
        ncol(namen_df)
      ),
      widths = "auto"
    )

    openxlsx::saveWorkbook(
      wb,
      werkbestand,
      overwrite = FALSE
    )

    lees_namenlijst(
      werkbestand,
      dubbele_r_fout = FALSE,
      dubbele_uitgeklapte_r_fout = FALSE
    )

    schrijf_controle(
      actie = "Nieuwe namenlijst aangemaakt",
      aantal_aangeleverd = aantal_aangeleverd,
      aantal_na_samenvoegen = aantal_na_samenvoegen,
      aantal_namen = nrow(namen_df)
    )

    message(
      "Nieuwe namenlijst aangemaakt:\n",
      werkbestand
    )

    invisible(
      werkbestand
    )
  }

  resultaat <- tryCatch(

    withCallingHandlers(
      uitvoer(),

      warning = function(w) {

        schrijf_log(
          paste0(
            "WARNING maak_namenlijst\n",
            conditionMessage(w)
          )
        )
      }
    ),

    error = function(e) {

      schrijf_log(
        paste0(
          "ERROR maak_namenlijst\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE maak_namenlijst"
  )

  resultaat
}

# Hulp functies maak namenlijst -------------------------------------------
uitklappen_namen <- function(x) {
  expand_one_string <- function(s) {
    # Eerste range [a:b]

    m <- regexpr(
      "\\[(\\d+):(\\d+)\\]",
      s,
      perl = TRUE
    )

    # Eerste losse index [a]

    t <- regexpr(
      "\\[(\\d+)\\]",
      s,
      perl = TRUE
    )

    if (
      m[1] == -1L &&
        t[1] == -1L
    ) {
      return(s)
    }

    if (m[1] != -1L) {
      hit <- regmatches(
        s,
        m
      )

      nums <- sub(
        "^\\[",
        "",
        hit
      )

      nums <- sub(
        "\\]$",
        "",
        nums
      )

      ab <- strsplit(
        nums,
        ":",
        fixed = TRUE
      )[[1]]

      a <- as.integer(
        ab[1]
      )

      b <- as.integer(
        ab[2]
      )

      prefix <- substr(
        s,
        1L,
        m[1] - 1L
      )

      suffix <- substr(
        s,
        m[1] +
          attr(
            m,
            "match.length"
          ),
        nchar(s)
      )

      array_prefix <- (
        nchar(prefix) >= 2L &&
          substr(
            prefix,
            nchar(prefix) - 1L,
            nchar(prefix)
          ) %in% c(
            "_a",
            "_s"
          )
      )

      if (array_prefix) {
        paste0(
          prefix,
          a:b,
          suffix
        )
      } else {
        paste0(
          prefix,
          "[",
          a:b,
          "]",
          suffix
        )
      }
    } else {
      hit <- regmatches(
        s,
        t
      )

      nums <- sub(
        "^\\[",
        "",
        hit
      )

      nums <- sub(
        "\\]$",
        "",
        nums
      )

      a <- as.integer(
        nums
      )

      prefix <- substr(
        s,
        1L,
        t[1] - 1L
      )

      suffix <- substr(
        s,
        t[1] +
          attr(
            t,
            "match.length"
          ),
        nchar(s)
      )

      array_prefix <- (
        nchar(prefix) >= 2L &&
          substr(
            prefix,
            nchar(prefix) - 1L,
            nchar(prefix)
          ) %in% c(
            "_a",
            "_s"
          )
      )

      if (array_prefix) {
        paste0(
          prefix,
          a,
          suffix
        )
      } else {
        paste0(
          prefix,
          "[",
          a,
          "]",
          suffix
        )
      }
    }
  }

  expand_fully <- function(s) {
    out <- s

    repeat {
      new_out <- unlist(
        lapply(
          out,
          expand_one_string
        ),
        use.names = FALSE
      )

      if (identical(
        new_out,
        out
      )) {
        break
      }

      out <- new_out
    }

    out
  }

  unlist(
    lapply(
      x,
      expand_fully
    ),
    use.names = FALSE
  )
}

samenvoegen_namen <- function(x) {
  if (!is.character(x)) {
    stop(
      "x moet een character-vector zijn.",
      call. = FALSE
    )
  }

  if (anyNA(x)) {
    stop(
      "x bevat NA-waarden.",
      call. = FALSE
    )
  }

  x <- unique(x)

  to_ranges <- function(v) {
    v <- sort(
      unique(
        as.integer(v)
      )
    )

    if (length(v) == 0L) {
      return(character(0))
    }

    brk <- c(
      TRUE,
      diff(v) != 1L
    )

    starts <- v[
      brk
    ]

    ends <- c(
      v[
        c(
          which(brk)[-1L] - 1L
        )
      ],
      v[length(v)]
    )

    ifelse(
      starts == ends,
      as.character(starts),
      paste0(
        starts,
        ":",
        ends
      )
    )
  }

  parse_one <- function(s) {
    parts <- strsplit(
      s,
      "\\[\\d+\\]",
      perl = TRUE
    )[[1]]

    idxs <- regmatches(
      s,
      gregexpr(
        "\\[(\\d+)\\]",
        s,
        perl = TRUE
      )
    )[[1]]

    if (
      length(idxs) == 1L &&
        identical(idxs, "")
    ) {
      idxs <- character(0)
    }

    idxs <- sub(
      "^\\[",
      "",
      idxs
    )

    idxs <- sub(
      "\\]$",
      "",
      idxs
    )

    idxs <- as.integer(
      idxs
    )

    list(
      parts = parts,
      idxs = idxs
    )
  }

  parsed <- lapply(
    x,
    parse_one
  )

  keys <- vapply(
    parsed,
    function(p) {
      paste(
        p$parts,
        collapse = "\u241F"
      )
    },
    character(1)
  )

  key_levels <- keys[
    !duplicated(keys)
  ]

  groups <- split(
    seq_along(x),
    factor(
      keys,
      levels = key_levels
    )
  )

  out <- character(0)

  for (g in groups) {
    p0 <- parsed[[g[1L]]]

    k <- length(
      p0$idxs
    )

    # Geen indices

    if (k == 0L) {
      out <- c(
        out,
        unique(
          x[g]
        )
      )

      next
    }

    # Matrix met indices

    M <- do.call(
      rbind,
      lapply(
        g,
        function(i) {
          parsed[[i]]$idxs
        }
      )
    )

    if (is.null(dim(M))) {
      M <- matrix(
        M,
        nrow = 1L
      )
    }

    ranges_per_pos <- lapply(
      seq_len(k),
      function(j) {
        to_ranges(
          M[, j]
        )
      }
    )

    grid <- expand.grid(
      ranges_per_pos,
      stringsAsFactors = FALSE
    )

    kandidaten <- character(
      nrow(grid)
    )

    for (r in seq_len(nrow(grid))) {
      s <- p0$parts[1L]

      for (j in seq_len(k)) {
        vervolg <- if (
          length(p0$parts) >= j + 1L
        ) {
          p0$parts[j + 1L]
        } else {
          ""
        }

        if (is.na(vervolg)) {
          vervolg <- ""
        }

        s <- paste0(
          s,
          "[",
          grid[r, j],
          "]",
          vervolg
        )
      }

      kandidaten[r] <- s
    }

    origineel <- sort(
      unique(
        x[g]
      )
    )

    kandidaat_uitgeklapt <- sort(
      unique(
        uitklappen_namen(
          kandidaten
        )
      )
    )

    if (
      identical(
        kandidaat_uitgeklapt,
        origineel
      )
    ) {
      out <- c(
        out,
        kandidaten
      )
    } else {
      # Geen veilige compressie mogelijk

      out <- c(
        out,
        origineel
      )
    }
  }

  unique(out)
}

naam_converteren <- function(x) {
  if (
    !is.character(x) ||
      length(x) != 1L ||
      is.na(x)
  ) {
    stop(
      "x moet een geldige characterwaarde zijn.",
      call. = FALSE
    )
  }

  split_index <- function(x) {
    match <- regexec(
      "^(.+?)(\\[[0-9]+(?::[0-9]+)?\\])?$",
      x,
      perl = TRUE
    )

    delen <- regmatches(
      x,
      match
    )[[1]]

    if (length(delen) == 0L) {
      stop(
        paste0(
          "Naam kan niet worden geinterpreteerd: '",
          x,
          "'."
        ),
        call. = FALSE
      )
    }

    list(
      naam = delen[2L],
      index = if (
        length(delen) >= 3L
      ) {
        delen[3L]
      } else {
        ""
      }
    )
  }

  delen <- strsplit(
    x,
    ".",
    fixed = TRUE
  )[[1]]

  if (length(delen) == 0L) {
    return(x)
  }

  # Laatste onderdeel = variabele

  var_deel <- split_index(
    delen[
      length(delen)
    ]
  )

  if (nzchar(var_deel$index)) {
    var_naam <- paste0(
      var_deel$naam,
      "_s",
      var_deel$index
    )
  } else {
    var_naam <- var_deel$naam
  }

  if (length(delen) == 1L) {
    return(var_naam)
  }

  blokken <- delen[
    -length(delen)
  ]

  # Alleen blokken met array-index behouden

  blok_namen <- vapply(
    blokken,
    function(blok) {
      blok_deel <- split_index(
        blok
      )

      if (!nzchar(blok_deel$index)) {
        return(NA_character_)
      }

      paste0(
        blok_deel$naam,
        "_a",
        blok_deel$index
      )
    },
    character(1)
  )

  blok_namen <- blok_namen[
    !is.na(blok_namen)
  ]

  if (length(blok_namen) == 0L) {
    return(var_naam)
  }

  paste(
    c(
      blok_namen,
      var_naam
    ),
    collapse = "."
  )
}

namen_converteren <- function(namen) {
  if (!is.character(namen)) {
    stop(
      "namen moet een character-vector zijn.",
      call. = FALSE
    )
  }

  if (anyNA(namen)) {
    stop(
      "namen bevat NA-waarden.",
      call. = FALSE
    )
  }

  data.frame(
    Blaisenamen = namen,
    Rnamen = vapply(
      namen,
      naam_converteren,
      character(1)
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

controleren_dubbele_namen <- function(
    df,
    fout = FALSE) {
  if (!is.data.frame(df)) {
    stop(
      "df moet een dataframe zijn.",
      call. = FALSE
    )
  }

  if (
    !all(
      c(
        "Blaisenamen",
        "Rnamen"
      ) %in% names(df)
    )
  ) {
    stop(
      paste0(
        "df moet de kolommen 'Blaisenamen' ",
        "en 'Rnamen' bevatten."
      ),
      call. = FALSE
    )
  }

  dubbel <- (
    duplicated(df$Rnamen) |
      duplicated(
        df$Rnamen,
        fromLast = TRUE
      )
  )

  if (!any(dubbel)) {
    return(character(0))
  }

  dubbele_df <- df[
    dubbel,
    c(
      "Blaisenamen",
      "Rnamen"
    ),
    drop = FALSE
  ]

  dubbele_df <- dubbele_df[
    order(
      dubbele_df$Rnamen,
      dubbele_df$Blaisenamen
    ), ,
    drop = FALSE
  ]

  dubbele_namen <- unique(
    dubbele_df$Rnamen
  )

  melding <- paste0(
    "De volgende R-namen zijn nog dubbel:\n",
    paste(
      paste0(
        "- ",
        dubbele_namen
      ),
      collapse = "\n"
    ),
    "\n\nPas deze namen aan voordat de namenlijst definitief wordt gemaakt."
  )

  if (fout) {
    stop(
      melding,
      call. = FALSE
    )
  } else {
    warning(
      melding,
      call. = FALSE
    )
  }

  dubbele_namen
}

lees_namenlijst <- function(
    bestand,
    dubbele_r_fout = FALSE,
    dubbele_uitgeklapte_r_fout = FALSE) {
  if (!file.exists(bestand)) {
    stop(
      paste0(
        "Namenbestand bestaat niet:\n",
        bestand
      ),
      call. = FALSE
    )
  }

  # Worksheets

  sheets <- openxlsx::getSheetNames(
    bestand
  )

  if (!"namen" %in% sheets) {
    stop(
      paste0(
        "Namenbestand heeft niet de juiste opzet.\n",
        "Worksheet 'namen' ontbreekt:\n",
        bestand
      ),
      call. = FALSE
    )
  }

  # Inlezen

  df <- openxlsx::read.xlsx(
    bestand,
    sheet = "namen"
  )

  verplichte_kolommen <- c(
    "Blaisenamen",
    "Rnamen"
  )

  ontbrekende_kolommen <- setdiff(
    verplichte_kolommen,
    names(df)
  )

  if (length(ontbrekende_kolommen) > 0L) {
    stop(
      paste0(
        "Namenbestand heeft niet de juiste opzet.\n",
        "Ontbrekende kolom(men):\n",
        paste(
          paste0(
            "- ",
            ontbrekende_kolommen
          ),
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  df <- df[
    ,
    verplichte_kolommen,
    drop = FALSE
  ]

  df$Blaisenamen <- as.character(
    df$Blaisenamen
  )

  df$Rnamen <- as.character(
    df$Rnamen
  )

  # Lege namen

  leeg_blaise <- (
    is.na(df$Blaisenamen) |
      trimws(
        df$Blaisenamen
      ) == ""
  )

  if (any(leeg_blaise)) {
    stop(
      paste0(
        "Er staan lege waarden in 'Blaisenamen'.\n",
        "Regel(s): ",
        paste(
          which(leeg_blaise) + 1L,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  leeg_r <- (
    is.na(df$Rnamen) |
      trimws(
        df$Rnamen
      ) == ""
  )

  if (any(leeg_r)) {
    stop(
      paste0(
        "Er staan lege waarden in 'Rnamen'.\n",
        "Regel(s): ",
        paste(
          which(leeg_r) + 1L,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  # Dubbele compacte Blaisenamen

  dubbel_blaise <- (
    duplicated(df$Blaisenamen) |
      duplicated(
        df$Blaisenamen,
        fromLast = TRUE
      )
  )

  if (any(dubbel_blaise)) {
    dubbele_blaise <- unique(
      df$Blaisenamen[
        dubbel_blaise
      ]
    )

    stop(
      paste0(
        "De volgende Blaisenamen staan dubbel in het bestand:\n",
        paste(
          paste0(
            "- ",
            dubbele_blaise
          ),
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  # Dubbele compacte R-namen

  dubbele_rnamen <- controleren_dubbele_namen(
    df,
    fout = dubbele_r_fout
  )

  # Per regel uitklappen

  blaisenamen_per_regel <- lapply(
    df$Blaisenamen,
    uitklappen_namen
  )

  rnamen_per_regel <- lapply(
    df$Rnamen,
    uitklappen_namen
  )

  aantal_blaise <- lengths(
    blaisenamen_per_regel
  )

  aantal_r <- lengths(
    rnamen_per_regel
  )

  verkeerde_regels <- which(
    aantal_blaise != aantal_r
  )

  if (length(verkeerde_regels) > 0L) {
    stop(
      paste0(
        "Blaisenamen en Rnamen leveren na uitklappen ",
        "niet hetzelfde aantal variabelen op.\n",
        "Regel(s): ",
        paste(
          verkeerde_regels + 1L,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  # Alles uitklappen

  blaisenamen_uitgeklapt <- unlist(
    blaisenamen_per_regel,
    use.names = FALSE
  )

  rnamen_uitgeklapt <- unlist(
    rnamen_per_regel,
    use.names = FALSE
  )

  # Ook overlappende Blaise-ranges voorkomen

  dubbel_blaise_uitgeklapt <- (
    duplicated(
      blaisenamen_uitgeklapt
    ) |
      duplicated(
        blaisenamen_uitgeklapt,
        fromLast = TRUE
      )
  )

  if (any(dubbel_blaise_uitgeklapt)) {
    stop(
      paste0(
        "Na het uitklappen zijn de volgende Blaisenamen dubbel:\n",
        paste(
          paste0(
            "- ",
            unique(
              blaisenamen_uitgeklapt[
                dubbel_blaise_uitgeklapt
              ]
            )
          ),
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  # Dubbele R-namen na uitklappen

  bron_regel <- rep(
    seq_len(nrow(df)),
    lengths(
      rnamen_per_regel
    )
  )

  dubbel_r_uitgeklapt <- (
    duplicated(
      rnamen_uitgeklapt
    ) |
      duplicated(
        rnamen_uitgeklapt,
        fromLast = TRUE
      )
  )

  nieuwe_conflicten <- character(0)

  if (any(dubbel_r_uitgeklapt)) {
    dubbele_uitgeklapte_namen <- unique(
      rnamen_uitgeklapt[
        dubbel_r_uitgeklapt
      ]
    )

    is_nieuw_conflict <- vapply(
      dubbele_uitgeklapte_namen,
      function(naam) {
        regels <- unique(
          bron_regel[
            rnamen_uitgeklapt == naam
          ]
        )

        compacte_namen <- unique(
          df$Rnamen[
            regels
          ]
        )

        length(compacte_namen) > 1L
      },
      logical(1)
    )

    nieuwe_conflicten <- dubbele_uitgeklapte_namen[
      is_nieuw_conflict
    ]

    if (length(nieuwe_conflicten) > 0L) {
      stop(
        paste0(
          "Na het uitklappen ontstaan dubbele R-namen ",
          "die in de compacte namenlijst niet zichtbaar zijn.\n",
          "Controleer de betreffende ranges.\n\n",
          paste(
            paste0(
              "- ",
              nieuwe_conflicten
            ),
            collapse = "\n"
          )
        ),
        call. = FALSE
      )
    }

    if (
      dubbele_uitgeklapte_r_fout &&
        length(
          dubbele_uitgeklapte_namen
        ) > 0L
    ) {
      stop(
        paste0(
          "Na het uitklappen zijn de volgende R-namen dubbel:\n",
          paste(
            paste0(
              "- ",
              dubbele_uitgeklapte_namen
            ),
            collapse = "\n"
          )
        ),
        call. = FALSE
      )
    }
  }

  list(
    compact = df,
    Blaisenamen = blaisenamen_uitgeklapt,
    Rnamen = rnamen_uitgeklapt,
    dubbele_Rnamen = dubbele_rnamen,
    nieuwe_conflicten = nieuwe_conflicten
  )
}

#' Afleidingen maken
#'
#' Voert een of meerdere conditionele afleidingen uit op een dataframe.
#' De functie ondersteunt onder andere koppelingen, lookups,
#' groepsaggregaties en metadata-controles.
#'
#' @param data Dataframe waarop de afleiding wordt uitgevoerd.
#' @param kolommen Naam of namen van de doelvariabelen.
#' @param ... Afleidingsregels als opeenvolgende combinaties van
#'   conditie en waarde.
#' @param controle Indien `TRUE` wordt uitgebreide controle-informatie
#'   opgebouwd in `controle_maak_afleiding`.
#' @param meta Optioneel metadata-dataframe. Indien niet opgegeven wordt
#'   geprobeerd metadata automatisch te vinden voor `data`.
#' @param behoud_rijen Indien `TRUE` worden alleen de rijen behouden
#'   waarvoor de conditie van de enige regel `TRUE` is.
#' @param behoud_kolommen Indien `TRUE` worden alleen de doelkolommen
#'   teruggegeven.
#' @param error Indien `TRUE` worden doelvariabelen gecontroleerd en
#'   wordt beschikbare metadata na de afleiding gekoppeld.
#' @param omschrijving Optionele omschrijving voor het controlebestand.
#'   Indien niet opgegeven wordt automatisch een omschrijving opgebouwd
#'   op basis van het aantal doelvariabelen en de naam van de dataset.
#' @return Het aangepaste dataframe, of bij `behoud_kolommen = TRUE`
#'   alleen de geselecteerde doelkolom(men).
#'
#' @export
maak_afleiding <- function(
    data,
    kolommen,
    ...,
    controle = TRUE,
    error = TRUE,
    meta = NULL,
    behoud_rijen = FALSE,
    behoud_kolommen = FALSE,
    omschrijving = NULL) {

  starttijd <- Sys.time()

  data_naam <- deparse(
    substitute(data)
  )

  caller_env <- parent.frame()

  raw_rules <- as.list(
    substitute(list(...))
  )[-1L]

  schrijf_log(
    "START maak_afleiding"
  )

  resultaat <- tryCatch(

    withCallingHandlers(

      {

        if (data.table::is.data.table(data)) {
          stop(
            paste0(
              "Je geeft een data table door, dit werkt niet. ",
              "Verander je object naar een data.frame"
            ),
            call. = FALSE
          )
        }

        if (
          !is.character(kolommen) ||
          length(kolommen) == 0L ||
          anyNA(kolommen) ||
          any(trimws(kolommen) == "")
        ) {
          stop(
            "'kolommen' moet een of meerdere geldige kolomnamen bevatten.",
            call. = FALSE
          )
        }

        kolommen_uniek <- unique(
          kolommen
        )

        if (is.null(omschrijving)) {

          aantal_kolommen <- length(
            kolommen_uniek
          )

          omschrijving <- paste0(
            aantal_kolommen,
            if (aantal_kolommen == 1L) {
              " variabele afleiden in "
            } else {
              " variabelen afleiden in "
            },
            data_naam
          )
        }

        if (!is.data.frame(data)) {
          data <- as.data.frame(data)
        }

        n_rijen_voor <- nrow(data)
        kolommen_voor <- names(data)

        meta_beschikbaar <- FALSE
        meta_controle <- NULL

        if (
          error &&
          !behoud_kolommen
        ) {

          meta <- meta_resolver(
            meta = meta,
            dataset_naam = data_naam,
            envir = caller_env,
            verplicht = FALSE
          )

          meta_beschikbaar <- !is.null(meta)
        }

        meta_namen_correct <- stats::setNames(
          rep(
            FALSE,
            length(kolommen_uniek)
          ),
          kolommen_uniek
        )

        if (meta_beschikbaar) {

          controle_namen <- controleren_meta_namen(
            variabele_naam = kolommen_uniek,
            meta = meta,
            error = error
          )

          if (isTRUE(controle_namen)) {

            meta_namen_correct[] <- TRUE

          } else {

            meta_namen_correct[
              !names(meta_namen_correct) %in%
                controle_namen
            ] <- TRUE
          }
        }

        if (
          length(raw_rules) == 0L ||
          length(raw_rules) %% 2L != 0L
        ) {
          stop(
            paste0(
              "Afleidingsregels moeten bestaan uit combinaties ",
              "van conditie en waarde."
            ),
            call. = FALSE
          )
        }

        regels <- vector(
          "list",
          length(raw_rules) / 2L
        )

        regel_nr <- 1L

        for (
          i in seq.int(
            1L,
            length(raw_rules),
            by = 2L
          )
        ) {

          regels[[regel_nr]] <- list(
            conditie = raw_rules[[i]],
            waarde = raw_rules[[i + 1L]]
          )

          regel_nr <- regel_nr + 1L
        }

        if (
          behoud_rijen &&
          length(regels) != 1L
        ) {
          stop(
            "behoud_rijen = TRUE kan alleen als precies 1 regel is opgegeven.",
            call. = FALSE
          )
        }

        kolommen_bestaand <- kolommen_uniek[
          kolommen_uniek %in% names(data)
        ]

        oude_waarden <- NULL

        if (length(kolommen_bestaand) > 0L) {

          oude_waarden <- lapply(
            kolommen_bestaand,
            function(kolom) {
              data[[kolom]]
            }
          )

          names(oude_waarden) <- kolommen_bestaand
        }

        for (kolom in kolommen) {

          if (!kolom %in% names(data)) {
            data[[kolom]] <- rep(
              NA,
              nrow(data)
            )
          }
        }

        selectie_controle <- numeric(0)

        schrijf_controle <- function() {

          berichten <- "Aantal rijen"
          aantallen <- nrow(data)

          for (kolom in kolommen_uniek) {

            if (kolom %in% kolommen_bestaand) {

              oud <- oude_waarden[[kolom]]
              nieuw <- data[[kolom]]

              gewijzigd <- (
                xor(
                  is.na(oud),
                  is.na(nieuw)
                ) |
                  (
                    !is.na(oud) &
                      !is.na(nieuw) &
                      oud != nieuw
                  )
              )

              aantal <- sum(
                gewijzigd,
                na.rm = TRUE
              )

              berichten <- c(
                berichten,
                paste0(
                  "Aantal gewijzigde waarden ",
                  kolom
                )
              )

              aantallen <- c(
                aantallen,
                aantal
              )

            } else {

              aantal <- sum(
                !is.na(
                  data[[kolom]]
                )
              )

              berichten <- c(
                berichten,
                paste0(
                  "Aantal gevulde waarden ",
                  kolom
                )
              )

              aantallen <- c(
                aantallen,
                aantal
              )
            }
          }

          if (length(selectie_controle) > 0L) {

            berichten <- c(
              berichten,
              names(selectie_controle)
            )

            aantallen <- c(
              aantallen,
              unname(selectie_controle)
            )
          }

          if (!is.null(meta_controle)) {

            berichten <- c(
              berichten,
              names(meta_controle)
            )

            aantallen <- c(
              aantallen,
              unname(meta_controle)
            )
          }

          rij <- data.frame(
            omschrijving = c(
              omschrijving,
              berichten
            ),
            aantal = c(
              "",
              aantallen
            ),
            stringsAsFactors = FALSE
          )

          schrijf_r_logging(
            "controlebestand",
            rij
          )

          duur <- round(
            as.numeric(
              difftime(
                Sys.time(),
                starttijd,
                units = "secs"
              )
            ),
            2
          )

          schrijf_log_controle(
            omschrijving = omschrijving,
            waarden = stats::setNames(
              rij$aantal[-1L],
              rij$omschrijving[-1L]
            ),
            duur = duur
          )

          invisible(NULL)
        }

        if (nrow(data) == 0L) {

          if (meta_beschikbaar) {

            geldige_meta_kolommen <- kolommen_uniek[
              vapply(
                kolommen_uniek,
                function(kolom) {
                  isTRUE(
                    meta_namen_correct[kolom]
                  )
                },
                logical(1)
              )
            ]

            if (length(geldige_meta_kolommen) > 0L) {

              data <- koppel_meta(
                dataset = data,
                meta = meta,
                variabele_naam = geldige_meta_kolommen,
                controle_range = FALSE,
                dataset_naam = data_naam,
                interne_aanroep = TRUE
              )

              meta_controle <- attr(
                data,
                "meta_controle"
              )

              attr(
                data,
                "meta_controle"
              ) <- NULL
            }
          }

          if (behoud_rijen) {

            selectie_controle <- c(
              selectie_controle,
              maak_selectie_controle(
                n_rijen_voor = n_rijen_voor,
                n_rijen_na = nrow(data)
              )
            )
          }

          if (behoud_kolommen) {

            selectie_controle <- c(
              selectie_controle,
              maak_selectie_controle(
                kolommen_voor = kolommen_voor,
                kolommen_na = kolommen_uniek
              )
            )
          }

          schrijf_controle()

          if (behoud_kolommen) {

            data <- if (
              length(kolommen) == 1L
            ) {
              data[[kolommen]]
            } else {
              data[
                ,
                kolommen,
                drop = FALSE
              ]
            }
          }

        } else {

          context <- maak_expressie_context()

          eval_env <- maak_expressie_omgeving(
            data = data,
            caller_env = caller_env,
            context = context
          )

          toon_waarden <- function(
    x,
    max = 10) {

            x <- unique(x)
            x <- as.character(x)

            if (length(x) > max) {

              paste(
                c(
                  x[seq_len(max)],
                  "..."
                ),
                collapse = ", "
              )

            } else {

              paste(
                x,
                collapse = ", "
              )
            }
          }

          bevat_has_match <- function(expr) {

            if (!is.call(expr)) {
              return(FALSE)
            }

            if (
              identical(
                expr[[1L]],
                as.name("has_match")
              )
            ) {
              return(TRUE)
            }

            onderdelen <- as.list(
              expr
            )[-1L]

            any(
              vapply(
                onderdelen,
                bevat_has_match,
                logical(1)
              )
            )
          }

          controles <- list()

          conditions <- vector(
            "list",
            length(regels)
          )

          output_condition <- NULL

          for (i in seq_along(regels)) {

            context$actieve_condition <- NULL

            regelnummer <- paste0(
              "#R",
              i
            )

            context$current_regel <- regelnummer
            context$laatste_match <- NULL
            context$laatste_lookup_values <- character(0)
            context$basis_condition <- NULL

            expr <- regels[[i]]$conditie

            conditie_heeft_match <- bevat_has_match(
              expr
            )

            if (conditie_heeft_match) {

              context$has_match_mode <- "basis"
              context$laatste_match <- NULL

              if (
                is.call(expr) &&
                identical(
                  as.character(expr[[1L]]),
                  "!"
                )
              ) {

                basis_condition <- eval(
                  expr[[2L]],
                  eval_env
                )

              } else {

                basis_condition <- eval(
                  expr,
                  eval_env
                )
              }

              basis_condition[
                is.na(basis_condition)
              ] <- FALSE

              if (length(basis_condition) == 1L) {

                basis_condition <- rep(
                  basis_condition,
                  nrow(data)
                )
              }

              if (
                length(basis_condition) !=
                nrow(data)
              ) {
                stop(
                  paste0(
                    "Basisconditie in ",
                    regelnummer,
                    " heeft lengte ",
                    length(basis_condition),
                    ", maar dataset heeft ",
                    nrow(data),
                    " rijen."
                  ),
                  call. = FALSE
                )
              }

              context$basis_condition <- basis_condition
              context$has_match_mode <- "normaal"
              context$laatste_match <- NULL

              condition <- eval(
                expr,
                eval_env
              )

            } else {

              context$has_match_mode <- "normaal"
              context$laatste_match <- NULL

              condition <- eval(
                expr,
                eval_env
              )

              basis_condition <- condition
              context$basis_condition <- basis_condition
            }

            if (!is.logical(condition)) {
              stop(
                paste0(
                  "Conditie in ",
                  regelnummer,
                  " moet een logische waarde opleveren."
                ),
                call. = FALSE
              )
            }

            condition[
              is.na(condition)
            ] <- FALSE

            if (length(condition) == 1L) {

              condition <- rep(
                condition,
                nrow(data)
              )
            }

            if (
              length(condition) !=
              nrow(data)
            ) {
              stop(
                paste0(
                  "Conditie in ",
                  regelnummer,
                  " heeft lengte ",
                  length(condition),
                  ", maar dataset heeft ",
                  nrow(data),
                  " rijen."
                ),
                call. = FALSE
              )
            }

            n_condition <- sum(
              condition
            )

            conditions[[i]] <- condition

            if (behoud_rijen) {
              output_condition <- condition
            }

            match_van_conditie <- context$laatste_match

            eval_env$huidige_data <- function() {

              data[
                condition,
                ,
                drop = FALSE
              ]
            }

            eval_env$gekoppelde_data <- function() {

              if (is.null(match_van_conditie)) {
                stop(
                  paste0(
                    "gekoppelde_data() kan alleen gebruikt worden ",
                    "als de conditie has_match() bevat."
                  ),
                  call. = FALSE
                )
              }

              match_van_conditie$tabel[
                match_van_conditie$m[
                  condition
                ],
                ,
                drop = FALSE
              ]
            }

            waarde_expr <- regels[[i]]$waarde

            context$actieve_condition <- condition

            if (length(kolommen) == 1L) {

              waarde_blokken <- list(
                waarde_expr
              )

            } else if (
              is.call(waarde_expr) &&
              identical(
                as.character(
                  waarde_expr[[1L]]
                ),
                "list"
              )
            ) {

              waarde_blokken <- as.list(
                waarde_expr
              )[-1L]

            } else {

              waarde_blokken <- list(
                waarde_expr
              )
            }

            controle_row <- data.frame(
              regel = regelnummer,
              aantal = n_condition,
              row.names = NULL,
              check.names = FALSE
            )

            j_col <- 1L

            for (blok in waarde_blokken) {

              value <- eval(
                blok,
                eval_env
              )

              if (is.list(value)) {
                value_list <- value
              } else {
                value_list <- list(value)
              }

              n_values <- length(
                value_list
              )

              if (
                j_col + n_values - 1L >
                length(kolommen)
              ) {
                stop(
                  "Meer waarden opgegeven dan kolommen bevat.",
                  call. = FALSE
                )
              }

              kolommen_blok <- kolommen[
                j_col:(
                  j_col +
                    n_values -
                    1L
                )
              ]

              for (k in seq_along(kolommen_blok)) {

                kolom <- kolommen_blok[k]
                value_k <- value_list[[k]]

                controle_row[[kolom]] <- if (
                  length(value_k) == 1L
                ) {

                  as.character(
                    value_k
                  )

                } else if (
                  length(value_k) == n_condition
                ) {

                  toon_waarden(
                    value_k
                  )

                } else if (
                  length(value_k) == nrow(data)
                ) {

                  toon_waarden(
                    value_k[
                      condition
                    ]
                  )

                } else {

                  NA_character_
                }

                data[
                  condition,
                  kolom
                ] <- if (
                  length(value_k) == 1L
                ) {

                  value_k

                } else if (
                  length(value_k) == n_condition
                ) {

                  value_k

                } else if (
                  length(value_k) == nrow(data)
                ) {

                  value_k[
                    condition
                  ]

                } else {

                  stop(
                    paste0(
                      "Lengte van waarde klopt niet voor kolom '",
                      kolom,
                      "'. length(value) = ",
                      length(value_k),
                      ", sum(condition) = ",
                      n_condition,
                      ", nrow(data) = ",
                      nrow(data),
                      "."
                    ),
                    call. = FALSE
                  )
                }
              }

              for (kolom in kolommen_blok) {
                eval_env[[kolom]] <- data[[kolom]]
              }

              j_col <- j_col + n_values
            }

            if (j_col <= length(kolommen)) {
              stop(
                "Minder waarden opgegeven dan kolommen bevat.",
                call. = FALSE
              )
            }

            if (controle) {

              controles[[
                length(controles) + 1L
              ]] <- controle_row
            }
          }

          context$actieve_condition <- NULL
          context$basis_condition <- NULL
          context$current_regel <- NULL

          if (meta_beschikbaar) {

            geldige_meta_kolommen <- kolommen_uniek[
              vapply(
                kolommen_uniek,
                function(kolom) {
                  isTRUE(
                    meta_namen_correct[kolom]
                  )
                },
                logical(1)
              )
            ]

            if (length(geldige_meta_kolommen) > 0L) {

              data <- koppel_meta(
                dataset = data,
                meta = meta,
                variabele_naam = geldige_meta_kolommen,
                dataset_naam = data_naam,
                interne_aanroep = TRUE
              )

              meta_controle <- attr(
                data,
                "meta_controle"
              )

              attr(
                data,
                "meta_controle"
              ) <- NULL
            }
          }

          condition_matrix <- do.call(
            cbind,
            conditions
          )

          aantal_regels_per_rij <- rowSums(
            condition_matrix
          )

          geen_regel <- sum(
            aantal_regels_per_rij == 0L
          )

          overlap <- sum(
            aantal_regels_per_rij > 1L
          )

          if (behoud_rijen) {

            data <- behoud_attributen(
              data = data,
              selectie = output_condition
            )

            if (!is.null(oude_waarden)) {

              oude_waarden <- lapply(
                oude_waarden,
                function(x) {
                  x[
                    output_condition
                  ]
                }
              )
            }

            selectie_controle <- c(
              selectie_controle,
              maak_selectie_controle(
                n_rijen_voor = n_rijen_voor,
                n_rijen_na = nrow(data)
              )
            )
          }

          if (behoud_kolommen) {

            selectie_controle <- c(
              selectie_controle,
              maak_selectie_controle(
                kolommen_voor = kolommen_voor,
                kolommen_na = kolommen_uniek
              )
            )
          }

          schrijf_controle()

          if (controle) {

            if (
              exists(
                "controle_maak_afleiding",
                envir = .GlobalEnv,
                inherits = FALSE
              )
            ) {

              controle_object <- get(
                "controle_maak_afleiding",
                envir = .GlobalEnv,
                inherits = FALSE
              )

            } else {

              controle_object <- list()
            }

            regels_df <- do.call(
              rbind,
              controles
            )

            for (kolom in kolommen_uniek) {

              if (is.null(
                controle_object[[kolom]]
              )) {

                controle_object[[kolom]] <- list(
                  regels = data.frame(),
                  geen_regel = 0L,
                  overlap = 0L,
                  matches = data.frame(),
                  wijzigingen = data.frame(),
                  freq = data.frame()
                )
              }

              nieuwe_regels <- regels_df[
                ,
                c(
                  "regel",
                  "aantal",
                  kolom
                ),
                drop = FALSE
              ]

              controle_object[[kolom]]$regels <- rbind(
                controle_object[[kolom]]$regels,
                nieuwe_regels
              )

              controle_object[[kolom]]$geen_regel <- geen_regel
              controle_object[[kolom]]$overlap <- overlap

              relevante_koppelingen <- Filter(
                function(x) {
                  !is.null(x$regel)
                },
                context$koppelingen
              )

              if (length(relevante_koppelingen) > 0L) {

                nieuwe_matches <- do.call(
                  rbind,
                  lapply(
                    relevante_koppelingen,
                    function(x) {

                      data.frame(
                        regel = x$regel,
                        tabel = x$tabel,
                        by = paste(
                          x$sleutel,
                          collapse = ", "
                        ),
                        value = if (
                          length(x$opgehaald) == 0L
                        ) {
                          NA_character_
                        } else {
                          paste(
                            x$opgehaald,
                            collapse = ", "
                          )
                        },
                        rijen_x = x$gecontroleerd,
                        match = x$match,
                        geen_match = x$geen_match,
                        dubbele_bronsleutels =
                          x$dubbele_bronsleutels,
                        row.names = NULL,
                        check.names = FALSE
                      )
                    }
                  )
                )

                controle_object[[kolom]]$matches <- rbind(
                  controle_object[[kolom]]$matches,
                  nieuwe_matches
                )
              }

              meta_class <- attr(
                data[[kolom]],
                "meta_class"
              )

              is_enum <- (
                length(meta_class) == 1L &&
                  !is.na(meta_class) &&
                  grepl(
                    "^ENUM\\[",
                    meta_class
                  )
              )

              if (
                kolom %in% kolommen_bestaand &&
                is_enum
              ) {

                oud <- oude_waarden[[kolom]]
                nieuw <- data[[kolom]]

                gewijzigd <- (
                  xor(
                    is.na(oud),
                    is.na(nieuw)
                  ) |
                    (
                      !is.na(oud) &
                        !is.na(nieuw) &
                        oud != nieuw
                    )
                )

                if (any(gewijzigd)) {

                  wijzigingen <- as.data.frame(
                    table(
                      van = oud[gewijzigd],
                      naar = nieuw[gewijzigd],
                      useNA = "ifany"
                    ),
                    stringsAsFactors = FALSE
                  )

                  wijzigingen <- wijzigingen[
                    wijzigingen$Freq > 0L,
                    ,
                    drop = FALSE
                  ]

                  names(wijzigingen) <- c(
                    "van",
                    "naar",
                    "aantal"
                  )

                } else {

                  wijzigingen <- data.frame(
                    van = character(0),
                    naar = character(0),
                    aantal = integer(0),
                    stringsAsFactors = FALSE
                  )
                }

                controle_object[[kolom]]$wijzigingen <- wijzigingen

              } else {

                controle_object[[kolom]]$wijzigingen <- data.frame()
              }

              if (is.numeric(data[[kolom]])) {

                controle_object[[kolom]]$freq <- freq_of_decielen(
                  data[[kolom]]
                )

              } else {

                controle_object[[kolom]]$freq <- freq_normaal(
                  data[[kolom]]
                )
              }

              names(
                controle_object[[kolom]]$freq
              )[1L] <- kolom

              class(
                controle_object[[kolom]]
              ) <- c(
                "GTST_controle_variabele",
                "list"
              )
            }

            class(
              controle_object
            ) <- c(
              "GTST_controle_afleiding",
              "list"
            )

            assign(
              "controle_maak_afleiding",
              controle_object,
              envir = .GlobalEnv
            )
          }

          if (behoud_kolommen) {

            data <- if (
              length(kolommen) == 1L
            ) {
              data[[kolommen]]
            } else {
              data[
                ,
                kolommen,
                drop = FALSE
              ]
            }
          }
        }

        data
      },

    warning = function(w) {

      schrijf_log(
        paste0(
          "WARNING maak_afleiding\n",
          conditionMessage(w)
        )
      )
    }
    ),

    error = function(e) {

      schrijf_log(
        paste0(
          "ERROR maak_afleiding\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE maak_afleiding"
  )

  resultaat
}

# Maak afleiding helpers --------------------------------------------------
freq_normaal <- function(x) {

  if (length(x) == 0L) {
    return(
      data.frame(
        waarde = character(0),
        aantal = integer(0),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- as.data.frame(
    table(
      x,
      useNA = "ifany"
    ),
    stringsAsFactors = FALSE
  )

  names(out) <- c(
    "waarde",
    "aantal"
  )

  out
}

behoud_attributen <- function(
    data,
    selectie) {

  attributen <- lapply(
    data,
    attributes
  )

  resultaat <- data[
    selectie,
    ,
    drop = FALSE
  ]

  for (kolom in names(resultaat)) {

    attrs <- attributen[[kolom]]

    if (length(attrs) == 0L) {
      next
    }

    for (attribuut in names(attrs)) {

      attr(
        resultaat[[kolom]],
        attribuut
      ) <- attrs[[attribuut]]
    }
  }

  resultaat
}

freq_of_decielen <- function(
    x,
    max_unieke = 50) {

  if (length(x) == 0L) {
    return(
      data.frame(
        waarde = character(0),
        aantal = integer(0),
        stringsAsFactors = FALSE
      )
    )
  }
  freq <- table(
    x,
    useNA = "ifany"
  )

  n_uniek <- length(
    unique(
      x[
        !is.na(x)
      ]
    )
  )

  # Bij weinig unieke waarden:
  # normale frequentietabel

  if (n_uniek <= max_unieke) {
    out <- as.data.frame(
      freq,
      stringsAsFactors = FALSE
    )

    names(out) <- c(
      "waarde",
      "aantal"
    )

    return(out)
  }

  # Bij veel unieke waarden:
  # decielen

  x_non_na <- x[
    !is.na(x)
  ]

  breaks <- stats::quantile(
    x_non_na,
    probs = seq(
      0,
      1,
      0.1
    ),
    na.rm = TRUE,
    names = FALSE,
    type = 1
  )

  breaks <- unique(
    breaks
  )

  # Extra zekerheid

  if (length(breaks) < 2L) {
    out <- as.data.frame(
      freq,
      stringsAsFactors = FALSE
    )

    names(out) <- c(
      "waarde",
      "aantal"
    )

    return(out)
  }

  breaks_txt <- formatC(
    breaks,
    format = "fg",
    digits = 12
  )

  labels <- paste0(
    c(
      "[",
      rep(
        "(",
        length(breaks) - 2L
      )
    ),
    breaks_txt[
      -length(breaks)
    ],
    ", ",
    breaks_txt[
      -1L
    ],
    "]"
  )

  intervallen <- cut(
    x_non_na,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = TRUE
  )

  out <- as.data.frame(
    table(intervallen),
    stringsAsFactors = FALSE
  )

  names(out) <- c(
    "waarde",
    "aantal"
  )

  n_na <- sum(
    is.na(x)
  )

  if (n_na > 0L) {
    out <- rbind(
      out,
      data.frame(
        waarde = NA,
        aantal = n_na
      )
    )
  }

  out
}

#' Wide data omzetten naar long formaat
#'
#' Zet arrayvariabelen zoals `Prefix_a1.X`, `Prefix_a2.X`, enzovoort
#' om naar long-formaat zoals `Prefix.X`.
#'
#' @param data Dataframe in wide-formaat.
#' @param prefix Prefix van de arrayvariabelen.
#' @param omschrijving Optionele omschrijving voor het controlebestand.
#'   Indien ingevuld worden controles over de omzetting toegevoegd
#'   aan het controlebestand.
#' @param ... Eventuele extra parameters die worden doorgegeven.
#'
#' @return Een dataframe in long-formaat.
#'
#' @examples
#' data_wide <- data.frame(
#'   huishouden = c(1, 2),
#'   Persoon_a1.leeftijd = c(40, 30),
#'   Persoon_a2.leeftijd = c(38, 5),
#'   Persoon_a1.geslacht = c(1, 1),
#'   Persoon_a2.geslacht = c(2, 2),
#'   check.names = FALSE
#' )
#'
#' data_long <- zet_wide_naar_long(
#'   data = data_wide,
#'   prefix = "Persoon"
#' )
#'
#' data_long
#'
#' @export
zet_wide_naar_long <- function(
    data,
    prefix,
    omschrijving = NULL,
    ...) {

  starttijd <- Sys.time()

  data_naam <- deparse(
    substitute(data)
  )

  if (is.null(omschrijving)) {
    omschrijving <- paste0(
      data_naam,
      " van wide naar long omzetten"
    )
  }

  schrijf_log(
    "START zet_wide_naar_long"
  )

  resultaat <- tryCatch(

    withCallingHandlers(

      {

        if (!is.data.frame(data)) {
          stop(
            "'data' moet een data.frame zijn.",
            call. = FALSE
          )
        }

        n_rijen_voor <- nrow(data)
        n_kolommen_voor <- ncol(data)

        data <- integer64_naar_numeric(
          data
        )

        if (
          !is.character(prefix) ||
          length(prefix) != 1L ||
          is.na(prefix) ||
          !nzchar(prefix)
        ) {
          stop(
            "'prefix' moet een geldige tekstwaarde zijn.",
            call. = FALSE
          )
        }

        prefix_regex <- gsub(
          "([][{}()+*^$|\\\\?.])",
          "\\\\\\1",
          prefix
        )

        patroon <- paste0(
          "^",
          prefix_regex,
          "_a([0-9]+)\\.(.+)$"
        )

        matches <- regexec(
          patroon,
          names(data)
        )

        delen <- regmatches(
          names(data),
          matches
        )

        is_array <- lengths(
          delen
        ) == 3L

        if (!any(is_array)) {
          stop(
            paste0(
              "Geen arrayvariabelen gevonden met prefix '",
              prefix,
              "'."
            ),
            call. = FALSE
          )
        }

        vars_array <- names(data)[
          is_array
        ]

        vars_vast <- names(data)[
          !is_array
        ]

        delen_array <- delen[
          is_array
        ]

        array_nr <- as.integer(
          vapply(
            delen_array,
            `[`,
            character(1),
            2L
          )
        )

        var_naam <- vapply(
          delen_array,
          `[`,
          character(1),
          3L
        )

        n_max <- max(
          array_nr
        )

        if (
          !identical(
            sort(
              unique(array_nr)
            ),
            seq_len(n_max)
          )
        ) {
          stop(
            paste0(
              "De arraynummers moeten aaneengesloten lopen van 1 t/m ",
              n_max,
              "."
            ),
            call. = FALSE
          )
        }

        vars_kind <- unique(
          var_naam
        )

        for (var in vars_kind) {

          nummers <- sort(
            array_nr[
              var_naam == var
            ]
          )

          if (
            !identical(
              nummers,
              seq_len(n_max)
            )
          ) {
            stop(
              paste0(
                "Variabele '",
                var,
                "' komt niet voor voor alle arrays 1 t/m ",
                n_max,
                "."
              ),
              call. = FALSE
            )
          }
        }

        n <- nrow(data)

        rij_idx <- rep(
          seq_len(n),
          each = n_max
        )

        resultaat <- data[
          rij_idx,
          vars_vast,
          drop = FALSE
        ]

        nieuwe_kolommen <- vector(
          "list",
          length(vars_kind)
        )

        names(nieuwe_kolommen) <- paste0(
          prefix,
          ".",
          vars_kind
        )

        for (j in seq_along(vars_kind)) {

          var <- vars_kind[j]

          bron <- vars_array[
            match(
              paste(
                seq_len(n_max),
                var
              ),
              paste(
                array_nr,
                var_naam
              )
            )
          ]

          classes <- vapply(
            data[bron],
            function(x) {
              class(x)[1L]
            },
            character(1)
          )

          niet_logical <- which(
            classes != "logical"
          )

          if (length(niet_logical) > 0L) {

            voorbeeld <- data[[
              bron[niet_logical[1L]]
            ]]

          } else {

            voorbeeld <- data[[
              bron[1L]
            ]]
          }

          uit <- voorbeeld[
            rep(
              NA_integer_,
              n * n_max
            )
          ]

          for (i in seq_len(n_max)) {

            x <- data[[bron[i]]]

            if (
              is.logical(x) &&
              all(is.na(x)) &&
              !is.logical(voorbeeld)
            ) {

              if (is.integer(voorbeeld)) {

                x <- as.integer(x)

              } else if (is.numeric(voorbeeld)) {

                x <- as.numeric(x)

              } else if (is.character(voorbeeld)) {

                x <- as.character(x)
              }
            }

            idx <- seq.int(
              from = i,
              by = n_max,
              length.out = n
            )

            uit[idx] <- x
          }

          nieuwe_kolommen[[j]] <- uit
        }

        resultaat <- cbind(
          resultaat,
          as.data.frame(
            nieuwe_kolommen,
            optional = TRUE,
            check.names = FALSE
          )
        )

        attr(
          resultaat,
          "wide_col_order"
        ) <- names(data)

        attr(
          resultaat,
          "wide_n_max"
        ) <- n_max

        attr(
          resultaat,
          "wide_prefix"
        ) <- prefix

        rownames(resultaat) <- NULL

        resultaat <- koppel_meta(
          dataset = resultaat,
          verplicht = FALSE,
          dataset_naam = data_naam,
          interne_aanroep = TRUE,
          ...
        )

        meta_controle <- attr(
          resultaat,
          "meta_controle"
        )

        attr(
          resultaat,
          "meta_controle"
        ) <- NULL

        rij <- data.frame(
          omschrijving = c(
            omschrijving,
            "Aantal rijen voor omzetting",
            "Aantal rijen na omzetting",
            "Aantal kolommen voor omzetting",
            "Aantal kolommen na omzetting",
            "Aantal arrayposities",
            "Aantal arrayvariabelen",
            names(meta_controle)
          ),
          aantal = c(
            "",
            n_rijen_voor,
            nrow(resultaat),
            n_kolommen_voor,
            ncol(resultaat),
            n_max,
            length(vars_kind),
            unname(meta_controle)
          ),
          stringsAsFactors = FALSE
        )

        schrijf_r_logging(
          "controlebestand",
          rij
        )

        duur <- round(
          as.numeric(
            difftime(
              Sys.time(),
              starttijd,
              units = "secs"
            )
          ),
          2
        )

        schrijf_log_controle(
          omschrijving = omschrijving,
          waarden = stats::setNames(
            rij$aantal[-1L],
            rij$omschrijving[-1L]
          ),
          duur = duur
        )

        resultaat
      },

      warning = function(w) {

        schrijf_log(
          paste0(
            "WARNING zet_wide_naar_long\n",
            conditionMessage(w)
          )
        )
      }
    ),

    error = function(e) {

      schrijf_log(
        paste0(
          "ERROR zet_wide_naar_long\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE zet_wide_naar_long"
  )

  resultaat
}

#' Long data omzetten naar wide formaat
#'
#' Zet long-arrayvariabelen zoals `Prefix.X` om naar wide
#' arrayvariabelen zoals `Prefix_a1.X`, `Prefix_a2.X`, enzovoort.
#'
#' @param data Dataframe in long-formaat.
#' @param ouderid Naam van de variabele die de bovenliggende eenheid
#'   identificeert.
#' @param prefix Prefix van de arrayvariabelen.
#' @param nmax Maximaal aantal arrayposities.
#' @param omschrijving Optionele omschrijving voor het controlebestand.
#'   Indien ingevuld worden controles over de omzetting toegevoegd
#'   aan het controlebestand.
#' @param ... Eventuele parameters voor verdere functies.
#'
#' @return Een dataframe in wide-formaat.
#'
#' @examples
#' data_long <- data.frame(
#'   huishouden = c(1, 1, 2, 2),
#'   inkomen = c(1000, 1000, 2000, 2000),
#'   Persoon.leeftijd = c(40, 38, 30, 5),
#'   Persoon.geslacht = c(1, 2, 1, 2)
#' )
#'
#' data_wide <- zet_long_naar_wide(
#'   data = data_long,
#'   ouderid = "huishouden",
#'   prefix = "Persoon",
#'   nmax = 2
#' )
#'
#' data_wide
#'
#' @export
zet_long_naar_wide <- function(
    data,
    ouderid,
    prefix,
    nmax,
    omschrijving = NULL,
    ...) {

  starttijd <- Sys.time()

  data_naam <- deparse(
    substitute(data)
  )

  if (is.null(omschrijving)) {
    omschrijving <- paste0(
      data_naam,
      " van long naar wide omzetten"
    )
  }

  schrijf_log(
    "START zet_long_naar_wide"
  )

  resultaat <- tryCatch(

    withCallingHandlers(

      {

        if (!is.data.frame(data)) {
          stop(
            "'data' moet een data.frame zijn.",
            call. = FALSE
          )
        }

        n_rijen_voor <- nrow(data)
        n_kolommen_voor <- ncol(data)

        data <- integer64_naar_numeric(
          data
        )

        if (
          !is.character(ouderid) ||
          length(ouderid) != 1L ||
          is.na(ouderid) ||
          !nzchar(ouderid)
        ) {
          stop(
            "'ouderid' moet een geldige kolomnaam zijn.",
            call. = FALSE
          )
        }

        if (!ouderid %in% names(data)) {
          stop(
            paste0(
              "Ouder-ID '",
              ouderid,
              "' komt niet voor in de dataset."
            ),
            call. = FALSE
          )
        }

        if (
          !is.character(prefix) ||
          length(prefix) != 1L ||
          is.na(prefix) ||
          !nzchar(prefix)
        ) {
          stop(
            "'prefix' moet een geldige tekstwaarde zijn.",
            call. = FALSE
          )
        }

        if (
          length(nmax) != 1L ||
          is.na(nmax) ||
          nmax < 1L ||
          nmax != as.integer(nmax)
        ) {
          stop(
            "'nmax' moet een positief geheel getal zijn.",
            call. = FALSE
          )
        }

        nmax <- as.integer(
          nmax
        )

        prefix_punt <- paste0(
          prefix,
          "."
        )

        is_kind <- startsWith(
          names(data),
          prefix_punt
        )

        vars_kind <- names(data)[
          is_kind
        ]

        vars_ouder <- names(data)[
          !is_kind
        ]

        if (length(vars_kind) == 0L) {
          stop(
            paste0(
              "Geen kindvariabelen gevonden met prefix '",
              prefix_punt,
              "'."
            ),
            call. = FALSE
          )
        }

        vars_rest <- substring(
          vars_kind,
          nchar(prefix_punt) + 1L
        )

        ouder <- data[[ouderid]]

        if (anyNA(ouder)) {
          stop(
            paste0(
              "Ouder-ID '",
              ouderid,
              "' bevat NA-waarden."
            ),
            call. = FALSE
          )
        }

        ouder_uniek <- unique(
          ouder
        )

        ouder_idx <- match(
          ouder,
          ouder_uniek
        )

        n_ouders <- length(
          ouder_uniek
        )

        ord <- order(
          ouder_idx,
          seq_along(ouder_idx)
        )

        positie <- integer(
          nrow(data)
        )

        if (nrow(data) > 0L) {
          positie[ord] <- sequence(
            tabulate(
              ouder_idx,
              nbins = n_ouders
            )
          )
        }

        werkelijk_max <- if (
          length(positie) == 0L
        ) {
          0L
        } else {
          max(
            positie
          )
        }

        if (werkelijk_max > nmax) {
          stop(
            paste0(
              "Er zijn maximaal ",
              werkelijk_max,
              " kinderen per '",
              ouderid,
              "', terwijl nmax = ",
              nmax,
              "."
            ),
            call. = FALSE
          )
        }

        eerste_ouder <- match(
          ouder_uniek,
          ouder
        )

        resultaat <- data[
          eerste_ouder,
          vars_ouder,
          drop = FALSE
        ]

        nieuwe_kolommen <- vector(
          "list",
          length(vars_kind) * nmax
        )

        nieuwe_namen <- character(
          length(nieuwe_kolommen)
        )

        teller <- 0L

        for (j in seq_along(vars_kind)) {

          bron <- data[[vars_kind[j]]]

          for (i in seq_len(nmax)) {

            teller <- teller + 1L

            uit <- bron[
              rep(
                NA_integer_,
                n_ouders
              )
            ]

            idx <- positie == i

            uit[
              ouder_idx[idx]
            ] <- bron[idx]

            nieuwe_kolommen[[teller]] <- uit

            nieuwe_namen[teller] <- paste0(
              prefix,
              "_a",
              i,
              ".",
              vars_rest[j]
            )
          }
        }

        names(
          nieuwe_kolommen
        ) <- nieuwe_namen

        resultaat <- cbind(
          resultaat,
          as.data.frame(
            nieuwe_kolommen,
            optional = TRUE,
            check.names = FALSE
          )
        )

        originele_volgorde <- attr(
          data,
          "wide_col_order"
        )

        opgeslagen_prefix <- attr(
          data,
          "wide_prefix"
        )

        if (
          identical(
            opgeslagen_prefix,
            prefix
          ) &&
          !is.null(
            originele_volgorde
          ) &&
          setequal(
            names(resultaat),
            originele_volgorde
          )
        ) {
          resultaat <- resultaat[
            ,
            originele_volgorde,
            drop = FALSE
          ]
        }

        rownames(resultaat) <- NULL

        resultaat <- koppel_meta(
          dataset = resultaat,
          verplicht = FALSE,
          dataset_naam = data_naam,
          interne_aanroep = TRUE,
          ...
        )

        meta_controle <- attr(
          resultaat,
          "meta_controle"
        )

        attr(
          resultaat,
          "meta_controle"
        ) <- NULL

        rij <- data.frame(
          omschrijving = c(
            omschrijving,
            "Aantal rijen voor omzetting",
            "Aantal rijen na omzetting",
            "Aantal kolommen voor omzetting",
            "Aantal kolommen na omzetting",
            "Aantal ouder-eenheden",
            "Maximaal aantal posities aanwezig",
            "Ingestelde maximale arraygrootte",
            "Aantal arrayvariabelen",
            names(meta_controle)
          ),
          aantal = c(
            "",
            n_rijen_voor,
            nrow(resultaat),
            n_kolommen_voor,
            ncol(resultaat),
            n_ouders,
            werkelijk_max,
            nmax,
            length(vars_kind),
            unname(meta_controle)
          ),
          stringsAsFactors = FALSE
        )

        schrijf_r_logging(
          "controlebestand",
          rij
        )

        duur <- round(
          as.numeric(
            difftime(
              Sys.time(),
              starttijd,
              units = "secs"
            )
          ),
          2
        )

        schrijf_log_controle(
          omschrijving = omschrijving,
          waarden = stats::setNames(
            rij$aantal[-1L],
            rij$omschrijving[-1L]
          ),
          duur = duur
        )

        resultaat
      },

      warning = function(w) {

        schrijf_log(
          paste0(
            "WARNING zet_long_naar_wide\n",
            conditionMessage(w)
          )
        )
      }
    ),

    error = function(e) {

      schrijf_log(
        paste0(
          "ERROR zet_long_naar_wide\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE zet_long_naar_wide"
  )

  resultaat
}

#' Zet JSON om naar R-code voor afleidingen
#'
#' Zet door een chatbot gegenereerde JSON om naar R-code voor het uitvoeren
#' van afleidingen met GTST.
#'
#' De JSON kan onder andere tabellen en toekenningen bevatten. Deze worden
#' vertaald naar R-code met aanroepen van [maak_afleiding()]. Daarbij worden
#' onder andere condities, hulpvariabelen, loops, lookups en verschillende
#' formules verwerkt.
#'
#' @param json Character. JSON die moet worden omgezet naar R-code. De invoer
#'   mag eventueel zijn omgeven door een Markdown JSON-codeblok.
#' @param dataset Character. Naam van het datasetobject dat in de gegenereerde
#'   R-code moet worden gebruikt. Standaard `"data"`.
#' @param prefix Character. Optioneel voorvoegsel dat wordt toegevoegd aan
#'   variabelen waarvoor een prefix nodig is. Standaard `""`.
#'
#' @return Character met de gegenereerde R-code.
#'
#' @export
json_naar_afleiding <- function(
    json,
    dataset = "data",
    prefix = "") {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is nodig.", call. = FALSE)
  }

  json <- trimws(as.character(json))
  json <- sub(
    "^```(?:json)?\\s*", "", json,
    ignore.case = TRUE, perl = TRUE
  )
  json <- sub("\\s*```$", "", json, perl = TRUE)

  x <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  if (is.list(x) && !is.null(x$type)) x <- list(x)

  if (!is.list(x)) {
    stop(
      "JSON moet een object of array van objecten bevatten.",
      call. = FALSE
    )
  }

  normaliseer_tekst <- function(x) {
    if (is.null(x) || length(x) == 0L) {
      return("")
    }

    x <- as.character(x)[1]
    x <- gsub("[\r\n]+", " ", x)
    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    x <- chartr("\u2018\u2019", "''", x)
    x <- gsub("\u2026", "..", x, fixed = TRUE)
    trimws(x)
  }

  escape_regex <- function(x) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
  }

  is_getal <- function(x) {
    x <- trimws(x)
    grepl("^-?(?:0|[1-9]\\d*)(?:\\.\\d+)?$", x)
  }

  is_string_literal <- function(x) {
    x <- trimws(x)
    (startsWith(x, "'") && endsWith(x, "'")) ||
      (startsWith(x, '"') && endsWith(x, '"'))
  }

  format_string_literal <- function(x) {
    x <- trimws(x)

    if (startsWith(x, "'")) {
      inhoud <- substring(x, 2, nchar(x) - 1)
      inhoud <- gsub('"', '\\"', inhoud, fixed = TRUE)
      return(paste0('"', inhoud, '"'))
    }

    x
  }

  quote_string <- function(x) {
    paste0('"', gsub('"', '\\"', x, fixed = TRUE), '"')
  }

  is_extern_bestand <- function(x) {
    x <- normaliseer_tekst(x)
    grepl("\\.[A-Za-z0-9]{1,10}$", x, perl = TRUE)
  }

  hulp_namen <- character(0)

  for (obj in x) {
    is_hulp <- isTRUE(obj$hulpvariabele)
    if (!is_hulp) next

    type <- tolower(normaliseer_tekst(obj$type))

    if (identical(type, "tabel") && !is.null(obj$kolommen)) {
      uitvoer <- vapply(
        obj$kolommen,
        function(k) {
          if (
            identical(
              tolower(normaliseer_tekst(k$rol)),
              "uitvoer"
            )
          ) {
            normaliseer_tekst(k$naam)
          } else {
            ""
          }
        },
        character(1)
      )

      uitvoer <- uitvoer[nzchar(uitvoer)]
      hulp_namen <- c(hulp_namen, uitvoer)
    }

    if (identical(type, "toekenning") && !is.null(obj$uitvoer)) {
      hulp_namen <- c(
        hulp_namen,
        normaliseer_tekst(obj$uitvoer)
      )
    }
  }

  hulp_namen <- unique(hulp_namen[nzchar(hulp_namen)])

  if (length(hulp_namen) == 0L) {
    hulp_map <- stats::setNames(character(0), character(0))
  } else {
    hulp_map <- stats::setNames(
      paste0("tmp_", hulp_namen),
      hulp_namen
    )
  }

  is_hulpnaam <- function(x) {
    x <- normaliseer_tekst(x)

    length(hulp_map) > 0L &&
      !is.null(names(hulp_map)) &&
      x %in% names(hulp_map)
  }

  parse_index_variabele <- function(x) {
    x <- normaliseer_tekst(x)

    m <- regexec(
      "^(.+)\\[([A-Za-z][A-Za-z0-9_]*(?:\\s*[+-]\\s*\\d+)?)\\]$",
      x,
      perl = TRUE
    )

    delen <- regmatches(x, m)[[1]]
    if (length(delen) == 0L) {
      return(NULL)
    }

    index <- gsub("\\s+", "", trimws(delen[3]))

    list(
      variabele = trimws(delen[2]),
      index = index
    )
  }

  bekende_variabelen <- character(0)
  bron_map <- character(0)
  expliciete_lookup_variabelen <- character(0)

  for (obj in x) {
    if (!is.null(obj$lookup_variabelen)) {
      expliciete_lookup_variabelen <- c(
        expliciete_lookup_variabelen,
        vapply(
          obj$lookup_variabelen,
          normaliseer_tekst,
          character(1)
        )
      )
    }

    if (!is.null(obj$kolommen)) {
      for (k in obj$kolommen) {
        naam <- normaliseer_tekst(k$naam)
        bekende_variabelen <- c(bekende_variabelen, naam)

        if (!is.null(k$bron)) {
          bron <- normaliseer_tekst(k$bron)

          if (nzchar(naam) && is_extern_bestand(bron)) {
            bron_map[naam] <- bron
          }
        }
      }
    }

    if (!is.null(obj$uitvoer)) {
      bekende_variabelen <- c(
        bekende_variabelen,
        normaliseer_tekst(obj$uitvoer)
      )
    }
  }

  bekende_variabelen <- unique(
    bekende_variabelen[nzchar(bekende_variabelen)]
  )

  expliciete_lookup_variabelen <- unique(
    expliciete_lookup_variabelen[
      nzchar(expliciete_lookup_variabelen)
    ]
  )

  is_lookup_variabele <- function(x) {
    x <- normaliseer_tekst(x)

    (
      length(bron_map) > 0L &&
        x %in% names(bron_map)
    ) ||
      x %in% expliciete_lookup_variabelen
  }

  extern_bestand_bij_variabele <- function(x) {
    x <- normaliseer_tekst(x)

    if (is_extern_bestand(x)) {
      return(x)
    }

    if (is_lookup_variabele(x)) {
      return(unname(bron_map[x]))
    }

    NULL
  }

  lookup_match_condities <- function(code) {
    code <- paste(code, collapse = "\n")

    m <- gregexpr(
      'lookup\\("([^"]+)"\\)',
      code,
      perl = TRUE
    )

    hits <- regmatches(code, m)[[1]]

    if (
      length(hits) == 0L ||
        identical(hits, character(0)) ||
        identical(hits[1], "")
    ) {
      return(character(0))
    }

    lookup_namen <- sub(
      '^lookup\\("([^"]+)"\\)$',
      "\\1",
      hits,
      perl = TRUE
    )

    lookup_namen <- unique(lookup_namen)

    condities <- vapply(
      lookup_namen,
      function(naam) {
        bestand <- extern_bestand_bij_variabele(naam)

        if (is.null(bestand) || !nzchar(bestand)) {
          stop(
            paste0(
              "Lookup-variabele '", naam,
              "' wordt gebruikt via lookup(), maar het externe bronbestand ",
              "kan niet worden bepaald. Een lookup() mag alleen worden gebruikt ",
              "na een has_match() op het bijbehorende externe bestand."
            ),
            call. = FALSE
          )
        }

        paste0(
          "has_match(",
          "SLEUTEL",
          ", ",
          quote_string(bestand),
          ', "SLEUTEL")'
        )
      },
      character(1)
    )

    unique(condities)
  }

  is_variabele_referentie <- function(x) {
    x <- normaliseer_tekst(x)

    if (x %in% bekende_variabelen) {
      return(TRUE)
    }
    if (is_lookup_variabele(x)) {
      return(TRUE)
    }
    if (is_hulpnaam(x)) {
      return(TRUE)
    }
    if (!is.null(parse_index_variabele(x))) {
      return(TRUE)
    }

    if (
      grepl(
        "^[A-Za-z][A-Za-z0-9.]*_[A-Za-z0-9_.]+$",
        x
      )
    ) {
      return(TRUE)
    }

    if (
      grepl(
        "^[A-Za-z][A-Za-z0-9_]*(?:\\.[A-Za-z][A-Za-z0-9_]*)+$",
        x
      ) &&
        !is_extern_bestand(x)
    ) {
      return(TRUE)
    }

    FALSE
  }

  format_variabele <- function(x, kolomnaam = FALSE) {
    x <- normaliseer_tekst(x)

    if (!isTRUE(kolomnaam) && is_lookup_variabele(x)) {
      return(paste0('lookup("', x, '")'))
    }

    idx <- parse_index_variabele(x)

    if (!is.null(idx)) {
      basis <- idx$variabele
      index <- idx$index

      if (is_hulpnaam(basis)) {
        basis <- unname(hulp_map[basis])

        if (isTRUE(kolomnaam)) {
          return(
            paste0(
              'paste0("',
              basis,
              '_s", ',
              index,
              ")"
            )
          )
        }

        return(
          paste0(
            dataset,
            '[[paste0("',
            basis,
            '_s", ',
            index,
            ")]]"
          )
        )
      }

      if (!grepl("\\.", basis)) {
        basis <- paste0(prefix, basis)
      }

      if (isTRUE(kolomnaam)) {
        return(
          paste0(
            'paste0("',
            basis,
            '_s", ',
            index,
            ")"
          )
        )
      }

      return(
        paste0(
          dataset,
          '[[paste0("',
          basis,
          '_s", ',
          index,
          ")]]"
        )
      )
    }

    if (is_hulpnaam(x)) {
      return(unname(hulp_map[x]))
    }
    if (startsWith(x, "tmp_")) {
      return(x)
    }
    if (grepl("\\.", x)) {
      return(x)
    }

    paste0(prefix, x)
  }

  split_top_level <- function(x) {
    x <- normaliseer_tekst(x)
    if (!nzchar(x)) {
      return(character(0))
    }

    chars <- strsplit(x, "", fixed = TRUE)[[1]]

    stukken <- character(0)
    huidig <- ""
    niveau <- 0L
    quote <- ""

    for (ch in chars) {
      if (nzchar(quote)) {
        huidig <- paste0(huidig, ch)

        if (identical(ch, quote)) quote <- ""
        next
      }

      if (ch %in% c("'", '"')) {
        quote <- ch
        huidig <- paste0(huidig, ch)
        next
      }

      if (identical(ch, "(")) {
        niveau <- niveau + 1L
        huidig <- paste0(huidig, ch)
        next
      }

      if (identical(ch, ")")) {
        niveau <- max(0L, niveau - 1L)
        huidig <- paste0(huidig, ch)
        next
      }

      if (identical(ch, ",") && niveau == 0L) {
        stukken <- c(stukken, trimws(huidig))
        huidig <- ""
        next
      }

      huidig <- paste0(huidig, ch)
    }

    stukken <- c(stukken, trimws(huidig))
    stukken[nzchar(stukken)]
  }

  is_integer_range <- function(x) {
    grepl(
      "^-?\\d+\\.\\.-?\\d+$",
      gsub("\\s+", "", x)
    )
  }

  is_numeric_range <- function(x) {
    grepl(
      "^-?\\d+(?:\\.\\d+)?\\.\\.-?\\d+(?:\\.\\d+)?$",
      gsub("\\s+", "", x)
    )
  }

  is_character_range <- function(x) {
    grepl(
      "^(['\"]).+\\1\\.\\.(['\"]).+\\2$",
      trimws(x),
      perl = TRUE
    )
  }

  split_range <- function(x) {
    strsplit(x, "..", fixed = TRUE)[[1]]
  }

  prefix_formule_variabelen <- function(formule) {
    vars <- unique(
      c(
        bekende_variabelen,
        hulp_namen,
        expliciete_lookup_variabelen
      )
    )

    vars <- vars[nzchar(vars)]

    vars <- vars[
      !vapply(
        vars,
        is_extern_bestand,
        logical(1)
      )
    ]

    vars <- vars[
      order(
        nchar(vars),
        decreasing = TRUE
      )
    ]

    for (v in vars) {
      if (is.null(parse_index_variabele(v))) next

      vervanging <- format_variabele(v)

      formule <- gsub(
        escape_regex(v),
        vervanging,
        formule,
        perl = TRUE
      )
    }

    for (v in vars) {
      if (!is.null(parse_index_variabele(v))) next

      vervanging <- format_variabele(v)

      patroon <- paste0(
        "(?<![A-Za-z0-9_.])",
        escape_regex(v),
        "(?![A-Za-z0-9_.])"
      )

      formule <- gsub(
        patroon,
        vervanging,
        formule,
        perl = TRUE
      )
    }

    tokens <- unique(
      regmatches(
        formule,
        gregexpr(
          "(?<![A-Za-z0-9_.])[A-Za-z][A-Za-z0-9.]*_[A-Za-z0-9_.]+(?![A-Za-z0-9_.])",
          formule,
          perl = TRUE
        )
      )[[1]]
    )

    tokens <- tokens[nzchar(tokens)]

    for (v in tokens) {
      if (
        startsWith(v, prefix) ||
          startsWith(v, "tmp_")
      ) {
        next
      }

      patroon <- paste0(
        "(?<![A-Za-z0-9_.])",
        escape_regex(v),
        "(?![A-Za-z0-9_.])"
      )

      formule <- gsub(
        patroon,
        format_variabele(v),
        formule,
        perl = TRUE
      )
    }

    formule
  }

  is_formule <- function(x) {
    x <- normaliseer_tekst(x)

    grepl("\\w+\\s*\\(", x) ||
      grepl(
        "\\sMOD\\s",
        paste0(" ", x, " "),
        ignore.case = TRUE
      ) ||
      grepl("[+*/]", x) ||
      grepl(
        "(?<!\\.)-(?!\\.)",
        x,
        perl = TRUE
      )
  }

  format_datumfuncties <- function(formule) {
    formule <- normaliseer_tekst(formule)

    patroon <- paste0(
      "TODATE\\s*\\(",
      "\\s*([^,()]+?)\\s*,",
      "\\s*([^,()]+?)\\s*,",
      "\\s*([^,()]+?)\\s*",
      "\\)",
      "\\s*\\+\\s*",
      "\\(\\s*0\\s*,\\s*0\\s*,\\s*",
      "RANDOM\\s*\\(\\s*(\\d+)\\s*\\)",
      "\\s*\\)"
    )

    repeat {
      m <- regexec(
        patroon,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )

      delen <- regmatches(formule, m)[[1]]
      if (length(delen) == 0L) break

      jaar <- trimws(delen[2])
      maand <- trimws(delen[3])
      dag <- trimws(delen[4])
      n <- trimws(delen[5])

      vervanging <- paste0(
        "as.Date(",
        "paste(",
        dag,
        ", ",
        maand,
        ", ",
        jaar,
        ', sep = "-"), ',
        'format = "%d-%m-%Y"',
        ") + ",
        "sample.int(",
        n,
        " + 1L, 1L) - 1L"
      )

      formule <- sub(
        patroon,
        vervanging,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )
    }

    patroon <- paste0(
      "TODATE\\s*\\(",
      "\\s*([^,()]+?)\\s*,",
      "\\s*0+\\s*,",
      "\\s*0+\\s*",
      "\\)",
      "\\s*\\+\\s*",
      "\\(\\s*0\\s*,",
      "\\s*([^,()]+?)\\s*,",
      "\\s*([^,()]+?)\\s*",
      "\\)"
    )

    repeat {
      m <- regexec(
        patroon,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )

      delen <- regmatches(formule, m)[[1]]
      if (length(delen) == 0L) break

      jaar <- trimws(delen[2])
      maand <- trimws(delen[3])
      dag <- trimws(delen[4])

      vervanging <- paste0(
        "as.Date(",
        "paste(",
        dag,
        ", ",
        maand,
        ", ",
        jaar,
        ', sep = "-"), ',
        'format = "%d-%m-%Y"',
        ")"
      )

      formule <- sub(
        patroon,
        vervanging,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )
    }

    patroon <- paste0(
      "TODATE\\s*\\(",
      "\\s*([^,()]+?)\\s*,",
      "\\s*([^,()]+?)\\s*,",
      "\\s*([^,()]+?)\\s*",
      "\\)",
      "\\s*\\+\\s*",
      "\\(\\s*0\\s*,\\s*0\\s*,",
      "\\s*([^,()]+?)\\s*",
      "\\)"
    )

    repeat {
      m <- regexec(
        patroon,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )

      delen <- regmatches(formule, m)[[1]]
      if (length(delen) == 0L) break

      jaar <- trimws(delen[2])
      maand <- trimws(delen[3])
      dag <- trimws(delen[4])
      dagen <- trimws(delen[5])

      vervanging <- paste0(
        "as.Date(",
        "paste(",
        dag,
        ", ",
        maand,
        ", ",
        jaar,
        ', sep = "-"), ',
        'format = "%d-%m-%Y"',
        ") + (",
        dagen,
        ")"
      )

      formule <- sub(
        patroon,
        vervanging,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )
    }

    patroon <- paste0(
      "TODATE\\s*\\(",
      "\\s*([^,()]+?)\\s*,",
      "\\s*([^,()]+?)\\s*,",
      "\\s*([^,()]+?)\\s*",
      "\\)"
    )

    repeat {
      m <- regexec(
        patroon,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )

      delen <- regmatches(formule, m)[[1]]
      if (length(delen) == 0L) break

      jaar <- trimws(delen[2])
      maand <- trimws(delen[3])
      dag <- trimws(delen[4])

      vervanging <- paste0(
        "as.Date(",
        "paste(",
        dag,
        ", ",
        maand,
        ", ",
        jaar,
        ', sep = "-"), ',
        'format = "%d-%m-%Y"',
        ")"
      )

      formule <- sub(
        patroon,
        vervanging,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )
    }

    formule <- gsub(
      "RANDOM\\s*\\(\\s*([0-9]+)\\s*\\)",
      "sample.int(\\1 + 1L, 1L) - 1L",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule
  }

  format_formule <- function(formule) {
    formule <- normaliseer_tekst(formule)
    formule <- format_datumfuncties(formule)
    formule <- prefix_formule_variabelen(formule)

    formule <- gsub(
      "\\bTRUNC\\s*\\(",
      "floor(",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    patroon_abstime <- paste0(
      "\\(?\\s*",
      "ABSTIME\\s*\\(([^()]+)\\)",
      "\\s*-\\s*",
      "ABSTIME\\s*\\(([^()]+)\\)",
      "\\s*\\)?",
      "\\s*/\\s*60000"
    )

    repeat {
      m <- regexec(
        patroon_abstime,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )

      delen <- regmatches(formule, m)[[1]]
      if (length(delen) == 0L) break

      eind <- trimws(delen[2])
      start <- trimws(delen[3])

      vervanging <- paste0(
        "round(as.numeric(",
        "strptime(",
        eind,
        ', format = "%H:%M:%S") - ',
        "strptime(",
        start,
        ', format = "%H:%M:%S"), ',
        'units = "mins"))'
      )

      formule <- sub(
        patroon_abstime,
        vervanging,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )
    }

    formule <- gsub(
      "ABSTIME\\s*\\(([^()]+)\\)",
      'strptime(\\1, format = "%H:%M:%S")',
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule <- gsub(
      "\\b(?:RF|REFUSAL)\\b",
      "-98",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule <- gsub(
      "\\bDK\\b",
      "-99",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule <- gsub(
      "\\bMIN\\s*\\(",
      "pmin(",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule <- gsub(
      "\\bMAX\\s*\\(",
      "pmax(",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule <- gsub(
      "\\bVAL\\s*\\(",
      "as.numeric(",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule <- gsub(
      "\\bSTR\\s*\\(",
      "as.character(",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule <- gsub(
      "\\bMOD\\b",
      "%%",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule <- gsub(
      "DAY\\s*\\(([^()]+)\\)",
      "as.numeric(substr(\\1, 1, 2))",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule <- gsub(
      "MONTH\\s*\\(([^()]+)\\)",
      "as.numeric(substr(\\1, 4, 5))",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    formule <- gsub(
      "YEAR\\s*\\(([^()]+)\\)",
      "as.numeric(substr(\\1, 7, 10))",
      formule,
      ignore.case = TRUE,
      perl = TRUE
    )

    patroon_substring <- paste0(
      "SUBSTRING\\s*\\(",
      "\\s*([^,()]+?)\\s*,",
      "\\s*([^,()]+?)\\s*,",
      "\\s*([^,()]+?)\\s*",
      "\\)"
    )

    repeat {
      m <- regexec(
        patroon_substring,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )

      delen <- regmatches(formule, m)[[1]]
      if (length(delen) == 0L) break

      var <- trimws(delen[2])
      start <- trimws(delen[3])
      lengte <- trimws(delen[4])

      if (
        grepl("^\\d+$", start) &&
          grepl("^\\d+$", lengte)
      ) {
        einde <- as.integer(start) +
          as.integer(lengte) -
          1L

        vervanging <- paste0(
          "substr(",
          var,
          ", ",
          start,
          ", ",
          einde,
          ")"
        )
      } else {
        vervanging <- paste0(
          "substr(",
          var,
          ", ",
          start,
          ", (",
          start,
          ") + (",
          lengte,
          ") - 1L)"
        )
      }

      formule <- sub(
        patroon_substring,
        vervanging,
        formule,
        ignore.case = TRUE,
        perl = TRUE
      )
    }

    formule <- prefix_formule_variabelen(formule)
    formule
  }

  format_rhs <- function(x) {
    x <- normaliseer_tekst(x)

    if (!nzchar(x)) {
      return("NA")
    }
    if (toupper(x) == "EMPTY") {
      return("NA")
    }
    if (toupper(x) %in% c("RF", "REFUSAL")) {
      return("-98")
    }
    if (toupper(x) == "DK") {
      return("-99")
    }
    if (is_getal(x)) {
      return(x)
    }
    if (is_string_literal(x)) {
      return(format_string_literal(x))
    }
    if (is_extern_bestand(x)) {
      return(quote_string(x))
    }

    if (
      x %in% bekende_variabelen ||
        is_lookup_variabele(x) ||
        is_hulpnaam(x) ||
        !is.null(parse_index_variabele(x)) ||
        is_variabele_referentie(x)
    ) {
      return(format_variabele(x))
    }

    if (is_formule(x)) {
      return(format_formule(x))
    }

    quote_string(x)
  }

  format_conditie <- function(variabele, waarde) {
    variabele <- normaliseer_tekst(variabele)
    waarde <- normaliseer_tekst(waarde)

    if (!nzchar(waarde) || identical(waarde, "-")) {
      return(NULL)
    }

    waarde_lager <- tolower(trimws(waarde))

    if (
      waarde_lager %in% c(
        "koppeling",
        "geen koppeling"
      )
    ) {
      bestand <- extern_bestand_bij_variabele(variabele)

      if (!is.null(bestand)) {
        match_code <- paste0(
          "has_match(",
          "SLEUTEL",
          ", ",
          quote_string(bestand),
          ', "SLEUTEL")'
        )

        if (identical(waarde_lager, "geen koppeling")) {
          return(paste0("!", match_code))
        }

        return(match_code)
      }
    }

    var_r <- format_variabele(variabele)

    if (
      grepl(
        "\\s+AND\\s+",
        waarde,
        ignore.case = TRUE,
        perl = TRUE
      )
    ) {
      waarde_split <- gsub(
        "\\s+AND\\s+",
        "\034",
        waarde,
        ignore.case = TRUE,
        perl = TRUE
      )

      onderdelen <- strsplit(
        waarde_split,
        "\034",
        fixed = TRUE
      )[[1]]

      onderdelen <- trimws(onderdelen)

      condities_and <- vapply(
        onderdelen,
        function(deel) {
          conditie <- format_conditie(
            variabele,
            deel
          )

          if (is.null(conditie)) {
            return("TRUE")
          }

          paste0(
            "(",
            conditie,
            ")"
          )
        },
        character(1)
      )

      return(
        paste(
          condities_and,
          collapse = " & "
        )
      )
    }

    if (
      grepl(
        "^<>\\s*EMPTY$",
        waarde,
        ignore.case = TRUE
      )
    ) {
      return(paste0("!is.na(", var_r, ")"))
    }

    if (
      grepl(
        "^(?:=\\s*)?EMPTY$",
        waarde,
        ignore.case = TRUE
      )
    ) {
      return(paste0("is.na(", var_r, ")"))
    }

    m <- regexec(
      "^(>=|<=|<>|>|<|=|\u2265|\u2264)\\s*(.+)$",
      waarde,
      perl = TRUE
    )

    delen <- regmatches(waarde, m)[[1]]

    if (length(delen) > 0L) {
      operator <- delen[2]
      rhs <- trimws(delen[3])

      operator <- switch(operator,
        "\u2265" = ">=",
        "\u2264" = "<=",
        operator
      )

      rhs_r <- format_rhs(rhs)

      if (identical(operator, "=")) {
        return(
          paste0(
            var_r,
            " %in% ",
            rhs_r
          )
        )
      }

      if (identical(operator, "<>")) {
        if (
          rhs %in% bekende_variabelen ||
            is_lookup_variabele(rhs) ||
            is_hulpnaam(rhs) ||
            !is.null(parse_index_variabele(rhs)) ||
            is_variabele_referentie(rhs) ||
            is_formule(rhs)
        ) {
          return(
            paste0(
              var_r,
              " != ",
              rhs_r
            )
          )
        }

        return(
          paste0(
            "!(",
            var_r,
            " %in% ",
            rhs_r,
            ")"
          )
        )
      }

      return(
        paste0(
          var_r,
          " ",
          operator,
          " ",
          rhs_r
        )
      )
    }

    onderdelen <- split_top_level(waarde)

    condities <- character(0)
    losse_waarden <- character(0)

    for (deel in onderdelen) {
      deel <- normaliseer_tekst(deel)
      if (!nzchar(deel)) next

      if (toupper(deel) == "EMPTY") {
        condities <- c(
          condities,
          paste0("is.na(", var_r, ")")
        )
        next
      }

      if (toupper(deel) %in% c("RF", "REFUSAL")) {
        losse_waarden <- c(losse_waarden, "-98")
        next
      }

      if (toupper(deel) == "DK") {
        losse_waarden <- c(losse_waarden, "-99")
        next
      }

      if (is_integer_range(deel)) {
        grenzen <- split_range(
          gsub("\\s+", "", deel)
        )

        heeft_voorloopnul <- function(x) {
          x <- sub("^-", "", x)

          nchar(x) > 1L &&
            startsWith(x, "0")
        }

        range_met_voorloopnul <-
          heeft_voorloopnul(grenzen[1]) ||
            heeft_voorloopnul(grenzen[2])

        if (range_met_voorloopnul) {
          ondergrens <- as.character(
            as.integer(grenzen[1])
          )

          bovengrens <- as.character(
            as.integer(grenzen[2])
          )

          condities <- c(
            condities,
            paste0(
              "as.numeric(",
              var_r,
              ") %in% ",
              ondergrens,
              ":",
              bovengrens
            )
          )
        } else {
          condities <- c(
            condities,
            paste0(
              var_r,
              " %in% ",
              grenzen[1],
              ":",
              grenzen[2]
            )
          )
        }

        next
      }

      if (
        is_numeric_range(deel) &&
          !is_integer_range(deel)
      ) {
        grenzen <- split_range(
          gsub("\\s+", "", deel)
        )

        condities <- c(
          condities,
          paste0(
            "(",
            var_r,
            " >= ",
            grenzen[1],
            " & ",
            var_r,
            " <= ",
            grenzen[2],
            ")"
          )
        )

        next
      }

      if (is_character_range(deel)) {
        grenzen <- split_range(deel)

        condities <- c(
          condities,
          paste0(
            "(",
            var_r,
            " >= ",
            format_string_literal(grenzen[1]),
            " & ",
            var_r,
            " <= ",
            format_string_literal(grenzen[2]),
            ")"
          )
        )

        next
      }

      losse_waarden <- c(
        losse_waarden,
        format_rhs(deel)
      )
    }

    if (length(losse_waarden) > 0L) {
      rhs <- if (length(losse_waarden) == 1L) {
        losse_waarden
      } else {
        paste0(
          "c(",
          paste(
            losse_waarden,
            collapse = ", "
          ),
          ")"
        )
      }

      condities <- c(
        condities,
        paste0(
          var_r,
          " %in% ",
          rhs
        )
      )
    }

    if (length(condities) == 0L) {
      return(NULL)
    }
    if (length(condities) == 1L) {
      return(condities)
    }

    paste0(
      "(",
      paste(
        condities,
        collapse = " | "
      ),
      ")"
    )
  }

  format_uitvoerwaarde <- function(waarde, uitvoer) {
    waarde <- normaliseer_tekst(waarde)

    if (!nzchar(waarde) || identical(waarde, "-")) {
      return(format_variabele(uitvoer))
    }

    if (toupper(waarde) == "EMPTY") {
      return("NA")
    }
    if (toupper(waarde) %in% c("RF", "REFUSAL")) {
      return("-98")
    }
    if (toupper(waarde) == "DK") {
      return("-99")
    }
    if (is_getal(waarde)) {
      return(waarde)
    }
    if (is_string_literal(waarde)) {
      return(format_string_literal(waarde))
    }
    if (is_extern_bestand(waarde)) {
      return(quote_string(waarde))
    }

    if (
      waarde %in% bekende_variabelen ||
        is_lookup_variabele(waarde) ||
        is_hulpnaam(waarde) ||
        !is.null(parse_index_variabele(waarde))
    ) {
      return(format_variabele(waarde))
    }

    if (is_formule(waarde)) {
      return(format_formule(waarde))
    }

    quote_string(waarde)
  }

  analyseer_loop <- function(loop) {
    index <- normaliseer_tekst(loop$index)
    van <- normaliseer_tekst(loop$van)
    tot <- normaliseer_tekst(loop$tot)

    if (!is_getal(van)) {
      stop(
        paste0("Niet-ondersteunde loopstart: ", van),
        call. = FALSE
      )
    }

    m <- regexec(
      "^MIN\\s*\\(\\s*(.+?)\\s*,\\s*(.+?)\\s*\\)$",
      tot,
      ignore.case = TRUE,
      perl = TRUE
    )

    delen <- regmatches(tot, m)[[1]]

    if (length(delen) > 0L) {
      arg1 <- trimws(delen[2])
      arg2 <- trimws(delen[3])

      arg1_getal <- is_getal(arg1)
      arg2_getal <- is_getal(arg2)

      if (!arg1_getal && arg2_getal) {
        grens_var <- arg1
        max_n <- as.integer(arg2)

        return(
          list(
            index = index,
            seq_code = paste0(
              "seq.int(",
              van,
              ", ",
              max_n,
              ")"
            ),
            rij_conditie = paste0(
              format_variabele(grens_var),
              " >= ",
              index
            )
          )
        )
      }

      if (arg1_getal && !arg2_getal) {
        max_n <- as.integer(arg1)
        grens_var <- arg2

        return(
          list(
            index = index,
            seq_code = paste0(
              "seq.int(",
              van,
              ", ",
              max_n,
              ")"
            ),
            rij_conditie = paste0(
              format_variabele(grens_var),
              " >= ",
              index
            )
          )
        )
      }

      if (arg1_getal && arg2_getal) {
        max_n <- min(
          as.integer(arg1),
          as.integer(arg2)
        )

        return(
          list(
            index = index,
            seq_code = paste0(
              "seq.int(",
              van,
              ", ",
              max_n,
              ")"
            ),
            rij_conditie = NULL
          )
        )
      }

      stop(
        paste0(
          "Loopgrens met MIN() wordt niet ondersteund wanneer ",
          "beide argumenten variabelen zijn: ",
          tot
        ),
        call. = FALSE
      )
    }

    if (is_getal(tot)) {
      return(
        list(
          index = index,
          seq_code = paste0(
            "seq.int(",
            van,
            ", ",
            tot,
            ")"
          ),
          rij_conditie = NULL
        )
      )
    }

    stop(
      paste0("Niet-ondersteunde loopgrens: ", tot),
      call. = FALSE
    )
  }

  format_loop_conditie <- function(x) {
    x <- normaliseer_tekst(x)
    if (!nzchar(x)) {
      return(NULL)
    }

    x <- gsub("<>", "!=", x, fixed = TRUE)
    x <- gsub("\u2260", "!=", x, fixed = TRUE)
    x
  }

  omwikkel_met_loops <- function(code, obj) {
    loops <- list()

    if (
      !is.null(obj$loops) &&
        length(obj$loops) > 0L
    ) {
      loops <- lapply(
        obj$loops,
        analyseer_loop
      )
    }

    loop_conditie <- NULL

    if (!is.null(obj$loop_conditie)) {
      loop_conditie <- format_loop_conditie(
        obj$loop_conditie
      )
    }

    if (
      !is.null(loop_conditie) &&
        nzchar(loop_conditie)
    ) {
      code <- paste0(
        "if (",
        loop_conditie,
        ") {\n",
        "  ",
        gsub(
          "\n",
          "\n  ",
          code,
          fixed = TRUE
        ),
        "\n}"
      )
    }

    if (length(loops) > 0L) {
      for (loop in rev(loops)) {
        code <- paste0(
          "for (",
          loop$index,
          " in ",
          loop$seq_code,
          ") {\n",
          "  ",
          gsub(
            "\n",
            "\n  ",
            code,
            fixed = TRUE
          ),
          "\n}"
        )
      }
    }

    code
  }

  verwerk_toekenning <- function(obj, nummer) {
    uitvoer <- normaliseer_tekst(obj$uitvoer)
    waarde <- normaliseer_tekst(obj$waarde)

    if (!nzchar(uitvoer)) {
      stop(
        paste0(
          "Toekenning ",
          nummer,
          " heeft geen uitvoervariabele."
        ),
        call. = FALSE
      )
    }

    is_hulp <- isTRUE(obj$hulpvariabele)
    idx <- parse_index_variabele(uitvoer)

    if (!is.null(idx)) {
      doel <- format_variabele(
        uitvoer,
        kolomnaam = TRUE
      )

      waarde_r <- format_uitvoerwaarde(
        waarde,
        uitvoer
      )

      extra <- if (is_hulp) {
        ",\n\n  behoud_kolommen = TRUE"
      } else {
        ""
      }

      code <- paste0(
        dataset,
        " <- maak_afleiding(\n",
        "  ",
        dataset,
        ",\n",
        "  ",
        doel,
        ",\n\n",
        "  TRUE,\n",
        "  ",
        waarde_r,
        extra,
        "\n",
        ")"
      )

      return(
        omwikkel_met_loops(
          code,
          obj
        )
      )
    }

    if (is_hulp) {
      hulp_naam <- format_variabele(uitvoer)

      waarde_r <- format_uitvoerwaarde(
        waarde,
        uitvoer
      )

      code <- paste0(
        hulp_naam,
        " <- maak_afleiding(\n",
        "  ",
        dataset,
        ",\n",
        '  "',
        hulp_naam,
        '",\n\n',
        "  TRUE,\n",
        "  ",
        waarde_r,
        ",\n\n",
        "  behoud_kolommen = TRUE\n",
        ")"
      )

      return(
        omwikkel_met_loops(
          code,
          obj
        )
      )
    }

    uitvoer_r <- paste0(
      '"',
      format_variabele(uitvoer),
      '"'
    )

    waarde_r <- format_uitvoerwaarde(
      waarde,
      uitvoer
    )

    code <- paste0(
      dataset,
      " <- maak_afleiding(\n",
      "  ",
      dataset,
      ",\n",
      "  ",
      uitvoer_r,
      ",\n\n",
      "  TRUE,\n",
      "  ",
      waarde_r,
      "\n",
      ")"
    )

    omwikkel_met_loops(
      code,
      obj
    )
  }

  format_na_actie <- function(
      actie,
      conditie = "TRUE") {
    if (
      is.null(actie$uitvoer) ||
        is.null(actie$waarde)
    ) {
      stop(
        "Iedere na_actie moet 'uitvoer' en 'waarde' bevatten.",
        call. = FALSE
      )
    }

    uitvoer <- normaliseer_tekst(actie$uitvoer)
    waarde <- normaliseer_tekst(actie$waarde)

    if (!nzchar(uitvoer)) {
      stop(
        "Een na_actie heeft een lege uitvoervariabele.",
        call. = FALSE
      )
    }

    if (
      is.null(conditie) ||
        !nzchar(trimws(conditie))
    ) {
      conditie <- "TRUE"
    }

    idx <- parse_index_variabele(uitvoer)

    waarde_r <- format_uitvoerwaarde(
      waarde,
      uitvoer
    )

    basis <- if (!is.null(idx)) {
      idx$variabele
    } else {
      uitvoer
    }

    if (is_hulpnaam(basis)) {
      doel <- if (!is.null(idx)) {
        format_variabele(
          uitvoer,
          kolomnaam = TRUE
        )
      } else {
        paste0(
          '"',
          format_variabele(uitvoer),
          '"'
        )
      }

      if (!is.null(idx)) {
        return(
          paste0(
            dataset,
            " <- maak_afleiding(\n",
            "  ",
            dataset,
            ",\n",
            "  ",
            doel,
            ",\n\n",
            "  ",
            conditie,
            ",\n",
            "  ",
            waarde_r,
            ",\n\n",
            "  behoud_kolommen = TRUE\n",
            ")"
          )
        )
      }

      hulp_object <- format_variabele(uitvoer)

      return(
        paste0(
          hulp_object,
          " <- maak_afleiding(\n",
          "  ",
          dataset,
          ",\n",
          "  ",
          doel,
          ",\n\n",
          "  ",
          conditie,
          ",\n",
          "  ",
          waarde_r,
          ",\n\n",
          "  behoud_kolommen = TRUE\n",
          ")"
        )
      )
    }

    doel <- if (!is.null(idx)) {
      format_variabele(
        uitvoer,
        kolomnaam = TRUE
      )
    } else {
      paste0(
        '"',
        format_variabele(uitvoer),
        '"'
      )
    }

    paste0(
      dataset,
      " <- maak_afleiding(\n",
      "  ",
      dataset,
      ",\n",
      "  ",
      doel,
      ",\n\n",
      "  ",
      conditie,
      ",\n",
      "  ",
      waarde_r,
      "\n",
      ")"
    )
  }

  verwerk_tabel <- function(obj, tabel_nummer) {
    kolommen <- obj$kolommen
    regels <- obj$regels

    if (
      is.null(kolommen) ||
        length(kolommen) == 0L
    ) {
      stop(
        paste0(
          "Tabel ",
          tabel_nummer,
          " heeft geen kolommen."
        ),
        call. = FALSE
      )
    }

    if (is.null(regels)) regels <- list()

    posities <- vapply(
      kolommen,
      function(k) as.integer(k$positie),
      integer(1)
    )

    volgorde <- order(posities)
    kolommen <- kolommen[volgorde]

    rollen <- vapply(
      kolommen,
      function(k) {
        tolower(
          normaliseer_tekst(k$rol)
        )
      },
      character(1)
    )

    namen <- vapply(
      kolommen,
      function(k) normaliseer_tekst(k$naam),
      character(1)
    )

    geldige_rollen <- c(
      "invoer",
      "uitvoer",
      "actie",
      "negeren"
    )

    onbekend <- !rollen %in% geldige_rollen

    if (any(onbekend)) {
      stop(
        paste0(
          "Tabel ",
          tabel_nummer,
          " bevat onbekende kolomrol(len): ",
          paste(
            unique(rollen[onbekend]),
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    invoer_idx <- which(rollen == "invoer")
    uitvoer_idx <- which(rollen == "uitvoer")
    actie_idx <- which(rollen == "actie")

    if (length(uitvoer_idx) == 0L) {
      stop(
        paste0(
          "Tabel ",
          tabel_nummer,
          " heeft geen uitvoervariabele."
        ),
        call. = FALSE
      )
    }

    is_hulp <- isTRUE(obj$hulpvariabele)

    if (
      is_hulp &&
        length(uitvoer_idx) != 1L
    ) {
      stop(
        paste0(
          "Hulpvariabelentabel ",
          tabel_nummer,
          " moet precies een uitvoervariabele bevatten."
        ),
        call. = FALSE
      )
    }

    uitvoer_namen <- namen[uitvoer_idx]
    loops <- list()

    if (
      !is.null(obj$loops) &&
        length(obj$loops) > 0L
    ) {
      loops <- lapply(
        obj$loops,
        analyseer_loop
      )
    }

    loop_rij_condities <- unlist(
      lapply(
        loops,
        function(z) z$rij_conditie
      ),
      use.names = FALSE
    )

    loop_rij_condities <- loop_rij_condities[
      nzchar(loop_rij_condities)
    ]

    regels_code <- character(0)
    regelcondities_code <- character(0)
    eerdere_regelcondities <- character(0)
    eerdere_invoer_waarden <- list()

    na_acties_per_regel <- vector(
      "list",
      length(regels)
    )

    for (i in seq_along(regels)) {
      regel_obj <- regels[[i]]

      if (
        is.list(regel_obj) &&
          !is.null(names(regel_obj)) &&
          "waarden" %in% names(regel_obj)
      ) {
        regel <- regel_obj$waarden

        if (!is.null(regel_obj$na_acties)) {
          na_acties_per_regel[i] <- list(
            regel_obj$na_acties
          )
        }
      } else {
        regel <- regel_obj
      }

      regel <- vapply(
        regel,
        normaliseer_tekst,
        character(1)
      )

      if (length(regel) != length(kolommen)) {
        stop(
          paste0(
            "Regel ",
            i,
            " van tabel ",
            tabel_nummer,
            " heeft ",
            length(regel),
            " waarden terwijl ",
            length(kolommen),
            " kolommen verwacht worden."
          ),
          call. = FALSE
        )
      }

      regel <- regel[volgorde]

      if (length(actie_idx) > 0L) {
        acties_regel <- na_acties_per_regel[[i]]

        for (j in actie_idx) {
          actie_waarde <- normaliseer_tekst(
            regel[j]
          )

          if (
            !nzchar(actie_waarde) ||
              identical(actie_waarde, "-")
          ) {
            next
          }

          acties_regel <- c(
            acties_regel,
            list(
              list(
                uitvoer = namen[j],
                waarde = actie_waarde
              )
            )
          )
        }

        na_acties_per_regel[i] <- list(
          acties_regel
        )
      }

      invoer_waarden <- vapply(
        regel[invoer_idx],
        normaliseer_tekst,
        character(1)
      )

      else_flags <- toupper(
        trimws(invoer_waarden)
      ) == "ELSE"

      regel_is_else <- any(else_flags)

      format_invoerkolom <- function(j, waarde) {
        conditie <- format_conditie(
          namen[j],
          waarde
        )

        if (is.null(conditie)) {
          return(NULL)
        }

        lookup_gates <- lookup_match_condities(
          conditie
        )

        if (length(lookup_gates) > 0L) {
          conditie <- paste(
            c(
              lookup_gates,
              conditie
            ),
            collapse = " &\n    "
          )
        }

        conditie
      }

      regelcondities <- character(0)

      if (!regel_is_else) {
        for (j in invoer_idx) {
          conditie <- format_invoerkolom(
            j,
            regel[j]
          )

          if (!is.null(conditie)) {
            regelcondities <- c(
              regelcondities,
              conditie
            )
          }
        }

        regelconditie_code <- if (
          length(regelcondities) == 0L
        ) {
          "TRUE"
        } else {
          paste(
            regelcondities,
            collapse = " &\n    "
          )
        }
      } else {
        else_lokaal <- which(else_flags)[1]

        for (k in seq_along(invoer_idx)) {
          if (else_flags[k]) next

          j <- invoer_idx[k]

          conditie <- format_invoerkolom(
            j,
            regel[j]
          )

          if (!is.null(conditie)) {
            regelcondities <- c(
              regelcondities,
              conditie
            )
          }
        }

        relevante_indexen <- seq_along(
          eerdere_regelcondities
        )

        if (
          else_lokaal > 1L &&
            length(eerdere_invoer_waarden) > 0L
        ) {
          prefix_posities <- seq_len(
            else_lokaal - 1L
          )

          relevante_indexen <- relevante_indexen[
            vapply(
              relevante_indexen,
              function(k) {
                eerder <- eerdere_invoer_waarden[[k]]

                if (length(eerder) < max(prefix_posities)) {
                  return(FALSE)
                }

                all(
                  eerder[prefix_posities] ==
                    invoer_waarden[prefix_posities]
                )
              },
              logical(1)
            )
          ]
        }

        relevante_eerdere <- eerdere_regelcondities[
          relevante_indexen
        ]

        if (length(relevante_eerdere) > 0L) {
          eerdere_code <- paste0(
            "(",
            paste(
              paste0(
                "(",
                relevante_eerdere,
                ")"
              ),
              collapse = " |\n      "
            ),
            ")"
          )

          else_code <- paste0(
            "!(",
            eerdere_code,
            ")"
          )

          regelcondities <- c(
            regelcondities,
            else_code
          )
        }

        regelconditie_code <- if (
          length(regelcondities) == 0L
        ) {
          "TRUE"
        } else {
          paste(
            regelcondities,
            collapse = " &\n    "
          )
        }
      }

      eerdere_regelcondities <- c(
        eerdere_regelcondities,
        regelconditie_code
      )

      eerdere_invoer_waarden[[
        length(eerdere_invoer_waarden) + 1L
      ]] <- invoer_waarden

      condities <- character(0)

      if (length(loop_rij_condities) > 0L) {
        condities <- c(
          condities,
          loop_rij_condities
        )
      }

      condities <- c(
        condities,
        regelconditie_code
      )

      conditie_code <- if (length(condities) == 0L) {
        "TRUE"
      } else {
        paste(
          condities,
          collapse = " &\n    "
        )
      }

      waarden <- vapply(
        seq_along(uitvoer_idx),
        function(j) {
          idx <- uitvoer_idx[j]

          format_uitvoerwaarde(
            regel[idx],
            namen[idx]
          )
        },
        character(1)
      )

      waarde_code <- if (length(waarden) == 1L) {
        waarden
      } else {
        paste0(
          "list(\n      ",
          paste(
            waarden,
            collapse = ",\n      "
          ),
          "\n    )"
        )
      }

      regels_code <- c(
        regels_code,
        paste0(
          "  # ",
          i,
          "\n",
          "  ",
          conditie_code,
          ",\n",
          "  ",
          waarde_code
        )
      )

      regelcondities_code[i] <- conditie_code
    }

    doel_code <- vapply(
      uitvoer_namen,
      function(uitvoer) {
        if (!is.null(parse_index_variabele(uitvoer))) {
          format_variabele(
            uitvoer,
            kolomnaam = TRUE
          )
        } else {
          paste0(
            '"',
            format_variabele(uitvoer),
            '"'
          )
        }
      },
      character(1)
    )

    if (length(doel_code) == 1L) {
      doel_code <- doel_code[1]
    } else {
      doel_code <- paste0(
        "c(\n    ",
        paste(
          doel_code,
          collapse = ",\n    "
        ),
        "\n  )"
      )
    }

    if (is_hulp) {
      hulp_naam <- format_variabele(
        uitvoer_namen[1]
      )

      code <- paste0(
        hulp_naam,
        " <- maak_afleiding(\n",
        "  ",
        dataset,
        ",\n",
        '  "',
        hulp_naam,
        '",\n\n',
        paste(
          regels_code,
          collapse = ",\n\n"
        ),
        ",\n\n",
        "  behoud_kolommen = TRUE\n",
        ")"
      )
    } else {
      code <- paste0(
        dataset,
        " <- maak_afleiding(\n",
        "  ",
        dataset,
        ",\n",
        "  ",
        doel_code,
        ",\n\n",
        paste(
          regels_code,
          collapse = ",\n\n"
        ),
        "\n",
        ")"
      )
    }

    na_code <- character(0)

    for (i in seq_along(na_acties_per_regel)) {
      acties <- na_acties_per_regel[[i]]

      if (
        is.null(acties) ||
          length(acties) == 0L
      ) {
        next
      }

      conditie_na <- regelcondities_code[i]

      for (actie in acties) {
        actie_code <- format_na_actie(
          actie,
          conditie = conditie_na
        )

        na_code <- c(
          na_code,
          actie_code
        )
      }
    }

    if (length(na_code) > 0L) {
      code <- paste(
        c(
          code,
          na_code
        ),
        collapse = "\n\n"
      )
    }

    code <- omwikkel_met_loops(
      code,
      obj
    )

    code
  }

  code <- character(0)

  for (i in seq_along(x)) {
    obj <- x[[i]]
    type <- tolower(
      normaliseer_tekst(
        obj$type
      )
    )

    if (identical(type, "tabel")) {
      code <- c(
        code,
        verwerk_tabel(
          obj,
          i
        )
      )
      next
    }

    if (identical(type, "toekenning")) {
      code <- c(
        code,
        verwerk_toekenning(
          obj,
          i
        )
      )
      next
    }

    stop(
      paste0(
        "Onbekend JSON-type bij object ",
        i,
        ": ",
        type
      ),
      call. = FALSE
    )
  }

  paste(
    code,
    collapse = "\n\n\n"
  )
}

#' Rijen en kolommen selecteren
#'
#' Selecteert rijen en/of kolommen uit een dataframe.
#' Bij rijselecties kunnen dezelfde expressiehelpers worden gebruikt
#' als in `maak_afleiding()`, waaronder `lookup()`, `has_match()`,
#' `agg()`, `find()`, `nz()` en `same()`.
#'
#' @param data Dataframe waarop de selectie wordt uitgevoerd.
#' @param rijen Optionele logische conditie waarmee rijen worden geselecteerd.
#' @param kolommen Optionele kolomnaam of kolomnamen die worden behouden.
#' @param omschrijving Optionele omschrijving voor het controlebestand.
#'   Indien niet opgegeven wordt automatisch een omschrijving opgebouwd
#'   op basis van de uitgevoerde selectie en de naam van de dataset.
#'
#' @return Een dataframe met de geselecteerde rijen en/of kolommen.
#'
#' @examples
#' personen <- data.frame(
#'   leeftijd = c(10, 25, 70),
#'   inkomen = c(NA, 2000, 3000)
#' )
#'
#' selecteer(
#'   personen,
#'   rijen = leeftijd >= 18
#' )
#'
#' selecteer(
#'   personen,
#'   kolommen = c(leeftijd, inkomen)
#' )
#'
#' @export
selecteer <- function(
    data,
    rijen = NULL,
    kolommen = NULL,
    omschrijving = NULL) {

  starttijd <- Sys.time()

  data_naam <- deparse(
    substitute(data)
  )

  caller_env <- parent.frame()

  rijen_expr <- substitute(rijen)
  kolommen_expr <- substitute(kolommen)

  rijen_opgegeven <- !missing(rijen) &&
    !identical(
      rijen_expr,
      quote(NULL)
    )

  kolommen_opgegeven <- !missing(kolommen) &&
    !identical(
      kolommen_expr,
      quote(NULL)
    )

  if (is.null(omschrijving)) {

    omschrijving <- if (
      rijen_opgegeven &&
      kolommen_opgegeven
    ) {

      paste0(
        "Rijen en kolommen selecteren uit ",
        data_naam
      )

    } else if (rijen_opgegeven) {

      paste0(
        "Rijen selecteren uit ",
        data_naam
      )

    } else if (kolommen_opgegeven) {

      paste0(
        "Kolommen selecteren uit ",
        data_naam
      )

    } else {

      paste0(
        "Selectie maken uit ",
        data_naam
      )
    }
  }

  schrijf_log(
    "START selecteer"
  )

  resultaat <- tryCatch(

    withCallingHandlers(

      {

        if (data.table::is.data.table(data)) {
          stop(
            paste0(
              "Je geeft een data table door, dit werkt niet. ",
              "Verander je object naar een data.frame"
            ),
            call. = FALSE
          )
        }

        if (!is.data.frame(data)) {
          data <- as.data.frame(data)
        }

        if (
          !rijen_opgegeven &&
          !kolommen_opgegeven
        ) {
          stop(
            "Geef minimaal 'rijen' of 'kolommen' op.",
            call. = FALSE
          )
        }

        if (
          !is.character(omschrijving) ||
          length(omschrijving) != 1L ||
          is.na(omschrijving) ||
          trimws(omschrijving) == ""
        ) {
          stop(
            "'omschrijving' moet een niet-lege tekstwaarde zijn.",
            call. = FALSE
          )
        }

        n_rijen_voor <- nrow(data)
        kolommen_voor <- names(data)

        selectie_controle <- numeric(0)

        if (rijen_opgegeven) {

          context <- maak_expressie_context()

          eval_env <- maak_expressie_omgeving(
            data = data,
            caller_env = caller_env,
            context = context
          )

          selectie <- eval(
            rijen_expr,
            eval_env
          )

          if (!is.logical(selectie)) {
            stop(
              "'rijen' moet een logische conditie opleveren.",
              call. = FALSE
            )
          }

          if (length(selectie) == 1L) {

            selectie <- rep(
              selectie,
              nrow(data)
            )
          }

          if (
            length(selectie) !=
            nrow(data)
          ) {
            stop(
              paste0(
                "'rijen' heeft lengte ",
                length(selectie),
                ", maar moet lengte 1 of nrow(data) (",
                nrow(data),
                ") hebben."
              ),
              call. = FALSE
            )
          }

          selectie[
            is.na(selectie)
          ] <- FALSE

          data <- behoud_attributen(
            data = data,
            selectie = selectie
          )

          selectie_controle <- c(
            selectie_controle,
            maak_selectie_controle(
              n_rijen_voor = n_rijen_voor,
              n_rijen_na = nrow(data)
            )
          )
        }

        if (kolommen_opgegeven) {

          kolommen_selectie <- if (
            is.character(kolommen_expr)
          ) {

            kolommen_expr

          } else if (
            is.call(kolommen_expr) &&
            identical(
              kolommen_expr[[1L]],
              as.name("c")
            )
          ) {

            onderdelen <- as.list(
              kolommen_expr
            )[-1L]

            vapply(
              onderdelen,
              function(x) {

                if (
                  is.symbol(x) ||
                  is.character(x)
                ) {

                  as.character(x)

                } else {

                  stop(
                    paste0(
                      "'kolommen' mag alleen kolomnamen bevatten, ",
                      "bijvoorbeeld c(A, B) of c('A', 'B')."
                    ),
                    call. = FALSE
                  )
                }
              },
              character(1)
            )

          } else if (is.symbol(kolommen_expr)) {

            naam <- as.character(
              kolommen_expr
            )

            if (naam %in% names(data)) {

              naam

            } else {

              waarde <- tryCatch(
                eval(
                  kolommen_expr,
                  envir = caller_env
                ),
                error = function(e) NULL
              )

              if (is.character(waarde)) {
                waarde
              } else {
                naam
              }
            }

          } else {

            waarde <- tryCatch(
              eval(
                kolommen_expr,
                envir = caller_env
              ),
              error = function(e) NULL
            )

            if (!is.character(waarde)) {
              stop(
                paste0(
                  "'kolommen' moet uit kolomnamen bestaan, ",
                  "bijvoorbeeld A, c(A, B) of c('A', 'B')."
                ),
                call. = FALSE
              )
            }

            waarde
          }

          if (
            length(kolommen_selectie) == 0L ||
            anyNA(kolommen_selectie) ||
            any(trimws(kolommen_selectie) == "")
          ) {
            stop(
              "'kolommen' moet een of meerdere geldige kolomnamen bevatten.",
              call. = FALSE
            )
          }

          dubbele_kolommen <- unique(
            kolommen_selectie[
              duplicated(kolommen_selectie)
            ]
          )

          if (length(dubbele_kolommen) > 0L) {
            stop(
              paste0(
                "Kolommen zijn dubbel opgegeven: ",
                paste(
                  dubbele_kolommen,
                  collapse = ", "
                )
              ),
              call. = FALSE
            )
          }

          ontbrekende_kolommen <- setdiff(
            kolommen_selectie,
            names(data)
          )

          if (length(ontbrekende_kolommen) > 0L) {
            stop(
              paste0(
                "Kolommen bestaan niet in data: ",
                paste(
                  ontbrekende_kolommen,
                  collapse = ", "
                )
              ),
              call. = FALSE
            )
          }

          data <- data[
            ,
            kolommen_selectie,
            drop = FALSE
          ]

          selectie_controle <- c(
            selectie_controle,
            maak_selectie_controle(
              kolommen_voor = kolommen_voor,
              kolommen_na = names(data)
            )
          )
        }

        rij <- data.frame(
          omschrijving = c(
            omschrijving,
            names(selectie_controle)
          ),
          aantal = c(
            "",
            unname(selectie_controle)
          ),
          stringsAsFactors = FALSE
        )

        schrijf_r_logging(
          "controlebestand",
          rij
        )

        duur <- round(
          as.numeric(
            difftime(
              Sys.time(),
              starttijd,
              units = "secs"
            )
          ),
          2
        )

        schrijf_log_controle(
          omschrijving = omschrijving,
          waarden = stats::setNames(
            rij$aantal[-1L],
            rij$omschrijving[-1L]
          ),
          duur = duur
        )

        data
      },

      warning = function(w) {

        schrijf_log(
          paste0(
            "WARNING selecteer\n",
            conditionMessage(w)
          )
        )
      }
    ),

    error = function(e) {

      schrijf_log(
        paste0(
          "ERROR selecteer\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE selecteer"
  )

  resultaat
}

#' Dataframes samenvoegen
#'
#' Voegt twee of meer dataframes samen per rij of per kolom.
#'
#' Bij `type = "rbind"` worden variabelen die niet in alle datasets
#' voorkomen automatisch toegevoegd en gevuld met `NA`.
#'
#' Bij `type = "cbind"` moeten alle datasets hetzelfde aantal rijen
#' hebben en mogen geen dubbele kolomnamen ontstaan.
#'
#' @param type Type samenvoeging. Kies `"rbind"` of `"cbind"`.
#' @param ... Twee of meer dataframes die moeten worden samengevoegd.
#' @param omschrijving Optionele omschrijving voor het controlebestand.
#'   Indien niet opgegeven wordt afhankelijk van `type`
#'   `"Dataframes samenvoegen per rij"` of
#'   `"Dataframes samenvoegen per kolom"` gebruikt.
#'
#' @return Een dataframe met de samengevoegde data.
#'
#' @examples
#' data1 <- data.frame(
#'   id = 1:2,
#'   waarde = c(10, 20)
#' )
#'
#' data2 <- data.frame(
#'   id = 3:4,
#'   waarde = c(30, 40)
#' )
#'
#' voeg_samen(
#'   "rbind",
#'   data1,
#'   data2
#' )
#'
#' links <- data.frame(
#'   id = 1:2
#' )
#'
#' rechts <- data.frame(
#'   waarde = c(10, 20)
#' )
#'
#' voeg_samen(
#'   "cbind",
#'   links,
#'   rechts
#' )
#'
#' @export
voeg_samen <- function(
    type,
    ...,
    omschrijving = NULL) {

  starttijd <- Sys.time()

  schrijf_log(
    "START voeg_samen"
  )

  resultaat <- tryCatch(

    withCallingHandlers(

      {

        type <- match.arg(
          type,
          c(
            "rbind",
            "cbind"
          )
        )

        datasets <- list(...)

        if (length(datasets) < 2L) {
          stop(
            "Geef minimaal twee dataframes op.",
            call. = FALSE
          )
        }

        geldig <- vapply(
          datasets,
          is.data.frame,
          logical(1)
        )

        if (!all(geldig)) {
          stop(
            "Alle objecten in '...' moeten dataframes zijn.",
            call. = FALSE
          )
        }

        if (is.null(omschrijving)) {

          omschrijving <- paste0(
            length(datasets),
            if (type == "rbind") {
              " dataframes onder elkaar samenvoegen"
            } else {
              " dataframes naast elkaar samenvoegen"
            }
          )
        }

        if (
          !is.character(omschrijving) ||
          length(omschrijving) != 1L ||
          is.na(omschrijving) ||
          trimws(omschrijving) == ""
        ) {
          stop(
            "'omschrijving' moet een niet-lege tekstwaarde zijn.",
            call. = FALSE
          )
        }

        if (type == "rbind") {


          # Rijen per inputdataset


          aantal_rijen_per_dataset <- vapply(
            datasets,
            nrow,
            integer(1)
          )


          # Controleren welke variabelen niet overal voorkomen


          alle_namen <- unique(
            unlist(
              lapply(
                datasets,
                names
              ),
              use.names = FALSE
            )
          )

          ontbrekend_per_dataset <- lapply(
            datasets,
            function(data) {
              setdiff(
                alle_namen,
                names(data)
              )
            }
          )

          aantal_niet_overal <- sum(
            vapply(
              alle_namen,
              function(naam) {
                !all(
                  vapply(
                    datasets,
                    function(data) {
                      naam %in% names(data)
                    },
                    logical(1)
                  )
                )
              },
              logical(1)
            )
          )

          if (aantal_niet_overal > 0L) {

            melding <- vapply(
              seq_along(datasets),
              function(i) {

                ontbrekend <- ontbrekend_per_dataset[[i]]

                if (length(ontbrekend) == 0L) {
                  return(
                    paste0(
                      "Dataset ",
                      i,
                      ": geen ontbrekende variabelen"
                    )
                  )
                }

                paste0(
                  "Dataset ",
                  i,
                  ": ",
                  paste(
                    ontbrekend,
                    collapse = ", "
                  )
                )
              },
              character(1)
            )

            warning(
              paste0(
                "Niet alle datasets bevatten dezelfde variabelen. ",
                "Ontbrekende variabelen worden gevuld met NA.\n",
                paste(
                  melding,
                  collapse = "\n"
                )
              ),
              call. = FALSE
            )
          }


          # Samenvoegen


          output <- data.table::rbindlist(
            datasets,
            use.names = TRUE,
            fill = TRUE
          )

          data.table::setDF(
            output
          )


          # Controle-output


          rij <- data.frame(
            omschrijving = c(
              omschrijving,
              "Aantal datasets",
              paste0(
                "Aantal rijen dataset ",
                seq_along(datasets)
              ),
              "Aantal rijen na samenvoegen",
              "Aantal kolommen na samenvoegen",
              "Aantal variabelen niet in alle datasets"
            ),
            aantal = c(
              "",
              length(datasets),
              aantal_rijen_per_dataset,
              nrow(output),
              ncol(output),
              aantal_niet_overal
            ),
            stringsAsFactors = FALSE
          )

        } else {


          # Rijen en kolommen per inputdataset


          aantal_rijen_per_dataset <- vapply(
            datasets,
            nrow,
            integer(1)
          )

          aantal_kolommen_per_dataset <- vapply(
            datasets,
            ncol,
            integer(1)
          )


          # Voor cbind moeten aantallen rijen gelijk zijn


          if (
            length(
              unique(
                aantal_rijen_per_dataset
              )
            ) != 1L
          ) {
            stop(
              paste0(
                "Bij 'cbind' moeten alle dataframes hetzelfde aantal rijen hebben.\n",
                "Aantal rijen per dataset: ",
                paste(
                  aantal_rijen_per_dataset,
                  collapse = ", "
                )
              ),
              call. = FALSE
            )
          }


          # Dubbele kolomnamen voorkomen


          alle_namen <- unlist(
            lapply(
              datasets,
              names
            ),
            use.names = FALSE
          )

          dubbele_namen <- unique(
            alle_namen[
              duplicated(alle_namen)
            ]
          )

          if (length(dubbele_namen) > 0L) {
            stop(
              paste0(
                "Bij 'cbind' ontstaan dubbele kolomnamen:\n",
                paste(
                  paste0(
                    "- ",
                    dubbele_namen
                  ),
                  collapse = "\n"
                )
              ),
              call. = FALSE
            )
          }


          # Samenvoegen


          output <- do.call(
            cbind,
            datasets
          )

          if (!is.data.frame(output)) {
            output <- as.data.frame(
              output,
              stringsAsFactors = FALSE
            )
          }


          # Controle-output


          rij <- data.frame(
            omschrijving = c(
              omschrijving,
              "Aantal datasets",
              "Aantal rijen",
              paste0(
                "Aantal kolommen dataset ",
                seq_along(datasets)
              ),
              "Aantal kolommen na samenvoegen"
            ),
            aantal = c(
              "",
              length(datasets),
              nrow(output),
              aantal_kolommen_per_dataset,
              ncol(output)
            ),
            stringsAsFactors = FALSE
          )
        }


        # Controlebestand


        schrijf_r_logging(
          "controlebestand",
          rij
        )


        # Technische logging


        duur <- round(
          as.numeric(
            difftime(
              Sys.time(),
              starttijd,
              units = "secs"
            )
          ),
          2
        )

        schrijf_log_controle(
          omschrijving = omschrijving,
          waarden = stats::setNames(
            rij$aantal[-1L],
            rij$omschrijving[-1L]
          ),
          duur = duur
        )

        output
      },

      warning = function(w) {

        schrijf_log(
          paste0(
            "WARNING voeg_samen\n",
            conditionMessage(w)
          )
        )
      }
    ),

    error = function(e) {

      schrijf_log(
        paste0(
          "ERROR voeg_samen\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE voeg_samen"
  )

  resultaat
}


Functie_Lees_Specificatie_Tabellen <- function(
    bestand,
    kop = "Specificatie",
    kop_hulpvariabelen = "Hulpvariabelen",
    kop_invoervariabelen = "Invoervariabelen",
    kop_codering_afleiding = "Codering afleiding") {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' is nodig.", call. = FALSE)
  }

  if (!file.exists(bestand)) {
    stop(paste0("Bestand bestaat niet:\n", bestand), call. = FALSE)
  }

  doc <- officer::read_docx(bestand)
  inhoud <- officer::docx_summary(doc)

  print(names(inhoud))
  print(bestand)

  verplichte_kolommen <- c(
    "doc_index", "content_type", "style_name", "text",
    "table_index", "row_id", "cell_id"
  )

  ontbrekend <- setdiff(verplichte_kolommen, names(inhoud))

  if (length(ontbrekend) > 0L) {
    stop(
      paste0(
        "De volgende kolommen ontbreken in docx_summary():\n",
        paste(ontbrekend, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  normaliseer_tekst <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    trimws(x)
  }

  inhoud$text_norm <- normaliseer_tekst(inhoud$text)

  heading_level <- function(style) {
    style <- normaliseer_tekst(style)
    if (!nzchar(style)) {
      return(NA_integer_)
    }

    patroon <- "(?:heading|kop)\\s*([0-9]+)"
    delen <- regmatches(
      style,
      regexec(patroon, style, ignore.case = TRUE, perl = TRUE)
    )[[1]]

    if (length(delen) < 2L) {
      return(NA_integer_)
    }
    as.integer(delen[2])
  }

  inhoud$heading_level <- vapply(
    inhoud$style_name, heading_level, integer(1)
  )

  bekende_sectiekoppen <- c(
    "codering afleiding", "invoervariabelen", "hulpvariabelen",
    "specificatie", "opmerkingen"
  )

  vind_sectie <- function(sectiekop, verplicht = TRUE) {
    kandidaten <- which(
      tolower(inhoud$text_norm) == tolower(sectiekop) &
        is.na(inhoud$table_index)
    )

    if (length(kandidaten) == 0L) {
      if (isTRUE(verplicht)) {
        stop(paste0("Kop '", sectiekop, "' is niet gevonden."), call. = FALSE)
      }

      return(list(
        gevonden = FALSE,
        inhoud = inhoud[0, , drop = FALSE],
        start = NA_real_,
        einde = NA_real_
      ))
    }

    if (length(kandidaten) > 1L) {
      stop(paste0("Kop '", sectiekop, "' komt meerdere keren voor."), call. = FALSE)
    }

    rij_start <- kandidaten[1]
    doc_start <- inhoud$doc_index[rij_start]

    volgende_sectie <- which(
      inhoud$doc_index > doc_start &
        is.na(inhoud$table_index) &
        tolower(inhoud$text_norm) %in% bekende_sectiekoppen
    )

    volgende_sectie <- volgende_sectie[
      tolower(inhoud$text_norm[volgende_sectie]) != tolower(sectiekop)
    ]

    einde_sectie <- if (length(volgende_sectie) > 0L) {
      min(inhoud$doc_index[volgende_sectie])
    } else {
      Inf
    }

    volgende_heading <- which(
      inhoud$doc_index > doc_start &
        !is.na(inhoud$heading_level)
    )

    einde_heading <- if (length(volgende_heading) > 0L) {
      min(inhoud$doc_index[volgende_heading])
    } else {
      Inf
    }

    doc_einde <- min(einde_sectie, einde_heading)

    sectie_inhoud <- inhoud[
      inhoud$doc_index > doc_start &
        inhoud$doc_index < doc_einde, ,
      drop = FALSE
    ]

    list(
      gevonden = TRUE,
      inhoud = sectie_inhoud,
      start = doc_start,
      einde = doc_einde
    )
  }

  haal_sectie_tekst <- function(sectie_inhoud) {
    sectie_inhoud[
      is.na(sectie_inhoud$table_index) &
        nzchar(sectie_inhoud$text_norm),
      c("doc_index", "content_type", "style_name", "text"),
      drop = FALSE
    ]
  }

  sectie_specificatie <- vind_sectie(kop, verplicht = TRUE)
  sectie_hulp <- vind_sectie(kop_hulpvariabelen, verplicht = FALSE)
  sectie_invoer <- vind_sectie(kop_invoervariabelen, verplicht = FALSE)
  sectie_codering <- vind_sectie(kop_codering_afleiding, verplicht = FALSE)

  specificatie <- sectie_specificatie$inhoud
  doc_start <- sectie_specificatie$start
  doc_einde <- sectie_specificatie$einde

  hulpvariabelen_inhoud <- sectie_hulp$inhoud
  invoervariabelen_inhoud <- sectie_invoer$inhoud
  codering_afleiding_inhoud <- sectie_codering$inhoud

  hulpvariabelen_tekst <- haal_sectie_tekst(hulpvariabelen_inhoud)
  invoervariabelen_tekst <- haal_sectie_tekst(invoervariabelen_inhoud)
  codering_afleiding_tekst <- haal_sectie_tekst(codering_afleiding_inhoud)

  tabel_data <- specificatie[
    !is.na(specificatie$table_index), ,
    drop = FALSE
  ]

  reconstrueer_tabel <- function(tbl) {
    tbl <- tbl[
      order(tbl$row_id, tbl$cell_id, tbl$doc_index), ,
      drop = FALSE
    ]

    aantal_kolommen <- max(tbl$cell_id, na.rm = TRUE)
    row_ids <- sort(unique(tbl$row_id))

    matrix_tabel <- matrix(
      "",
      nrow = length(row_ids),
      ncol = aantal_kolommen
    )

    for (i in seq_along(row_ids)) {
      huidige_row <- row_ids[i]
      rij <- tbl[tbl$row_id == huidige_row, , drop = FALSE]
      cell_ids <- sort(unique(rij$cell_id))

      for (cell in cell_ids) {
        cel <- rij[rij$cell_id == cell, , drop = FALSE]
        teksten <- normaliseer_tekst(cel$text)
        teksten <- teksten[nzchar(teksten)]

        waarde <- if (length(teksten) == 0L) {
          ""
        } else {
          paste(teksten, collapse = "\n")
        }

        matrix_tabel[i, cell] <- waarde
      }
    }

    rownames(matrix_tabel) <- paste0("rij_", row_ids)
    colnames(matrix_tabel) <- paste0("kolom_", seq_len(aantal_kolommen))

    list(
      matrix = matrix_tabel,
      aantal_kolommen = aantal_kolommen,
      aantal_rijen = length(row_ids),
      row_ids = row_ids,
      eerste_doc_index = min(tbl$doc_index),
      laatste_doc_index = max(tbl$doc_index),
      origineel = tbl
    )
  }

  if (nrow(tabel_data) == 0L) {
    tabellen <- list()
  } else {
    tabel_indices <- sort(unique(tabel_data$table_index))

    tabellen <- lapply(
      tabel_indices,
      function(index) {
        tbl <- tabel_data[
          tabel_data$table_index == index, ,
          drop = FALSE
        ]

        resultaat <- reconstrueer_tabel(tbl)
        resultaat$table_index <- index
        resultaat
      }
    )

    names(tabellen) <- paste0("tabel_", tabel_indices)
  }

  tekst <- specificatie[
    is.na(specificatie$table_index) &
      nzchar(specificatie$text_norm),
    c("doc_index", "content_type", "style_name", "text"),
    drop = FALSE
  ]

  list(
    tabellen = tabellen,
    tekst = tekst,
    specificatie = specificatie,
    specificatie_start = doc_start,
    specificatie_einde = doc_einde,
    codering_afleiding = list(
      gevonden = sectie_codering$gevonden,
      tekst = codering_afleiding_tekst,
      inhoud = codering_afleiding_inhoud,
      start = sectie_codering$start,
      einde = sectie_codering$einde
    ),
    invoervariabelen = list(
      gevonden = sectie_invoer$gevonden,
      tekst = invoervariabelen_tekst,
      inhoud = invoervariabelen_inhoud,
      start = sectie_invoer$start,
      einde = sectie_invoer$einde
    ),
    hulpvariabelen = list(
      gevonden = sectie_hulp$gevonden,
      tekst = hulpvariabelen_tekst,
      inhoud = hulpvariabelen_inhoud,
      start = sectie_hulp$start,
      einde = sectie_hulp$einde
    )
  )
}

Functie_Verwerk_Specificatie_Tabellen <- function(specificatie) {
  if (!is.list(specificatie)) {
    stop(
      "'specificatie' moet de output zijn van Functie_Lees_Specificatie_Tabellen().",
      call. = FALSE
    )
  }

  if (!"tabellen" %in% names(specificatie)) {
    stop(
      paste0(
        "'specificatie' bevat geen element 'tabellen'.\n",
        "Gebruik de output van Functie_Lees_Specificatie_Tabellen()."
      ),
      call. = FALSE
    )
  }

  tabellen <- specificatie$tabellen

  if (!is.list(tabellen)) {
    stop("'specificatie$tabellen' moet een lijst zijn.", call. = FALSE)
  }

  normaliseer_tekst <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    x <- gsub("\u2018|\u2019", "'", x, perl = TRUE)
    x <- gsub("\u201C|\u201D", "\"", x, perl = TRUE)
    x <- gsub("\u2013|\u2014", "-", x, perl = TRUE)
    x <- gsub("\u2026", "...", x, fixed = TRUE)
    trimws(x)
  }

  verwerk_tabel <- function(tabel, tabel_naam) {
    if (is.matrix(tabel)) {
      matrix_tabel <- tabel
    } else if (
      is.list(tabel) &&
        "matrix" %in% names(tabel) &&
        is.matrix(tabel$matrix)
    ) {
      matrix_tabel <- tabel$matrix
    } else {
      stop(
        paste0("Tabel '", tabel_naam, "' bevat geen geldige matrix."),
        call. = FALSE
      )
    }

    if (ncol(matrix_tabel) == 0L) {
      stop(paste0("Tabel '", tabel_naam, "' bevat geen kolommen."), call. = FALSE)
    }

    if (nrow(matrix_tabel) == 0L) {
      stop(paste0("Tabel '", tabel_naam, "' bevat geen rijen."), call. = FALSE)
    }

    matrix_tabel[] <- normaliseer_tekst(matrix_tabel)

    lege_rij <- apply(
      matrix_tabel,
      1,
      function(x) all(!nzchar(x))
    )

    if (any(lege_rij)) {
      matrix_tabel <- matrix_tabel[!lege_rij, , drop = FALSE]
    }

    if (nrow(matrix_tabel) == 0L) {
      stop(
        paste0("Tabel '", tabel_naam, "' bevat alleen lege rijen."),
        call. = FALSE
      )
    }

    header <- matrix_tabel[1, , drop = TRUE]

    bron <- if (nrow(matrix_tabel) >= 2L) {
      matrix_tabel[2, , drop = TRUE]
    } else {
      rep("", ncol(matrix_tabel))
    }

    if (nrow(matrix_tabel) >= 3L) {
      regels <- matrix_tabel[
        3:nrow(matrix_tabel), ,
        drop = FALSE
      ]
    } else {
      regels <- matrix(
        character(0),
        nrow = 0L,
        ncol = ncol(matrix_tabel)
      )
      colnames(regels) <- colnames(matrix_tabel)
    }

    kolommen <- data.frame(
      positie = seq_len(ncol(matrix_tabel)),
      naam = unname(header),
      bron = unname(bron),
      stringsAsFactors = FALSE
    )

    if (nrow(regels) > 0L) {
      oorspronkelijke_rijen <- rownames(matrix_tabel)

      if (
        !is.null(oorspronkelijke_rijen) &&
          length(oorspronkelijke_rijen) >= 3L
      ) {
        rownames(regels) <- oorspronkelijke_rijen[
          3:length(oorspronkelijke_rijen)
        ]
      }
    }

    table_index <- if (
      is.list(tabel) &&
        "table_index" %in% names(tabel)
    ) {
      tabel$table_index
    } else {
      NA_integer_
    }

    eerste_doc_index <- if (
      is.list(tabel) &&
        "eerste_doc_index" %in% names(tabel)
    ) {
      tabel$eerste_doc_index
    } else {
      NA_integer_
    }

    laatste_doc_index <- if (
      is.list(tabel) &&
        "laatste_doc_index" %in% names(tabel)
    ) {
      tabel$laatste_doc_index
    } else {
      NA_integer_
    }

    list(
      table_index = table_index,
      eerste_doc_index = eerste_doc_index,
      laatste_doc_index = laatste_doc_index,
      aantal_kolommen = ncol(matrix_tabel),
      aantal_regels = nrow(regels),
      kolommen = kolommen,
      header = unname(header),
      bron = unname(bron),
      regels = regels,
      matrix = matrix_tabel
    )
  }

  if (length(tabellen) == 0L) {
    verwerkte_tabellen <- list()
  } else {
    tabel_namen <- names(tabellen)

    if (is.null(tabel_namen) || any(!nzchar(tabel_namen))) {
      tabel_namen <- paste0("tabel_", seq_along(tabellen))
    }

    verwerkte_tabellen <- lapply(
      seq_along(tabellen),
      function(i) {
        verwerk_tabel(
          tabel = tabellen[[i]],
          tabel_naam = tabel_namen[i]
        )
      }
    )

    names(verwerkte_tabellen) <- tabel_namen
  }

  specificatie_kolomnamen <- character(0)

  if (length(verwerkte_tabellen) > 0L) {
    specificatie_kolomnamen <- unique(
      unlist(
        lapply(
          verwerkte_tabellen,
          function(x) normaliseer_tekst(x$kolommen$naam)
        ),
        use.names = FALSE
      )
    )

    specificatie_kolomnamen <- specificatie_kolomnamen[
      nzchar(specificatie_kolomnamen)
    ]
  }

  verwerk_sectie <- function(sectie, filter_op_specificatie = FALSE) {
    resultaat <- list(
      gevonden = FALSE,
      tekst = NULL,
      inhoud = NULL,
      namen = character(0)
    )

    if (is.null(sectie) || !is.list(sectie)) {
      return(resultaat)
    }

    resultaat$gevonden <- isTRUE(sectie$gevonden)

    if (!is.null(sectie$tekst)) {
      resultaat$tekst <- sectie$tekst

      if (
        is.data.frame(resultaat$tekst) &&
          "text" %in% names(resultaat$tekst)
      ) {
        resultaat$tekst$text <- normaliseer_tekst(
          resultaat$tekst$text
        )
      }
    }

    if (!is.null(sectie$inhoud)) {
      resultaat$inhoud <- sectie$inhoud

      if (
        is.data.frame(resultaat$inhoud) &&
          "text" %in% names(resultaat$inhoud)
      ) {
        resultaat$inhoud$text_norm <- normaliseer_tekst(
          resultaat$inhoud$text
        )
      }
    }

    if (
      !is.null(resultaat$tekst) &&
        is.data.frame(resultaat$tekst) &&
        "text" %in% names(resultaat$tekst)
    ) {
      kandidaten <- unlist(
        lapply(
          resultaat$tekst$text,
          function(x) {
            x <- as.character(x)

            if (is.na(x) || !nzchar(trimws(x))) {
              return(character(0))
            }

            regels <- strsplit(x, "[\r\n]+", perl = TRUE)[[1]]
            regels <- normaliseer_tekst(regels)
            regels <- regels[nzchar(regels)]

            if (length(regels) == 0L) {
              return(character(0))
            }
            regels[1]
          }
        ),
        use.names = FALSE
      )

      kandidaten <- kandidaten[
        grepl(
          "^[A-Za-z][A-Za-z0-9_.]*(?:\\[[A-Za-z0-9_+ -]+\\])?$",
          kandidaten
        )
      ]

      kandidaten <- unique(kandidaten[nzchar(kandidaten)])

      if (isTRUE(filter_op_specificatie)) {
        kandidaten <- kandidaten[
          tolower(kandidaten) %in%
            tolower(specificatie_kolomnamen)
        ]
      }

      resultaat$namen <- kandidaten
    }

    resultaat
  }

  uitvoer_resultaat <- verwerk_sectie(
    sectie = if ("codering_afleiding" %in% names(specificatie)) {
      specificatie$codering_afleiding
    } else {
      NULL
    },
    filter_op_specificatie = TRUE
  )

  invoer_resultaat <- verwerk_sectie(
    sectie = if ("invoervariabelen" %in% names(specificatie)) {
      specificatie$invoervariabelen
    } else {
      NULL
    },
    filter_op_specificatie = TRUE
  )

  hulp_resultaat <- verwerk_sectie(
    sectie = if ("hulpvariabelen" %in% names(specificatie)) {
      specificatie$hulpvariabelen
    } else {
      NULL
    },
    filter_op_specificatie = FALSE
  )

  list(
    tabellen = verwerkte_tabellen,
    tekst = if ("tekst" %in% names(specificatie)) {
      specificatie$tekst
    } else {
      NULL
    },
    specificatie_start = if ("specificatie_start" %in% names(specificatie)) {
      specificatie$specificatie_start
    } else {
      NULL
    },
    specificatie_einde = if ("specificatie_einde" %in% names(specificatie)) {
      specificatie$specificatie_einde
    } else {
      NULL
    },
    uitvoervariabelen = uitvoer_resultaat,
    invoervariabelen = invoer_resultaat,
    hulpvariabelen = hulp_resultaat
  )
}

Functie_Maak_LLM_Input_Specificatie <- function(specificatie_verwerkt) {
  if (!is.list(specificatie_verwerkt)) {
    stop(
      "'specificatie_verwerkt' moet de output zijn van Functie_Verwerk_Specificatie_Tabellen().",
      call. = FALSE
    )
  }

  if (!"tabellen" %in% names(specificatie_verwerkt)) {
    stop(
      paste0(
        "'specificatie_verwerkt' bevat geen element 'tabellen'.\n",
        "Gebruik de output van Functie_Verwerk_Specificatie_Tabellen()."
      ),
      call. = FALSE
    )
  }

  normaliseer_tekst <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    x <- gsub("\u2018|\u2019", "'", x, perl = TRUE)
    x <- gsub("\u201C|\u201D", "\"", x, perl = TRUE)
    x <- gsub("\u2013|\u2014|\u2212", "-", x, perl = TRUE)
    x <- gsub("\u2026", "...", x, fixed = TRUE)
    trimws(x)
  }

  escape_waarde <- function(x) {
    x <- normaliseer_tekst(x)
    x <- gsub("\\\\", "\\\\\\\\", x, perl = TRUE)
    x <- gsub("\r\n|\r|\n", "\\\\n", x, perl = TRUE)
    x <- gsub("\"", "\\\\\"", x, fixed = TRUE)
    paste0("\"", x, "\"")
  }

  haal_namen <- function(naam_element) {
    if (
      !naam_element %in% names(specificatie_verwerkt) ||
        !is.list(specificatie_verwerkt[[naam_element]]) ||
        !"namen" %in% names(specificatie_verwerkt[[naam_element]])
    ) {
      return(character(0))
    }

    namen <- normaliseer_tekst(
      specificatie_verwerkt[[naam_element]]$namen
    )

    unique(namen[nzchar(namen)])
  }

  maak_variabelenblok <- function(titel, namen, omschrijving, leegmelding) {
    if (length(namen) == 0L) {
      return(c(titel, "", paste0("[", leegmelding, "]")))
    }

    c(
      titel, "",
      omschrijving, "",
      paste0("- ", namen)
    )
  }

  maak_tabelblok <- function(tabel, tabel_naam) {
    if (!is.list(tabel)) {
      stop(paste0("Tabel '", tabel_naam, "' heeft geen geldige structuur."), call. = FALSE)
    }

    if (!"kolommen" %in% names(tabel)) {
      stop(paste0("Tabel '", tabel_naam, "' bevat geen 'kolommen'."), call. = FALSE)
    }

    if (!"regels" %in% names(tabel)) {
      stop(paste0("Tabel '", tabel_naam, "' bevat geen 'regels'."), call. = FALSE)
    }

    kolommen <- tabel$kolommen
    regels <- tabel$regels

    table_index <- if (
      "table_index" %in% names(tabel) &&
        length(tabel$table_index) == 1L &&
        !is.na(tabel$table_index)
    ) {
      as.character(tabel$table_index)
    } else {
      "onbekend"
    }

    eerste_doc_index <- if (
      "eerste_doc_index" %in% names(tabel) &&
        length(tabel$eerste_doc_index) == 1L &&
        !is.na(tabel$eerste_doc_index)
    ) {
      as.character(tabel$eerste_doc_index)
    } else {
      "onbekend"
    }

    laatste_doc_index <- if (
      "laatste_doc_index" %in% names(tabel) &&
        length(tabel$laatste_doc_index) == 1L &&
        !is.na(tabel$laatste_doc_index)
    ) {
      as.character(tabel$laatste_doc_index)
    } else {
      "onbekend"
    }

    kolomregels <- vapply(
      seq_len(nrow(kolommen)),
      function(i) {
        paste0(
          kolommen$positie[i],
          " | naam=", escape_waarde(kolommen$naam[i]),
          " | bron=", escape_waarde(kolommen$bron[i])
        )
      },
      character(1)
    )

    if (nrow(regels) == 0L) {
      regelregels <- "[geen datarijen]"
    } else {
      regelregels <- vapply(
        seq_len(nrow(regels)),
        function(i) {
          waarden <- vapply(
            seq_len(ncol(regels)),
            function(j) paste0(j, "=", escape_waarde(regels[i, j])),
            character(1)
          )

          fysieke_rij <- if (
            !is.null(rownames(regels)) &&
              nzchar(rownames(regels)[i])
          ) {
            rownames(regels)[i]
          } else {
            as.character(i)
          }

          paste0(fysieke_rij, " | ", paste(waarden, collapse = " | "))
        },
        character(1)
      )
    }

    paste(
      c(
        paste0("[TABEL ", tabel_naam, "]"),
        paste0("table_index: ", table_index),
        paste0("doc_index_start: ", eerste_doc_index),
        paste0("doc_index_einde: ", laatste_doc_index),
        paste0("aantal_kolommen: ", nrow(kolommen)),
        "",
        "KOLOMMEN:",
        kolomregels,
        "",
        "REGELS:",
        regelregels
      ),
      collapse = "\n"
    )
  }

  uitvoervariabelen <- haal_namen("uitvoervariabelen")
  invoervariabelen <- haal_namen("invoervariabelen")
  hulpvariabelen <- haal_namen("hulpvariabelen")

  uitvoervariabelen_blok <- maak_variabelenblok(
    "UITVOERVARIABELEN",
    uitvoervariabelen,
    "De volgende variabelen zijn in het Word-document expliciet als uitvoervariabele gedefinieerd:",
    "geen uitvoervariabelen gevonden"
  )

  invoervariabelen_blok <- maak_variabelenblok(
    "INVOERVARIABELEN",
    invoervariabelen,
    "De volgende variabelen zijn in het Word-document expliciet als invoervariabele gedefinieerd:",
    "geen invoervariabelen gevonden"
  )

  hulpvariabelen_blok <- maak_variabelenblok(
    "HULPVARIABELEN",
    hulpvariabelen,
    "De volgende variabelen zijn in het Word-document expliciet als hulpvariabele gedefinieerd:",
    "geen hulpvariabelen gevonden"
  )

  tekst <- if ("tekst" %in% names(specificatie_verwerkt)) {
    specificatie_verwerkt$tekst
  } else {
    NULL
  }

  onderdelen <- list()
  onderdeel_nr <- 0L

  if (!is.null(tekst) && is.data.frame(tekst) && nrow(tekst) > 0L) {
    if (
      !"doc_index" %in% names(tekst) ||
        !"text" %in% names(tekst)
    ) {
      stop(
        "'tekst' moet minimaal de kolommen 'doc_index' en 'text' bevatten.",
        call. = FALSE
      )
    }

    for (i in seq_len(nrow(tekst))) {
      waarde <- normaliseer_tekst(tekst$text[i])
      if (!nzchar(waarde)) next

      onderdeel_nr <- onderdeel_nr + 1L
      onderdelen[[onderdeel_nr]] <- list(
        soort = "tekst",
        doc_index = as.numeric(tekst$doc_index[i]),
        subindex = 0,
        inhoud = paste0(
          "[TEKST doc_index=", tekst$doc_index[i], "]\n", waarde
        )
      )
    }
  }

  tabellen <- specificatie_verwerkt$tabellen

  if (length(tabellen) > 0L) {
    tabel_namen <- names(tabellen)

    if (is.null(tabel_namen) || any(!nzchar(tabel_namen))) {
      tabel_namen <- paste0("tabel_", seq_along(tabellen))
    }

    for (i in seq_along(tabellen)) {
      tabel <- tabellen[[i]]

      eerste_doc_index <- if (
        is.list(tabel) &&
          "eerste_doc_index" %in% names(tabel) &&
          length(tabel$eerste_doc_index) == 1L &&
          !is.na(tabel$eerste_doc_index)
      ) {
        as.numeric(tabel$eerste_doc_index)
      } else {
        Inf
      }

      onderdeel_nr <- onderdeel_nr + 1L
      onderdelen[[onderdeel_nr]] <- list(
        soort = "tabel",
        doc_index = eerste_doc_index,
        subindex = 1,
        inhoud = maak_tabelblok(
          tabel = tabel,
          tabel_naam = tabel_namen[i]
        )
      )
    }
  }

  if (length(onderdelen) == 0L) {
    specificatie_blok <- c(
      "SPECIFICATIE", "",
      "[geen tekst of tabellen gevonden]"
    )
  } else {
    volgorde <- order(
      vapply(onderdelen, function(x) x$doc_index, numeric(1)),
      vapply(onderdelen, function(x) x$subindex, numeric(1))
    )

    onderdelen <- onderdelen[volgorde]
    blokken <- vapply(onderdelen, function(x) x$inhoud, character(1))

    specificatie_blok <- c(
      "SPECIFICATIE", "",
      "Belangrijk:",
      "- De tabellen hieronder zijn al fysiek uit Word gereconstrueerd.",
      "- Elke tabelkolom heeft een vaste fysieke positie.",
      "- Lege cellen worden weergegeven als \"\".",
      "- Waarden mogen nooit horizontaal naar een andere kolom worden verschoven.",
      "- REGELS bevatten nog de ruwe fysieke tabelwaarden; er is nog geen carry-forward toegepast.",
      "",
      blokken
    )
  }

  paste(
    c(
      uitvoervariabelen_blok, "",
      invoervariabelen_blok, "",
      hulpvariabelen_blok, "",
      specificatie_blok
    ),
    collapse = "\n"
  )
}

#' Inhoudelijke controles uitvoeren
#'
#' Voert een of meerdere inhoudelijke controles uit. Indien als eerste
#' argument een dataframe wordt opgegeven, kunnen kolommen uit dat dataframe
#' rechtstreeks in de controles worden gebruikt. Zonder dataframe kunnen
#' controles rechtstreeks worden uitgevoerd op objecten uit de aanroepende
#' omgeving.
#'
#' Controles worden opgegeven als opeenvolgende combinaties van een
#' omschrijving en een logische expressie.
#'
#' De functie ondersteunt dezelfde expressiehelpers als `maak_afleiding()`
#' en `selecteer()`: `lookup()`, `has_match()`, `agg()`, `find()`, `nz()`
#' en `same()`.
#'
#' De uitgevoerde controles en eventuele fouten worden opgenomen in het
#' technische logbestand indien logging actief is. De functie schrijft niet
#' naar het controlebestand.
#'
#' @param ... Optioneel eerst een dataframe, gevolgd door controles als
#'   opeenvolgende combinaties van `"omschrijving", controle`.
#'
#' @return Onzichtbaar `TRUE` als alle controles slagen.
#'   Indien een of meerdere controles falen, stopt de functie met
#'   een samengevoegde foutmelding.
#'
#' @examples
#' personen <- data.frame(
#'   hh = c(1, 1, 2),
#'   leeftijd = c(20, 25, 40)
#' )
#'
#' controleer_data(
#'   personen,
#'   "Leeftijd mag niet negatief zijn",
#'   leeftijd >= 0
#' )
#'
#' dataset1 <- data.frame(
#'   a = 1:3,
#'   b = 4:6
#' )
#'
#' dataset2 <- data.frame(
#'   x = 1:3,
#'   y = 4:6
#' )
#'
#' controleer_data(
#'   "Aantal kolommen is gelijk",
#'   ncol(dataset1) == ncol(dataset2)
#' )
#'
#' @export
controleer_data <- function(...) {

  starttijd <- Sys.time()
  caller_env <- parent.frame()

  schrijf_log(
    "START controleer_data"
  )

  resultaat <- tryCatch(

    withCallingHandlers(

      {

        args <- as.list(
          substitute(list(...))
        )[-1L]

        if (length(args) == 0L) {
          stop(
            "Er zijn geen controles opgegeven.",
            call. = FALSE
          )
        }

        data <- NULL
        data_opgegeven <- FALSE

        eerste_waarde <- tryCatch(
          eval(
            args[[1L]],
            envir = caller_env
          ),
          error = function(e) NULL
        )

        if (is.data.frame(eerste_waarde)) {

          data <- eerste_waarde
          data_opgegeven <- TRUE

          args <- args[-1L]
        }

        if (length(args) == 0L) {
          stop(
            "Er zijn geen controles opgegeven.",
            call. = FALSE
          )
        }

        if (length(args) %% 2L != 0L) {
          stop(
            paste0(
              "Geef controles steeds op als: ",
              "\"omschrijving\", controle."
            ),
            call. = FALSE
          )
        }

        context <- maak_expressie_context()

        eval_env <- maak_expressie_omgeving(
          data = data,
          caller_env = caller_env,
          context = context
        )

        fouten <- character(0)
        log_controles <- character(0)

        teller <- 0L

        for (
          i in seq.int(
            1L,
            length(args),
            by = 2L
          )
        ) {

          teller <- teller + 1L

          naam <- eval(
            args[[i]],
            envir = caller_env
          )

          controle_expr <- args[[i + 1L]]

          if (
            length(naam) != 1L ||
            !is.character(naam) ||
            is.na(naam) ||
            !nzchar(naam)
          ) {
            stop(
              paste0(
                "Controle ",
                teller,
                " heeft geen geldige omschrijving."
              ),
              call. = FALSE
            )
          }

          context$laatste_match <- NULL
          context$laatste_lookup_values <- character(0)
          context$actieve_condition <- NULL
          context$basis_condition <- NULL
          context$current_regel <- paste0(
            "#C",
            teller
          )
          context$has_match_mode <- "normaal"

          uitkomst <- tryCatch(

            eval(
              controle_expr,
              envir = eval_env
            ),

            error = function(e) {

              stop(
                paste0(
                  "Fout tijdens controle '",
                  naam,
                  "':\n",
                  conditionMessage(e)
                ),
                call. = FALSE
              )
            }
          )

          if (!is.logical(uitkomst)) {
            stop(
              paste0(
                "Controle '",
                naam,
                "' geeft geen TRUE/FALSE-resultaat terug."
              ),
              call. = FALSE
            )
          }

          if (data_opgegeven) {

            toegestane_lengtes <- unique(
              c(
                1L,
                nrow(data)
              )
            )

            if (
              !length(uitkomst) %in%
              toegestane_lengtes
            ) {
              stop(
                paste0(
                  "Controle '",
                  naam,
                  "' geeft ",
                  length(uitkomst),
                  " waarden terug; verwacht 1 of ",
                  nrow(data),
                  "."
                ),
                call. = FALSE
              )
            }

          } else {

            if (length(uitkomst) != 1L) {
              stop(
                paste0(
                  "Controle '",
                  naam,
                  "' geeft ",
                  length(uitkomst),
                  " waarden terug; zonder dataframe wordt ",
                  "precies 1 TRUE/FALSE-resultaat verwacht."
                ),
                call. = FALSE
              )
            }
          }

          fout <- !is.na(uitkomst) &
            !uitkomst

          aantal_fout <- sum(
            fout
          )

          if (aantal_fout > 0L) {

            status <- paste0(
              aantal_fout,
              " fout"
            )

            fouten <- c(
              fouten,
              paste0(
                "[",
                teller,
                "] ",
                naam,
                " (",
                aantal_fout,
                " fout)"
              )
            )

          } else {

            status <- "OK"
          }

          log_controles <- c(
            log_controles,
            paste0(
              "Controle ",
              teller,
              " | ",
              naam,
              " | ",
              status
            )
          )
        }

        context$current_regel <- NULL
        context$laatste_match <- NULL
        context$actieve_condition <- NULL
        context$basis_condition <- NULL

        if (length(log_controles) > 0L) {

          schrijf_log(
            paste(
              log_controles,
              collapse = "\n"
            )
          )
        }

        if (length(fouten) > 0L) {
          stop(
            paste0(
              "Inhoudelijke controles niet geslaagd:\n\n",
              paste(
                fouten,
                collapse = "\n"
              )
            ),
            call. = FALSE
          )
        }

        TRUE
      },

      warning = function(w) {

        schrijf_log(
          paste0(
            "WARNING controleer_data\n",
            conditionMessage(w)
          )
        )
      }
    ),

    error = function(e) {

      schrijf_log(
        paste0(
          "ERROR controleer_data\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  duur <- round(
    as.numeric(
      difftime(
        Sys.time(),
        starttijd,
        units = "secs"
      )
    ),
    2
  )

  schrijf_log(
    paste0(
      "EINDE controleer_data | ",
      duur,
      " sec"
    )
  )

  invisible(resultaat)
}
