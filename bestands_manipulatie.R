#' Bestand inlezen
#'
#' Leest een bestand in op basis van de bestandsextensie.
#'
#' Wrapper functie om het inlezen van bestanden heen. <br>
#' O.b.v. de extensie van het bestand wordt een inlees functie aangeroepen om deze in te lezen
#' (bv ```csv = utils::read.csv2```). Indien een omschrijving wordt meegeveven worden ook
#' automatisch enkele gegevens opsgelagen in het verslagbestand (r data.frame). Voor het inlezen
#' slaan we het aantal kolommen, rijen en de laatste wijzigingsdatum op van het bestand.
#' | Extension | `datatable = FALSE`          | `datatable = TRUE`     |
#' | --------- | ---------------------------- | ---------------------- |
#' | `.csv`    | [utils::read.csv2()]         | [data.table::fread()]  |
#' | `.txt`    | [utils::read.table()]        | [data.table::fread()]  |
#' | `.rds`    | [base::readRDS()]            | —                      |
#' | `.xlsx`   | [openxlsx2::read_xlsx()]     | —                      |
#' | `.sav`    | [lees_sav_bestand_in()]      | —                      |
#' | `.asc`    | [readr::read_fwf()]          | —                      |
#'
#' @param bestand Pad naar het bestand.
#' @param omschrijving Optionele omschrijving voor de controle.
#'   Indien niet opgegeven wordt `"Bestand inlezen"` gebruikt.
#' @param datatable Indien TRUE wordt voor csv- en txt-bestanden
#'   data.table::fread gebruikt.
#' @param ... Extra argumenten voor de gebruikte inleesfunctie.
#'
#' @return Een dataframe met de ingelezen data.
#'
#' @export
#'
#' @examples
#' lees_bestand_in("voorbeelddata/test.sav", n_max = 12)
#' lees_bestand_in("voorbeelddata/test.csv", datatable = TRUE)
lees_bestand_in <- function(
    bestand,
    omschrijving = NULL,
    datatable = FALSE,
    ...) {
  starttijd <- Sys.time()

  schrijf_log(
    "START lees_bestand_in"
  )

  resultaat <- tryCatch(
    withCallingHandlers(
      {
        if (
          !is.character(bestand) ||
            length(bestand) != 1L ||
            is.na(bestand) ||
            trimws(bestand) == ""
        ) {
          stop(
            "'bestand' moet een geldig bestandspad zijn.",
            call. = FALSE
          )
        }

        if (is.null(omschrijving)) {
          omschrijving <- paste0(
            basename(bestand),
            " inlezen"
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

        ext <- tolower(
          tools::file_ext(bestand)
        )

        toegestane_extensies <- list(
          csv = function(...) utils::read.csv2(...),
          rds = function(...) base::readRDS(...),
          sav = function(...) GTST::lees_sav_bestand_in(...),
          xlsx = function(...) openxlsx2::read_xlsx(...),
          asc = function(...) readr::read_fwf(...),
          txt = function(...) utils::read.table(...)
        )

        if (datatable) {
          toegestane_extensies$csv <- function(...) data.table::fread(...)
          toegestane_extensies$txt <- function(...) data.table::fread(...)
        }

        if (!ext %in% names(toegestane_extensies)) {
          stop(
            "Bestandstype .",
            ext,
            " wordt nog niet ondersteund",
            call. = FALSE
          )
        }

        data <- toegestane_extensies[[ext]](
          bestand,
          ...
        )

        if (ext == "asc") {
          # asc returned tibble, voor de volledigheid ff omzetten naar df
          data <- as.data.frame(data)
        }

        if (data.table::is.data.table(data)) {
          data.table::setDF(data)
        }

        laatst_aangepast <- format(
          file.info(bestand)$mtime,
          "%d-%m-%Y %H:%M:%S"
        )

        rij <- data.frame(
          omschrijving = c(
            omschrijving,
            "Bestand",
            "Aantal rijen",
            "Aantal kolommen",
            "Laatst aangepast bestand"
          ),
          aantal = c(
            "",
            bestand,
            nrow(data),
            ncol(data),
            laatst_aangepast
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
            "WARNING lees_bestand_in\n",
            conditionMessage(w)
          )
        )
      }
    ),
    error = function(e) {
      schrijf_log(
        paste0(
          "ERROR lees_bestand_in\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE lees_bestand_in"
  )

  resultaat
}

#' Bestand wegschrijven
#'
#' Schrijft een bestand weg op basis van de extensie van `bestand`.
#'
#' Ondersteunde bestandstypen zijn CSV, RDS, XLSX, SAV en ASC.
#' Voor CSV kan met `datatable = TRUE` gebruik worden gemaakt van
#' [data.table::fwrite()]. Voor RDS en SAV wordt waar mogelijk metadata
#' gekoppeld voordat het bestand wordt weggeschreven.
#'
#' Extra argumenten voor de onderliggende wegschrijffunctie kunnen via
#' `...` worden doorgegeven.
#' #' | Extension | `datatable = FALSE`       | `datatable = TRUE`     |
#' | --------- | ------------------------- | ---------------------- |
#' | `.csv`    | [readr::write_csv()]      | [data.table::fwrite()] |
#' | `.rds`    | [base::saveRDS()]         | —                      |
#' | `.xlsx`   | [openxlsx2::write_xlsx()] | —                      |
#' | `.sav`    | [haven::write_sav()]      | —                      |
#' | `.asc`    | [base::writeLines()]      | —                      |
#'
#' @param data Object dat moet worden weggeschreven. Voor de gebruikelijke
#'   dataformaten is dit een dataframe.
#' @param bestand Pad naar het bestand dat moet worden aangemaakt.
#'   De extensie bepaalt welk bestandsformaat wordt gebruikt.
#' @param datatable Indien `TRUE` wordt voor CSV-bestanden
#'   [data.table::fwrite()] gebruikt. Anders wordt [utils::write.csv2()]
#'   gebruikt.
#' @param omschrijving Optionele omschrijving voor het controlebestand.
#'   Indien niet opgegeven wordt `"Bestand wegschrijven"` gebruikt.
#' @param ... Extra argumenten voor de onderliggende wegschrijffunctie.
#'
#' @return Onzichtbaar het pad naar het weggeschreven bestand.
#'
#' @examples
#' bestand <- file.path(
#'   tempdir(),
#'   "mtcars.rds"
#' )
#'
#' schrijf_bestand_weg(
#'   mtcars,
#'   bestand
#' )
#'
#' file.exists(bestand)
#'
#' @export
schrijf_bestand_weg <- function(
    data,
    bestand,
    datatable = FALSE,
    omschrijving = NULL,
    ...) {
  starttijd <- Sys.time()

  data_naam <- deparse(
    substitute(data)
  )

  schrijf_log(
    "START schrijf_bestand_weg"
  )

  resultaat <- tryCatch(
    withCallingHandlers(
      {
        if (
          !is.character(bestand) ||
            length(bestand) != 1L ||
            is.na(bestand) ||
            trimws(bestand) == ""
        ) {
          stop(
            "'bestand' moet een geldig bestandspad zijn.",
            call. = FALSE
          )
        }

        if (is.null(omschrijving)) {
          omschrijving <- paste0(
            basename(bestand),
            " wegschrijven"
          )
        }

        if (
          !is.logical(datatable) ||
            length(datatable) != 1L ||
            is.na(datatable)
        ) {
          stop(
            "'datatable' moet TRUE of FALSE zijn.",
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

        ext <- tolower(
          tools::file_ext(
            bestand
          )
        )

        switch(ext,
          csv = {
            if (datatable) {
              data.table::fwrite(
                data,
                bestand,
                ...
              )
            } else {
              utils::write.csv2(
                data,
                bestand,
                ...
              )
            }
          },
          rds = {
            schrijf_rds_bestand_weg(
              data,
              bestand,
              data_naam = data_naam,
              ...
            )
          },
          xlsx = {
            openxlsx2::write_xlsx(
              data,
              bestand,
              ...
            )
          },
          sav = {
            schrijf_sav_bestand_weg(
              data,
              bestand,
              data_naam = data_naam,
              ...
            )
          },
          asc = {
            base::writeLines(
              data,
              bestand,
              ...
            )
          },
          stop(
            sprintf(
              paste0(
                "Niet ondersteunde extensie: '.%s'. ",
                "Ondersteund zijn: csv, rds, xlsx, sav, asc"
              ),
              ext
            ),
            call. = FALSE
          )
        )

        rij <- data.frame(
          omschrijving = c(
            omschrijving,
            "Bestand",
            "Aantal rijen",
            "Aantal kolommen"
          ),
          aantal = c(
            "",
            bestand,
            nrow(data),
            ncol(data)
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

        invisible(
          bestand
        )
      },
      warning = function(w) {
        schrijf_log(
          paste0(
            "WARNING schrijf_bestand_weg\n",
            conditionMessage(w)
          )
        )
      }
    ),
    error = function(e) {
      schrijf_log(
        paste0(
          "ERROR schrijf_bestand_weg\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE schrijf_bestand_weg"
  )

  resultaat
}

#' Wrapper om sav bestand inlezen
#'
#' Wordt gebruikt in lees bestand in om sav bestanden in te lezen
#'
#' Extra functionaliteit: optie om labels te zappen en kolommen hoofdlettergevoelig te lezen.
#' Stel je kolom heet AFL_x in je dataset en jij kiest bij col_slect 'afl_x', worden de waardes
#' van kolom AFL_x toegekend aan afl_x.
#'
#' @param bestand in te lezen bestand
#' @param zap logical, of de meta waardes weggahaald kunnen worden
#' @param col_select in te lezen kolommen
#' @param ... extra params die aan haven::read_sav gegeven kunnen worden
#'
#' @export
#'
#' @returns ingelezen sav bestand als data.frame
lees_sav_bestand_in <- function(bestand, zap = TRUE, col_select = NULL, ...) {
  if (!is.null(col_select)) {
    alle_kolommen <- names(haven::read_sav(bestand, n_max = 0))

    gedeelde_kolommen <- sapply(col_select, function(requested) {
      hit <- alle_kolommen[tolower(alle_kolommen) == tolower(requested)]
      if (length(hit) == 0) {
        warning(sprintf("Kolom '%s'niet in bestand gevonden.", requested))
        return(NA_character_)
      }
      if (length(hit) > 1) {
        warning(sprintf(
          "Kolom '%s' is geen unieke kolomnaam: %s. We gebruiken de eerste match",
          requested, paste(hit, collapse = ", ")
        ))
        hit <- hit[1]
      }
      hit
    })

    gedeelde_kolommen <- gedeelde_kolommen[!is.na(gedeelde_kolommen)]

    df <- haven::read_sav(bestand, col_select = dplyr::all_of(gedeelde_kolommen), ...)

    namen <- stats::setNames(gedeelde_kolommen, col_select[!is.na(gedeelde_kolommen)])
    df <- dplyr::rename(df, !!!namen)
  } else {
    df <- haven::read_sav(bestand, ...)
  }

  if (zap) {
    df <- df |>
      haven::zap_labels() |>
      haven::zap_label() |>
      haven::zap_formats() |>
      haven::zap_widths() |>
      haven::zap_missing()
  }
  return(df)
}

#' Schrijf een RDS-bestand weg
#'
#' Interne hulpfunctie voor het wegschrijven van een RDS-bestand met
#' gekoppelde metadata.
#'
#' Deze functie wordt aangeroepen door [schrijf_bestand_weg()]. Voor het
#' wegschrijven wordt geprobeerd metadata aan de dataset te koppelen met
#' [koppel_meta()]. Wanneer geen metadata beschikbaar is, wordt het bestand
#' zonder gekoppelde metadata weggeschreven.
#'
#' Argumenten in `...` die bij [koppel_meta()] horen, worden aan die functie
#' doorgegeven. Overige argumenten worden doorgegeven aan [base::saveRDS()].
#'
#' @param data Dataset die moet worden weggeschreven.
#' @param bestand Character. Pad waar het RDS-bestand moet worden opgeslagen.
#' @param data_naam Character. Naam van de dataset. Deze naam wordt gebruikt
#'   om eventueel automatisch gekoppelde metadata te vinden.
#' @param ... Extra argumenten voor [koppel_meta()] of [base::saveRDS()].
#'
#' @return Geeft niets expliciet terug. Het RDS-bestand wordt naar `bestand`
#'   weggeschreven.
#'
#' @keywords internal
schrijf_rds_bestand_weg <- function(data, bestand, data_naam, ...) {
  args <- list(...)

  meta_args <- args[
    names(args) %in% names(formals(GTST::koppel_meta))
  ]

  rds_args <- args[
    !names(args) %in% names(formals(GTST::koppel_meta))
  ]

  data <- do.call(
    GTST::koppel_meta,
    c(
      list(dataset = data),
      meta_args,
      list(
        verplicht = FALSE,
        dataset_naam = data_naam
      )
    )
  )

  do.call(
    base::saveRDS,
    c(
      list(object = data, file = bestand),
      rds_args
    )
  )
}

#' Schrijf een SAV-bestand weg
#'
#' Interne hulpfunctie voor het wegschrijven van een dataframe als
#' SPSS SAV-bestand.
#'
#' Indien metadata beschikbaar is, worden onder andere variabelelabels,
#' value labels, datatypes en SPSS-formaten toegepast. Metadata kan expliciet
#' worden meegegeven of automatisch worden opgehaald op basis van `data_naam`
#' en `Meta_Koppelingen`.
#'
#' ENUM-variabelen met codes met voorloopnullen worden als character
#' behouden. Andere ENUM-variabelen worden waar mogelijk numeriek
#' weggeschreven.
#'
#' @param data Dataframe dat moet worden weggeschreven.
#' @param bestand Character. Pad waar het SAV-bestand moet worden opgeslagen.
#' @param meta Optioneel data.frame met metadata. Wanneer `NULL`, wordt
#'   geprobeerd de metadata automatisch te bepalen via `Meta_Koppelingen`.
#' @param controle_range Logisch. Indien `TRUE`, worden de variabelen voor
#'   export gecontroleerd aan de hand van de metadata. Standaard `FALSE`.
#' @param data_naam Optionele naam van de dataset. Deze naam wordt gebruikt
#'   om eventueel automatisch gekoppelde metadata te vinden. Wanneer `NULL`,
#'   wordt geprobeerd de naam af te leiden van het object dat aan `data`
#'   is meegegeven.
#' @param ... Extra argumenten die worden doorgegeven aan
#'   [haven::write_sav()].
#'
#' @return Onzichtbaar het dataframe zoals het naar het SAV-bestand is
#'   geschreven.
#'
#' @keywords internal
schrijf_sav_bestand_weg <- function(
    data,
    bestand,
    meta = NULL,
    controle_range = FALSE,
    data_naam = NULL,
    ...) {
  # Eerst controleren voordat verdere verwerking plaatsvindt

  if (!is.data.frame(data)) {
    stop(
      "'data' moet een dataframe zijn.",
      call. = FALSE
    )
  }

  if (is.null(data_naam)) {
    data_naam <- deparse(
      substitute(data)
    )
  }

  # Metadata bepalen
  #
  # Expliciet meegegeven metadata heeft altijd voorrang.

  meta <- meta_resolver(
    meta = meta,
    dataset_naam = data_naam,
    envir = parent.frame(),
    verplicht = FALSE
  )

  # Geen metadata beschikbaar:
  # dataframe rechtstreeks wegschrijven

  if (is.null(meta)) {
    sav <- data

    w <- pmax(
      nchar(
        names(sav)
      ) + 2L,
      8L
    )

    for (i in seq_along(sav)) {
      attr(
        sav[[i]],
        "display_width"
      ) <- w[i]
    }

    haven::write_sav(
      sav,
      bestand
    )

    return(
      invisible(sav)
    )
  }

  # 1. Metadata koppelen

  data <- koppel_meta(
    dataset = data,
    meta = meta,
    controle_range = controle_range,
    controle_namen = TRUE
  )

  # 2. Datatypes aanpassen

  for (naam in names(data)) {
    meta_class <- attr(
      data[[naam]],
      "meta_class"
    )

    if (
      is.null(meta_class) ||
        is.na(meta_class) ||
        meta_class == ""
    ) {
      next
    }

    class_type <- sub(
      "\\[.*$",
      "",
      meta_class
    )

    # STR

    if (class_type == "STR") {
      data[[naam]] <- as.character(
        data[[naam]]
      )

      # INT
    } else if (class_type == "INT") {
      data[[naam]] <- as.integer(
        data[[naam]]
      )

      # REAL
    } else if (class_type == "REAL") {
      data[[naam]] <- as.numeric(
        data[[naam]]
      )

      # ENUM
    } else if (class_type == "ENUM") {
      value_labels <- attr(
        data[[naam]],
        "value_labels"
      )

      codes <- if (
        !is.null(value_labels)
      ) {
        names(value_labels)
      } else {
        character(0)
      }

      heeft_voorloopnul <- any(
        grepl(
          "^0[0-9]+$",
          codes
        )
      )

      if (heeft_voorloopnul) {
        # Bijvoorbeeld 01 moet "01" blijven

        data[[naam]] <- as.character(
          data[[naam]]
        )
      } else {
        data_num <- suppressWarnings(
          as.numeric(
            as.character(
              data[[naam]]
            )
          )
        )

        codes_num <- suppressWarnings(
          as.numeric(codes)
        )

        data_numeriek <- all(
          is.na(data[[naam]]) |
            !is.na(data_num)
        )

        codes_numeriek <- (
          length(codes) == 0L ||
            all(
              !is.na(codes_num)
            )
        )

        if (
          data_numeriek &&
            codes_numeriek
        ) {
          data[[naam]] <- as.integer(
            data_num
          )
        } else {
          data[[naam]] <- as.character(
            data[[naam]]
          )
        }
      }
    }
  }

  # 3. Metadata opnieuw koppelen
  #
  # De datatypeconversies hierboven verwijderen
  # attributen van de betreffende vectoren.

  data <- koppel_meta(
    dataset = data,
    meta = meta,
    controle_range = FALSE,
    controle_namen = FALSE
  )

  # 4. Haven-labelled maken

  sav <- as.data.frame(
    lapply(
      data,
      convert_to_labelled
    ),
    check.names = FALSE
  )

  # 5. Blokpaden verwijderen uit SAV-namen

  names(sav) <- sav_namen(
    dataset = data,
    meta = meta
  )

  # 6. Display width

  w <- pmax(
    nchar(
      names(sav)
    ) + 2L,
    8L
  )

  for (i in seq_along(sav)) {
    attr(
      sav[[i]],
      "display_width"
    ) <- w[i]
  }

  # 7. SAV wegschrijven

  haven::write_sav(
    sav,
    bestand,
    ...
  )

  invisible(sav)
}

# Sav schrijven helpers -------------------------------------------------------
convert_to_labelled <- function(variable) {
  label <- attr(
    variable,
    "label"
  )

  value_labels <- attr(
    variable,
    "value_labels"
  )

  # Value labels omzetten naar haven-formaat

  if (
    !is.null(value_labels) &&
      length(value_labels) > 0L
  ) {
    codes <- names(
      value_labels
    )

    labels <- unname(
      value_labels
    )

    if (is.numeric(variable)) {
      codes_sav <- suppressWarnings(
        as.numeric(codes)
      )

      if (!anyNA(codes_sav)) {
        label_map <- stats::setNames(
          codes_sav,
          labels
        )
      } else {
        label_map <- stats::setNames(
          codes,
          labels
        )
      }
    } else {
      # Character codes bewust character houden
      # zodat bijvoorbeeld "01" behouden blijft.

      label_map <- stats::setNames(
        codes,
        labels
      )
    }

    if (is.logical(variable)) {
      variable <- as.character(
        variable
      )
    }

    variable <- haven::labelled_spss(
      variable,
      labels = label_map
    )
  }

  # Variabelelabel

  if (
    !is.null(label) &&
      !is.na(label) &&
      label != ""
  ) {
    attr(
      variable,
      "label"
    ) <- label
  }

  # SPSS-format

  if (is.numeric(variable)) {
    geldige_waarden <- variable[
      !is.na(variable)
    ]

    has_decimals <- (
      length(geldige_waarden) > 0L &&
        any(
          abs(
            geldige_waarden -
              round(geldige_waarden)
          ) >
            .Machine$double.eps^0.5
        )
    )

    decimals <- if (
      has_decimals
    ) {
      2L
    } else {
      0L
    }

    lengte_data <- if (
      length(geldige_waarden) > 0L
    ) {
      max(
        nchar(
          as.character(
            geldige_waarden
          )
        ),
        na.rm = TRUE
      )
    } else {
      2L
    }

    lengte_codes <- if (
      !is.null(value_labels) &&
        length(value_labels) > 0L
    ) {
      max(
        nchar(
          names(value_labels)
        ),
        na.rm = TRUE
      )
    } else {
      0L
    }

    max_val <- max(
      lengte_data,
      lengte_codes,
      2L
    )

    width <- max_val +
      if (
        decimals > 0L
      ) {
        decimals + 1L
      } else {
        0L
      }

    attr(
      variable,
      "format.spss"
    ) <- sprintf(
      "F%d.%d",
      width,
      decimals
    )
  } else if (is.character(variable)) {
    geldige_waarden <- variable[
      !is.na(variable)
    ]

    lengte_data <- if (
      length(geldige_waarden) > 0L
    ) {
      max(
        nchar(
          geldige_waarden
        ),
        na.rm = TRUE
      )
    } else {
      2L
    }

    lengte_codes <- if (
      !is.null(value_labels) &&
        length(value_labels) > 0L
    ) {
      max(
        nchar(
          names(value_labels)
        ),
        na.rm = TRUE
      )
    } else {
      0L
    }

    max_len <- max(
      lengte_data,
      lengte_codes,
      2L
    )

    attr(
      variable,
      "format.spss"
    ) <- sprintf(
      "A%d",
      max_len
    )
  }

  variable
}

sav_namen <- function(
    dataset,
    meta) {
  mapping <- meta_mapping(
    variabele_naam = names(dataset),
    meta = meta
  )

  sav_namen <- names(
    dataset
  )

  gevonden <- !is.na(
    mapping$variable
  )

  sav_namen[gevonden] <- mapping$variable[
    gevonden
  ]

  # SPSS is niet hoofdlettergevoelig

  dubbele_namen <- unique(
    sav_namen[
      duplicated(
        tolower(sav_namen)
      ) |
        duplicated(
          tolower(sav_namen),
          fromLast = TRUE
        )
    ]
  )

  if (length(dubbele_namen) > 0L) {
    stop(
      paste0(
        "Na het verwijderen van de blokpaden ontstaan dubbele SAV-namen: ",
        paste(
          dubbele_namen,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  sav_namen
}
