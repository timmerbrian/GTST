#' Koppel metadata aan een dataset
#'
#' Koppelt metadata aan variabelen in een dataset. De metadata kan expliciet
#' worden meegegeven of automatisch worden opgehaald op basis van de naam van
#' de dataset en `Meta_Koppelingen`.
#'
#' Per gekoppelde variabele kunnen onder andere de metadata-attributen
#' `meta_class`, `label` en `value_labels` worden toegevoegd. Optioneel kunnen
#' de variabelen vooraf worden gecontroleerd op aanwezigheid in de metadata en
#' op de bijbehorende metadataregels, zoals datatype, bereik en toegestane
#' waarden.
#'
#' @param dataset Dataset waaraan de metadata wordt gekoppeld.
#' @param meta Optioneel data.frame met metadata, doorgaans ingelezen met
#'   [lees_meta_in()]. Wanneer `NULL`, wordt geprobeerd de metadata automatisch
#'   te bepalen via `Meta_Koppelingen`.
#' @param variabele_naam Character vector met de namen van de variabelen
#'   waarvoor metadata moet worden gekoppeld. Standaard alle variabelen in
#'   `dataset`.
#' @param controle_range Logisch. Indien `TRUE`, worden de geselecteerde
#'   variabelen gecontroleerd aan de hand van de metadata, bijvoorbeeld op
#'   datatype, bereik en toegestane waarden.
#' @param controle_namen Logisch. Indien `TRUE`, wordt gecontroleerd of de
#'   opgegeven variabelen voorkomen in de metadata.
#' @param error Logisch. Bepaalt of problemen bij de metadata-controle als
#'   error (`TRUE`) of, waar ondersteund, als warning (`FALSE`) worden
#'   afgehandeld.
#' @param verplicht Logisch. Indien `TRUE`, moet metadata beschikbaar zijn.
#'   Indien `FALSE`, mag de functie doorgaan wanneer geen metadata kan worden
#'   gevonden.
#' @param dataset_naam Optionele naam van de dataset die wordt gebruikt voor
#'   het automatisch opzoeken van metadata. Standaard wordt de naam afgeleid
#'   van het object dat aan `dataset` is meegegeven.
#' @param omschrijving Optionele omschrijving voor het controlebestand en de
#'   technische logging. Bij `NULL` wordt automatisch een omschrijving
#'   samengesteld op basis van de metadata en de dataset.
#' @param interne_aanroep Logisch. Geeft aan of `koppel_meta()` intern vanuit
#'   een andere GTST-functie wordt aangeroepen. Bij `TRUE` schrijft
#'   `koppel_meta()` geen eigen controleblok en geen afzonderlijke technische
#'   start- en eindlogging. Standaard `FALSE`.
#'
#' @return De dataset met gekoppelde metadata-attributen. Wanneer geen metadata
#'   beschikbaar is en `verplicht = FALSE`, wordt de dataset zonder gekoppelde
#'   metadata teruggegeven.
#'
#' @examples
#' meta_voorbeeld <- data.frame(
#'   variable = c("Leeftijd", "Reisafstand"),
#'   class = c("INT[0:120]", "REAL[0:1000]"),
#'   label = c("Leeftijd in jaren", "Reisafstand in kilometers"),
#'   stringsAsFactors = FALSE
#' )
#'
#' attr(meta_voorbeeld, "meta_index") <- data.frame(
#'   variable = c("Leeftijd", "Reisafstand"),
#'   meta_rij = c(1, 2),
#'   stringsAsFactors = FALSE
#' )
#'
#' Personen_demo <- data.frame(
#'   Leeftijd = c(42, 28, 67),
#'   Reisafstand = c(18, 7, 3)
#' )
#'
#' Personen_demo <- koppel_meta(
#'   Personen_demo,
#'   meta = meta_voorbeeld,
#'   controle_range = FALSE
#' )
#'
#' @export
koppel_meta <- function(
    dataset,
    meta = NULL,
    variabele_naam = names(dataset),
    controle_range = TRUE,
    controle_namen = TRUE,
    error = TRUE,
    verplicht = TRUE,
    dataset_naam = NULL,
    omschrijving = NULL,
    interne_aanroep = FALSE) {

  starttijd <- Sys.time()

  dataset_expr <- substitute(dataset)
  meta_expr <- substitute(meta)

  if (is.null(dataset_naam)) {
    dataset_naam <- deparse(
      dataset_expr
    )
  }

  if (is.null(omschrijving)) {

    if (!identical(meta_expr, quote(NULL))) {

      meta_naam <- deparse(
        meta_expr
      )

    } else if (
      exists(
        "Meta_Koppelingen",
        envir = parent.frame(),
        inherits = TRUE
      )
    ) {

      Meta_Koppelingen <- get(
        "Meta_Koppelingen",
        envir = parent.frame(),
        inherits = TRUE
      )

      if (dataset_naam %in% names(Meta_Koppelingen)) {

        meta_naam <- unname(
          Meta_Koppelingen[[dataset_naam]]
        )

      } else {

        meta_naam <- "Metadata"
      }

    } else {

      meta_naam <- "Metadata"
    }

    omschrijving <- paste0(
      meta_naam,
      " koppelen aan ",
      dataset_naam
    )
  }

  if (!interne_aanroep) {
    schrijf_log(
      "START koppel_meta"
    )
  }

  uitvoering <- function() {

    aantal_aangeboden <- length(
      variabele_naam
    )

    meta <- meta_resolver(
      meta = meta,
      dataset_naam = dataset_naam,
      envir = parent.frame(),
      verplicht = verplicht
    )

    if (is.null(meta)) {

      aantal_gekoppeld <- 0L
      aantal_niet_gekoppeld <- aantal_aangeboden

      attr(
        dataset,
        "meta_controle"
      ) <- c(
        "Aantal variabelen met metadata" =
          aantal_gekoppeld,
        "Aantal variabelen zonder metadata" =
          aantal_niet_gekoppeld
      )

      if (!interne_aanroep) {

        rij <- data.frame(
          omschrijving = c(
            omschrijving,
            "Aantal variabelen aangeboden",
            "Aantal variabelen met metadata",
            "Aantal variabelen zonder metadata"
          ),
          aantal = c(
            "",
            aantal_aangeboden,
            aantal_gekoppeld,
            aantal_niet_gekoppeld
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
      }

      cat(
        "Geen meta kunnen koppelen\n"
      )

      return(dataset)
    }

    if (controle_namen) {

      resultaat_namen <- controleren_meta_namen(
        variabele_naam = variabele_naam,
        meta = meta,
        error = error
      )

      if (!isTRUE(resultaat_namen)) {

        variabele_naam <- setdiff(
          variabele_naam,
          resultaat_namen
        )
      }
    }

    variabele_naam <- intersect(
      variabele_naam,
      names(dataset)
    )

    mapping <- meta_mapping(
      variabele_naam = variabele_naam,
      meta = meta
    )

    mapping <- mapping[
      !is.na(mapping$meta_rij),
      ,
      drop = FALSE
    ]

    aantal_gekoppeld <- nrow(
      mapping
    )

    aantal_niet_gekoppeld <- (
      aantal_aangeboden -
        aantal_gekoppeld
    )

    if (nrow(mapping) > 0L) {

      if (controle_range) {

        controleren_range(
          variabele_naam = mapping$dataset_naam,
          dataset = dataset,
          meta = meta,
          error = error
        )
      }

      hoofdregels <- which(
        !is.na(meta$variable) &
          trimws(
            as.character(
              meta$variable
            )
          ) != ""
      )

      for (i in seq_len(nrow(mapping))) {

        naam <- mapping$dataset_naam[i]
        meta_rij <- mapping$meta_rij[i]

        volgende_rij <- hoofdregels[
          hoofdregels > meta_rij
        ]

        if (length(volgende_rij) == 0L) {

          eindrij <- nrow(meta)

        } else {

          eindrij <- volgende_rij[1L] - 1L
        }

        detail_rijen <- if (
          eindrij > meta_rij
        ) {

          seq.int(
            meta_rij + 1L,
            eindrij
          )

        } else {

          integer(0)
        }

        values <- meta$value[
          detail_rijen
        ]

        labels <- meta$label[
          detail_rijen
        ]

        geldig <- (
          !is.na(values) &
            trimws(
              as.character(
                values
              )
            ) != ""
        )

        values <- as.character(
          values[geldig]
        )

        labels <- as.character(
          labels[geldig]
        )

        attr(
          dataset[[naam]],
          "meta_class"
        ) <- meta$class[
          meta_rij
        ]

        attr(
          dataset[[naam]],
          "label"
        ) <- meta$label[
          meta_rij
        ]

        if (length(values) > 0L) {

          value_labels <- labels

          names(
            value_labels
          ) <- values

          attr(
            dataset[[naam]],
            "value_labels"
          ) <- value_labels

        } else {

          attr(
            dataset[[naam]],
            "value_labels"
          ) <- NULL
        }
      }
    }

    attr(
      dataset,
      "meta_controle"
    ) <- c(
      "Aantal variabelen met metadata" =
        aantal_gekoppeld,
      "Aantal variabelen zonder metadata" =
        aantal_niet_gekoppeld
    )

    if (!interne_aanroep) {

      rij <- data.frame(
        omschrijving = c(
          omschrijving,
          "Aantal variabelen aangeboden",
          "Aantal variabelen met metadata",
          "Aantal variabelen zonder metadata"
        ),
        aantal = c(
          "",
          aantal_aangeboden,
          aantal_gekoppeld,
          aantal_niet_gekoppeld
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
    }

    dataset
  }

  if (interne_aanroep) {

    resultaat <- uitvoering()

  } else {

    resultaat <- tryCatch(

      withCallingHandlers(

        uitvoering(),

        warning = function(w) {

          schrijf_log(
            paste0(
              "WARNING koppel_meta\n",
              conditionMessage(w)
            )
          )
        }
      ),

      error = function(e) {

        schrijf_log(
          paste0(
            "ERROR koppel_meta\n",
            conditionMessage(e)
          )
        )

        stop(e)
      }
    )
  }

  if (!interne_aanroep) {
    schrijf_log(
      "EINDE koppel_meta"
    )
  }

  resultaat
}

### Hulpfuncties koppel meta ----------------------------------------------
controleren_meta_namen <- function(
    variabele_naam,
    meta,
    error = error) {
  signal <- if (error) stop else warning

  mapping <- meta_mapping(
    variabele_naam = variabele_naam,
    meta = meta
  )

  ontbrekend <- mapping$dataset_naam[
    is.na(mapping$meta_rij)
  ]

  if (length(ontbrekend) == 0L) {
    return(TRUE)
  }

  signal(
    paste0(
      "De volgende variabelen komen niet voor in de metadata: ",
      paste(
        ontbrekend,
        collapse = ", "
      )
    ),
    call. = FALSE
  )

  ontbrekend
}

controleren_range <- function(
    dataset,
    variabele_naam = NULL,
    meta = NULL,
    error = error) {
  signal <- if (error) stop else warning
  dataset_naam <- deparse(
    substitute(dataset)
  )

  meta <- meta_resolver(
    meta = meta,
    dataset_naam = dataset_naam,
    envir = parent.frame(),
    verplicht = TRUE
  )

  # Geen variabelen opgegeven

  if (is.null(variabele_naam)) {
    variabele_naam <- names(dataset)
  }

  fouten <- character(0)

  aantal_variabelen <- length(
    variabele_naam
  )

  # Mapping een keer maken

  mapping <- meta_mapping(
    variabele_naam = variabele_naam,
    meta = meta
  )

  meta_rij_lookup <- mapping$meta_rij

  names(meta_rij_lookup) <- mapping$dataset_naam

  # Eindregel per metadata-variabele

  hoofdregels <- which(
    !is.na(meta$variable) &
      trimws(
        as.character(meta$variable)
      ) != ""
  )

  eindregels <- c(
    hoofdregels[-1L] - 1L,
    nrow(meta)
  )

  names(eindregels) <- as.character(
    hoofdregels
  )

  if (aantal_variabelen > 50L) {
    message(
      "Rangecontrole gestart voor ",
      aantal_variabelen,
      " variabelen."
    )
  }

  for (i in seq_along(variabele_naam)) {
    naam <- variabele_naam[i]

    # Dataset

    if (!naam %in% names(dataset)) {
      fouten <- c(
        fouten,
        paste0(
          naam,
          ": komt niet voor in de dataset"
        )
      )

      next
    }

    waarden <- dataset[[naam]]

    if (all(is.na(waarden))) {
      next
    }

    # Metadataregel

    gevonden_rij <- unname(
      meta_rij_lookup[naam]
    )

    if (is.na(gevonden_rij)) {
      fouten <- c(
        fouten,
        paste0(
          naam,
          ": komt niet voor in de metadata"
        )
      )

      next
    }

    eindrij <- unname(
      eindregels[
        as.character(gevonden_rij)
      ]
    )

    # Class

    class_meta <- trimws(
      as.character(
        meta$class[gevonden_rij]
      )
    )

    if (
      is.na(class_meta) ||
        class_meta == ""
    ) {
      fouten <- c(
        fouten,
        paste0(
          naam,
          ": geen class opgegeven"
        )
      )

      next
    }

    # Datatype

    datatype_fout <- controleren_datatype(
      variabele_naam = naam,
      dataset = dataset,
      meta = meta,
      meta_rij = gevonden_rij
    )

    if (!is.null(datatype_fout)) {
      fouten <- c(
        fouten,
        datatype_fout
      )
    }

    # Key

    key_meta <- trimws(
      tolower(
        as.character(
          meta$key[gevonden_rij]
        )
      )
    )

    if (
      !is.na(key_meta) &&
        key_meta == "ja"
    ) {
      dubbele_waarden <- unique(
        waarden[
          duplicated(waarden) |
            duplicated(
              waarden,
              fromLast = TRUE
            )
        ]
      )

      dubbele_waarden <- dubbele_waarden[
        !is.na(dubbele_waarden)
      ]

      if (length(dubbele_waarden) > 0L) {
        fouten <- c(
          fouten,
          paste0(
            naam,
            ": sleutel niet uniek (",
            paste(
              dubbele_waarden,
              collapse = ", "
            ),
            ")"
          )
        )
      }
    }

    # Class-specifieke controle

    class_type <- sub(
      "\\[.*$",
      "",
      class_meta
    )

    if (class_type == "ENUM") {
      controle <- controleren_enum(
        variabele_naam = naam,
        dataset = dataset,
        meta = meta,
        meta_rij = gevonden_rij,
        eindrij = eindrij,
        melding = FALSE,
        error = error
      )

      if (
        controle$werkelijk_aantal !=
          controle$verwacht_aantal
      ) {
        fouten <- c(
          fouten,
          paste0(
            naam,
            ": ",
            class_meta,
            " verwacht ",
            controle$verwacht_aantal,
            " opties, maar metadata bevat ",
            controle$werkelijk_aantal
          )
        )
      }

      if (
        length(
          controle$ongeldige_waarden
        ) > 0L
      ) {
        fouten <- c(
          fouten,
          paste0(
            naam,
            ": niet-toegestane waarden (",
            paste(
              controle$ongeldige_waarden,
              collapse = ", "
            ),
            ")"
          )
        )
      }
    } else if (class_type == "STR") {
      controle <- controleren_str(
        variabele_naam = naam,
        dataset = dataset,
        meta = meta,
        meta_rij = gevonden_rij,
        eindrij = eindrij,
        melding = FALSE,
        error = error
      )

      if (length(controle) > 0L) {
        fouten <- c(
          fouten,
          paste0(
            naam,
            ": voldoet niet aan ",
            class_meta,
            " (",
            paste(
              controle,
              collapse = ", "
            ),
            ")"
          )
        )
      }
    } else if (
      class_type %in% c(
        "INT",
        "REAL"
      )
    ) {
      controle <- controleren_num(
        variabele_naam = naam,
        dataset = dataset,
        meta = meta,
        meta_rij = gevonden_rij,
        eindrij = eindrij,
        melding = FALSE,
        error = error
      )

      if (length(controle) > 0L) {
        fouten <- c(
          fouten,
          paste0(
            naam,
            ": voldoet niet aan ",
            class_meta,
            " (",
            paste(
              controle,
              collapse = ", "
            ),
            ")"
          )
        )
      }
    } else {
      fouten <- c(
        fouten,
        paste0(
          naam,
          ": onbekende class '",
          class_meta,
          "'"
        )
      )
    }

    # Voortgang

    if (
      aantal_variabelen > 50L &&
        i %% 30L == 0L
    ) {
      message(
        i,
        " van ",
        aantal_variabelen,
        " variabelen gecontroleerd."
      )
    }
  }

  if (
    aantal_variabelen > 50L &&
      aantal_variabelen %% 30L != 0L
  ) {
    message(
      aantal_variabelen,
      " van ",
      aantal_variabelen,
      " variabelen gecontroleerd."
    )
  }

  # Samenvatting

  if (length(fouten) == 0L) {
    return(TRUE)
  }

  signal(
    paste0(
      "Metadata-controle heeft ",
      length(fouten),
      " fout(en) gevonden:\n",
      "\n- ",
      paste(
        fouten,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
  fouten
}

controleren_enum <- function(
    variabele_naam,
    dataset,
    meta,
    meta_rij,
    eindrij,
    melding = TRUE,
    error = error) {
  signal <- if (error) stop else warning
  class_meta <- trimws(
    as.character(
      meta$class[meta_rij]
    )
  )

  verwacht_aantal <- as.integer(
    sub(
      "^ENUM\\[(\\d+)\\]$",
      "\\1",
      class_meta
    )
  )

  if (is.na(verwacht_aantal)) {
    stop(
      paste0(
        "Ongeldige ENUM-class: '",
        class_meta,
        "'."
      ),
      call. = FALSE
    )
  }

  optie_rijen <- if (
    eindrij > meta_rij
  ) {
    seq.int(
      meta_rij + 1L,
      eindrij
    )
  } else {
    integer(0)
  }

  toegestane_opties <- meta$value[
    optie_rijen
  ]

  toegestane_opties <- toegestane_opties[
    !is.na(toegestane_opties) &
      trimws(
        as.character(
          toegestane_opties
        )
      ) != ""
  ]

  werkelijk_aantal <- length(
    toegestane_opties
  )

  if (
    werkelijk_aantal != verwacht_aantal &&
      melding
  ) {
    signal(
      paste0(
        "Variabele '",
        variabele_naam,
        "' heeft class ",
        class_meta,
        ", maar er zijn ",
        werkelijk_aantal,
        " opties opgenomen in meta$value."
      ),
      call. = FALSE
    )
  }

  waarden_data <- unique(
    dataset[[variabele_naam]]
  )

  waarden_data <- waarden_data[
    !is.na(waarden_data)
  ]

  toegestane_tekst <- trimws(
    as.character(
      toegestane_opties
    )
  )

  waarden_data_tekst <- trimws(
    as.character(
      waarden_data
    )
  )

  ongeldige_opties <- waarden_data[
    !waarden_data_tekst %in%
      toegestane_tekst
  ]

  if (
    length(ongeldige_opties) > 0L &&
      melding
  ) {
    signal(
      paste0(
        "Variabele '",
        variabele_naam,
        "' bevat niet-toegestane opties: ",
        paste(
          ongeldige_opties,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  list(
    ongeldige_waarden = ongeldige_opties,
    verwacht_aantal = verwacht_aantal,
    werkelijk_aantal = werkelijk_aantal
  )
}

controleren_str <- function(
    variabele_naam,
    dataset,
    meta,
    meta_rij,
    eindrij,
    melding = TRUE,
    error = error) {
  signal <- if (error) stop else warning
  class_meta <- trimws(
    as.character(
      meta$class[meta_rij]
    )
  )

  maximale_lengte <- as.integer(
    sub(
      "^STR\\[(\\d+)\\]$",
      "\\1",
      class_meta
    )
  )

  if (is.na(maximale_lengte)) {
    stop(
      paste0(
        "Ongeldige STR-class: '",
        class_meta,
        "'."
      ),
      call. = FALSE
    )
  }

  # Eventuele toegestane uitzonderingscodes

  optie_rijen <- if (
    eindrij > meta_rij
  ) {
    seq.int(
      meta_rij + 1L,
      eindrij
    )
  } else {
    integer(0)
  }

  toegestane_codes <- meta$value[
    optie_rijen
  ]

  toegestane_codes <- toegestane_codes[
    !is.na(toegestane_codes) &
      trimws(
        as.character(
          toegestane_codes
        )
      ) != ""
  ]

  toegestane_codes <- trimws(
    as.character(
      toegestane_codes
    )
  )

  waarden <- unique(
    dataset[[variabele_naam]]
  )

  waarden <- waarden[
    !is.na(waarden)
  ]

  waarden_tekst <- as.character(
    waarden
  )

  te_controleren <- waarden_tekst[
    !trimws(
      waarden_tekst
    ) %in% toegestane_codes
  ]

  te_lang <- te_controleren[
    nchar(
      te_controleren
    ) > maximale_lengte
  ]

  if (
    length(te_lang) > 0L &&
      melding
  ) {
    signal(
      paste0(
        "Variabele '",
        variabele_naam,
        "' heeft class ",
        class_meta,
        " en bevat waarden met meer dan ",
        maximale_lengte,
        " tekens: ",
        paste(
          te_lang,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  te_lang
}

controleren_num <- function(
    variabele_naam,
    dataset,
    meta,
    meta_rij,
    eindrij,
    melding = TRUE,
    error = error) {
  signal <- if (error) stop else warning
  class_meta <- trimws(
    as.character(
      meta$class[meta_rij]
    )
  )

  nummer_class <- lezen_num_class(
    class_meta
  )

  class_type <- nummer_class$class_type
  minimum <- nummer_class$minimum
  maximum <- nummer_class$maximum

  if (minimum > maximum) {
    stop(
      paste0(
        "Ongeldige range in class '",
        class_meta,
        "': minimum is groter dan maximum."
      ),
      call. = FALSE
    )
  }

  # Eventuele uitzonderingscodes

  optie_rijen <- if (
    eindrij > meta_rij
  ) {
    seq.int(
      meta_rij + 1L,
      eindrij
    )
  } else {
    integer(0)
  }

  toegestane_codes <- meta$value[
    optie_rijen
  ]

  toegestane_codes <- toegestane_codes[
    !is.na(toegestane_codes) &
      trimws(
        as.character(
          toegestane_codes
        )
      ) != ""
  ]

  toegestane_codes <- trimws(
    as.character(
      toegestane_codes
    )
  )

  waarden <- unique(
    dataset[[variabele_naam]]
  )

  waarden <- waarden[
    !is.na(waarden)
  ]

  waarden_tekst <- trimws(
    as.character(
      waarden
    )
  )

  controleren <- waarden[
    !waarden_tekst %in%
      toegestane_codes
  ]

  controleren_tekst <- trimws(
    as.character(
      controleren
    )
  )

  controleren_num <- suppressWarnings(
    as.numeric(
      controleren_tekst
    )
  )

  niet_numeriek <- is.na(
    controleren_num
  )

  buiten_range <- (
    !niet_numeriek &
      (
        controleren_num < minimum |
          controleren_num > maximum
      )
  )

  if (class_type == "INT") {
    niet_geheel <- (
      !niet_numeriek &
        controleren_num %% 1 != 0
    )
  } else {
    niet_geheel <- rep(
      FALSE,
      length(controleren_num)
    )
  }

  fout <- (
    niet_numeriek |
      buiten_range |
      niet_geheel
  )

  ongeldige_waarden <- controleren[
    fout
  ]

  if (
    length(ongeldige_waarden) > 0L &&
      melding
  ) {
    signal(
      paste0(
        "Variabele '",
        variabele_naam,
        "' voldoet niet aan class ",
        class_meta,
        " (toegestane range ",
        minimum,
        " t/m ",
        maximum,
        "). Ongeldige waarden: ",
        paste(
          ongeldige_waarden,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  ongeldige_waarden
}

controleren_datatype <- function(
    variabele_naam,
    dataset,
    meta,
    meta_rij) {
  class_meta <- trimws(
    as.character(
      meta$class[meta_rij]
    )
  )

  class_type <- sub(
    "\\[.*$",
    "",
    class_meta
  )

  waarden <- dataset[[variabele_naam]]

  # Volledig lege variabele

  if (all(is.na(waarden))) {
    return(NULL)
  }

  # INT / REAL

  if (
    class_type %in% c(
      "INT",
      "REAL"
    )
  ) {
    if (!is.numeric(waarden)) {
      return(
        paste0(
          variabele_naam,
          ": class ",
          class_meta,
          " moet numeric zijn in R, maar is ",
          paste(
            class(waarden),
            collapse = "/"
          )
        )
      )
    }

    return(NULL)
  }

  # STR

  if (class_type == "STR") {
    if (!is.character(waarden)) {
      return(
        paste0(
          variabele_naam,
          ": class ",
          class_meta,
          " moet character zijn in R, maar is ",
          paste(
            class(waarden),
            collapse = "/"
          )
        )
      )
    }

    return(NULL)
  }

  # ENUM

  if (class_type == "ENUM") {
    hoofdregels <- which(
      !is.na(meta$variable) &
        trimws(
          as.character(
            meta$variable
          )
        ) != ""
    )

    volgende_rij <- hoofdregels[
      hoofdregels > meta_rij
    ]

    if (length(volgende_rij) == 0L) {
      eindrij <- nrow(meta)
    } else {
      eindrij <- volgende_rij[1] - 1L
    }

    detail_rijen <- if (
      eindrij > meta_rij
    ) {
      seq.int(
        meta_rij + 1L,
        eindrij
      )
    } else {
      integer(0)
    }

    codes <- meta$value[
      detail_rijen
    ]

    codes <- trimws(
      as.character(
        codes[
          !is.na(codes) &
            trimws(
              as.character(codes)
            ) != ""
        ]
      )
    )

    heeft_voorloopnullen <- any(
      grepl(
        "^0[0-9]+$",
        codes
      )
    )

    if (heeft_voorloopnullen) {
      if (!is.character(waarden)) {
        return(
          paste0(
            variabele_naam,
            ": ",
            class_meta,
            " bevat codes met voorloopnullen en moet daarom ",
            "character zijn in R, maar is ",
            paste(
              class(waarden),
              collapse = "/"
            )
          )
        )
      }
    } else {
      if (!is.numeric(waarden)) {
        return(
          paste0(
            variabele_naam,
            ": ",
            class_meta,
            " bevat geen voorloopnullen en moet daarom ",
            "numeric zijn in R, maar is ",
            paste(
              class(waarden),
              collapse = "/"
            )
          )
        )
      }
    }

    return(NULL)
  }

  NULL
}

#' Metadata inlezen
#'
#' Leest metadata uit een Excelbestand, controleert de structuur,
#' verwerkt valuesets en bouwt een metadata-index op.
#'
#' De metadata uit alle tabbladen wordt samengevoegd. Vervolgens worden
#' eventuele valuesets verwerkt, worden variabelenamen gecontroleerd en
#' wordt een metadata-index opgebouwd met de uitgeklapte variabelenamen.
#'
#' Na het inlezen worden kenmerken van de metadata toegevoegd aan het
#' controlebestand: het aantal tabbladen, het aantal variabelen, het aantal
#' uitgeklapte variabelen, het aantal gebruikte valuesets en het aantal
#' keyvariabelen.
#'
#' @param bestand Pad naar het Excelbestand met metadata.
#' @param error Logical. Bepaalt of geconstateerde metadatafouten tot een
#'   error (`TRUE`) of warning (`FALSE`) leiden.
#' @param omschrijving Optionele omschrijving voor het controlebestand.
#'   Indien niet opgegeven wordt `"Metadata inlezen"` gebruikt.
#'
#' @return Een dataframe met gecombineerde metadata. Het attribuut
#'   `meta_index` bevat de uitgeklapte variabelenamen en de
#'   bijbehorende metadataregels.
#'
#' @examples
#' bestand <- file.path(
#'   tempdir(),
#'   "voorbeeld_metadata.xlsx"
#' )
#'
#' meta <- data.frame(
#'   blokpad = c("", "", ""),
#'   variable = c("geslacht", "", ""),
#'   label = c("Geslacht", "Man", "Vrouw"),
#'   class = c("ENUM[2]", "", ""),
#'   key = c("", "", ""),
#'   value = c("", "1", "2"),
#'   valueset = c("", "", ""),
#'   stringsAsFactors = FALSE
#' )
#'
#' openxlsx::write.xlsx(
#'   meta,
#'   bestand,
#'   sheetName = "Persoon"
#' )
#'
#' meta_ingelezen <- lees_meta_in(
#'   bestand
#' )
#'
#' meta_ingelezen
#' attr(meta_ingelezen, "meta_index")
#'
#' @export
lees_meta_in <- function(
    bestand,
    error = TRUE,
    omschrijving = NULL) {

  starttijd <- Sys.time()

  schrijf_log(
    "START lees_meta_in"
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
            "Metadata inlezen uit ",
            basename(bestand)
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

        tabbladen <- openxlsx::getSheetNames(
          bestand
        )

        lijst <- lapply(
          seq_along(tabbladen),
          function(i) {

            tabblad <- tabbladen[i]

            df <- as.data.frame(
              openxlsx2::read_xlsx(
                bestand,
                sheet = tabblad,
                convert = FALSE
              )
            )

            controleren_meta_structuur(
              meta = df,
              tabblad = tabblad,
              error = error
            )

            df$Tabblad <- tabblad

            df
          }
        )

        data <- do.call(
          rbind,
          lijst
        )

        rownames(data) <- NULL

        data <- uitklappen_valuesets(
          data
        )

        controleren_meta_unieke_namen(
          data,
          error = error
        )

        hoofdregels <- which(
          !is.na(data$variable) &
            trimws(
              as.character(
                data$variable
              )
            ) != ""
        )

        uitgeklapt <- lapply(
          data$variable[hoofdregels],
          expand_range
        )

        aantallen <- lengths(
          uitgeklapt
        )

        meta_index <- data.frame(
          variable = unlist(
            uitgeklapt,
            use.names = FALSE
          ),
          meta_rij = rep(
            hoofdregels,
            aantallen
          ),
          stringsAsFactors = FALSE
        )

        attr(
          data,
          "meta_index"
        ) <- meta_index

        gebruikte_valuesets <- unique(
          data$valueset[
            !is.na(data$valueset) &
              trimws(
                as.character(
                  data$valueset
                )
              ) != ""
          ]
        )

        aantal_keys <- sum(
          !is.na(data$key) &
            data$key == "Ja"
        )

        rij <- data.frame(
          omschrijving = c(
            omschrijving,
            "Bestand",
            "Aantal tabbladen",
            "Aantal variabelen",
            "Aantal uitgeklapte variabelen",
            "Aantal valuesets gebruikt",
            "Aantal keyvariabelen"
          ),
          aantal = c(
            "",
            bestand,
            length(tabbladen),
            length(hoofdregels),
            nrow(meta_index),
            length(gebruikte_valuesets),
            aantal_keys
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
            "WARNING lees_meta_in\n",
            conditionMessage(w)
          )
        )
      }
    ),

    error = function(e) {

      schrijf_log(
        paste0(
          "ERROR lees_meta_in\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE lees_meta_in"
  )

  resultaat
}

### Hulpfuncties lees meta in ---------------------------------------------
controleren_meta_structuur <- function(
    meta,
    tabblad,
    verwachte_kolommen = c(
      "blokpad",
      "variable",
      "label",
      "class",
      "key",
      "value",
      "valueset"
    ),
    error = error) {
  signal <- if (error) stop else warning
  fout <- function(...) {
    signal(
      paste0(
        "Fout in metadata, tabblad '",
        tabblad,
        "': ",
        ...
      ),
      call. = FALSE
    )
  }

  # 1. Kolommen

  if (anyDuplicated(names(meta))) {
    fout("dubbele kolomnamen aangetroffen.")
  }

  ontbrekende_kolommen <- setdiff(
    verwachte_kolommen,
    names(meta)
  )

  if (length(ontbrekende_kolommen) > 0L) {
    fout(
      "verplichte kolom(men) ontbreken: ",
      paste(
        ontbrekende_kolommen,
        collapse = ", "
      )
    )

    return(FALSE)
  }

  # 2. Geen ongewenste spaties

  controle_kolommen <- c(
    "blokpad",
    "variable",
    "class",
    "key",
    "value",
    "valueset"
  )

  for (kolom in controle_kolommen) {
    x <- meta[[kolom]]

    fout_spaties <- (
      !is.na(x) &
        x != trimws(x)
    )

    if (any(fout_spaties)) {
      fout(
        "voor- of achterliggende spaties gevonden in kolom '",
        kolom,
        "', regel(s): ",
        paste(
          which(fout_spaties) + 1L,
          collapse = ", "
        )
      )
    }
  }

  # 3. Volledig lege regels

  lege_regel <- apply(
    meta,
    1,
    function(x) {
      all(
        is.na(x) |
          trimws(
            as.character(x)
          ) == ""
      )
    }
  )

  if (any(lege_regel)) {
    fout(
      "volledig lege regel(s) gevonden: ",
      paste(
        which(lege_regel) + 1L,
        collapse = ", "
      )
    )
  }

  # 4. Hoofdregels / detailregels

  hoofdregel <- (
    !is.na(meta$variable) &
      trimws(
        as.character(meta$variable)
      ) != ""
  )

  if (!any(hoofdregel)) {
    fout(
      "het tabblad bevat geen variabelen."
    )

    return(FALSE)
  }

  detailregel <- !hoofdregel

  eerste_hoofdregel <- which(
    hoofdregel
  )[1]

  if (eerste_hoofdregel > 1L) {
    fout(
      "regel(s) voor de eerste variabele gevonden."
    )
  }

  # 5. Hoofdregel moet class hebben

  geen_class <- (
    hoofdregel &
      (
        is.na(meta$class) |
          meta$class == ""
      )
  )

  if (any(geen_class)) {
    fout(
      "geen class opgegeven bij hoofdregel(s): ",
      paste(
        which(geen_class) + 1L,
        collapse = ", "
      )
    )
  }

  # 6. Value hoofdregel moet leeg zijn

  value_op_hoofdregel <- (
    hoofdregel &
      !is.na(meta$value) &
      meta$value != ""
  )

  if (any(value_op_hoofdregel)) {
    fout(
      "value moet leeg zijn op een variabele-hoofdregel. ",
      "Fout op regel(s): ",
      paste(
        which(value_op_hoofdregel) + 1L,
        collapse = ", "
      )
    )
  }

  # 7. Detailregels

  detail_zonder_value <- (
    detailregel &
      (
        is.na(meta$value) |
          meta$value == ""
      )
  )

  if (any(detail_zonder_value)) {
    fout(
      "detailregel zonder value gevonden op regel(s): ",
      paste(
        which(detail_zonder_value) + 1L,
        collapse = ", "
      )
    )
  }

  velden_detail_leeg <- c(
    "blokpad",
    "class",
    "key",
    "valueset"
  )

  for (kolom in velden_detail_leeg) {
    fout_detail <- (
      detailregel &
        !is.na(meta[[kolom]]) &
        meta[[kolom]] != ""
    )

    if (any(fout_detail)) {
      fout(
        "kolom '",
        kolom,
        "' moet leeg zijn op detailregels. Regel(s): ",
        paste(
          which(fout_detail) + 1L,
          collapse = ", "
        )
      )
    }
  }

  # 8. Labels

  label_ontbreekt <- (
    hoofdregel &
      (
        is.na(meta$label) |
          meta$label == ""
      )
  )

  if (any(label_ontbreekt)) {
    fout(
      "label ontbreekt op hoofdregel(s): ",
      paste(
        which(label_ontbreekt) + 1L,
        collapse = ", "
      )
    )
  }

  label_ontbreekt_bij_value <- (
    detailregel &
      !is.na(meta$value) &
      meta$value != "" &
      (
        is.na(meta$label) |
          meta$label == ""
      )
  )

  if (any(label_ontbreekt_bij_value)) {
    fout(
      "bij iedere value moet een label zijn opgegeven. ",
      "Label ontbreekt op regel(s): ",
      paste(
        which(label_ontbreekt_bij_value) + 1L,
        collapse = ", "
      )
    )
  }

  # 9. Key

  key_fout <- (
    hoofdregel &
      !is.na(meta$key) &
      !meta$key %in% c("", "Ja")
  )

  if (any(key_fout)) {
    fout(
      "key mag alleen leeg of 'Ja' zijn. Fout op regel(s): ",
      paste(
        which(key_fout) + 1L,
        collapse = ", "
      )
    )
  }

  # 10. Class-formaat

  class_hoofd <- trimws(
    as.character(meta$class)
  )

  ontbrekende_specificatie <- (
    hoofdregel &
      !is.na(class_hoofd) &
      class_hoofd != "" &
      !grepl(
        "^(ENUM|STR|INT|REAL)\\[.+\\]$",
        class_hoofd
      )
  )

  if (any(ontbrekende_specificatie)) {
    fout(
      "class moet altijd een specificatie tussen [ ] bevatten. ",
      "Bijvoorbeeld ENUM[2], STR[20], INT[2], REAL[3], ",
      "INT[0:100] of REAL[0:100]. ",
      "Fout op regel(s): ",
      paste(
        which(
          ontbrekende_specificatie
        ) + 1L,
        collapse = ", "
      )
    )
  }

  geldige_class <- paste0(
    "^(",
    "ENUM\\[\\d+\\]",
    "|STR\\[\\d+\\]",
    "|INT\\[[1-9]\\d*\\]",
    "|REAL\\[[1-9]\\d*\\]",
    "|INT\\[-?\\d+(?:\\.\\d+)?:-?\\d+(?:\\.\\d+)?\\]",
    "|REAL\\[-?\\d+(?:\\.\\d+)?:-?\\d+(?:\\.\\d+)?\\]",
    ")$"
  )

  fout_class <- (
    hoofdregel &
      !is.na(class_hoofd) &
      class_hoofd != "" &
      !grepl(
        geldige_class,
        class_hoofd
      )
  )

  if (any(fout_class)) {
    fout(
      "ongeldige class op regel(s): ",
      paste(
        which(fout_class) + 1L,
        collapse = ", "
      ),
      ". Toegestane notaties zijn ENUM[n], STR[n], ",
      "INT[n], REAL[n], INT[min:max] en REAL[min:max]."
    )
  }

  # 11. Valueset

  valueset_gevuld <- (
    hoofdregel &
      !is.na(meta$valueset) &
      trimws(
        as.character(meta$valueset)
      ) != ""
  )

  fout_valueset_class <- (
    valueset_gevuld &
      !grepl(
        "^ENUM\\[",
        class_hoofd
      )
  )

  fout_valueset_class[
    is.na(fout_valueset_class)
  ] <- FALSE

  if (any(fout_valueset_class)) {
    fout(
      "valueset mag alleen worden gebruikt bij ENUM-variabelen. ",
      "Fout op regel(s): ",
      paste(
        which(
          fout_valueset_class
        ) + 1L,
        collapse = ", "
      )
    )
  }

  # 12. Per variabele controleren

  hoofdregels <- which(
    hoofdregel
  )

  for (i in seq_along(hoofdregels)) {
    rij <- hoofdregels[i]

    eindrij <- if (
      i == length(hoofdregels)
    ) {
      nrow(meta)
    } else {
      hoofdregels[i + 1L] - 1L
    }

    detail_rijen <- if (
      eindrij > rij
    ) {
      seq.int(
        rij + 1L,
        eindrij
      )
    } else {
      integer(0)
    }

    class_meta <- trimws(
      as.character(
        meta$class[rij]
      )
    )

    if (
      is.na(class_meta) ||
        class_meta == ""
    ) {
      next
    }

    values <- if (
      length(detail_rijen) > 0L
    ) {
      meta$value[detail_rijen]
    } else {
      character(0)
    }

    values <- values[
      !is.na(values) &
        trimws(
          as.character(values)
        ) != ""
    ]

    # Lokale values moeten uniek zijn

    values_tekst <- trimws(
      as.character(values)
    )

    if (anyDuplicated(values_tekst)) {
      fout(
        "dubbele value(s) bij variabele '",
        meta$variable[rij],
        "': ",
        paste(
          unique(
            values_tekst[
              duplicated(values_tekst)
            ]
          ),
          collapse = ", "
        )
      )
    }

    # ENUM

    if (grepl(
      "^ENUM\\[",
      class_meta
    )) {
      verwacht <- as.integer(
        sub(
          "^ENUM\\[(\\d+)\\]$",
          "\\1",
          class_meta
        )
      )

      valueset <- trimws(
        as.character(
          meta$valueset[rij]
        )
      )

      heeft_valueset <- (
        !is.na(valueset) &&
          valueset != ""
      )

      # Met valueset wordt de definitieve controle
      # pas uitgevoerd na het samenvoegen van valuesets.

      if (!heeft_valueset) {
        if (length(values) != verwacht) {
          fout(
            "variabele '",
            meta$variable[rij],
            "' heeft ",
            class_meta,
            " maar ",
            length(values),
            " values."
          )
        }
      }
    }

    # INT / REAL

    if (grepl(
      "^(INT|REAL)\\[",
      class_meta
    )) {
      nummer_class <- lezen_num_class(
        class_meta
      )

      minimum <- nummer_class$minimum
      maximum <- nummer_class$maximum

      if (minimum > maximum) {
        fout(
          "minimum is groter dan maximum bij variabele '",
          meta$variable[rij],
          "': ",
          class_meta
        )
      }

      if (length(values) > 0L) {
        numeriek <- suppressWarnings(
          as.numeric(values)
        )

        if (any(is.na(numeriek))) {
          fout(
            "niet-numerieke uitzonderingscode bij variabele '",
            meta$variable[rij],
            "': ",
            paste(
              values[
                is.na(numeriek)
              ],
              collapse = ", "
            )
          )
        }
      }
    }
  }

  TRUE
}

controleren_meta_unieke_namen <- function(meta,
                                          error = TRUE) {
  signal <- if (error) stop else warning

  hoofdregels <- (
    !is.na(meta$variable) &
      trimws(
        as.character(meta$variable)
      ) != ""
  )

  variabelen <- trimws(
    as.character(
      meta$variable[hoofdregels]
    )
  )

  dubbele_variabelen <- unique(
    variabelen[
      duplicated(variabelen)
    ]
  )

  if (length(dubbele_variabelen) > 0L) {
    signal(
      paste0(
        "Metadata bevat dubbele variabelenamen: ",
        paste(
          dubbele_variabelen,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  uitgeklapte_variabelen <- unlist(
    lapply(
      variabelen,
      expand_range
    ),
    use.names = FALSE
  )

  dubbele_uitgeklapte_variabelen <- unique(
    uitgeklapte_variabelen[
      duplicated(
        uitgeklapte_variabelen
      )
    ]
  )

  if (
    length(
      dubbele_uitgeklapte_variabelen
    ) > 0L
  ) {
    signal(
      paste0(
        "Metadata bevat na het uitklappen dubbele variabelenamen: ",
        paste(
          dubbele_uitgeklapte_variabelen,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  blokpaden <- trimws(
    as.character(
      meta$blokpad[hoofdregels]
    )
  )

  blokpaden[
    is.na(blokpaden)
  ] <- ""

  volledige_namen <- Map(
    function(blokpad, variabele) {

      if (blokpad == "") {
        naam <- variabele
      } else {
        naam <- paste0(
          blokpad,
          ".",
          variabele
        )
      }

      expand_range(
        naam
      )
    },
    blokpaden,
    variabelen
  )

  uitgeklapte_volledige_namen <- unlist(
    volledige_namen,
    use.names = FALSE
  )

  dubbele_volledige_namen <- unique(
    uitgeklapte_volledige_namen[
      duplicated(
        uitgeklapte_volledige_namen
      )
    ]
  )

  if (length(dubbele_volledige_namen) > 0L) {
    signal(
      paste0(
        "Metadata bevat na het combineren van blokpad en variabelenaam ",
        "dubbele volledige variabelenamen: ",
        paste(
          dubbele_volledige_namen,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  TRUE
}

uitklappen_valuesets <- function(meta) {
  if (!"valueset" %in% names(meta)) {
    stop(
      "Kolom 'valueset' ontbreekt in de metadata.",
      call. = FALSE
    )
  }

  # 1. Hoofdregels bepalen

  hoofdregels <- which(
    !is.na(meta$variable) &
      trimws(
        as.character(meta$variable)
      ) != ""
  )

  if (length(hoofdregels) == 0L) {
    return(meta)
  }

  eindregels <- c(
    hoofdregels[-1L] - 1L,
    nrow(meta)
  )

  names(eindregels) <- as.character(
    hoofdregels
  )

  # 2. Informatie per variabele opbouwen

  info <- lapply(
    hoofdregels,
    function(rij) {
      eindrij <- unname(
        eindregels[
          as.character(rij)
        ]
      )

      detail_rijen <- if (
        eindrij > rij
      ) {
        seq.int(
          rij + 1L,
          eindrij
        )
      } else {
        integer(0)
      }

      detail_rijen <- detail_rijen[
        !is.na(
          meta$value[detail_rijen]
        ) &
          trimws(
            as.character(
              meta$value[detail_rijen]
            )
          ) != ""
      ]

      valueset <- trimws(
        as.character(
          meta$valueset[rij]
        )
      )

      if (
        is.na(valueset) ||
          valueset == ""
      ) {
        valueset <- NA_character_
      }

      list(
        rij = rij,
        eindrij = eindrij,
        detail_rijen = detail_rijen,
        variable = as.character(
          meta$variable[rij]
        ),
        class = trimws(
          as.character(
            meta$class[rij]
          )
        ),
        valueset = valueset
      )
    }
  )

  # 3. Alleen variabelen met valueset

  heeft_valueset <- vapply(
    info,
    function(x) {
      !is.na(x$valueset)
    },
    logical(1)
  )

  if (!any(heeft_valueset)) {
    return(meta)
  }

  info_valueset <- info[
    heeft_valueset
  ]

  # 4. Valueset mag alleen bij ENUM

  fout_class <- vapply(
    info_valueset,
    function(x) {
      !grepl(
        "^ENUM\\[\\d+\\]$",
        x$class
      )
    },
    logical(1)
  )

  if (any(fout_class)) {
    verkeerde <- info_valueset[
      fout_class
    ]

    stop(
      paste0(
        "Valuesets mogen alleen bij ENUM-variabelen worden gebruikt. ",
        "Fout bij: ",
        paste(
          vapply(
            verkeerde,
            function(x) {
              x$variable
            },
            character(1)
          ),
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  # 5. Alle valuesets

  alle_valuesets <- unique(
    vapply(
      info_valueset,
      function(x) {
        x$valueset
      },
      character(1)
    )
  )

  # 6. Basisdefinitie bepalen
  #
  # De eerste variabele met deze valueset die
  # detailregels bevat definieert de basisset.

  definitie_lookup <- list()

  for (naam_valueset in alle_valuesets) {
    kandidaten <- info_valueset[
      vapply(
        info_valueset,
        function(x) {
          identical(
            x$valueset,
            naam_valueset
          ) &&
            length(
              x$detail_rijen
            ) > 0L
        },
        logical(1)
      )
    ]

    if (length(kandidaten) == 0L) {
      stop(
        paste0(
          "Valueset '",
          naam_valueset,
          "' wordt gebruikt, maar bevat nergens ",
          "een basisdefinitie met values."
        ),
        call. = FALSE
      )
    }

    definitie_lookup[[naam_valueset]] <- kandidaten[[1]]
  }

  # 7. Iedere variabele controleren

  for (x in info_valueset) {
    bron <- definitie_lookup[[x$valueset]]

    basis_values <- trimws(
      as.character(
        meta$value[
          bron$detail_rijen
        ]
      )
    )

    is_bron <- identical(
      x$rij,
      bron$rij
    )

    if (is_bron) {
      totaal_values <- basis_values
    } else {
      eigen_values <- trimws(
        as.character(
          meta$value[
            x$detail_rijen
          ]
        )
      )

      dubbel <- intersect(
        basis_values,
        eigen_values
      )

      if (length(dubbel) > 0L) {
        stop(
          paste0(
            "Variabele '",
            x$variable,
            "' gebruikt valueset '",
            x$valueset,
            "' en bevat daarnaast eigen value(s) ",
            "die al in de valueset voorkomen: ",
            paste(
              dubbel,
              collapse = ", "
            ),
            "."
          ),
          call. = FALSE
        )
      }

      totaal_values <- c(
        basis_values,
        eigen_values
      )
    }

    verwacht_aantal <- as.integer(
      sub(
        "^ENUM\\[(\\d+)\\]$",
        "\\1",
        x$class
      )
    )

    werkelijk_aantal <- length(
      totaal_values
    )

    if (
      werkelijk_aantal != verwacht_aantal
    ) {
      stop(
        paste0(
          "Variabele '",
          x$variable,
          "' gebruikt valueset '",
          x$valueset,
          "' en heeft na samenvoegen ",
          werkelijk_aantal,
          " values, maar class ",
          x$class,
          " verwacht ",
          verwacht_aantal,
          "."
        ),
        call. = FALSE
      )
    }
  }

  # 8. Metadata opnieuw opbouwen

  resultaat <- vector(
    "list",
    length(hoofdregels)
  )

  for (i in seq_along(hoofdregels)) {
    rij <- hoofdregels[i]

    eindrij <- unname(
      eindregels[
        as.character(rij)
      ]
    )

    blok <- meta[
      rij:eindrij, ,
      drop = FALSE
    ]

    valueset <- trimws(
      as.character(
        meta$valueset[rij]
      )
    )

    heeft_set <- (
      !is.na(valueset) &&
        valueset != ""
    )

    # Geen valueset

    if (!heeft_set) {
      resultaat[[i]] <- blok
      next
    }

    bron <- definitie_lookup[[valueset]]

    # Basisdefinitie zelf

    if (identical(
      rij,
      bron$rij
    )) {
      resultaat[[i]] <- blok
      next
    }

    # Basis-detailregels kopieren

    bron_details <- meta[
      bron$detail_rijen, ,
      drop = FALSE
    ]

    velden_leeg <- intersect(
      c(
        "blokpad",
        "variable",
        "class",
        "key",
        "valueset"
      ),
      names(bron_details)
    )

    for (kolom in velden_leeg) {
      bron_details[[kolom]] <- NA
    }

    if ("Tabblad" %in% names(meta)) {
      bron_details$Tabblad <- meta$Tabblad[rij]
    }

    eigen_details <- if (
      nrow(blok) > 1L
    ) {
      blok[
        -1L, ,
        drop = FALSE
      ]
    } else {
      blok[
        FALSE, ,
        drop = FALSE
      ]
    }

    nieuw_blok <- rbind(
      blok[1L, , drop = FALSE],
      bron_details,
      eigen_details
    )

    resultaat[[i]] <- nieuw_blok
  }

  # 9. Alles combineren

  resultaat <- do.call(
    rbind,
    resultaat
  )

  rownames(resultaat) <- NULL

  resultaat
}
