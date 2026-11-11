#' Voeg regels toe aan een loggingobject
#'
#' Interne hulpfunctie die een of meerdere regels toevoegt aan een
#' loggingobject in de globale omgeving.
#'
#' Wanneer het loggingobject nog niet bestaat, wordt het aangemaakt.
#' Wanneer `witregel = TRUE`, wordt voor de nieuwe regels een lege regel
#' toegevoegd als het loggingobject al bestaat.
#'
#' @param df_naam Character. Naam van het loggingobject in de globale omgeving.
#' @param rij Data.frame met de regel of regels die aan het loggingobject
#'   moeten worden toegevoegd.
#' @param witregel Logisch. Geeft aan of voor de nieuwe regels een lege regel
#'   moet worden toegevoegd. Standaard `TRUE`.
#'
#' @return Geeft niets expliciet terug. Het opgegeven loggingobject wordt
#'   aangemaakt of bijgewerkt in de globale omgeving.
#'
#' @keywords internal
schrijf_r_logging <- function(df_naam, rij, witregel = TRUE) {
  if (exists(df_naam, envir = .GlobalEnv)) {
    # Witregels voegen we boven aan toe zodat je meer controle hebt
    # over hoe de output eruit ziet.
    if (witregel) {
      rij <- rbind(rij[1, ], rij)
      rij[1, ] <- ""
    }

    df <- get(df_naam, envir = .GlobalEnv)
    df <- rbind(df, rij)
    assign(df_naam, df, envir = .GlobalEnv)
  } else {
    assign(df_naam, rij, envir = .GlobalEnv)
  }
}

#' log df filteren
#'
#' Interne hulpfunctie
#'
#' @param df df die formatted moet worden
#'
#' @returns formatted df
# Info kolom hoeft alleen voor de eerste rij gevuld te worden, hier schonen we dit op
# ihkv leesbaarheid
filter_log_df <- function(df) {
  if (nrow(df) > 1) {
    df[2:nrow(df), "Info"] <- NA
  }
  return(df)
}

#' Witregel toevoegen aan log df
#'
#' Interne hulpfunctie
#'
#' @param df df die formatting nodig heeft
#'
#' @returns formatted df
voeg_witregel_toe <- function(df) {
  kopie <- df
  kopie[, ] <- lapply(kopie[, ], as.character)
  kopie[1, ] <- ""
  regel <- kopie[1, ]
  df <- rbind(regel, df)
  return(df)
}

#' Wrapper om log op te schonen
#'
#' Interne hulpfunctie
#'
#' @param df df die formatted moet worden
#' @returns formatted df
bewerk_log <- function(df) {
  df <- filter_log_df(df)
  df <- voeg_witregel_toe(df)
  return(df)
}

#' Interne helper
#'
#' Helper om te kijken of we in een rproject zetten
#'
#' @returns logical, zitten we in een rproject
is_r_project <- function() {
  if (!is.null(rstudioapi::getActiveProject())) {
    TRUE
  } else {
    FALSE
  }
}

#' Interne helper
#'
#' @returns logical, zitten we in rstudio
is_rstudio_actief <- function() {
  if (Sys.getenv("RSTUDIO") == "1") {
    TRUE
  } else {
    FALSE
  }
}

#' Is gedraaid door callr
#'
#' Hulp functie die checkt of een script gedraaid is door callr (GTST dashboard)
#'
#' In het master dahboard runnen we scripts met CALLR. Hier geven we een env var mee
#' om aan te geven dat deze via callr gedraaid is. Deze halen we hier op om te kijken of
#' het script op deze manier aangeroepen is.
#'
#' @returns logical, is het script gedraaid via callr
is_callr_script <- function() {
  if (identical(Sys.getenv("CALLR_GEDRAAID"), "TRUE")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Vind scriptnaam
#'
#' @param met_extensie Geef de scriptnaam inclusief extensie terug (standaard TRUE).
#' @return De scriptnaam, eventueel zonder extensie.
#' @export
vind_script_naam <- function(met_extensie = TRUE) {
  if (is_rstudio_actief()) {
    scriptnaam <- basename(rstudioapi::getSourceEditorContext()$path)
  } else if (is_callr_script()) {
    scriptpad <- commandArgs()[grepl("\\.R$", commandArgs(), ignore.case = TRUE)]
    scriptnaam <- basename(utils::tail(scriptpad, 1L))
  } else {
    stop("Kan de scriptnaam niet bepalen buiten RStudio of callr.")
  }

  if (!met_extensie) {
    scriptnaam <- tools::file_path_sans_ext(scriptnaam)
  }

  scriptnaam
}

#' Stop draaien van scripts met GTST dashboard integratie
#'
#' Als je een script wil stoppen met draaien, maar deze niet in het masterdashboard
#' als error wil bestempelen (zodat je verdere scripts wel kan draaien)
#'
#' @param bericht error message
#' @param ... eventuele paramters die je aan stop wil doorgeven
#'
#' @returns stopt script
#' @export
#'
#' @examples
#' stop("Geen inputbestand, we slaan deze stap over. We gaan wel door met verdere script")
stop <- function(bericht, ...) {
  base::stop(bericht, ...)
}

expand_range <- function(x) {
  if (is.na(x) || trimws(x) == "") {
    return("")
  }

  x <- trimws(x)

  patroon <- "\\[(\\d+)\\:(\\d+)\\]"

  # Geen range: oorspronkelijke waarde teruggeven

  if (!grepl(patroon, x)) {
    return(x)
  }

  match <- regexec(
    patroon,
    x
  )

  onderdelen <- regmatches(
    x,
    match
  )[[1]]

  van <- as.integer(
    onderdelen[2]
  )

  tot <- as.integer(
    onderdelen[3]
  )

  waarden <- seq.int(
    van,
    tot
  )

  # Range vervangen door iedere mogelijke waarde

  uitgebreid <- vapply(
    waarden,
    function(i) {
      sub(
        patroon,
        as.character(i),
        x
      )
    },
    character(1)
  )

  # Ook eventuele tweede ranges uitklappen

  unlist(
    lapply(
      uitgebreid,
      expand_range
    ),
    use.names = FALSE
  )
}

lezen_num_class <- function(class_meta) {
  class_meta <- trimws(
    as.character(class_meta)
  )

  # 1. Expliciete range
  #
  # INT[1:20]
  # REAL[-10.5:25.5]

  patroon_range <- paste0(
    "^(INT|REAL)\\[",
    "(-?\\d+(?:\\.\\d+)?):",
    "(-?\\d+(?:\\.\\d+)?)",
    "\\]$"
  )

  delen <- regmatches(
    class_meta,
    regexec(
      patroon_range,
      class_meta
    )
  )[[1]]

  if (length(delen) > 0L) {
    return(
      list(
        class_type = delen[2],
        minimum = as.numeric(delen[3]),
        maximum = as.numeric(delen[4]),
        aantal_posities = NULL
      )
    )
  }

  # 2. Aantal posities
  #
  # INT[2]  -> -99:99
  # REAL[3] -> -999:999

  patroon_posities <- "^(INT|REAL)\\[([1-9]\\d*)\\]$"

  delen <- regmatches(
    class_meta,
    regexec(
      patroon_posities,
      class_meta
    )
  )[[1]]

  if (length(delen) > 0L) {
    aantal_posities <- as.integer(
      delen[3]
    )

    maximum <- 10^aantal_posities - 1
    minimum <- -maximum

    return(
      list(
        class_type = delen[2],
        minimum = minimum,
        maximum = maximum,
        aantal_posities = aantal_posities
      )
    )
  }

  stop(
    paste0(
      "Ongeldige numerieke class: '",
      class_meta,
      "'."
    ),
    call. = FALSE
  )
}

meta_ophalen <- function(
    dataset_naam,
    envir = parent.frame()) {
  # Datasetnaam moet als tekst worden opgegeven

  if (is.data.frame(dataset_naam)) {
    warning(
      "Geef de naam van de dataframe op, niet de dataframe zelf.",
      call. = FALSE
    )

    return(NULL)
  }

  # Bestaat Meta_Koppelingen?

  if (!exists(
    "Meta_Koppelingen",
    envir = envir,
    inherits = TRUE
  )) {
    warning(
      paste0(
        "Object 'Meta_Koppelingen' is niet gevonden. ",
        "Metadata wordt niet automatisch gekoppeld."
      ),
      call. = FALSE
    )

    return(NULL)
  }

  Meta_Koppelingen <- get(
    "Meta_Koppelingen",
    envir = envir,
    inherits = TRUE
  )

  # Is metadata geregistreerd?

  if (!dataset_naam %in% names(Meta_Koppelingen)) {
    warning(
      paste0(
        "Voor dataset '",
        dataset_naam,
        "' is geen metadata geregistreerd."
      ),
      call. = FALSE
    )

    return(NULL)
  }

  meta_naam <- unname(
    Meta_Koppelingen[
      dataset_naam
    ]
  )

  # Bestaat metadata-object?

  if (!exists(
    meta_naam,
    envir = envir,
    inherits = TRUE
  )) {
    warning(
      paste0(
        "Voor dataset '",
        dataset_naam,
        "' is metadata '",
        meta_naam,
        "' geregistreerd, maar dit object is niet gevonden."
      ),
      call. = FALSE
    )

    return(NULL)
  }

  meta <- get(
    meta_naam,
    envir = envir,
    inherits = TRUE
  )

  if (!is.data.frame(meta)) {
    warning(
      paste0(
        "Metadata-object '",
        meta_naam,
        "' voor dataset '",
        dataset_naam,
        "' is geen dataframe."
      ),
      call. = FALSE
    )

    return(NULL)
  }

  meta
}

meta_resolver <- function(
    meta = NULL,
    dataset_naam = NULL,
    envir = parent.frame(),
    verplicht = TRUE) {
  # Handmatig opgegeven metadata heeft altijd voorrang

  if (!is.null(meta)) {
    if (!is.data.frame(meta)) {
      stop(
        "'meta' moet een dataframe zijn.",
        call. = FALSE
      )
    }

    return(meta)
  }

  # Zonder datasetnaam kan automatische koppeling niet

  if (
    is.null(dataset_naam) ||
      length(dataset_naam) != 1L ||
      is.na(dataset_naam) ||
      dataset_naam == ""
  ) {
    if (verplicht) {
      stop(
        paste0(
          "Geen metadata opgegeven en de bijbehorende dataset ",
          "kon niet worden bepaald."
        ),
        call. = FALSE
      )
    }

    return(NULL)
  }

  # Automatisch ophalen

  meta <- meta_ophalen(
    dataset_naam = dataset_naam,
    envir = envir
  )

  if (is.null(meta)) {
    if (verplicht) {
      stop(
        paste0(
          "Geen metadata gevonden voor dataset '",
          dataset_naam,
          "'."
        ),
        call. = FALSE
      )
    }

    return(NULL)
  }

  if (!is.data.frame(meta)) {
    stop(
      paste0(
        "De metadata gekoppeld aan dataset '",
        dataset_naam,
        "' is geen dataframe."
      ),
      call. = FALSE
    )
  }

  meta
}

meta_mapping <- function(
    variabele_naam,
    meta) {
  meta_index <- attr(
    meta,
    "meta_index"
  )

  if (is.null(meta_index)) {
    stop(
      paste0(
        "Metadata bevat geen 'meta_index'. ",
        "Lees de metadata in met lees_meta_in()."
      ),
      call. = FALSE
    )
  }

  meta_variabelen <- meta_index$variable

  gevonden <- vapply(
    variabele_naam,
    function(naam) {
      kandidaten <- meta_variabelen[
        naam == meta_variabelen |
          endsWith(
            naam,
            paste0(
              ".",
              meta_variabelen
            )
          )
      ]

      if (length(kandidaten) == 0L) {
        return(NA_character_)
      }

      # Langste match nemen
      #
      # Bijvoorbeeld bij:
      # X
      # A.X

      kandidaten[
        which.max(
          nchar(kandidaten)
        )
      ]
    },
    character(1)
  )

  meta_rij <- meta_index$meta_rij[
    match(
      gevonden,
      meta_index$variable
    )
  ]

  data.frame(
    dataset_naam = variabele_naam,
    variable = unname(gevonden),
    meta_rij = unname(meta_rij),
    stringsAsFactors = FALSE
  )
}

integer64_naar_numeric <- function(data) {
  is_integer64 <- vapply(
    data,
    function(x) {
      inherits(x, "integer64")
    },
    logical(1)
  )

  if (any(is_integer64)) {
    data[is_integer64] <- lapply(
      data[is_integer64],
      as.numeric
    )
  }

  data
}


# Interne expressie-infrastructuur ------------------------------------------


#' Maak interne expressiecontext
#'
#' Maakt een interne omgeving voor gedeelde status van
#' expressiehelpers zoals has_match(), lookup() en agg().
#'
#' @keywords internal
maak_expressie_context <- function() {
  context <- new.env(
    parent = emptyenv()
  )

  context$laatste_match <- NULL
  context$laatste_lookup_values <- character(0)

  context$actieve_condition <- NULL
  context$basis_condition <- NULL
  context$current_regel <- NULL
  context$has_match_mode <- "normaal"

  context$koppelingen <- list()

  context
}



# Groepssleutels normaliseren



#' Normaliseer groepssleutels
#'
#' Zet een enkele vector, list, dataframe of matrix om naar
#' een naamloze lijst met groepsvectoren.
#'
#' @param by Een of meerdere groepssleutels.
#' @param lengte Verwachte lengte van iedere groepssleutel.
#'
#' @keywords internal
normaliseer_groepen <- function(
    by,
    lengte) {
  if (is.data.frame(by)) {
    by_list <- unname(
      as.list(by)
    )
  } else if (is.matrix(by)) {
    by_list <- unname(
      as.list(
        as.data.frame(by)
      )
    )
  } else if (is.list(by)) {
    by_list <- unname(by)
  } else {
    by_list <- list(by)
  }

  if (length(by_list) == 0L) {
    stop(
      "by moet minimaal een groepssleutel bevatten.",
      call. = FALSE
    )
  }

  verkeerde_lengtes <- (
    lengths(by_list) != lengte
  )

  if (any(verkeerde_lengtes)) {
    stop(
      paste0(
        "Alle sleutels in by moeten dezelfde lengte hebben ",
        "als de te groeperen vector."
      ),
      call. = FALSE
    )
  }

  by_list
}



# Expressieomgeving



#' Maak interne expressieomgeving
#'
#' Maakt een evaluatieomgeving met de gedeelde GTST-
#' expressiehelpers.
#'
#' @param data Optioneel dataframe waarvan kolommen rechtstreeks
#'   beschikbaar worden in expressies.
#' @param caller_env Omgeving waarin externe objecten worden gezocht.
#' @param context Interne expressiecontext.
#'
#' @keywords internal
maak_expressie_omgeving <- function(
    data = NULL,
    caller_env = parent.frame(),
    context = NULL) {
  if (is.null(context)) {
    context <- maak_expressie_context()
  }

  if (!is.environment(context)) {
    stop(
      "context moet een environment zijn.",
      call. = FALSE
    )
  }

  if (
    !is.null(data) &&
      !is.data.frame(data)
  ) {
    stop(
      "data moet NULL of een dataframe zijn.",
      call. = FALSE
    )
  }

  lookup <- function(
      x = NULL,
      tabel = NULL,
      by = NULL,
      value = NULL) {
    if (is.null(value)) {
      value <- x

      if (is.null(context$laatste_match)) {
        stop(
          paste0(
            "lookup('waarde') kan alleen na has_match(). ",
            "Gebruik anders lookup(x, tabel, by, value)."
          ),
          call. = FALSE
        )
      }

      x <- context$laatste_match$x
      tabel <- context$laatste_match$tabel
      by <- context$laatste_match$by
      m <- context$laatste_match$m
    } else {
      if (length(by) != 1L) {
        stop(
          paste0(
            "Bij de expliciete vorm lookup(x, tabel, by, value) ",
            "wordt momenteel een sleutel ondersteund. ",
            "Gebruik has_match(list(...), tabel, by = ...) gevolgd ",
            "door lookup('waarde') voor meerdere sleutels."
          ),
          call. = FALSE
        )
      }

      m <- match(
        x,
        tabel[[by]]
      )
    }

    value <- as.character(value)

    ontbrekende_values <- setdiff(
      value,
      names(tabel)
    )

    if (length(ontbrekende_values) > 0L) {
      stop(
        paste0(
          "Ontbrekende kolommen in lookup(): ",
          paste(
            ontbrekende_values,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    out <- lapply(
      value,
      function(v) {
        if (nrow(tabel) == 0L) {
          rep(
            NA,
            length(m)
          )
        } else {
          tabel[[v]][m]
        }
      }
    )

    context$laatste_lookup_values <- unique(
      c(
        context$laatste_lookup_values,
        value
      )
    )

    if (
      !is.null(context$laatste_match) &&
        !is.null(context$laatste_match$koppeling_id)
    ) {
      koppeling_id <- context$laatste_match$koppeling_id

      if (koppeling_id <= length(context$koppelingen)) {
        context$koppelingen[[
          koppeling_id
        ]]$opgehaald <- unique(
          c(
            context$koppelingen[[
              koppeling_id
            ]]$opgehaald,
            value
          )
        )
      }
    }

    if (length(value) == 1L) {
      out[[1L]]
    } else {
      out
    }
  }

  has_match <- function(
      x,
      tabel,
      by = NULL) {
    tabel_expr <- substitute(tabel)

    tabel_naam <- deparse(
      tabel_expr
    )

    if (is.null(by)) {
      if (
        is.list(x) ||
          is.data.frame(x) ||
          is.matrix(x)
      ) {
        stop(
          paste0(
            "Bij meerdere sleutels moet 'by' expliciet worden ",
            "opgegeven, bijvoorbeeld ",
            "by = c('Key1', 'Key2')."
          ),
          call. = FALSE
        )
      }

      by <- deparse(
        substitute(x)
      )
    }

    if (
      !is.character(by) ||
        length(by) < 1L
    ) {
      stop(
        "by moet een of meerdere kolomnamen bevatten.",
        call. = FALSE
      )
    }

    if (
      !is.data.frame(tabel) &&
        exists(
          tabel_naam,
          envir = caller_env,
          inherits = TRUE
        )
    ) {
      tabel_parent <- get(
        tabel_naam,
        envir = caller_env,
        inherits = TRUE
      )

      if (is.data.frame(tabel_parent)) {
        tabel <- tabel_parent
      }
    }

    if (!is.data.frame(tabel)) {
      stop(
        paste0(
          "tabel moet een dataframe zijn. Gevonden object '",
          tabel_naam,
          "' is: ",
          paste(
            class(tabel),
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    ontbrekende_kolommen <- setdiff(
      by,
      names(tabel)
    )

    if (length(ontbrekende_kolommen) > 0L) {
      stop(
        paste0(
          "Kolom(men) ",
          paste0(
            "'",
            ontbrekende_kolommen,
            "'",
            collapse = ", "
          ),
          " bestaan niet in tabel '",
          tabel_naam,
          "'."
        ),
        call. = FALSE
      )
    }

    if (length(by) == 1L) {
      if (
        is.data.frame(x) ||
          is.matrix(x)
      ) {
        if (ncol(x) != 1L) {
          stop(
            paste0(
              "x bevat meerdere kolommen, maar by bevat ",
              "maar een sleutel."
            ),
            call. = FALSE
          )
        }

        x_list <- list(
          x[[1L]]
        )
      } else if (is.list(x)) {
        if (length(x) != 1L) {
          stop(
            paste0(
              "x bevat meerdere sleutels, maar by bevat ",
              "maar een sleutel."
            ),
            call. = FALSE
          )
        }

        x_list <- unname(x)
      } else {
        x_list <- list(x)
      }
    } else {
      if (is.data.frame(x)) {
        x_list <- unname(
          as.list(x)
        )
      } else if (is.matrix(x)) {
        x_list <- unname(
          as.list(
            as.data.frame(x)
          )
        )
      } else if (is.list(x)) {
        x_list <- unname(x)
      } else {
        stop(
          paste0(
            "Bij meerdere sleutels moet x een list, dataframe ",
            "of matrix zijn. Gebruik bijvoorbeeld ",
            "list(Key1, Key2)."
          ),
          call. = FALSE
        )
      }

      if (length(x_list) != length(by)) {
        stop(
          paste0(
            "Het aantal sleutels in x (",
            length(x_list),
            ") is niet gelijk aan het aantal kolommen in by (",
            length(by),
            ")."
          ),
          call. = FALSE
        )
      }
    }

    lengtes_x <- lengths(
      x_list
    )

    if (length(unique(lengtes_x)) != 1L) {
      stop(
        "Alle sleutelvectoren in x moeten dezelfde lengte hebben.",
        call. = FALSE
      )
    }

    n_x <- lengtes_x[[1L]]

    encode_key <- function(values) {
      encoded <- lapply(
        values,
        function(z) {
          z <- as.character(z)

          ifelse(
            is.na(z),
            "<NA>",
            paste0(
              nchar(z),
              ":",
              z
            )
          )
        }
      )

      do.call(
        paste,
        c(
          encoded,
          sep = "\034"
        )
      )
    }

    if (nrow(tabel) == 0L) {
      m <- rep(
        NA_integer_,
        n_x
      )
    } else if (length(by) == 1L) {
      m <- match(
        x_list[[1L]],
        tabel[[by]]
      )
    } else {
      sleutel_x <- encode_key(
        x_list
      )

      sleutel_tabel <- encode_key(
        tabel[by]
      )

      m <- match(
        sleutel_x,
        sleutel_tabel
      )
    }

    resultaat <- !is.na(m)

    if (identical(
      context$has_match_mode,
      "basis"
    )) {
      context$laatste_match <- list(
        x = x_list,
        tabel = tabel,
        by = by,
        tabel_naam = tabel_naam,
        m = m,
        koppeling_id = NULL
      )

      return(
        rep(
          TRUE,
          n_x
        )
      )
    }

    if (nrow(tabel) == 0L) {
      dubbele_bronsleutels <- 0L
    } else {
      if (length(by) == 1L) {
        sleutel_tabel <- as.character(
          tabel[[by]]
        )

        sleutel_tabel[
          is.na(sleutel_tabel)
        ] <- "<NA>"
      } else {
        sleutel_tabel <- encode_key(
          tabel[by]
        )
      }

      sleutel_frequenties <- table(
        sleutel_tabel,
        useNA = "ifany"
      )

      dubbele_bronsleutels <- sum(
        sleutel_frequenties > 1L
      )
    }

    basis_condition <- context$basis_condition

    if (is.null(basis_condition)) {
      basis_condition <- rep(
        TRUE,
        n_x
      )
    }

    if (length(basis_condition) == 1L) {
      basis_condition <- rep(
        basis_condition,
        n_x
      )
    }

    if (length(basis_condition) != n_x) {
      stop(
        paste0(
          "Interne fout: basisconditie heeft lengte ",
          length(basis_condition),
          ", terwijl has_match() ",
          n_x,
          " waarden controleert."
        ),
        call. = FALSE
      )
    }

    basis_condition[
      is.na(basis_condition)
    ] <- FALSE

    gecontroleerd <- sum(
      basis_condition
    )

    aantal_match <- sum(
      basis_condition &
        resultaat
    )

    aantal_geen_match <- sum(
      basis_condition &
        !resultaat
    )

    koppeling_id <- length(
      context$koppelingen
    ) + 1L

    context$koppelingen[[
      koppeling_id
    ]] <- list(
      regel = context$current_regel,
      tabel = tabel_naam,
      sleutel = by,
      gecontroleerd = gecontroleerd,
      match = aantal_match,
      geen_match = aantal_geen_match,
      dubbele_bronsleutels = dubbele_bronsleutels,
      opgehaald = character(0)
    )

    context$laatste_match <- list(
      x = x_list,
      tabel = tabel,
      by = by,
      tabel_naam = tabel_naam,
      m = m,
      koppeling_id = koppeling_id
    )

    resultaat
  }

  nz <- function(
      x,
      value = 0) {
    replace(
      x,
      is.na(x),
      value
    )
  }

  same <- function(
      x,
      y) {
    (is.na(x) & is.na(y)) |
      (
        !is.na(x) &
          !is.na(y) &
          x == y
      )
  }

  agg <- function(
      x,
      by,
      FUN = sum,
      na.rm = TRUE) {
    by_list <- normaliseer_groepen(
      by = by,
      lengte = length(x)
    )

    if (!is.null(
      context$actieve_condition
    )) {
      if (
        length(
          context$actieve_condition
        ) != length(x)
      ) {
        stop(
          paste0(
            "Interne fout: actieve conditie heeft ",
            "een verkeerde lengte."
          ),
          call. = FALSE
        )
      }

      x[
        !context$actieve_condition
      ] <- NA
    }

    do.call(
      stats::ave,
      c(
        list(x),
        by_list,
        list(
          FUN = function(z) {
            if (all(is.na(z))) {
              NA
            } else {
              FUN(
                z,
                na.rm = na.rm
              )
            }
          }
        )
      )
    )
  }

  find <- function(
      conditie,
      waarde,
      by,
      FUN = max,
      na.rm = TRUE) {
    by_list <- normaliseer_groepen(
      by = by,
      lengte = length(conditie)
    )

    if (!is.logical(conditie)) {
      stop(
        "conditie moet een logische vector zijn.",
        call. = FALSE
      )
    }

    conditie[
      is.na(conditie)
    ] <- FALSE

    if (
      is.data.frame(waarde) ||
        is.matrix(waarde)
    ) {
      waarde <- unname(
        as.list(
          as.data.frame(waarde)
        )
      )
    }

    if (is.list(waarde)) {
      verkeerde_lengtes <- (
        lengths(waarde) !=
          length(conditie)
      )

      if (any(verkeerde_lengtes)) {
        stop(
          paste0(
            "Alle variabelen in waarde moeten dezelfde lengte ",
            "hebben als conditie en by."
          ),
          call. = FALSE
        )
      }

      return(
        lapply(
          waarde,
          function(x) {
            agg(
              ifelse(
                conditie,
                x,
                NA
              ),
              by_list,
              FUN = FUN,
              na.rm = na.rm
            )
          }
        )
      )
    }

    if (
      length(waarde) !=
        length(conditie)
    ) {
      stop(
        paste0(
          "conditie, waarde en by moeten dezelfde ",
          "lengte hebben."
        ),
        call. = FALSE
      )
    }

    agg(
      ifelse(
        conditie,
        waarde,
        NA
      ),
      by_list,
      FUN = FUN,
      na.rm = na.rm
    )
  }

  inhoud <- list(
    lookup = lookup,
    has_match = has_match,
    agg = agg,
    find = find,
    nz = nz,
    same = same
  )

  if (!is.null(data)) {
    inhoud <- c(
      as.list(data),
      inhoud
    )
  }

  list2env(
    inhoud,
    parent = caller_env
  )
}



# Selectiecontrole



#' Maak selectiecontrole
#'
#' Maakt uniforme controle-aantallen voor rij- en
#' kolomselecties.
#'
#' @param n_rijen_voor Aantal rijen voor selectie.
#' @param n_rijen_na Aantal rijen na selectie.
#' @param kolommen_voor Kolomnamen voor selectie.
#' @param kolommen_na Kolomnamen na selectie.
#'
#' @keywords internal
maak_selectie_controle <- function(
    n_rijen_voor = NULL,
    n_rijen_na = NULL,
    kolommen_voor = NULL,
    kolommen_na = NULL) {
  uitkomst <- numeric(0)


  # Rijen


  if (
    !is.null(n_rijen_voor) ||
      !is.null(n_rijen_na)
  ) {
    if (
      is.null(n_rijen_voor) ||
        is.null(n_rijen_na)
    ) {
      stop(
        paste0(
          "Voor rijselectie moeten zowel n_rijen_voor ",
          "als n_rijen_na worden opgegeven."
        ),
        call. = FALSE
      )
    }

    uitkomst <- c(
      uitkomst,
      "Aantal rijen voor selectie" = n_rijen_voor,
      "Aantal rijen behouden" = n_rijen_na,
      "Aantal rijen verwijderd" = (
        n_rijen_voor -
          n_rijen_na
      )
    )
  }



  # Kolommen


  if (
    !is.null(kolommen_voor) ||
      !is.null(kolommen_na)
  ) {
    if (
      is.null(kolommen_voor) ||
        is.null(kolommen_na)
    ) {
      stop(
        paste0(
          "Voor kolomselectie moeten zowel kolommen_voor ",
          "als kolommen_na worden opgegeven."
        ),
        call. = FALSE
      )
    }

    kolommen_voor <- as.character(
      kolommen_voor
    )

    kolommen_na <- as.character(
      kolommen_na
    )

    aantal_behouden <- length(
      kolommen_na
    )

    aantal_verwijderd <- length(
      setdiff(
        kolommen_voor,
        kolommen_na
      )
    )

    uitkomst <- c(
      uitkomst,
      "Aantal kolommen voor selectie" = length(kolommen_voor),
      "Aantal kolommen behouden" = aantal_behouden,
      "Aantal kolommen verwijderd" = aantal_verwijderd
    )
  }

  uitkomst
}
