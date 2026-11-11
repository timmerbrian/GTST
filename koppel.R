#' Wrapper functie merge
#'
#' Wrapper functie om merge heen zodat koppelen generiek gaat
#'
#' Functie waarmee standaard gekoppeld wordt & bepaalde informatie wordt opgeslagen
#' in een verslagbestand, zoals het aan gekoppelde vs niet gekoppelde rijen.
#'
#' @param x,y te koppelen data.frames
#' @param by,by.x,by.y kolommen om op te koppelen
#' @param naam_x naam van databestand x, om de logging leesbaarder te maken
#' @param naam_y naam van databestand y, om de logging leesbaarder te maken
#' @param omschrijving optioneel omschrijving om de logging van de koppeling aan het verslagbestand toe te voegen
#' @param ... argumenten om door te geven
#'
#' @returns gekoppeld data.frame object
#' @export
#' @import data.table
#'
#' @examples
#' invisible(koppel(
#'   mtcars,
#'   mtcars,
#'   by = "mpg"
#' ))
#'
#' # Met aanmaken log object
#' steekproef <- data.frame(
#'   id = c(1, 2, 4)
#' )
#'
#' data <- data.frame(
#'   id = 1:4
#' )
#'
#' samen <- GTST::koppel(
#'   data,
#'   steekproef,
#'   by = "id",
#'   naam_x = "Ingelezen bronbestand",
#'   naam_y = "Voorbeeldbestand steekproef",
#'   omschrijving = "Verrijken data & steekproef",
#'   all.x = TRUE
#' )
#'
#' controlebestand
koppel <- function(
    x,
    y,
    by = NULL,
    by.x = NULL,
    by.y = NULL,
    naam_x = NULL,
    naam_y = NULL,
    omschrijving = NULL,
    ...) {

  starttijd <- Sys.time()

  if (is.null(naam_x)) {
    naam_x <- deparse(
      substitute(x)
    )
  }

  if (is.null(naam_y)) {
    naam_y <- deparse(
      substitute(y)
    )
  }

  schrijf_log(
    "START koppel"
  )

  resultaat <- tryCatch(

    withCallingHandlers(

      {

        if (is.null(omschrijving)) {
          omschrijving <- paste0(
            naam_y,
            " koppelen aan ",
            naam_x
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

        args <- list(...)

        arg_names <- names(args)

        if (is.null(arg_names)) {
          arg_names <- rep(
            "",
            length(args)
          )
        }

        meta_namen <- names(
          formals(
            GTST::koppel_meta
          )
        )

        is_meta <- nzchar(
          arg_names
        ) &
          arg_names %in% meta_namen

        meta_args <- args[
          is_meta
        ]

        koppel_args <- args[
          !is_meta
        ]

        if (!is.null(by)) {

          by_x <- by
          by_y <- by

        } else {

          if (
            is.null(by.x) ||
            is.null(by.y)
          ) {
            stop(
              "Geef by.x en by.y aan als je by leeg laat!",
              call. = FALSE
            )
          }

          if (
            length(by.x) !=
            length(by.y)
          ) {
            stop(
              "by.x en by.y moeten evenveel sleutels bevatten.",
              call. = FALSE
            )
          }

          by_x <- by.x
          by_y <- by.y
        }

        missend_x <- setdiff(
          by_x,
          names(x)
        )

        missend_y <- setdiff(
          by_y,
          names(y)
        )

        if (length(missend_x) > 0L) {
          stop(
            "Sleutelkolom(men) ontbreken in ",
            naam_x,
            ": ",
            paste(
              missend_x,
              collapse = ", "
            ),
            call. = FALSE
          )
        }

        if (length(missend_y) > 0L) {
          stop(
            "Sleutelkolom(men) ontbreken in ",
            naam_y,
            ": ",
            paste(
              missend_y,
              collapse = ", "
            ),
            call. = FALSE
          )
        }

        gedeelde_kolommen <- intersect(
          setdiff(
            names(x),
            by_x
          ),
          setdiff(
            names(y),
            by_y
          )
        )

        n_gedeelde_kolommen <- length(
          gedeelde_kolommen
        )

        if (n_gedeelde_kolommen > 0L) {
          warning(
            "Let op, de volgende kolommen zijn niet uniek:\n",
            paste(
              gedeelde_kolommen,
              collapse = "\n"
            ),
            call. = FALSE
          )
        }

        if (!data.table::is.data.table(x)) {
          x <- data.table::as.data.table(x)
        }

        if (!data.table::is.data.table(y)) {
          y <- data.table::as.data.table(y)
        }

        n_rijen_voor <- nrow(x)

        x_keys <- unique(
          x[
            ,
            .SD,
            .SDcols = by_x
          ]
        )

        y_keys <- unique(
          y[
            ,
            .SD,
            .SDcols = by_y
          ]
        )

        if (!identical(by_x, by_y)) {
          data.table::setnames(
            y_keys,
            by_y,
            by_x
          )
        }

        n_x_uniek <- nrow(
          x_keys
        )

        n_y_uniek <- nrow(
          y_keys
        )

        n_gedeeld <- nrow(
          data.table::fintersect(
            x_keys,
            y_keys
          )
        )

        n_x_niet_in_y <- n_x_uniek -
          n_gedeeld

        n_y_niet_in_x <- n_y_uniek -
          n_gedeeld

        rm(
          x_keys,
          y_keys
        )

        if (!"sort" %in% names(koppel_args)) {
          koppel_args$sort <- FALSE
        }

        resultaat <- do.call(
          data.table::merge.data.table,
          c(
            list(
              x = x,
              y = y,
              by.x = by_x,
              by.y = by_y
            ),
            koppel_args
          )
        )

        n_rijen_na <- nrow(
          resultaat
        )

        data.table::setDF(
          resultaat
        )

        resultaat <- do.call(
          GTST::koppel_meta,
          c(
            list(
              dataset = resultaat
            ),
            meta_args,
            list(
              verplicht = FALSE,
              error = TRUE,
              dataset_naam = naam_x,
              interne_aanroep = TRUE
            )
          )
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
            paste0(
              "Aantal unieke sleutels in ",
              naam_x
            ),
            paste0(
              "Aantal unieke sleutels in ",
              naam_y
            ),
            "Gekoppelde unieke sleutels",
            paste0(
              "Unieke sleutels alleen in ",
              naam_x
            ),
            paste0(
              "Unieke sleutels alleen in ",
              naam_y
            ),
            "Dubbele kolomnamen",
            paste0(
              "Aantal rijen ",
              naam_x,
              " voor koppeling"
            ),
            "Aantal rijen na koppeling",
            names(meta_controle)
          ),
          aantal = c(
            "",
            n_x_uniek,
            n_y_uniek,
            n_gedeeld,
            n_x_niet_in_y,
            n_y_niet_in_x,
            n_gedeelde_kolommen,
            n_rijen_voor,
            n_rijen_na,
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
          waarden = c(
            "Sleutel(s)" = paste(
              paste0(
                by_x,
                " = ",
                by_y
              ),
              collapse = ", "
            ),
            stats::setNames(
              rij$aantal[-1L],
              rij$omschrijving[-1L]
            )
          ),
          duur = duur
        )

        resultaat
      },

      warning = function(w) {

        schrijf_log(
          paste0(
            "WARNING koppel\n",
            conditionMessage(w)
          )
        )
      }
    ),

    error = function(e) {

      schrijf_log(
        paste0(
          "ERROR koppel\n",
          conditionMessage(e)
        )
      )

      stop(e)
    }
  )

  schrijf_log(
    "EINDE koppel"
  )

  resultaat
}

# TODO: subfunctie van maken?
# TODO: fixen als we sleutels goed eruit kunnnen halen
