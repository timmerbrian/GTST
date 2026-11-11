#' @export
print.GTST_controle_variabele <- function(
    x,
    ...) {
  if (
    !is.null(x$regels) &&
      nrow(x$regels) > 0L
  ) {
    cat("\nREGELS\n\n")

    print(
      x$regels,
      row.names = FALSE
    )

    cat(
      "\nGeen regel van toepassing: ",
      x$geen_regel,
      "\n",
      sep = ""
    )

    cat(
      "Overlap tussen regels:     ",
      x$overlap,
      "\n",
      sep = ""
    )
  }

  if (
    !is.null(x$matches) &&
      nrow(x$matches) > 0L
  ) {
    cat("\nKOPPELINGEN\n")

    for (
      i in seq_len(
        nrow(x$matches)
      )
    ) {
      match_row <- x$matches[
        i, ,
        drop = FALSE
      ]

      cat(
        "\n ",
        match_row$regel,
        "  ",
        match_row$tabel,
        "\n",
        sep = ""
      )

      cat(
        "      Sleutel                  ",
        match_row$by,
        "\n",
        sep = ""
      )

      gecontroleerd <- match_row$rijen_x
      aantal_match <- match_row$match

      percentage <- if (
        gecontroleerd == 0L
      ) {
        0
      } else {
        100 *
          aantal_match /
          gecontroleerd
      }

      cat(
        "      Match                     ",
        format(
          aantal_match,
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        ),
        " / ",
        format(
          gecontroleerd,
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        ),
        " (",
        formatC(
          percentage,
          format = "f",
          digits = 1,
          decimal.mark = ","
        ),
        "%)\n",
        sep = ""
      )

      if (
        match_row$geen_match > 0L
      ) {
        cat(
          "      Geen match                ",
          format(
            match_row$geen_match,
            big.mark = ".",
            decimal.mark = ",",
            scientific = FALSE
          ),
          "\n",
          sep = ""
        )
      }

      if (
        match_row$dubbele_bronsleutels > 0L
      ) {
        cat(
          "      Dubbele bronsleutels      ",
          format(
            match_row$dubbele_bronsleutels,
            big.mark = ".",
            decimal.mark = ",",
            scientific = FALSE
          ),
          "\n",
          sep = ""
        )
      }

      if (
        !is.na(match_row$value) &&
          nzchar(match_row$value)
      ) {
        cat(
          "      Opgehaald                 ",
          match_row$value,
          "\n",
          sep = ""
        )
      }
    }
  }

  if (
    !is.null(x$wijzigingen) &&
      nrow(x$wijzigingen) > 0L
  ) {
    cat("\nWIJZIGINGEN\n\n")

    aantal_rijen <- nrow(
      x$wijzigingen
    )

    print(
      utils::head(
        x$wijzigingen,
        10L
      ),
      row.names = FALSE
    )

    if (aantal_rijen > 10L) {
      cat(
        "\n",
        aantal_rijen,
        " rijen totaal; gebruik $wijzigingen voor de volledige tabel.\n",
        sep = ""
      )
    }
  }

  if (
    !is.null(x$freq) &&
      nrow(x$freq) > 0L
  ) {
    cat("\nVERDELING\n\n")

    aantal_rijen <- nrow(
      x$freq
    )

    print(
      utils::head(
        x$freq,
        10L
      ),
      row.names = FALSE
    )

    if (aantal_rijen > 10L) {
      cat(
        "\n",
        aantal_rijen,
        " rijen totaal; gebruik $freq voor de volledige tabel.\n",
        sep = ""
      )
    }
  }

  invisible(x)
}
