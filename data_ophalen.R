#' Data ophalen via een extern Python-script
#'
#' Roept een Python-script aan om data op te halen op basis van de
#' meegegeven parameters.
#'
#' De functie roept een lokaal geconfigureerd Python-script aan.
#' De bestandslocatie wordt niet in de GTST-code vastgelegd.
#' Stel vooraf de omgevingsvariabele `GTST_DATA_OPHALEN_SCRIPT` in op het
#' pad naar een eigen `DataOphalen.py`. Optioneel kan via `GTST_PYTHON_ENV`
#' een bestaande virtualenv worden gekozen.
#' De externe Python-code wordt niet meegeleverd met GTST.
#'
#' @param uitvoermap Character. Map waar de opgehaalde bestanden worden
#'   opgeslagen.
#' @param datacontract_code Character. Datacontractcode van de data die
#'   opgehaald moet worden.
#' @param enquete_codes Enquetecodes van de data die opgehaald moet worden.
#'   Indien leeg (`""` of `list()`), wordt automatisch gezocht naar
#'   enquetecodes die bij de datacontractcode horen.
#' @param versie Versie van de data die opgehaald moet worden.
#' @param afnemer Character. Afnemer van de data die opgehaald moet worden.
#' @param uitvoering_soort Character. Soort uitvoering, bijvoorbeeld `"PROD"`
#'   of `"LT"`.
#' @param kanaal Character. Optioneel kanaal waarop de responsedata wordt
#'   gefilterd. Standaard `""`, waarmee geen kanaalfilter wordt toegepast.
#'
#' @return Het resultaat van de Python-functie `DataOphalen`. De opgehaalde
#'   metadata- en databestanden worden in `uitvoermap` geplaatst.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Fictieve configuratie en voorbeeldwaarden:
#' Sys.setenv(GTST_DATA_OPHALEN_SCRIPT = "/voorbeeld/scripts/DataOphalen.py")
#' Sys.setenv(GTST_PYTHON_ENV = "/voorbeeld/virtualenvs/gtst")
#' data_ophalen(
#'   uitvoermap = "/voorbeeld/uitvoer",
#'   datacontract_code = "CONTRACT_DEMO_01",
#'   enquete_codes = list("ENQUETE_DEMO_01", "ENQUETE_DEMO_02"),
#'   versie = "1",
#'   afnemer = "DEMO_AFNEMER",
#'   uitvoering_soort = "TEST"
#' )
#' }
# TODO: standaardiseren naam parameters
data_ophalen <- function(
    uitvoermap,
    datacontract_code,
    enquete_codes,
    versie,
    afnemer,
    uitvoering_soort,
    kanaal = "") {
  data_ophalen_mainscript <- Sys.getenv(
    "GTST_DATA_OPHALEN_SCRIPT",
    unset = ""
  )

  if (!nzchar(data_ophalen_mainscript)) {
    stop(
      "Stel GTST_DATA_OPHALEN_SCRIPT in op het pad naar een eigen Python-script.",
      call. = FALSE
    )
  }
  if (!file.exists(data_ophalen_mainscript)) {
    stop(
      "Het ingestelde Python-script bestaat niet: ", data_ophalen_mainscript,
      call. = FALSE
    )
  }

  python_env_pad <- Sys.getenv("GTST_PYTHON_ENV", unset = "")
  if (nzchar(python_env_pad)) {
    if (!dir.exists(python_env_pad)) {
      stop("De ingestelde Python-omgeving bestaat niet: ", python_env_pad,
           call. = FALSE)
    }
    reticulate::use_virtualenv(python_env_pad, required = TRUE)
  }

  python_env <- new.env(parent = emptyenv())

  reticulate::source_python(
    data_ophalen_mainscript,
    envir = python_env
  )

  data_ophalen_functie <- get(
    "DataOphalen",
    envir = python_env
  )

  data_ophalen_functie(
    UitvoerMap = uitvoermap,
    DataContractCode = datacontract_code,
    EnqueteCodes = enquete_codes,
    Versie = versie,
    Afnemer = afnemer,
    UitvoeringSoort = uitvoering_soort,
    Kanaal = kanaal
  )
}
