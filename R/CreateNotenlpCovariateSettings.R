#' createNotenlpCovariateSettings
#'
#' Create a covariateSettings object for constructing covariates from the note_nlp table in the OMOP CDM.
#'
#' @export

createNotenlpCovariateSettings <- function(
  # GENERAL SETTINGS
  useNotenlpData = TRUE,
  startDay=-30,
  endDay=0,
  analysisId=998,
  # NOTE & NOTE_NLP TABLE SETTINGS
  note_databaseschema="cdm",
  note_tablename="note",
  notenlp_databaseschema="cdm",
  notenlp_tablename="note_nlp",
  # FILTER SETTINGS
  filter_select_domains="",
  filter_include_nonexist=FALSE,
  filter_include_temporal=FALSE,
  filter_only_standard=FALSE,
  # SAVING AND LOADING
  covariateDataSave="",
  covariateDataLoad="")
{
  covariateSettings <- list(useNotenlpData = useNotenlpData,
                            startDay=startDay,
                            endDay=endDay,
                            analysisId=analysisId,
                            note_databaseschema=note_databaseschema,
                            note_tablename=note_tablename,
                            notenlp_databaseschema=notenlp_databaseschema,
                            notenlp_tablename=notenlp_tablename,
                            filter_select_domains=filter_select_domains,
                            filter_include_nonexist=filter_include_nonexist,
                            filter_include_temporal=filter_include_temporal,
                            filter_only_standard=filter_only_standard,
                            covariateDataSave=covariateDataSave,
                            covariateDataLoad=covariateDataLoad)
  attr(covariateSettings, "fun") <- "getNotenlpCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
