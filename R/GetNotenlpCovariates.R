#' getNotenlpCovariateData
#'
#' This covariate builder creates text representation (Triton) covariates from a cohort in the OMOP cdm
#'
#' @param connection A connection to the server containing the schema as created using the connect function in the DatabaseConnector package.
#' @param oracleTempSchema A schema where temp tables can be created in Oracle.
#' @param cdmDatabaseSchema The name of the database schema that contains the OMOP CDM instance. Requires read permissions to this database. On SQL Server, this should specifiy both the database and the schema, so for example 'cdm_instance.dbo'.
#' @param cohortTable Name of the table holding the cohort for which we want to construct covariates. If it is a temp table, the name should have a hash prefix, e.g. '#temp_table'. If it is a non-temp table, it should include the database schema, e.g. 'cdm_database.cohort'.
#' @param cohortId For which cohort ID should covariates be constructed? If set to -1, covariates will be constructed for all cohorts in the specified cohort table.
#' @param cdmVersion The version of the Common Data Model used. Currently only cdmVersion = "5" is supported.
#' @param rowIdField The name of the field in the cohort temp table that is to be used as the row_id field in the output table. This can be especially usefull if there is more than one period per person.
#' @param covariateSettings An object of type covariateSettings as created using the createTextRepCovariateSettings function.
#' @param aggregated Should aggregate statistics be computed instead of covariates per cohort entry?
#' @return a covariateData object that can be used my other cdm framework functions.
#' @export

getNotenlpCovariateData <- function(connection,
                                    oracleTempSchema = NULL,
                                    cdmDatabaseSchema,
                                    cohortTable,
                                    cohortId,
                                    cdmVersion = "5",
                                    rowIdField,
                                    covariateSettings,
                                    aggregated = FALSE) {

  if(aggregated){
    stop("Currently no aggregation of note_nlp covariates implemented.")
  }

  cs<-covariateSettings

  if(cs$covariateDataLoad!=""){
    t1 <- Sys.time()
    ParallelLogger::logInfo(paste("\tLoading covariateData from",cs$covariateDataLoad))
    covariateData<-FeatureExtraction::loadCovariateData(file = cs$covariateDataLoad)
    ParallelLogger::logInfo(paste0("Done, total time: ",round(difftime(Sys.time(),t1, units = 'min'),2)," min"))
    return(covariateData) # return covariates and skip creation
  }

  ParallelLogger::logInfo("Constructing features on server")
  # Get the sql query
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "createNotenlpCovariates.sql",
                                           packageName = "Triton",
                                           dbms = attr(connection, "dbms"),
                                           cohort_table = cohortTable,
                                           cohort_id = cohortId,
                                           row_id_field = rowIdField,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           note_database=cs$note_databaseschema,
                                           note_table=cs$note_tablename,
                                           notenlp_database=cs$notenlp_databaseschema,
                                           notenlp_table=cs$notenlp_tablename,
                                           end_day=cs$endDay,
                                           start_day=cs$startDay,
                                           domains=paste0("'",paste(cs$filter_select_domains, collapse = "', '"),"'"),
                                           include_nonexist=cs$filter_include_nonexist,
                                           include_temporal=cs$filter_include_temporal,
                                           standard=cs$filter_only_standard,
                                           analysis_id=cs$analysisId,
                                           analysis_name="Note_nlp covariates")
  assign("sql",sql,envir = .GlobalEnv)
  # create the covariates:
  DatabaseConnector::executeSql(connection, sql)


  ParallelLogger::logInfo("Fetching data from server")
  start <- Sys.time()
  covariateData <- Andromeda::andromeda()
  sqlCovariates <- "SELECT * FROM #covariates"
  sqlCovariates <- SqlRender::translate(sqlCovariates,targetDialect = attr(connection, "dbms"),oracleTempSchema = oracleTempSchema)
  DatabaseConnector::querySqlToAndromeda(connection = connection,
                                         sql = sqlCovariates,
                                         andromeda = covariateData,
                                         andromedaTableName = "covariates",
                                         snakeCaseToCamelCase = TRUE)
  sqlCovRef <- "SELECT * FROM #cov_ref"
  sqlCovRef <- SqlRender::translate(sqlCovRef,targetDialect = attr(connection, "dbms"),oracleTempSchema = oracleTempSchema)
  DatabaseConnector::querySqlToAndromeda(connection = connection,
                                         sql = sqlCovRef,
                                         andromeda = covariateData,
                                         andromedaTableName = "covariateRef",
                                         snakeCaseToCamelCase = TRUE)
  sqlAnRef <- "SELECT * FROM #analysis_ref"
  sqlAnRef <- SqlRender::translate(sqlAnRef,targetDialect = attr(connection, "dbms"),oracleTempSchema = oracleTempSchema)
  DatabaseConnector::querySqlToAndromeda(connection = connection,
                                         sql = sqlAnRef,
                                         andromeda = covariateData,
                                         andromedaTableName = "analysisRef",
                                         snakeCaseToCamelCase = TRUE)
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Fetching data took ", signif(delta, 3), " ", attr(delta, "units"))

  class(covariateData) <- "CovariateData"

  if(cs$covariateDataSave!=""){
    ParallelLogger::logInfo(paste("\tSaving covariateData to",cs$covariateDataSave))
    FeatureExtraction::saveCovariateData(covariateData,file = cs$covariateDataSave)
    covariateData<-FeatureExtraction::loadCovariateData(file = cs$covariateDataSave)
  }

  return(covariateData)
}
