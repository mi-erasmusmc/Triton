buildNoteSQLquery<-function(connection,
                            cdmDatabaseSchema,
                            cohortTable,
                            cohortId,
                            rowIdField,
                            obsWindowDays,
                            obsWindowStartDay){
  ##==## Creates sql query for extracting cohort notes from cdm database ##==##
  sql <- paste("SELECT c.@row_id_field, notes.note_id, notes.note_date, notes.note_text",
               "FROM @cohort_table AS c",
               "LEFT JOIN @cdm_database_schema.note as notes ",
               "ON (c.subject_id=notes.person_id ",
               "AND (c.cohort_start_date + @window_start*interval '1 day') > notes.note_date ",
               "AND ((c.cohort_start_date + @window_start*interval '1 day') + @days_window*interval '1 day') < notes.note_date)",
               "WHERE 1=1 {@cohort_id != -1} ? {AND c.cohort_definition_id = @cohort_id}")
  sql <- SqlRender::render(sql,
                           cohort_table = cohortTable,
                           cohort_id = cohortId,
                           row_id_field = rowIdField,
                           cdm_database_schema = cdmDatabaseSchema,
                           days_window = obsWindowDays,
                           window_start = obsWindowStartDay)
  sqlquery <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
}

hasData <- function(data) {
  ##==## checks if data has data ##==##
  return(!is.null(data) && (data %>% count() %>% pull()) > 0)
}

getUniqueId<-function(Names, idstaken, idrange=NULL){
  ##==## creates a unique(random) id for a covariate ##==##
  ParallelLogger::logInfo("\t\tCreating unique covariate ids")
  Names<-as.data.frame(Names)
  colnames(Names)<-"name"
  uniqueNames<-as.data.frame(unique(Names))
  colnames(uniqueNames)<-"name"
  ## generate the ids
  if(is.null(idrange)){idrange<-c(1,2147482)}
  uniqueNames$ids<-as.integer(paste0((as.integer(runif(n=nrow(uniqueNames),min = idrange[1],max = idrange[2]))),999))
  ## change the ids if duplicated or taken
  while ((length(uniqueNames$ids[duplicated(uniqueNames$ids)])>0) || (!all(is.na(match(uniqueNames$ids,idstaken))))) {
    #print(paste0("duplicated:",length(uniqueNames$ids[duplicated(uniqueNames$ids)]))) #Debug
    #print(paste0("taken:",length(uniqueNames$ids[!is.na(match(uniqueNames$ids,idstaken))]))) #Debug
    dubs<-duplicated(uniqueNames$ids) # duplicated ids
    taken<-!is.na(match(uniqueNames$ids,idstaken)) # ids that are already taken
    both<-dubs | taken # combine both
    uniqueNames$ids[both]<-as.integer(paste0((as.integer(runif(n=length(both[both]),min = 0,max = 2147482))),999)) # create new id for those
  }
  Names<-dplyr::left_join(Names,uniqueNames,by = "name")
  return(Names$ids)
}

appendCovariateData<- function(tempCovariateData,covariateData){
  ##==## appends covariate objects ##==##
  if (is.null(covariateData)) {
    covariateData <- tempCovariateData
  } else {
    if (hasData(covariateData$covariates)) {
      if (hasData(tempCovariateData$covariates)) {
        Andromeda::appendToTable(covariateData$covariates, tempCovariateData$covariates)
      }
    } else if (hasData(tempCovariateData$covariates)) {
      covariateData$covariates <- tempCovariateData$covariates
    }
    if (hasData(covariateData$covariatesContinuous)) {
      if (hasData(tempCovariateData$covariatesContinuous)) {
        Andromeda::appendToTable(covariateData$covariatesContinuous, tempCovariateData$covariatesContinuous)
      } else if (hasData(tempCovariateData$covariatesContinuous)) {
        covariateData$covariatesContinuous <- tempCovariateData$covariatesContinuous
      }
    }
    Andromeda::appendToTable(covariateData$covariateRef, tempCovariateData$covariateRef)
    Andromeda::appendToTable(covariateData$analysisRef, tempCovariateData$analysisRef)
    for (name in names(attr(tempCovariateData, "metaData"))) {
      if (is.null(attr(covariateData, "metaData")[name])) {
        attr(covariateData, "metaData")[[name]] <- attr(tempCovariateData, "metaData")[[name]]
      } else {
        attr(covariateData, "metaData")[[name]] <- list(attr(covariateData, "metaData")[[name]],
                                                        attr(tempCovariateData, "metaData")[[name]])
      }
    }
  }
  return(covariateData)
}

GetDictionaryVocab <- function(connection,
                               cdmDatabaseSchema,
                               dictionaryVocabIds,
                               ngrams){
  ##==## Creates dictionary from specific omop cdm vocabulary ##==##
}
