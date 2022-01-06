hasData <- function(data) {
  ##==## checks if data has data ##==##
  return(!is.null(data) && (data %>% count() %>% pull()) > 0)
}

getUniqueId<-function(Names, idstaken, idrange=NULL, analysisId=999){
  ##==## creates a unique(random) id for a covariate ##==##
  if(is.null(idrange)){idrange<-c(1,2147482)}
  ParallelLogger::logInfo(paste0("\t\tCreating unique covariate ids between ",idrange[1], " and ",idrange[2]))
  Names<-as.data.frame(Names)
  colnames(Names)<-"name"
  uniqueNames<-as.data.frame(unique(Names))
  colnames(uniqueNames)<-"name"
  ## generate the ids
  uniqueNames$ids<-as.integer(paste0((as.integer(runif(n=nrow(uniqueNames),min = idrange[1],max = idrange[2]))),analysisId))
  ## change the ids if duplicated or already taken
  while ((length(uniqueNames$ids[duplicated(uniqueNames$ids)])>0) || (!all(is.na(match(uniqueNames$ids,idstaken))))) {
    #print(paste0("duplicated:",length(uniqueNames$ids[duplicated(uniqueNames$ids)]))) #Debug
    #print(paste0("taken:",length(uniqueNames$ids[!is.na(match(uniqueNames$ids,idstaken))]))) #Debug
    dubs<-duplicated(uniqueNames$ids) # duplicated ids
    taken<-!is.na(match(uniqueNames$ids,idstaken)) # ids that are already taken
    both<-dubs | taken # combine both
    uniqueNames$ids[both]<-as.integer(paste0((as.integer(runif(n=length(both[both]),min = idrange[1],max = idrange[2]))),analysisId)) # create new id for those
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

selectEmbTerms<-function(toks,wordsInEmb){
  ##==## Select the terms in the tokens that are also in the embedding ##==##
  toks<-toks[toks %in% wordsInEmb]
  return(toks)
}

aggrNoteEmbedding<-function(toks,wordEmb){
  ##==## Aggregate over word embeddings to create a note embedding ##==##
  noteEmb<-wordEmb[toks,]
  noteEmb<-noteEmb[complete.cases(noteEmb),]
  noteEmb<-colSums(noteEmb)/length(toks)
  return(noteEmb)
}
