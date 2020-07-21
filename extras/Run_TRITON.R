library(DatabaseConnector)
library(Andromeda)
library(FeatureExtraction)
library(Triton)
# enough java ram for sql query import, increase it for larger queries
options(java.parameters = "-Xmx8000m")

# Optional: Set output folder for saving vocabulary
outputFolder <-paste0(getwd(),"/output")
dir.create(outputFolder, showWarnings = FALSE) # Create folder if does not exist

#=========================================================
# Creating custom preprocessor and tokenizer functions
#=========================================================

#=== Custom preprocessing function example ===#
custom_preprocessing<-function(string){
  res<-tolower(string) %>% # Text to lower case
    stringr::str_replace_all("[0-9]+", " ") %>% # remove numbers
    stringr::str_replace_all("_+", " ") # remove all under scores
  return(res)
}

#=== Custom tokenizer function example ===#
custom_tokenizer <- function(strings) {
  res <- stringi::stri_split_boundaries(as.matrix(strings), type = "word",skip_word_none = TRUE) # Split the sentences into words
  return(res)
}

#== Optional: Test your preprocessing and tokenization functions ==#
text<-c("Patient: 1: has fever and\n headache?<>)(", "Patient 2: has___diabetes")
custom_tokenizer(unlist(lapply(text, custom_preprocessing)))

#===============================================
#  Create the text representation settings
#===============================================
triton_covariateSettings <- Triton::createTextRepCovariateSettings(
  useTextData = TRUE,
  startDay = -365,
  endDay = 0,
  preprocessor_function = custom_preprocessing,
  tokenizer_function = custom_tokenizer,
  stopwords = stopwords::stopwords("en"),
  custom_pruning_regex = "(-+)|(hydro[a-z]+)", # remove all minus signs and all words starting with "hydro" (example)
  ngrams = 1:2,
  vocab_term_max = Inf,
  term_count_min = 50,
  term_count_max = Inf,
  doc_count_min = 50,
  doc_count_max = Inf,
  doc_proportion_max = 0.5,
  doc_proportion_min = 0.005,
  dictionaryVocabIds=NULL, # Provide cdm vocabulary ids to perform Dictionary search, otherwise NULL.
  representations=c("tf","tfidf"), # list of representations to be generated
  vocabFile = paste0(outputFolder,"/","vocab")) # Provide location and name to save vocab, otherwise NULL.

# The triton_covariateSettings can now be used within the OHDSI framework.
# For example in the PatientLevelPrediction package.

# To construct the covariates and create a covariateData object use the
# getDbCovariateData function in the FeatureExtraction package.

#===========================================================================
# Construct the covariates using FeatureExtraction package from a CDM cohort
#===========================================================================

# Creat database connection details
connectionDetails <- createConnectionDetails(
  dbms = "",
  server = "",
  user ="",
  password = "",
  port = 0)

# The database schema containing the OMOP CDM data
cdmDatabaseSchema <- ""
# The database schema with the cohort tables
cohortDatabaseSchema <- ""
# Table name of the cohort
cohortTable <- ""
# The cohort id
cohortId <- 1
# The Cohort row_id field
rowIdField<-"subject_id"

# Construct the covariateData
covariateData <- FeatureExtraction::getDbCovariateData(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  cohortId = cohortId,
  rowIdField = rowIdField,
  covariateSettings = TextRep_CovSet)

# Inspect the covariateData object:
inspectData<-as.data.frame(covariateData$covariates)
inspectRef<-as.data.frame(covariateData$covariateRef)
inspectAn<-as.data.frame(covariateData$analysisRef)
inspectMeta<-attr(covariateData,"metaData")

# Retrieve vocab if saved
vocab<-readRDS(paste0(outputFolder,"/","vocab"))

