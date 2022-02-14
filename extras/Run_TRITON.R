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
  res<-tolower(string)# Text to lower case
  res<-stringr::str_replace_all(res,"\\\\n", " ") # remove new lines
  res<-stringr::str_replace_all(res,"[0-9]+", " ") # remove numbers
  res<-stringr::str_replace_all(res,"_+", " ") # remove all under scores
  return(res)
}

#=== Custom tokenizer function example ===#
custom_tokenizer <- function(strings) {
  res <- stringi::stri_split_boundaries(as.matrix(strings), type = "word",skip_word_none = TRUE) # Split the sentences into words
  names(res)<-names(strings)
  return(res)
}

#== Optional: Test your preprocessing and tokenization functions ==#
text<-c("Patient: 112: has fever and\n headache?<>)(", "Patient 2: has___diabetes")
names(text)<-c("doc1","doc2")
custom_tokenizer(unlist(lapply(text, custom_preprocessing)))
#Output should be a (named) list of character vectors:
# $doc1
# [1] "patient"  "has"      "fever"    "and"      "headache"
#
# $doc2
# [1] "patient"  "has"      "diabetes"


#===============================================
#  Create the text representation settings
#===============================================
triton_covariateSettings <- Triton::createTritonCovariateSettings(
  # GENERAL SETTINGS
  useNoteData = TRUE,
  startDay = -30,
  endDay = -1,
  idrange = c(1,1000000),
  parallel = FALSE,
  analysisId = 999,
  # NOTE TABLE SETTINGS
  note_databaseschema = NULL, #default keep same as cdm
  note_tablename = "note", #default keep same as in cdm
  note_customWhere = "",
  # PIPELINE SETTINGS
  pipe_preprocess_function = custom_preprocessing,
  pipe_tokenizer_function = custom_tokenizer,
  pipe_ngrams = 1,
  pipe_saveVocab = FALSE,
  pipe_outputFolder = outputFolder,
  # OPTIONAL: TERM FILTER SETTINGS
  filter_stopwords = NULL,
  filter_custom_regex = NULL,
  filter_vocab_term_max = NULL,
  filter_term_count_min = NULL,
  filter_term_count_max = NULL,
  filter_doc_count_min = NULL,
  filter_doc_count_max = NULL,
  filter_doc_proportion_max = NULL,
  filter_doc_proportion_min = NULL,
  # REPRESENTATION SETTINGS
  representations = c("BoW_bin","TextStats"),  # choose from "BoW_bin","BoW_freq","BoW_tfidf","WordEmb_avg","WordEmb_sum","TopicModel_lsa","TextStats"
  BoW_validationVarImpTable = NULL, #Required for validation
  Word_embeddings = NULL, #Required for WordEmb
  TopicModel_file = NULL, #Required for TopicModel
  # OPTIONAL: SAVING AND LOADING
  covariateDataSave="",
  covariateDataLoad="")

# The triton_covariateSettings can now be used within the OHDSI framework.
# For example in the PatientLevelPrediction package.

#===========================================================================
# Construct the covariates using FeatureExtraction package from a CDM cohort
#===========================================================================

# To construct the covariates and create a covariateData object use the
# getDbCovariateData function in the FeatureExtraction package.

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
  covariateSettings = triton_covariateSettings) # The created covariate settings object

# Inspect the covariateData object:
inspectData<-as.data.frame(covariateData$covariates)
inspectRef<-as.data.frame(covariateData$covariateRef)
inspectAn<-as.data.frame(covariateData$analysisRef)
inspectMeta<-attr(covariateData,"metaData")

# Retrieve vocab if saved
vocab<-readRDS(paste0(outputFolder,"/","vocab"))

