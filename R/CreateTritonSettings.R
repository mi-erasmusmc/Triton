#' createTritonCovariateSettings
#'
#' Create a covariateSettings object for constructing text representation (Triton) covariates from the notes table in the OMOP CDM.
#'
#' @importFrom magrittr "%>%"
#' @param useTextData logical; option to disable the creation of text representation covariates.
#' @param startDay integer; start day before the index date for with the text representations have to be computed.
#' @param endDay integer; end day before the index date for with the text representations have to be computed.
#' @param customWhere (optional) character; with a SQL where statement to filter the note import. Example "WHERE note_source_value='communication'".
#' @param preprocessor_function function; to preprocess the stings before tokenization. Default is \code{\link{tolower}}.
#' @param tokenizer_function character or function; to tokenize the strings. Default is quanteda tokenizer (\code{\link[quanteda]{tokens}}), with argument "word". Other possible arguments are "fasterword", "fastestword", "sentence", and "character". It is also possible to provide a custom tokenizer function. This function should take the document strings as input and should return a list of character vectors (tokens).
#' @param stopwords character vector; of list of stopwords that will be removed. Default is NULL. See \code{\link[stopwords]{stopwords}} for generating stopwords.
#' @param custom_pruning_regex (optional) character; regular expression (regex) that selects tokens that will be removed. Default is \code{NULL}.
#' @param ngrams integer vector; specifying the number of elements to be concatenated in each ngram. For example: \code{c(1,2)} creates all unigrams and bigrams; \code{c(1:3)} creats all unigrams, bigrams, and trigrams. Default is 1: no ngrams (unigram).
#' @param vocab_term_max integer; maximum number of terms in vocabulary, takes top most frequent terms. Default is \code{Inf}.
#' @param term_count_min integer; minimum number of occurences over all documents.
#' @param term_count_max integer; maximum number of occurences over all documents.Default is \code{Inf}.
#' @param doc_count_min integer; term will be kept when number of documents that contain this term is larger than this value.
#' @param doc_count_max integer; term will be kept when number of documents that contain this term is lower than this value. Default is \code{Inf}.
#' @param doc_proportion_max numeric; maximum proportion (0.-1.) of documents which should contain term.
#' @param doc_proportion_min numeric; minimum proportion (0.-1.) of documents which should contain term.
#' @param representations character vector; of text representations that should be constructed, chose from \code{"tf"} and \code{"tfidf"}. Multiple representations can be constructed at once: \code{c("tf","tfidf")}.
#' @param t2v_word_embeddings (optional) character; of a data.frame loaded in the R environment that contains the word embeddings. First column must contain the word, the other n-1 columns contain the embedding values.
#' @param lda_model (optional) character; name of a data.frame loaded in the R environment that contains a lda model object.
#' @param lsa_model (optional) character; name of a data.frame loaded in the R environment that contains a lsa model object.
#' @param stm_model (optional) character; name of a data.frame loaded in the R environment that contains a stm model object.
#' @param idrange (optional) integer vector; specifying the range of integers that can be used to generate the covariateids, max is 2147482. Default is c(1,2147482).
#' @param outputFolder (optional) character; file path and name for saving output files. Default is \code{NULL}.
#' @param parallel logical; to indicate whether multi-threading should be used. Default is True.
#' @param saveVocab logical; option to save the generated vocabulary as rds file in the outputFolder.
#' @param covariateDataSave (optional) character; location and file name of where the created covariateData must be stored.
#' @param covariateDataLoad (optional) character; location and file name of where the created covariateData must be loaded from. Anything else is ignored, just the covariateData is loaded and returned.
#' @param validationVarImpTable (optional) data.frame; used for validation. A varImp data.frame with the covariate names and covariate values of a trained model. The varImp data.frame can be found in plpResult$model$varImp or plpModel$varImp.
#' @return covariateSettings object, that can be used by the OHDSI FeatureExtraction package.
#' @export

createTritonCovariateSettings <- function(useTextData = TRUE,
                                        startDay=-30,
                                        endDay=0,
                                        preprocessor_function=NULL,
                                        tokenizer_function="word",
                                        stopwords=NULL,
                                        custom_pruning_regex=NULL,
                                        ngrams=1,
                                        vocab_term_max=Inf,
                                        term_count_min=50,
                                        term_count_max=Inf,
                                        doc_count_min=50,
                                        doc_count_max=Inf,
                                        doc_proportion_max=0.5,
                                        doc_proportion_min=0.001,
                                        representations=c("tb"),
                                        t2v_word_embeddings=NULL,
                                        lda_model=NULL,
                                        lsa_model=NULL,
                                        stm_model=NULL,
                                        idrange=NULL,
                                        outputFolder=NULL,
                                        parallel=TRUE,
                                        saveVocab=FALSE,
                                        covariateDataSave="",
                                        covariateDataLoad="",
                                        customWhere="",
                                        validationVarImpTable=NULL) {

  if(saveVocab & is.null(outputFolder)) stop("Specify the outputFolder to save the vocab")
  if("text2vec" %in% representations & is.null(t2v_word_embeddings)) stop("Provide the t2v_word_embeddings to use text2vec")
  if("lda" %in% representations & is.null(lda_model)) stop("Provide a trained lda_model to use lda")
  if("lsa" %in% representations & is.null(lsa_model)) stop("Provide a trained lsa_model to use lsa")
  if("stm" %in% representations & is.null(stm_model)) stop("Provide a trained stm_model to use stm")

  covariateSettings <- list(useTextData = useTextData,
                            startDay=startDay,
                            endDay=endDay,
                            preprocessor_function=preprocessor_function,
                            tokenizer_function=tokenizer_function,
                            stopwords=stopwords,
                            ngrams=ngrams,
                            vocab_term_max=vocab_term_max,
                            term_count_min=term_count_min,
                            term_count_max=term_count_max,
                            doc_count_min=doc_count_min,
                            doc_count_max=doc_count_max,
                            doc_proportion_max=doc_proportion_max,
                            doc_proportion_min=doc_proportion_min,
                            custom_pruning_regex=custom_pruning_regex,
                            representations=representations,
                            t2v_word_embeddings=t2v_word_embeddings,
                            lda_model=lda_model,
                            lsa_model=lsa_model,
                            stm_model=stm_model,
                            idrange=idrange,
                            outputFolder=outputFolder,
                            parallel=parallel,
                            saveVocab=saveVocab,
                            covariateDataSave=covariateDataSave,
                            covariateDataLoad=covariateDataLoad,
                            customWhere=customWhere,
                            validationVarImpTable=validationVarImpTable)
  attr(covariateSettings, "fun") <- "getTritonCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
