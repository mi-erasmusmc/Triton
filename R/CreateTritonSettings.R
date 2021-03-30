#' createTextRepCovariateSettings
#'
#' Create a covariateSettings object for constructing text representation covariates from the notes table in the OMOP CDM.
#'
#' @importFrom magrittr "%>%"
#' @param useTextData logical; option to disable the creation of text representation covariates.
#' @param startDay integer; start day before the index date for with the text representations have to be computed.
#' @param endDay integer; end day before the index date for with the text representations have to be computed.
#' @param preprocessor_function function; to preprocess the stings before tokenization. Default is \code{\link{tolower}}.
#' @param tokenizer_function character or function; to tokenize the strings. Default is quanteda tokenizer (\code{\link[quanteda]{tokens}}), with argument "word". Other possible arguments are "fasterword", "fastestword", "sentence", and "character". It is also possible to provide a custom tokenizer function. This function should take the document strings as input and should return a list of character vectors (tokens).
#' @param custom_pruning_regex (optional) character; regular expression (regex) that selects tokens that will be removed. Default is \code{NULL}.
#' @param stopwords character vector; of list of stopwords that will be removed. Default is NULL. See \code{\link[stopwords]{stopwords}} for generating stopwords.
#' @param ngrams integer vector; specifying the number of elements to be concatenated in each ngram. For example: \code{c(1,2)} creates all unigrams and bigrams; \code{c(1:3)} creats all unigrams, bigrams, and trigrams. Default is 1: no ngrams (unigram).
#' @param vocab_term_max integer; maximum number of terms in vocabulary, takes top most frequent terms. Default is \code{Inf}.
#' @param term_count_min integer; minimum number of occurences over all documents.
#' @param term_count_max integer; maximum number of occurences over all documents.Default is \code{Inf}.
#' @param doc_count_min integer; term will be kept when number of documents that contain this term is larger than this value.
#' @param doc_count_max integer; term will be kept when number of documents that contain this term is lower than this value. Default is \code{Inf}.
#' @param doc_proportion_max numeric; maximum proportion (0.-1.) of documents which should contain term.
#' @param doc_proportion_min numeric; minimum proportion (0.-1.) of documents which should contain term.
#' @param representations character vector; of text representations that should be constructed, chose from \code{"tf"} and \code{"tfidf"}. Multiple representations can be constructed at once: \code{c("tf","tfidf")}.
#' @param dictionaryVocabIds (optional) integer vector; of omop cdm vocabulary ids that are used for dictionary matching. Set to \code{NULL}(default) to turn off dictionary matching.
#' @param outcomeFolder (optional) character; file path and name for saving output files. Default is \code{NULL}.
#' @param saveVocab logica; option to save the generated vocabulary as rds file in the outputFolder.
#' @return covariateSettings object, that can be used by the OHDSI FeatureExtraction package.
#' @export

createTextRepCovariateSettings <- function(useTextData = TRUE,
                                        startDay=-30,
                                        endDay=0,
                                        preprocessor_function=tolower,
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
                                        dictionaryVocabIds=NULL,
                                        representations=c("tfidf"),
                                        idrange=NULL,
                                        outputFolder=NULL,
                                        saveVocab=FALSE) {

  if(saveVocab & is.null(outputFolder)) stop("specify the outputFolder to save the vocab")

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
                            dictionaryVocabIds=dictionaryVocabIds,
                            custom_pruning_regex=custom_pruning_regex,
                            representations=representations,
                            idrange=idrange,
                            outputFolder=outputFolder,
                            saveVocab=saveVocab)
  attr(covariateSettings, "fun") <- "getTextRepCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
