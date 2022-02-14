# 🧜‍♂️ TRITON: Text Represented In Terms Of Numeric-features

**UNDER ACTIVE DEVELOPMENT. USE WITH CARE.**

A covariate builder that constructs text representation covariates for a cohort in the OMOP common data model. The constructed covariateData can be used within the OHDSI framework: [HADES](https://ohdsi.github.io/Hades/), such as the [patient-level prediction package](https://github.com/OHDSI/PatientLevelPrediction).

## Introduction
This covariate builder is an extension to the [FeatureExtraction package](https://github.com/OHDSI/PatientLevelPrediction). The package consists of two functions, the first function (*createTritonCovariateSettings*) is for creating the covariates settings (similar to *FeatureExtraction::createCovariateSettings*), specifying the text representations to be created and the NLP pipeline settings. The second function (*getTritonCovariateData*) is referenced by the covariate settings and builds the covariates.

## Features
- Constructs a *FeatureExtraction* covariateData object based on a population cohort.
- The covariateData object can be used in all other HADES packages that take covariateSettings or covariateData objects as input.
- TRITON provides a customizable natural language processing pipeline for creating bag-of-word , embeddings, and topic model representations.
- The full NLP pipeline is stored within the covariateSettings object making the process **transparent** and **reproducable**.
- The NLP pipeline is language independent.

Implemented text representations:
- Text summary statistics (`textstats`)
- Word counts/term frequencies (`BoW_bin`,`BoW_freq`)
- Term frequency–inverse document frequency (`BoW_tfidf`)
- Topic models (`TopicModel_lsa`,`TopicModel_lda`)
- Document embeddings as averaged or summed word embeddings (`WordEmb_avg`,`WordEmb_sum`)
- Import concepts from the note_nlp table

## Technology
TRITON is an R package. For the NLP pipeline it makes primarily use of the functions in [Quanteda](https://github.com/quanteda/quanteda/), an R package for managing and analyzing text (created by [Kenneth Benoit](https://kenbenoit.net/)).

## System requirements
Requires R (version 3.6.0 or higher). Installation on Windows requires RTools.

## Installation
The package is easily installed using devtools.
````
devtools::install_github("mi-erasmusmc/Triton")
````
Altenatively, this repository is cloned or downloaded to a local machine, then the package is manually build in R.

## Usage
The covariateSettings object for constructing the text representaton covariates over a cohort are created using the *createTritonCovariateSettings()* function.

**Observation window** - Text representations are created over an observation window, which is defined by the *startDay* and *endDay* relative to the cohort index date.

**Preprocessing** - Preprocessing of the note text is done passing by passing a function(*pipe_preprocess_function*) that takes a unprocessed string as input and outputs a processed string (example: *base::tolower*).

**Tokenization** - A tokenizer function has to be provided (*pipe_tokenizer_function*) that takes a string as input and outputs a list of tokens (example: *stringi::stri_split_boundaries* or *tokenizers::tokenize_words*). For simplicity the tokenizer functions of quanteda are implemented within TRITON and can be used by passing the string *"word"* (for other options see [quanteda docs](https://quanteda.io/reference/tokens.html)).

**Stopwords** - A list of terms can be provided (*filter_stopwords*) that will be removed from the tokenized text. Often stopwords are removed, which do not add much meaning to a sentence (example: stopwords::stopwords("en")). Note that choice of stopwords is language dependent.

**Pruning** - Pruning can be used to reduce the size of the vocabulary (the set of distinct terms in all the documents), by removing terms that occur very often or very little. Terms can be pruned based on their term frequency (occurence count) and document frequency (document count in which the term occurs). A maximimum and minimum can be provided for the term and document frequency (*filter_term_count_min*, *filter_term_count_max*, *filter_doc_count_min*, *filter_doc_count_max*). It is also possible to provide a maximum and minimum proportion for the doc frequency (example: *filter_doc_proportion_max = 0.5*, the term may occur in at most half of the documents).

**Text Represenation** - One or more text representations to be constructed and combined into the same covariateData object. Bag-of-Words representations are currently `BoW_bin` for the binary term occurance, `BoW_freq` for the term frequency, and `BoW_tfidf` for the term frequency–inverse document frequency, a seperate covariate is created for each term.
A topic model (`TopicModel_lsa`,`TopicModel_lda`) can be trained or an existing topic model can be loaded to use the topic posterior probabilities as text representations. 
An existing embedding can be used to create aggregated, averaged (`WordEmb_avg`) or summed (`WordEmb_sum`), word embedding representations.

#### Example
This example creates covariateSettings for constructing TFIDF uni- and bigram covariates over a observation window of 30 days before the cohort index date. The raw note text is preprocessed by coversion to lowercase and word tokens are created using the quanteda word tokenizer, removing english stopwords. The words are pruned based on the frequency and the percentage of documents with a word.
```r
triton_covariateSettings <- createTritonCovariateSettings(
  # Observation window #
  startDay = -30,
  endDay = 0,
  
  # NLP pipeline functions #
  pipe_preprocess_function = tolower,
  pipe_tokenizer_function = "word",
  pipe_ngrams = 1:2,
  
  # Prune the terms/vocabulary #
  filter_stopwords = stopwords::stopwords("en"),
  filter_term_count_min = 50,
  filter_doc_proportion_max = 0.5,
  filter_doc_proportion_min = 0.005,
  
  # Specify the representations #
  representations=c("BoW_tfidf"))
```

The R script file [extras/Run_TRITON.r](https://github.com/mi-erasmusmc/Triton/blob/master/extras/Run_TRITON.R) contains a more eleborate example.

## Good to know
- The default analysisId for the generated covariates is 999.
- Normaly the covariateId is the conceptId\*1000 + analysisId. The text represenations do not have conceptIds so the covariateId for each covariate is a randomnumber\*1000 + 999 (i.e. 1885860999).

