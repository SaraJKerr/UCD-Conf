Enhanced not Distant: Examining Independence in the Novels of Austen, Edgeworth, and Owenson
================
Sara J Kerr
February 2018

### Introduction

This document provides the code and results for the analysis conducted for the paper 'Enhanced not Distant: Examining Independence in the Novels of Austen, Edgeworth, and Owenson' presented at the 'Digital Cultures, Big Data and Society' conference on 15th and 16th February 2018.

This paper proposes a shift in focus from close and distant reading, and suggests instead considering the combined use of these methods as enhanced reading, which has the purpose of further improving the quality and extent of close readings of the texts through the use of digital methods. As a core part of this redefinition, is the importance of greater openness with regard to the texts, tools, and the parameters used. This provides the opportunity of making textual analysis a little more replicable, capitalising on the strengths of computational methods, allowing the researcher to repeat a particular analysis, and view the source of an interpretation.

### Loading Packages

The first step, having set the working directory, is to load the packages needed for the analysis. If packages are not already available use `install.packages` or install using the *Tools* menu in **R Studio**.

### Creating A Lexicon

As a starting point for analysis I wanted to create a lexicon of terms which were associated with independence, and where possible to consider the meanings and connotations of independence during this period. 187 texts from the first two periods, 1710–1780 and 1780–1850, of CLMET3.1, totalling just over 22 million words, were selected and a word embedding model, using the `wordVectors` package, was created with 300 dimensions and a window of 12 words. CLMET3.1 is available free on request from [https://perswww.kuleuven.be/⇠u0044428/clmet.htm](https://perswww.kuleuven.be/⇠u0044428/clmet.htm).

``` r
prep_word2vec("plain", "Clmet_processed_period_1_2.txt", lowercase = T)

clmet <- train_word2vec("Clmet_processed_period_1_2.txt",
                        output = "Clmet_model_period_1_2.bin", threads = 1,
                        vectors = 300, window = 12)
```

The model created is available in this GitHub repository.
The created model is read into R and vector addition was used to identify the terms of interest. For this study the terms closest to `"independence" + "education"`, `"independence" + "conduct" + "propriety"`, and `"independence" + "rank"`.

``` r
clmet <- read.vectors("Clmet_model_period_1_2.bin")
```

    ## Filename ends with .bin, so reading in binary format

    ## Reading a word2vec binary file of 49302 rows and 300 columns

``` r
education <- closest_to(clmet, ~  "independence" + "education", n = 500)
education <- education$word

conduct <- closest_to(clmet, ~ "independence" + "conduct" + "propriety", n =  500)
conduct <- conduct$word

rank <- closest_to(clmet, ~ "independence" + "rank", n = 500)
rank <- rank$word
```

The terms can be combined and the top words for each term can be viewed:

``` r
lexicon_erc <- as.data.frame(cbind(education, rank, conduct))
colnames(lexicon_erc) <- c("Education", "Rank", "Conduct")
head(lexicon_erc)
```

    ##      Education         Rank      Conduct
    ## 1    education independence    propriety
    ## 2 independence         rank      conduct
    ## 3      freedom      dignity independence
    ## 4       habits      station     prudence
    ## 5      society   reputation      freedom
    ## 6     learning      freedom    character

The Austen, Edgeworth, Owenson corpus was converted into a Document Term Matrix using the `tm` package.

``` r
input.dir <- "AEO_corpus"
# Read the name of all .txt files
files <- dir(input.dir, "\\.txt")

# Create Volatile Corpus
docs <- VCorpus(DirSource(input.dir))

# Preprocess corpus
docs <- tm_map(docs, removePunctuation)   # Remove punctuation
docs <- tm_map(docs, removeNumbers)      # Remove numbers
docs <- tm_map(docs, tolower)   # Convert to lowercase
docs <- tm_map(docs, stripWhitespace)   # Strip whitespace
docs <- tm_map(docs, PlainTextDocument)

# Create formats needed for analysis
# Create a Document Term Matrix
dtm <- DocumentTermMatrix(docs)

rownames(dtm) <- files # Allocates text names to rows

# Convert DTM to matrix
m <- as.matrix(dtm)
```

The 500 terms in the lexicon\_erc were then filtered to remove any which were not present in the AEO corpus and the top 350 terms saved as the `lexicon_in_aeo`. The top 100 words which did not appear in the AEO corpus were saved as `lexicon_ex`.

``` r
# Identify independence terms which appear in the AEO texts
words <- colnames(m)
rank_terms <- rank[which(rank %in% words == T)]
education_terms <- education[which(education %in% words == T)]
conduct_terms <- conduct[which(conduct %in% words == T)]

# words not in AEO Corpus
ex_rank_terms <- rank[which(rank %in% words == F)]
ex_education_terms <- education[which(education %in% words == F)]
ex_conduct_terms <- conduct[which(conduct %in% words == F)]

# Create lexicon with top 350 filtered terms for each category
lexicon_in_aeo <- as.data.frame(cbind(education_terms[1:350], rank_terms[1:350], 
                        conduct_terms[1:350]))
colnames(lexicon_in_aeo) <- c("Education", "Rank", "Conduct")

# Words not appearing in AEO - top 100

lexicon_ex <- as.data.frame(cbind(ex_education_terms[1:100], ex_rank_terms[1:100],
                    ex_conduct_terms[1:100]))
colnames(lexicon_ex) <- c("Education", "Rank", "Conduct")
```

The top terms in each of these data frames can be viewed.

``` r
head(lexicon_in_aeo)
```

    ##      Education         Rank      Conduct
    ## 1    education independence    propriety
    ## 2 independence         rank      conduct
    ## 3      freedom      dignity independence
    ## 4       habits      station     prudence
    ## 5      society   reputation      freedom
    ## 6     learning      freedom    character

``` r
head(lexicon_ex)
```

    ##      Education           Rank           Conduct
    ## 1       ampler aggrandisement     animadverting
    ## 2      deprave   independency comprehensiveness
    ## 3  unblameable    descendents        unswerving
    ## 4 aggrandizing        burgher        humanizing
    ## 5      instill        insular          overstep
    ## 6      unlearn        weening       culpability

Examining the top terms in the `lexicon_in_aeo` data frame reveals that a number of terms are shared, for example *freedom*. To try to make the focus of each set of terms as close to each sub-theme as possible, unique terms are extracted, as are the top 120 terms which appear in more than one category.

``` r
# Calculating unique terms for education, rank and conduct
ed <- as.character(lexicon_in_aeo[,1])
ra <- as.character(lexicon_in_aeo[,2])
con <- as.character(lexicon_in_aeo[,3])

words <- c(ra, con) 
ed_terms <- ed[which(ed %in% words != T)]
words <- c(ed, con) 
ra_terms <- ra[which(ra %in% words != T)]
words <- c(ed, ra) 
con_terms <- con[which(con %in% words != T)]

# Identify terms which appear in one or more category

words2 <- c(ed_terms, ra_terms, con_terms)
edracon <- c(ed, ra, con)

generic_terms <- edracon[which(edracon %in% words2 != T)]

# Combine unique terms
lex_erc_unique <- as.data.frame(cbind(ed_terms[1:120], ra_terms[1:120], con_terms[1:120],
                        generic_terms[1:120]))
colnames(lex_erc_unique) <- c("Education", "Rank", "Conduct", "Generic")

# View the top 10 terms
lex_erc_unique[1:10, ]
```

    ##        Education        Rank     Conduct      Generic
    ## 1       learning     exalted   propriety    education
    ## 2   institutions   affluence     justice independence
    ## 3        parents      honors    judgment      freedom
    ## 4    improvement preeminence      regard       habits
    ## 5    instruction   greatness   rectitude      society
    ## 6    acquirement    nobility  generosity  independent
    ## 7       educated       ranks    delicacy     acquired
    ## 8     discipline      beauty approbation   reputation
    ## 9  establishment       glory benevolence      talents
    ## 10 retrenchments     possess     candour   literature

### Exploring Frequencies

Now we can start to examine the texts in our corpus further using the `lexicon_erc_unique`. The first step is to calculate the relative frequencies of the terms selected - this adjusts for text length.
