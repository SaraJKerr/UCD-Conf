Enhanced not Distant: Examining Independence in the Novels of Austen, Edgeworth, and Owenson
================
Sara J Kerr
February 2018

-   [Introduction](#introduction)
-   [Loading Packages](#loading-packages)
-   [Raw and Weighted Frequencies](#raw-and-weighted-frequencies)
-   [Collocations](#collocations)
-   [Creating A Lexicon](#creating-a-lexicon)
-   [Exploring Frequencies - Thematic Terms](#exploring-frequencies---thematic-terms)
-   [Changes Over Time](#changes-over-time)
-   [More Collocations](#more-collocations)
-   [Word Embedding Models](#word-embedding-models)
-   [Putting the Terms into Context](#putting-the-terms-into-context)

### Introduction

This document provides the code and results for the analysis conducted for the paper 'Enhanced not Distant: Examining Independence in the Novels of Austen, Edgeworth, and Owenson' presented at the 'Digital Cultures, Big Data and Society' conference on 15th and 16th February 2018.

This paper proposes a shift in focus from close and distant reading, and suggests instead considering the combined use of these methods as enhanced reading, which has the purpose of further improving the quality and extent of close readings of the texts through the use of digital methods. As a core part of this redefinition, is the importance of greater openness with regard to the texts, tools, and the parameters used. This provides the opportunity of making textual analysis a little more replicable, capitalising on the strengths of computational methods, allowing the researcher to repeat a particular analysis, and view the source of an interpretation.

### Loading Packages

The first step, having set the working directory, is to load the packages needed for the analysis. If packages are not already available use `install.packages` or install using the *Tools* menu in **R Studio**.

### Raw and Weighted Frequencies

At the most straightforward level, computational techniques allow the examination of the frequency of words. The raw frequency is the actual count of a word in a text, whereas the relative frequency is the frequency normalised by representing it as a proportion of the text as a whole.

The Austen, Edgeworth, Owenson corpus was converted into a Document Term Matrix using the `tm` package. The DTM represents the raw frequencies of each word in the corpus by text.

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

Although raw frequencies can be interesting when examining a single text, if the purpose is to compare texts some form of normalisation needs to take place. For this paper term frequency-inverse document frequency (Tf-Idf) of each term will be used. This weighting metric is a statistical measure of how important a word is to a document in a corpus.

``` r
corp_tfidf <- weightTfIdf(dtm)

corp_tfidf <- as.matrix(corp_tfidf)
corp_tfidf <- as.data.frame(corp_tfidf)

corp_tfidf$Text <- rownames(corp_tfidf) # Adds a column of text names
corp_tfidf$Author <- gsub("_.*","", corp_tfidf$Text) # adds a column of author names

corp_tf <- corp_tfidf %>%
         gather(key, Value, - Text, - Author) 

colnames(corp_tf) <- c("Text", "Author", "Word", "Value") 
```

The weightings of the focus terms *conduct*, *education* and *rank* can be viewed. The term *status* which was originally noted in the conference abstract was changed to *rank* as *status* only became associated with social class in 1820 (Historical Thesaurus), and only came to mean having or conferring social status in 1950.

``` r
corp_tf %>%
        filter(Word == "education" | Word == "rank" | Word == "conduct") %>%
        ggplot(aes(x = Author, y = Value, col = Author)) +
        geom_jitter(alpha = 0.7) +
        theme_bw() +
        theme() +
        ggtitle("Tf-Idf Weightings of 'conduct', 'education' and 'rank'") +
        labs(y = "Tf-Idf") +
        facet_grid(. ~ Word)
```

![](Enhanced_files/figure-markdown_github/unnamed-chunk-1-1.png)

The texts in which the terms are most significant can be identified.

``` r
corp_tf %>%
         filter(Author == "JA") %>%
         filter(Word == "education" | Word == "rank" | Word == "conduct") %>%
         group_by(Word, Text) 
```

    ## # A tibble: 18 x 4
    ## # Groups:   Word, Text [18]
    ##              Text Author      Word        Value
    ##             <chr>  <chr>     <chr>        <dbl>
    ##  1 JA_1811_SS.txt     JA   conduct 2.049709e-05
    ##  2 JA_1813_PP.txt     JA   conduct 1.280943e-05
    ##  3 JA_1814_MP.txt     JA   conduct 1.690630e-05
    ##  4  JA_1815_E.txt     JA   conduct 1.079577e-05
    ##  5 JA_1818_NA.txt     JA   conduct 1.309263e-05
    ##  6  JA_1818_P.txt     JA   conduct 1.524586e-05
    ##  7 JA_1811_SS.txt     JA education 5.693635e-06
    ##  8 JA_1813_PP.txt     JA education 6.126249e-06
    ##  9 JA_1814_MP.txt     JA education 8.875805e-06
    ## 10  JA_1815_E.txt     JA education 6.477459e-06
    ## 11 JA_1818_NA.txt     JA education 8.728422e-07
    ## 12  JA_1818_P.txt     JA education 4.814482e-06
    ## 13 JA_1811_SS.txt     JA      rank 3.480653e-06
    ## 14 JA_1813_PP.txt     JA      rank 1.248373e-05
    ## 15 JA_1814_MP.txt     JA      rank 5.167607e-06
    ## 16  JA_1815_E.txt     JA      rank 7.919645e-06
    ## 17 JA_1818_NA.txt     JA      rank 1.778630e-06
    ## 18  JA_1818_P.txt     JA      rank 1.798625e-05

``` r
corp_tf %>%
         filter(Author == "ME") %>%
         filter(Word == "education" | Word == "rank" | Word == "conduct") %>%
         group_by(Word, Text) 
```

    ## # A tibble: 48 x 4
    ## # Groups:   Word, Text [48]
    ##              Text Author    Word        Value
    ##             <chr>  <chr>   <chr>        <dbl>
    ##  1 ME_1800_CR.txt     ME conduct 1.973869e-06
    ##  2  ME_1801_B.txt     ME conduct 2.363105e-05
    ##  3 ME_1804_PT.txt     ME conduct 1.002845e-05
    ##  4 ME_1805_MG.txt     ME conduct 3.119718e-05
    ##  5  ME_1806_E.txt     ME conduct 2.200556e-05
    ##  6  ME_1806_L.txt     ME conduct 2.352751e-05
    ##  7  ME_1809_A.txt     ME conduct 2.080481e-05
    ##  8  ME_1809_D.txt     ME conduct 0.000000e+00
    ##  9  ME_1809_M.txt     ME conduct 1.678134e-05
    ## 10 ME_1809_MF.txt     ME conduct 1.464425e-05
    ## # ... with 38 more rows

``` r
corp_tf %>%
         filter(Author == "SO") %>%
         filter(Word == "education" | Word == "rank" | Word == "conduct") %>%
         group_by(Word, Text) 
```

    ## # A tibble: 18 x 4
    ## # Groups:   Word, Text [18]
    ##              Text Author      Word        Value
    ##             <chr>  <chr>     <chr>        <dbl>
    ##  1 SO_1803_SC.txt     SO   conduct 3.136617e-05
    ##  2 SO_1806_NS.txt     SO   conduct 2.039377e-05
    ##  3 SO_1806_WG.txt     SO   conduct 5.963713e-06
    ##  4  SO_1811_M.txt     SO   conduct 1.355892e-05
    ##  5 SO_1814_OD.txt     SO   conduct 1.577622e-05
    ##  6 SO_1818_FM.txt     SO   conduct 9.502075e-06
    ##  7 SO_1803_SC.txt     SO education 1.500121e-05
    ##  8 SO_1806_NS.txt     SO education 3.159598e-06
    ##  9 SO_1806_WG.txt     SO education 7.288982e-06
    ## 10  SO_1811_M.txt     SO education 1.595167e-06
    ## 11 SO_1814_OD.txt     SO education 8.605213e-06
    ## 12 SO_1818_FM.txt     SO education 5.701245e-06
    ## 13 SO_1803_SC.txt     SO      rank 1.111587e-05
    ## 14 SO_1806_NS.txt     SO      rank 4.038666e-05
    ## 15 SO_1806_WG.txt     SO      rank 3.510729e-05
    ## 16  SO_1811_M.txt     SO      rank 2.600434e-05
    ## 17 SO_1814_OD.txt     SO      rank 2.922538e-05
    ## 18 SO_1818_FM.txt     SO      rank 3.485305e-05

It may not be a surprise that *Sense and Sensibility* has the highest relative frequency of Austen's novels for the term *conduct*, or that *Mansfield Park* has the highest for *education*, or that Edgeworth's *Absentee* has the highest relative frequency for *rank* but it shows how word frequency is linked to themes within the text.

### Collocations

The three focal terms for the study - education, rank, conduct - can be by considering the collocations. Collocations are calculated based on the raw frequency of the words and are read vertically. For this study I have only compared the collocations which are positioned one step away from the focus word. First we load the function:

``` r
# Collocations
# Adapted from Stefan Th Gries - Quantitative Corpus Linguistics with R

# Collocation function

# Variables
# input - corpus: path to file in speech marks
# word: in speech marks 
# span: number of context
# output: path to file in speech marks - output file saved to folder

collocation <- function(corpus, word, num, output) {
        corpus_x <- tolower(scan(file = corpus, what = "char",
                                 sep = "\n", quiet = T))
        search_word <- paste0("\\b", word, "\\b")
        span <- num; 
        span <- (-span:span) 
        output_x <- output
        
        cleaned <- strsplit(corpus_x, "\\W")
        cleaned <- unlist(cleaned)
        cleaned <- cleaned[which(cleaned != "")]
        
        pos_match <- grep(search_word, cleaned, perl = T)
        
        results <- list()
        for(i in 1:length(span)) {
                collocate_pos <- pos_match + span[i]
                collocates <- cleaned[collocate_pos]
                sorted_collocates <- sort(table(collocates), decreasing = T)
                results[[i]] <- sorted_collocates
        }
        
        lengths <- sapply(results, length)
        
        cat(paste(rep(c("Word_", "Frequency_"), length(span)), 
                  rep(span, each = 2), sep = ""),"\n", sep = ",", file = output)
        
        for(k in 1:max(lengths)) {
                output_string <- paste(names(sapply(results, "[", k)),
                                       sapply(results, "[", k), sep = ",")
                output_string2 <- gsub("NA\tNA", ",", output_string, perl = T)
                cat(output_string2, "\n", sep = ",", file = output, append = T)
        }
}

# To run 
# collocation("Combined_Texts/ja.txt", "power", 2, "ja_coll_power.csv")
```

Then we will run the function on each of the three corpora.

``` r
collocation("Combined_Texts/ja.txt", "education", 1, "Collocation/ja_coll_ed.csv")

collocation("Combined_Texts/me.txt", "education", 1, "Collocation/me_coll_ed.csv")

collocation("Combined_Texts/so.txt", "education", 1, "Collocation/so_coll_ed.csv")

ja_ed <- read.csv("Collocation/ja_coll_ed.csv")
ja_ed <- ja_ed[, -7]
head(ja_ed)
```

    ##   Word_.1 Frequency_.1    Word_0 Frequency_0 Word_1 Frequency_1
    ## 1      of            7 education          65    and          14
    ## 2      an            5      <NA>          NA    had           6
    ## 3     her            5      <NA>          NA     of           3
    ## 4     the            5      <NA>          NA     to           3
    ## 5     and            4      <NA>          NA   from           2
    ## 6      as            2      <NA>          NA     he           2

``` r
me_ed <- read.csv("Collocation/me_coll_ed.csv")
me_ed <- me_ed[, -7]
head(me_ed)
```

    ##   Word_.1 Frequency_.1    Word_0 Frequency_0 Word_1 Frequency_1
    ## 1      of           25 education         164    and          22
    ## 2     the           21      <NA>          NA     of          17
    ## 3     his           11      <NA>          NA  which           9
    ## 4     and           10      <NA>          NA     as           6
    ## 5     her           10      <NA>          NA    but           5
    ## 6      an            7      <NA>          NA    the           5

``` r
so_ed <- read.csv("Collocation/so_coll_ed.csv")
so_ed <- so_ed[, -7]
head(so_ed)
```

    ##   Word_.1 Frequency_.1    Word_0 Frequency_0 Word_1 Frequency_1
    ## 1      of           14 education          68    and          12
    ## 2     and            8      <NA>          NA    had          10
    ## 3     the            7      <NA>          NA     of           6
    ## 4     her            3      <NA>          NA    the           4
    ## 5    that            3      <NA>          NA  which           3
    ## 6      to            3      <NA>          NA     at           2

``` r
collocation("Combined_Texts/ja.txt", "rank", 1, "Collocation/ja_coll_ra.csv")

collocation("Combined_Texts/me.txt", "rank", 1, "Collocation/me_coll_ra.csv")

collocation("Combined_Texts/so.txt", "rank", 1, "Collocation/so_coll_ra.csv")

ja_ra <- read.csv("Collocation/ja_coll_ra.csv")
ja_ra <- ja_ra[, -7]
head(ja_ra)
```

    ##   Word_.1 Frequency_.1 Word_0 Frequency_0 Word_1 Frequency_1
    ## 1      of            9   rank          42    and          10
    ## 2     the            4   <NA>          NA     of           7
    ## 3     his            3   <NA>          NA     as           3
    ## 4     for            2   <NA>          NA     in           3
    ## 5     her            2   <NA>          NA    she           2
    ## 6    high            2   <NA>          NA  below           1

``` r
me_ra <- read.csv("Collocation/me_coll_ra.csv")
me_ra <- me_ra[, -7]
head(me_ra)
```

    ##   Word_.1 Frequency_.1 Word_0 Frequency_0 Word_1 Frequency_1
    ## 1      of           39   rank         171    and          53
    ## 2     and           14   <NA>          NA     of          26
    ## 3     the           14   <NA>          NA     in          14
    ## 4    high            9   <NA>          NA     or           6
    ## 5 certain            8   <NA>          NA     to           4
    ## 6    same            7   <NA>          NA    who           4

``` r
so_ra <- read.csv("Collocation/so_coll_ra.csv")
so_ra <- so_ra[, -7]
head(so_ra)
```

    ##   Word_.1 Frequency_.1 Word_0 Frequency_0 Word_1 Frequency_1
    ## 1      of           32   rank         190    and          42
    ## 2     the           17   <NA>          NA     in          18
    ## 3     and           10   <NA>          NA     of          13
    ## 4     her            7   <NA>          NA     or           7
    ## 5     his            7   <NA>          NA    but           6
    ## 6      to            6   <NA>          NA     to           6

``` r
collocation("Combined_Texts/ja.txt", "conduct", 1, "Collocation/ja_coll_co.csv")

collocation("Combined_Texts/me.txt", "conduct", 1, "Collocation/me_coll_co.csv")

collocation("Combined_Texts/so.txt", "conduct", 1, "Collocation/so_coll_co.csv")

ja_co <- read.csv("Collocation/ja_coll_co.csv")
ja_co <- ja_co[, -7]
head(ja_co)
```

    ##   Word_.1 Frequency_.1  Word_0 Frequency_0 Word_1 Frequency_1
    ## 1     his           23 conduct         160    and          17
    ## 2       s           21    <NA>          NA     in          10
    ## 3      of           17    <NA>          NA     of          10
    ## 4     her           15    <NA>          NA      i           5
    ## 5      my            9    <NA>          NA  which           5
    ## 6     own            9    <NA>          NA     as           4

``` r
me_co <- read.csv("Collocation/me_coll_co.csv")
me_co <- me_co[, -7]
head(me_co)
```

    ##   Word_.1 Frequency_.1  Word_0 Frequency_0  Word_1 Frequency_1
    ## 1       s           36 conduct         313     and          35
    ## 2     her           35    <NA>          NA      of          28
    ## 3     his           31    <NA>          NA      to          17
    ## 4      my           19    <NA>          NA towards          17
    ## 5     the           17    <NA>          NA     the          11
    ## 6    good           12    <NA>          NA       i           9

``` r
so_co <- read.csv("Collocation/so_coll_co.csv")
so_co <- so_co[, -7]
head(so_co)
```

    ##   Word_.1 Frequency_.1  Word_0 Frequency_0 Word_1 Frequency_1
    ## 1     the           24 conduct         178     of          29
    ## 2     her           19    <NA>          NA    and          18
    ## 3      my           14    <NA>          NA     in           7
    ## 4     his           12    <NA>          NA  which           7
    ## 5      of           11    <NA>          NA    you           7
    ## 6    your           10    <NA>          NA    the           6

The collocations reveal subtle differences between the author's approaches to these key terms, as well as some broader insights into their attitudes.

However, basing a thematic analysis on results for a single term may be limiting and exclude areas worth of further investigation.

### Creating A Lexicon

As a starting point for further analysis I wanted to create a lexicon of terms which were associated with independence, and where possible to consider the meanings and connotations of independence during this period. 187 texts from the first two periods, 1710–1780 and 1780–1850, of CLMET3.1, totalling just over 22 million words, were selected and a word embedding model, using the `wordVectors` package, was created with 300 dimensions and a window of 12 words. CLMET3.1 is available free on request from the CLMET website.

``` r
prep_word2vec("plain", "Clmet_processed_period_1_2.txt", lowercase = T)

clmet <- train_word2vec("Clmet_processed_period_1_2.txt",
                        output = "Bin_files/Clmet_model_period_1_2.bin", threads = 1,
                        vectors = 300, window = 12)
```

The model created is available in this GitHub repository.
The created model is read into R and vector addition was used to identify the terms of interest. For this study the terms closest to `"independence" + "education"`, `"independence" + "conduct" + "propriety"`, and `"independence" + "rank"`.

``` r
clmet <- read.vectors("Bin_files/Clmet_model_period_1_2.bin")
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

Comparing the terms which do not appear in the AEO corpus we see a number of terms which are infrequent in modern usage. This may be due to the broader scope of the CLMET3.1 corpus in terms of date, or text type.

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

### Exploring Frequencies - Thematic Terms

The lexicon created earlier is used to extract the terms of interest with their relative frequency.

``` r
education_w <- as.character(lex_erc_unique[,1])
rank_w <- as.character(lex_erc_unique[,2])
conduct_w <- as.character(lex_erc_unique[,3])
generic_w <- as.character(lex_erc_unique[,4])

# Create sub-theme results
corp_education <- subset.data.frame(corp_tfidf, select = education_w)
corp_education$Text <- rownames(corp_education)
corp_education$Theme <- rep("Education", times = nrow(corp_education))

corp_rank <- subset.data.frame(corp_tfidf, select = rank_w)
corp_rank$Text <- rownames(corp_rank)
corp_rank$Theme <- rep("Rank", times = nrow(corp_rank))

corp_conduct <- subset.data.frame(corp_tfidf, select = conduct_w)
corp_conduct$Text <- rownames(corp_conduct)
corp_conduct$Theme <- rep("Conduct", times = nrow(corp_conduct))

corp_generic <- subset.data.frame(corp_tfidf, select = generic_w)
corp_generic$Text <- rownames(corp_generic)
corp_generic$Theme <- rep("Generic", times = nrow(corp_generic))

corp_join <- full_join(corp_rank, corp_education, by = c("Text", "Theme")) %>%
              full_join(corp_conduct, by = c("Text", "Theme")) %>%
              full_join(corp_generic, by = c("Text", "Theme")) 

# combine the sub-themes
corp_tf_aeo <- corp_join %>%
        gather(key, Value, - Text, -Theme)

# remove NA rows
corp_tf_aeo <- filter(corp_tf_aeo, Value != "NA")

colnames(corp_tf_aeo) <- c("Text", "Theme", "Word", "Value")  

# Extract the Author name from the Text variable
corp_tf_aeo$Author <- gsub("_.*","", corp_tf_aeo$Text)

# View the results
head(corp_tf_aeo)
```

    ##             Text Theme    Word Value Author
    ## 1 JA_1811_SS.txt  Rank exalted     0     JA
    ## 2 JA_1813_PP.txt  Rank exalted     0     JA
    ## 3 JA_1814_MP.txt  Rank exalted     0     JA
    ## 4  JA_1815_E.txt  Rank exalted     0     JA
    ## 5 JA_1818_NA.txt  Rank exalted     0     JA
    ## 6  JA_1818_P.txt  Rank exalted     0     JA

The Tf-Idf weightings of the terms can now be plotted.

``` r
pt <- corp_tf_aeo %>%
        filter(Value > 0, Theme != "Generic") %>%
        ggplot(aes(x = Author, y = Value, col = Author)) +
        geom_jitter(alpha = 0.4) +
        scale_y_continuous(trans='log2') +
        theme_bw() +
        theme() +
        ggtitle("Independence Related Terms by Theme") +
        labs(y = "TF-IDF Weightings",
             subtitle = "Each theme consists of the top 120 unique terms for that theme from the independence lexicon") +
        facet_grid(. ~ Theme)

pt
```

![](Enhanced_files/figure-markdown_github/Plot%20of%20terms%20by%20theme-1.png)

### Changes Over Time

To examine how an author's use of words varies over time the date of each text is added to the data.

``` r
corp_tf_aeo$Year <- gsub("[^0-9]", "", corp_tf_aeo$Text)
```

However, this can be misleading if there is more than one text published in a single year. Therefore, it is more revealing to plot by individual text, especially as the text names include the date of the text. The terms can now be plotted:

``` r
corp_tf_aeo %>%
        filter(Value > 0 & Author == "JA" & Theme != "Generic") %>%
        ggplot(aes(x = Text, y = Value, col = Theme)) +
        geom_jitter(alpha = 0.6) +
        scale_y_continuous(trans='log2') +
        theme_bw() +
        theme(axis.text.x=element_text(angle=45, hjust = 1)) +
        ggtitle("Austen Terms by Text and Theme") +
        labs(y = "TF-IDF Weighting") 
```

![](Enhanced_files/figure-markdown_github/Plotting%20terms%20by%20text%20JA-1.png)

``` r
corp_tf_aeo %>%
        filter(Value > 0 & Author == "ME" & Theme != "Generic") %>%
        ggplot(aes(x = Text, y = Value, col = Theme)) +
        geom_jitter(alpha = 0.6) +
        scale_y_continuous(trans='log2') +
        theme_bw() +
        theme(axis.text.x=element_text(angle=45, hjust = 1)) +
        ggtitle("Edgeworth Terms by Text and Theme") +
        labs(y = "TF-IDF Weighting") 
```

![](Enhanced_files/figure-markdown_github/Plotting%20terms%20by%20date%20ME-1.png)

``` r
corp_tf_aeo %>%
        filter(Value > 0 & Author == "SO" & Theme != "Generic") %>%
        ggplot(aes(x = Text, y = Value, col = Theme)) +
        geom_jitter(alpha = 0.6) +
        scale_y_continuous(trans='log2') +
        theme_bw() +
        theme(axis.text.x=element_text(angle=45, hjust = 1)) +
        ggtitle("Owenson Terms by Text and Theme") +
        labs(y = "TF-IDF Weighting") 
```

![](Enhanced_files/figure-markdown_github/Plotting%20terms%20by%20date%20SO-1.png)

These plots allow us to consider the themes across the author's corpus. The data can be examined in closer detail.

``` r
corp_tf_aeo %>%
        filter(Value > 0 & Theme != "Generic") %>%
        filter(Text == "JA_1814_MP.txt" | Text == "ME_1814_P.txt" | Text == "SO_1814_OD.txt") %>%
        ggplot(aes(x = Text, y = Value, col = Author)) +
        #geom_point(alpha = 0.6) +
        geom_jitter(alpha = 0.6) +
        scale_y_continuous(trans='log2') +
        theme_bw() +
        theme(axis.text.x=element_text(angle=45, hjust = 1)) +
        ggtitle("1814 Texts - Terms by Theme") +
        labs(y = "TF-IDF Weighting") +
        facet_grid(. ~ Theme)
```

![](Enhanced_files/figure-markdown_github/1814%20in%20detail-1.png)

The highest weighted terms can be extracted from the larger set and then be examined by author.

``` r
high_tf <-corp_tf_aeo %>% filter(Value > 6.103516e-05) # Value taken from plot Terms by Theme

ja_high_tf <- high_tf %>% filter(Author == "JA") %>%
                          arrange(desc(Value))

head(ja_high_tf, n = 10)
```

    ##              Text   Theme        Word        Value Author Year
    ## 1  JA_1813_PP.txt Generic connections 0.0004484997     JA 1813
    ## 2   JA_1818_P.txt Generic  usefulness 0.0002464093     JA 1818
    ## 3   JA_1818_P.txt    Rank  precedence 0.0001971274     JA 1818
    ## 4  JA_1811_SS.txt Generic connections 0.0001348562     JA 1811
    ## 5  JA_1818_NA.txt    Rank     wealthy 0.0001330871     JA 1818
    ## 6  JA_1814_MP.txt Generic  usefulness 0.0001297918     JA 1814
    ## 7   JA_1815_E.txt Conduct   unreserve 0.0001227371     JA 1815
    ## 8   JA_1815_E.txt Generic   disparity 0.0001152263     JA 1815
    ## 9  JA_1811_SS.txt Conduct   unreserve 0.0001078850     JA 1811
    ## 10 JA_1811_SS.txt Generic  ineligible 0.0001043365     JA 1811

``` r
me_high_tf <- high_tf %>% filter(Author == "ME") %>%
                          arrange(desc(Value))

head(me_high_tf, n = 10)
```

    ##              Text     Theme       Word        Value Author Year
    ## 1   ME_1809_D.txt Education calculator 0.0009817831     ME 1809
    ## 2   ME_1806_E.txt   Conduct       ment 0.0005420596     ME 1806
    ## 3   ME_1814_P.txt   Generic  patronage 0.0004007973     ME 1814
    ## 4  ME_1809_MF.txt Education     school 0.0003251715     ME 1809
    ## 5   ME_1809_D.txt Education  political 0.0003129363     ME 1809
    ## 6  ME_1809_MF.txt      Rank    servile 0.0003026719     ME 1809
    ## 7  ME_1809_MF.txt   Generic   industry 0.0002791113     ME 1809
    ## 8   ME_1812_V.txt Education  political 0.0002750027     ME 1812
    ## 9  ME_1805_MG.txt      Rank  sovereign 0.0002632692     ME 1805
    ## 10  ME_1809_D.txt      Rank enjoyments 0.0002578649     ME 1809

``` r
so_high_tf <- high_tf %>% filter(Author == "SO") %>%
                          arrange(desc(Value))

head(so_high_tf, n = 10)
```

    ##              Text     Theme      Word        Value Author Year
    ## 1  SO_1814_OD.txt      Rank     honor 0.0038272525     SO 1814
    ## 2  SO_1818_FM.txt      Rank     honor 0.0027547435     SO 1818
    ## 3   SO_1811_M.txt   Generic  religion 0.0014534832     SO 1811
    ## 4  SO_1814_OD.txt      Rank     favor 0.0010754358     SO 1814
    ## 5   SO_1811_M.txt Education religious 0.0008139506     SO 1811
    ## 6  SO_1806_WG.txt      Rank  milesian 0.0007732440     SO 1806
    ## 7   SO_1811_M.txt   Conduct      zeal 0.0005527687     SO 1811
    ## 8  SO_1806_NS.txt      Rank  splendor 0.0004939533     SO 1806
    ## 9  SO_1806_NS.txt   Conduct      ment 0.0004626252     SO 1806
    ## 10 SO_1818_FM.txt      Rank    honors 0.0003734970     SO 1818

### More Collocations

A number of the terms which have been repeated - power, country, fortune, feelings - can be explored further by considering the collocations.

``` r
collocation("Combined_Texts/ja.txt", "power", 1, "Collocation/ja_coll_power.csv")
collocation("Combined_Texts/ja.txt", "country", 1, "Collocation/ja_coll_country.csv")
collocation("Combined_Texts/ja.txt", "fortune", 1, "Collocation/ja_coll_fortune.csv")
collocation("Combined_Texts/ja.txt", "impropriety", 1, "Collocation/ja_coll_impropriety.csv")

collocation("Combined_Texts/me.txt", "power", 1, "Collocation/me_coll_power.csv")
collocation("Combined_Texts/me.txt", "country", 1, "Collocation/me_coll_country.csv")
collocation("Combined_Texts/me.txt", "fortune", 1, "Collocation/me_coll_fortune.csv")
collocation("Combined_Texts/me.txt", "impropriety", 1, "Collocation/me_coll_impropriety.csv")

collocation("Combined_Texts/so.txt", "power", 1, "Collocation/so_coll_power.csv")
collocation("Combined_Texts/so.txt", "country", 1, "Collocation/so_coll_country.csv")
collocation("Combined_Texts/so.txt", "fortune", 1, "Collocation/so_coll_fortune.csv")
collocation("Combined_Texts/so.txt", "impropriety", 1, "Collocation/so_coll_impropriety.csv")

ja_impropriety <- read.csv("Collocation/ja_coll_impropriety.csv")
ja_impropriety <- ja_impropriety[, -7]
head(ja_impropriety)
```

    ##   Word_.1 Frequency_.1      Word_0 Frequency_0 Word_1 Frequency_1
    ## 1     the            7 impropriety          20     of           9
    ## 2      or            2        <NA>          NA     in           5
    ## 3    _is_            1        <NA>          NA     is           1
    ## 4     and            1        <NA>          NA     it           1
    ## 5  beyond            1        <NA>          NA nobody           1
    ## 6 conduct            1        <NA>          NA     oh           1

``` r
me_impropriety<- read.csv("Collocation/me_coll_impropriety.csv")
me_impropriety <- me_impropriety[, -7]
head(me_impropriety)
```

    ##   Word_.1 Frequency_.1      Word_0 Frequency_0 Word_1 Frequency_1
    ## 1     any            3 impropriety          14     of           5
    ## 2      no            3        <NA>          NA     in           4
    ## 3     the            3        <NA>          NA    and           1
    ## 4      or            2        <NA>          NA  could           1
    ## 5      an            1        <NA>          NA    not           1
    ## 6   great            1        <NA>          NA     on           1

``` r
so_impropriety<- read.csv("Collocation/so_coll_impropriety.csv")
so_impropriety <- so_impropriety[, -7]
head(so_impropriety)
```

    ##     Word_.1 Frequency_.1      Word_0 Frequency_0 Word_1 Frequency_1
    ## 1       the            3 impropriety           6     of           4
    ## 2       any            1        <NA>          NA  apply           1
    ## 3      some            1        <NA>          NA    she           1
    ## 4 unguarded            1        <NA>          NA   <NA>          NA

### Word Embedding Models

Although the terms related to the three themes have been explored, it is also interesting to apply a word embedding model to each author corpus to identify the terms that the author associates with education, rank and conduct.

``` r
ja <- read.vectors("Bin_files/ja_ucd.bin")
```

    ## Filename ends with .bin, so reading in binary format

    ## Reading a word2vec binary file of 5971 rows and 300 columns

``` r
me <- read.vectors("Bin_files/me_ucd.bin")
```

    ## Filename ends with .bin, so reading in binary format

    ## Reading a word2vec binary file of 10140 rows and 300 columns

``` r
so <- read.vectors("Bin_files/so_ucd.bin")
```

    ## Filename ends with .bin, so reading in binary format

    ## Reading a word2vec binary file of 9863 rows and 300 columns

The word embedding analysis is carried out using a custom function which creates a number of outputs, allowing the results to be examined at a later date.

``` r
# w2v_analysis2 analyses a chosen term in a vector space model
# The function takes 6 arguments:
# vsm - a vector space model 
# words - a character vector of focus words
# seed - an integer
# path - the path to the folder you want files saved to 
# ref_name - the reference name for the exported files 
# num - the number of nearest words you wish to examine

# The function will create a vector which is the average of the words input and 
# will output a wordlist of the n nearest words, a csv of the words and their
# positions, and a plot of the 2D reduction of the vector space
# model using the Barnes-Hut implementation of t-SNE. The points for each word
# are marked in red so the labels can be moved by ggrepel for ease of reading.
# An HTML network graph for the chosen word will also be created
# set.seed is used to ensure replicability

w2v_analysis2 <- function(vsm, words, seed, path, ref_name, num) {
        # Set the seed
        if (!missing(seed))
                set.seed(seed)
        
        # Identify the nearest 10 words to the average vector of search terms
        ten <- closest_to(vsm, vsm[[words]])
        
        # Identify the nearest n words to the average vector of search terms and 
        # save as a .txt file
        main <- nearest_to(vsm, vsm[[words]], num)
        wordlist <- names(main)
        filepath <- paste0(path, ref_name)
        write(wordlist, paste0(filepath, ".txt"))
        
        
        # Create a subset vector space model
        new_model <- vsm[[wordlist, average = F]]
        
        # Run Rtsne to reduce new VSM to 2D (Barnes-Hut)
        reduction <- Rtsne(as.matrix(new_model), dims = 2, initial_dims = 50, 
                           perplexity = 30, theta = 0.5, check_duplicates = F,
                           pca = F, max_iter = 1000, verbose = F, 
                           is_distance = F, Y_init = NULL)
        
        # Extract Y (positions for plot) as a dataframe and add row names
        df <- as.data.frame(reduction$Y)
        rows <- rownames(new_model)
        rownames(df) <- rows
        
        # Save dataframe as .csv file
        write.csv(df, paste0(filepath, ".csv"))
        
        # Create t-SNE plot and save as jpeg
        ggplot(df) +
                geom_point(aes(x = V1, y = V2), color = "red") +
                geom_text_repel(aes(x = V1, y = V2, label = rownames(df),
                                    size = 8)) +
                xlab("Dimension 1") +
                ylab("Dimension 2 ") +
                theme_bw(base_size = 16) + 
                theme(legend.position = "none") +
                ggtitle(paste0("2D reduction of VSM ", ref_name, " using t_SNE"))
        
        ggsave(paste0(ref_name, ".jpeg"), path = path, width = 24, 
               height = 18, dpi = 100)
        
        # Create a network plot of the words
        sim <- cosineSimilarity(new_model, new_model) %>% round(2)
        # convert those below threshold to 0
        sim[sim < max(sim)/2] <- 0
        g <- graph_from_incidence_matrix(sim)
        
        #g3 <- make_ego_graph(graph = g, order = 1, nodes = V(g)[name == word], 
        #             mode = "out", mindist = 0)
        # Create a graph object
        # edges <- get.edgelist(g3[[1]])
        # edges <- as.data.frame(edges)
        # 
        # nodes <- as.data.frame(unique(edges[, 2])) # Removes duplicate words
        # 
        # net <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)
        # 
        # net <- simplify(net, remove.multiple = T, remove.loops = T) # removes loops
        # #net # This prints a section of the vertex names and links
        # 
        # # This acts as a test that the graph will plot (lables have been removed)
        # #plot(net, edge.arrow.size = .4, vertex.label = NA)
        # 
        # 
        # # Create visNetwork version of graph
        # 
        # data <- toVisNetworkData(net)
        # visNetwork(nodes = data$nodes, edges = data$edges, main = "Network of Terms") %>%
        #         visOptions(highlightNearest = T) %>%
        #         visSave(paste0(ref_name, ".html"))
                

        # Larger plot - Takes a long time show
        edges <- get.edgelist(g)
        # Name columns
         colnames(edges) <- c("from", "to")
         g2 <- graph(edges = edges)
         g2 <- simplify(g2) # removes loops
     # Community detection based on greedy optimization of modularity
        # cfg <- cluster_fast_greedy(as.undirected(g2))
        # V(g2)$community <- cfg$membership
        # pal2 <- rainbow(33, alpha = 0.7) 
        # V(g2)$color <- pal2[V(g2)$community]
        data <- toVisNetworkData(g2)
         visNetwork(nodes = data$nodes, edges = data$edges, main = "Network of Terms") %>%
                visOptions(highlightNearest = T) %>%
                visSave(paste0(ref_name, ".html"))
        
                
        #new_list <- list("Ten closest" = ten, "Status" = "Analysis Complete") 
        return(ten) # replace with new_list if running as stand alone.
        
}
```

The function can now be applied to the individual text corpora.

``` r
w2v_analysis2(ja, c("independence", "education"), 42, "W2V", "ja_ed", 300)
```

    ##              word similarity to vsm[[words]]
    ## 1       education                  0.8449396
    ## 2    independence                  0.8322372
    ## 3      illiterate                  0.6543468
    ## 4         decorum                  0.6452732
    ## 5       affluence                  0.6429039
    ## 6      liberality                  0.6412369
    ## 7         foreign                  0.6372045
    ## 8  respectability                  0.6258070
    ## 9       privilege                  0.6203424
    ## 10         nicety                  0.6148619

``` r
w2v_analysis2(ja, c("independence", "conduct", "propriety"), 42, "W2V", "ja_con", 300)
```

    ##            word similarity to vsm[[words]]
    ## 1     propriety                  0.7644699
    ## 2  independence                  0.7480361
    ## 3       conduct                  0.7204257
    ## 4     arrogance                  0.7146520
    ## 5       decorum                  0.6949807
    ## 6       confide                  0.6767660
    ## 7    individual                  0.6625409
    ## 8     obstinacy                  0.6622826
    ## 9         atone                  0.6594536
    ## 10  inferiority                  0.6591436

``` r
w2v_analysis2(ja, c("independence", "rank"), 42, "W2V", "ja_ra", 300)
```

    ##              word similarity to vsm[[words]]
    ## 1            rank                  0.8373875
    ## 2    independence                  0.8345598
    ## 3     inferiority                  0.6439992
    ## 4           model                  0.6436069
    ## 5         decorum                  0.6345736
    ## 6         foreign                  0.6274890
    ## 7  respectability                  0.6268024
    ## 8         display                  0.6238370
    ## 9      illiterate                  0.6100427
    ## 10     liberality                  0.6099524

``` r
w2v_analysis2(me, c("independence", "education"), 42, "W2V", "me_ed", 300)
```

    ##              word similarity to vsm[[words]]
    ## 1    independence                  0.8304337
    ## 2       education                  0.8033509
    ## 3        precepts                  0.5826519
    ## 4  respectability                  0.5729094
    ## 5   subordination                  0.5602049
    ## 6       greatness                  0.5496903
    ## 7          habits                  0.5437650
    ## 8     diminishing                  0.5423602
    ## 9          fruits                  0.5391290
    ## 10 understandings                  0.5312719

``` r
w2v_analysis2(me, c("independence", "conduct", "propriety"), 42, "W2V", "me_con", 300)
```

    ##            word similarity to vsm[[words]]
    ## 1  independence                  0.7840707
    ## 2     propriety                  0.7612972
    ## 3       conduct                  0.6842888
    ## 4   diminishing                  0.6202232
    ## 5     greatness                  0.6163460
    ## 6    compatible                  0.5915785
    ## 7      acrimony                  0.5883454
    ## 8      rudeness                  0.5765445
    ## 9    definition                  0.5749813
    ## 10      biassed                  0.5732127

``` r
w2v_analysis2(me, c("independence", "rank"), 42, "W2V", "me_ra", 300)
```

    ##              word similarity to vsm[[words]]
    ## 1            rank                  0.8224599
    ## 2    independence                  0.7891450
    ## 3  respectability                  0.6387306
    ## 4         combine                  0.5662938
    ## 5   subordination                  0.5657170
    ## 6       greatness                  0.5591790
    ## 7         fashion                  0.5525931
    ## 8         station                  0.5509154
    ## 9      connexions                  0.5458156
    ## 10       commerce                  0.5448092

``` r
w2v_analysis2(so, c("independence", "education"), 42, "W2V", "so_ed", 300)
```

    ##            word similarity to vsm[[words]]
    ## 1     education                  0.8788888
    ## 2  independence                  0.8286950
    ## 3  acquirements                  0.6614207
    ## 4      industry                  0.6554561
    ## 5  distinctions                  0.6488139
    ## 6        tedium                  0.6440423
    ## 7     affluence                  0.6432243
    ## 8   overbearing                  0.6398824
    ## 9   restriction                  0.6383883
    ## 10        unity                  0.6355840

``` r
w2v_analysis2(so, c("independence", "conduct", "propriety"), 42, "W2V", "so_con", 300)
```

    ##            word similarity to vsm[[words]]
    ## 1       conduct                  0.7907343
    ## 2     propriety                  0.7536789
    ## 3         dence                  0.7254965
    ## 4  independence                  0.7191851
    ## 5      converts                  0.7128375
    ## 6        ruling                  0.7118501
    ## 7        candid                  0.7117820
    ## 8      failings                  0.7081640
    ## 9       decorum                  0.7067157
    ## 10    stability                  0.7009322

``` r
w2v_analysis2(so, c("independence", "rank"), 42, "W2V", "so_ra", 300)
```

    ##             word similarity to vsm[[words]]
    ## 1           rank                  0.8369797
    ## 2   independence                  0.7741316
    ## 3       opulence                  0.6687496
    ## 4      emolument                  0.6592271
    ## 5  consideration                  0.6480145
    ## 6         wealth                  0.6364553
    ## 7      servility                  0.6324808
    ## 8       patriots                  0.6239748
    ## 9     assumption                  0.6214578
    ## 10     supremacy                  0.6135674

### Putting the Terms into Context

To examine the relevance of the terms in the context of the novels a KWIC function is used.

``` r
text_kwic <- function(files, input, word, context) {
        corpus <- make_word_list(files, input)
        context <- as.numeric(context)
        keyword <- tolower(word)
        result <- NULL
        # create the KWIC readout
        for (i in 1:length(corpus)) {
                hits <- which(corpus[[i]] == keyword)
                let <- files[i]
                if(length(hits) > 0){
                        for(j in 1:length(hits)) {
                                start <- hits[j] - context
                                if(start < 1) {
                                        start <- 1
                                }
                                end <- hits[j] + context
                                myrow <- cbind(let, hits[j],
                                               paste(corpus[[i]][start: (hits[j] -1)],
                                                     collapse = " "),
                                               paste(corpus[[i]][hits[j]],
                                                     collapse = " "),
                                               paste(corpus[[i]][(hits[j] +1): end],
                                                     collapse = " "))
                                result <- rbind(result, myrow)
                        }
                        
                } else {
                        z <- paste0(let, " YOUR KEYWORD WAS NOT FOUND\n")
                        cat(z)
                }
        }
        colnames(result) <- c("file", "position", "left",
                              "keyword", "right")
        return(result)
}

# Function used within text_kwic

make_word_list <- function(files, input.dir) {
        # create an empty list for the results
        word_list <- list()
        # read in the files and process them
        for(i in 1:length(files)) {
                text <- scan(paste(input.dir, files[i], sep = "/"), 
                             what = "character", sep = "\n")   
                text <- paste(text, collapse = " ")
                text_lower <- tolower(text)
                text_words <- strsplit(text_lower, "\\W")
                text_words <- unlist(text_words)
                text_words <- text_words[which(text_words != "")]
                word_list[[files[i]]] <- text_words
        }
        return(word_list)
}
```

The function takes an input directory, a list of files, the key word, and a number indicating the amount of context either side of the key word.

``` r
input_dir <- "aeo_corpus" # path to .txt letters' folder
files <- dir(input_dir, "\\.txt") # vector of file names

education <- as.data.frame(text_kwic(files, input_dir, "education", 6))
```

The first few lines can be examined, as the files are ordered by file name the first examples are from Austen:

``` r
head(education, n = 10)
```

    ##              file position                                 left   keyword
    ## 1  JA_1811_SS.txt     4169   his understanding was good and his education
    ## 2  JA_1811_SS.txt     9676   all the deficiencies of nature and education
    ## 3  JA_1811_SS.txt    31379      body at times whatever be their education
    ## 4  JA_1811_SS.txt    38981      powers had received no aid from education
    ## 5  JA_1811_SS.txt    39031   for the neglect of abilities which education
    ## 6  JA_1811_SS.txt    42184           his eyes to her defects of education
    ## 7  JA_1811_SS.txt    64308 strictest sense by watching over her education
    ## 8  JA_1811_SS.txt    78009       to the misfortune of a private education
    ## 9  JA_1811_SS.txt    94094              at it from his style of education
    ## 10 JA_1811_SS.txt   116184                by him to her want of education
    ##                                       right
    ## 1        had given it solid improvement but
    ## 2         supported the good spirits of sir
    ## 3          or state know your own happiness
    ## 4       she was ignorant and illiterate and
    ## 5    might have rendered so respectable but
    ## 6             while the same period of time
    ## 7              myself had the nature of our
    ## 8  while he himself though probably without
    ## 9              it was always to be expected
    ## 10         and till her last letter reached

``` r
tail(education, n = 10)
```

    ##               file position                                   left
    ## 288 SO_1818_FM.txt    56612 that education which wealth can obtain
    ## 289 SO_1818_FM.txt    56619     had made gentlemen of his brothers
    ## 290 SO_1818_FM.txt    56628              a fine lady of his sister
    ## 291 SO_1818_FM.txt    56640          than their father and want of
    ## 292 SO_1818_FM.txt    56839         of those who stood indebted to
    ## 293 SO_1818_FM.txt    58183          to them con with your college
    ## 294 SO_1818_FM.txt    59109       a stiletto and combining with an
    ## 295 SO_1818_FM.txt    59201         man formed alike by nature and
    ## 296 SO_1818_FM.txt    59742             she had gone to finish her
    ## 297 SO_1818_FM.txt    61435       bishop than miss kate lesley her
    ##       keyword                                      right
    ## 288 education         had made gentlemen of his brothers
    ## 289 education                    had made a fine lady of
    ## 290 education               had made his sons wiser than
    ## 291 education             had left himself upon the last
    ## 292 education alone for their distinctions then released
    ## 293 education            and your speaking french like a
    ## 294 education      whose object was pretension and whose
    ## 295 education               to betray the land that gave
    ## 296 education              her first simple name she had
    ## 297 education               had been founded by the maid

``` r
input_dir <- "aeo_corpus" # path to .txt letters' folder
files <- dir(input_dir, "\\.txt") # vector of file names

country <- as.data.frame(text_kwic(files, input_dir, "country", 6))
```

``` r
head(country, n = 10)
```

    ##              file position                                left keyword
    ## 1  JA_1811_SS.txt     8147     interest in the appearance of a country
    ## 2  JA_1811_SS.txt     8476     the valley and reached into the country
    ## 3  JA_1811_SS.txt     9821              of a new family in the country
    ## 4  JA_1811_SS.txt    12303   never stirred from home the whole country
    ## 5  JA_1811_SS.txt    13236              john what is he in the country
    ## 6  JA_1811_SS.txt    13455          property of his own in the country
    ## 7  JA_1811_SS.txt    20305        of mind by driving about the country
    ## 8  JA_1811_SS.txt    21958 week after colonel brandon left the country
    ## 9  JA_1811_SS.txt    22437           when i next came into the country
    ## 10 JA_1811_SS.txt    26107        a week after his leaving the country
    ##                                         right
    ## 1         which they were to inhabit overcame
    ## 2       beyond the hills which surrounded the
    ## 3                  was always a matter of joy
    ## 4      about them abounded in beautiful walks
    ## 5                 that is good news however i
    ## 6            that he resided there only while
    ## 7  the carriages were then ordered willoughby
    ## 8          his heart seemed more than usually
    ## 9            would be that barton cottage was
    ## 10          marianne was prevailed on to join

``` r
input_dir <- "aeo_corpus" # path to .txt letters' folder
files <- dir(input_dir, "\\.txt") # vector of file names

power <- as.data.frame(text_kwic(files, input_dir, "power", 6))
```

``` r
power[100:110, ]
```

    ##               file position                                left keyword
    ## 100 JA_1814_MP.txt    71277     must be so completely beyond my   power
    ## 101 JA_1814_MP.txt    71984          suppose it would be in her   power
    ## 102 JA_1814_MP.txt    78555    some means of enjoyment in their   power
    ## 103 JA_1814_MP.txt    79031        offer with such means in his   power
    ## 104 JA_1814_MP.txt    83849        he will do everything in his   power
    ## 105 JA_1814_MP.txt    86194      happiness which he felt in his   power
    ## 106 JA_1814_MP.txt    88233    could not but admit the superior   power
    ## 107 JA_1814_MP.txt    94778       it was the advice of absolute   power
    ## 108 JA_1814_MP.txt    97613 hear some pleasant assurance of her   power
    ## 109 JA_1814_MP.txt   100037 have implied the possibility of her   power
    ## 110 JA_1814_MP.txt   102328         not have supposed it in the   power
    ##                                         right
    ## 100        to command miss crawford may chuse
    ## 101             was looking at edmund for his
    ## 102 which no subsequent connexions can supply
    ## 103                      he had a right to be
    ## 104                  to get you made he knows
    ## 105          and the only preparation for the
    ## 106              of one pleasure over his own
    ## 107                  and she had only to rise
    ## 108             from one who she thought must
    ## 109             over my heart ever ceasing my
    ## 110    of any concurrence of circumstances to

``` r
input_dir <- "aeo_corpus" # path to .txt letters' folder
files <- dir(input_dir, "\\.txt") # vector of file names
impropriety <- as.data.frame(text_kwic(files, input_dir, "impropriety", 6))
```

``` r
head(impropriety, n = 10)
```

    ##              file position                                   left
    ## 1  JA_1811_SS.txt    17795   should hold myself guilty of greater
    ## 2  JA_1811_SS.txt    20862             if there had been any real
    ## 3  JA_1811_SS.txt    20938                 are to be the proof of
    ## 4  JA_1811_SS.txt    57589               did not blind her to the
    ## 5  JA_1811_SS.txt   110025    time reflecting on the propriety or
    ## 6  JA_1813_PP.txt    64763          she was _now_ struck with the
    ## 7  JA_1813_PP.txt    65574              both must be hurt by such
    ## 8  JA_1813_PP.txt    74017            had never been blind to the
    ## 9  JA_1813_PP.txt    77639 remembered its warmth and softened its
    ## 10 JA_1813_PP.txt    77928    embarrassment and every idea of the
    ##        keyword                                       right
    ## 1  impropriety                in accepting a horse from my
    ## 2  impropriety                      in what i did i should
    ## 3  impropriety             in conduct we are all offending
    ## 4  impropriety             of their having been written at
    ## 5  impropriety of speedily hazarding her narration without
    ## 6  impropriety        of such communications to a stranger
    ## 7  impropriety        of conduct she felt depressed beyond
    ## 8  impropriety                of her father s behaviour as
    ## 9  impropriety               of expression when all of the
    ## 10 impropriety          of her being found there recurring
