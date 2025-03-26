install.packages(c("quanteda", "devtools", "quanteda.textmodels", "quanteda.corpora", "quanteda.textplots"))
# or if that does not work:
devtools::install_github("quanteda/quanteda.corpora")
install.packages("quanteda.corpora")
library("quanteda.corpora")
install.packages("devtools")


require(quanteda)
require(quanteda.corpora)
require(quanteda.textplots)
require(quanteda.textmodels)
require(tidyverse)

# ---------------------------- #
# Some TaD basics!             #
# Author: Carolina Torreblanca #
# 03/26/2025                   #
# ---------------------------- #

# ---- Working with Quanteda ----- #

# one of the pre-loaded datsaets on SOTU speeches
speeches <- (data_corpus_sotu)
summary(speeches, n = 10) # Corpus consists of 241 documents

# You can filter easily with dplyr-ish syntax
# We can use any of the variables in the metadata to filter
summary(corpus_subset(speeches, President == "Obama")) # 8 documents

# lets create a new dataset with only obama speeches
obama_sotu <- corpus_subset(speeches, President == "Obama")

# you can get the text of each speech
as.character(obama_sotu)[1]

# You can get the individual tokens of each speech
obama_tokens <- tokens(obama_sotu)
obama_tokens[[1]][1:20]
# Notice some tokens are punctuation, others are not very informative

# lets try removing punctuation
obama_tokens <- tokens(obama_sotu, remove_punct = T)
obama_tokens[[1]][1:20]

# better, but still, "Mr", "of" etc. not very informative
# these are stopwords - we will deal with them later
head(stopwords("english"), 15)

# ---- Words in Context ----- #

(obama_kwic <- kwic(tokens(obama_sotu), pattern = "immigration", valuetype = "regex", window = 6))

# ---- Document-term Matrix ----- #

obama_dfm <- dfm(obama_tokens)
head(obama_dfm)
dim(obama_dfm)

# Too many columns! Lets see if removing stopwords can help

obama_dfm_2 <- dfm(obama_tokens, remove = stopwords("english"),
                   remove_punct = T)
dim(obama_dfm_2) # not bad
head(obama_dfm_2)
# we can stem to improve it even more


obama_dfm_3 <- dfm(obama_tokens, remove = stopwords("english"),
                   stem =T,
                   remove_punct = T)
dim(obama_dfm_3) # MUCH MUCH better
head(obama_dfm_3)

# ---- Making a word cloud ----- #

# compare the one with stemming to
textplot_wordcloud(obama_dfm_3, max_words = 100)
# the one without
textplot_wordcloud(obama_dfm_2, max_words = 100)

# ---- Dictionaries ----- #

out_dict <- dictionary(list(
  positive = c("good", "nice", "excellent", "positive", "fortunate",
               "successful", "effective", "efficient", "beneficial", "valuable", "useful",
               "advantageous", "productive", "profitable", "rewarding", "worthwhile",
               "correct", "superior", "happy", "bueno", "prosper"),
  negative = c("bad", "awful", "nasty", "negative", "unfortunate",
               "problematic", "concerning", "troubling", "worrisome", "alarming", "disturbing",
               "harmful", "damaging", "destructive", "ruinous", "catastrophic", "disastrous",
               "wrong", "inferior", "miserable", "terrible", "abusive")))

# Run the conservative manifestos through this dictionary
speeches_dic <- dfm_lookup(dfm(tokens(speeches)), dictionary = out_dict)

# Visualize this
sent_data_out <- convert(speeches_dic, to = "data.frame") %>%
  rowwise() %>%
  mutate(prop_negative = negative / sum(negative, positive) * 100,
         year = abs(parse_number(doc_id)))

ggplot(sent_data_out, aes(x = year, y = prop_negative)) +
  geom_line() +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "% Negative", x = "", title= "Sentiment") +
  theme_classic()

# ---- Text models with Quanteda ----- #

require(quanteda.textmodels)
# Lets take the entire corpus
speeches_dfm <- dfm(tokens(speeches), remove = stopwords("english"),
                    stem =T,
                    remove_punct = T)

# nd = number of semantic dimensions
# lets say 2: economics and social issues
# Note: it is unspurevised but WE have to make a substantive decision
# re the number of semantic dimensions there are

lsa_model <- textmodel_lsa(speeches_dfm, nd = 2)
# you can seee, each doc is assigned a score for 2 dimension
head(lsa_model$docs)
# lets see what words load onto the dimensions
features <- data.frame(feature = rownames(lsa_model$features),
                       dim1 = lsa_model$features[,1],
                       dim2 = lsa_model$features[,2])

pacman::p_load(ggrepel)

ggplot(features, aes(x = dim1, y = dim2)) +
  geom_point() +
  geom_text_repel(data = filter(features, dim1 > .12 | dim2 >.12),
                  aes(label = feature)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(x = "Dimension 1", y = "Dimension 2",
       title = "LSA Feature Loading")

# we can visualize it
docs <- data.frame(doc = rownames(lsa_model$docs),
                   dim1 = lsa_model$docs[,1],
                   dim2 = lsa_model$docs[,2])

# Create plot with ggplot2
ggplot(docs, aes(x = dim1, y = dim2, label = doc)) +
  geom_point() +
  geom_text_repel() +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "red") +
  labs(x = "Dimension 1", y = "Dimension 2",
       title = "LSA Document Map")

# 2 semantic dimensions is probably not great!

