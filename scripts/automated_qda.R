####
# Automated QDA 
#
# ES 200
# https://content-analysis-with-r.com/
###

library(pacman)
p_load(quanteda, readtext, quanteda.textplots, quanteda.textstats, tidyverse, RColorBrewer, here)

# Read in Documents for Analysis
documents <- readtext("data/caf/*") 

# Prune the Name so they are more readable - drops file suffix
documents$doc_id <- str_sub(documents$doc_id, start = 1, end = -5)

# Creates corpus
my.corpus <- corpus(documents)
docvars(my.corpus, "Textno") <- sprintf("%02d", 1:ndoc(my.corpus))

my.corpus

# Basic Stats
my.corpus.stats <- summary(my.corpus)
my.corpus.stats$Text <- reorder(my.corpus.stats$Text, 1:ndoc(my.corpus), order = T)
my.corpus.stats

# Plot Tokens (words) and Types (unique words)
ggplot(my.corpus.stats, aes(Text, Tokens, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Tokens per Document") + xlab("") + ylab("")

ggplot(my.corpus.stats, aes(Text, Types, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Unique Words per Document") + xlab("") + ylab("")
load("dictionaries/policy_agendas_english.RData")


doc.tokens <- tokens(my.corpus) # Creates the tokens for documents
# concordance <- kwic(doc.tokens, "data")

kwic(doc.tokens, "climate")

kwic(doc.tokens, "climate", valuetype = "regex", case_insensitive = FALSE) %>% 
  group_by(docname) %>% summarise(hits = n()) %>% 
  mutate(percentage = hits/(doc.tokens$Tokens/100), searchterm = "climate") %>% 
  arrange(desc(percentage))

kwic(doc.tokens, "climate", valuetype = "regex", case_insensitive = FALSE) %>% group_by(docname) %>% summarise(hits = n())

kwic(tokens(doc.tokens), pattern = "climate") %>%
  textplot_xray()

textplot_xray(
  kwic(doc.tokens, pattern = "communities"),
  kwic(doc.tokens, pattern = "environment"),
  kwic(doc.tokens, pattern = "future")
)

textplot_xray(
  kwic(doc.tokens, pattern = "adaptation"),
  kwic(doc.tokens, pattern = "mitigation")
)


textplot_xray(kwic(my.corpus, "climate", valuetype = "regex", case_insensitive = FALSE)) + 
  ggtitle("Lexical dispersion of \"climate\" in Documents")


# Create Document-Feature Matrix (Engine for)

dfm_my.corpus <- corpus_subset(my.corpus) %>% 
  dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 10, verbose = FALSE)

features_my.corpus <- textstat_frequency(dfm_my.corpus, n = 100)

# Sort by reverse frequency order
features_my.corpus$feature <- with(features_my.corpus, reorder(feature, -frequency))

ggplot(features_my.corpus, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

features_my.corpus <- textstat_frequency(dfm_my.corpus, n = 25)

# Sort by reverse frequency order
features_my.corpus$feature <- with(features_my.corpus, reorder(feature, -frequency))

ggplot(features_my.corpus, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Dictionary - Policy Agendas
# https://www.snsoroka.com/data-lexicoder/ 


dict <- dictionary(file = "data/policy_agendas_english.lcd")
dfm.plans <- dfm(my.corpus, dictionary = dict)
dfm.plans.prop <- dfm_weight(dfm.plans, scheme = "prop")
dfm.plans.prop.lib <- convert(dfm.plans.prop, "data.frame") %>% 
  bind_cols(my.corpus.stats) 


dfm.plans.pa <- convert(dfm.plans, "data.frame") %>%
  rename(State = doc_id) %>%
  select(State, macroeconomics, healthcare, environment, civil_rights, energy, transportation) %>%
  gather(macroeconomics:transportation, key = "Topic", value = "Share") %>% 
  group_by(State) %>% 
  mutate(Share = Share/sum(Share)) %>% 
  mutate(Topic = as_factor(Topic))

ggplot(dfm.plans.pa, aes(State, Share, colour = Topic, fill = Topic)) + 
  geom_bar(stat="identity") + 
  scale_colour_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Pastel1") + 
  ggtitle("Topics in the Climate Plan corpus based on the Policy-Agendas dictionary") +
  xlab("") +
  ylab("Share of topics [%]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(dfm.plans.prop.lib, aes(country, populism)) + geom_boxplot(outlier.size = 0) + geom_jitter(aes(country,populism), position = position_jitter(width = 0.4, height = 0), alpha = 0.1, size = 0.2, show.legend = F) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + xlab("") + ylab("Share of populism [%]") + ggtitle("Share of populismus in speeches within the EU-Speech corpus")