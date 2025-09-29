library(udpipe)

rPath = "/mnt/store/learn/econometrics/code/2-4/"

# Download and load the English model, if not already done
# Uncomment the following line to download the model
# udmodel_english <- udpipe_download_model(language = "english")
# udmodel_english <- udpipe_load_model(udmodel_english$file_model)

# Load the model
udmodel_file <- "english-ewt-ud-2.5-191206.udpipe"
udmodel_english <- udpipe_load_model(udmodel_file)

text = c("The quick brown fox jumps over the lazy dog.",
         "I am learning Natural Language Processing.",
         "Data science is an interdisciplinary field.")

annotated_text <- udpipe_annotate(udmodel_english, x = text)
df_annotated <- as.data.frame(annotated_text)
print(df_annotated$lemma)

rFile = "speech_en.txt"
rFile = paste0(rPath, rFile)

speech = readLines(rFile, encoding = "UTF-8")
head(speech)
annotated_speech <- udpipe_annotate(udmodel_english, x = speech)
df_speech <- as.data.frame(annotated_speech)
print(df_speech$lemma)

rFile = "intel_news.txt"
rFile = paste0(rPath, rFile)

news = readLines(rFile, encoding = "UTF-8")
head(news)
annotated_news <- udpipe_annotate(udmodel_english, x = news)
df_news <- as.data.frame(annotated_news)
print(df_news$lemma)

# Explore the annotated data
str(df_news)
table(df_news$upos)

## Tokenization + finds sentences, does POS tagging and lemmatization but does not execute dependency parsing
x <- udpipe_annotate(udmodel_english, x = news, tagger = "default", parser = "none")
x <- as.data.frame(x)
table(x$upos)
table(x$dep_rel)
