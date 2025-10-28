# install.packages("devtools")
# devtools::install_github("lchiffon/wordcloud2", force = TRUE)

library(jiebaR)
library(wordcloud2)

rFile = "251025_ptt.txt"
doc = readLines(rFile)
jieba = worker(stop_word = "stop-zh-tw-withpunc.txt")

wordseg = segment(doc, jieba)
head(wordseg, 50) # word segmentation results

# word frequency
TF = freq(wordseg)
# TF[which(TF == "台積電"),] # "台積電" frequency

# order by word frequency
TF.order = TF[order(-TF[,2]), ]
TF.order[1:15,] # Top 15

# word cloud (first 20)
# Opens browser
wordcloud2(TF.order[1:200,])

# Set the cloud to look like a word
# Opens browser and takes time
# Execute in R command line, and after browser opens, refresh
letterCloud(TF.order, word = "TSMC", color = "random-light") 

# Set the cloud to look like a shape
# Opens browser and takes time
# Execute in R command line, and after browser opens, refresh
figure = "taiwan.png"
wordcloud2(TF.order, figPath = figure, size = 1.5, backgroundColor = "white")