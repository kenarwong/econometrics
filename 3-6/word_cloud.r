# install.packages("devtools")
# devtools::install_github("lchiffon/wordcloud2", force = TRUE)

library(jiebaR)
library(wordcloud2)

rFile = "251025_ptt.txt"
rStopWordFile = "stop-zh-tw-withpunc.txt"
rPath = "/mnt/store/learn/econometrics/code/3-6"
rFilepath = file.path(rPath, rFile)
doc = readLines(rFilepath)
jieba = worker(stop_word = file.path(rPath, rStopWordFile)) 

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
figPath = file.path(rPath, figure)
wordcloud2(TF.order, figPath = figPath, size = 1.5, backgroundColor = "white")