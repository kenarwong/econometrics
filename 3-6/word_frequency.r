library(jiebaR)

# rFile = "speech_ch.txt"
rFile = "tsm_news.txt"
rStopWordFile = "stop-zh-tw-withpunc.txt"
rPath = "/mnt/store/learn/econometrics/code/3-6"
rFilepath = file.path(rPath, rFile)

doc = readLines(rFilepath)
# jieba = worker(stop_word = ".txt")
jieba = worker(stop_word = file.path(rPath, rStopWordFile)) 

wordseg = segment(doc, jieba)
head(wordseg, 50) # word segmentation results

# word frequency
TF = freq(wordseg)
TF[which(TF == "台積電"),] # "台積電" frequency

# pie chart
# order by word frequency
TF.order = TF[order(-TF[,2]), ]
TF.order[1:15,] # Top 15

Freq = TF.order[1:20, 2]
Label = paste(TF.order[1:20, 1], Freq)

pie(Freq, labels = Label, cex=0.5, col=rainbow(length(Label)), main = "词频")

# save to file
png(file.path(rPath, "word_frequency_pie.png"), width=800, height=600)
pie(Freq, labels = Label, cex=0.5, col=rainbow(length(Label)), main = "词频")
dev.off()
