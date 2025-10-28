library(jiebaR)

# rFile = "speech_ch.txt"
rFile = "tsm_news.txt"

doc = readLines(rFile)
# jieba = worker(stop_word = ".txt")
jieba = worker()

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
png("word_frequency_pie.png", width=800, height=600)
pie(Freq, labels = Label, cex=0.5, col=rainbow(length(Label)), main = "词频")
dev.off()
