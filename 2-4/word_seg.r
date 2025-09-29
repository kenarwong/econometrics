library(jiebaR)

Jieba = worker()
chinese = c("衍生性金融商品与期货选择权，金融衍生品与期权。")
head(chinese)
segment(chinese, Jieba)

rPath = "/mnt/store/learn/econometrics/code/2-4/"
rFile = "speech_ch.txt"
rFile = paste0(rPath, rFile)

speech = readLines(rFile, encoding = "UTF-8")
head(speech)

segment(speech, Jieba)

rFile = "tsm_news.txt"
rFile = paste0(rPath, rFile)

news = readLines(rFile, encoding = "UTF-8")
head(news)

segment(news, Jieba)