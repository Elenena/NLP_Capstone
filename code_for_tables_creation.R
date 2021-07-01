library(dplyr)
library(data.table)
library(tidyr)
dir.create("word_predictor") #App folder where needed data tables, ui.R and server.R will be stored

# Load data in English from corpora
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "training.zip")
unzip("training.zip")
con<-file("final/en_US/en_US.blogs.txt", encoding = "UTF-8")
blogs<-readLines(con)
close(con)
con<-file("final/en_US/en_US.twitter.txt", encoding = "UTF-8")
twitter<-readLines(con)
close(con)
con<-file("final/en_US/en_US.news.txt", "rb", encoding = "UTF-8")
news<-readLines(con)
close(con)

#Conversion of words to uppercase, profanity filtering (list of profane words at
#https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en),
#creation of custom tokens (weblinks, email adresses, numbers, prices, dates)
#and splitting in single words/tokens
# (I've been working with uppercase words in those first steps. Case sensitivity will be handled
#later on in this file)
profane<-readLines("word_predictor/en")
corpora<-toupper(c(blogs, news, twitter))
corpora<-sample(corpora)
bounds<-round(seq(0,length(corpora), len=51))
dir.create("split")
dir.create("Tokenized")
dir.create("WordList")
for (i in 1:50) {
    saveRDS(corpora[(bounds[i]+1):bounds[i+1]], paste0("split/",i,".RDS"))
}
for (i in 1:50) {
    s<-readRDS(paste0("split/",i,".RDS"))
    for(x in profane) {
        s<-gsub(paste0("([^A-Z]|^)",x,"([^A-Z]|$)"), "\\1 <BADWORD> \\2", s, ignore.case = T)
    }
    s<-gsub("HTTP://.+( |$)|WWW..+( |$)"," <WEBLINK> \\1",s)
    s<-gsub("[A-Z0-9]{3,}@[A-Z0-9]{3,}\\.[A-Z0-9]{2,3}", " <MAIL_ADRESS> ", s)
    s<-gsub("[0-9]{2}.[0-9]{2}.[0-9]{4}|[0-9]{4}.[0-9]{2}.[0-9]{2}","<DATE>",s)
    s<-gsub("([€£\\$] *)[0-9]+[//.|,]*[0-9]*", "\\1 <PRICE> ", s)
    s<-gsub("[0-9]+[//.|,]*[0-9]*( *[€£\\$])", " <PRICE> \\1", s)
    s<-gsub("( +|^|-*|\\+*)[0-9]+[//.|,]*[0-9]*( +|$)", "\\1 <NUMBER> \\2", s)
    s<-gsub("[^A-Z0-9<>]+$","",s)
    s<-gsub("^[^A-Z0-9<>]+","",s)
    s<-strsplit(s, "( +[^A-Z0-9<>]*)|([^A-Z0-9<>]* +)|[\\.]{2,}|[-]{2,}|\\\"|[^A-Z0-9]'")
    s<-sapply(s, function(x) x[x !=""])
    saveRDS(s, paste0("Tokenized/chunk",i,".RDS"))
    s<-unlist(s)
    saveRDS(s, paste0("WordList/chunk",i,".RDS"))
}

# Selection of training dataset (chunks 1-35, i.e. 70% of total). Chunks 36-45 will be used in the
#testing process, while chunks 46-50 will never appear in the Github code and were used as a
#validation dataset during the model creation/selection.
temp<-list.files("WordList", full.names = T)
words<-lapply(temp[1:35], readRDS)

#Creation of dictionary that will be used in the conversion of words to integers
#(The definitive dictionary used by the app will be created later in this file)
words<-unlist(words)
one<-as.data.frame(table(words))
one<-one[order(one$Freq, decreasing=T),]
rownames(one)<-1:nrow(one)
write.csv(one, "unigrams.csv")
unigrams <- read.csv("unigrams.csv")
unigrams$cum<-cumsum(unigrams$Freq)
c<-cut(unigrams$cum,50)
levels(c)<-1:50
unigrams$df<-c
unigrams<-select(unigrams, -3, -4)
saveRDS(unigrams, "unigrams.RDS")
dict<-unigrams$X
names(dict)<-unigrams$words
saveRDS(dict, "dict.RDS")

#Converting training dataset's tokens to integers
dict<-readRDS("dict.RDS")
dict<-dict[!duplicated(names(dict))]
to_dict<-function(v) {
    sapply(v, function(x) {
        p<-dicvec[x]
        if(length(p)==0) {0}
        else {p}
    })
}
coded<-list()
temp<-list.files("Tokenized", full.names = T)[1:35]
for (i in 1:length(temp)) {
    chunk<-readRDS(temp[i])
    l<-sapply(chunk, length)
    chunk<-chunk[which(l!=0 & l!=1)]
    u<-unique(unlist(chunk))
    dicvec<-dict[u]
    coded[[i]]<-sapply(chunk, function(x) as.integer(to_dict(x)))
}
saveRDS(coded,"tokcoded.RDS")

#Digrams creation and selection of the most frequent for each first word
dig_gen<-function(v) {
    d<-list()
    for (i in 1:(length(v)-1)) {
        d[[i]]<-v[i:(i+1)]
    }
    d
}
coded<-readRDS("tokcoded.RDS")
digrams<-list()
for (i in 1:35) {
    chunk<-coded[[i]]
    digr<-unlist(sapply(chunk, dig_gen), recursive = F)
    digr<-as.data.frame(do.call(rbind, digr))
    digrams[[i]]<- digr %>% group_by_all() %>% summarize(Frequency=n())
}
digrams<-do.call(rbind,digrams)
digrams<-digrams %>% group_by(V1, V2) %>% summarize(frequency=sum(Frequency))
digrams<- digrams %>% filter(V1!=0 & V2!=0)
digrams<-ungroup(digrams)
digrams<-digrams[order(digrams$frequency, decreasing=T),]
digrams<-digrams %>% group_by(V1) %>% filter(frequency==max(frequency)) %>% select(-3) %>% ungroup()
digrams<-filter(digrams, V2==min(V2))
digrams$best1<-1L
digrams$V1<-as.integer(digrams$V1)
digrams$V2<-as.integer(digrams$V2)
saveRDS(digrams, "digrams.RDS")

#Trigrams creation and selection of the most frequent for each first couple of word
tri_gen<-function(v) {
    d<-list()
    for (i in 1:(length(v)-2)) {
        d[[i]]<-v[i:(i+2)]
    }
    d
}

coded<-readRDS("tokcoded.RDS")
trigrams<-list()
for (i in 1:35) {
    chunk<-coded[[i]]
    chunk<-sapply(chunk, as.integer)
    l<-sapply(chunk, length)
    chunk<-chunk[which(l!=2)]
    tri<-unlist(sapply(chunk, tri_gen), recursive = F)
    tri<-as.data.frame(do.call(rbind, tri))
    trigrams[[i]]<- tri %>% group_by_all() %>% summarize(Frequency=n())
}
saveRDS(trigrams, "tri_unsorted.RDS")
rm(list=ls())
gc()
trigrams<-readRDS("tri_unsorted.RDS")
dir.create("tri_unsorted")
for(i in 1:35) {
    saveRDS(trigrams[[i]], paste0("tri_unsorted/chunk",i,".RDS"))
}
dir.create("five_tri")
temp<-list.files("tri_unsorted", full.names = T)
for(i in c(1,6,11,16,21,26,31)) {
    five<-lapply(temp[i:(i+4)], readRDS)
    five<-do.call(rbind,five)
    five<-five %>% group_by(V1, V2, V3) %>% summarize(frequency=sum(Frequency))
    five<- five %>% filter(V1!=0 & V2!=0 & V3!=0) %>% ungroup()
    five<-five[order(five$frequency, decreasing=T),]
    saveRDS(five, paste0("five_tri/chunk",i,".RDS"))
}
temp<-list.files("five_tri", full.names = T)
trigrams<-lapply(temp, readRDS)
trigrams<-do.call(rbind, trigrams)
trigrams<-trigrams %>% group_by(V1, V2) %>% filter(frequency==max(frequency)) %>% select(-4) %>% ungroup()
trigrams<-filter(trigrams, V3==min(V3))
trigrams$best2<-2L
trigrams<-trigrams[!duplicated(trigrams),]
saveRDS(trigrams, "trigrams.RDS")

#Tetragrams creation and selection of the most frequent for each first 3 words
tetra_gen<-function(v) {
    d<-list()
    for (i in 1:(length(v)-3)) {
        d[[i]]<-v[i:(i+3)]
    }
    d
}
coded<-readRDS("tokcoded.RDS")
tetragrams<-list()
for (i in 1:35) {
    chunk<-coded[[i]]
    chunk<-sapply(chunk, as.integer)
    l<-sapply(chunk, length)
    chunk<-chunk[which(l>3)]
    tet<-unlist(sapply(chunk, tetra_gen), recursive = F)
    tet<-as.data.frame(do.call(rbind, tet))
    tetragrams[[i]]<- tet %>% group_by_all() %>% summarize(Frequency=n()) %>% ungroup()
}
saveRDS(tetragrams, "tet_unsorted.RDS")
tetragrams<-readRDS("tet_unsorted.RDS")
dir.create("tet_unsorted")
for(i in 1:35) {
    saveRDS(tetragrams[[i]], paste0("tet_unsorted/chunk",i,".RDS"))
}
dir.create("five_tet")
temp<-list.files("tet_unsorted", full.names = T)
for(i in c(1,8,15,22,29)) {
    five<-lapply(temp[i:(i+6)], readRDS)
    five<-do.call(rbind,five)
    five<-five %>% group_by(V1, V2, V3, V4) %>% summarize(frequency=sum(Frequency)) %>% ungroup()
    five<- five %>% filter(V1!=0 & V2!=0 & V3!=0 & V4!=0)
    five<-five[order(five$frequency, decreasing=T),]
    saveRDS(five, paste0("five_tet/chunk",i,".RDS"))
}
temp<-list.files("five_tet", full.names = T)
tetragrams<-readRDS(temp[[1]])
for(i in 2:5) {
    tetragrams<-full_join(tetragrams,readRDS(temp[[i]]), by=c("V1"="V1", "V2"="V2", "V3"="V3", "V4"="V4"))
    tetragrams<-replace_na(tetragrams, list(frequency.x=0, frequency.y=0))
    tetragrams<-tetragrams %>% mutate(frequency=frequency.x+frequency.y) %>% select(-5, -6)
}
saveRDS(tetragrams, "tetragrams.RDS")
tetragrams<-readRDS("tetragrams.RDS")
number<-tetragrams %>% group_by(V1) %>% summarize(n=n())
number$cum<-cumsum(number$n)
number$c<-cut(number$cum,10)
levels(number$c)<-1:10
number<-select(number, V1, c)
tetragrams<-left_join(tetragrams, number)
tetragrams<-group_split(tetragrams, c, .keep = F)
dir.create("tetragrams_split")
for (i in 1:10) {
    saveRDS(tetragrams[[i]], paste0("tetragrams_split/chunk",i,".RDS"))
}

temp<-list.files("tetragrams_split", full.names = T)
tetragrams<-list()
for (i in 1:10) {
    tet<-readRDS(temp[[i]])
    tet<-tet %>% group_by(V1, V2, V3) %>% filter(frequency==max(frequency)) %>%
        select(-5) %>% filter(V4==min(V4))%>% ungroup()
    tetragrams[[i]]<-tet[!duplicated(tet),]
}
tetragrams<-do.call(rbind, tetragrams)
saveRDS(tetragrams, "tetragrams.RDS")

#Pentagrams creation and selection of the most frequent for each first 4 words
penta_gen<-function(v) {
    d<-list()
    for (i in 1:(length(v)-4)) {
        d[[i]]<-v[i:(i+4)]
    }
    d
}
coded<-readRDS("tokcoded.RDS")
pentagrams<-list()
for (i in 1:35) {
    chunk<-coded[[i]]
    chunk<-sapply(chunk, as.integer)
    l<-sapply(chunk, length)
    chunk<-chunk[which(l>4)]
    pen<-unlist(sapply(chunk, penta_gen), recursive = F)
    pen<-as.data.frame(do.call(rbind, pen))
    pentagrams[[i]]<- pen %>% group_by_all() %>% summarize(Frequency=n()) %>% ungroup()
}
saveRDS(pentagrams, "pen_unsorted.RDS")
pentagrams<-readRDS("pen_unsorted.RDS")
dir.create("pen_unsorted")
for(i in 1:35) {
    saveRDS(pentagrams[[i]], paste0("pen_unsorted/chunk",i,".RDS"))
}
dir.create("five_pen")
temp<-list.files("pen_unsorted", full.names = T)
for(i in c(1,8,15,22,29)) {
    five<-lapply(temp[i:(i+6)], readRDS)
    five<-do.call(rbind,five)
    five<-five %>% group_by(V1, V2, V3, V4, V5) %>% summarize(frequency=sum(Frequency)) %>% ungroup()
    five<- five %>% filter(V1!=0 & V2!=0 & V3!=0 & V4!=0 & V5!=0)
    five<-five[order(five$frequency, decreasing=T),]
    saveRDS(five, paste0("five_pen/chunk",i,".RDS"))
}
temp<-list.files("five_pen", full.names = T)
pentagrams<-readRDS(temp[[1]])
for(i in 2:5) {
    pentagrams<-full_join(pentagrams,readRDS(temp[[i]]), by=c("V1"="V1", "V2"="V2", "V3"="V3", "V4"="V4", "V5"="V5"))
    pentagrams<-replace_na(pentagrams, list(frequency.x=0, frequency.y=0))
    pentagrams<-pentagrams %>% mutate(frequency=frequency.x+frequency.y) %>% select(-6, -7)
}
saveRDS(pentagrams, "pentagrams.RDS")
pentagrams<-readRDS("pentagrams.RDS")
number<-pentagrams %>% group_by(V1) %>% summarize(n=n())
number$cum<-cumsum(number$n)
number$c<-cut(number$cum,10)
levels(number$c)<-1:10
number<-select(number, V1, c)
pentagrams<-left_join(pentagrams, number)
pentagrams<-group_split(pentagrams, c, .keep = F)
dir.create("pentagrams_split")
for (i in 1:10) {
    saveRDS(pentagrams[[i]], paste0("pentagrams_split/chunk",i,".RDS"))
}
temp<-list.files("pentagrams_split", full.names = T)
pentagrams<-list()
for (i in 1:10) {
    pen<-readRDS(temp[[i]])
    pen<-pen %>% group_by(V1, V2, V3, V4) %>% filter(frequency==max(frequency)) %>%
        select(-6) %>% filter(V5==min(V5))%>% ungroup()
    pentagrams[[i]]<-pen[!duplicated(pen),]
}
pentagrams<-do.call(rbind, pentagrams)
saveRDS(pentagrams, "pentagrams.RDS")

#Creation of the df for prediction with 4 words
pentagrams <- readRDS("pentagrams.RDS")
tetragrams <- readRDS("tetragrams.RDS")
tetragrams$best3<-3L
trigrams <- readRDS("trigrams.RDS")
pentet<-full_join(pentagrams, tetragrams)
pentettrig<-full_join(pentet, trigrams)
digrams <- readRDS("digrams.RDS")
df4<-full_join(pentettrig, digrams)
df4<-replace_na(df4, list(best1=0, best2=0, best3=0))
saveRDS(df4, "df4.RDS")
df4<-readRDS("df4.RDS")
df4<-replace_na(df4, list(best1=0, best2=0, best3=0))
df4<-df4 %>% mutate(best=as.integer(paste0(best1, best2, best3)))
df4<- df4 %>% select(-6,-7,-8)
df4<-replace_na(df4, list(V1=0, V2=0, V3=0, V4=0, V5=0))
class(df4$V1)<-"integer"
class(df4$V2)<-"integer"
class(df4$V3)<-"integer"
class(df4$V4)<-"integer"
class(df4$V5)<-"integer"
saveRDS(df4, "df4.RDS")

#Creating the  first 25 indexed data tables that will be used by the prediction function
#(one for each of the most frequent 25 tokens)
dir.create("word_predictor/dt")
df4 <- readRDS("df4.RDS")
df4<- df4 %>% filter(V1<=26)
df4<- df4 %>% group_by(V1)
singles<- df4 %>% group_split(.keep=F)
keys<- df4 %>% group_keys()
names(singles)<-keys$V1
sapply(seq_along(singles), function(x) {dt<-as.data.table(singles[[x]])
setkey(dt, V2, V3, V4, best)
saveRDS(dt, file=paste0("word_predictor/dt/",names(singles[x]), ".RDS"))
})

#Creating 200 indexed datatables, also to be used by the prediction function,
# with all the remaining data. Also creating a file "number.RDS" that will be used by the app
#to find the appropriate data table to use based on input words.
df4 <- readRDS("df4.RDS")
number<-df4 %>% group_by(V1) %>% summarize(n=n())
number$cum<-cumsum(number$n)
number<-filter(number, V1>25)
df4<-filter(df4, V1>25)
number$c<-cut(number$cum,200)
levels(number$c)<-26:226
number<-select(number, V1, c)
number<-rbind(number, data.frame(V1=1:25, c=NA))
number<-as.data.table(number)
setkey(number, V1)
saveRDS(number, "word_predictor/number.RDS")
df4<-left_join(df4, number)
df4<-group_split(df4, c, .keep = F)
for(i in 1:200) {dt<-as.data.table(df4[[i]])
setkey(dt, V1, V2, V3, V4, best)
saveRDS(dt, paste0("word_predictor/dt/",i+25, ".RDS"))
}

#Determining which are the most frequent number, price, weblink etc...to be shown when one of my
#custom tokens is predicted (for <BADWORD>, I'll leave <BADWORD> displayed).
#NOTE: Due to my error (forgot to set seed at the very beginning of the file, before corpora
#sampling) this step and the next (upper/lowercase selection) will be performed on the entire dataset. 
#Actually, this selection on custom tokens could have been performed only on the training dataset,
#but I designed the 2 steps as a single process.
dir.create("detokenization")
con<-file("final/en_US/en_US.blogs.txt", encoding = "UTF-8")
blogs<-readLines(con)
close(con)
con<-file("final/en_US/en_US.twitter.txt", encoding = "UTF-8")
twitter<-readLines(con)
close(con)
con<-file("final/en_US/en_US.news.txt", "rb", encoding = "UTF-8")
news<-readLines(con)
close(con)
corpora<-c(blogs, news, twitter)
set.seed(2410)
corpora<-sample(corpora)
bounds<-round(seq(0,length(corpora), len=51))
dir.create("detokenization/split")
dir.create("detokenization/Tokenized")
dir.create("detokenization/WordList")
for (i in 1:50) {
    saveRDS(corpora[(bounds[i]+1):bounds[(i+1)]], paste0("detokenization/split/",i,".RDS"))
}
for (i in 1:50) {
    s<-readRDS(paste0("split/",i,".RDS"))
    s<-strsplit(s, "( +[^A-Za-z0-9]*)|([^A-Za-z0-9]* +)|[\\.]{2,}|[-]{2,}|\\\"|[^A-Za-z0-9]'")
    s<-sapply(s, function(x) x[x !=""])
    saveRDS(s, paste0("detokenization/Tokenized/chunk",i,".RDS"))
    s<-sapply(s, function(x) x[2:length(x)])
    s<-unlist(s)
    saveRDS(s, paste0("detokenization/WordList/chunk",i,".RDS"))
}
numbers<-list()
weblink<-list()
mail<-list()
date<-list()
price1<-list()
price2<-list()
for(i in 1:50) {
    chunk<-readRDS(paste0("detokenization/Tokenized/chunk",i,".RDS"))
    numbers[[i]]<-unlist(sapply(chunk, function(x)
        grep("( +|^|-*|\\+*)[0-9]+[//.|,]*[0-9]*( +|$)", x, value=T)))
    weblink[[i]]<-unlist(sapply(chunk, function(x)
        grep("HTTP://.+( |$)|WWW..+( |$)", x, value=T, ignore.case = T)))
    mail[[i]]<-unlist(sapply(chunk, function(x)
        grep("[A-Z0-9]{3,}@[A-Z0-9]{3,}\\.[A-Z0-9]{2,3}", x, value=T, ignore.case = T)))
    date[[i]]<-unlist(sapply(chunk, function(x)
        grep("[0-9]{2}.[0-9]{2}.[0-9]{4}|[0-9]{4}.[0-9]{2}.[0-9]{2}", x, value=T, ignore.case = T)))
    price1[[i]]<-unlist(sapply(chunk, function(x)
        grep("([€£\\$] *)[0-9]+[//.|,]*[0-9]*", x, value=T, ignore.case = T)))
    price2[[i]]<-unlist(sapply(chunk, function(x)
        grep("[0-9]+[//.|,]*[0-9]*( *[€£\\$])", x, value=T, ignore.case = T)))
}
numbers<-unlist(numbers)
numbers<-as.data.frame(table(numbers))
filter(numbers, Freq==max(Freq)) %>% select(numbers) #MOST FREQUENT NUMBER IS "2"
weblink<-unlist(weblink)
weblink<-as.data.frame(table(weblink))
filter(weblink, Freq==max(Freq)) %>% select(weblink) #MOST FREQUENT weblink IS "www.RadioTAGr.com/WFUV"
mail<-unlist(mail)
mail<-as.data.frame(table(mail))
filter(mail, Freq==max(Freq)) %>% select(mail) # most frequent email adress is khasheck@gmail.com
date<-unlist(date)
date<-as.data.frame(table(date))
filter(date, Freq==max(Freq)) %>% select(date) #most frequent date is 03/17/2011
#to change in Sys.date() after testing
price1<-unlist(price1)
price1<-as.data.frame(table(price1))
price2<-unlist(price2)
price2<-as.data.frame(table(price2))
names(price1)<-c("price", "Freq")
names(price2)<-c("price", "Freq")
price<-rbind(price1, price2)
filter(price, Freq==max(Freq)) %>% select(price) #most frequent price is $5

#Choice the best upper/lower case format for each word (see NOTE at line 369)
#and creation of the definitive dictionary used by the Shiny app
temp<-list.files("detokenization/WordList", full.names = T)
words<-lapply(temp, readRDS)
words<-unlist(words)
words<-as.data.frame(table(words))
write.csv(words, "detokenization/words.csv")
words<-read.csv("detokenization/words.csv")
words$capital<-toupper(words$words)
dict <- readRDS("dict.RDS")
words<-filter(words, capital %in% names(dict))
words<-group_by(words, capital) %>% filter(Freq==max(Freq)) %>% ungroup()
words<-words[!duplicated(words$capital),]
dict<-data.table(capital=names(dict), code=dict)
dict<-left_join(dict, words)
index<-which(is.na(dict$words))
for(i in index) {
    dict[i,]<-replace_na(dict[i,], list(words=tolower(dict[i,]$capital)))
}
dict<- select(dict, code, words, capital)
setkey(dict, code, capital)
dict<-dict[!duplicated(dict$words),]
saveRDS(dict, "word_predictor/dict.RDS")

#Creating prediction based on 1 previous word for German and Finnish languages. For German, words
# making up the 90% of my sample were used. For Finnish, this percentage was reduced to 80%.
#I used a sample of 200000 sentences for each language.
#The final "foreign.RDS" will be used by the application.
dir.create("foreign")
#German
con<-file("final/de_DE/de_DE.blogs.txt", encoding = "UTF-8")
blogs<-readLines(con)
close(con)
con<-file("final/de_DE/de_DE.twitter.txt", encoding = "UTF-8")
twitter<-readLines(con)
close(con)
con<-file("final/de_DE/de_DE.news.txt", "rb", encoding = "UTF-8")
news<-readLines(con)
close(con)
corpora<-c(blogs, news, twitter)
set.seed(2410)
corpora<-sample(corpora, 200000)
s<-gsub("HTTP://.+( |$)|WWW..+( |$)"," <WEBLINK> \\1",corpora, ignore.case = T)
s<-gsub("[A-Z0-9]{3,}@[A-Z0-9]{3,}\\.[A-Z0-9]{2,3}", " <MAIL_ADRESS> ", s, ignore.case = T)
s<-gsub("[0-9]{2}.[0-9]{2}.[0-9]{4}|[0-9]{4}.[0-9]{2}.[0-9]{2}","<DATE>",s)
s<-gsub("([€£\\$] *)[0-9]+[//.|,]*[0-9]*", "\\1 <PRICE> ", s)
s<-gsub("[0-9]+[//.|,]*[0-9]*( *[€£\\$])", " <PRICE> \\1", s)
s<-gsub("( +|^|-*|\\+*)[0-9]+[//.|,]*[0-9]*( +|$)", "\\1 <NUMBER> \\2", s)
s<-gsub("[^A-Z0-9<>]+$","",s, ignore.case = T)
s<-gsub("^[^A-Z0-9<>]+","",s, ignore.case = T)

s<-strsplit(s, "( +[^A-Za-z0-9<>]*)|([^A-Za-z0-9<>]* +)|[\\.]{2,}|[-]{2,}|\\\"|[^A-Za-z0-9]'")
s<-sapply(s, function(x) x[x !=""])
saveRDS(s, "foreign/german_tokens.RDS")
s<-unlist(s)
saveRDS(s, "foreign/german_words.RDS")
one<-as.data.frame(table(s))
one<-one[order(one$Freq, decreasing=T),]
rownames(one)<-1:nrow(one)
write.csv(one, "foreign/german_unigrams.csv")

unigrams<-read.csv("foreign/german_unigrams.csv")
unigrams$Cumulative_frequency_Percent<-round(cumsum(unigrams$Freq/sum(unigrams$Freq)*100),2)
top90<-unigrams[unigrams$Cumulative_frequency_Percent<=90,]
top90$capital<-toupper(top90$s)
top90<-group_by(top90, capital) %>% filter(Freq==max(Freq)) %>% ungroup()
top90<-top90[!duplicated(top90$capital),]
dict <- readRDS("word_predictor/dict.RDS")
top90<- top90 %>% filter(!(capital %in% dict$capital))
german<-top90$s
saveRDS(german, "foreign/german_dict.RDS")
digrams_gen<-function(v) {
    if (length(v)>1) {
        d<-list()
        for (i in 1:(length(v)-1)) {
            d[[i]]<-v[i:(i+1)]
        }
        d
    }
}
tokenized <- readRDS("foreign/german_tokens.RDS")
digrams<-unlist(sapply(tokenized, digrams_gen), recursive = F)
digrams<-as.data.frame(do.call(rbind, digrams))
digrams<-digrams %>% group_by(V1) %>% filter(Frequency==max(Frequency)) %>% select(-3) %>% ungroup()
digrams<-digrams[!duplicated(digrams$V1),]
digrams<-as.data.table(digrams)
setkey(digrams, V1)
digrams<-digrams[V1 %in% german]
digrams$V1<-toupper(digrams$V1)
saveRDS(digrams, "foreign/german_digrams.RDS")

#Finnish
con<-file("final/fi_FI/fi_FI.blogs.txt", encoding = "UTF-8")
blogs<-readLines(con)
close(con)
con<-file("final/fi_FI/fi_FI.twitter.txt", encoding = "UTF-8")
twitter<-readLines(con)
close(con)
con<-file("final/fi_FI/fi_FI.news.txt", "rb", encoding = "UTF-8")
news<-readLines(con)
close(con)
corpora<-c(blogs, news, twitter)
set.seed(2410)
corpora<-sample(corpora, 200000)
s<-gsub("HTTP://.+( |$)|WWW..+( |$)"," <WEBLINK> \\1",corpora, ignore.case = T)
s<-gsub("[A-Z0-9]{3,}@[A-Z0-9]{3,}\\.[A-Z0-9]{2,3}", " <MAIL_ADRESS> ", s, ignore.case = T)
s<-gsub("[0-9]{2}.[0-9]{2}.[0-9]{4}|[0-9]{4}.[0-9]{2}.[0-9]{2}","<DATE>",s)
s<-gsub("([€£\\$] *)[0-9]+[//.|,]*[0-9]*", "\\1 <PRICE> ", s)
s<-gsub("[0-9]+[//.|,]*[0-9]*( *[€£\\$])", " <PRICE> \\1", s)
s<-gsub("( +|^|-*|\\+*)[0-9]+[//.|,]*[0-9]*( +|$)", "\\1 <NUMBER> \\2", s)
s<-gsub("[^A-Z0-9<>]+$","",s, ignore.case = T)
s<-gsub("^[^A-Z0-9<>]+","",s, ignore.case = T)
s<-strsplit(s, "( +[^A-Za-z0-9<>]*)|([^A-Za-z0-9<>]* +)|[\\.]{2,}|[-]{2,}|\\\"|[^A-Za-z0-9]'")
s<-sapply(s, function(x) x[x !=""])
saveRDS(s, "foreign/finnish_tokens.RDS")
s<-unlist(s)
saveRDS(s, "foreign/finnish_words.RDS")
one<-as.data.frame(table(s))
one<-one[order(one$Freq, decreasing=T),]
rownames(one)<-1:nrow(one)
write.csv(one, "foreign/finnish_unigrams.csv")
unigrams<-read.csv("foreign/finnish_unigrams.csv")
unigrams$Cumulative_frequency_Percent<-round(cumsum(unigrams$Freq/sum(unigrams$Freq)*100),2)
top80<-unigrams[unigrams$Cumulative_frequency_Percent<=80,]
top80$capital<-toupper(top80$s)
top80<-group_by(top80, capital) %>% filter(Freq==max(Freq)) %>% ungroup()
top80<-top80[!duplicated(top80$capital),]
dict <- readRDS("word_predictor/dict.RDS")
top80<- top80 %>% filter(!(capital %in% dict$capital))
german<-readRDS("foreign/german_dict.RDS")
top80<- top80 %>% filter(!(capital %in% toupper(german)))
finnish<-top80$s
saveRDS(finnish, "foreign/finnish_dict.RDS")
digrams_gen<-function(v) {
    if (length(v)>1) {
        d<-list()
        for (i in 1:(length(v)-1)) {
            d[[i]]<-v[i:(i+1)]
        }
        d
    }
}
tokenized <- readRDS("foreign/finnish_tokens.RDS")
digrams<-unlist(sapply(tokenized, digrams_gen), recursive = F)
digrams<-as.data.frame(do.call(rbind, digrams))
digrams<-digrams %>% group_by(V1) %>% filter(Frequency==max(Frequency)) %>% select(-3) %>% ungroup()
digrams<-digrams[!duplicated(digrams$V1),]
digrams<-as.data.table(digrams)
setkey(digrams, V1)
digrams<-digrams[V1 %in% finnish]
digrams$V1<-toupper(digrams$V1)
saveRDS(digrams, "foreign/finnish_digrams.RDS")

german_digrams <- readRDS("foreign/german_digrams.RDS")
finnish_digrams <- readRDS("foreign/finnish_digrams.RDS")
foreign<-rbind(german_digrams, finnish_digrams)
setkey(foreign, V1)
saveRDS(foreign, "word_predictor/foreign.RDS")

file.copy("en", "word_predictor")