#Preparing sentences in the test dataset to be passed to the prediction function
#for testing on local machine (removing the last word of each sentence and saving
#the first part and the last word in the rows of a dataframe)

sen<-list()
for(i in 36:45) {
    sen[[i-35]]<-unlist(readRDS(paste0("split/",i, ".RDS")))
}
test_sentences<-unlist(sen)
test_sentences<-gsub("[^A-Z0-9]+$", "", test_sentences)
s<-strsplit(test_sentences,
            "( +[^A-Z0-9<>]*)|([^A-Z0-9<>]* +)|[\\.]{2,}|[-]{2,}|\\\"|[^A-Z0-9<>]'")
s<-sapply(s, function(x) x[length(x)])
empty<-sapply(s, function(x) which(length(x)==0))
empty<-which(empty==1)
s<-unlist(s)
test_sentences<-test_sentences[-(empty)]
begin<-character()
for(i in 1:length(test_sentences)) {
    begin[i]<-substr(test_sentences[i], 1, (nchar(test_sentences[i])-nchar(s[i])))
}
test_df<-data.frame(sentence=begin, correct=s)
saveRDS(test_df, "test_df.RDS")

# Prediction function (copy-pasted from server.UI, except for the predicted date that is the most
#frequent found in the corpora, while in the Shiny app it's system date)
library(data.table)

dict <- readRDS("word_predictor/dict.RDS")
number <- readRDS("word_predictor/number.RDS")
profane<-readLines("word_predictor/en")
foreign<-readRDS("word_predictor/foreign.RDS")
loaded<-list()
for(i in 1:25) {
    loaded[[i]]<-readRDS(paste0("word_predictor/dt/",i,".RDS"))
}

word_predict<-function(sen) {
    s<-tokenizer(sen)
    v1<-dict[capital==s[1], code]
    if(length(v1)==0) v1<-NA
    v2<-dict[capital==s[2], code]
    if(length(v2)==0) v2<-NA
    v3<-dict[capital==s[3], code]
    if(length(v3)==0) v3<-NA
    v4<-dict[capital==s[4], code]
    if(length(v4)==0) {if(length(foreign[V1==s[4],V2])!=0) {return(afterPeriod(foreign[V1==s[4],V2],sen))
    } else v4<-NA}
    w<-integer()
    if(v1 %in% number$V1 && v2 %in% number$V1 && v3 %in% number$V1 && v4 %in% number$V1) {
        if(v1<26) {w<-loaded[[v1]][V2==v2 & V3==v3 & V4==v4, V5]
        } else {m<-number[V1==v1,c]
        m<-readRDS(paste0("dt/",m,".RDS"))
        w<-m[V1==v1 & V2==v2 & V3==v3 & V4==v4, V5]}
        if(length(w)==0) w<-0
        if(w==0) {
            if(v2<26) {w<-unique(loaded[[v2]][V2==v3 & V3==v4 & best %in% c(23,3,103,123), V4])
            } else {m<-number[V1==v2,c]
            m<-readRDS(paste0("dt/",m,".RDS"))
            w<-unique(m[V1==v2 & V2==v3 & V3==v4 & best %in% c(23,3,103,123), V4])}
        }
        if(length(w)==0) w<-0
        if(w==0) {
            if(v3<26) {w<-unique(loaded[[v3]][V2==v4 & best %in% c(23,20,123,120), V3])
            } else{m<-number[V1==v3,c]
            m<-readRDS(paste0("dt/",m,".RDS"))
            w<-unique(m[V1==v3 & V2==v4 & best %in% c(23,20,123,120), V3])}
        }
        if(length(w)==0) w<-0
        if(w==0){
            if(v4<26) {w<-unique(loaded[[v4]][best %in% c(103, 123, 100, 120), V2])
            } else {m<-number[V1==v4,c]
            m<-readRDS(paste0("dt/",m,".RDS"))
            w<-unique(m[V1==v4 & best %in% c(103, 123, 100, 120), V2])}
        }
        if(length(w)==0) w<-0
        if(w==0) return(afterPeriod("the", sen))
    }
    else if(v2 %in% number$V1 && v3 %in% number$V1 && v4 %in% number$V1) {
        if(v2<26) {w<-unique(loaded[[v2]][V2==v3 & V3==v4 & best %in% c(23,3,103,123), V4])
        } else{m<-number[V1==v2,c]
        m<-readRDS(paste0("dt/",m,".RDS"))
        w<-unique(m[V1==v2 & V2==v3 & V3==v4 & best %in% c(23,3,103,123), V4])}
        if(length(w)==0) w<-0
        if(w==0) {
            if(v3<26) {w<-unique(loaded[[v3]][V2==v4 & best %in% c(23,20,123,120), V3])
            } else {m<-number[V1==v3,c]
            m<-readRDS(paste0("dt/",m,".RDS"))
            w<-unique(m[V1==v3 & V2==v4 & best %in% c(23,20,123,120), V3])}
        }
        if(length(w)==0) w<-0
        if(w==0) {
            if(v4<26) {w<-unique(loaded[[v4]][best %in% c(103, 123, 100, 120), V2])
            } else {m<-number[V1==v4,c]
            m<-readRDS(paste0("dt/",m,".RDS"))
            w<-unique(m[V1==v4 & best %in% c(103, 123, 100, 120), V2])}
            if(length(w)==0) w<-0
            if(w==0) return(afterPeriod("the", sen))
        }
    }
    else if(v3 %in% number$V1 && v4 %in% number$V1) {
        if(v3<26) {w<-unique(loaded[[v3]][V2==v4 & best %in% c(23,20,123,120), V3])
        } else {m<-number[V1==v3,c]
        m<-readRDS(paste0("dt/",m,".RDS"))
        w<-unique(m[V1==v3 & V2==v4 & best %in% c(23,20,123,120), V3])}
        if(length(w)==0) w<-0
        if(w==0) {
            if(v4<26) {w<-unique(loaded[[v4]][best %in% c(103, 123, 100, 120), V2])
            } else {m<-number[V1==v4,c]
            m<-readRDS(paste0("dt/",m,".RDS"))
            w<-unique(m[V1==v4 & best %in% c(103, 123, 100, 120), V2])}
        }
        if(length(w)==0) w<-0
        if(w==0) return(afterPeriod("the", sen))
    }
    else if(v4 %in% number$V1) {
        if(v4<26) {w<-unique(loaded[[v4]][best %in% c(103, 123, 100, 120), V2])
        } else {m<-number[V1==v4,c]
        m<-readRDS(paste0("dt/",m,".RDS"))
        w<-unique(m[V1==v4 & best %in% c(103, 123, 100, 120), V2])}
        if(length(w)==0) w<-0
        if(w==0) return(afterPeriod("the", sen))
    }
    else return(afterPeriod("the", sen)) 
    afterPeriod(decoder(w),sen)
}

tokenizer<-function(s) {
    if(nchar(s)>200) s<-substr(s, (nchar(s)-200), nchar(s))
    s<-toupper(s)
    a<-" "
    b<-" "
    c<-" "
    d<-" "
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
    s<-unlist(strsplit(s, "( +[^A-Z0-9<>]*)|([^A-Z0-9<>]* +)|[\\.]{2,}|[-]{2,}|\\\"|[^A-Z0-9<>]'"))
    if(length(s)>3) a<-s[length(s)-3]
    if(length(s)>2) b<-s[length(s)-2]
    if(length(s)>1) c<-s[length(s)-1]
    if(length(s)>0) d<-s[length(s)]
    c(a,b,c,d)
}

decoder<-function(w) {
    if(w==11) return("2")
    if(w==524) return("www.RadioTAGr.com/WFUV")
    if(w==3339) return("khasheck@gmail.com")
    if(w==3972) return("03/17/2011")
    if(w==79) return("$5")
    dict[code==w, words]
}

afterPeriod<- function(x, previous) {
    previous<-substr(previous, (nchar(previous)-1), nchar(previous))
    if(previous %in% c(':\"', ". ", "? ", "! ")) return(paste0(toupper(substring(x, 1,1)), substring(x, 2)))
    previous<-substr(previous, (nchar(previous)), nchar(previous))
    if(previous %in% c(".", "?", "!")) {
        return(paste0(" ",toupper(substring(x, 1,1)), substring(x, 2)))
    }
    x
}

#Running test and saving results. Accuracy is case-insensitive
test<-readRDS("test_df.RDS")
a<-Sys.time()
for(i in 1:nrow(test)) {
    test$prediction[i]<-word_predict(test$sentence[i])
}
b<-Sys.time()
test$Correct_prediction<-test$correct==toupper(test$prediction)
round(sum(test$Correct_prediction)/nrow(test)*100,2) #Accuracy %
round(difftime(b,a, units="sec")/nrow(test),4) # Mean time for prediction
saveRDS(test,"test_results4sen2.RDS")
saveRDS(a, "start_time_test2.RDS")
saveRDS(b, "stop_time_test2.RDS")

# accuracy 46.52%
# Time difference of 0.0982 secs

#Some exploration of test results
c<-as.data.frame(table(test$correct))
c<-c[order(c$Freq, decreasing = T),]
p<-as.data.frame(table(test$prediction))
p<-p[order(p$Freq, decreasing = T),]
names(c)<-c("Correct word", "Correct word Frequency")
names(p)<-c("Predicted word", "Predicted word Frequency")
length(unique(c$`Correct word`)) # 82059 different words
sum(unique(c$`Correct word`) %in% dict$capital) # of which 67149 are in my english dictionary (81.83%)
sum(unique(c$`Correct word`) %in% toupper(foreign$V2)) #and 1189 (1.45%) are in foreign dictionary
length(unique(p$`Predicted word`)) # 50062 different words
t<-cbind(c[1:100,], p[1:100,])
saveRDS(t, "prediction_frequencies_top100.RDS")

tokenized_test_df<-list()
for(i in 36:45) {
    tokenized_test_df[[i-35]]<-readRDS(paste0("Tokenized/chunk",i,".RDS"))
}
tokenized_test_df<-unlist(tokenized_test_df, recursive = F)
l<-sapply(tokenized_test_df, length)
tokenized_test_df<-tokenized_test_df[l>0]
l<-sapply(tokenized_test_df, length)
pred1word<-test[l==2,]
sum(pred1word$Correct_prediction)/nrow(pred1word)*100 #Accuracy for prediction with 1 word= 10.48%
pred2word<-test[l==3,]
sum(pred2word$Correct_prediction)/nrow(pred2word)*100 #Accuracy for prediction with 2 words= 22.66%
pred3word<-test[l==4,]
sum(pred3word$Correct_prediction)/nrow(pred3word)*100 #Accuracy for prediction with 3 words= 35.87%
pred4word<-test[l>=5,]
sum(pred4word$Correct_prediction)/nrow(pred4word)*100 #Accuracy for prediction with 4 words= 49.20%







