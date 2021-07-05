library(shiny)
library(data.table)

dict <- readRDS("dict.RDS")
number <- readRDS("number.RDS")
profane<-readLines("en")
foreign<-readRDS("foreign.RDS")
loaded<-list()
for(i in 1:25) {
    loaded[[i]]<-readRDS(paste0("dt/",i,".RDS"))
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
    if(w==3972) return(format(Sys.Date()))
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

shinyServer(function(input, output) {
output$word<-renderText(word_predict(input$sentence))
})


