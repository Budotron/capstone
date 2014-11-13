# load required packages
packages<-c("tm", "qdap", "actuar", "stringi", "stringr", "data.table")
sapply(packages, require, character.only = TRUE)


# function getdata() checks for the existence of a directory containing a file to 
# be downloaded, and if it is not present, downloads a linked file and stores it 
# in a directory in the current workspace. 
#
# input: a URL linked to a file to be downloaded, desired name for the 
#        directory, desired name for the downloaded file, extension for the 
#        file. 
# output : the path to the downloaded file

getdata<-function(fileUrl, dir, filename, ext){
        # create directory, if it is not already present
        dirName<-paste(dir, sep = "")
        if(!file.exists(dirName)){
                dir.create(path = dirName)
        }
        # Get the data, unless this step has already been done
        dest<-paste("./", dirName,"/", filename, ext, sep = "")
        if(!file.exists(dest)){
                download.file(url = fileUrl, 
                              destfile = dest, 
                              method = "curl") 
                datedownloaded<-date()
        }
        dest
}
fileURL1 <- 
        "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip" 
dataset<-getdata(fileUrl = fileURL1, 
                 dir = "swiftkey", 
                 filename = "dataset", 
                 ext = ".zip")
if(!exists("./final")){
        swiftKey<-unzip(zipfile = dataset)
}
# obtain a list of profane words to filter out of the corpus
fileURL2<-
        "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
if(!exists("./badwords/badwords.text")){
        profanities<-getdata(fileUrl = fileURL2, 
                             dir = "badwords",
                             filename = "badwords", 
                             ext = ".txt")
}



# function sampleData() obtains an equally divided sample of the first n 
# elements of text in each of three different feeds (twitter, blogs, and news)
#
# inputs: language (en, ru, de, fi), feed, sampleSize (whole number), each (T
# obtains a sample of size sample size from each feed; F obtains a sample of 
# size sample size, divided among the number of elements in feed)
# 
# output: matrix of size (sample size from each feed x 1)

sampleData<-function(language, feed, sampleSize=10000000, each){
        alltext<-swiftKey[grep(language, swiftKey)]
        feed<-lapply(X = feed, function(feed){
                alltext[grep(feed, alltext)]
        })
        if (each == F){
                sampnum<-floor(sampleSize/length(feed))
                samp<-sapply(feed, function(feed){
                        con<-feed
                        readLines(con, sampnum, encoding = "UTF-8", skipNul = T)
                })
                closeAllConnections()
        }else if (each==T){
                sampnum<-sampleSize
                samp<-sapply(feed, function(feed){
                        con<-feed
                        readLines(con, sampnum, encoding = "UTF-8", skipNul = T)
                })
                closeAllConnections()
        }
        samp<-unroll(samp, bycol = T)
        samp
}

# Strip punctuation from text while treating emoticons as words
stripPunct<-function(text){
        emots <- as.character(outer(c(":", ";", ":-", ";-", "'"),
                                     c(")", "(", "]", "[", "D", "o", "O", "P", "p", "m", "ll"), stri_paste))
        escape_regex <- function(r) {
                stri_replace_all_regex(r, "\\(|\\)|\\[|\\]", "\\\\$0")
        }
        regex <- stri_c("(", stri_c(escape_regex(emots), "'", "-", collapse="|"), ")")
        stri_replace_all_regex(text, stri_c(regex, "|\\p{P}"), "$1")
}

# profanity filtering
filterProfanities<-function(text){
        badwords<-fread(profanities, header = F, stringsAsFactors = T, sep = "\n")
        badwords<-paste("\\<", badwords$V1, "\\>", collapse = "|")
        gsub(pattern = badwords, replacement = "", x = text)
}


cleanSample<-function(text){
        stripOdd<-iconv(text, "latin1", "ASCII", sub="")
        cleanText<-filterProfanities(stripOdd)
        stripPunct(cleanText)
}

sample<-sampleData(language = "en", 
                   feed = c("twitter"), 
                   sampleSize = 100,
                   each = T)

cleanSample(sample)