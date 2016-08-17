### Hashtag Analysis

library(lubridate)
library(ggplot2)
library(caret)
library(plyr)
library(stringi)
library(stringr)
load(file = 'post_facebook_clean.Rda')

###################### FACEBOOK PART ########################
atTF = grepl('@', post_facebook$message)
hashtagTF = grepl('#', post_facebook$message)
post_facebook$atOrNot = atTF
post_facebook$hashtagOrNot = hashtagTF
 # save(post_facebook, file = 'post_facebook_clean.Rda')

## hashtag
hashList = str_extract_all(post_facebook$message[hashtagTF], '#\\w{1,20}')
index = which(hashtagTF == T)
count = 0
for (i in index){
      count = count + 1
      post_facebook[i, "hashtagContent"] = paste(unlist(hashList[count]), collapse = ' ')
      print(i)
}
 # save(post_facebook, file = 'post_facebook_clean.Rda')

hashMessy = unlist(hashList)
sort(table(post_facebook$hashtagContent), decreasing = T)
count(hashMessy)

## at
atList = str_extract_all(post_facebook$message[atTF], '@\\w{1,20}')
index = which(atTF == T)
count = 0
for (i in index){
      count = count + 1
      post_facebook[i, "atContent"] = paste(unlist(atList[count]), collapse = ' ')
      print(i)
}
# save(post_facebook, file = 'post_facebook_clean.Rda')

hashMessy = unlist(hashList)
sort(table(hashMessy), decreasing = T)
test = count(hashMessy)

## sub1 with hashtag, sub2 without hashtag
data.sub1 = post_facebook[post_facebook$hashtagOrNot == T,]
data.sub2 = post_facebook[post_facebook$hashtagOrNot == F,]
(mean(data.sub1$likes_count) - mean(data.sub2$likes_count)) / mean(data.sub2$likes_count)
(mean(data.sub1$comments_count) - mean(data.sub2$comments_count)) / mean(data.sub2$comments_count)
(mean(data.sub1$shares_count) - mean(data.sub2$shares_count)) / mean(data.sub2$shares_count)

## sub1 with @, sub2 without @
data.sub1 = post_facebook[post_facebook$atOrNot == T,]
data.sub2 = post_facebook[post_facebook$atOrNot == F,]
(mean(data.sub1$likes_count) - mean(data.sub2$likes_count)) / mean(data.sub2$likes_count)
(mean(data.sub1$comments_count) - mean(data.sub2$comments_count)) / mean(data.sub2$comments_count)
(mean(data.sub1$shares_count) - mean(data.sub2$shares_count)) / mean(data.sub2$shares_count)

#############################################################

