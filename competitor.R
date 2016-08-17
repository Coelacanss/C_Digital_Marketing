### This script is to study the use of social media of Genesee's competitor Small Town Brewery

library(Rfacebook)
library(httpuv)
library(RCurl)
library(rjson)
library(httr)
library(RColorBrewer)
library(twitteR)
library(tm)
library(SnowballC)
library(plyr)

##################  Facebook ####################
##
## API and Access
fb_oauth <- fbOAuth(app_id="1504407903188968", app_secret="a2f31c81a5bb1c740680b26c779782a7",
                    extended_permissions = TRUE)
myaccess_token = 'CAACEdEose0cBAMR1XQzVAY4Q5yyZCiHdIDHq6VNUQsZBX9bUqNAM9cDOFNiDpz9nYU10sJVgrDXftR7AfL4plikSHbtB3Hz2JK6NdFzctLcxZBaWW3O0pVO6jj3u6wiNhgvVaBEn8BCMLLl68Aw3pFGR9bJi6CErMHsZBmMwLeNzKOCAPVpzAL1uSWZAEZAfO20UBN0qlCaKWy0NTEOU1X'

post_stb <- getPage(page = 'smalltownbrewery', token = myaccess_token, n = 1030, since = NULL, until = NULL, feed = FALSE)
 # save(post_stb, file = 'post_stb.Rda')

created_time = post_stb$created_time
created_time = gsub('T', ' ', created_time)
created_time = gsub('+0000', '', created_time)
created_year = year(created_time)
created_month = month(created_time)
created_day = day(created_time)
created_hour = hour(created_time)
created_minute = minute(created_time)
created_second = second(created_time)
created_weekday = wday(created_time)
created_weekday = created_weekday -1
created_weekday[created_weekday == 0] = 7

post_stb$created_time = created_time
post_stb$year = created_year
post_stb$month = created_month
post_stb$day = created_day
post_stb$hour = created_hour
post_stb$minute = created_minute
post_stb$second = created_second
post_stb$weekday = created_weekday
 # save(post_stb, file = 'post_stb.Rda')

atTF = grepl('@', post_stb$message)
hashtagTF = grepl('#', post_stb$message)
post_stb$atOrNot = atTF
post_stb$hashtagOrNot = hashtagTF
post_stb$hashtagContent = NULL
post_stb$atContent = NULL

## hashtag
hashList = str_extract_all(post_stb$message[hashtagTF], '#\\w{1,20}')
index = which(hashtagTF == T)
count = 0
for (i in index){
      count = count + 1
      post_stb[i, "hashtagContent"] = paste(unlist(hashList[count]), collapse = ' ')
      print(i)
}
 # save(post_stb, file = 'post_stb.Rda')

## at
atList = str_extract_all(post_stb$message[atTF], '@\\w{1,20}')
index = which(atTF == T)
count = 0
for (i in index){
      count = count + 1
      post_stb[i, "atContent"] = paste(unlist(atList[count]), collapse = ' ')
      print(i)
}
 # save(post_stb, file = 'post_stb.Rda')

###################################################
load(file = 'post_stb.Rda')

emptyData = matrix(data = 0, nrow = 4, ncol = 3)
emptyData = as.data.frame(emptyData)
colnames(emptyData) <- c('hour', 'comments_count', 'post')
emptyData$hour = c(7:10)

## likes by hour
likesByHour <- aggregate(likes_count ~ hour, data = post_stb, mean)
postNum = count(post_stb$hour)
likesByHour$post = postNum$freq
likesByHour = rbind(likesByHour, emptyData)
ggplot(data = likesByHour) +
      theme_bw() +
      geom_line(aes(x = hour, y = likes_count), color = 'red') +
      geom_line(aes(x = hour, y = post), color = 'blue') +
      ggtitle('Average Likes By Post Time (Hours)')
## Blue line is the number of posts by hours.

## shares by hour
sharesByHour <- aggregate(shares_count ~ hour, data = post_stb, mean)
postNum = count(post_stb$hour)
sharesByHour$post = postNum$freq
sharesByHour = rbind(sharesByHour, emptyData)
ggplot(data = sharesByHour) +
      theme_bw() +
      geom_line(aes(x = hour, y = shares_count), color = 'red') +
      geom_line(aes(x = hour, y = post), color = 'blue') +
      ggtitle('Average Shares By Post Time (Hours)')
## Blue line is the number of posts by hours.

## comments by hours
commentsByHour <- aggregate(comments_count ~ hour, data = post_stb, mean)
postNum = count(post_stb$hour)
commentsByHour$post = postNum$freq
commentsByHour = rbind(commentsByHour, emptyData)
ggplot(data = commentsByHour) +
      theme_bw() +
      geom_line(aes(x = hour, y = comments_count), color = 'red') +
      geom_line(aes(x = hour, y = post), color = 'blue') +
      ggtitle('Average Comments By Post Time (Hours)')
## Blue line is the number of posts by hours.

## comments by weekday
commentsByDay <- aggregate(comments_count ~ weekday, data = post_stb, mean)
postNum = count(post_stb$weekday)
commentsByDay$post = postNum$freq
ggplot(data = commentsByDay) +
      theme_bw() +
      geom_line(aes(x = weekday, y = comments_count), color = 'red') +
      geom_line(aes(x = weekday, y = post), color = 'blue') +
      ggtitle('Average Comments By Post Time (Weekdays)')
## Blue line is the number of posts by weekdays.

## likes by weekdays
likesByDay <- aggregate(likes_count ~ weekday, data = post_stb, mean)
postNum = count(post_stb$weekday)
likesByDay$post = postNum$freq
ggplot(data = likesByDay) +
      theme_bw() +
      geom_line(aes(x = weekday, y = likes_count), color = 'red', lty = 1) +
      geom_line(aes(x = weekday, y = post), color = 'blue', lty = 1) +
      ggtitle('Average Likes By Post Time (Weekdays)')
## Blue line is the number of posts by weekdays.

## shares by weekdays
sharesByDay <- aggregate(shares_count ~ weekday, data = post_stb, mean)
postNum = count(post_stb$weekday)
sharesByDay$post = postNum$freq
ggplot(data = sharesByDay) +
      theme_bw() +
      geom_line(aes(x = weekday, y = shares_count), color = 'red', lty = 1) +
      geom_line(aes(x = weekday, y = post), color = 'blue', lty = 1) +
      ggtitle('Average Likes By Post Time (Weekdays)')
## Blue line is the number of posts by weekdays.

## comments by type
commentsByType <- aggregate(comments_count ~ type, data = post_stb, mean)
postNum = count(post_stb$type)
commentsByType$post = postNum$freq

## shares by type
sharesByType <- aggregate(shares_count ~ type, data = post_stb, mean)
postNum = count(post_stb$type)
sharesByType$post = postNum$freq

## likes by type
likesByType <- aggregate(likes_count ~ type, data = post_stb, mean)
postNum = count(post_stb$type)
likesByType$post = postNum$freq

####################### Hashtag Analysis ##########################
##

hashMessy = unlist(hashList)
sort(table(hashMessy), decreasing = T)
test = count(hashMessy)

## sub1 with hashtag, sub2 without hashtag
data.sub1 = post_stb[post_stb$hashtagOrNot == T,]
data.sub2 = post_stb[post_stb$hashtagOrNot == F,]
(mean(data.sub1$likes_count) - mean(data.sub2$likes_count)) / mean(data.sub2$likes_count)
(mean(data.sub1$comments_count) - mean(data.sub2$comments_count)) / mean(data.sub2$comments_count)
(mean(data.sub1$shares_count) - mean(data.sub2$shares_count)) / mean(data.sub2$shares_count)

## sub1 with @, sub2 without @
data.sub1 = post_stb[post_stb$atOrNot == T,]
data.sub2 = post_stb[post_stb$atOrNot == F,]
(mean(data.sub1$likes_count) - mean(data.sub2$likes_count)) / mean(data.sub2$likes_count)
(mean(data.sub1$comments_count) - mean(data.sub2$comments_count)) / mean(data.sub2$comments_count)
(mean(data.sub1$shares_count) - mean(data.sub2$shares_count)) / mean(data.sub2$shares_count)

brandHashtagTF = grepl('smalltownbrewery', post_stb$hashtagContent, ignore.case = T)
data.sub1 = post_stb[brandHashtagTF,]
data.sub2 = post_stb[-brandHashtagTF,]
(mean(data.sub1$likes_count) - mean(data.sub2$likes_count)) / mean(data.sub2$likes_count)
(mean(data.sub1$comments_count) - mean(data.sub2$comments_count)) / mean(data.sub2$comments_count)
(mean(data.sub1$shares_count) - mean(data.sub2$shares_count)) / mean(data.sub2$shares_count)


################### Distribution by Time ######################
##
post_stb$created_time = as.Date(post_stb$created_time)
ggplot(data = post_stb) +
      geom_point(aes(x = created_time, y = likes_count)) +
      ylim(0,800)
