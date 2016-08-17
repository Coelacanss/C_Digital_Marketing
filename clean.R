### This script is to clean data from Genesee social media platfrom.

library(lubridate)
library(ggplot2)
library(caret)
library(plyr)
load(file = 'post_facebook_clean.Rda')

post_facebook$weekday = whichday
post_facebook$create_time = created_time

created_time = gsub('T', ' ', created_time)
whichday = wday(created_time)
whichday = whichday - 1
whichday[whichday == 0] = 7

######################  Facebook  PART  #########################
##
## likes by post type
ggplot(data = post_facebook) +
      theme_bw() +
      geom_point(aes(x = type, y = likes_count, color = type))

## likes by hour
 likesByHour <- aggregate(likes_count ~ hour, data = post_facebook, mean)
 postNum = count(post_facebook$hour)
 likesByHour$post = postNum$freq
 likesByHour = rbind(likesByHour, emptyData)
# save(likesByHour, file = 'fblikesByHour.Rda')
# load(file = 'fblikesByHour.Rda')
ggplot(data = likesByHour) +
      theme_bw() +
      geom_line(aes(x = hour, y = likes_count), color = 'red') +
      geom_line(aes(x = hour, y = post), color = 'blue') +
      ggtitle('Average Likes By Post Time (Hours)')
      ## Blue line is the number of posts by hours.

## likes by weekdays
likesByDay <- aggregate(likes_count ~ weekday, data = post_facebook, mean)
postNum = count(post_facebook$weekday)
likesByDay$post = postNum$freq
# save(likesByDay, file = 'fblikesByDay.Rda')
# load(file = 'fblikesByDay.Rda')
ggplot(data = likesByDay) +
      theme_bw() +
      geom_line(aes(x = weekday, y = likes_count), color = 'red', lty = 1) +
      geom_line(aes(x = weekday, y = post), color = 'blue', lty = 1) +
      ggtitle('Average Likes By Post Time (Weekdays)')
## Blue line is the number of posts by weekdays.


## likes by type
likesByType <- aggregate(likes_count ~ type, data = post_facebook, mean)
postNum = count(post_facebook$type)
likesByType$post = postNum$freq
# save(file = 'fblikesByType.Rda')
# load(file = 'fblikesByType.Rda')
ggplot(data = likesByType) +
      theme_bw() +
      geom_line(aes(x = type, y = likes_count), color = 'red') +
      geom_line(aes(x = type, y = post), color = 'blue') +
      ggtitle('Average Likes By Post Time (Hours)')
      ## Blue line is the number of posts by types.


## comments by hour
commentsByHour <- aggregate(comments_count ~ hour, data = post_facebook, mean)
postNum = count(post_facebook$hour)
commentsByHour$post = postNum$freq
commentsByHour = rbind(commentsByHour, emptyData)
# save(likesByHour, file = 'commentsByHour.Rda')
# load(file = 'fbcommmentsByHour.Rda')
ggplot(data = commentsByHour) +
      theme_bw() +
      geom_line(aes(x = hour, y = comments_count), color = 'red') +
      geom_line(aes(x = hour, y = post), color = 'blue') +
      ggtitle('Average Comments By Post Time (Hours)')
## Blue line is the number of posts by hours.


## comments by weekday
commentsByDay <- aggregate(comments_count ~ weekday, data = post_facebook, mean)
postNum = count(post_facebook$weekday)
commentsByDay$post = postNum$freq
# save(commentsByDay, file = 'fbcommentsByDay.Rda')
# load(file = 'fbcommmentsByDay.Rda')
ggplot(data = commentsByDay) +
      theme_bw() +
      geom_line(aes(x = weekday, y = comments_count), color = 'red') +
      geom_line(aes(x = weekday, y = post), color = 'blue') +
      ggtitle('Average Comments By Post Time (Weekdays)')
## Blue line is the number of posts by weekdays.


## comments by type
commentsByType <- aggregate(comments_count ~ type, data = post_facebook, mean)
postNum = count(post_facebook$type)
commentsByType$post = postNum$freq
# save(file = 'fbcommentsByType.Rda')
# load(file = 'fbcommentsByType.Rda')
ggplot(data = commentsByType) +
      theme_bw() +
      geom_line(aes(x = type, y = comments_count), color = 'red') +
      geom_line(aes(x = type, y = post), color = 'blue') +
      ggtitle('Average Comments By Post Type')
## Blue line is the number of posts by types.


## shares by hour
sharesByHour <- aggregate(shares_count ~ hour, data = post_facebook, mean)
postNum = count(post_facebook$hour)
sharesByHour$post = postNum$freq
sharesByHour = rbind(sharesByHour, emptyData)
# save(sharesByHour, file = 'sharesByHour.Rda')
# load(file = 'sharesByHour.Rda')
ggplot(data = sharesByHour) +
      theme_bw() +
      geom_line(aes(x = hour, y = shares_count), color = 'red') +
      geom_line(aes(x = hour, y = post), color = 'blue') +
      ggtitle('Average Shares By Post Time (Hours)')
## Blue line is the number of posts by hours.


## shares by weekdays
sharesByDay <- aggregate(shares_count ~ weekday, data = post_facebook, mean)
postNum = count(post_facebook$weekday)
sharesByDay$post = postNum$freq
# save(sharesByDay, file = 'fbsharesByDay.Rda')
# load(file = 'fbsharesByDay.Rda')
ggplot(data = sharesByDay) +
      theme_bw() +
      geom_line(aes(x = weekday, y = shares_count), color = 'red', lty = 1) +
      geom_line(aes(x = weekday, y = post), color = 'blue', lty = 1) +
      ggtitle('Average Likes By Post Time (Weekdays)')
## Blue line is the number of posts by weekdays.


## shares by type
sharesByType <- aggregate(shares_count ~ type, data = post_facebook, mean)
postNum = count(post_facebook$type)
sharesByType$post = postNum$freq
# save(file = 'fbcommentsByType.Rda')
# load(file = 'fbcommentsByType.Rda')
ggplot(data = sharesByType) +
      theme_bw() +
      geom_line(aes(x = type, y = shares_count), color = 'red') +
      geom_line(aes(x = type, y = post), color = 'blue') +
      ggtitle('Average Shares By Post Type')
## Blue line is the number of posts by types.


emptyData = matrix(data = 0, nrow = 9, ncol = 3)
emptyData = as.data.frame(emptyData)
colnames(emptyData) <- c('hour', 'likes_count', 'post')
emptyData$hour = c(3:11)


######################  Twitter  PART  #########################
##


