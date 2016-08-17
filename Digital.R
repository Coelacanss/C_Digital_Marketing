### This script is for Digital MKT Final Project

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

################## Getting Data From Facebook ####################
##
## API and Access
fb_oauth <- fbOAuth(app_id="1504407903188968", app_secret="a2f31c81a5bb1c740680b26c779782a7",
                    extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
load("fb_oauth")

myaccess_token = 'CAACEdEose0cBAHXp9N6cXG4CGNYwCi6t61jzZBEYROwPBpFhcBGZBAlRqZBzJkjFCIMsZCq3hrl7ZCdLBLhYPzCQB7abGuK4DfwmuqFcI8JE5CWyPXDlWaHGZADjlGuYxiauIQyYOb1ETRcrZBnXiobWk7J66S0pak3mXEKF0WvoLthJ1HZCBreWcJ55ZAerzmSmI5jzLRkVLZCZAxrjmjwMJZBP'
options(RCurlOptions = list(verbose = FALSE, capath = system.file('CrulSSL', 'cacert.pem', package = 'Rcurl'),
                            ssl.verifypeer = FALSE))

post_facebook <- getPage(page = 'geneseebrewery', token = myaccess_token, n = 1030, since = NULL, until = NULL, feed = FALSE)
# load(file = 'post_facebook.Rda')

getUsers('geneseebrewery', token = myaccess_token, private_info = T)
getFriends(token = myaccess_token, simplify = T)
getLikes(user = 'geneseebrewery' ,token = myaccess_token)


################## Getting Data From Twitter #####################
##
## API and Access
api_start <- function() {
      consumer_key <- 'IJMFtpl48Ld8Do0LfRGhr7l1t'
      consumer_secret <- 'HPupjjOydONh2W0s0qHmPcdWiWA7rK5PeNb3rIquof6uWWznrZ'
      access_token <- '494108863-niGNy3OnKSpqufEY7v2K0fee2U9nIlHqwjk1W2MR'
      access_secret <- 'EE5MruqCPigWJRr9fXkilUBu0Z9u3LVMt9Ecxud7bpaDq'
      setup_twitter_oauth(consumer_key,
                          consumer_secret,
                          access_token,
                          access_secret)
}

genesee <- userTimeline('GeneseeBrewery', n=4170, includeRts = TRUE, excludeReplies = FALSE)
post_genesee <- twListToDF(genesee)
# load(file = 'post_twitter.Rda')

count = 7987
for (id in idList[7988:9110]) {
      
      count = count + 1
      temp <- try(lookupUsers(users = id, includeNA = T))
      if (identical(class(temp), "try-error")){
            next()
      }
      temp.data = twListToDF(temp)
      fans_twitter = rbind(fans_twitter, temp.data)
      print(count)
      if (count %% 179 == 111) {
            Sys.sleep(900)
            api_start()
      }
}

fans_twitter = fans_twitter[!duplicated(fans_twitter$id),]

# save(fans_twitter, file = 'fans_twitter.Rda')
# load('fans_twitter.Rda')


################### Getting Data From Instagram ###################
##
## API staff
library(httr)
library(instaR)
full_url <- oauth_callback()
full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
print(full_url)

app_name <- "URLearning"
client_id <- "e87fa11652834926995426af3522a0e5"
              bfc312fde001af7e5b05e33e694a4abd
client_secret <- "10ca901b7f9244379738332098fd063c"
scope = "public_content"
# token = instaOAuth(client_id, client_secret, scope = "basic")    ## for instaR

instagram <- oauth_endpoint(
      authorize = "https://api.instagram.com/oauth/authorize",
      access = "https://api.instagram.com/oauth/access_token")
myapp <- oauth_app(app_name, client_id, client_secret)

ig_oauth <- oauth2.0_token(instagram, myapp,scope="public_content", 
                           type = "application/x-www-form-urlencoded",cache=FALSE)
tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
token <- tmp[[1]][4]

username <- 'geneseebrewery'
user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=',
                                   username,'&access_token=',token,sep="")),
                      unexpected.escape = "keep")

test <- getUserMedia(userid = '1310392788', token = token, n = 5, folder = 'test')
View(test)


## Scarping 

library(XML)
library(rvest)
library(Unicode)
library(stringr)
library(stringi)
library(lubridate)
date.default <- as.POSIXct('1970-01-01 00:00:00 EST')

# create dataframe
data_ins <- matrix(data = NA, nrow = 341, ncol = 6)
data_ins = as.data.frame(data_ins)
features = c('picID', 'datetime', 'text', 'likesNumber', 'commentsNumber', 'pageLink')
colnames(data_ins) <- features

# load link file
linkList <- read.csv(file = 'Ins - Link.csv')
linkList$Link = as.character(linkList$Link)

for (i in 57:57) {
      
      print(i)  # for tracking
      
      urlFile = linkList$Link[i]
      web <- try(readLines(urlFile))
      if (identical(class(web), "try-error")){
            next()
      }
      
      webContent = web[!is.na(str_extract(web, '.*language_code.*'))]
      webText = web[!is.na(str_extract(web, '.*meta content.*name="description".*'))]
      
      # extract text
      text = gsub('    <meta content=\"“', '', webText)
      text = gsub('”\" name=\"description\" />', '', text)
      
      # extract likes number
      likeNum = str_extract(webContent, "\\blikes.{11}\\d{1,4}")
      likeNum = as.numeric(str_extract(likeNum, "\\d{1,4}"))
      
      # extract comments number
      commentNum = str_extract(webContent, "\\bcomments.{11}\\d{1,4}")
      commentNum = as.numeric(str_extract(commentNum, "\\d{1,4}"))
      
      # extract pic ID
      picID = web[!is.na(str_extract(web, '.*<meta property="og:image".*'))]
      picID = str_extract(picID, '\\d{6,8}.*jpg')
      
      # extract date and time
      secondPassed = str_extract(webContent, "\\bdate.{2}\\d{8,11}")
      secondPassed = as.numeric(str_extract(secondPassed, "\\d.*"))
      datePost = date.default + seconds(secondPassed) - hours(5)
      datePost = as.character(datePost)
      
      # save data into dataframe
      data_ins[i,'picID'] = picID
      data_ins[i,'datetime'] = datePost
      data_ins[i,'text'] = text
      data_ins[i,'likesNumber'] = likeNum
      data_ins[i,'commentsNumber'] = commentNum
      data_ins[i,'pageLink'] = urlFile
      
      if (i %% 20 == 0) {
            Sys.sleep(60)
      }

}
data_ins$picID = str_extract(data_ins$picID, '\\d{6,8}.*')

# save(post_ins, file = 'post_ins.Rda')
# load(file = 'post_ins.Rda')


#####################################################

load('post_facebook.Rda')
load('post_twitter.Rda')
load('post_ins.Rda')
load('fans_twitter.Rda')
linkList <- read.csv(file = 'Ins - Link.csv')
