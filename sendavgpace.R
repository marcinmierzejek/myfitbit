library(httr)
library(jsonlite)

# converting number of seconds to format min:sec
secondsToTime <- function (pSeconds) {
  hours <- floor(pSeconds / 3600)
  seconds <- pSeconds - hours * 3600
  minutes <- floor(seconds / 60)
  if (minutes == 60) {
    minutes <- 0
    hours <- hours + 1
  }
  seconds <- seconds - minutes * 60
  if (seconds == 60) {
    seconds <- 0
    minutes <- minutes + 1
  }
  
  if (hours > 0) {
    result <- paste(hours,
                    sprintf("%02d", minutes), 
                    sprintf("%02d", seconds), 
                    sep = ":")  
  } else {
    result <- paste(minutes, 
                    sprintf("%02d", seconds), 
                    sep = ":")
  }
  
  return (result)
}

# load configuration
#srcPath <- getSrcDirectory(function(x) {x})
#source(paste(srcPath, "config.R", sep = "/"))
source("C:/Users/mierzejm/Google Drive/privs/prace/myfitbit/config.R")

# connect to fitbit
fitbitEndPoint <- oauth_endpoint(request = request_token_uri,
                                 authorize = authorize_uri,
                                 access = access_uri)

myapp <- oauth_app(app_name, client_id, client_secret)

# get OAuth token
fitbitToken <- oauth2.0_token(fitbitEndPoint,
                              myapp,
                              scope = c("activity"),
                              use_basic_auth = TRUE,
                              cache = FALSE)

# define URL to access resources
fitbitUrl <- paste("https://api.fitbit.com/1/user/-/activities/list.json?",
                   "beforeDate=", Sys.Date() + 1,
                   "&offset=0&limit=100&sort=desc",
                   sep = "")

# make API requests
resp <- GET(url = fitbitUrl,
            config(token = fitbitToken))

# get list of activities from the http response
activities <- fromJSON(content(resp, as = 'text'))$activities
# leave only running and few interesting attributes
activities <- activities[activities$activityName == "Run", c("activityName", "startTime", "distance", "activeDuration")]

# distance in kilometres
distance <- sum(activities$distance) 
# duration is seconds
duration <- sum(activities$activeDuration) / 1000

paceTxt <- secondsToTime(round(duration / distance))

# send e-mail with stats

Sys.setenv(JAVA_HOME="")

library(mailR)

# subject 
subject <- paste("average pace = ", paceTxt, sep = "")

# body
body <- "date\tdistance\tduration\tpace"
for (i in 1:nrow(activities)) {
  row <- paste(activities$startTime[i], 
               activities$distance[i],
               secondsToTime(activities$activeDuration[i] / 1000),
               secondsToTime(round((activities$activeDuration[i] / 1000) / activities$distance[i])),
               sep = "\t")
  body <- paste(body, row, sep = "\n")
  
}

# send mail
send.mail(from = from,
          to = to,
          subject = subject,
          body = body,
          authenticate = TRUE,
          smtp = list(host.name = smtp_host,
                      port = smtp_port,
                      user.name = smtp_user,
                      passwd = smtp_passwd,
                      ssl = TRUE))