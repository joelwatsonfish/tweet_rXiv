library(rtweet)
library(tidyverse)
library(tidytext)
#library(ggplot2)
#library(magick)
library(glue)
#library(webshot) 
library(tibble)
get_token()
#
tweet_rXiv_filename      <- "tweet_rXiv.rds"
tweeple_filename         <- "tweeple.csv"
#
#
# Set to FALSE if you don't want to download the images people post
download_images          <- FALSE
#
#
# This is the number of tweets to download for a user in the specified
# tweeple csv file if there is no record of downloading their timelines
# previously.  The maximum Twitter allows is 3200, which can take
# an hour or so to download if you want to get all the associated
# upthread-conversations.  (Length of time is due to the rate limit
# Twitter sets.)
num_of_initial_tweets    <- 40
#
#
# Twitter restricts the number of timeline pulls one can make.  These lines 
# of code determine what that rate limit is, deterimine the current number 
# left, and then sets a counter to track the number of pulls throughout the
# execution of the code.  In particular, when the rate limit has been met,
# the code execution will pause until the Twitter resets it.  (It takes
# about 15 minutes for the rate limit to reset.)  The final line in this 
# block of code creates a buffer for error in miscounting.
rate_limit_pull  <- rate_limit()
tweet_rate_limit <- subset(rate_limit_pull,
                           query=="statuses/user_timeline")[["limit"]]
tweet_rate_limit_counter <- tweet_rate_limit -
  subset(rate_limit_pull,query=="statuses/user_timeline")[["remaining"]]
tweet_rate_limit <- tweet_rate_limit - 100
### print(glue("The initial tweet_rate_limit is {tweet_rate_limit}"))
### print(glue("The initial tweet_rate_limit_counter is {tweet_rate_limit_counter}"))
#
#
timeline_fudge <- 16
#
#
# This block of code reads in the users whose tweets are to be archived.  
# It removes entries for which follow_conversation == FALSE if the entry 
# is also listed as follow_conversation == TRUE.  
tweeple          <- read.csv(tweeple_filename, stringsAsFactors=FALSE)
tweeple_convo    <- subset(tweeple, follow_conversation)
tweeple_no_convo <- subset(tweeple, !follow_conversation)
tweeple_no_convo <- subset(
  tweeple_no_convo, user_name %in% setdiff(tweeple_no_convo$user_name, 
                                           tweeple_convo$user_name))
tweeple <- rbind(tweeple_convo, tweeple_no_convo)
write.csv(tweeple, tweeple_filename, row.names=FALSE)
#
#
# These lines of code are for constucting a data frame of the same type as
# that returned from the get_timeline() function, but with zero entries.
# It's useful to have such a structure to initialize temporary data frames.
user_proteus        <- str_extract(tweeple$user_name[1], "[0-9a-zA-Z_]*$")
tweet_proteus       <- get_timeline(user=user_proteus, n=1)
blank_tweet         <- tweet_proteus[-1,]
#
#
# These lines of code read in previously rXived tweets (if they exist)
old_tweets_exists <- file.exists(tweet_rXiv_filename)
all_pulled_tweets <- blank_tweet
if (old_tweets_exists){
  old_tweets <- readRDS(tweet_rXiv_filename)
} else {
  old_tweets <- blank_tweet
}
#
#
# The user_name sometimes has an @ in front of it, and this function removes it
Remove_AT_Symbol <- function(given.name){
  return(str_extract(given.name, "[0-9a-zA-Z_]*$"))
}
#
# This is just the usual rbind for combining two data frames of tweets, but
# it sorts the result along the 'status_id' variable as well.
Sort_Bind_Tweets <- function(given_tweets_1, given_tweets.2) {
  tweets_to_return <- rbind(given_tweets_1, given_tweets.2)
  return(tweets_to_return[order(tweets_to_return$status_id, decreasing=TRUE),])
}
#
# This code sorts the givne tweets along the 'status_id' variable
Sort_Tweets <- function(given_tweets) {
  return(given_tweets[order(given_tweets$status_id, decreasing=TRUE),])
}
#
#
# Twitter caps the number of pulls one can make at 300 per 15 minutes, so
# this function increments a pull counter each time it's run, and if
# the counter gets to the max, it pauses for 15 minutes.
Check_Rate_Limit <- function(){
###  print(glue("tweet_rate_limit_counter is {tweet_rate_limit_counter}"))
#  if (tweet_rate_limit_counter %% 10 == 0) {
#    remaining <- subset(rate_limit(),query=="statuses/user_timeline")[["remaining"]]
#    print(glue("remaining is {remaining}"))
#  }
  if (tweet_rate_limit_counter > tweet_rate_limit){
    print("Rate limit met. Execution paused for 16 minutes.")
    Sys.sleep(1000)
    tweet_rate_limit_counter <<- 0
  }
}
#
#
# The function is essentially the same as get_timeline(), but it runs 
# Check_Rate_Limit() to pause the tweet pulls in the case that the rate
# limit has been met (or rather, nearly met).
Pull_Timeline <- function(given_user_name, given_max_id=NULL, given_number) {
###  print("=========================================")
  to_return <- blank_tweet
  tweet_rate_limit_counter <<- tweet_rate_limit_counter + 1
  if (given_number > 1) {
    tweet_rate_limit_counter <<- tweet_rate_limit_counter + timeline_fudge
  }
  Check_Rate_Limit()
###    remaining1 <- subset(rate_limit(),query=="statuses/user_timeline")[["remaining"]]
###    print(glue("remaining1 is {remaining1}"))
  to_return <- get_timeline(user=given_user_name, 
                            max_id=given_max_id, 
                            n=given_number, 
                            check=FALSE)
###    remaining2 <- subset(rate_limit(),query=="statuses/user_timeline")[["remaining"]]
###    print(glue("remaining2 is {remaining2}"))
###    n_tweets_div  <- as.integer(nrow(to_return)/200) +1
###    print(glue("Change in remaining: {remaining1-remaining2}"))
###    print(glue("Number of tweets pulled divided by 200: {n_tweets_div}"))
  return(to_return)
}
#
#
# This is the main block of code for pulling the tweets from the users listed
# in the tweeple.csv file.  That file keeps track of the status_id of their 
# most recently acquired tweet, and this algorithm successively pulls tweets
# until that most recently acquired tweet is again obtained.  In the case that
# the tweeple.csv file does not have a recorded most recently acquired tweet
# for the user in question, it just pulls a fixed number of tweets specified
# by the num_of_initial_tweets variable.
all_pulled_tweets <- blank_tweet
for (k in 1:nrow(tweeple)){                                                               
  user_name       <- Remove_AT_Symbol(tweeple$user_name[k])
  latest_tweet    <- tweeple$latest_tweet_id[k]
  in_conversation <- tweeple$follow_conversation[k]
  pulled_tweets   <- blank_tweet
  max_pulls       <- 10 
  num_tweet_base  <- 10 
  pulled_max_id   <- NULL
  valid_pull      <- FALSE
  if (is.na(tweeple$latest_tweet_id[k])) {
    pulled_tweets <- blank_tweet
#    pulled_tweets <- get_timeline(user=user_name, n=num_of_initial_tweets)
    pulled_tweets <- Pull_Timeline(user_name, NULL, num_of_initial_tweets)
###    print(unique(pulled_tweets$screen_name))
###    print(nrow(pulled_tweets))
    if (nrow(pulled_tweets) > 0 ) {
      valid_pull <- TRUE
    }
  } else {
    for (i in 1:max_pulls) {
###      print(glue("i is {i}"))
      temp <- blank_tweet
      temp <- Pull_Timeline(user_name, pulled_max_id, num_tweet_base*2^i)
      if (nrow(temp) > 0) {
        valid_pull <- TRUE
      }
      if (nrow(temp) == num_tweet_base*2^i) { 
        # Run this block if the right number of tweets were pulled
        pulled_max_id <- temp$status_id[num_tweet_base*2^i]
        if (i==1) {
          pulled_tweets <- temp
        } else {
          pulled_tweets <- Sort_Bind_Tweets(pulled_tweets,temp[-1,] )
        }
        if (nrow(subset(pulled_tweets, status_id <= latest_tweet)) > 0) {
          break
        }
      } else {                                
        # Run this block if the wrong number of tweets were pulled 
        if (nrow(temp) >0) {
          pulled_tweets <- Sort_Bind_Tweets(pulled_tweets, temp[-1,])
        }
        break 
      }
    } 
    pulled_tweets <- subset(pulled_tweets, status_id > latest_tweet)
  }
  if (nrow(pulled_tweets) > 0) {
    pulled_tweets$in_conversation <- in_conversation
    tweeple$latest_tweet_id[k] <- pulled_tweets$status_id[1]
    all_pulled_tweets <- Sort_Bind_Tweets(all_pulled_tweets, pulled_tweets)
  } 
  if (!valid_pull) {
    print(glue("WARNING: No tweets pulled from {tweeple$user_name[k]}"))
  }
}
all_pulled_tweets$has_upthread     <- replicate(nrow(all_pulled_tweets),FALSE)
all_pulled_tweets$media_downloaded <- FALSE
tweet_stack <- Sort_Tweets(subset(all_pulled_tweets, in_conversation))
#
#
#
###    remaining <- subset(rate_limit(),query=="statuses/user_timeline")[["remaining"]]
###    print(glue("remaining is {remaining}"))
###
###for(this_name in screen_names) {
###  print(this_name)
###  print(nrow(subset(all_pulled_tweets,screen_name== this_name)))
###}
###
###screen_names <- unique(all_pulled_tweets$screen_name)
###colnames(tweet_stack)
#
#
# Here we combine old_tweets with those pulled tweets which are 
# not part of a conversation
if (old_tweets_exists){
  old_tweets <- Sort_Bind_Tweets(subset(all_pulled_tweets, !in_conversation), 
                                 old_tweets)
} else {
  old_tweets <- Sort_Tweets(subset(all_pulled_tweets, !in_conversation))
}
#
#
# This function finds the oldest tweet in tweet_stack (which is the data 
# frame of tweets that need to be processed), returns it, and removes it 
# from tweet_stack.
Excise_Oldest_Tweet_From_Stack <- function() {
  num_rows <- nrow(tweet_stack)
  return_tweet       <- tweet_stack[num_rows,]
  if (num_rows > 1) {
    tweet_stack <<- tweet_stack[1:(num_rows-1),]
  } else {
    tweet_stack <<- blank_tweet
  }
  return(return_tweet)
}
#
# This function removes a tweet from old_tweets given its index, 
# and then returns it
Excise_Old_Tweet <- function(given_index) {
  num_of_old_tweets <- nrow(old_tweets)
  tweet_to_return <- old_tweets[given_index,]
  if (given_index == 1) {
    old_tweets <<- old_tweets[-1,]
  } else if (given_index == num_of_old_tweets) {
    old_tweets <<- old_tweets[1:(num_of_old_tweets-1),]
  } else {
    first_chunk <- old_tweets[1:(given_index-1),]
    second_chunk <- old_tweets[(given_index+1):num_of_old_tweets,]
    old_tweets <<- rbind(first_chunk,second_chunk) 
  }
  return(tweet_to_return)
}
#
#
# This is the workhorse function.  Give it a user_id and status_id, and it 
# will check to see if we have the corresponding tweet in old_tweets, 
# download it and add it to the tweet_stack if we don't, and if we do have 
# it, it checks to see if we have the upthread, and if we don't then it adds 
# the new tweet to the tweet_stack.
Find_And_Add_To_Stack <- function(given_user_id, 
                                  given_status_id, 
                                  get_upthread) {
  tweet_index <- which(old_tweets$status_id == given_status_id)
  if (length(tweet_index)>0) {   
  # CASE 1: The needed tweet is in old_tweets
    tweet_index <- tweet_index[1]
    if ((!old_tweets$has_upthread[tweet_index]) & (get_upthread==TRUE) ) { 
    # Do the following because the quoted tweet is in 
    # old_tweets AND we don't have its upthread
      new_tweet    <-  Excise_Old_Tweet(tweet_index) 
      tweet_stack  <<- Sort_Bind_Tweets(tweet_stack, new_tweet)
    } 
  } else {
  # CASE 2: The reply-to tweet is NOT in old_tweets
    new_tweet <- blank_tweet
    new_tweet <- Pull_Timeline(given_user_id, given_status_id, 1)
    if (nrow(new_tweet) > 0) {
      if (new_tweet$status_id == given_status_id) { 
      # The above condition is satisfied as long as a new tweet was pulled; 
      # Twitter doesn't always return the desired tweet
        new_tweet$has_upthread     <- FALSE
        new_tweet$media_downloaded <- FALSE
        if (get_upthread==TRUE) {
          new_tweet$in_conversation  <- TRUE
          tweet_stack <<- Sort_Bind_Tweets(new_tweet,tweet_stack)
        } else {
          new_tweet$in_conversation  <- FALSE
          old_tweets <<- Sort_Bind_Tweets(new_tweet,old_tweets)
        }
      } 
    }
  }
}
#
#
# This is the big block of code which processes newly obtained tweets which 
# are part of a conversation (by downloading the upthread if needed)
while (nrow(tweet_stack)>0){
  print(glue("The number of tweets in the stack is {nrow(tweet_stack)}"))
  focus_tweet              <- Excise_Oldest_Tweet_From_Stack()                       
  focus_tweet$has_upthread <- TRUE
  #
  reply_to_status_id       <- focus_tweet$reply_to_status_id[1]
  reply_to_user_id         <- focus_tweet$reply_to_user_id[1]
  if (!is.na(reply_to_status_id)) {
  # Do the following because the focus_tweet is a reply
    Find_And_Add_To_Stack(reply_to_user_id, 
                          reply_to_status_id, 
                          get_upthread=TRUE)
  }
  quoted_status_id         <- focus_tweet$quoted_status_id[1]
  quoted_user_id           <- focus_tweet$quoted_user_id[1]
  if (!is.na(quoted_status_id)) {
  # Do the following because the focus_tweet is a quote
    Find_And_Add_To_Stack(quoted_user_id, 
                          quoted_status_id, 
                          get_upthread=FALSE)
  }
  # We are done with the focus_tweet, so move it to old_tweets
  old_tweets               <- Sort_Bind_Tweets(focus_tweet, old_tweets)
}
#
#
# This function is used to remove duplicate entries, but the execution here
# just removes the tweets corresponding to the indices that it is sent.
# Actually, I think this function might be needlessly complicated; something
# like 
#   old_tweets <<- old_tweets[(-1)*given.indices,]
# might be sufficient.
Remove_Duplicates <- function(given.indices) {
  given.indices <- sort(given.indices, decreasing = TRUE)
  for (i in 1:length(given.indices))
  {
    given_index <- given.indices[i]
    num_of_old_tweets <- nrow(old_tweets)
    if (given_index == 1) {
      old_tweets <<- old_tweets[-1,]
    } else if (given_index == num_of_old_tweets) {
      old_tweets <<- old_tweets[1:(num_of_old_tweets-1),]
    } else {
      first_chunk <- old_tweets[1:(given_index-1),]
      second_chunk <- old_tweets[(given_index+1):num_of_old_tweets,]
      old_tweets <<- rbind(first_chunk,second_chunk) 
    }
  }
}
#
#
# This block of code looks for duplicated status ids in old_tweets and removes 
# the duplicates
indices_of_all_duplicates <- integer(0)
duplicated_status_id <- 
  unique(old_tweets$status_id[duplicated(old_tweets$status_id)])
if (length(duplicated_status_id) >0) {
  for (k in 1:length(duplicated_status_id)) {
    temp <- subset(old_tweets, status_id==duplicated_status_id[k])
    replacement_tweet <- temp[1,]
    replacement_tweet$has_upthread    <- (sum(temp$has_upthread)>0)
    replacement_tweet$in_conversation <- (sum(temp$in_conversation)>0)
    indices_of_duplicates <- 
      sort(which(old_tweets$status_id==duplicated_status_id[k]), 
           decreasing=TRUE)
    old_tweets[indices_of_duplicates[1],] <- replacement_tweet
    indices_of_all_duplicates <- 
      c(indices_of_all_duplicates, indices_of_duplicates[-1])
  }
  if (length(indices_of_all_duplicates)>0) {
    Remove_Duplicates(indices_of_all_duplicates)
  }
}
#
#
# The following function tries to download the given media. It uses the
# tryCatch() function in case there is an error occurs during the download.
# This is necessary since otherwise an error in downloading would cause the 
# code to halt.  
Careful_Download <- function(given_urls, given_index) {
  success_counter <- 0
  for (this_url in given_urls) {
    download_filename <- str_extract(this_url, "[-0-9a-zA-Z_\\.]*$")
    pathname <- glue("./media/{download_filename}")
    tryCatch(
      expr = {
        download.file(this_url, pathname, mode='wb')
        success_counter <- success_counter + 1
      },
      error = function(e){
        message("Error: File probably not downloaded.")
        print(e)
      },
      warning = function(w){
        message("Error: File probably not downloaded.")
        print(w)
      }
    )
  }
  if (success_counter == length(given_urls)) {
    old_tweets[given_index,"media_downloaded"] <<- TRUE 
  } else {
    old_tweets[given_index,"media_downloaded"] <<- FALSE
  }
}
#
# 
# This block of code downloads media files if it needs to.
if (download_images) {
  tweets_with_media <- subset(old_tweets, 
                              (!is.na(ext_media_url) & !media_downloaded))
  status_ids_with_media <- tweets_with_media$status_id
  indices_with_media <- which(old_tweets$status_id %in% status_ids_with_media)
  num_of_tweets_with_media <- length(indices_with_media)
  if (num_of_tweets_with_media > 0){ 
    for (this_index  in indices_with_media){
      new_urls <- old_tweets$ext_media_url[[this_index]] 
      Careful_Download(new_urls, this_index)
    }
  }
}
#
# This records any modifications to the tweeple data frame, like updating
# the most recently downloaded tweet
write.csv(tweeple, tweeple_filename, row.names=FALSE)
#
#
# This writes all the tweets to a file.
saveRDS(old_tweets, tweet_rXiv_filename)
