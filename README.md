# This is tweet_rXiv

### Introduction

The purpose of this code is to archive the tweets and/or conversations of a short list of users.  It makes use of the rtweet package, and requires creating a Twitter app and establishing authorization.  This is not as bad as it sounds, and the steps to achieve this can be found here. https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html  The rtweet package has some nice basic tools, so if you are not familiar with them you should check out the following brief introduction.  https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html


### Use of tweet_rXiv

(There is a TL;DR at the bottom)

Each time tweet_rXiv.R is run, it will read in the tweeple.csv file as a data frame, which holds the information about which users to archive, and which to follow conversations.  For example, the initial tweeple.csv file looks as follows:


"user_name","follow_conversation","latest_tweet_id"
"@realDonaldTrump",FALSE,NA
"@JoeBiden",TRUE,NA

which indicates that we want to archive Donald Trump's and Joe Biden's tweets, but we also want to archive Joe Biden's conversations. (More on that momentarily.)  The latest_tweet_id column is mostly used for internal purposes as a means to keep track of what the most recently archived tweet (status_id) is for that user.  An entry of NA specifies that there is no record of a most recently archived tweet for that user.  Note that if you want to archive more user's tweets, they need to be specified in this file using the above convention.  Remember: no spaces!

After loading the tweeple.csv file, tweet_rXiv will then open previously downloaded tweets stored in tweet_rXiv.rds if the file exists, and then it will download the tweets of the listed users.  If latest_tweet_id is NA then the number of initial tweets to be downloaded for that user is specified by the following line of code: 

num_of_initial_tweets    <- 40

You can increase this number up to 3200, which is maximum number the rtweet package can obtain via the get_timeline() function.  Actually, it's worse than this in the sense that you can't use the get_timeline() function to get any tweets older than your most recent 3200.  I think older tweets can be obtained if you pay Twitter for a premium account, but that's like $150/month, and I didn't think it was worth that cost just to archive my oldest tweets.

Thus in the sample case, tweet_rXiv will download @realDonaldTrump's and @JoeBiden's 40 most recent tweets, but then it attempts to acquire the conversations @JoeBiden is in.  That is, if one of @JoeBiden's tweets is a reply to somone else's tweet, tweet_rXiv will download that tweet as well, and if that tweet is a reply to someone else's tweet then tweet_rXiv will download that tweet too, etc.  The algorithm stops downloading tweets once the top of the thread is found, or when it tries to download a deleted or protected tweet, or the tweet of someone who has blocked you.  

A word of warning about downloading lots of threads: This can take a while.  Either Twitter or the rtweet package throttles the number of get_timeline() calls to 900 per 15 minutes (i.e. about 1 per second).  Each new tweet in a conversation thread is downloaded one at a time (rather than in bulk) so building many conversation threads will hit the rate limit set by Twitter.  Thus there is a counter in the code which keeps track of this, and pauses the process until the rate limit is reset.  Thus you may see the following warning appear:

Rate limit met. Execution paused for 16 minutes.

Note, if @JoeBiden is in a conversation with someone, and someone replies to him (who is not in the tweeple.csv file), tweet_rXiv will not download their tweet.  It only obtains up-threads of the users you list.

After downloading tweets, tweet_rXiv will then check to see if it should download images from those tweets.  This is determined by the following line:

download_images <- FALSE

When FALSE, it will not, when TRUE it will cycle through all tweets (including all older ones) and download the linked images.  This is the one point where it (presumably) matters that you are on a Mac or Linux based machine.  Specifically because the images are downloaded to the directory 

./media/

in the following line:

pathname <- glue("./media/{download_filename}")

I haven't tested this on a Windows machine yet, so presumably the syntax may need to be changed in that case.  The last thing tweet_rXiv will do is save updates made to the tweeple.csv file (like the ID of the most recently downloaded tweet) and save the newly acquired tweets with all the previously saved tweets to the tweet_rXiv.rds file.

To search through your archived tweets, first read them in:

```{r}
library(rtweet)
library(tidyverse)
library(tidytext)
library(glue)
library(tibble)
get_token()
archived_tweets <- readRDS("tweet_rXiv.rds")
```

The most useful column names are probably as follows:
```{r}
user_id        # ID number of a owner of a tweet
status_id      # ID of the tweet
screen_name    # Screen name of owner of a tweet (like realDonaldTrump or JoeBiden)
text           # Body text of the tweet
```

It's also worth noting that the status_id completely determines the url for the tweet.  For example,332308211321425920 is the status_id of Trump's claim about his IQ, and a link to the tweet can be constructed as follows:

https://twitter.com/AnyName/status/332308211321425920/

You can also search through your archived tweets with a function like the following.

```{r}
Search_Tweets <- function(given_tweets, given_regex, case_sensitive=FALSE) {
  tweet_text_to_search <- given_tweets$text
  if (case_sensitive == FALSE) {
    regex_string <- paste0(paste0("(?i)(",given_regex),")")
  } else {
    regex_string <- given_regex
  }
  tweet_selector <- str_detect(tweet_text_to_search, regex_string)
  to_return <- given_tweets[tweet_selector,]
  if (nrow(to_return) == 0) {
    print("No tweets found.")
  }
  return(to_return)
}
```

The function makes use of the str_detect() function to search through tweets, so the search syntax uses regex.  For example to search (without case sensitivity) for "critical" and "studies" use

```{r}
search_result_tweets <- Search_Tweets(archived_tweets,"(critical)&(studies)")
print(search_result_tweets$text)
```

and to do a case sensitive search for "Critical" or "Theory" use

```{r}
search_result_tweets <- Search_Tweets(archived_tweets,"(Critical)|(Studies)")
print(search_result_tweets$text)
```

### TL;DR

If you want archive the tweets of someone, add the following line to the tweeple.csv file:

"@realDonaldTrump",FALSE,NA


If you want to archive all the up-threads of someone, add the following line to the tweeple.csv file:

"@JoeBiden",TRUE,NA

If you want to download all the images of the tweets you archive, modify the tweet_rXiv.R line to the following:

download_images          <- TRUE

If you want to get the maximum number of tweets from a newly listed user in the tweeple.csv file, modigy the tweet_rXiv.R line to the following:

num_of_initial_tweets    <- 3200

Creating many upthreads may take quite a while.  If you are on a Windows machine, you may need to change the following line to the correct syntax for directory structure:

```{r}
pathname <- glue("./media/{download_filename}")
```

The tweet_rXiv.R script should be run regularly to collect archive tweets.  The following code block is used to read in previously archived tweets.

```{r}
library(rtweet)
library(tidyverse)
library(tidytext)
library(glue)
library(tibble)
get_token()
archived_tweets <- readRDS("tweet_rXiv.rds")
```
