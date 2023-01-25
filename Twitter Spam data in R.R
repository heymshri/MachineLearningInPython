head(TwitterSpam,10)

dim(TwitterSpam)
str(TwitterSpam)

spamfollower_spammer <- with(TwitterSpam, no_follower[label == "spammer"])
var(spamfollower_spammer)
spamfollower_nonspammer <- with(TwitterSpam, no_follower[label == "non-spammer"])
var(spamfollower_nonspammer)

sd(spamfollower_spammer)
sd(spamfollower_nonspammer)

summary(TwitterSpam$no_tweets)


hist(TwitterSpam$no_tweets,
     main="Histogram of Posted Tweets",
     xlab="Tweets number",
     col="red"
)