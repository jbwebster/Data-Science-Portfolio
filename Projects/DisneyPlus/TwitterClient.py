
# API Key: ekyvnWXgEx9m6fGAfLJ5hHKQO
# API Secret Key: vBAb2VW3ZO3Sh31aldEDLL1daGuyRtIvs54USsR2ZasuM4zNP2
# Access Token: 1195439042667712512-blIytqSaiMb7gec5P8KvlW7wG3hC5I
# Access Token Secret: SerKL1fQ0qCMwFSZzUkj8vGTOsiFWSHJx7SFZun0KxjMN

import re
import tweepy
from tweepy import OAuthHandler
from textblob import TextBlob
import json
from datetime import datetime


class TwitterClient(object):
  def __init__(self):
    # Keys and tokens from Twitter
    api_key = 'ekyvnWXgEx9m6fGAfLJ5hHKQO'
    api_secret = 'vBAb2VW3ZO3Sh31aldEDLL1daGuyRtIvs54USsR2ZasuM4zNP2'
    access_token = '1195439042667712512-blIytqSaiMb7gec5P8KvlW7wG3hC5I'
    access_secret = 'SerKL1fQ0qCMwFSZzUkj8vGTOsiFWSHJx7SFZun0KxjMN'
    
    try:
      # Create OAuthHandler object
      self.auth = OAuthHandler(api_key, api_secret)
      # Set token
      self.auth.set_access_token(access_token, access_secret)
      # Create tweepy object
      self.api = tweepy.API(self.auth)
    except:
      print('Error: Authentication failure')
    
    
  def get_tweets(self, query, count):
    tweets = []
    try:
      # Fetch tweets
      for tweet in tweepy.Cursor(self.api.search, q=query, lang='en', tweet_mode = 'extended', wait_on_rate_limit = True).items(count):
        if 'retweeted_status' in dir(tweet):
          full_text = tweet.retweeted_status.full_text
          retweet = "Retweet"
        else:
          full_text = tweet.full_text
          retweet = "Original Tweet"
        dict = {'Screen Name': tweet.user.screen_name,
                'User Name': tweet.user.name,
                'Text': full_text,
                'Retweet': retweet,
                'Created_at': tweet.created_at.strftime("%d-%b-%Y")
        }
        tweets.append(dict)
    except tweepy.TweepError as e:
      print('Error: ' + str(e))
    return tweets

def main():
  # Create TwitterClient object, as defined above
  print('Creating TwitterClient')
  api = TwitterClient()
  
  # Get Tweets
  print('Getting Tweets')
  tweets = api.get_tweets(query = "#DisneyPlus", count = 100000)
  
  # Confirm number of tweets received
  n = len(tweets)
  print('Number of tweets retrieved: ' + str(n))
  
  # Save Tweets
  print('Not Saving Tweets')
  #with open('DisneyPlus_tweets.txt', 'a') as f:
   # json.dump(tweets, f)
  print('Saving backup')
  with open('backup_DisneyPlus_SecondTry.txt', 'w') as f:
    json.dump(tweets, f)
  
if __name__ == "__main__": 
    # calling main function 
    main()   
    
