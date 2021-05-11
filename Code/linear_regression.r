
options(scipen=200)
mo2Num <- function(x) match(tolower(x), tolower(month.abb))
for(country in c("us","in","br","au")){
  
  # Read covid_data and music_data
  covid_data <- read.csv(paste("covid_data/",country,"_week.csv",sep=""))
  music_data <- read.csv(paste("music_data/",country,"_waf.csv", sep=""))
  # Get the features of each music, I only selected 5
  music_sub <- music_data[c("Year", "Month", "Day", "danceability", "energy","speechiness","valence","tempo","liveness")]
  # get the month and day value in each row of Covid data (e.g. 21-Feb) 
  new_date <- strsplit(unlist(covid_data["Date"]),"-")
  feature_mean <- data.frame()
  for(dates in new_date){
    month <- mo2Num(dates[2])
    day <- as.numeric(dates[1])
    # find the mean value of each feature for each week
    features <- music_sub[which((music_sub$Year==2020) & (music_sub$Month==month) & (music_sub$Day==day)),]
    means = apply(features[4:ncol(features)],2,mean)
    feature_mean = rbind(feature_mean, means)
  }
  colnames(feature_mean) <- c("danceability", "energy","speechiness","valence","tempo","liveness")
  case_and_features <- cbind(covid_data, feature_mean)
  
  # filter out the lines that has NA value
  case_and_features <- na.omit(case_and_features)
  
  par(mfrow=c(2,3))
  # plot the relation between the weekly new cases and danceability
  for(i in c("danceability", "energy","speechiness","valence","tempo","liveness")){
    sformula = paste(i, "New.Cases",sep="~")
    relation <- lm(sformula, data=case_and_features)
    print("-------")
    print(country)
    print(i)
    print(summary(relation))
    print("-------")
    plot(unlist(case_and_features["New.Cases"]), unlist(case_and_features[i]), xlab="New Cases", ylab=i) 
    lines(unlist(case_and_features["New.Cases"]), relation$fitted.values)
  }
  
}


