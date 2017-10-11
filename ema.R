library(tidyverse)
library(nlme)
library(lubridate)
#setwd("C:/Users/mkeutmann/Dropbox/herbener/dissertation/analysis")
setwd("~/Dropbox/herbener/dissertation/analysis")
se <- function(x) sd(x)/sqrt(length(x))

# Reading, cleaning, and merging the EMA data --------------------------------------------
emaFull <- read.csv("EMA+Final+Version_September+11%2C+2017_14.06.csv")
ema <- read.csv("EMA+Final+Version_September+11%2C+2017_14.06.csv", skip = 4, header = F)
emaHeaders <- read.csv("EMA+Final+Version_September+11%2C+2017_14.06.csv", header = F, nrows = 1,
                       as.is = T)
colnames(ema) <- emaHeaders
emaC <- ema %>%
    rename(id = addcode)

bedtimeFull <- read.csv("EMA+Bedtime_September+8%2C+2017_12.44.csv")
bedtime <- read.csv("EMA+Bedtime_September+8%2C+2017_12.44.csv", skip = 4, header = F)
bedtimeHeaders <- read.csv("EMA+Bedtime_September+8%2C+2017_12.44.csv", header = F, nrows = 1,
                           as.is = T)
colnames(bedtime) <- bedtimeHeaders
bedtimeC <- filter(bedtime, !is.na(addcode)) %>%
  mutate(id              = addcode,
         happyYest       = Q33,
         excitedYest     = Q34,
         sadYest         = Q36,
         angryYest       = Q35,
         irritableYest   = Q37,
         upsetYest       = Q38,
         posYest         = (happyYest + excitedYest) / 2,
         negYest         = (sadYest + irritableYest + upsetYest) / 3,
         happyTom        = Q39,
         excitedTom      = Q40,
         sadTom          = Q41,
         angryTom        = Q42,
         irritableTom    = Q43,
         upsetTom        = Q44,
         posTom          = (happyTom + excitedTom) / 2,
         negTom          = (sadTom + irritableTom + upsetTom) / 3) %>%
    arrange(id, EndDate)

suppressWarnings(emaAllDay <- bind_rows(emaC, bedtimeC))

emaAllDayC <- emaAllDay %>%
    rename(enjoyPast       = Q3,
           enjoyPresent    = Q11,
           enjoyFuture     = Q15,
           activityCurrent = Q25,
           happy           = Q6,
           excited         = Q7,
           sad             = Q8,
           anxious         = Q9,
           irritable       = Q23,
           upset           = Q24,
           numSoc          = Q13,
           closeSoc        = Q29) %>%
    mutate(pos  = (happy + excited)/2,
           neg  = (sad + anxious + irritable + upset)/4,
           neg3 = (sad + irritable + upset)/3)

attach(emaAllDayC)
emaAllDayC$activityCurrent[activityCurrent == 1] <- "Inactive"
emaAllDayC$activityCurrent[activityCurrent == 2] <- "Self-care"
emaAllDayC$activityCurrent[activityCurrent == 3] <- "Household chores"
emaAllDayC$activityCurrent[activityCurrent == 4] <- "Errands/appointments"
emaAllDayC$activityCurrent[activityCurrent == 5] <- "Leisure"
emaAllDayC$activityCurrent[activityCurrent == 6] <- "Work/school"
emaAllDayC$activityCurrent[activityCurrent == 7] <- "Other"

emaAllDayC$closeSoc[closeSoc == 1] <- 1
emaAllDayC$closeSoc[closeSoc == 3] <- 2
emaAllDayC$closeSoc[closeSoc == 4] <- 3
emaAllDayC$closeSoc[closeSoc == 5] <- 4
emaAllDayC$closeSoc[closeSoc == 6] <- 5
detach(emaAllDayC)

# EMA cleaning by number of obs and time between obs ----------------------------------------------------------
comboByPerson <- group_by(emaAllDayC, id)

numObs <- summarise(comboByPerson, n())

qplot(numObs$`n()`, xlab = "Number of EMA observations", xlim = c(0, 80), binwidth = 2)
median(numObs$`n()`)

tooFew <- numObs$id[numObs$'n()' < 20]

##removing participants with too few total observations
emaAllDayC2 <- filter(emaAllDayC, !id %in% tooFew)

emaAllDayC2$cleanTime <- as.POSIXct(emaAllDayC2$EndDate)

emaAllDayC2 <- arrange(emaAllDayC2, id, cleanTime)

emaAllDayC2$timeSince <- c(NA, diff(emaAllDayC2$cleanTime, 1))

##removing observations that are too close together
emaAllDayC3 <- filter(emaAllDayC2, timeSince < 0 | timeSince > 2700) %>%
  group_by(id)

summary(summarise(emaAllDayC3, n()))

# Reading and cleaning demoClin data --------------------------------------
demoClin <- read.csv("demoClin - Sheet1.csv", na.strings = "-")
demoClinC <- demoClin %>%
  mutate(id = as.integer(id)) %>%
  group_by(group) %>%
  arrange(id)

# demoClin descriptives ---------------------------------------------------
summarise(demoClinC, N = n(),
          WTARmean = mean(WTARSS, na.rm = T),
          WTARSD = sd(WTARSS, na.rm = T),
          ageMean = mean(age),
          ageSD = sd(age),
          eduMean = mean(edu, na.rm = T),
          eduSD = sd(edu, na.rm = T))

summarise(demoClinC, HDRSMean = mean(HDRS),
          HDRSsd = sd(HDRS),
          TEPSTotalMean = mean(TEPSTotal, na.rm = T),
          TEPSTotalSD = sd(TEPSTotal, na.rm = T),
          TEPSAnticMean = mean(TEPSAntic, na.rm = T),
          TEPSAnticSD = sd(TEPSAntic, na.rm = T),
          TEPSConsumMean = mean(TEPSConsum, na.rm = T),
          TEPSConsumSD = sd(TEPSConsum, na.rm = T))

summarise(demoClinC,
          HQLSTotalMean = mean(HQLSTotal, na.rm = T),
          HQLSTotalSD = sd(HQLSTotal, na.rm = T),
          ChapmanTotalMean = mean(ChapmanTotal, na.rm = T),
          ChapmanTotalSD = sd(ChapmanTotal, na.rm = T))

summarise(demoClinC,
          PANNSTotalMean = mean(PANNSTot, na.rm = T),
          PANNSTotSD = sd(PANNSTot, na.rm = T),
          PANNSPosMean = mean(PANNSPos, na.rm = T),
          PANNSPosSD = sd(PANNSPos, na.rm = T),
          PANNSNegMean = mean(PANNSNeg, na.rm = T),
          PANNSNegSD = sd(PANNSNeg, na.rm = T),
          PANNSGenMean = mean(PANNSGen, na.rm = T),
          PANNSGenSD = sd(PANNSGen, na.rm = T))

table(demoClinC$group, demoClinC$sex)
table(demoClinC$group, demoClinC$race)
t.test(demoClinC$WTARSS ~ demoClinC$group)
t.test(demoClinC$age ~ demoClinC$group)
t.test(demoClinC$edu ~ demoClinC$group)
chisq.test(table(demoClinC$group, demoClinC$sex))
chisq.test(table(demoClinC$group, demoClinC$race))
t.test(demoClinC$HDRS ~ demoClinC$group)
t.test(demoClinC$HQLSTotal ~ demoClinC$group)
t.test(demoClinC$TEPSConsum ~ demoClinC$group)
t.test(demoClinC$TEPSAntic ~ demoClinC$group)
t.test(demoClinC$TEPSTotal ~ demoClinC$group)
t.test(demoClinC$HQLSTotal ~ demoClinC$group)
t.test(demoClinC$ChapmanTotal ~ demoClinC$group)

# Merge EMA and demoClin data and calculate time leads/lags -------------------------------
finalData <- left_join(emaAllDayC3, demoClinC) %>%
    mutate(prevPred         = lag(enjoyFuture, 1),
           nextRecall       = lead(enjoyPast, 1),
           predictionBias   = prevPred - enjoyPresent,
           recallBias       = nextRecall - enjoyPresent) %>%
    select(-c(RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference,
              DistributionChannel, Q18_Browser, Q18_Version, Q18_Resolution, IPAddress,
              ResponseId, LocationLatitude, LocationLongitude, Status))

finalDataByPerson <- group_by(finalData, id, group)
numObsWithGroup <- summarise(finalDataByPerson, n())
t.test(numObsWithGroup$'n()' ~ numObsWithGroup$group)
numOb
summarise(numObsWithGroup, sdN = sd('n()'))

# Activity Pie Chart ------------------------------------------------------
comboByGroupOnly <- group_by(finalData, group)
activityTable <- count(comboByGroupOnly, activityCurrent)
activityTableC <- activityTable %>%
  filter(!is.na(group), !is.na(activityCurrent))

chisq.test(table(activityTableC$activityCurrent, activityTableC$activityCurrent))

slices <- activityTableC$n[activityTableC$group == "HC"]
lbls   <- activityTableC$activityCurrent[activityTableC$group == "HC"]
pct    <- round(slices/sum(slices)*100)
lbls   <- paste(lbls, pct) # add percents to labels 
lbls   <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices, labels = lbls, main="Healthy Controls", col = rainbow(7))

slices <- activityTableC$n[activityTableC$group == "SZ"]
lbls   <- activityTableC$activityCurrent[activityTableC$group == "SZ"]
pct    <- round(slices/sum(slices)*100)
lbls   <- paste(lbls, pct) # add percents to labels 
lbls   <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices, labels = lbls, main="Schizophrenia", col = rainbow(7))

# Mean level enjoyment ----------------------------------------------------------
finalDataByPerson <- group_by(finalData, id, group)

enjoySummaryByPerson <- summarise(finalDataByPerson,
                                  enjoyPastMean    = mean(enjoyPast, na.rm = T),
                                  enjoyPresentMean = mean(enjoyPresent, na.rm = T),
                                  enjoyFutureMean  = mean(enjoyFuture, na.rm = T))

chartData <- gather(enjoySummaryByPerson, key = "key", value = "value", -c(group, id)) %>%
  group_by(group, key) %>%
  summarise(mean = mean(value, na.rm = T), se = se(value)) %>%
  mutate(key = as.factor(key))

chartData$key <- recode(chartData$key, enjoyFutureMean = "Future")
chartData$key <- recode(chartData$key, enjoyPastMean = "Past")
chartData$key <- recode(chartData$key, enjoyPresentMean = "Present")
chartData$key <- factor(chartData$key, levels = c("Past", "Present", "Future"))

ggplot(data = chartData, aes(x = key, y = mean, group = group, colour = group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1)

t.test(enjoySummaryByPerson$enjoyPastMean ~ enjoySummaryByPerson$group)
t.test(enjoySummaryByPerson$enjoyPresentMean ~ enjoySummaryByPerson$group)
t.test(enjoySummaryByPerson$enjoyFutureMean ~ enjoySummaryByPerson$group)

enjoySummaryByPersonLong <- gather(enjoySummaryByPerson, key = "key", value = "value", -c(group, id))
anova <- aov(value ~ key*group + Error(id/key) + group, data = enjoySummaryByPersonLong)
summary(anova)

# Mean level bias -----------------------------------------------------------------------------
finalDataByPerson <- group_by(finalData, id, group)

biasSummaryByPerson <- summarise(finalDataByPerson,
                                 predictionBiasMean = mean(predictionBias, na.rm = T),
                                 recallBiasMean = mean(recallBias, na.rm = T))

chartData <- gather(biasSummaryByPerson, key = "key", value = "value", -c(group, id)) %>%
    group_by(group, key) %>%
    summarise(mean = mean(value, na.rm = T), se = se(value)) %>%
    mutate(key = as.factor(key))

chartData$key <- recode(chartData$key, predictionBiasMean = "Prediction Bias")
chartData$key <- recode(chartData$key, recallBiasMean = "Recall Bias")
chartData$key <- factor(chartData$key, levels = c("Prediction Bias", "Recall Bias"))

ggplot(data = chartData, aes(x = key, y = mean, group = group, colour = group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1)

t.test(biasSummaryByPerson$predictionBiasMean ~ biasSummaryByPerson$group)
t.test(biasSummaryByPerson$recallBiasMean ~ biasSummaryByPerson$group)

biasSummaryByPersonLong <- gather(biasSummaryByPerson, key = "key", value = "value", -c(group, id))
anova <- aov(value ~ key*group + Error(id/key) + group, data = biasSummaryByPersonLong)
summary(anova)

biasSummaryByPersonHC <- filter(biasSummaryByPerson, group == "HC")
biasSummaryByPersonSZ <- filter(biasSummaryByPerson, group == "SZ")

t.test(biasSummaryByPersonHC$predictionBiasMean)
t.test(biasSummaryByPersonSZ$predictionBiasMean)
t.test(biasSummaryByPersonHC$recallBiasMean)
t.test(biasSummaryByPersonSZ$recallBiasMean)

# Mean level mood ---------------------------------------------------------
moodSummaryByPerson <- summarise(finalDataByPerson, meanPos = mean(pos, na.rm = T),
                                 meanNeg = mean(neg, na.rm = T),
                                 meanHappy = mean(happy, na.rm = T),
                                 meanExcited = mean(excited, na.rm = T),
                                 meanSad = mean(sad, na.rm = T),
                                 meanAnxious = mean(anxious, na.rm = T),
                                 meanIrritable = mean(irritable, na.rm = T),
                                 meanUpset = mean(upset, na.rm = T))

moodSummaryByPersonC <- filter(moodSummaryByPerson, !is.na(group))

moodSummaryByGroup <- group_by(moodSummaryByPersonC, group)

chartData2 <- gather(moodSummaryByPersonC, key = "key", value = "value", -c(group, id)) %>%
  group_by(group, key) %>%
  summarise(mean = mean(value, na.rm = T), se = se(value)) %>%
  mutate(key = as.factor(key))

chartData2$key <- recode(chartData2$key, meanNeg = "Negative")
chartData2$key <- recode(chartData2$key, meanPos = "Positive")
chartData2$key <- recode(chartData2$key, meanHappy = "Happy")
chartData2$key <- recode(chartData2$key, meanExcited = "Excited")
chartData2$key <- recode(chartData2$key, meanSad = "Sad")
chartData2$key <- recode(chartData2$key, meanAnxious = "Anxious")
chartData2$key <- recode(chartData2$key, meanIrritable = "Irritable")
chartData2$key <- recode(chartData2$key, meanUpset = "Upset")
chartData2$key <- factor(chartData2$key, levels = c("Anxious", "Irritable", "Sad", "Upset", "Negative",
                                                    "Happy", "Excited", "Positive"))

ggplot(data = chartData2, aes(x = key, y = mean, group = group, colour = group), xlab = "Emotion") +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
  xlab("Emotion") +
    ylim(1, 3.25)

t.test(moodSummaryByPersonC$meanPos ~ moodSummaryByPersonC$group)
t.test(moodSummaryByPersonC$meanNeg ~ moodSummaryByPersonC$group)
t.test(moodSummaryByPersonC$meanAnxious ~ moodSummaryByPersonC$group)

model <- lme(fixed = pos ~ group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = neg ~ group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = happy ~ group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = excited ~ group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = anxious ~ group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = irritable ~ group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = sad ~ group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = upset ~ group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)


# Social interactions --------------------------------------------------------------
socSummaryByPerson <- summarise(finalDataByPerson, meanNumSoc = mean(numSoc, na.rm = T),
                                meanCloseSoc = mean(closeSoc, na.rm = T))

socSummaryByPersonC <- filter(socSummaryByPerson, !is.na(group))

socSummaryByGroup <- group_by(socSummaryByPersonC, group)

t.test(socSummaryByPersonC$meanNumSoc ~ socSummaryByPersonC$group)
t.test(socSummaryByPersonC$meanCloseSoc ~ socSummaryByPersonC$group)

ggplot(data = socSummaryByPersonC, aes(x = meanNumSoc)) +
    geom_histogram(data = filter(socSummaryByPersonC, group == "HC"), fill = "red", alpha = 0.2) +
    geom_histogram(data = filter(socSummaryByPersonC, group == "SZ"), fill = "blue", alpha = 0.2) +
    theme()

ggplot(socSummaryByPersonC, aes(x = meanCloseSoc)) +
    geom_histogram(data = filter(socSummaryByPersonC, group == "HC"), fill = "red", alpha = 0.2) +
    geom_histogram(data = filter(socSummaryByPersonC, group == "SZ"), fill = "blue", alpha = 0.2)

model <- lme(fixed = numSoc ~ group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = closeSoc ~ group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

#Does social interaction affect enjoyment?
model <- lme(fixed = enjoyPresent ~ numSoc, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = enjoyPresent ~ numSoc*group, random = ~1|id, data = finalData,
             na.action = na.omit)
summary(model)

model <- lme(fixed = enjoyPresent ~ closeSoc, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = enjoyPresent ~ closeSoc*group, random = ~1|id, data = finalData,
             na.action = na.omit)
summary(model)

#Does number of social interaction partners affect mood?
model <- lme(fixed = pos ~ numSoc, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = pos ~ numSoc*group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = neg ~ numSoc, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = neg ~ numSoc*group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

#Does closeness of social interaction partners affect mood?
model <- lme(fixed = pos ~ closeSoc, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = pos ~ closeSoc*group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = neg ~ closeSoc, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = neg ~ closeSoc*group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

# Other analyses ----------------------------------------------------------
ggplot(finalData, aes(enjoyPresent, prevPred)) +
    geom_jitter(aes(color = group), width = .3, height = .3)
cor(finalData$enjoyPresent, finalData$prevPred, use = "pairwise.complete.obs")

# HLM 2-level models -----------------------------------------------------------------

# No predictors
dataModel1 <- filter(finalData, !is.na(enjoyPresent))
model1 <- lme(fixed = enjoyPresent~1, random = ~1|id, data = dataModel1)
summary(model1)
intervals(model1)

#Previous prediction as fixed predictor
dataModel2 <- finalData %>%
  mutate(currDate = as.Date(cleanTime),
         prevDate = lag(currDate, 1),
         dateDiff = currDate - prevDate) %>%
  filter(!is.na(enjoyPresent), !is.na(prevPred), dateDiff == 0)
model2 <- lme(fixed = enjoyPresent~prevPred, random = ~1|id, data = dataModel2)
summary(model2)

#Previous prediction as fixed predictor (with centered variables)
dataModel2$enjoyPresentC <- dataModel2$enjoyPresent - mean(dataModel2$enjoyPresent)
dataModel2$prevPredC <- dataModel2$prevPred - mean(dataModel2$prevPred)

model2C <- lme(fixed = enjoyPresentC~prevPredC, random = ~1|id, data = dataModel2)
summary(model2C)

#Next recall as fixed predictor
dataModel3 <- finalData %>%
  mutate(currDate = as.Date(cleanTime),
         nextDate = lead(currDate, 1),
         dateDiff = currDate - nextDate) %>%
  filter(!is.na(enjoyPresent), !is.na(nextRecall), dateDiff == 0)
model3 <- lme(fixed = enjoyPresent~nextRecall, random = ~1|id, data = dataModel3)
summary(model3)

#Random slopes and intercepts: Previous prediction
dataModel4 <- finalData %>%
  mutate(currDate = as.Date(cleanTime),
         prevDate = lag(currDate, 1),
         dateDiff = currDate - prevDate) %>%
  filter(!is.na(enjoyPresent), !is.na(prevPred), dateDiff == 0)
model4 <- lme(fixed = enjoyPresent~prevPred, random = ~prevPred|id, data = dataModel4)
summary(model4)

#Random slopes and intercepts: Next recall
dataModel5 <- finalData %>%
  mutate(currDate = as.Date(cleanTime),
         nextDate = lead(currDate, 1),
         dateDiff = currDate - nextDate) %>%
  filter(!is.na(enjoyPresent), !is.na(nextRecall), dateDiff == 0)
model5 <- lme(fixed = enjoyPresent~nextRecall, random = ~nextRecall|id, data = dataModel5)
summary(model5)

# HLM 2-level models with group as fixed effect ---------------------------
# Group only
dataModel11A <- filter(finalData, !is.na(enjoyPresent))
model11A <- lme(fixed = enjoyPresent~group, random = ~1|id, data = dataModel11A)
summary(model11A)

dataModel11B <- filter(finalData, !is.na(enjoyPast))
model11B <- lme(fixed = enjoyPast~group, random = ~1|id, data = dataModel11B)
summary(model11B)

dataModel11C <- filter(finalData, !is.na(enjoyFuture))
model11C <- lme(fixed = enjoyFuture~group, random = ~1|id, data = dataModel11C)
summary(model11C)

# Previous prediction by group interaction
dataModel12 <- finalData %>%
  mutate(currDate = as.Date(cleanTime),
         prevDate = lag(currDate, 1),
         dateDiff = currDate - prevDate) %>%
  filter(!is.na(enjoyPresent), !is.na(prevPred), dateDiff == 0)
model12 <- lme(fixed = enjoyPresent~prevPred*group, random = ~1|id, data = dataModel12)
summary(model12)

# Previous prediction - no group interaction
dataModel13 <- finalData %>%
    mutate(currDate = as.Date(cleanTime),
           prevDate = lag(currDate, 1),
           dateDiff = currDate - prevDate) %>%
    filter(!is.na(enjoyPresent), !is.na(prevPred), dateDiff == 0)
model13 <- lme(fixed = enjoyPresent~prevPred + group, random = ~1|id, data = dataModel13)
summary(model13)

# Next recall by group interaction
dataModel14 <- finalData %>%
    mutate(dateTime    = ymd_hms(EndDate),
           currDate    = as.Date(EndDate),
           currHour    = hour(dateTime),
           dateRevised = ifelse(currHour < 3, currDate - 1, currDate),
           nextDate    = lag(dateRevised, 1),
           dateDiff    = currDate - nextDate) %>%
    filter(!is.na(enjoyPresent), !is.na(nextRecall), dateDiff == 0)
model14 <- lme(fixed = enjoyPresent~nextRecall*group, random = ~1|id, data = dataModel14)
summary(model14)

# Next recall: no group interaction
dataModel15 <- finalData %>%
    mutate(dateTime    = ymd_hms(EndDate),
           currDate    = as.Date(EndDate),
           currHour    = hour(dateTime),
           dateRevised = ifelse(currHour < 3, currDate - 1, currDate),
           nextDate    = lag(dateRevised, 1),
           dateDiff    = currDate - nextDate) %>%
    filter(!is.na(enjoyPresent), !is.na(nextRecall), dateDiff == 0)
model15 <- lme(fixed = enjoyPresent~nextRecall + group, random = ~1|id, data = dataModel15)
summary(model15)

# Bedtime mood forecast and memory: data cleaning  -----------------------------------------------
bedtimeC2 <- bedtimeC %>%
    rename(happy            = Q6,
           excited          = Q7,
           sad              = Q8,
           anxious          = Q9,
           irritable        = Q23,
           upset            = Q24,
           numSoc           = Q13,
           closeSoc         = Q29) %>%
    mutate(pos              = (happy + excited)/2,
           neg              = (sad + anxious + irritable + upset)/4,
           neg3             = (sad + irritable + upset)/3,
           dateTime         = ymd_hms(EndDate),
           currDate         = as.Date(EndDate),
           currHour         = hour(dateTime),
           dateRevised      = ifelse(currHour < 3, currDate - 1, currDate),
           nextDate         = lead(dateRevised, 1),
           prevDate         = lag(dateRevised, 1),
           prevPosPred      = lag(posTom, 1),
           prevNegPred      = lead(negTom, 1),
           nextPosRecall    = lag(posYest, 1),
           nextNegRecall    = lead(negYest, 1),
           posEmoPredBias   = posTom - pos,
           negEmoPredBias   = negTom - neg,
           posEmoRecallBias = posYest - pos,
           negEmoRecallBias = negYest - neg)
           
bedtimeC3 <- left_join(bedtimeC2, demoClinC)

# Bedtime mood forecast and memory stats (incl. HLM models) ---------------------------------------
ggplot(data = bedtimeC3, aes(x = posYest)) +
    geom_histogram(data = filter(bedtimeC3, group == "HC"), fill = "red", alpha = 0.2) +
    geom_histogram(data = filter(bedtimeC3, group == "SZ"), fill = "blue", alpha = 0.2) +
    theme()

# Group only
dataModel15A <- filter(bedtimeC3, !is.na(pos))
model15A <- lme(fixed = pos~group, random = ~1|id, data = dataModel15A)
summary(model15A)

dataModel15B <- filter(bedtimeC3, !is.na(neg3))
model15B <- lme(fixed = neg3~group, random = ~1|id, data = dataModel15B)
summary(model15B)

dataModel15C <- filter(bedtimeC3, !is.na(posYest))
model15C <- lme(fixed = posYest~group, random = ~1|id, data = dataModel15C)
summary(model15C)

dataModel15D <- filter(bedtimeC3, !is.na(negYest))
model15D <- lme(fixed = negYest~group, random = ~1|id, data = dataModel15D)
summary(model15D)

dataModel15E <- filter(bedtimeC3, !is.na(posTom))
model15E <- lme(fixed = posTom~group, random = ~1|id, data = dataModel15E)
summary(model15E)

dataModel15F <- filter(bedtimeC3, !is.na(negTom))
model15F <- lme(fixed = negTom~group, random = ~1|id, data = dataModel15F)
summary(model15F)

## Positive emotion (previous night's prediction with group interaction)
dataModel16 <- bedtimeC3 %>%
    mutate(dateDiff = dateRevised - prevDate) %>%
    filter(!is.na(pos), !is.na(prevPosPred), dateDiff == 1)

model16 <- lme(fixed = pos~prevPosPred*group, random = ~1|id, data = dataModel16)
summary(model16)

## Positive emotion (previous night's prediction - no group interaction)
dataModel17 <- bedtimeC3 %>%
    mutate(dateDiff = dateRevised - prevDate) %>%
    filter(!is.na(pos), !is.na(prevPosPred), dateDiff == 1)
model17 <- lme(fixed = pos~prevPosPred + group, random = ~1|id, data = dataModel17)
summary(model17)

## Negative emotion (previous night's prediction with group interaction)
dataModel18 <- bedtimeC3 %>%
    mutate(dateDiff = dateRevised - prevDate) %>%
    filter(!is.na(neg), !is.na(prevNegPred), dateDiff == 1)
model18 <- lme(fixed = neg~prevNegPred*group, random = ~1|id, data = dataModel18)
summary(model18)

## Negative emotion (previous night's prediction - no group interaction)
dataModel19 <- bedtimeC3 %>%
    mutate(dateDiff = dateRevised - prevDate) %>%
    filter(!is.na(neg), !is.na(prevNegPred), dateDiff == 1)
model19 <- lme(fixed = neg~prevNegPred + group, random = ~1|id, data = dataModel19)
summary(model19)

## Positive emotion (next night's recall with group interaction)
dataModel20 <- bedtimeC3 %>%
    mutate(dateDiff = nextDate - dateRevised) %>%
    filter(!is.na(pos), !is.na(nextPosRecall), dateDiff == 1)
model20 <- lme(fixed = pos~nextPosRecall*group, random = ~1|id, data = dataModel20)
summary(model20)

## Positive emotion (next night's recall - no group interaction)
dataModel21 <- bedtimeC3 %>%
    mutate(dateDiff = nextDate - dateRevised) %>%
    filter(!is.na(pos), !is.na(nextPosRecall), dateDiff == 1)
model21 <- lme(fixed = pos~nextPosRecall + group, random = ~1|id, data = dataModel21)
summary(model21)

## Negative emotion (next night's recall with group interaction)
dataModel22 <- bedtimeC3 %>%
    mutate(dateDiff = nextDate - dateRevised) %>%
    filter(!is.na(neg), !is.na(nextNegRecall), dateDiff == 1)
model22 <- lme(fixed = neg~nextNegRecall*group, random = ~1|id, data = dataModel22)
summary(model20)

## Negative emotion (next night's recall - no group interaction)
dataModel23 <- bedtimeC3 %>%
    mutate(dateDiff = nextDate - dateRevised) %>%
    filter(!is.na(neg), !is.na(nextNegRecall), dateDiff == 1)
model23 <- lme(fixed = neg~nextNegRecall + group, random = ~1|id, data = dataModel23)
summary(model23)

# Checking stuff ------------------------------------------------------------------------------
testAllDayC2 <- select(emaAllDayC2, id, EndDate, enjoyFuture, enjoyPresent, timeSince)
testAllDayC3 <- select(emaAllDayC3, id, EndDate, enjoyFuture, enjoyPresent, timeSince)
testFinalData <- select(finalData, id, EndDate, enjoyFuture, enjoyPresent, prevPred, timeSince)
testModel2 <- select(dataModel2, id, EndDate, enjoyFuture, enjoyPresent, prevPred, currDate,
                     prevDate, dateDiff)

# Pleasure bias HLM models -----------------------------------------------------------------------
dataModel16A <- filter(finalData, !is.na(predictionBias))
model16A <- lme(fixed = predictionBias~group, random = ~1|id, data = dataModel16A)
summary(model16A)

dataModel16B <- filter(finalData, !is.na(recallBias))
model16B <- lme(fixed = recallBias~group, random = ~1|id, data = dataModel16B)
summary(model16B)

finalDataHC <- filter(finalData, group == "HC")
finalDataSZ <- filter(finalData, group == "SZ")

t.test(finalDataHC$predictionBias)
t.test(finalDataHC$recallBias)
t.test(finalDataSZ$predictionBias)
t.test(finalDataSZ$recallBias)

# Bedtime bias (incl. HLM models) ---------------------------------------------------------------
dataModel17A <- filter(bedtimeC3, !is.na(posEmoPredBias))
model17A <- lme(fixed = posEmoPredBias~group, random = ~1|id, data = dataModel17A)
summary(model17A)

dataModel17B <- filter(bedtimeC3, !is.na(negEmoPredBias))
model17B <- lme(fixed = negEmoPredBias~group, random = ~1|id, data = dataModel17B)
summary(model17B)

dataModel17C <- filter(bedtimeC3, !is.na(posEmoRecallBias))
model17C <- lme(fixed = posEmoRecallBias~group, random = ~1|id, data = dataModel17C)
summary(model17C)

dataModel17D <- filter(bedtimeC3, !is.na(negEmoRecallBias))
model17D <- lme(fixed = negEmoRecallBias~group, random = ~1|id, data = dataModel17D)
summary(model17D)

bedByPerson <- bedtimeC3 %>%
    group_by(id, group) %>%
    summarise(meanPosEmoPredBias = mean(posEmoPredBias, na.rm = T),
              meanNegEmoPredBias = mean(negEmoPredBias, na.rm = T),
              meanPosEmoRecallBias = mean(posEmoRecallBias, na.rm = T),
              meanNegEmoRecallBias = mean(negEmoRecallBias, na.rm = T))

t.test(bedByPerson$meanPosEmoPredBias ~ bedByPerson$group)
t.test(bedByPerson$meanNegEmoPredBias ~ bedByPerson$group)
t.test(bedByPerson$meanPosEmoRecallBias ~ bedByPerson$group)
t.test(bedByPerson$meanNegEmoRecallBias ~ bedByPerson$group)

chartData <- gather(bedByPerson, key = "key", value = "value", -c(group, id)) %>%
    group_by(group, key) %>%
    summarise(mean = mean(value, na.rm = T), se = se(value)) %>%
    mutate(key = as.factor(key))

chartData$key <- recode(chartData$key, meanPosEmoPredBias = "Positive Prediction")
chartData$key <- recode(chartData$key, meanNegEmoPredBias = "Negative Prediction")
chartData$key <- recode(chartData$key, meanPosEmoRecallBias = "Positive Recall")
chartData$key <- recode(chartData$key, meanNegEmoRecallBias = "Negative Recall")
#chartData$key <- factor(chartData$key, levels = c("Past", "Present", "Future"))

ggplot(data = chartData, aes(x = key, y = mean, group = group, colour = group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
    xlab("Type of Bias")

bedByPersonHC <- filter(bedByPerson, group == "HC")
bedByPersonSZ <- filter(bedByPerson, group == "SZ")

t.test(bedByPersonHC$meanPosEmoPredBias)
t.test(bedByPersonHC$meanNegEmoPredBias)
t.test(bedByPersonHC$meanPosEmoRecallBias)
t.test(bedByPersonHC$meanNegEmoRecallBias)
t.test(bedByPersonSZ$meanPosEmoPredBias)
t.test(bedByPersonSZ$meanNegEmoPredBias)
t.test(bedByPersonSZ$meanPosEmoRecallBias)
t.test(bedByPersonSZ$meanNegEmoRecallBias)

# Affective forecasting: how many days --------------------------------------------------------
finalDataByPersonByDayByGroup <- finalData %>%
    mutate(currDate = as.Date(EndDate)) %>% 
    group_by(currDate, id, group)

maxEmo <- summarise(finalDataByPersonByDayByGroup, maxHappy     = max(happy, na.rm = T),
                                                   maxExcited   = max(excited, na.rm = T),
                                                   maxSad       = max(sad, na.rm = T),
                                                   maxAnxious   = max(anxious, na.rm = T),
                                                   maxIrritable = max(irritable, na.rm = T),
                                                   maxUpset     = max(upset, na.rm = T)) %>%
    mutate(veryHappy     = ifelse(maxHappy >= 4, 1, 0),
           veryExcited   = ifelse(maxExcited >= 4, 1, 0),
           verySad       = ifelse(maxSad >= 4, 1, 0),
           veryAnxious   = ifelse(maxAnxious >= 4, 1, 0),
           veryIrritable = ifelse(maxIrritable >= 4, 1, 0),
           veryUpset     = ifelse(maxUpset >= 4, 1, 0))

numVery <- maxEmo %>%
    group_by(id, group) %>%
    summarise(numDays = n(),
              propDaysVeryHappy     = sum(veryHappy)/numDays,
              propDaysVeryExcited   = sum(veryExcited)/numDays,
              propDaysVerySad       = sum(verySad)/numDays,
              propDaysVeryAnxious   = sum(veryAnxious)/numDays,
              propDaysVeryIrritable = sum(veryIrritable)/numDays,
              propDaysVeryUpset     = sum(veryUpset)/numDays,
              propDaysPos           = (propDaysVeryHappy + propDaysVeryExcited) / 2,
              propDaysNeg           = (propDaysVerySad + propDaysVeryAnxious + propDaysVeryAnxious + 
                                       propDaysVeryIrritable) / 4)

hist(numVery$propDaysVeryHappy, breaks = 10)
ggplot(data = numVery, aes(x = propDaysVeryHappy)) +
    geom_histogram(data = filter(numVery, group == "HC"), fill = "red", alpha = 0.4) +
    geom_histogram(data = filter(numVery, group == "SZ"), fill = "blue", alpha = 0.4) +
    ylim(0, 8)

t.test(numVery$propDaysVeryHappy ~ numVery$group)
t.test(numVery$propDaysVeryExcited ~ numVery$group)
t.test(numVery$propDaysVerySad ~ numVery$group)
t.test(numVery$propDaysVeryAnxious ~ numVery$group)
t.test(numVery$propDaysVeryIrritable ~ numVery$group)
t.test(numVery$propDaysVeryUpset ~ numVery$group)
t.test(numVery$propDaysPos ~ numVery$group)
t.test(numVery$propDaysNeg ~ numVery$group)

prePost <- read.csv("Pre and Post EMA Questionnaires - 2017-09-15.csv") %>%
    rename(id = ccmid)
moodForecast <- left_join(numVery, prePost) %>%
    mutate(propExpectHappy     = expectHappy / 6,
           propExpectExcited   = expectExcited / 6,
           propExpectSad       = expectSad / 6,
           propExpectAnxious   = expectAnxious / 6,
           propExpectIrritable = expectIrritable / 6,
           propExpectUpset     = expectUpset / 6,
           propExpectPos       = (propExpectHappy + propExpectExcited) / 2,
           propExpectNeg       = (propExpectSad + propExpectAnxious + propExpectIrritable +
                                  propExpectUpset) / 4,
           happyDiff           = propExpectHappy - propDaysVeryHappy,
           excitedDiff         = propExpectExcited - propDaysVeryExcited,
           sadDiff             = propExpectSad - propDaysVerySad,
           anxiousDiff         = propExpectAnxious - propDaysVeryAnxious,
           irritableDiff       = propExpectIrritable - propDaysVeryIrritable,
           upsetDiff           = propExpectUpset - propDaysVeryUpset,
           posDiff             = propExpectPos - propDaysPos,
           negDiff             = propExpectNeg - propDaysNeg)

t.test(moodForecast$propExpectHappy ~ moodForecast$group)
t.test(moodForecast$propExpectExcited ~ moodForecast$group)
t.test(moodForecast$propExpectSad ~ moodForecast$group)
t.test(moodForecast$propExpectAnxious ~ moodForecast$group)
t.test(moodForecast$propExpectIrritable ~ moodForecast$group)
t.test(moodForecast$propExpectUpset ~ moodForecast$group)
t.test(moodForecast$propExpectPos ~ moodForecast$group)
t.test(moodForecast$propExpectNeg ~ moodForecast$group)

t.test(moodForecast$happyDiff ~ moodForecast$group)
t.test(moodForecast$excitedDiff ~ moodForecast$group)
t.test(moodForecast$sadDiff ~ moodForecast$group)
t.test(moodForecast$anxiousDiff ~ moodForecast$group)
t.test(moodForecast$irritableDiff ~ moodForecast$group)
t.test(moodForecast$upsetDiff ~ moodForecast$group)
t.test(moodForecast$posDiff ~ moodForecast$group)
t.test(moodForecast$negDiff ~ moodForecast$group)

manovaExpect <- manova(cbind(propExpectHappy, propExpectExcited, propExpectSad, propExpectAnxious,
                       propExpectIrritable, propExpectUpset) ~ group, data = moodForecast)
summary(manovaExpect)
summary.aov(manovaExpect)

manovaExpectPosNeg <- manova(cbind(propExpectPos, propExpectNeg) ~ group, data = moodForecast)
summary(manovaExpectPosNeg)
summary.aov(manovaExpectPosNeg)

manovaDaysVery <- manova(cbind(propDaysVeryHappy, propDaysVeryExcited, propDaysVerySad,
                               propDaysVeryAnxious, propDaysVeryIrritable, propDaysVeryUpset)
                         ~ group, data = moodForecast)
summary(manovaDaysVery)
summary.aov(manovaDaysVery)

manovaDaysVeryPosNeg <- manova(cbind(propDaysPos, propDaysNeg) ~ group, data = moodForecast)
summary(manovaDaysVeryPosNeg)
summary.aov(manovaDaysVeryPosNeg)

manovaDiff <- manova(manova(cbind(happyDiff, excitedDiff, sadDiff, anxiousDiff, irritableDiff,
                                  upsetDiff) ~ group, data = moodForecast))
summary(manovaDiff)
summary.aov(manovaDiff)

manovaDiffPosNeg <- manova(manova(cbind(posDiff, negDiff) ~ group, data = moodForecast))
summary(manovaDiffPosNeg)
summary.aov(manovaDiffPosNeg)

happyModel <- lm(propDaysVeryHappy ~ propExpectHappy*group, data = moodForecast)
summary(happyModel)

excitedModel <- lm(propDaysVeryExcited ~ propExpectExcited*group, data = moodForecast)
summary(excitedModel)

sadModel <- lm(propDaysVerySad ~ propExpectSad*group, data = moodForecast)
summary(sadModel)

anxiousModel <- lm(propDaysVeryAnxious ~ propExpectAnxious*group, data = moodForecast)
summary(anxiousModel)

irritableModel <- lm(propDaysVeryIrritable ~ propExpectIrritable*group, data = moodForecast)
summary(irritableModel)

upsetModel <- lm(propDaysVeryUpset ~ propExpectUpset*group, data = moodForecast)
summary(upsetModel)

moodForecastHC <- filter(moodForecast, group == "HC")
moodForecastSZ <- filter(moodForecast, group == "SZ")

t.test(moodForecastHC$posDiff)
t.test(moodForecastSZ$posDiff)
t.test(moodForecastHC$negDiff)
t.test(moodForecastSZ$negDiff)

# Clinical correlates -------------------------------------------------------------------------
# Pleasure recall bias
model <- lme(fixed = recallBias ~ PANNSNeg, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ PANNSPos, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ PANNSGen, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ PANNSTot, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ HQLSInterpersonal, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ HQLSInstrumental, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ HQLSIntrapsychic, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ HQLSTotal, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

chartData <- finalData %>%
    group_by(id, group) %>%
    summarise(meanRecallBias = mean(recallBias, na.rm = T), meanHQLSTotal = mean(HQLSTotal))

model <- lme(fixed = recallBias ~ HQLSTotal, random = ~1|id, data = filter(finalData, group == "HC"),
             na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ HQLSTotal, random = ~1|id, data = filter(finalData, group == "SZ"),
             na.action = na.omit)
summary(model)
    
ggplot(chartData, aes(meanRecallBias, meanHQLSTotal)) +
    geom_jitter(aes(color = group), width = .3, height = .3)

model <- lme(fixed = recallBias ~ HQLSSocial, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ HQLSWORK, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ HQLSMotivation, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ ChapmanPhysical, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ ChapmanSocial, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ ChapmanTotal, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ TEPSAntic, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ TEPSConsum, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ TEPSTotal, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ BISDrive, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ BISFun, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ BISReward, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ BISTotal, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = recallBias ~ HDRS, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

# Positive emotion prediction bias (24 hr)
model <- lme(fixed = posEmoPredBias ~ PANNSNeg, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ PANNSPos, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ PANNSGen, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ PANNSTot, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ HQLSInterpersonal, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ HQLSInstrumental, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ HQLSIntrapsychic, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ HQLSTotal, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ HQLSSocial, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ HQLSWORK, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ HQLSMotivation, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ ChapmanPhysical, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ ChapmanSocial, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ ChapmanTotal, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ TEPSAntic, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ TEPSConsum, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ TEPSTotal, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ BISDrive, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ BISFun, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ BISReward, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ BISTotal, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ HDRS, random = ~1|id, data = bedtimeC3, na.action = na.omit)
summary(model)

chartData <- bedtimeC3 %>%
    group_by(id, group) %>%
    summarise(meanPosEmoPredBias = mean(posEmoPredBias, na.rm = T), meanHDRS = mean(HDRS))

ggplot(chartData, aes(meanPosEmoPredBias, meanHDRS)) +
    geom_jitter(aes(color = group), width = .3, height = .3)

t.test(demoClinC$HDRS ~ demoClinC$group)

model <- lme(fixed = posEmoPredBias ~ HDRS, random = ~1|id, data = filter(bedtimeC3, group == "HC"),
             na.action = na.omit)
summary(model)

model <- lme(fixed = posEmoPredBias ~ HDRS, random = ~1|id, data = filter(bedtimeC3, group == "SZ"),
             na.action = na.omit)
summary(model)

# Positive emotion prediction bias (week-long)

moodForecastClinical <- left_join(moodForecast, demoClinC)

model <- lme(fixed = posDiff ~ PANNSNeg, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ PANNSPos, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ PANNSGen, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ PANNSTot, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ HQLSInterpersonal, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ HQLSInstrumental, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ HQLSIntrapsychic, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ HQLSTotal, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ HQLSSocial, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ HQLSWORK, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ HQLSMotivation, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ ChapmanPhysical, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ ChapmanSocial, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ ChapmanTotal, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

chartData <- moodForecastClinical %>%
    group_by(id, group) %>%
    summarise(meanPosDiff = mean(posDiff, na.rm = T), meanChapmanTotal = mean(ChapmanTotal))

ggplot(chartData, aes(meanPosDiff, meanChapmanTotal)) +
    geom_jitter(aes(color = group), width = .3, height = .3)

t.test(demoClinC$ChapmanTotal ~ demoClinC$group)

model <- lme(fixed = posDiff ~ TEPSAntic, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ TEPSConsum, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ TEPSTotal, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ BISDrive, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ BISFun, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ BISReward, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ BISTotal, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ HDRS, random = ~1|id, data = moodForecastClinical, na.action = na.omit)
summary(model)

chartData <- moodForecastClinical %>%
    group_by(id, group) %>%
    summarise(meanPosDiff = mean(posDiff, na.rm = T), meanHDRS = mean(HDRS))

ggplot(chartData, aes(meanPosDiff, meanHDRS)) +
    geom_jitter(aes(color = group), width = .3, height = .3)

model <- lme(fixed = posDiff ~ HDRS, random = ~1|id, data = filter(moodForecastClinical, group == "HC"),
             na.action = na.omit)
summary(model)

model <- lme(fixed = posDiff ~ HDRS, random = ~1|id, data = filter(moodForecastClinical, group == "SZ"),
             na.action = na.omit)
summary(model)

# New analyses (after meeting with Ellen) -----------------------------------------------------
#Does activity affect enjoyment?
finalData$activityCurrent <- as.factor(finalData$activityCurrent)
finalData$activityCurrent <- relevel(finalData$activityCurrent, ref = "Inactive")

model <- lme(fixed = enjoyPresent ~ activityCurrent, random = ~1|id, data = finalData,
             na.action = na.omit)
summary(model)

model <- lme(fixed = enjoyPresent ~ activityCurrent*group, random = ~1|id, data = finalData,
             na.action = na.omit)
summary(model)

model <- lme(fixed = enjoyPresent ~ activityCurrent, random = ~1|id, data = finalDataSZ,
             na.action = na.omit)
summary(model)

#This one works.
activityByPerson <- finalData %>%
    group_by(id, group, activityCurrent) %>%
    summarise(meanEnjoyPresent = mean(enjoyPresent, na.rm = T))

chartData <- activityByPerson %>%
    group_by(activityCurrent) %>%
    summarise(mean = mean(meanEnjoyPresent, na.rm = T), se = se(meanEnjoyPresent)) %>%
    filter(!is.na(activityCurrent))

ggplot(data = chartData, aes(x = activityCurrent, y = mean)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .1) +
    xlab("Activity")

#Does activity affect mood?
model <- lme(fixed = pos ~ activityCurrent, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = pos ~ activityCurrent*group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = neg ~ activityCurrent, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = neg ~ closeSoc*group, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

#Does current negative mood impact recall or prediction or pleasure?
model <- lme(fixed = enjoyPresent ~ pos, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)

model <- lme(fixed = enjoyPresent ~ neg, random = ~1|id, data = finalData, na.action = na.omit)
summary(model)



cor(finalData$happy, finalData$excited, use = "pairwise.complete.obs")
cor(finalData$anxious, finalData$irritable, use = "pairwise.complete.obs")
cor(finalData$anxious, finalData$sad, use = "pairwise.complete.obs")
cor(finalData$anxious, finalData$upset, use = "pairwise.complete.obs")
cor(finalData$irritable, finalData$sad, use = "pairwise.complete.obs")
cor(finalData$irritable, finalData$upset, use = "pairwise.complete.obs")
cor(finalData$happy, finalData$sad, use = "pairwise.complete.obs")
