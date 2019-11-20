 # Initial analysis of fast-food data

library(dplyr)
library(tidyr)
library(ggplot2)
library(usmap)
library(caret)

setwd("~/Desktop/Data-Science-Portfolio/Projects/fast-food")


# A sample of 10,000 fast food restaurants in the US
# Remember that its a sample of a much larger dataset,
# so any findings here are reflective only of this
# dataset, and may potentially be different than reality.
data <- read.csv("datasets/FastFoodRestaurants.csv")
population <- read.csv("datasets/StatePop.csv")
names(population) <- c("FullName", "state", "x2019PopEst", "x2010Pop")
population$x2010Pop <- as.character(population$x2010Pop)
population$x2010Pop <- gsub(",", "", population$x2010Pop)
population$x2010Pop <- as.numeric(population$x2010Pop)
population$pt <- (population$x2010Pop / sum(population$x2010Pop)) * 100


obesity <- read.csv("datasets/StateObesity.csv")
names(obesity) <- c("FullName", "state", "PercentObese")

data.proc <- data
data.proc$count <- 1



# There are multiple restaurants that appear multiple times, with variations in their names. Fix it
data.proc$name <- tolower(data.proc$name)
data.proc$name <- gsub('[[:punct:]]', '', data.proc$name)
n <- length(unique(data$name)) - length(unique(data.proc$name))
# 61 duplicated names were merged by removing variations in capitilization and punctuation


# Some duplication still exists due to spacing, for example,
# aw all american food and aw allamerican food still exist
data.proc$name <- gsub('[[:space:]]', '', data.proc$name)
p <- length(unique(data$name)) - length(unique(data.proc$name))
# merged an additional 11 restaurants


# There are several more restaurants that have a couple rare variations
# of there names, but because they are rare, they likely won't impact
# analysis much. If a particular restaurant becomes particularly interesting
# look deeper into alternate names for them specifically

# For some reasons Colorado Springs is listed as its own state
data.proc$province <- gsub('Co Spgs', 'CO', data.proc$province)



compressed.name <- data.proc %>%
  group_by(name) %>%
  summarise(Count = sum(count)) 

compressed.name <- compressed.name[order(-compressed.name$Count),]
top10 <- compressed.name[1:10,]
sum(top10$Count) # Over 71% of our dataset is made up of 10 restaurants
top10$name <- factor(top10$name, levels=c("mcdonalds", "burgerking", "tacobell",
                                          "wendys", "arbys", "kfc", "subway",
                                          "sonicdrivein", "dominospizza", "jackinthebox"))
figpath <- "figures/top10.jpg"
p <- ggplot(top10, aes(x=name, y=Count, fill=name)) +
  geom_col() +
  theme(legend.position="none")
ggsave(figpath, p)
####
compressed.state <- data.proc %>%
  group_by(province) %>%
  summarise(Count = sum(count))
names(compressed.state) <- c("state", "Count")
compressed.state$pt <- (compressed.state$Count / 10000) * 100

figpath <- "foodperstate.jpg"
p <- plot_usmap(data=compressed.state, values = 'pt', color='blue') +
  scale_fill_continuous(low="white", high="blue", name="% of Fast Food Restaurants", label=scales::comma) +
  theme(legend.position = "none") 
ggsave(figpath, p)

figpath <- "figures/popperstate.jpg"
p <-plot_usmap(data=population, values = 'pt', color='blue') +
  scale_fill_continuous(low="white", high="blue", name="% of US Population", label=scales::comma) +
  theme(legend.position = "right") +
  ggtitle("Population, 2010 Census")
ggsave(figpath, p)

figpath <- "figures/obesityperstate.jpg"
p <- plot_usmap(data=obesity, values = 'PercentObese', color='blue') +
  scale_fill_continuous(low="white", high="blue", name="% of population that is obese", label=scales::comma) +
  theme(legend.position = "right") +
  ggtitle("US Obesity Rates")
ggsave(figpath, p)


compressed.state <- subset(compressed.state , select = -c(pt))
df <- merge(compressed.state, population, by="state")
df <- merge(df, obesity, by="state")
df <- subset(df, select=c(state, Count, x2010Pop, PercentObese, pt))
df$PplPerRes <- df$x2010Pop / df$Count
df$ExpectedRes <- (df$pt/100) * 10000
df$CountMinusExpected <- df$Count - df$ExpectedRes

figpath <- "figures/pplperres.jpg"
p <- plot_usmap(data=df, values = 'PplPerRes', color='blue') +
  scale_fill_continuous(low="white", high="blue", name="Population/Restaurants", label=scales::comma) +
  theme(legend.position = "right") +
  ggtitle("Number of people per fast food restaurant")
ggsave(figpath, p)

med <- median(df$PercentObese)
df$ObeseQuant <- ifelse(df$PercentObese >= med, "Greater than average obesity", "Lower than average obesity")

figpath <- "figures/obsminusexpcol.jpg"
p <- ggplot(df, aes(x=state, y=CountMinusExpected, fill=ObeseQuant)) +
  geom_col() +
  labs(x="State", y="Observed - Expected")
ggsave(figpath, p)


FFSum <- sum(df$Count)
df$FFpt <- (df$Count / sum(df$Count)) * 100

ggplot(df, aes(x=pt, y=FFpt, color=ObeseQuant)) +
  geom_point() +
  geom_smooth(method="lm")

figpath <- "figures/obesemedianstate.jpg"
p <- plot_usmap(data=df, values='ObeseQuant', exclude=c("DC")) +
  theme(legend.position="right")
ggsave(figpath, p)

figpath <- "figures/popvsfastfoodcount.jpg"
p <- ggplot(df, aes(x=x2010Pop, y=Count, color=ObeseQuant)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_x_continuous(labels= scales::comma)
ggsave(figpath, p)

state.name <- data.proc %>%
  group_by(province, name) %>%
  summarise(Count = sum(count))


#RESAMPLING
original.diffs <- df %>%
  group_by(ObeseQuant) %>%
  summarise(med = median(CountMinusExpected))
greater.obesity <- original.diffs[which(original.diffs$ObeseQuant == "Greater than average obesity"), 2][1]
lower.obesity <- original.diffs[which(original.diffs$ObeseQuant == "Lower than average obesity"), 2][1]
diff <- as.numeric(greater.obesity) - as.numeric(lower.obesity)
diffs <- c()
df.resample <- subset(df, select = c(CountMinusExpected, ObeseQuant)) 
for (i in 1:10000) {
  df.resample$shuffled <- sample(df.resample$ObeseQuant, size=50, replace = FALSE) 
  resampled.diffs <- df.resample %>%
    group_by(shuffled) %>%
    summarise(shuffled.med = median(CountMinusExpected))
  greater.obesity <- resampled.diffs[which(resampled.diffs$shuffled == "Greater than average obesity"), 2][1]
  lower.obesity <- resampled.diffs[which(resampled.diffs$shuffled == "Lower than average obesity"), 2][1]
  new.diff <- as.numeric(greater.obesity) - as.numeric(lower.obesity)
  diffs <- c(diffs, new.diff)
}
diffs <- as.data.frame(diffs)
diffs$extreme <- ifelse(diffs$diffs >= diff | diffs$diffs <= -diff, 1, 0)
pt <- (1 - (sum(diffs$extreme) / 10000)) * 100

ggplot(diffs, aes(x=diffs)) +
  geom_density(color="darkblue", fill="darkblue") +
  geom_vline(xintercept = diff) +
  geom_vline(xintercept = -diff) +
  labs(x="Difference in Permutated Medians") +
  annotate("text", x=0, y=0.020, label=paste0(pt, "%")) +
  ggtitle("Distribution of Differences after 10000 Permutation Tests") +
  geom_segment(aes(x = 40,
                   y = 0.025,
                   xend = -40,
                   yend = 0.025), colour='black', size=0.5,
                   arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = -40,
                   y = 0.025,
                   xend = 40,
                   yend = 0.025), colour='black', size=0.5,
               arrow = arrow(length = unit(0.5, "cm"))) + 
  theme_minimal()

df.resample$shuffled <- sample(df.resample$ObeseQuant, size=50, replace = FALSE)

# Restaurant specific impact
mcdonalds <- subset(state.name, name=="mcdonalds")
names(mcdonalds) <- c("state", "name", "mcd_count")
mcdonalds <- merge(mcdonalds, population, by="state")
mcdonalds <- merge(mcdonalds, obesity, by="state")
mcdonalds <- subset(mcdonalds, select=c("state", "name", "mcd_count", "x2010Pop", "PercentObese"))
med <- median(mcdonalds$PercentObese)
mcdonalds$ObeseQuant <- ifelse(mcdonalds$PercentObese >= med, "Greater than average obesity", "Lower than average obesity")

ggplot(mcdonalds, aes(x=x2010Pop, y=mcd_count, color=ObeseQuant)) +
  geom_point() +
  geom_smooth(method="lm",se=F)








# MACHINE LEARNING
dataset <- data.proc
dataset <- subset(dataset, select=c("name", "province", "count"))
names(dataset) <- c("restaurant", "state", "count")
dataset <- dataset %>%
  group_by(state, restaurant) %>%
  summarise(Count = sum(count))

dataset <- subset(dataset, restaurant %in% top10$name)
dataset <- dataset %>%
  spread(key=restaurant, value=Count)
values <- c(0,0,0,0,0,0,0,0,0,0,0)
names(values) <- colnames(dataset)
values <- as.list(values)
dataset <- replace_na(dataset, values)
dataset <- merge(dataset, population, by="state")
dataset <- merge(dataset, obesity, by="state")
dataset <- merge(dataset, compressed.state, by="state")
dataset <- subset(dataset, select = -c(FullName.x, x2019PopEst, pt, FullName.y, state))
med <- median(dataset$PercentObese)
dataset$ObesityQuantile <- ifelse(dataset$PercentObese <= med, "Lower than average obesity", "Greater than average obesity")

dataset.copy <- subset(dataset, select = -c(PercentObese))
dataset.copy$ObesityQuantile <- as.factor(dataset.copy$ObesityQuantile)

set.seed(42)
model <- glm(ObesityQuantile ~ ., data = dataset.copy, family="binomial")

p <- predict(model, dataset.copy, type="response")
q <- ifelse(p > 0.5, "Lower than average obesity", "Greater than average obesity")
confusionMatrix(factor(q), dataset.copy$ObesityQuantile)
