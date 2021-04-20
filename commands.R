x <- 1:10
y <- rnorm(10)
plot(x,y)

fit <- lm(y ~ x)
dat <- read.csv("Desktop/R Programming/femaleControlsPopulation.csv")
head(dat)
names(dat)[1]
dat[12,1]
weights <- dat$Bodyweight
weights[11]

length(weights)

View(dat)
weights <- dat$Bodyweight
mean(weights[24])

set.seed(1)
i <- sample(3:7,1)
dat$Bodyweight[i]

dat <- read.csv("Desktop/R Programming/femaleControlsPopulation.csv")
controls <- filter(dat, Bodyweight==26.60)
controls
View(controls)

controls <- select(controls, Bodyweight)
unlist(controls)

controls <- filter(dat, Bodyweight==26.60) %>% select(Bodyweight) %>% unlist(controls)
controls

x <- father.son$fheight
round(sample(x,20),1)
hist(x,breaks=seq(floor(min(x)),ceiling(max(x))),main="",xlab="Height")
x
mean(x>70)
## Perform Normal approximation
1-pnorm(70,mean(x),sd(x))

## Quantile Quantile Plot or Q-Q Plot
ps <- ( seq(0,99) + 0.5 )/100 
qs <- quantile(x, ps)
normalqs <- qnorm(ps, mean(x), popsd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1) ##identity line

qqnorm(x)
qqline(x)

## Download this RData file to your working directory. Then load the data into R w
load("Desktop/R Programming/skew.Rdata")
## should have a 1000 x 9 dimensional matrix 'dat':
dim(dat)

## Using QQ-plots, compare the distribution of each column of the matrix 
## to a normal. That is, use qqnorm() on each column. To accomplish this quickly,
## you can use the following line of code to set up a grid for 3x3=9 plots. 
##(“mfrow” means we want a multifigure grid filled in row-by-row. Another choice
## is mfcol.)
par(mfrow = c(3,3))
#Then you can use a for loop, to loop through the columns, and display one qqnorm() plot at a time. 
for (i in 1:9) {
  x <- dat[,i]
  qqnorm(x,xlab="quantiles",main=paste0("Col No=",i))
  qqline(x)
}
#plotting multiple histograms
par(mfrow = c(3,3))
#Then you can use a for loop, to loop through the columns, and display one qqnorm() plot at a time. 
for (i in 1:9) {
  x <- dat[,i]
  hist(x,xlab="X",main=paste0("Col.No=",i))
}

## Examine each of these two columns using a histogram. Note which column has “positive skew”, in other words the histogram shows a long tail to the right (toward larger values). Note which column has “negative skew”, that is, a long tail to the left (toward smaller values). Note that positive skew looks like an up-shaping curve in a qqnorm() plot, while negative skew looks like a down-shaping curve.
par(mfrow=c(1,1))
hist(dat[,4])

## Which column has negative skew (a long tail to the left)?
hist(dat[,9])

## Boxplots
data(exec.pay, package = "UsingR")
hist(exec.pay) 
qqnorm(exec.pay)
qqline(exec.pay)

## In addition to qq-plots, a practical summary of data is to compute 3 percentiles: 25-th, 50-th (the median) and the 75-th. A boxplot shows these 3 values along with a range of the points within median ± 1.5 (75-th percentile -25th-percentile). Values outside this range are shown as points and sometimes refereed to as outliers.
boxplot(exec.pay, ylab="10,000's of dollars", ylim=c(0,400)) ## ylim( limits ) sets the y-axis limits for the current axes or chart

## The InsectSprays data set measures the counts of insects in agricultural experimental units treated with different insecticides. This dataset is included in R,
head(InsectSprays) ## gives count
InsectSprays$spray ## gives sprays

#Boxplot Using split
boxplot(split(InsectSprays$count, InsectSprays$spray))

#using formula
boxplot(InsectSprays$count ~ InsectSprays$spray)

## The InsectSprays data set is included in R. The dataset reports the counts of insects in agricultural experimental units treated with different insecticides. Make a boxplot and determine which insecticide appears to be most effective.
boxplot(count~spray, InsectSprays,xlab="spray",ylab="count",cex=0)


## Let’s consider a random sample of finishers from the New York City Marathon in 2002. This dataset can be found in the UsingR package. Load the library and then load the nym.2002 dataset.
## Use boxplots and histograms to compare the finishing times of males and females. Which of the following best describes the difference?
library(dplyr)
data(nym.2002, package="UsingR")
str(nym.2002)  ## Display internal structure of an R object

head(nym.2002)

library(dplyr)
males<-filter(nym.2002, gender=="Male") 
females<-filter(nym.2002, gender=="Female") 
library(rafalib)
mypar(1,3) ## MIPAR generates a grain size distribution as well as data statistics

boxplot(females$time, males$time)
hist(females$time,xlim=c(range( nym.2002$time))) ## xlim( limits ) sets the x-axis limits for the current axes or chart
hist(males$time,xlim=c(range( nym.2002$time)))

## Solution: Male and females have similar right skewed distributions with the former, 20 minutes shifted to the left.


## Introduction to random variables
## So are the hf mice heavier? Mouse 24 at 20.73 grams is one of the
## lightest mice, while Mouse 21 at 34.02 grams is one of the heaviest. Both are on
## the hf diet. Just from looking at the data, we see there is
## *variability*. Claims such as the one above usually refer to the
## averages. So let's look at the average of each group: 

## Read data from file URL
dir <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/"
filename <- "femaleMiceWeights.csv"
url <- paste0(dir, filename)
dat <- read.csv(url)
View(dat)
library(dplyr)

control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(treatment) - mean(control)

population <- read.csv("Desktop/R Programming/femaleControlsPopulation.csv")
class(population)

## Convert data frame to vector
population <- unlist(population)
class(population)

## Take 12 mice sample
mean(sample(population, 12))



## Random variables exercises
#For these exercises, we will be using the following dataset:
install.packages("downloader")
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
head(x)

## What is the average of these weights?
mean(x)

## After setting the seed at 1, set.seed(1) take a random sample of size 5. What is the absolute value (use abs) of the difference between the average of the sample and the average of all the values?
set.seed(1)
sample<-sample(x,5)
abs(mean(sample)-mean(x))

## After setting the seed at 5, set.seed(5), take a random sample of size 5. What is the absolute value of the difference between the average of the sample and the average of all the values?
set.seed(5)
sample<-sample(x,5)
abs(mean(sample)-mean(x))


## Introduction to Null Distributions
## How to use statistical inference to support scientific statements
population <- read.csv("Desktop/R Programming/femaleControlsPopulation.csv")
## Convert data frame to vector
population <- unlist(population)
control <- sample(population, 12)
treatment <- sample(population, 12)
obsdiff <- mean(treatment) - mean(control)

## Null Hypothesis or Null Distribution
n <- 10000
nulls <- vector("numeric",n) ## We define a numeric vector and save 'n'

for (i in 1:n) {
  control <- sample(population, 12)
  treatment <- sample(population, 12)
  nulls[i] <- mean(treatment) - mean(control)
}
max(nulls)
hist(nulls)

## The values in `null` form what we call the *null distribution*. We will define this more formally below.
## So what percent of the 10,000 are bigger than `obsdiff`?
mean(nulls >= obsdiff)
mean(abs(nulls) >= obsdiff)

##### Questions on Null DIstribution
## Set the seed at 1, then using a for-loop take a random sample of 5 mice 
## 1,000 times. Save these averages. What percent of these 1,000 averages 
## are more than 1 ounce away from the average of x ?

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist(read.csv(filename))
head(x)

set.seed(1) ## so that we get same results
n <- 1000
averages5 <- vector("numeric", n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
hist(averages5)

mean(abs(averages5 - mean(x)) > 1)

#### Probability Distribution
install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)

## Create a vector ‘x’ of the life expectancies of each country for the 
## year 1952. Plot a histogram of these life expectancies to see the spread 
##of the different countries.
library(dplyr)
x<-filter(gapminder,year==1952) %>% select(lifeExp) %>% unlist
head(x)

## In statistics, the empirical cumulative distribution function (or empirical cdf or empirical distribution function) is the function F(a) for any a, which tells you the proportion of the values which are less than or equal to a.
## We can compute F in two ways: the simplest way is to type mean(x <= a). This calculates the number of values in x which are less than or equal a, divided by the total number of values in x, in other words the proportion of values less than or equal to a.
## The second way, which is a bit more complex for beginners, is to use the ecdf() function. This is a bit complicated because this is a function that doesn’t return a value, but a function.

## Let’s continue, using the simpler, mean() function.

## Question: What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?
mean(x<=40)
## Question: What is the proportion of countries in 1952 that have a life expectancy between 40 and 60 years? Hint: this is the proportion that have a life expectancy less than or equal to 60 years, minus the proportion that have a life expectancy less than or equal to 40 years.
mean(x<=60) - mean(x<=40)


#### Normal Distribution or Gaussian Distribution
## Here μ and σ are referred to as the mean and the standard deviation 
## of the population (we explain these in more detail in another section). 
## If this normal approximation holds for our list, then the population mean 
## and variance of our list can be used in the formula above. An example of 
## this would be when we noted above that only 1.5% of values on the null
## distribution were above obsdiff. We can compute the proportion of values
## below a value x with pnorm(x,mu,sigma) without knowing all the values. 
## The normal approximation works very well here:

n <- 10000
null <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  null[i] <- mean(treatment) - mean(control)
}
1 - pnorm(obsdiff,mean(null),sd(null)) ## pnorm gives the formula Pr(a<x<b)=∫ba12πσ2‾‾‾‾‾√exp(−(x−μ)22σ2)dx


## Question: Using the same process as before (in Null Distribution Exercises),
## set the seed at 1, then using a for-loop take a random sample of 5 mice 
## 1,000 times. Save these averages. After that, set the seed at 1, then using 
## a for-loop take a random sample of 50 mice 1,000 times. Save these averages:
# make averages5
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
hist(averages5) ##take a look

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
hist(averages50) ##take a look

## Question: For the last set of averages, the ones obtained from a sample 
## size of 50, what proportion are between 23 and 25?
mean( averages50 < 25 & averages50 > 23)
## OR
pnorm(25,mean(averages50),sd(averages50))-pnorm(23,mean(averages50),sd(averages50))

## Question: Note that you can use the function pnorm() to find the proportion 
## of observations below a cutoff x given a normal distribution with mean
## mu and standard deviation sigma with pnorm(x, mu, sigma) or
## pnorm( (x-mu)/sigma ).
## What is the proportion of observations between 23 and 25 in a normal 
## distribution with average 23.9 and standard deviation 0.43?
pnorm(25,23.9,0.43)-pnorm(23,23.9,0.43)
## OR
pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43) 


#### Population, Samples and Estimates
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 

## We can then access the population values and determine, for example, 
## how many we have. Here we compute the size of the control population
## which are mice on control diet:
library(dplyr)
controlPopulation <- filter(dat,Sex == "F" & Diet == "chow") %>% 
  select(Bodyweight) %>% unlist
length(controlPopulation)

## We usually denote these values as x1,…,xm. In this case, m is the number 
## computed above. We can do the same for the high fat diet population:
hfPopulation <- filter(dat,Sex == "F" & Diet == "hf") %>%  
  select(Bodyweight) %>% unlist
length(hfPopulation)

## We can then define summaries of interest for these populations, such as the
## mean and variance.

#We will remove the lines that contain missing values:
dat <- na.omit( dat )

## Question: Use dplyr to create a vector x with the body weight of all males
## on the control (chow) diet. What is this population’s average?  
library(dplyr)
x<- filter(dat,Sex == "M" & Diet == "chow") %>% 
  select(Bodyweight) %>% unlist
length(x)
mean(x)

## Question: Now use the rafalib package and use the popsd() function to compute the population 
## standard deviation.
library(rafalib)
popsd(x)


## Question: Set the seed at 1. Take a random sample  𝑋  of size 25 from x.
set.seed(1)
X<- sample(x,25)
mean(X)

## Question: Use dplyr to create a vector y with the body weight of all males on the high 
## fat hf) diet. What is this population's average?
library(dplyr)
y<- filter(dat,Sex == "M" & Diet == "hf") %>%  
  select(Bodyweight) %>% unlist
length(y)
mean(y)

## Question: Now use the rafalib package and use the popsd() function to compute the
## population standard deviation.
library(rafalib)
popsd(y)

## Question: Set the seed at 1. Take a random sample  𝑌  of size 25 from y.
## What is the sample average?
set.seed(1)
Y<- sample(y,25)
mean(Y)

## Question: What is the difference in absolute value between  𝑦¯−𝑥¯  and  𝑌¯−𝑋¯ ?
a<-mean(y)-mean(x)
b<-abs(mean(X)-mean(Y))
a-b
#or
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )


#### Central Limit Theorem
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

## Question: If a list of numbers has a distribution that is well approximated by the normal 
## distribution, what proportion of these numbers are within one standard deviation away from the 
## list's average?
pnorm(1)-pnorm(-1)

## Question: What proportion of these numbers are within two standard deviations away from the 
## list's average?
pnorm(2)-pnorm(-2)

## Question: What proportion of these numbers are within three standard deviations away from the 
## list's average?
pnorm(3)-pnorm(-3)

## Question: Define y to be the weights of males on the control diet. What proportion of the mice are within 
## one standard deviation away from the average weight (remember to use popsd for the population sd)?
library(dplyr)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)  ## How much y varies from the mean -> y - mean(y)
mean( abs(z) <=1 )

## Question: What proportion of these numbers are within two standard deviations away from the list’s average?
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <=2 )

## Question: Note that the numbers for the normal distribution and our weights are relatively close. 
## Also, notice that we are indirectly comparing quantiles of the normal distribution to quantiles of the 
## mouse weight distribution. We can actually compare all quantiles using a qqplot.
## Which of the following best describes the qq-plot comparing mouse weights to the normal distribution?
library(rafalib)
mypar(1,1)
qqnorm(z)
abline(0,1)
## Answer: The mouse weights are well approximated by the normal distribution, although the larger values 
## (right tail) are larger than predicted by the normal. This is consistent with the differences seen between 
## question 3 and 6.

## Question: Create the above qq-plot for the four populations: male/females on each of the two diets.
## What is the most likely explanation for the mouse weights being well approximated? What is the best
## explanation for all these being well approximated by the normal distribution?
mypar(2,2)  ## This function optimizes graphical parameters for the RStudio plot window.
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1) ## function abline() can be used to add vertical, horizontal or regression lines to a graph
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)

## Here we are going to use the function replicate() to learn about the distribution of random variables.
## All the above exercises relate to the normal distribution as an approximation of the distribution of a 
## fixed list of numbers or a population. We have not yet discussed probability in these exercises. If the 
## distribution of a list of numbers is approximately normal, then if we pick a number at random from this 
## distribution, it will follow a normal distribution. However, it is important to remember that stating that
## some quantity has a distribution does not necessarily imply this quantity is random. Also, keep in mind 
## that this is not related to the central limit theorem. The central limit applies to averages of random 
##variables. Let's explore this concept.

## We will now take a sample of size 25 from the population of males on the chow diet. The average of this 
## sample is our random variable. We will use the replicate() function to observe 10,000 realizations of this 
## random variable. Set the seed at 1, then generate these 10,000 averages. Make a histogram and qq-plot of 
## these 10,000 numbers against the normal distribution.

## We can see that, as predicted by the CLT, the distribution of the random variable is very well approximated
## by the normal distribution.
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

## Question: What is the average of the distribution of the sample average?
mean(avgs)

## Question: What is the standard deviation of the distribution of sample averages (use popsd())?
popsd(avgs)


#### t-test and t-statistics
## Read in and prepare data
dir <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/"
filename <- "femaleMiceWeights.csv"
url <- paste0(dir, filename)
dat <- read.csv(url)
control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist

diff <- mean(treatment) - mean(control)
print(diff)

## We also learned that the standard error of this random variable is the population standard deviation 
## divided by the square root of the sample size:
## SE(X¯)=σ/√N

## We use the sample standard deviation as an estimate of the population standard deviation. In R, we simply 
## use the sd function and the SE is:
sd(control)/sqrt(length(control))

## This is the SE of the sample average, but we actually want the SE of diff. We saw how statistical theory 
## tells us that the variance of the difference of two random variables is the sum of its variances, so we 
## compute the variance and take the square root:
se <- sqrt( 
  var(treatment)/length(treatment) + 
    var(control)/length(control) 
)
se

## t-statistics is the ratio of two random variables and thus a random variable. Once we know the distribution
## of this random variable, we can then easily compute a p-value
tstat <- diff/se 

## To calculate a p-value all we need to do is ask: how often does a normally distributed random variable
## exceed diff? R has a built-in function, pnorm, to answer this specific question. pnorm(a) returns the 
## probability that a random variable following the standard normal distribution falls below a. To obtain the 
## probability that it is larger than a, we simply use 1-pnorm(a). We want to know the probability of seeing 
##something as extreme as diff: either smaller (more negative) than -abs(diff) or larger than abs(diff). 
## We call these two regions “tails” and calculate their size:
righttail <- 1 - pnorm(abs(tstat)) 
lefttail <- pnorm(-abs(tstat))
pval <- lefttail + righttail
print(pval)
## In this case, the p-value is smaller than 0.05 and using the conventional cutoff of 0.05, we would call
## the difference "statistically significant".


#### t-distribution
library(rafalib)
mypar(1,2)

qqnorm(treatment)
qqline(treatment,col=2)

qqnorm(control)
qqline(control,col=2)

## If we use this approximation, then statistical theory tells us that the distribution of the random variable
## tstat follows a t-distribution. This is a much more complicated distribution than the normal. The 
## t-distribution has a location parameter like the normal and another parameter called degrees of freedom. 
## R has a nice function that actually computes everything for us.
t.test(treatment, control)
## Output ##
## Welch Two Sample t-test

## data:  treatment and control
## t = 2.0552, df = 20.236, p-value = 0.053
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.04296563  6.08463229
## sample estimates:
##   mean of x mean of y 
## 26.83417  23.81333 

## To see just the p-value, we can use the $ extractor:
result <- t.test(treatment,control)
result$p.value

## t-distribution exercises
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename) 

## Question: Suppose we are interested in the proportion of times we see a 6 when rolling n=100 die. This is a
## random variable which we can simulate with x=sample(1:6, n, replace=TRUE) and the proportion we are 
## interested in can be expressed as an average: mean(x==6). Because the die rolls are independent, the CLT 
## applies.
## We want to roll n dice 10,000 times and keep these proportions. This random variable (proportion of 6s) has
## mean p=1/6 and variance p(1-p)/n. So according to CLT z = (mean(x==6) - p) / sqrt(p(1-p)/n) should be normal
## with mean 0 and SD 1. Set the seed to 1, then use replicate to perform the simulation, and report what 
## proportion of times z was larger than 2 in absolute value (CLT says it should be about 0.05).
set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)

## Question: In the previous example used, the original data is binary (either 6 or not). In this case, the 
## success probability also affects the appropriateness of the CLT. With very low probabilities, we need 
## larger sample sizes for the CLT to “kick in”.
## Run the simulation from exercise 1, but for different values of p and n. For which of the following is the
## normal approximation best?
## p=0.5 and n=5
## p=0.5 and n=30
## p=0.01 and n=30
## p=0.01 and n=100
ps <- c(0.5,0.5,0.01,0.01) ## collection
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}
## Solution: p=0.5 and n=30 ##

## Question: As we have already seen, the CLT also applies to averages of quantitative data. A major difference with binary 
## data, for which we know the variance is  𝑝(1−𝑝) , is that with quantitative data we need to estimate the population 
## standard deviation.
## In several previous exercises we have illustrated statistical concepts with the unrealistic situation of having access to
## the entire population. In practice, we do *not* have access to entire populations. Instead, we obtain one random sample and
## need to reach conclusions analyzing that data. dat is an example of a typical simple dataset representing just one sample.
## We have 12 measurements for each of two populations:
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
## We think of  𝑋  as a random sample from the population of all mice in the control diet and  𝑌  as a random sample from 
## the population of all mice in the high fat diet.
## Define the parameter  𝜇𝑋  as the average of the control population. We estimate this parameter with the sample average 
## 𝑋¯ . What is the sample average?
str(X)
mean(X)

## Question: We don’t know μX, but want to use X¯ to understand μX. Which of the following uses CLT to understand how well
## X¯ approximates μX?
## Solution: X¯  follows a normal distribution with mean μX and standard deviation σX√12 where σX is the population standard
## deviation.

## Question: The result of 4 and 5 tell us that we know the distribution of the difference between our estimate and what we
## want to estimate, but don’t know. However, the equation involves the population standard deviation σX, which we don’t know.
## Given what we discussed, what is your estimate of σX ?
sd(X)

## Question: now we can compute Y¯−X¯ as well as an estimate of this standard error and construct a t-statistic. 
## What number is this t-statistic?
( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
## OR
t.test(Y,X)$stat


#### Inference I: P-values, Confidence Intervals and Power Calculations
## T-test Exercises
library(downloader) 
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
## This is a large dataset (1,236 cases), and we will pretend that it contains the entire population in which we are 
## interested. We will study the differences in birth weight between babies born to smoking and non-smoking mothers.
## First, let's split this into two birth weight datasets: one of birth weights to non-smoking mothers and the other of 
## birth weights to smoking mothers.
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
## we can look for the true population difference in means between smoking and non-smoking birth weights.
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

## The population difference of mean birth weights is about 8.9 ounces. The standard deviations of the nonsmoking and smoking 
## groups are about 17.4 and 18.1 ounces, respectively.
## We are interested in testing whether the birth weights of babies born to non-smoking mothers are significantly different 
## from the birth weights of babies born to smoking mothers.

## Question: Set the seed at 1 and obtain a samples from the non-smoking mothers (dat.ns) of size  𝑁=25 . Then, without 
## resetting the seed, take a sample of the same size from and smoking mothers (dat.s). Compute the t-statistic (call it tval).
## What is the absolute value of the t-statistic?
set.seed(1)
dat.ns<-sample(bwt.nonsmoke,25)
dat.s<-sample(bwt.smoke,25)
tval<-t.test(dat.ns,dat.s)$statistic  ## t-statistic
tval
## OR
N=25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N)
dat.s <- sample(bwt.smoke , N)

X.ns <- mean(dat.ns)
sd.ns <- sd(dat.ns)

X.s <- mean(dat.s)
sd.s <- sd(dat.s)

sd.diff <- sqrt(sd.ns^2/N+sd.s^2/N)
tval <- (X.ns - X.s)/sd.diff
tval

## Recall that we summarize our data using a t-statistics because we know that in situations where the null hypothesis is 
## true (what we mean when we say “under the null”) and the sample size is relatively large, this t-value will have an
## approximate standard normal distribution. Because we know the distribution of the t-value under the null, we can 
## quantitatively determine how unusual the observed t-value would be if the null hypothesis were true.
## The standard procedure is to examine the probability a t-statistic that actually does follow the null hypothesis would have
## larger absolute value than the absolute value of the t-value we just observed – this is called a two-sided test.
## We have computed these by taking one minus the area under the standard normal curve between -abs(tval) and abs(tval).
## In R, we can do this by using the pnorm function, which computes the area under a normal curve from negative infinity up 
## to the value given as its first argument:
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval


#### Confidence Intervals
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

## First, let's split this into two birth weight datasets: one of birth weights to non-smoking mothers and the other of birth
## weights to smoking mothers.
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
## look for the true population difference in means between smoking and non-smoking birth weights.
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

## Question: Set the seed at 1 and obtain two samples, each of size N = 25, from non-smoking mothers (dat.ns) and smoking 
## mothers (dat.s). If instead of CLT, we use the t-distribution approximation, what do we add and subtract to obtain a 99% 
## confidence interval (use 2*N-2 degrees of freedom)?
N <- 25
set.seed(1)
dat.ns <- sample(bwt.nonsmoke, N) 
dat.s <- sample(bwt.smoke, N) 
qt(0.995,2*N-2)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N ) ## .995 is confidence interval, 2*N-2 is dof
## Quantile Function(qt) : In probability and statistics, the quantile function, associated with a probability distribution of a 
## random variable, specifies the value of the random variable such that the probability of the variable being less than or 
## equal to that value equals the given probability.

## Question: Set the seed at 1 and take a random sample of N=5 measurements from each of the smoking and nonsmoking datasets.
## What is the p-value (use the t-test function)?
N=5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N) 
dat.s <- sample(bwt.smoke , N) 
t.test(dat.s, dat.ns)$p.value



#### Power Calculations
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

## Question: Set the seed at 1 and take a random sample of N=5 measurements from each of the smoking and nonsmoking datasets.
## You used the t-test function to find the p-value.
## The p-value is larger than 0.05 so using the typical cut-off, we would not reject. This is a type II error. 
## Which of the following is not a way to decrease this type of error?
  ## Increase our chance of a type I error.(reduces type II error)
  ## Take a larger sample size.(reduces type II error)
  ## Use a higher α level.(reduces type II error)
  ## Find a population for which the null is not true.
N=5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N) 
dat.s <- sample(bwt.smoke , N) 
t.test(dat.s, dat.ns)$p.value>0.05
## Solution: Find a population for which the null is not true.

## Question: Set the seed at 1, then use the replicate() function to repeat the code used in the exercise above 10,000 times.
## What proportion of the time do we reject at the 0.05 level?
N=5
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)
## OR
set.seed(1)
N <- 5
alpha <- 0.05
reject <- function(N, alpha=0.05){
  dat.ns <- sample(bwt.nonsmoke , N) 
  dat.s <- sample(bwt.smoke , N) 
  pval <- t.test(dat.s,dat.ns )$p.value
  pval < alpha
}
reject(N)

B <- 10000
rejections <- replicate(B,reject(N))
mean(rejections) 


#### Monte Carlo Simulation ####

## Monte Carlo methods, or Monte Carlo experiments, are a broad class of computational algorithms that rely on repeated random
## sampling to obtain numerical results. The underlying concept is to use randomness to solve problems that might be 
## deterministic in principle.
library(dplyr)
dat <- read.csv("mice_pheno.csv")
controlPopulation <- filter(dat,Sex == "F" & Diet == "chow") %>%  
  select(Bodyweight) %>% unlist

## We will build a function that automatically generates a t-statistic under the null hypothesis for a any sample size of n.
ttestgenerator <- function(n) {
  #note that here we have a false "high fat" group where we actually
  #sample from the chow or control population. 
  #This is because we are modeling the null.
  cases <- sample(controlPopulation,n)
  controls <- sample(controlPopulation,n)
  #Generate tstatistic
  tstat <- (mean(cases)-mean(controls)) / 
    sqrt( var(cases)/n + var(controls)/n ) 
  return(tstat)
}
ttests <- replicate(1000, ttestgenerator(10))
## With 1,000 Monte Carlo simulated occurrences of this random variable, we can now get a glimpse of its distribution:
hist(ttests)

## Quantile-quantile plot comparing 1000 Monte Carlo simulated t-statistics to theoretical normal distribution.
qqnorm(ttests)
abline(0,1)

## Question: Set the seed at 1, use rnorm to generate a random sample of size 5,X1,…,X5 from a standard normal distribution, 
## then compute the t-statistic t=√5(X¯/s) with s the sample standard deviation. What value do you observe?
## rnorm() is the R function that simulates random variables having a specified normal distribution
set.seed(1)
X<-rnorm(5)
t<-(sqrt(5)*mean(X))/sd(X)
t

## Question: You have just performed a Monte Carlo simulation using rnorm , a random number generator for normally distributed 
## data. Gosset’s mathematical calculation tells us that the t-statistic defined in the previous exercises, a random variable, 
## follows a t-distribution with N−1 degrees of freedom. Monte Carlo simulations can be used to check the theory: we generate
## many outcomes and compare them to the theoretical result. Set the seed to 1, generate B=1000 t-statistics as done in exercise 
## 1. What proportion is larger than 2?
set.seed(1)
ttestgenerator <- function(n) {
  X <- rnorm(n)
  tstat <- (sqrt(n)*mean(X))/sd(X) 
  return(tstat)
}

ttests <- replicate(1000, ttestgenerator(5))
mean(ttests>=2)


#### Permutation Tests ####
## A permutation test gives a simple way to compute the sampling distribution for any test statistic, under the strong null 
## hypothesis that a set of genetic variants has absolutely no effect on the outcome.
## We will use the following dataset to demonstrate the use of permutations:
library(dplyr)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

## Question: We will generate the following random variable based on a sample size of 10 and observe the following difference:
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)

## The question is whether this observed difference is statistically significant. We do not want to rely on the assumptions 
## needed for the normal or t-distribution approximations to hold, so instead we will use permutations. We will reshuffle the 
## data and recompute the mean. We can create one permuted sample with the following code:
dat <- c(smokers,nonsmokers)
shuffle <- sample( dat )
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
mean(smokersstar)-mean(nonsmokersstar)

## The last value is one observation from the null distribution we will construct. Set the seed at 1, and then repeat the
## permutation 1,000 times to create a null distribution. What is the permutation derived p-value for our observation?
set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)-mean(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 



#### Association Tests ####
d = read.csv("assoctest.csv")
head(d)

## This dataframe reflects the allele status (either AA/Aa or aa) and the case/control status for 72 individuals.
## Compute the Chi-square test for the association of genotype with case/control status (using the table() function and the
## chisq.test() function). Examine the table to see if it look enriched for association by eye. What is the X-squared statistic?
tab <- table(d$allele, d$case)
tab
chisq.test(tab)
## 
##  Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  tab
## X-squared = 3.3437, df = 1, p-value = 0.06746
chisq.test(tab)$p.value

## Question: Compute the Fisher’s exact test ( fisher.test() ) for the same table. What is the p-value?
tab = table(d$allele, d$case)
fisher.test(tab)
## 
##  Fisher's Exact Test for Count Data
## 
## data:  tab
## p-value = 0.05194
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.940442 8.493001
## sample estimates:
## odds ratio 
##   2.758532



#### EDA - Exploratory Data Analysis

## Here we will use the plots we've learned about to explore a dataset: some stats on a random sample of runners from the 
## New York City Marthon in 2002. This data set can be found in the UsingR package (used in the previous assessment). 
## Load the library and then load the nym.2002 data set with the following line:

## Question: Use dplyr to create two new data frames: males and females, with the data for each gender. 
## For males, what is the Pearson correlation between age and time to finish?

data(nym.2002, package="UsingR")
head(nym.2002)
library(dplyr)
males <- filter(nym.2002, gender=="Male") 
females <- filter(nym.2002, gender=="Female") 

cor(males$age,males$time)


## Question: For females, what is the Pearson correlation between age and time to finish?
## Hint: The Pearson correlation is the default correlation returned by the R function cor()
cor(females$age,females$time)


## Question: If we interpret these correlations without visualizing the data, we would conclude that the older we get, 
## the slower we run marathons, regardless of gender. Look at scatterplots and boxplots of times stratified by age groups 
## (20-25, 25-30, etc..).
## After examining the data, what is a more reasonable conclusion?
library(rafalib)
mypar(2,2)
plot(females$age, females$time)
plot(males$age, males$time)
group <- floor(females$age/5) * 5
boxplot(females$time~group)
group <- floor(males$age/5) * 5
boxplot(males$time~group)
## Finish times are constant up through around 50-60, then they get slower.


#### Plots to avoid
## The dslabs package has a dataset called divorce_margarine that has data from 2000 to 2009 about the per capita 
## US consumption of margarine and the divorces per 1000 in Maine. Run this example.
install.packages("dslabs")
library(dslabs)
data("divorce_margarine")
plot(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)
cor(divorce_margarine$margarine_consumption_per_capita, divorce_margarine$divorce_rate_maine)










