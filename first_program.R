print("hello world")
x <- 5
y <- 10
print(x)
print(y)

class(x)

class("hello")

logi <- TRUE
class(logi)

number_guitar_strings <- 6
number_mandolin_strings <- 8

number_guitar_strings > number_mandolin_strings

number_guitar_strings != number_mandolin_strings

"köpek" > "kedi"

help("log")

log(4,2)
log(x = 4, base =2)

sqrt(25)

min(1, 0.75 , 1.25)

nchar("duygu")

#data setlerini düzenlerken işimize yarayabilir

user_number <- 2200469021
as.character(user_number)

user_character <- "2200469021"
as.numeric(user_character)


install.packages("stringr")
library("stringr")
str_count("mississippi","i")


install.packages("dslabs")
library(dslabs)
data(murders)
class(murders)

#structure of an object
str(murders)

#data.frame':	51 obs. of  5 variables:
 #$ state     : chr  "Alabama" "Alaska" "Arizona" "Arkansas" ...
 #$ abb       : chr  "AL" "AK" "AZ" "AR" ...
 #$ region    : Factor w/ 4 levels "Northeast","South",..: 2 4 4 2 4 4 1 2 2 2 ...
 #$ population: num  4779736 710231 6392017 2915918 37253956 ...
 #$ total     : num  135 19 232 93 1257 ...

head(murders)

#objelerin içindeki değişkenlere ulaşmak istediğimizde $ işaretini kullanıyoruz.
  
names(murders)

murders$population

length(murders$population)

murders$state
#order fonksiyonuu en az cinayet değerine sahip eyaletin indexiden en büyük değere sahip eyaletin indexinde kadar sıralar
index <- order(murders$total)
index

murders$total

#index parametresini girdiğimizde o indexe karşılık gelen eyaletleri gösterir
murders$abb[index]

max(murders$total) #max değeri gösterir
min(murders$total)

which.max(murders$total)#max değere sahip eyaletin indexini gösterir
which.min(murders$total)

murders$state[which.max(murders$total)]
murders$state[which.min(murders$total)]

murder_rate <- (murders$total / murders$population)*100000
murders$state[order(murder_rate,decreasing = TRUE)]

ind = which.min(murder_rate)
if (murder_rate[ind] < 0.25) {
  print(murders$state[ind])
} else {
  print("there is no state less than this")
}

print(min(murder_rate))
print(which.min(murder_rate))
print(murders$state[which.min(murder_rate)])

##### vector oluşturma ###

codes <- c(380,124,818)
country <- c("italy","canada","egypt")
names(codes) <- country
print(codes)

codes <- c(italy=380, canada=124, egypt= 818)
print(codes)

seq(1, 10)

seq(1, 10 ,2)

codes[2]

codes[1:2]

## we have a special value for misssing data : NA

x <- c("1", "a" ,"3")
as.numeric(x)

x <- 500
x > 430 & x < 679


a <- 30
if (a %% 2 == 0) {
  print("a is even")
} else {
  print("a is odd")
}

x <- 20
y <- 18

if (x == y) {
  print(" x and y are equal")
} else if (x > y) {
  print("x is greater than y")
} else {
  print("x is less than y")
}

# ifelse statement

a <- 5
ifelse(a >0, 1/a, NA)

a <- c(0,1,2,-4,5)
ifelse (a>0, 1/a, NA)

z <- c(TRUE, FALSE, TRUE)
any(z)
all(z)

#function

avg <- function(x) {
  s <- sum(x)
  n <- length(x)
  s/n
}

x <- 1:100
avg(x)

#calculating the area of a rectangle



area <- function(w,h) {
  return(w*h)
}
area(5,6)

area <- function(w,h) {
  w*h
}
area(3,4)

area <- function(w,h) {
  a <- w * h
  a
}
area(1,4)

compute_s_n <- function(n) {
  x <- 1:n
  sum(x)
}
sapply(1:m, compute_s_n)

m <- 25

s_n <- vector(length = m)
for (i in 1:m) {
  s_n[i] <- compute_s_n(i)
}
n <- 1:m
plot(n, s_n)
lines(n, n*(n+1)/2)

install.packages("tidyverse")
library(tidyverse)

library(dslabs)
data(murders)
head(heights)

prop.table(table(heights$sex))

average <- sum(x) / length(x)
sd <- sqrt(sum( (x - average)^2 / (length(x)-1)))

library(dslabs)
data("heights")
index <- heights$sex == "Male"
x <- heights$height[index]
average <- mean(x)
sd <- sd(x)
c(average = average, sd = sd)


z <- (x - average) / sd  #bu değer ortalamadan kaç standart sapma uzak 
sum(abs(z) < 2) # kaç tane adam 2 standart sapmanın içinde

mean(abs(z) < 2) # yüzde kaçı 2 standart sapmanın içinde
help("pnorm")

pnorm(70.5, mean(x), sd(x))

### observed quantiles therotical quantilesa ne kadar yakınsa veri seti normal dağılıma o kadar uyumludur

p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x,p)
therotical_quantiles <- qnorm(p,mean = mean(x), sd = sd(x))
plot(therotical_quantiles, observed_quantiles)
abline(0,1)

p <- seq(0.05, 0.95, 0.05)
z <- scale(x)
observed_quantiles <- quantile(z, p)
therotical_quantiles <- qnorm(p)
plot(therotical_quantiles, observed_quantiles)
abline(0,1)

## data visualization
library(ggplot2)
library(tidyverse)
library(dslabs)
data("murders")
ggplot(data=murders)
p <- murders %>% ggplot()
p + geom_point(aes(x = population/10^6, y = total, size = 2)) + geom_text(aes(population/10^6, total, label = abb),nudge_x = 1)

install.packages("ggrepel")
install.packages("ggthemes")
library(ggthemes)
library(ggrepel)
### first define the slope of the line
r <- murders %>% summarize(rate = sum(total) / sum(population) * 10^6) %>% .$rate
## now make the plot. 
murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

data(gapminder)
head(gapminder)

str(gapminder)

gapminder %>% filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>% select(country, infant_mortality)

gapminder %>% filter(year == 1962) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point()

gapminder %>% filter(year %in% c(1962, 2012)) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point() + facet_grid(.~year)

gapminder %>% filter(year %in% c(1962, 1970,1980, 1990, 2000, 2012)) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point() + facet_wrap(~year)






