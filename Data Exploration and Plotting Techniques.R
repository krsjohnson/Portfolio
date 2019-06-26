#Krista Johnson
#URLs of sites used in code


library(lattice)
library(maps)
library(ggplot2)
library(gmodels)
library(RColorBrewer)

print(load(url("http://eeyore.ucdavis.edu/stat141/Data/vehicles.rda")))

# Explore dataset - number of observations

dim(vposts)
# 34677 rows
# 26 variables


# Identify the names of variables
namevehicle = names(vposts)
#id,title, body, lat, long, posted, updated, drive, odometer, type, header, condition, cylinders,
#fuel, compact, transmission, byOwner, city, time, description, location, url, price, year, maker,
#makerMethod

# Identify class of variables - put into list
diffclasses = sapply(vposts, class)


#calculate the outliers by using the IQR
quantile_price = quantile(vposts$price, na.rm = TRUE)
q1 = quantile_price[[2]]
q3 = quantile_price[[4]]
iqr = q3 - q1
lowfence = q1 - iqr
highfence = iqr + q3

# Calculate Average, median, and quartiles of various vehicle prices

######This was before the outliers were removed#########
avg_price = mean(vposts$price, na.rm = TRUE)
#49449.9
med_price = median(vposts$price, na.rm = TRUE)
#6700
quant_price = quantile(vposts$price, seq(0,1, length = 11), na.rm = TRUE)
#http://www.r-bloggers.com/quartiles-deciles-and-percentiles/ (got this off the web)
#0%       10%       20%       30%       40%       50%       60%       70%       80%       90% 
#  1      1200      2499      3500      4995      6700      8900     11888     15490     21997 
#100% 
#600030000 
max(vposts$price, na.rm = TRUE)


#########This is after the outliers are removed#############
no_outliers = subset(vposts, vposts$price <= highfence)

#http://www.statmethods.net/management/subset.html
new_avgprice = mean(no_outliers$price, na.rm = TRUE)
new_medprice = median(no_outliers$price, na.rm = TRUE)
new_quantprice = quantile(no_outliers$price, seq(0,1, length = 11), na.rm = TRUE)


#Plot your distribution
hist(no_outliers$price, breaks = seq(0, 30000, by = 1000), main = "Distribution of Prices of Vehicles",
     xlab = "Price")
legend("topright", c("Average", "Median", "Deciles"), lty = 1, col = c("blue", "red", "green"),
       bty = "n")
abline(v = new_avgprice, col = "blue", lwd = 3, lty = 2)
abline(v = new_medprice, col = "red", lwd = 3, lty = 1)
abline(v = new_quantprice, col = "green", lwd = 3, lty = 2)

#Determine the different vehicle types and their corresponding proportions
table(vposts$type)
unique(vposts$type)
length(unique(vposts$type))
#coupe       SUV         sedan       hatchback   wagon       van         <NA>        convertible
#pickup      truck       mini-van    other       bus         offroad    
table(vposts$type)
table(vposts$type)/sum(table(vposts$type))
#bus convertible       coupe   hatchback    mini-van     offroad       other 
#0.001171147 0.037583178 0.086558424 0.043598616 0.024114985 0.003513442 0.035453820 
#pickup       sedan         SUV       truck         van       wagon 
#0.048389673 0.374767101 0.224168219 0.063987224 0.026989619 0.029704552  



# Plot relationship between fuel and transmission type
rel_type_fuel = ggplot(na.omit(vposts[,c("fuel", "type", "transmission")]))
rel_type_fuel + geom_bar(aes(x = fuel, fill = factor(type))) + facet_wrap(~transmission)

# Explore different cities represented in the dataset
length(unique(vposts$city, na.rm = TRUE))
#There are 7 different cities

#Display proportion of 'For Sale by Owner' and 'For Sale by Dealer' by city
table(vposts$city)

all = table(vposts$byOwner, vposts$city)
barplot(all, col = c("mediumseagreen", "lightsalmon"), xlab = "Cities", ylab = "Frequency",
        main = "Plot of cars for sale by owner and dealer", 
        legend = c("Dealer", "Owner"), ylim = c(2000, 3000), beside = TRUE, 
        xpd = FALSE)


# Examine highest price for vehicle 
max(vposts$price, na.rm = TRUE)
#600030000

#Fix incorrect prices
bad = (vposts$price == 600030000 | vposts$price == 30002500 | vposts$price == 9999999)
max(vposts$price[!bad], na.rm = TRUE)


# Look at the 3 most common makes of cars by city and by for sale by owner vs for sale by dealer
everything = with(vposts, table(maker, city, byOwner))
owneryes = everything[,,2]
findmostmakers = function(i){
  most = order(i, decreasing = TRUE)[1:3]
  rownames(owneryes)[most]
}
apply(owneryes, 2, findmostmakers)

#boston      chicago     denver      lasvegas    nyc      sac         sfbay   
# "ford"      "chevrolet" "ford"      "ford"      "nissan" "toyota"    "toyota"
# "honda"     "ford"      "chevrolet" "chevrolet" "toyota" "ford"      "honda" 
# "chevrolet" "honda"     "toyota"    "toyota"    "honda"  "chevrolet" "ford"  

ownerno = everything[,,1]
findmostmakers = function(i){
  most = order(i, decreasing = TRUE)[1:3]
  rownames(ownerno)[most]
}
apply(ownerno, 2, findmostmakers)

#     boston      chicago     denver      lasvegas    nyc      sac         sfbay   
#    "ford"      "chevrolet" "ford"      "ford"      "nissan" "ford"      "toyota"
#    "toyota"    "ford"      "chevrolet" "nissan"    "toyota" "toyota"    "ford"  
#    "chevrolet" "nissan"    "dodge"     "chevrolet" "honda"  "chevrolet" "bmw" 

# Visualization of age of cars by age and city
names(vposts)
unique(vposts$city)

xyplot(vposts$city~vposts$year | vposts$byOwner, vposts, xlim = c(1870, 2020),
       xlab = "Vehicle Age", ylab = "Cities", 
       main = "Distribution of Vehicle Age by Cities and if Sold by Owner")

#Locations plotted on a map
map("state")
with(vposts, points(vposts$long, vposts$lat, col = "red", pch = "."), na.rm = TRUE)
#the points are all in the United states and all of the points are clustered around
#the major cities. However there are some points that are not close to the major cities
#and there are a lot clustered around southern California, even though a city is not
#listed near that region. 



#Another view of distibution
pl = ggplot(na.omit(vposts[,c("fuel", "type", "transmission", "drive")]))
pl + geom_bar(aes(x = fuel, fill = factor(type))) + facet_wrap(~drive ~transmission)

#Plot relationship between odometer and age of car
newodometer = which(vposts$odometer < 300000)
newyear = vposts$year[which(vposts$odometer < 300000)]
newprice = vposts$price[which(vposts$odometer < 300000)]
xyplot(newodometer~newyear, na.rm = TRUE, xlim = c(1870, 2020), ylim = c(0, 45000), 
       xlab = "Vehicle Year", ylab = "Odometer Reading", 
       main = "The Relationship between Odometer Reading and Age of Vehicle", pch=".")




#Plot relationship between odometer and price of car
xyplot(newodometer~newprice, na.rm = TRUE, xlim = c(0, 100000), ylim = c(0, 45000), 
       xlab = "Vehicle Price", ylab = "Odometer Reading", 
       main = "The Relationship between Odometer Reading and Price of Vehicle", 
       pch=".")


#Explore characteristics of older cars
#http://www.classiccarclub.org/grand_classics/approved_classics.html
old = subset(vposts, vposts$year <= 1948 & vposts$year >= 1915)
sort(table(old$maker))
#The three highest makers for classic cars were ford, chevrolet, with willys and buick tied

oldinfo = ggplot(na.omit(old[,c("year", "maker", "price")]))
oldinfo + geom_point(aes(x = year, y = price)) + facet_wrap(~maker)
#For bugatti, there is only one post for a vehicle manufactured in the late 1920s being sold for
#a small amount of money. For buick, a majority of the vehicles being sold were manufactured
#in the early 1940s and are being sold for between $0 - 20000. For Cadillac, the two posts consist
#of cars that were manufactured around 1940 and around 1948. The older model is priced at close to 
#$40000, while the newer model is being sold for close to $0. For chevrolet, the posts are for
#vehicles ranging from early 1930s, up to late 1940s. The highest priced vehicles are the older
#models, however, there are low priced older and younger models. For Desoto, there is only one vehicle
#posted for sale. It was manufactured in the late 1920s and is being sold for less than $10000.
#For Dodge, a majority of the models being sold were manufactured in the late 1930s, early 1940s.
#All of them are being sold for less than $20000. For Ford, the models range from early 1920s, to 
#late 1940s. The highest priced models were manufactured between 1930 and 1940. For Hudson,
#there are only two posts for vehicles manufactured in the 1930s. The higher priced model is the younger
#model. For international vehicles, there was only one post for a vehicle manufactured in the late
#1930s and is being sold for almost $0. For Jeep, there is one post of a vehicle manufactured in the
#early 1940s and is being sold for less than $10000. For Lincoln, there is only one post for a 
#vehicle manufactured around 1940, that is being sold for around $20000. For Mercury, there are only
#two posts for vehicles manufactured in the late 1940s. The older model is being sold for around
#$30000, which is greater in price than the younger model. For Plymouth, there are only two posts
#for vehicles that were manufactured in the late 1920s and the late 1930s. The younger model is
#being sold for a substantially more amount of money, around $60000. For Willys, vehicles being sold
#were all manufactured after 1938. There is one vehicle that was manufactured in 1941, same as two other
#posts, that is being sold for $125000, while the other posts are selling for $25000. The price decreases
#as the models get younger. 

