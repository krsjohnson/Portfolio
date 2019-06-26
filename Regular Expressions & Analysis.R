#Krista Johnson


load(url("http://eeyore.ucdavis.edu/stat141/Data/vehicles.rda"))

#Extract price from body of post
get_prices = function(data, price_data){
  rx = "(\\$[0-9 ,]*[0-9,]+(\\.[0-9]{2})?|[0-9]+[0-9,]*(\\.[0-9]{2})?\\$)"
  #pull out the index
  price_index = grep(rx,data)
  #see which ones match the regular expression
  prices = gregexpr(rx, data[price_index])
  #pull out the prices
  price_text = regmatches(data[price_index], prices)
  #Get rid of other characters that aren't numbers
  price_only = lapply(price_text, function(text) as.numeric(gsub('[$,*! ]+', '', text)))
  #choose the largest price (if there is only one, that one will be the largest)
  choose_price = as.matrix(sapply(price_only, function(numbers) max(numbers, na.rm = TRUE)))
  #round up to the nearest dollar to get rid of the .
  no_cents = lapply(choose_price, ceiling)
  #bind the index and the actual prices so you can use them for checking match rate
  return(cbind(price_index, no_cents))
}

one_price = get_prices(vposts$body, vposts$price)
price_index = as.numeric(one_price[,1])
price_values = one_price[,2]
#create new column
vposts$new_price = NA
vposts$new_price[price_index] = price_values

listed_price = as.numeric(vposts$price[price_index])
#Find the values that matched
matches = mapply(identical, price_values, listed_price)
sum(matches)/length(matches)
###############################################################################################################

#Extract the VIN number from body of posted message if present
get_vin = function(data){
  regex_vin = '(VIN|vin|Vin)([:#.])+(.){0,5}([0-9a-zA-Z]){17}(Stock)*'
  vin_index = grep(regex_vin, data)
  vin_text = regmatches(data[vin_index], regexpr(regex_vin, data[vin_index]))
  only_vin = gsub('(VIN|vin|Vin)([:#. ])+(.{0,5})([0-9A-Z]{17})(Stock)*', "\\4", vin_text)
  vin_number = as.list(gsub('([0-9A-Z]{17}).*', '\\1', only_vin))
  return(list(vin_index, vin_number))
}
vin_numbers = as.matrix(cbind(unlist(get_vin(vposts$body)[1]), unlist(get_vin(vposts$body)[2]))) 
index = as.numeric(vin_numbers[,1])
values = vin_numbers[,2]
vposts$vin = NA
vposts$vin[index] = values
#####################################################################################################

#Extract phone numbers from the body of the posted message
get_phone = function(data){
  regex_phone = '((\\([0-9]{3}\\)|[0-9]{3})[ -]?[0-9]{3}\\-[0-9]{4})'
  phone_index = grep(regex_phone, data)
  phone = grepl(regex_phone, data[phone_index])
  phone_text = regmatches(data[phone_index], regexpr(regex_phone, data[phone_index]))
  no_space_phone = gsub(' ', '', phone_text)
  only_phone_numbers = gsub('[()-]', ' ', no_space_phone)
  return(cbind(phone_index, only_phone_numbers))
}

phone_numbers = get_phone(vposts$body)
index_for_phone = as.numeric(phone_numbers[,1])
values_for_phone = phone_numbers[,2]

vposts$phone = NA
vposts$phone[index_for_phone] = values_for_phone
####################################################################################################

#Extract emails from the body of the posted message
get_email = function(data){
  regex_email = '[a-zA-z0-9._]+@[a-zA-Z0-9\n\t .]{1,25}\\.(com|org|net|edu|COM|ORG|NET|EDU)'
  email_index = grep(regex_email, data)
  email_text = regmatches(data[email_index], regexpr(regex_email, data[email_index]))
  only_email = gsub('[\n\t ]', '', email_text)
  return(cbind(email_index, only_email))
}

emails = get_email(vposts$body)
index_for_email = as.numeric(emails[,1])
values_for_email = emails[,2]

vposts$emails = NA
vposts$emails[index_for_email] = values_for_email
#####################################################################################################

#Extract the year from the body of the post and compare value to values in the year column
get_year = function(body, description){
  regex_year = '(18|19|20)[0-9][0-9]'
  #Find the index of the body posts that have a year in them
  year_index = grep(regex_year, body)
  #Count how many have a year in them
  year = grepl(regex_year, body)
  #Find the index of the posts that didn't have a year in the body
  non_year_index = which(year == FALSE)
  #pull out the year
  year_text = as.numeric(regmatches(body[year_index[-non_year_index]], regexpr(regex_year, body[year_index[-non_year_index]])))
  #Use the non_year_index to find a corresponding index for the vposts data
  non_year_original_vpost_index = year_index[non_year_index]
  year_des = grep(regex_year, description[non_year_original_vpost_index])
  #count how many years are found in the description that weren't found in the body
  yeard = grepl(regex_year, description[non_year_original_vpost_index])
  #Pull out the year from the description of observations that weren't found in the body
  yeardes_text = as.numeric(regmatches(description[non_year_original_vpost_index], regexpr(regex_year, description[non_year_original_vpost_index])))
  
  ###Try to find the indecies for yeard that are true
  true_year_descriptions = which(yeard == TRUE)
  ####Find the index where they fall in the subset of false year indecies from original data
  year_descriptions_index = year_des[true_year_descriptions]
  ###Find where they are in the original data
  description_index_original = non_year_original_vpost_index[true_year_descriptions]
  
  all_year_index = c(description_index_original, year_index[-non_year_index])
  all_year_index_text = c(yeardes_text, year_text)
  cbind(all_year_index, all_year_index_text)
}
years = get_year(vposts$body, vposts$description)
index_for_year = as.numeric(years[,1])
values_for_year = years[,2]

vposts$new_year = NA
vposts$new_year[index_for_year] = values_for_year
match_year = mapply(identical, as.integer(vposts$new_year), vposts$year)
sum(match_year)/length(match_year)
######################################################################################################

#Extract model info
get_models = function(description, maker){
  models_regex = '([A-Za-z]{2,}) ([^ -]+)'
  models_index = grep(models_regex, description)
  #see which of the original posts match the regular expression
  models = grepl(models_regex, description)
  #pull out the words
  models_text = regmatches(description[models_index], regexpr(models_regex, description[models_index]))
  #look for make followed by model and then only pull out the model
  models_only = gsub('([A-Za-z]+) ([A-Za-z0-9 ]{1,})', '\\2', models_text)
  #get rid of the pointing had icon
  final_models = gsub("[^A-Za-z0-9]", "", models_only)
  #get rid of common words that are not makes or models
  final_models_really = tolower(gsub('(SALE[A-Za-z]+|CREDIT|Navigator[A-Za-z]?)', NA , final_models))
  
  #find all the uniqe makes in the maker column and collapse them with the or to use as my regular expression
  makers = paste(unique(maker), collapse = "|")
  #make sure none of the models that I have extracted are really the name of the make
  models_without_make = gsub(makers, NA, final_models_really)
  cbind(models_index, models_without_make)
}
models = get_models(vposts$description, vposts$maker)
index_for_model = as.numeric(models[,1])
values_for_model = models[,2]

vposts$model = NA
vposts$model[index_for_model] = values_for_model

#made this from copying and pasting the model names (human error could mess up the match rate)
car_models = as.list(read.csv("~/Downloads/List_Car_Makes.csv", header = FALSE))
model_string = as.vector(unlist(car_models))
#Get rid of strange invisible characters that aren't the normal characters
Encoding(model_string) <- "latin1"
#change everything to lower case and collapse it into a regular expression
list_models = tolower(paste(unique(model_string), collapse = "|"))
check_model = grepl(list_models, vposts$model)
sum(check_model)/length(check_model)
######################################################################################################

###########Modeling###################

#I only wanted to look at how "good" cars compared
good =  c("good", "like new", "new", "excellent", "very good", "mint","nice", "restored",
          "muscle car restoration", "superb original", "nice teuck", "207,400")
good_info = subset(vposts, vposts$condition %in% good)

values = c("price", "odometer", "year")

corolla_info = good_info[which(good_info$model == "corolla"),]
#Only use the observations where the prices are a confirmed match
corolla_price_matches = mapply(identical, as.integer(corolla_info$new_price), corolla_info$price)
#while going through the plots, I found an outlier that was interfering with the distribution of my data
#so I decided to remove it
corolla_data = corolla_info[which(corolla_price_matches == TRUE)[-23],]


plot(corolla_data[values], main = "Relationships between Corolla Variables")

corolla_line = lm(price ~ odometer + year, data = corolla_data)
corolla_residuals = resid(corolla_line)
corolla_fitted = fitted(corolla_line)
plot(corolla_fitted, corolla_residuals, main = "Corolla residuals plot", ylab = "residual values", xlab = 'fitted values')
#the residuals vs. fitted values for the corolla appear to be slightly non-linear
qqnorm(corolla_residuals, main = "Corolla Normal Q-Q Plot")
#However, the qq plot shows that the data is relatively normal
summary(corolla_line)
#Check if interaction is significant
reduced_corolla = lm(price ~ odometer + year, data = corolla_data)
full_corolla = lm(price ~ odometer + year + (odometer*year), data = corolla_data)
anova(reduced_corolla, full_corolla)

#Predict almost new car
predict(full_corolla,data.frame(odometer=100, year=2015),interval="prediction")
#Now if the car was 15 years old
predict(full_corolla,data.frame(odometer=200000, year=2000),interval="prediction")

civic_info = good_info[which(good_info$model == "civic"),]
civic_price_matches = mapply(identical, as.integer(civic_info$new_price), civic_info$price)
civic_data = civic_info[which(civic_price_matches == TRUE),]


plot(civic_data[values], main = "Relationships between Civic Variables")

civic_line = lm(price ~ odometer + year, data = civic_data)
civic_residuals = resid(civic_line)
civic_fitted = fitted(civic_line)
plot(civic_fitted, civic_residuals, main = "Civic residuals plot", ylab = "residual values", xlab = "fitted values")
#Once again, the residuals vs. fitted values for the civic appear to be non-linear
qqnorm(civic_residuals, main = "Civic Normal Q-Q Plot")
#The qq plot shows that data to be approximately normally distributed
summary(civic_line)
#Check if interaction is significant
reduced_civic = lm(price ~ odometer + year, data = civic_data)
full_civic = lm(price ~ odometer + year + (odometer*year), data = civic_data)
anova(reduced_civic, full_civic)

#Predict price of almost new car
predict(full_civic,data.frame(odometer=100, year=2015),interval="prediction")
#Car that is 15 years old
predict(full_civic,data.frame(odometer=200000, year=2000),interval="prediction")

#Finally, I wanted to determine if the price distributions were different for each car on the east and 
#west coast
west_corolla = corolla_data$price[which(corolla_data$city == "sfbay")]
east_corolla = corolla_data$price[which(corolla_data$city == "boston")]
hist(west_corolla, main = "Price Distribution of Corollas in SF", xlab = "Price (in dollars)", 
     ylab = "Number of Posts in Corolla Subset of sfbay", xlim = c(0, 20000), ylim = c(0, 18))
hist(east_corolla, main = "Price Distribution of Corollas in Boston", xlab = "Price (in dollars)", 
     ylab = "Number of Posts in Corolla Subset of boston", xlim = c(0, 20000), ylim = c(0, 18))

west_civic = civic_data$price[which(civic_data$city == "sfbay")]
east_civic = civic_data$price[which(civic_data$city == "boston")]
hist(west_civic, main = "Price Distribution of Civics in SF", xlab = "Price (in dollars)", 
     ylab = "Number of Posts in Civic Subset of sfbay", xlim = c(0, 20000), ylim = c(0, 18))
hist(east_civic, main = "Price Distribution of Civics in Boston", xlab = "Price (in dollars)", 
     ylab = "Number of Posts in Civic Subset of boston", xlim = c(0, 20000), ylim = c(0,18))
