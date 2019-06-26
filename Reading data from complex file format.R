###Krista Johnson



alllists = lapply(c("cloudhigh", "cloudmid", "cloudlow", "ozone", "pressure", "surftemp", "temperature"), 
                  function(p) list.files("~/Downloads/NASA 2", pattern = p, full.names = TRUE))
setwd("~/Downloads/NASA 2")


#Reading and extracting values from complex file format (requires us to skip first 7 lines of the file)
findinfo = function(x){
  orig = read.table(x, skip = 7, stringsAsFactors = FALSE)
    #get the points that are in the fourth columns onward
  measurements = as.numeric(as.matrix(orig[, -(1:3)]))
  
  #Pull out the longitude values (which are the first column in the data)
  long = as.character(orig[[1]])
  long = rep(long, times = 24)
    #divide the longitude values into the number and the character
  difflongs = substr(long, 1, nchar(long)-1)            #Pull out only the number for the longitude
  charlongs = substr(long, nchar(long), nchar(long))    #Pull out only the letter for the longitude
    #use the index of the characters that are S for multiplying -1 to the same index in the number vector
  difflongs[charlongs == "S"] = as.numeric(difflongs[charlongs == "S"])*(-1)
  
  #Find the latitude values
  top = readLines(x, n= 7)
    #split up the line
  toplat = strsplit(top[6], " +")[[1]]
  lat = rep(toplat[-1], each = 24)
  difflats = substr(lat, 1, nchar(lat)-1)           #Pull out only the number for the latitude
  charlats = substr(lat, nchar(lat), nchar(lat))    #Pull out only the letter for the talitude
    #use the index of the characters that are W for multiplying -1 to the same index in the number vector
  difflats[charlats == "W"] = as.numeric(difflats[charlats == "W"])*(-1)
  
  #Find the dates
  dateinfo = readLines(x, n = 5) 
    #Split the line to extract the values
  datelines = strsplit(dateinfo, "\n")
  datesmall = strsplit(datelines[[5]], ":")
    #assuming all of the dates are the same length, find only the date portion of the string
  date = substring(datesmall[[1]][2], 2, 12)
    #change it to a date class
  date = as.Date(date, format = "%d-%b-%Y")
  
  #Find elevation points#
  data3 = read.table("intlvtn.dat", check.names = FALSE)
    #Loop through data frame and pull out the points
  points = list(rapply(data3, paste))
    #Convert them to numeric
  points = rapply(points, as.numeric)
    #Put them into a data frame
  elevation_pts = data.frame(elevation = points)
  
  #Compile everything
  newdf = data.frame(measurement = measurements, long = as.numeric(difflongs), lat = as.numeric(difflats), 
                     date = rep(date, each = length(lat)), elevation = elevation_pts)
}



#Read multiple files for a variable
readall = function(i){
  dfs = data.frame(lapply(1:7, function(t) {
      #run through each of the seven variables separately
    dfs=i[[t]]
      #Change the values to character strings so they can be used as the text file name in the function
    chardfs = sapply(dfs, as.character)
    dfsnew = lapply(chardfs, findinfo)
    allofthem = lapply(dfsnew[1:72], rbind)
    allofthem = do.call(rbind, dfsnew)}), row.names = NULL)
}
alldata = readall(alllists)
#remove the duplicates
alldata = alldata[, -c(7:10, 12:15, 17:20, 22:25, 27:30, 32:35)]
colnames(alldata) = c("cloudhigh","long", "lat", "date", "elevation", "cloudmid", "cloudlow", "ozone", "pressure",
                      "surftemp", "temperature")


#########Step 2################
read_check = function(texts){
  dat = read.table(texts, skip = 7)
  newdat = data.frame(measurement = as.matrix(dat[, -(1:3)]))
  col1 = as.numeric(as.matrix(dat[, -(1:3)]))
  checkdf = data.frame(measurements = col1)
}

check = function(i){
  dfs = data.frame(lapply(1:7, function(t) {
    dfs=i[[t]]
    chardfs = sapply(dfs, as.character)
    dfsnew = lapply(chardfs, read_check)
    allofthem = lapply(dfsnew[1:72], rbind)
    allofthem = do.call(rbind, dfsnew)}))
}

the_check = check(alllists)

length(the_check[,1]) == length(alldata[,1])
#This value returns a TRUE so we know they return the same length




#Plot Temperature vs Pressure
  #divide the cloudlow data into four categories
cutcloud = cut(alldata$cloudlow, c(0,15.0, 23.5, 34.5, 100))
  #assign them colors
pretties = c("coral3", "darkblue", "darkolivegreen", "lightblue3")[cutcloud]
  #plot the data
with(alldata, plot(pressure, temperature, col = pretties, pch = 20))
title(main = "Plot of Pressure vs. Temperature by Cloudlow")
legend("topleft", legend = c("0 - 15.0", "15.0 - 23.5", "23.5 - 34.5", "34.5 - 100"), 
       lty = 1, col = c("coral3", "darkblue", "darkolivegreen", "lightblue3"), bty = "n", cex = .75)

#Plot the four corners
find_extremes = function(lati, longi){
    #find the mins and maxs of the longitude and latitude
  minlat = min(as.numeric(lati))
  minlong = min(as.numeric(longi))
  maxlat = max(as.numeric(lati))
  maxlong = max(as.numeric(longi))
    #Put them into a data frame
  extremes = data.frame(minlat = minlat, minlong = minlong, maxlat = maxlat, maxlong = maxlong)
}
extreme_values = find_extremes(alldata$lat, alldata$long)


corner_plots = function(cornerlat, cornerlong, points, time){
    #Find the temperature points that correspond with each combination of mins, maxs, longitudes, and latitudes
  minmin = points[cornerlat == extreme_values$minlat & 
                    cornerlong == extreme_values$minlong]
  minmax = points[cornerlat == extreme_values$minlat & 
                    cornerlong == extreme_values$maxlong]
  maxmin = points[cornerlat == extreme_values$maxlat & 
                    cornerlong == extreme_values$minlong]
  maxmax = points[cornerlat == extreme_values$maxlat & 
                    cornerlong == extreme_values$maxlong]
    #Put them in a data frame so it is easier to pull out columns for plotting
  allcorners = data.frame(minmin = minmin, minmax = minmax, maxmin = maxmin, maxmax = maxmax, 
                          year = unique(time))
  par(mfrow = c(2,2))
    #Plot the data
  with(allcorners, plot(year, minmin, ylim = c(200, 350), ylab = "measurement"))
  title(main = "Plot for Corner 1 (Minimum Latitude and Longitude)")
  with(allcorners, plot(year, minmax, ylim = c(200, 350), ylab = "measurement"))
  title(main = "Plot for Corner 2 (Minimum Latitude, Maximum Longitude)")
  with(allcorners, plot(year, maxmin, ylim = c(200, 350), ylab = "measurement"))
  title(main = "Plot for Corner 3 (Maximum Latitude, Minimum Longitude)")
  with(allcorners, plot(year, maxmax, ylim = c(200, 350), ylab = "measurement"))
  title(main = "Plot for Corner 4 (Maximum Latitude, Maximum Longitude)")
}
corner_plots(alldata$lat, alldata$long, alldata$temperature, alldata$date)


#Compute Mean and SD for each location over time
  #separate the longitude and latitude values
single = alldata[, 2:3]
  #Create a column with both in it, separated by a space
q3data = mapply(paste, single[,1], single[,2])
  #Find the unique combinations of latitudes and longitudes
oncevalues = unique(q3data)
  #Make a new column using the unique combinations
alldata$new = oncevalues

  #Subset the data and loop through each point over time to find the mean and standard deviation
splitpressure = split(alldata$pressure, alldata$new)
meanpress = sapply(splitpressure, mean)
stdevpress = sqrt(as.numeric(sapply(splitpressure, var)))
splitcloudhigh = split(alldata$cloudhigh, alldata$new)
meanhigh = sapply(splitcloudhigh, mean)
stdevhigh = sqrt(as.numeric(sapply(splitcloudhigh, var)))
splitcloudlow = split(alldata$cloudlow, alldata$new)
meanlow = sapply(splitcloudlow, mean)
stdevlow = sqrt(as.numeric(sapply(splitcloudlow, var)))
splitcloudmid = split(alldata$cloudhigh, alldata$new)
meanmid = sapply(splitcloudmid, mean)
stdevmid = sqrt(as.numeric(sapply(splitcloudmid, var)))
splitozone = split(alldata$ozone, alldata$new)
meanozone = sapply(splitozone, mean)
stdevozone = sqrt(as.numeric(sapply(splitozone, var)))
splitsurftemp = split(alldata$surftemp, alldata$new)
meansurf = sapply(splitsurftemp, mean)
stdevsurf = sqrt(as.numeric(sapply(splitsurftemp, var)))
splittemperature = split(alldata$temperature, alldata$new)
meantemp = sapply(splittemperature, mean)
stdevtemp = sqrt(as.numeric(sapply(splittemperature, var)))
allmeanstdv = data.frame(meanlow = meanlow, stdvlow = stdevlow, meanhigh = meanhigh, stdvhigh = stdevhigh,
                         meanmid = meanmid, stdvmid = stdevmid, meanpressure = meanpress, stdvpressure = stdevpress,
                         meanozone = meanozone, stdvozone = stdevozone, meansurf = meansurf, stdvsurf = stdevsurf,
                         meantemp = meantemp, stdvtemp = stdevtemp, row.names = NULL)


#Display avg. pressure on map
library(maps)
par(mfrow = c(1,1))
  #Make the plotting area more specific to the region where the data is located
map(xlim = c(-120, -30), ylim = c(-60, 50))
  #Divide the pressure points into 4 different categories
areas = cut(as.numeric(meanpress), c(650, 924, 977, 995, 1000))
  #Plot the points on the map with a legend
with(alldata, points(lat, long, pch = "20", col = rev(heat.colors(4, alpha = 1)[areas])))
legend("bottomleft" , legend = c("650-924", "924-977", "977-995", "995-1000"), 
       lty = 1, col = rev(heat.colors(4, alpha = 1)), bty = "n", cex = .75)
title(main = "Map of Average Pressure Readings by Latitiude and Longitude", cex.main = .75)

#Plot avg. elevation and surface temp.
data4 = read.table("intlvtn.dat", check.names = FALSE)
  #Pull out the elevation points
points = list(rapply(data4, paste))
elevation = rapply(points, as.numeric)
  #Plot the points against the averages of the surface temperature
plot(meansurf, elevation, main = "Surface Temp vs Elevation", xlab = "surface temperature")

