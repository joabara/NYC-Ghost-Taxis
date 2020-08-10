### HEADER, DESCRIPTION is in main function


### Functions

## IMPORTING FUNCTIONS
import_url_sample <- function(url)
{
	
	#imports data from url
	data = read.csv(url(url), TRUE, ",")

	random_sample = sample(nrow(data), 1e5)

	#returns a 100000 obs. sample from each month
	sample_data = data[random_sample, ]

	return(sample_data)
}


## DATA STRUCTURING FUNCTIONS
seg_time <- function(datetime)
{
	#breaking up the datetime into segments
	div = strsplit(datetime, " ")
	time = as.character(div[[1]][2])

	time_stamp = strsplit(time, ":")

	#selecting hour and min values
	hour = time_stamp[[1]][1]
	min = time_stamp[[1]][2]

	hour = as.numeric(hour)
	min = as.numeric(min)

	#tranform hours into numbers (i.e 15.25 = 15:15 or 3:15 PM)
	minhour = hour + min/60

	return(as.numeric(minhour))
}

seg_month <- function(datetime)
{
	#breaking up the datetime into segments
	div = strsplit(datetime, " ")
	date = as.character(div[[1]][1])

	date_stamp = strsplit(date, "-")

	month = as.numeric(date_stamp[[1]][2])

	#return the month values, so that the program can label what month is being analyzed
	return(as.numeric(month))
}


## ANALYSIS FUNCTIONS
vector_test <- function(lat_coef, lon_coef, supply_drop)
{
	#regression coefficients (beta parameter values, for the statisticians)
	y = lat_coef[[2]]
	x = lon_coef[[2]]

	#vector of direction (for every degree in longitude, avg. latitude changes by "vector")
	vector = y/x

	#printout of results
	output_line = c("VECTOR DIR (LAT, LONG): ", y,", ", x)
	slope_line = c("SLOPE: ", vector)
	pct_drop = c("CAB SUPPLY PERCENT CHANGE: ", supply_drop,"%")

	print(paste(output_line, collapse=""))
	print(paste(slope_line, collapse=""))
	print(paste(pct_drop, collapse=""))
}

analyze_dip <- function(month_sample)
{
	#coerce date data into string format for segmenting
	month_sample$tpep_pickup_datetime = as.character(month_sample$tpep_pickup_datetime)

	#adding minhour and hour collumn to the dataset
	month_sample$minhour = lapply(month_sample$tpep_pickup_datetime, seg_time)
	month_sample$month = lapply(month_sample$tpep_pickup_datetime, seg_month)

	month_sample$minhour = unlist(month_sample$minhour)
	month_sample$month = unlist(month_sample$month)

	#This is so that if one wanted to run the analysis again, they can use any month from 2015
	months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

	selected_month = months[month_sample$month[2]]
	hist_main = c(selected_month, "2015")
	hist_main = paste(hist_main, collapse=' ')

	#Frequency chart for the selected month to analyze
	hist(month_sample$minhour, breaks=48, main= hist_main, xlab="Hour of Day")

	#locator tool to designate the dropzone for vector/supply analysis
	l = locator()

	#rounded time to the nearest tenth for the lat_time table
	month_sample$rounded_time = round(month_sample$minhour/0.1)*0.1

	#latitutde restriction to exclude Staten Island and below
	city_lat_cutoff = month_sample[which(month_sample$dropoff_latitude > 40.5 & month_sample$dropoff_latitude < 40.8), ]

	#looking_glass is the whole area of the afternoon that we mean to examine
	looking_glass = city_lat_cutoff[which(city_lat_cutoff$minhour > 15 & city_lat_cutoff$minhour < 18), ]

	#When cabs start dropping is where we wanna regress
	dropzone = city_lat_cutoff[which(city_lat_cutoff$minhour > l$x[1] & city_lat_cutoff$minhour < l$x[2]),]

	#Regression on latitudes and longitudes
	reg_lat = lm(dropzone$dropoff_latitude ~ dropzone$minhour, dropzone)
	reg_lon = lm(dropzone$dropoff_longitude ~ dropzone$minhour, dropzone)

	#change in supply, multipled by 100 for % interpretation
	supply_drop = (l$y[2]/l$y[1]-1)*100

	#Log printout of statistical results
	print("-------------------------------------------------------------------")
	print(hist_main)
	vector_test(coef(reg_lat), coef(reg_lon), supply_drop)
	print("-------------------------------------------------------------------")

	#Avg. Latitude over every increment of 6 minutes through 3-6 PM
	lat_time = tapply(looking_glass$dropoff_latitude, looking_glass$rounded_time, mean, na.rm = TRUE)

	#labels for graphic content
	times = c("3:00","3:06","3:12","3:18","3:24","3:30","3:36","3:42","3:48","3:54","4:00","4:06","4:12","4:18","4:24","4:30","4:36","4:42","4:48","4:54","5:00","5:06","5:12","5:18","5:24","5:30","5:36","5:42","5:48","5:54","6:00")
	labels = c("Average", "Midtown")

	#plot title with selected month and time
	title = c(hist_main, "Avg. Latitude (15:00-18:00)")
	title = paste(title, collapse= " ")

	### Outputs
	#This is the specified months plot from 3-5, showing how avg latitude changes throughout the afternoon 
	plot(lat_time, type = "l", col = "blue", main = title, ylab="Latitude", xlab="Time", ylim=c(40.748, 40.758), xaxt="n", asp=1000)

	#axis for time labels for easy reading
	axis(1, at=seq(0, 30, by=1), labels= times, col.axis="blue", las=1)

	#average line
	abline(h=mean(looking_glass$dropoff_latitude), col="red")

	#midtown line
	abline(h=40.75, col="orange")

	#legend for clarity
	legend("topright", title="Legend", labels, lwd=2, col = c("red", "orange"))
}

###Inputs

##RUN THE FOLLOWING 11 COMMENTED LINES ONLY ONCE, THEN RECOMMENT WHEN DATA IS COMPLETELY IMPORTED - PLEASE NOTE - DATA INPUT TAKES A WHILE
	#print("Importing csv files...")
	#print("Importing March data...")
	#march_2015_sample = import_url_sample("https://storage.googleapis.com/tlc-trip-data/2015/yellow_tripdata_2015-03.csv")
	#print("March data imported and sampled.")
	#print("Importing June data...")
	#june_2015_sample = import_url_sample("https://storage.googleapis.com/tlc-trip-data/2015/yellow_tripdata_2015-06.csv")
	#print("June data imported and sampled.")
	#print("Importing September data...")
	#sept_2015_sample = import_url_sample("https://storage.googleapis.com/tlc-trip-data/2015/yellow_tripdata_2015-09.csv")
	#print("September data imported and sampled.")
	#print("Import query complete.")


### Main program function
main <- function() 
{
	### HEADER
	print("The Daily NYC Taxi Crisis – An Urban Legend?")
	print("-------------------------------------------------------------------")
	print("Name: Alessandro Joabar")
	print("Files: main.R")
	print("-------------------------------------------------------------------")
	## DESCRIPTION
	description = c("Nine million people in New York City depend on public transportation to move around the city. The Taxi and Limousine Commission as well as the mayors administration is responsible for making sure that the supply of cabs can accommodate the demand at all times of day.",
					"Determine whether there is a significant drop in available cabs during the afternoon peak period, and compare it across three different months to evaluate consistency.")
	print(paste(description, collapse=""))
	print("-------------------------------------------------------------------")

	## ERROR CORRECTION METHODS

	# MARCH ANALYSIS WITH ERROR HANDLING
	march = tryCatch({
	    expr=analyze_dip(march_2015_sample)
	}, warning = function(w) {
	    print("WARNING")
	}, error = function(e) {
		print("MARCH DATA MISSING.")
	})

	# JUNE ANALYSIS WITH ERROR HANDLING
	june = tryCatch({
	    expr=analyze_dip(june_2015_sample)
	}, warning = function(w) {
	    print("WARNING")
	}, error = function(e) {
		print("JUNE DATA MISSING.")
	})

	# SEPTEMBER ANALYSIS WITH ERROR HANDLING
	sept = tryCatch({
	    expr=analyze_dip(sept_2015_sample)
	}, warning = function(w) {
	    print("WARNING")
	}, error = function(e) {
		print("SEPTEMBER DATA MISSING.")
	})
}

setwd("~/School/2015-16 Junior Year/R")
getwd()

#run main program
main()





