#===========================================
# @Project: Analyzing simulated hotel data
# @Name: simulate_data
# @author: slow-data
# @date: 2018/05
#===========================================


rm(list = ls()); gc()


## --- Simulate hotel data
set.seed(123)
n <- 20000

# 1. hotel_id
hotel_id <- as.integer(10000001:10020000)

# 2. country_code
country_code <- as.factor(sample(x = c("fr", "it"), size = n, replace = TRUE))

# 3. city
idx_fr <- country_code == "fr"
idx_it <- country_code == "it"
city <- rep("na", n)
city[idx_fr] <- sample(x = c("Cannes", "Chamonix", "Nice", "Paris", "Tignes"), size = sum(idx_fr), replace = TRUE)
city[idx_it] <- sample(x = c("Milan", "Rome", "Salerno", "Sorrento"), size = sum(idx_it), replace = TRUE)
city <- as.factor(city)

# 4. first_opened_date
first_opened_date <- sample(x = seq(as.Date("2013-01-01"), as.Date("2016-12-31"), by="day"), size = n, replace = TRUE)

# 5. closed_date
closed_date1 <- first_opened_date + sapply(rnorm(n, 200, 150), function(i) round(max(0,i),0)) 
closed_date <- as.Date(ifelse(closed_date1>as.Date("2018-05-27"), as.Date("2018-05-27"), closed_date1), origin="1970-01-01") # cap closed_date by date of analysis    
idx_open <- as.integer(sample(x = 1:n, size = 0.65*n, replace = FALSE))
closed_date[idx_open] <- NA

# 6. avg_rate_per_night_euro
idx_cheap <- as.integer(sample(x = 1:n, size = n/2, replace = FALSE))
idx_expensive <- !((1:n) %in% idx_cheap)
avg_rate_per_night_euro <- rep(0,n) 
avg_rate_per_night_euro[idx_cheap] <- sapply(rnorm(n/2, 60, 20), function(i) round(max(10,i),0))
avg_rate_per_night_euro[idx_expensive] <- sapply(rnorm(n/2, 200, 50), function(i) round(max(100,i),0))

# 7. open status
open_status <- as.factor(ifelse(is.na(closed_date), "OPEN", "CLOSED"))

# 8. nr_rooms
nr_rooms <- as.integer(sapply(rgamma(n = 1000, shape = 0.5, rate = 0.01), function(i) round(max(1, i),0)))

# 9. accomodation_type
accomodation_type <- as.factor(sample(x = c("Apartment", "Bed and Breakfast", "Hotel", "Resort", "Villa"), size = n, replace = TRUE))

# 10. quality_score
quality_score <- as.integer(sample(x = 3:10, size = n, replace = TRUE))

# 11. cancellation_policy
cancellation_policy <- as.factor(sample(x = c("FLEXIBLE", "NON-REFUNDABLE", "STRICT"), size = n, replace = TRUE))

# 12. payment_method
payment_method <- as.factor(sample(x = c("bc", "ca", "ca,bt", "cc, ca, bt", "cc,bt", "cc,ca", "cc,ca,bt,bc"), size = n, replace = TRUE))

# 13. preferred_contact_method
preferred_contact_method <- as.factor(sample(x = c("email", "phone", "sms"), size = n, replace = TRUE))


data <- data.frame(hotel_id = hotel_id, country_code = country_code, city = city, 
                   first_opened_date = first_opened_date, closed_date = closed_date, 
                   avg_rate_per_night_euro = avg_rate_per_night_euro, open_status = open_status,
                   nr_rooms = nr_rooms, accomodation_type = accomodation_type, 
                   quality_score = quality_score, cancellation_policy = cancellation_policy, 
                   payment_method = payment_method, preferred_contact_method = preferred_contact_method)

str(data)
head(data)

# save workspace
rm(list=setdiff(ls(), "data"))
save.image("data/data.RData")
