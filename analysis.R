#===========================================
# @Project: Analyzing simulated hotel data
# @Name: analysis
# @author: slow-data
# @date: 2018/05
#===========================================


## --- Objective

# Given a dataset with properties contracted by a booking platform, 
# setup an analysis to understand the dynamics and the reason of openings and closings 


# ---------------------------------------
# Importing data
# --------------------------------------

rm(list = ls()); gc()
load("data/data.RData")

# ---------------------------------------
# Libraries
# --------------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(TTR)
library(effects)
library(caret)
library(RColorBrewer)
library(plyr)




# --------------------------------
# cleaning data & trasnforming data
# ---------------------------------

summary(data)

# hotel_id is a key for data?
nrow(data)==length(unique(data$hotel_id))

# calculate other variables
data$first_opened_year <- strftime(data$first_opened_date, "%Y")
data$first_opened_yearmonth <- strftime(data$first_opened_date, "%Y%m")
table(data$first_opened_yearmonth)
data$closed_yearmonth <- strftime(data$closed_date, "%Y%m")


# ----------------------------------------
# Dataset France
# ----------------------------------------

data_fr <- data %>%
    filter(country_code=="fr") 

# drop unused levels
table(data_fr$country_code)
data_fr$country_code <- droplevels(data_fr$country_code)
table(data_fr$country_code)
data_fr$city <- droplevels(data_fr$city)


# ----------------------------------------
# Overview France
# ----------------------------------------

data_fr %>%
  count(first_opened_year, city)

nrow(data_fr)

data_fr %>%
  filter(!is.na(closed_date)) %>%
  nrow()

data_fr %>%
  filter(open_status=="OPEN") %>%
  count(city)

data_fr %>%
  count(city)



# ----------------------------------------
# Visualizing France
# ----------------------------------------

data_gg1 <- data_fr %>%
  filter(open_status=="OPEN") %>%
  count(city, accomodation_type)

windowsFonts(
  A=windowsFont("Arial Black"),
  B=windowsFont("Bookman Old Style"),
  C=windowsFont("Comic Sans MS"),
  D=windowsFont("Symbol")
)

gg1 <- ggplot(data = data_gg1, aes(x = city, y = n, fill = accomodation_type)) 
gg1 + geom_bar(stat = "identity") +
  labs(title = expression(paste("N. of French properties opened across 2013-2016, \n still opened as of 2018-05-27")), 
       x="", y="") +
  theme(plot.title = element_text(hjust = 0, vjust=-4, size = 14, family = "Times"),
        axis.text.x = element_text(size = 12, angle = 45, family = "Times"),
        axis.text.y = element_text(size = 12, family = "Times"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey")        
  ) +
  guides(fill=guide_legend(title="Accomodation Type"))


# by average rate x night
data_gg2 <- data_fr %>%
  filter(open_status=="OPEN" & avg_rate_per_night_euro>0) %>%
  count(city, avg_rate_per_night_euro)

gg2 <- ggplot(data_gg2, aes(x = avg_rate_per_night_euro, group = city, fill = city)) +
  geom_density(alpha = .4) +
  xlab("Average rate per night") +
  ylab("") +
  ggtitle("Distribution of Average rate per night - by City") +
  guides(fill=guide_legend("City")) +
  #  scale_fill_discrete(labels = c("No*", "Si")) +
  theme(plot.title = element_text(hjust = 0, vjust=2, size = 14, family = "Times"),
        axis.text.x = element_text(size = 12, family = "Times"),
        axis.text.y = element_text(size = 12, family = "Times"),
        panel.background = element_rect(fill = "white")
  ) 

gg2



# try to explain the bimodality

# by average rate x night
data_gg3 <- data_fr %>%
  filter(open_status=="OPEN" & avg_rate_per_night_euro>0) %>%
  count(city, avg_rate_per_night_euro, accomodation_type)

gg3 <-ggplot(data_gg3, aes(x = avg_rate_per_night_euro, group = city, fill = city)) +
  geom_density(alpha = .4) +
  facet_wrap( ~ accomodation_type, ncol = 2) +
  xlab("Average rate per night (???)") +
  ylab("") +
  ggtitle("Distribution of Average rate per night - by City and Property type") +
  guides(fill=guide_legend("City")) +
  #  scale_fill_discrete(labels = c("No*", "Si")) +
  theme(plot.title = element_text(hjust = 0, vjust=2, size = 14, family = "Times"),
        axis.text.x = element_text(size = 12, family = "Times"),
        axis.text.y = element_text(size = 12, family = "Times"),
        panel.background = element_rect(fill = "white")
  ) 

gg3


# by number of rooms
data_gg4 <- data_fr %>%
  filter(open_status=="OPEN") %>%
  count(city, nr_rooms)

gg4 <-ggplot(data_gg4, aes(x = nr_rooms, group = city, fill = city)) +
  geom_density(alpha = .4) +
  xlab("Average rate per night") +
  ylab("") +
  ggtitle("Distribution of Average rate per night - by City") +
  guides(fill=guide_legend("City")) +
  #  scale_fill_discrete(labels = c("No*", "Si")) +
  theme(plot.title = element_text(hjust = 0, vjust=2, size = 14, family = "Times"),
        axis.text.x = element_text(size = 12, family = "Times"),
        axis.text.y = element_text(size = 12, family = "Times"),
        panel.background = element_rect(fill = "white")
  ) 

gg4


# by quality score
data_gg5 <- data_fr %>%
  filter(open_status=="OPEN") %>%
  count(city, quality_score)

gg5 <- ggplot(data = data_gg5, aes(x = city, y = n, fill = quality_score)) 
gg5 + geom_bar(stat = "identity") +
  labs(title="",
       x="", y="") +
  theme(plot.title = element_text(hjust = 0, vjust=2, size = 14, family = "Times"),
        axis.text.x = element_text(size = 12, family = "Times"),
        axis.text.y = element_text(size = 12, family = "Times"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "grey")        
  ) +
  guides(fill=guide_legend(title="Quality Score"))






# --------------------------------------------
# # in which French cities do we have hotels?
# --------------------------------------------

data_g1 <- data_fr %>% count(city) %>% arrange(n)

g <- ggplot(data = data_g1, aes(x = reorder(city, -n)  , y = n)) 
g + geom_bar(stat = "identity") + 
  labs(title="Number of hotels in French cities",
       x="City", y="") +
  theme_minimal()



# ----------------------------------------------------------------
# how many properties we're opening and closing and why - OVERALL
# ----------------------------------------------------------------


x2 <- data_fr %>% count(first_opened_yearmonth)
names(x2) <- c("period", "n_opened")

y2 <- data_fr %>% count(closed_yearmonth)
names(y2) <- c("period", "n_closed")

z2 <- full_join(x2, y2, by = c("period"))
z2 <- z2 %>%
  filter(!is.na(period))

cleanDataTable = function(DT) {
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

cleanDataTable(z2)

z2$opened <- cumsum(z2$n_opened)-cumsum(z2$n_closed)
z2_201612 <- z2 %>% filter(period<=201612)

z2_new <- z2_201612[,c("period", "n_opened", "n_closed")]
setDT(z2_new)
z2_Melted <- melt(z2_new,id=c("period"))

ggplot(z2_Melted) + geom_line(aes(x=period, y=value, colour=variable, group = variable)) +
  ylab(label="") + 
  xlab("") +
  scale_linetype(guide = guide_legend(override.aes = list(alpha = 1)), labels = c("N. opened", "N. closed")) + 
  theme_minimal() +
  scale_colour_discrete(name = "", labels=c("N. openings","N. closings")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = c(201301, 201307, 201401, 201407, 201501, 201507, 201601, 201607, 201612))




# -----------------------------------------------------------------
# how many properties we're opening and closing and why - by City
# -----------------------------------------------------------------

x22 <- data_fr %>% count(first_opened_yearmonth, city)
names(x22) <- c("period", "city", "n_opened")

y22 <- data_fr %>% count(closed_yearmonth, city)
names(y22) <- c("period", "city", "n_closed")

z22 <- full_join(x22, y22, by = c("period", "city"))
z22 <- z22 %>%
  filter(!is.na(period))

cleanDataTable = function(DT) {
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

cleanDataTable(z22)

z22$opened <- cumsum(z22$n_opened)-cumsum(z22$n_closed)
z22_201612 <- z22 %>% filter(period<=201612)

z22_new <- z22_201612[,c("period", "city", "n_opened", "n_closed")]
setDT(z22_new)
z22_Melted <- melt(z22_new,id=c("period", "city"))

ggplot(z22_Melted) + geom_line(aes(x=period, y=value, colour=variable, group = variable)) +
  ylab(label="") + 
  xlab("") +
  scale_linetype(guide = guide_legend(override.aes = list(alpha = 1)), labels = c("N. opened", "N. closed")) + 
  theme_minimal() +
  scale_colour_discrete(name = "", labels=c("N. openings","N. closings")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = c(201301, 201307, 201401, 201407, 201501, 201507, 201601, 201607, 201612)) +
  facet_wrap( ~ city, ncol = 2)




# ----------------------------------
# Understanding closing 
# ----------------------------------

CL2palette=rev(brewer.pal(n = 9, name = "RdYlBu"))

data_fr <- data_fr %>% 
  mutate(close = if_else(
    !is.na(closed_date), 
    1, 
    0)) 

data_fr_mod <- data_fr %>%
  select(-c(hotel_id, country_code, first_opened_date, closed_date, open_status, 
            first_opened_yearmonth, closed_yearmonth, first_opened_year)) %>%
  filter(avg_rate_per_night_euro>0)


## --- Model 1
logistic <- glm(close ~. , data=data_fr_mod, family=binomial)
summary(logistic)
plot(allEffects(logistic))

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coefficients(logistic))
exp(coefficients(logistic))



# ---------------
# Univariates
# ---------------

data_fr$quality_score2 <- cut(data_fr$quality_score, c(0,3,6,9,10))
data_fr$avg_rate_per_night_euro2 <- cut(data_fr$avg_rate_per_night_euro, c(0,50,100,150,200,max(data_fr_mod$avg_rate_per_night_euro)))
levels(data_fr$avg_rate_per_night_euro2)
revalue(data_fr$avg_rate_per_night_euro2, c("(200,368]"="(200+)"))

data_fr$accomodation_type2 <-
  with(data_fr,
       ifelse(
         accomodation_type=="Villa", 1,
         ifelse(
           accomodation_type=="Resort", 2,
           ifelse(
             accomodation_type=="Hotel", 3,
             ifelse(
               accomodation_type=="Bed and Breakfast", 4,
               5)))))


# accomodation type
data_fr %>%
  group_by(accomodation_type) %>%
  summarise(
    closed = sum(!is.na(closed_date)),
    n = sum(!is.na(hotel_id))) %>%
  mutate(closed_rate = closed/n)

# avg_rate_per_night_euro2
data_fr %>%
  group_by(avg_rate_per_night_euro2) %>%
  summarise(
    closed = sum(!is.na(closed_date)),
    n = sum(!is.na(hotel_id))) %>%
  mutate(closed_rate = closed/n)

# quality score
data_fr %>%
  group_by(quality_score) %>%
  summarise(
    closed = sum(!is.na(closed_date)),
    n = sum(!is.na(hotel_id))) %>%
  mutate(closed_rate = closed/n)