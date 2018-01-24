#assignment 2
library(dplyr)
library(ggplot2)
library(tidyr)
raw_listings <- read.csv('data/listings.csv')

#please note: they don't actually go to plot unless you run them directly in the console

#' Metrics:
#' *mean price per neighbourhood
#' *mean price per bed (price / beds) [NOT bedrooms, just beds] per neighbourhood
#' *mean ratings (total) per neighbourhood
#' *distribution of types of housing (full, partial) per neighbourhood
#' *distribution of bed types (real, sofa) per neighbourhood

#copied from the session 2 script
clean_price <- function(price) as.numeric(gsub('\\$|,', '', price))
raw_listings %>%
  mutate(nprice = clean_price(price)) %>%
  select(name, price, nprice)
raw_listings %>% filter(!is.na(bedrooms))

listings <- raw_listings %>%
  filter(!is.na(bedrooms), !is.na(bathrooms)) %>%
  mutate(price = clean_price(price),
         weekly_price = clean_price(weekly_price),
         monthly_price = clean_price(monthly_price))

#raw_listings has outlived its usefulness and can go away
rm(raw_listings)

#clean up things to only what we need
listings <- listings %>% select(neighbourhood_cleansed, price, beds, bed_type, room_type, review_scores_rating)

#filter out NA things and zero beds (why the hell would you rent a room without beds)
listings <- listings %>%
  filter(!is.na(neighbourhood_cleansed), !is.na(price), !is.na(beds), !is.na(bed_type), !is.na(room_type), !is.na(review_scores_rating)) %>%
  filter(!beds == 0)

#part 1: mean price per neighbourhood
listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_price = mean(price)) %>% 
  ggplot(aes(x=neighbourhood_cleansed, y=avg_price, fill=avg_price)) +
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(x='Neighbourhood', y='Mean price')

#part 2: mean price per bed, per neighbourhood
listings_ppb <- data.frame(listings$price/listings$beds)
listings$price_per_bed <- listings_ppb$listings.price.listings.beds
rm(listings_ppb)
#run here
listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_ppb = mean(price_per_bed)) %>% 
  ggplot(aes(x=neighbourhood_cleansed, y=avg_ppb, fill=avg_ppb)) +
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(x='Neighbourhood', y='Mean price per bed')

#part 3: mean ratings per neighbourhood
listings %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(avg_rating = mean(review_scores_rating)) %>% 
  ggplot(aes(x=neighbourhood_cleansed, y=avg_rating, fill=avg_rating)) +
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=60, hjust=1)) +
  labs(x='Neighbourhood', y='Mean rating')

#part 4: distribution of types of housing (full, partial) per neighbourhood
listings %>%
  group_by(neighbourhood_cleansed) %>% 
  ggplot(aes(x=neighbourhood_cleansed, y=room_type, fill=room_type)) +
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=60, hjust=1),
                                    axis.text.y=element_blank(),
                                    axis.ticks.y=element_blank()) +
  labs(x='Neighbourhood', y='Frequency')

#part 5: distribution of bed types (real, sofa) per neighbourhood
listings %>%
  group_by(neighbourhood_cleansed) %>% 
  ggplot(aes(x=neighbourhood_cleansed, y=bed_type, fill=bed_type)) +
  geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=60, hjust=1),
                                    axis.text.y=element_blank(),
                                    axis.ticks.y=element_blank()) +
  labs(x='Neighbourhood', y='Frequency')
