## Factors to Look Out for that Rack up the Price of AirBnb Listings in NYC

```{r}
library(tidyverse)
library(data.table)
library(leaflet)

AB_NYC_2019 <- read_csv("~/Desktop/Math 488P/Project/AB_NYC_2019.csv")
AB_NYC_2019 <- rename(AB_NYC_2019, borough = neighbourhood_group)
```


New York City, the most popular city in the United States, is a breeding ground for tourism. Whether you come to New York for the food, the musical and artistic talent, or the nightlife, choosing where you are going to stay is a very important part of the NYC experience! AirBnb, a popular lodging mobile app and website, has been altering the way that we think about where we choose to stay in a new place. The app has become increasingly popular since it was founded in 2008, and the hotel industry has been taking a huge hit ever since. The interesting thing about AirBnb is that, as long as you have a space for guests, anyone can be a host. Therefore, the app is extremely common in big cities like NY, where anybody can rent out their apartment or room for a ton of cash.

Using a sample of almost 50,000 AirBnb listings across all 5 boroughs in NYC in 2019, we gathered some useful information on the factors that AirBnb hosts consider when determining the price of their listings. This could help you in choosing the most affordable option that also satisfies your needs when traveling to NYC.   

As college students who love to travel, price is a huge consideration for us when choosing where to stay in a new city, and we are sure that many of you feel the same way. There is definitely a correlation between price and the location of each AirBnb listing in New York. As you can imagine, the more expensive the AirBnb is, the “better” the neighborhood that is located in. And the other way around as well; the cheaper the AirBnb, the “worse” the location it is in.

```{r}
AB_NYC_2019 %>%
  ggplot() + geom_boxplot(aes(x = price, y = borough, color = room_type)) + 
  coord_cartesian(xlim =c(0, 500)) + labs(x = "Price" , y = "Borough" , title = "Price of Listings by Borough and Room Type in 2019")
```


This is a boxplot depicting the prices of the AirBnb listings in each borough, separated by room type: shared room, private room, or entire home/apartment. For each boxplot, the left edge of the box represents the 25th percentile of price, the middle line of the box represents the mean price, and the right edge represents the 75th percentile of price. All of the dots represent "outliers," because those prices are far from the mean, and there are not too many of them.

Brooklyn and Manhattan are both the most populated boroughs with AirBnb rentals, and therefore they also contain the highest mean price of listings. Out of just over 25,000 full home/apartment listings, over 13,000 belong to Manhattan, and over 9,500 belong to Brooklyn. Staten Island and the Bronx are low on this list, with 176 and 379, respectively. We can see from the graph that even as number of listings increase, price also increases. Therefore, the demand in Brooklyn and Manhattan is much higher than the demand in other boroughs. 

This pattern continues for the other two room types: private room and shared room. It is unsurprising that the average price for a private room and a shared room decreases tremendously from the entire home, as for most of the boroughs, the 75th percentile of the price of shared homes are less than the 25th percentile of the price of private homes. In addition, the average across all boroughs of the shared room type had an average price of $70.13, while the private room had an average price of $89.78, and the entire home/apartment had an average price over double that: $211.79. This is likely due to the lack of privacy that a shared home brings, since facilities are often shared, and guests will often have to interact with their hosts as they enter and leave the home. For New York City travelers, this is much less likely to be an issue than for travelers to an area where there isn't as much to do. NYC travelers are often out and about all day anyway, so if you don't mind seeing someone else, or possibly waiting to use the restroom, it will save you a ton of money to share a home with your host, or whoever else is occupying the space. 


Personally, we have never heard of "shared rooms" before we conducted this analysis, so we aren't entirely sure what that entails. It does sound like a bit too much privacy is extracted with that one, so since the price difference is not too severe, we would recommend the private room in a home as your best bet. 


Are you ever looking for the perfect place to stay and one of the listings catches your eye with just one word? Well, whether you realize it or not, the language used to describe AirBnb listings could possibly have more of an effect than you know. We compared the average and median prices of AirBnb listings in Manhattan that contained the word "cozy," and listings that contained the word "luxury," and compared both of those to the average and median prices of all of the listings in Manhattan.

	
```{r}
cozy_rooms <- AB_NYC_2019 %>% 
  filter(borough == "Manhattan" , grepl( "Cozy | cozy" , name)) 

totals <- AB_NYC_2019 %>% 
  filter(borough == "Manhattan")

luxury_rooms <- AB_NYC_2019 %>% 
  filter(borough == "Manhattan" , grepl( "Luxury | luxury" , name))

c<- cozy_rooms %>%
  summarize(
    count = n(),
    Word = "Cozy",
    mean(price),
    median(price)
  )

l<- luxury_rooms%>%
  summarize(
    count = n(),
    Word = "Luxury",
    mean(price),
    median(price)
  )

t <-totals%>%
  summarize(
    count = n(),
    Word = "Total",
    mean(price),
    median(price)
  )

alist <- list(c,l,t)
rbindlist(alist)
```
	
	
The table above shows that words such as “cozy” and “luxury” are associated with different prices of AirBnb listings. The word "cozy" seems to be associated with less expensive listings, while the word "luxury" seems to be associated with more expensive listings. This could have to do with the connotation that these words have in our minds: "cozy" usually seems to resemble a smaller, lived in, maybe even slightly cluttered apartment, while luxury seems to describe a spacious, modern place. However, you cannot directly prove that a place is "cozy" or "luxury"! Overall, we are not saying that these words influence the price, but the price may influence the words that the host uses to describe their home. Keep this interesting observation in mind the next time that you are browsing through listings. 


So, the question now becomes, what makes up the most expensive AirBnb listings? Well, we narrowed down the top 40 most expensive listings into a graph, so that we can see if specific location makes a difference in price. The blue points represent full homes/apartments, while grey points represent a private room in an apartment. 

```{r}

top_listings <- AB_NYC_2019 %>% filter(minimum_nights <5) %>% top_n(n = 40, wt = price) %>% arrange(desc(price))

pal <- colorFactor(c("navy", "grey"),
                   domain = c("Entire home/apt", "Private Room"))
                   

leaflet(top_listings) %>%
  addTiles("Most Expensive NYC AirBnb Listings") %>%
  setView(-74.00, 40.71, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = 10,
    color = ~pal(room_type),
    stroke = TRUE
)

```




We can conclude that Manhattan and Brooklyn have the most traffic through AirBnbs, and the most expensive ones at that. You can also see that the majority of these top 40 most expensive AirBnbs are for the entire apartment. But, what is interesting is there are a few that are only private rooms. Out of the 40,  6 of those AirBnbs are only renting out a private room, those must be some nice private rooms! 



Overall, we can conclude that there are many things to consider when choosing a NYC AirBnb to make sure that you are getting the best deal for your money. Price is influenced by room type and borough, and probably specific neighborhoods in each borough. Therefore, in order to stay within budget, most people will have to make trade offs when choosing where to stay in New York. This could look like staying in Queens to avoid the prices of Manhattan, or choosing a private room in a shared home to avoid the privacy expenses. Whatever you choose to do, make sure to decide what is most important to you, and try to stay within reason.
