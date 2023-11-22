Wholesale_store
================
Nathasya Pramudita
2023-11-22

``` r
library(tidyverse) # cleaning, analyze, and visualizing the data
library(extrafont) # add fonts
library(patchwork) # combine multiple graph into one
```

# Introduction

This is the dataset about online store called **Wholesale and Orders**,
the dataset is created by \[JMP Case Study Library\]
(<https://www.jmp.com/en_gb/academic/case-study-library.html#boston>).
The main purpose of this project is to understand the dataset and find
pattern in the dataset, to draw some conclusion so the stakeholder can
draw conclusion based on data finding.

Important point for this dataset:

- The range of this dataset is between 2017-January to 2021-Descember.
- This dataset is part of JMP Case Study Library.
- According to Kaggle, the usability of this dataset is 10/10, so
  there’s not much cleaning that I do, except changing the data type and
  fix the variables title.

## Find the objection

There’s some question that need to answer as we’re analyzing the data
frames, which is:

1.  Visualizing history of sold product through all the records of data.
2.  Finding average arrival day based on `customer status` membership.
3.  Finding is there any particular day that customers often pick to
    shopping.
4.  Find the most profitable product that our supplier sold (and who’s
    supplier that sold the most profitable product to our store).
5.  What’s average spending of all our customers based on
    `customer status`.
6.  created visualization of each profit based on `product category`
    (arrange it from the higher to smaller).


## Import and Wrangling the data

``` r
# import the dataset
orders <- read_csv("~/Datasets/Wholesale_and_order/orders.csv") %>% 
  rename_all(tolower) %>% 
  rename(`total price` = `total retail price for this order`,
         `retail price` = `cost price per unit`,
         `date order` = `date order was placed`,
         `date arrive` = `delivery date`) %>% 
  mutate(`date order` = dmy(`date order`),
         `date arrive` = dmy(`date arrive`),
         `customer status` = str_to_title(`customer status`),
         `customer status` = factor(`customer status`, levels = c("Silver", "Gold", "Platinum")))
```

    ## Rows: 185013 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Customer Status, Date Order was placed, Delivery Date
    ## dbl (6): Customer ID, Order ID, Product ID, Quantity Ordered, Total Retail P...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# join two dataset
order_product <-  read_csv("~/Datasets/Wholesale_and_order/product-supplier.csv") %>% 
  rename_all(tolower) %>% 
  inner_join(orders, by = "product id") %>% 
  select(-contains("id")) %>% 
  mutate(`price per unit` = `total price`/ `quantity ordered`,
         `profit` = `price per unit` - `retail price`) %>% 
  relocate(`price per unit`, `total price`, `retail price`, profit, .after = last_col())
```

    ## Rows: 5504 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (6): Product Line, Product Category, Product Group, Product Name, Suppli...
    ## dbl (2): Product ID, Supplier ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Analyzing and visualized the dataframe

### To answer some objection

``` r
# line chart of average shopping every years (2017 - 2021)
order_product %>%
  mutate(year_month = make_date(year = year(`date order`), month = month(`date order`))) %>% 
  group_by(year_month) %>%
  summarize(mean = mean(`total price`)) %>% 
  ggplot(aes(year_month, mean)) +
  geom_line(linewidth = .5, color = "brown") +
  geom_point(size = 2, color = "#593332") +
  theme_classic() +
  theme(text = element_text(family = "Constantia")) +
  labs(x = "",
       y = "Average Shopping",
       title = "Average of Customers Shopping",
       subtitle = "From 2017 - 2021")
```

![](wholesale_store_files/figure-gfm/average%20shopping%20history-1.png)<!-- -->

The line looks fluctuation through each years. While in the `August` the
product sold the lowest, but every end of the month (between `November`
to `December`) the sold record are highest. And they slump down again in
the mid or second quarter of the years.

``` r
# average days the goods arrived to the customers
order_product %>% 
  select(`customer status`, `date order`, `date arrive`) %>% 
  mutate(avg_order_arrive = as.numeric(difftime(`date arrive`, `date order`, units = "days"))) %>% 
  group_by(`customer status`) %>% 
  summarize(avg_order_arrive = mean(avg_order_arrive)) # group_by `customer status`
```

    ## # A tibble: 3 × 2
    ##   `customer status` avg_order_arrive
    ##   <fct>                        <dbl>
    ## 1 Silver                        1.06
    ## 2 Gold                          1.05
    ## 3 Platinum                      1.08

Seems there’s no big differences between `Platinum`, `Gold`, and
`Silver` membership.

``` r
# see what day customers orders the most
order_product %>% 
  mutate(wday = wday(`date order`, label = T)) %>% 
  ggplot(aes(wday, fill = wday)) +
  geom_bar(linewidth = .7, show.legend = F) +
  theme_dark() + 
  theme(text = element_text(family = "Constantia")) +
  scale_fill_manual(values = c("#132043", "#1f4172", "#f1b4bb", "#f5f5f5", "#f99417", "#4d4c7d", "#363062")) +
  labs(x = "",
       y = "Customers Orders",
       title = "Favorite Day to Shopping")
```

![](wholesale_store_files/figure-gfm/fav%20day%20for%20shopping-1.png)<!-- -->

We find some interesting finding here. There’s increase of frequent from
`Sunday` to `Tuesday`, but fall down in `Wednesday`, and stagnan between
`Thursday` to `Saturday`.

``` r
# find the most profitable suppliers
order_product %>% 
  group_by(`supplier name`) %>% 
  summarize(sum = sum(profit)) %>% 
  mutate(sum = sum/1000) %>% 
  arrange(desc(sum)) %>% 
  top_n(7) %>% 
  ggplot(aes(x = reorder(`supplier name`, sum), y = sum, fill = `supplier name`)) +
  geom_col(linewidth = 4, show.legend = F) +
  coord_flip() +
  theme_linedraw() +
  theme(text = element_text(family = "Constantia")) +
  scale_fill_brewer(palette = "BuPu") +
  labs(x = "",
       y = "Total Profit in Thousand",
       title = "Supplier With the Highest Profitable")
```

    ## Selecting by sum

![](wholesale_store_files/figure-gfm/find%20the%20most%20profitable%20suppliers-1.png)<!-- -->

``` r
# what `Eclipe inc` sold
order_product %>% 
  filter(`supplier name` %in% c("Eclipse Inc")) %>% 
  select(`product category`) %>% 
  count(`product category`) %>% 
  arrange(desc(n))
```

    ## # A tibble: 4 × 2
    ##   `product category`     n
    ##   <chr>              <int>
    ## 1 Shoes              13058
    ## 2 Clothes            12174
    ## 3 Children Sports     3524
    ## 4 Winter Sports        120

``` r
# the most profitable product
most_profitable <- order_product %>% 
  group_by(`product category`) %>% 
  summarize(sum = sum(profit)) %>% 
  mutate(sum = sum/1000) %>% 
  arrange(desc(sum)) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(`product category`, sum), sum, fill = `product category`)) +
  geom_col(show.legend = F) +
  coord_flip() +
  theme_linedraw() +
  theme(text = element_text(family = "Constantia")) +
  labs(x = "",
       y = "Total Price in Million",
       title = "The Most Profitable Product",
       subtitle = "Based on Product Categories")
```

    ## Selecting by sum

``` r
most_sold <- order_product %>% 
  group_by(`product category`) %>% 
  summarize(sum = sum(`total price`)) %>% 
  mutate(sum = sum/1000) %>% 
  arrange(desc(sum)) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(`product category`, sum), sum, fill = `product category`)) +
  geom_col(show.legend = F) +
  coord_flip()+
  theme_linedraw() +
  theme(text = element_text(family = "Constantia")) +
  labs(x = "",
       y = "Total Price in Million",
       title = "Product That Sold The Most")
```

    ## Selecting by sum

``` r
most_profitable / most_sold
```

![](wholesale_store_files/figure-gfm/combine%20two%20graph%20into%20one-1.png)<!-- -->

So far there’s not much different from the arrange of the most
profitable and sold product except `Clothes` and `Shoes` position, while
in `The Most Profitable Product` Clothes in number three, in
`Product That Sold The Most` are in position four. (and so does the
`Running - Jogging` and `Racket Sports`, they’re in position eight and
nine).

``` r
# average total shopping based on `customer status`
order_product %>%
  group_by(`customer status`) %>% 
  summarize(mean = mean(`total price`)) %>% 
  ggplot(aes(reorder(`customer status`, mean), mean, fill = `customer status`)) +
  geom_col(width = .7,show.legend = F) +
  theme_linedraw() +
  theme(text = element_text(family = "Constantia")) +
  scale_fill_manual(breaks = c("Silver", "Gold", "Platinum"),
                    values = c("#f9f298", "#E5E4E2", "#EAFAFD")) +
  labs(x = "",
       y = "Average Spending",
       title = "Average Total Spending Based on Customers Status Members")
```

![](wholesale_store_files/figure-gfm/average%20total%20shopping%20based%20on%20customer%20status-1.png)<!-- -->

The difference didn’t see clearly. Let’s put it in the table instead.

``` r
order_product %>%
  group_by(`customer status`) %>% 
  summarize(mean = mean(`total price`))
```

    ## # A tibble: 3 × 2
    ##   `customer status`  mean
    ##   <fct>             <dbl>
    ## 1 Silver             139.
    ## 2 Gold               138.
    ## 3 Platinum           139.

Now we see there’s not much different of average spending between three
group of `customer status` membership. This means customer with
`Platinum` status would spend the same ammont of money as customer with
`Silver` status.

# Conclusion

1.  There’s no significant breakthrough through every years (between
    2017 to 2021) of the sales records.
2.  `Wednesday` is the least day that customer pick to shopping, while
    through `Sunday` to `Wednesday` are the higher (and they’re stagnant
    in `Thursday` to `Saturday`)
3.  `Eclipse Inc` supplier sold their product the cheapest, thus makes
    us have the higher profitable wholesaler supplier. While the product
    that they sold are; Shoes, clothes, children sport, and winter
    sport.
4.  THe different between product that sold the most profit and product
    that customers brought the most are not that conspicious. While we
    still get 2x of profit each time the product sold, but we need to
    push `Clothes` product more, cause they bring the most profitable
    than `Shoes`.
5.  There’s no different significant of average spending between three
    groups of `customer status` member. Company need to give customers
    some benefit why they need to upgrade their membership, and give
    some special offers for `Platinum` membership.

===
