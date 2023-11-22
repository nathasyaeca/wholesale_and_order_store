# load the packages
library(tidyverse)
library(extrafont) # add new fonts

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
# join two dataset
order_product <-  read_csv("~/Datasets/Wholesale_and_order/product-supplier.csv") %>% 
  rename_all(tolower) %>% 
  inner_join(orders, by = "product id") %>% 
  select(-contains("id")) %>% 
  mutate(`price per unit` = `total price`/ `quantity ordered`,
         `profit` = `price per unit` - `retail price`) %>% 
  relocate(`price per unit`, `total price`, `retail price`, profit, .after = last_col())

# analyzing it
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
# what `Eclipe inc` sold
order_product %>% 
  filter(`supplier name` %in% c("Eclipse Inc")) %>% 
  select(`product category`) %>% 
  count(`product category`) %>% 
  arrange(desc(n))
# the most profitable product
order_product %>% 
  group_by(`product category`) %>% 
  summarize(sum = sum(profit)) %>% 
  mutate(sum = sum/1000) %>% 
  arrange(desc(sum)) %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(`product category`, sum), sum, fill = `product category`)) +
  geom_col(width = .5, show.legend = F) +
  coord_flip() +
  theme_linedraw() +
  theme(text = element_text(family = "Constantia")) +
  labs(x = "",
       y = "Total Price in Thousand",
       title = "The Most Profitable Product",
       subtitle = "Based on Product Categories") # color palette not yet decided
# versus the most pick product by customers
order_product %>% 
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
       y = "Total Price in Thousand",
       title = "Product That Get Pick The Most by Customers") # color palette not yet decided
# average days the goods arrived to the customers
order_product %>% 
  select(`customer status`, `date order`, `date arrive`) %>% 
  mutate(avg_order_arrive = as.numeric(difftime(`date arrive`, `date order`, units = "days"))) %>% 
  group_by(`customer status`) %>% 
  summarize(avg_order_arrive = mean(avg_order_arrive)) # group_by `customer status`
# see what day customers orders the most
order_product %>% 
  mutate(wday = wday(`date order`, label = T)) %>% 
  ggplot(aes(wday, fill = wday)) +
  geom_bar(linewidth = .7, show.legend = F) +
  theme_linedraw() + 
  theme(text = element_text(family = "Constantia")) +
  scale_fill_manual(values = myColor) +
  labs(title = "Most Frequent Day of Customers Orders Product",
       x = "",
       y = "Customers Orders")
# average total shopping based on `customer status`
order_product %>%
  group_by(`customer status`) %>% 
  summarize(mean = mean(`total price`)) %>% 
  ggplot(aes(reorder(`customer status`, mean), mean, fill = `customer status`)) +
  geom_col(width = .7,show.legend = F) +
  theme_dark() +
  theme(text = element_text(family = "Constantia")) +
  scale_fill_manual(breaks = c("Silver", "Gold", "Platinum"),
                    values = c("#f9f298", "#E5E4E2", "#EAFAFD")) +
  labs(x = "",
       y = "Average Spending",
       title = "Average Total Spending Based on Customers Status Members")
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
