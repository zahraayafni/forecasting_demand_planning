# Forecasting Sales on Each Product Segmentation for Demand Planning - Smooth
By Zahrah Ayu Afifah Febriani

Greetings! In this session, We will try to use the [online retail data](https://www.kaggle.com/mashlyn/online-retail-ii-uci) from Kaggle to forecast sales for the next 3 month on each product segmentation in UK. In dealing with demand planning, we need to make our forecast as precise as possible. Thus, doing forecasting on a smaller scope may save us from extreme biases. We can apply different forecasting method by considering the pattern of each product segments. This task will be done in R. 

This gonna be a very long article, I will provide you shortcuts that will take you to a certain topic.

1. [Data Cleaning](#load-and-clean-the-data)
1. [Product Segmentation](#product-segmentation)
- [Find Active Days](#find-active-days)  
- [Find ADI and CV^2](#find-adi-and-cv^2)
- [Product Classifications](#product-classifications)
3. [Forecast Sales - Smooth Product](#forecast-sales---smooth-product)
- [Hierarchical Forecast](#hierarchical-forecast)
- [Forecast Sales on Smooth Products](#forecast-sales-on-smooth-products)
4. [Conclusion](#conclusion)

Well, let's get started!

## Load and clean the data
First of all, we need to load, clean, and manipulate the data. We will import some library that will help us to finish the task.

```R
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(forecast)
```
**dplyr** will help us manipulate the data. **tidyr** will clean the data. We will use **lubridate** to work with date data type. **readr** to read data. **forecast** will be useful to forecast the data, and **ggplot** will help us visualize the data. 

We use _read_csv_ function to import the data
```R
online_retail <- read_csv("../Data/online_retail_II.csv")
```
### Let's see how the data look like!
This is the view for the first 6 data.
![Data Head](/Images/head_data.jpg)

### Filtering Data
Because we want to forecast only the UK data, then we need to select the UK data. To make sure that there's only the UK data, we can use *table()*.
```R
online_retail <- online_retail %>% filter(Country == "United Kingdom")
table(online_retail$Country)
```
Here is the result, we have 981330 sales data.
![UK only](/Images/ukonly.jpg)

Then, how about the statistical summary for the data?
![Data Summary](/Images/summary1.jpg)
There are negative values on the *Quantity* and *Price*. Thus, we have to filter the data to take only the positive values.
```R
online_retail <- online_retail %>% filter(Quantity >= 0 & Price >= 0)
```
Then we will try to make sure that the data we collected contain no unknown products. Because the data description said that the *StockCode is A 5-digit integral number uniquely assigned to each distinct product*. Then, we can filter the data to get the products which have less than 5 digit *StockCode* or more than 6 digit *StockCode* (Since there are a lot of products has 6 digit *StockCode*).
![StockCode](/Images/scode.jpg)
Then, we need to remove data that its *StockCode* equals to ADJUST2, AMAZONFEE, B, BANK CHARGES, D, m, M, S, TEST001, TEST002.
```R
'%!in%' <- function(x,y)!('%in%'(x,y))
r_scode <- c("ADJUST2", "AMAZONFEE", "B", "BANK CHARGES", "D", "m", "M", "S", "TEST001", "TEST002")
online_retail <- online_retail %>% filter(StockCode %!in% r_scode)
```
After we filter data based on *StockCode*, are we ready to build our model? Not so, we will continue to the next step!

## Product Segmentation
We will segment our product into 4 classes. There are intermittent, lumpy, smooth, and erratic.
![Product Segmentation](/Images/forecastability-demand-patterns.png)
We can find the variability in demand timing by calculating the average interval of time between every two consecutive demand or _Average Demand Interval (ADI)_. Variability in demand quantity called CV^2 (CV Squared). CV squared is the square of standard deviation divided by average of the sales.

### Find Active Days
Before we find ADI and CV^2, we have to consider the holidays that may exists in our data. We need to find all active days (no holiday) in the sales of each products. First, we should extract the date from *InvoiceDate*.
```R
online_retail$date <- as.Date(online_retail$InvoiceDate)
all_dates <- online_retail %>% group_by(date) %>% distinct(date)
```
Then, find the minimum and maximum date which value is "2009-12-01" and "2011-12-09", respectively.
```R
min_date <- min(all_dates$date)
max_date <- max(all_dates$date)
```
Generate all dates that exists between the minimum and maximum date.
```R
dates_sequence <- seq.Date(from = min_date, to= max_date, by= 1)
```
The holiday date is the date in *dates_sequence* that did not exist in *all_dates*. Then, we can find it by anti joining both of them. We have 135 holiday date in *holidays*.
```R
holidays <- dates_sequence %>% anti_join(all_dates)
```
After we have the list of holiday dates, write a function to count how many holiday exists between two dates.
```R
holidays_count <- function(curr_date, prev_date) {
  curr_date <- as.Date(curr_date)
  prev_date <- as.Date(prev_date)
  
  seq_df <- data.frame(date= seq.Date(prev_date, curr_date, by = 1))
  holidays_count <- seq_df %>% inner_join(holidays, by= "date") %>% nrow()
  
  return(holidays_count)
}
```
Next, we will count all active days (no holiday) in the sales of each products. Let's start by finding the sales for all products per day.
```R
retail_grouping <- online_retail %>% group_by(date, Description) %>% summarise(total_sales= sum(Quantity, na.rm = T))
```
![Sales per Product per Day](/Images/salespp.jpg)

Then, we will lag the data to find the previous purchase date for every purchased products.
```R
lagged <- retail_grouping %>% select(date, Description) %>% group_by(Description) %>% nest() %>% mutate(prev_data = lapply(data, lag)) %>% unnest()
```
![Lagged](/Images/lagged.jpg)
Find the number of holidays between *date* and *date1*, please make sure to remove the NA values in *date1*.
```R
lagged <- lagged %>% filter(is.na(date1) == F)

for (i in 1:nrow(lagged)) {
  lagged$holidays[i] <- holidays_count(lagged$date[i], lagged$date1[i])
  print(i) # to see the running progress
}
```
Then to count the number of active days, let's find the duration between *date* and *date1*. The number of active days is the result of subtraction between duration and holidays.
```R
lagged$duration <- lagged$date - lagged$date1
lagged$duration <- as.numeric(lagged$duration)
lagged$active_days <- lagged$duration - lagged$holidays
```
![Lagged Active Days](/Images/lagged_active_days.jpg) 

### Find ADI and CV^2
Well, the data is ready for product segmentation! Now, let's find the ADI and CV^2 as the definition in the beginning of this part.
```R
product_segmentation <- lagged %>% group_by(Description) %>% summarise(ADI = mean(active_days, na.rm = T))

cv_sq <- retail_grouping %>% group_by(Description) %>% summarise(average = mean(total_sales, na.rm = T), sd = sd(total_sales, na.rm = T)) %>% mutate(cv_squared = (sd/average)^2)
```
Then integrate data in *product_segmentation* and *cv_sq*
```R
product_segmentation <- product_segmentation %>% left_join(cv_sq)
```
### Product Classifications
At the end, we need to classify the products based on the rule for each class as ilustrated in picture below.
![Product Segmentation](/Images/prod_seg.png) 
```R
product_segmentation <- product_segmentation %>% mutate(classification= case_when(ADI < 1.32 & cv_squared < 0.49 ~ "smooth", ADI > 1.32 & cv_squared > 0.49 ~ "lumpy", ADI > 1.32 & cv_squared < 0.49 ~ "intermittent", ADI < 1.32 & cv_squared > 0.49 ~ "erratic"))
```
Finally, let's visualize the product segmentation to get the better insights. As the respond to skewness towards large values on *ADI* and *cv_squared*, then we will use logarithmic scales on our chart. 
```R
product_segmentation %>% ggplot(aes(x= log(ADI), y= log(cv_squared), color= classification)) + geom_point() + ggtitle("Product Segmentation for Demand Planning - UK") + theme_minimal()
```
![UK Product Segmentation](/Images/product_segment_uk.png) 

To get the number of product on each class, we do
```R
table(product_segmentation$classification)
prop.table(table(product_segmentation$classification))
```
![Product Segmentation Result](/Images/seg_result.jpg) 
Here we know that there are 311 products categorized as erratic products, 674 products categorized as intermittent products, 4128 products categorized as lumpy products, and 11 products categorized as smooth product.

## Forecast Sales - Smooth Product
Now, we're on the forecasting step. In this step, we have to build a forecasting model with the best forecasting method to be applied in each product segments. In this session, I will give the example for the smooth product. 

Firstly, let's select all smooth products in our data.
```R
smooth_products <- product_segmentation %>% filter(classification == "smooth")
sales_on_smooth <- online_retail %>% right_join(smooth_products)
```

### Hierarchical Forecast
If we want to forecast sales per products or SKUs, then we have to construct the hierarchical structure two levels (level 0 = total, level 1= smooth products). We need to make all *StockCode* has the same number of digit. Let's change it to 6 digit of stock code.
```R
sales_on_smooth <- sales_on_smooth %>% mutate(stockcode6 = case_when(nchar(StockCode, allowNA = F, type = "chars") == 5 ~ paste(StockCode, "-", sep = ""), nchar(StockCode, allowNA = F, type = "chars") == 6 ~ StockCode))
```
Then, we can analyze the best time-frame that we should use to forecast the data. Here are the results for weekly, monthly, and yearly sales for each smooth products.
- Weekly Sales
![Weekly Sales on Smooth](/Images/pivoted_weekly_smooth.jpg)
- Monthly Sales
![Monthly Sales on Smooth](/Images/pivoted_monthly_smooth.jpg) 
- Yearly Sales
![Yearly Sales on Smooth](/Images/pivoted_yearly_smooth.jpg)

We can clearly see from the pictures above that we have a lot of zeros in our data. We will try to forecast our sales in weekly. then, look for the best method that we have. We can use library *hts* to accomplish this. 
```R
library(hts)
hirarchical_weekly_smooth <- hts(ts_weekly)
```
![Hierarchical Weekly Smooth](/Images/hirarchical_weekly_smooth.jpg)

Since we have only 2 levels in our hierarchical model, we can try both top-down and bottom-up forecast distribution method. The forecasting method that we'll try is Exponential Smoothing, Random Walk, and ARIMA. Here are the result of performance measure for all these methods.
- Bottom-Up Approach

|  | Exponential Smoothing  | Random Walk  | Arima  |
| :---:   | :-: | :-: | :-: |
| RMSE | 59.303408 | 55.412093 | 50.421949 |
| MAE | 51.609212 | 49.500000 | 46.626606 |
| MAPE | 628.364308 | 563.733058 | 402.104774 |

- Top-Down Approach

|  | Exponential Smoothing  | Random Walk  | Arima  |
| :---:   | :-: | :-: | :-: |
| RMSE | 55.413188 | 55.412093 | 50.731779 |
| MAE | 49.500601 | 49.500000 | 46.978488 |
| MAPE | 563.754452 | 563.733058 | 360.314137 |

As we see in the above, ARIMA shows the best performance for our data. 

### Forecast Sales on Smooth Products
We will do an experiment to forecast sales on smooth product weekly. Let's begin with creating time-series data for smooth product sales.
```R
weekly_smooth <- sales_on_smooth %>% group_by(week, year) %>% summarise(sales = sum(Quantity, na.rm = T)) %>% arrange(year, week)
ts_weekly_smooth <- ts(weekly_smooth[,3], start = c(2009, 48), frequency = 52)
autoplot(ts_weekly_smooth) + ggtitle("Weekly Sales for Smooth Product") + theme_minimal()
```
![Weekly Sales Smooth Product](/Images/weekly_sales_smooth.png)
Then, let's look into the data seasonal plot
![Weekly Seasonal Plot](/Images/weekly_seasonal_plot.png)
We can move deeper into time-series data components by decompose our data, then we get
![Decomposition Plot](/Images/weekly_decomposition.png)
These are the auto-correlation plot for ACF and PACF
![ACF Plot](/Images/acf_plots.png)
![PACF Plot](/Images/pacf_plots.png)
From all the above diagrams, we can assume that the data has trend and seasonality. We will try to forecast sales of our data with **SARIMA** method. To get the best model, we'll do grid search for SARIMA parameters (p,d,q,P,D,Q).
```R
p = d = q = P = D = Q = c(0, 1, 2)
grid <- expand.grid(p, d, q, P, D, Q)
grid$S <- 52
``` 
The above code will generate all possible parameters for SARIMA which values limited to 0, 1, and 2. Then, we can apply our possible parameters in *grid*. Don't forget to save the parameters and AIC values on a list. The training data will be end at week 29 in 2011.
```R
result_list <- list()
train <- window(ts_weekly_smooth, end= c(2011, 29))

for (i in 1:nrow(grid)) { 
  
  tryCatch({
    model <- arima(train, order = c(grid[i, 1], grid[i, 2], grid[i, 3]), seasonal= list(order= c(grid[i, 4], grid[i, 5], grid[i, 6]), S= grid[i, 7] ))
    result_list[[i]] <- data.frame(grid[i,], model$aic)
    print(i)
  }, error= function(e){})
  
}
```
Now, we have a list of parameters and AIC value from its model. The Akaike Information Critera (AIC) is one of the goodness-fit-criteria. The lower value of AIC, the better model we have. Here are our *result_list* values.
![result_list](/Images/results_grid.jpg)
This is our best SARIMA parameters and the SARIMA model.
![Best SARIMA parameters](/Images/best_parameter.jpg)
![Our SARIMA model](/Images/sarima_model.jpg)
Then, we can plot our actual time-series data and the prediction based on our SARIMA model. We predict for the next 10 weeks.
```R
library(TSPred)

plotarimapred(ts_weekly_smooth, best_arima, xlim=c(min(time(ts_weekly_smooth)), 2012.115))
```
![Forecast Plot](/Images/smooth_fcast.png)
Here are numerical result for our forecast
![Smooth Product Forecast](/Images/smooth_fcast_num.jpg)
## Conclusion
Finally, we are at the end of our session. We may conclude that for our smooth product category, demand follows the seasonal pattern. For the demand planning, we have to make sure that our products available in high number at the early weeks of 2012. 

Thanks a lot for reading this. I know that this experiment still far from perfect. I'd love to hear any suggestions or comments from all of you. Feel free to reach me on my [Linked In](https://www.linkedin.com/in/zahrah-ayu-afifah-febriani/). Good Day Everyone! 
