Data Sources and Preparation
================

- <a href="#summary" id="toc-summary">SUMMARY</a>
  - <a href="#i-daily-data" id="toc-i-daily-data">I. Daily Data</a>
  - <a href="#ii-hourly-data" id="toc-ii-hourly-data">II. Hourly Data</a>
  - <a href="#iii-monthly-data" id="toc-iii-monthly-data">III. Monthly
    Data</a>
- <a href="#1-load-prepared-daily-data"
  id="toc-1-load-prepared-daily-data">1. Load prepared daily data</a>
- <a href="#2-load-and-prepare-half-hourly-n3rgy-data"
  id="toc-2-load-and-prepare-half-hourly-n3rgy-data">2. Load and prepare
  half hourly (n3rgy) data</a>
  - <a href="#checks" id="toc-checks">Checks</a>
    - <a href="#deal-with-missing-data" id="toc-deal-with-missing-data">Deal
      with missing data</a>
    - <a href="#look-for-discrepancies-with-supplier-data"
      id="toc-look-for-discrepancies-with-supplier-data">Look for
      discrepancies with supplier data</a>
  - <a href="#aggregate-by-hour-in-local-time"
    id="toc-aggregate-by-hour-in-local-time">Aggregate by hour in local
    time</a>
- <a href="#3-load-and-prepare-solar-yield-estimate-data"
  id="toc-3-load-and-prepare-solar-yield-estimate-data">3. Load and
  prepare solar yield estimate data</a>

# SUMMARY

## I. Daily Data

#### i) [EDF](https://my.edfenergy.com/user/login)

Monthly csv downloads of daily household gas and electricity consumption
data were aggregated into a spreadsheet and exported to a csv file. The
data covers the period 15/03/2021 - 30/6/2023. Pivot tables were used to
cross check monthly totals with the EDF energy hub to highlight any copy
paste errors.

Changelog:  
31/03/2023 gas usage updated 29/5/23 from previous estimate of 30 to
27.63 Kwh  
04/06/2023 2023 April and May figures added  
10/07/2023 June figures added

#### ii) Solis app

Solar PV was installed 20/9/2022. Daily yield data was manually
transcribed from the Solis app to a spreadsheet. Monthly totals were
checked against the app to reveal transcription errors. This highlighted
some discrepancies between the monthly and aggregated daily totals
within the app. The assumption was made that the monthly total was
affected less by wi-fi connectivity issues and therefore recorded more
accurately. In consequence some “zero yield” days were filled with
estimates to match the monthly total. It was also noticed that the most
recent days of yield data were later updated to slightly higher figures.
The reason was not clear. Possibly some type of calibration or
reconciliation with meter readings. The yield data was combined with
degree days and supplier consumption data into one dataset of daily
records.

Changelog:  
Updates from “zero yield” days to bring monthly total in line with
app:-  
22/11/22 yield estimate = 3  
15/2/23 yield estimate = 5  
16/2/23 yield estimate = 5  
17/2/23 yield estimate = 5  
18/2/23 yield estimate = 4.3

10/7/23 June 2023 figures added

#### iii) [Degree days](https://www.degreedays.net/)

Degree days were calculated using www.degreedays.net with a base
temperature of 15.5 degrees C. Temperature data was taken from Liverpool
airport weather station. Data was downloaded in csv format and added
into the spreadsheet of daily data.

Changelog:  
10/7/23 June 2023 figures added

## II. Hourly Data

#### [N3rgy](https://data.n3rgy.com/consumer/home)

Personal half hourly energy consumption data were downloaded in
2-monthly batches. (the website had a 90 days download limit). The first
available data was October 2021. The data were aggregated into one
dataset of hourly data in local time using R (see below) and written to
a csv.

Checks were made for duplicates and missing entries, and a few gaps were
filled. Cross checks for the electric data were made against the
supplier provided daily data. This highlighted a few discrepancies when
EDF had supplied an estimate rather than actual meter read. The largest
of these was 2.2 Kwh on 20/10/2021. Rounding errors may also contribute
to smaller discrepancies; The n3rgy data is provided to 3dp whereas the
EDF daily data is 2dp.  
Monthly totals were all within 1% except for March 2023 (electric, n3rgy
was missing 1.5 days data), and May, June 2023 for gas (out by under 2.5
kwh for each). The total difference over the 20 months of available
n3rgy data was under 20KWh for electric. This is less than half a
percent so not expected to impact this study. Samples of hourly figures
were also compared with EDF’s energy hub data and are largely consistent
with hourly discrepancies less than .01 KWh.

The half hourly data was aggregated by hour in local time.

Changelog:  
10 missing records created for:-  
“2021-11-12 07:30:00 UTC” “2022-01-10 03:00:00 UTC” “2022-03-04 06:00:00
UTC” “2022-05-06 08:00:00 UTC” “2022-07-14 08:00:00 UTC” “2022-09-30
09:30:00 UTC” “2022-12-01 05:30:00 UTC” “2023-01-19 19:00:00 UTC”
“2023-03-09 00:00:00 UTC” “2023-04-29 06:30:00 UTC”  
The electric consumption entry was filled by repeating the previous
record.

4/6/23 Hourly summary amended from hour ending to the centre of the
range to align with supplier data.  
11/7/23 June 2023 figures added  
13/7/23 gas data added from 10/11/22 NB measured in cubic metres so is
converted into Kwh.This may introduce some small discrepancies with
supplier data as calorific values vary.  
20/8/23 checks added to demonstrate correct aggregation by hour

## III. Monthly Data

#### [Personal solar yield estimate](https://re.jrc.ec.europa.eu/pvg_tools/en/)

Location, size of PV array, azimuth and inclination were used to
personalise solar yield estimates from the PVGIS tool from the European
Commission. (3.95 KWp, 14% system loss, slope 30, azimuth -35). PVGIS
uses satellite solar radiation data with climate reanalysis models to
produce energy yield information. Monthly data were downloaded from the
website as a double tab delimited file. This was imported and prepared
in R by selecting and renaming columns. The annual estimate of
generation is 3422 KWh with standard deviation given as 117 Kwh over the
15 years of the solar radiation database.

------------------------------------------------------------------------

``` r
library(tidyverse) # core 
library(skimr)
library(data.table)
```

# 1. Load prepared daily data

``` r
daily_data <- read_csv("Data/230630_daily.csv", col_types = cols(date = col_date(format = "%d/%m/%y")))

# quick overview
skim_without_charts(daily_data)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | daily_data |
| Number of rows                                   | 838        |
| Number of columns                                | 5          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| Date                                             | 1          |
| numeric                                          | 4          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2021-03-15 | 2023-06-30 | 2022-05-07 |      838 |

**Variable type: numeric**

| skim_variable    | n_missing | complete_rate |  mean |    sd |   p0 |  p25 |   p50 |   p75 |   p100 |
|:-----------------|----------:|--------------:|------:|------:|-----:|-----:|------:|------:|-------:|
| gas_usage_kwh    |         0 |             1 | 24.83 | 21.58 | 0.00 | 7.41 | 16.66 | 38.87 | 117.60 |
| elec_usage_kwh   |         0 |             1 |  5.83 |  2.21 | 0.71 | 4.39 |  5.81 |  7.20 |  13.45 |
| degree_days_15.5 |         0 |             1 |  4.47 |  3.94 | 0.00 | 0.90 |  3.80 |  7.40 |  18.80 |
| solar_yield_kwh  |         0 |             1 |  3.20 |  6.34 | 0.00 | 0.00 |  0.00 |  3.48 |  27.90 |

``` r
# check for missing entries
diffs <- daily_data$date - shift(daily_data$date)

daily_data[c(which(diffs>1)-1, which(diffs>1)),] %>%
  arrange(date)
```

    ## # A tibble: 0 × 5
    ## # … with 5 variables: date <date>, gas_usage_kwh <dbl>, elec_usage_kwh <dbl>,
    ## #   degree_days_15.5 <dbl>, solar_yield_kwh <dbl>

No missing dates

``` r
tail(daily_data)
```

    ## # A tibble: 6 × 5
    ##   date       gas_usage_kwh elec_usage_kwh degree_days_15.5 solar_yield_kwh
    ##   <date>             <dbl>          <dbl>            <dbl>           <dbl>
    ## 1 2023-06-25          4.8            3.85              0.1            15.1
    ## 2 2023-06-26          8.39           2.8               0.7            20.8
    ## 3 2023-06-27          5.29           3.06              0.5             4.4
    ## 4 2023-06-28          2.01           3.25              0.2             5.7
    ## 5 2023-06-29          9.85           2.91              0.5            22.9
    ## 6 2023-06-30          3.96           3.18              1.1             6.9

# 2. Load and prepare half hourly (n3rgy) data

``` r
# Half hourly electric import data in 2 month batches
dat1 <- read_csv("Data/n3rgy/20211001-20211201.csv", show_col_types = FALSE)
dat2 <- read_csv("Data/n3rgy/20211201-20220201.csv", show_col_types = FALSE)
dat3 <- read_csv("Data/n3rgy/20220201-20220401.csv", show_col_types = FALSE)
dat4 <- read_csv("Data/n3rgy/20220401-20220601.csv", show_col_types = FALSE)
dat5 <- read_csv("Data/n3rgy/20220601-20220801.csv", show_col_types = FALSE)
dat6 <- read_csv("Data/n3rgy/20220801-20221001.csv", show_col_types = FALSE)
dat7 <- read_csv("Data/n3rgy/20221001-20221201.csv", show_col_types = FALSE)
dat8 <- read_csv("Data/n3rgy/20221201-20230201.csv", show_col_types = FALSE)
dat9 <- read_csv("Data/n3rgy/20230201-20230401.csv", show_col_types = FALSE)
dat10 <- read_csv("Data/n3rgy/20230401-20230601.csv", show_col_types = FALSE)
dat11 <- read_csv("Data/n3rgy/20230601-20230701.csv", show_col_types = FALSE)

# Combine into one dataframe and sort by date
elec_halfhr <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, dat11) %>% 
  select('timestamp (UTC)' ,'energyConsumption (kWh)' ) %>% 
  rename( 'UTC_time' = 'timestamp (UTC)') %>%
  rename( 'elec_import_kwh' = 'energyConsumption (kWh)') %>%
  arrange(UTC_time)
```

``` r
# Half hourly gas import data
datg1 <- read_csv("Data/n3rgy/gas/20221110-20221201.csv", show_col_types = FALSE)
datg2 <- read_csv("Data/n3rgy/gas/20221201-20230201.csv", show_col_types = FALSE)
datg3 <- read_csv("Data/n3rgy/gas/20230201-20230401.csv", show_col_types = FALSE)
datg4 <- read_csv("Data/n3rgy/gas/20230401-20230601.csv", show_col_types = FALSE)
datg5 <- read_csv("Data/n3rgy/gas/20230601-20230701.csv", show_col_types = FALSE)

gas_halfhr <- rbind(datg1, datg2, datg3, datg4, datg5) %>% 
  select('timestamp (UTC)' ,'energyConsumption (m3)' ) %>% 
  rename( 'UTC_time' = 'timestamp (UTC)') %>%
  rename( 'gas_m3' = 'energyConsumption (m3)') %>%
  arrange(UTC_time)

## convert m3 to kwh 
# (https://www.gov.uk/guidance/gas-meter-readings-and-bill-calculation)
gas_halfhr$gas_kwh <- gas_halfhr$gas_m3*39.5*1.02264/3.6
```

## Checks

``` r
skim_without_charts(elec_halfhr)
```

|                                                  |             |
|:-------------------------------------------------|:------------|
| Name                                             | elec_halfhr |
| Number of rows                                   | 30545       |
| Number of columns                                | 2           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| numeric                                          | 1           |
| POSIXct                                          | 1           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: numeric**

| skim_variable   | n_missing | complete_rate | mean |   sd |  p0 |  p25 |  p50 |  p75 | p100 |
|:----------------|----------:|--------------:|-----:|-----:|----:|-----:|-----:|-----:|-----:|
| elec_import_kwh |         0 |             1 | 0.12 | 0.15 |   0 | 0.04 | 0.07 | 0.13 |  2.2 |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max        | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:-----------|:--------------------|---------:|
| UTC_time      |         0 |             1 | 2021-10-01 00:30:00 | 2023-07-01 | 2022-08-15 07:00:00 |    30545 |

``` r
str(elec_halfhr)
```

    ## tibble [30,545 × 2] (S3: tbl_df/tbl/data.frame)
    ##  $ UTC_time       : POSIXct[1:30545], format: "2021-10-01 00:30:00" "2021-10-01 01:00:00" ...
    ##  $ elec_import_kwh: num [1:30545] 0.064 0.088 0.449 0.09 0.056 0.034 0.04 0.049 0.036 0.042 ...

``` r
tail(elec_halfhr)
```

    ## # A tibble: 6 × 2
    ##   UTC_time            elec_import_kwh
    ##   <dttm>                        <dbl>
    ## 1 2023-06-30 21:30:00           0.287
    ## 2 2023-06-30 22:00:00           0.39 
    ## 3 2023-06-30 22:30:00           0.093
    ## 4 2023-06-30 23:00:00           0.062
    ## 5 2023-06-30 23:30:00           0.076
    ## 6 2023-07-01 00:00:00           0.044

``` r
skim_without_charts(gas_halfhr)
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | gas_halfhr |
| Number of rows                                   | 11175      |
| Number of columns                                | 3          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| numeric                                          | 2          |
| POSIXct                                          | 1          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |  p0 | p25 |  p50 |  p75 | p100 |
|:--------------|----------:|--------------:|-----:|-----:|----:|----:|-----:|-----:|-----:|
| gas_m3        |         0 |             1 | 0.06 | 0.09 |   0 |   0 | 0.01 | 0.11 | 0.78 |
| gas_kwh       |         0 |             1 | 0.69 | 1.05 |   0 |   0 | 0.08 | 1.23 | 8.81 |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max        | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:-----------|:--------------------|---------:|
| UTC_time      |         0 |             1 | 2022-11-10 04:30:00 | 2023-07-01 | 2023-03-06 14:00:00 |    11175 |

``` r
str(gas_halfhr)
```

    ## tibble [11,175 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ UTC_time: POSIXct[1:11175], format: "2022-11-10 04:30:00" "2022-11-10 05:00:00" ...
    ##  $ gas_m3  : num [1:11175] 0 0 0 0 0.041 0 0.229 0.057 0.051 0 ...
    ##  $ gas_kwh : num [1:11175] 0 0 0 0 0.46 ...

``` r
tail(gas_halfhr)
```

    ## # A tibble: 6 × 3
    ##   UTC_time            gas_m3 gas_kwh
    ##   <dttm>               <dbl>   <dbl>
    ## 1 2023-06-30 21:30:00  0       0    
    ## 2 2023-06-30 22:00:00  0       0    
    ## 3 2023-06-30 22:30:00  0.022   0.247
    ## 4 2023-06-30 23:00:00  0       0    
    ## 5 2023-06-30 23:30:00  0       0    
    ## 6 2023-07-01 00:00:00  0       0

``` r
# tidy up
rm(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9, dat10, dat11)
rm(datg1, datg2, datg3, datg4, datg5)
```

``` r
# check for missing entries
diffs = elec_halfhr$UTC_time - shift(elec_halfhr$UTC_time)
elec_halfhr[c(which(diffs>30)-1, which(diffs>30)),] %>%
  arrange(UTC_time)
```

    ## # A tibble: 22 × 2
    ##    UTC_time            elec_import_kwh
    ##    <dttm>                        <dbl>
    ##  1 2021-11-12 07:00:00           0.044
    ##  2 2021-11-12 08:00:00           0.071
    ##  3 2022-01-10 02:30:00           0.1  
    ##  4 2022-01-10 03:30:00           0.089
    ##  5 2022-03-04 05:30:00           0.034
    ##  6 2022-03-04 06:30:00           0.039
    ##  7 2022-05-06 07:30:00           0.413
    ##  8 2022-05-06 08:30:00           0.176
    ##  9 2022-07-14 07:30:00           0.035
    ## 10 2022-07-14 08:30:00           0.274
    ## # … with 12 more rows

``` r
diffs = gas_halfhr$UTC_time - shift(gas_halfhr$UTC_time)
gas_halfhr[c(which(diffs>30)-1, which(diffs>30)),] %>%
  arrange(UTC_time)
```

    ## # A tibble: 2 × 3
    ##   UTC_time            gas_m3 gas_kwh
    ##   <dttm>               <dbl>   <dbl>
    ## 1 2023-05-30 12:30:00      0       0
    ## 2 2023-05-30 13:30:00      0       0

It turns out there are 10 isolated missing electric measurements and 1
missing gas measurement up until 1/7/23. There is no obvious reason -
perhaps due to lost connectivity. All electric records are missing
between 21/3/23 13:00 and 23/3/23 00:00. Gas records are only available
from 10/11/22.

### Deal with missing data

Create the single missing measurements by repeating the previous value:

``` r
fill_dates <- c(ymd_hm("2021-11-12 07:30"), ymd_hm("2022-01-10 03:00"), ymd_hm("2022-03-04 06:00"), ymd_hm("2022-05-06 08:00"), ymd_hm("2022-07-14 08:00"), ymd_hm("2022-09-30 09:30"), ymd_hm("2022-12-01 05:30"), ymd_hm("2023-01-19 19:00"), ymd_hm("2023-03-09 00:00"), ymd_hm("2023-04-29 06:30"))

filled_elec_halfhr <- rbind(
    elec_halfhr, 
    data.frame(
      UTC_time = c(ymd_hm("2021-11-12 07:30"), ymd_hm("2022-01-10 03:00"), ymd_hm("2022-03-04 06:00"), ymd_hm("2022-05-06 08:00"), ymd_hm("2022-07-14 08:00"), ymd_hm("2022-09-30 09:30"), ymd_hm("2022-12-01 05:30"), ymd_hm("2023-01-19 19:00"), ymd_hm("2023-03-09 00:00"), ymd_hm("2023-04-29 06:30")), 
      elec_import_kwh=NA)) %>%
  arrange(UTC_time)

filled_gas_halfhr <- rbind(
  gas_halfhr,
  data.frame(
    UTC_time = c(ymd_hm("2023-05-30 13:00")), 
    gas_m3=NA, gas_kwh=NA)) %>%
  arrange(UTC_time)


# Fill missing data by repeating previous value.
ixs <- which(is.na(filled_elec_halfhr$elec_import_kwh))
filled_elec_halfhr$elec_import_kwh <- 
  nafill(filled_elec_halfhr$elec_import_kwh, "locf")

filled_gas_halfhr$gas_kwh <- 
  nafill(filled_gas_halfhr$gas_kwh, "locf")

# check the records filled
filled_elec_halfhr[ixs,] 
```

    ## # A tibble: 10 × 2
    ##    UTC_time            elec_import_kwh
    ##    <dttm>                        <dbl>
    ##  1 2021-11-12 07:30:00           0.044
    ##  2 2022-01-10 03:00:00           0.1  
    ##  3 2022-03-04 06:00:00           0.034
    ##  4 2022-05-06 08:00:00           0.413
    ##  5 2022-07-14 08:00:00           0.035
    ##  6 2022-09-30 09:30:00           0.035
    ##  7 2022-12-01 05:30:00           0.042
    ##  8 2023-01-19 19:00:00           0.725
    ##  9 2023-03-09 00:00:00           0.13 
    ## 10 2023-04-29 06:30:00           0.008

10 records created.

``` r
# check for duplicates
nrow(filled_elec_halfhr[duplicated(filled_elec_halfhr$UTC_time), ])
```

    ## [1] 0

``` r
nrow(filled_gas_halfhr[duplicated(filled_gas_halfhr$UTC_time), ])
```

    ## [1] 0

``` r
# check again for missing entries
diffs = filled_elec_halfhr$UTC_time - shift(filled_elec_halfhr$UTC_time)

filled_elec_halfhr[c(which(diffs>30)-1, which(diffs>30)),] %>%
  arrange(UTC_time)
```

    ## # A tibble: 2 × 2
    ##   UTC_time            elec_import_kwh
    ##   <dttm>                        <dbl>
    ## 1 2023-03-21 13:00:00           0.096
    ## 2 2023-03-23 00:00:00           0.06

``` r
diffs = filled_gas_halfhr$UTC_time - shift(filled_gas_halfhr$UTC_time)

filled_gas_halfhr[c(which(diffs>30)-1, which(diffs>30)),] %>%
  arrange(UTC_time)
```

    ## # A tibble: 0 × 3
    ## # … with 3 variables: UTC_time <dttm>, gas_m3 <dbl>, gas_kwh <dbl>

All looks good. Just the large gap between 21st and 23rd March 2023 in
the electric data.

``` r
skim_without_charts(filled_elec_halfhr)
```

|                                                  |                    |
|:-------------------------------------------------|:-------------------|
| Name                                             | filled_elec_halfhr |
| Number of rows                                   | 30555              |
| Number of columns                                | 2                  |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                    |
| Column type frequency:                           |                    |
| numeric                                          | 1                  |
| POSIXct                                          | 1                  |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                    |
| Group variables                                  | None               |

Data summary

**Variable type: numeric**

| skim_variable   | n_missing | complete_rate | mean |   sd |  p0 |  p25 |  p50 |  p75 | p100 |
|:----------------|----------:|--------------:|-----:|-----:|----:|-----:|-----:|-----:|-----:|
| elec_import_kwh |         0 |             1 | 0.12 | 0.15 |   0 | 0.04 | 0.07 | 0.13 |  2.2 |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max        | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:-----------|:--------------------|---------:|
| UTC_time      |         0 |             1 | 2021-10-01 00:30:00 | 2023-07-01 | 2022-08-15 07:00:00 |    30555 |

``` r
str(filled_elec_halfhr)
```

    ## tibble [30,555 × 2] (S3: tbl_df/tbl/data.frame)
    ##  $ UTC_time       : POSIXct[1:30555], format: "2021-10-01 00:30:00" "2021-10-01 01:00:00" ...
    ##  $ elec_import_kwh: num [1:30555] 0.064 0.088 0.449 0.09 0.056 0.034 0.04 0.049 0.036 0.042 ...

``` r
str(filled_gas_halfhr)
```

    ## tibble [11,176 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ UTC_time: POSIXct[1:11176], format: "2022-11-10 04:30:00" "2022-11-10 05:00:00" ...
    ##  $ gas_m3  : num [1:11176] 0 0 0 0 0.041 0 0.229 0.057 0.051 0 ...
    ##  $ gas_kwh : num [1:11176] 0 0 0 0 0.46 ...

``` r
# combine gas and elec data
n3rgy <- merge(filled_elec_halfhr, filled_gas_halfhr,
                    by="UTC_time",all=TRUE)

# fill NAs following merge with 0 (21,22/3/23 for elec) 
n3rgy$elec_import_kwh <- 
  nafill(n3rgy$elec_import_kwh, fill=0)
```

### Look for discrepancies with supplier data

Compare daily totals with dataset from EDF

``` r
# n3rgy data
n3rgy$date <- as.Date(n3rgy$UTC_time)
n3rgy_summary <-
  n3rgy %>%
  group_by(date) %>%
  summarise(n3rgy_elec_import_kwh=sum(elec_import_kwh),
            n3rgy_gas_kwh=sum(gas_kwh),
            )

daily_summary <- merge(n3rgy_summary, daily_data, by="date")


# looking for discrepancies 

# days where the EDF and n3rgy data disagree by more than 1kwh
daily_summary[which(abs(daily_summary$n3rgy_elec_import_kwh - daily_summary$elec_usage_kwh) > 1), c("date","n3rgy_elec_import_kwh", "elec_usage_kwh")]
```

    ##           date n3rgy_elec_import_kwh elec_usage_kwh
    ## 9   2021-10-09                 6.976           5.76
    ## 20  2021-10-20                 5.575           7.38
    ## 21  2021-10-21                 6.394           4.62
    ## 53  2021-11-22                 4.746           6.03
    ## 54  2021-11-23                 7.788           6.48
    ## 537 2023-03-21                 1.980           4.10
    ## 538 2023-03-22                 0.000           4.10

``` r
daily_summary[which(abs(daily_summary$n3rgy_gas_kwh - daily_summary$gas_usage_kwh) > 1), c("date","n3rgy_gas_kwh", "gas_usage_kwh")]
```

    ##           date n3rgy_gas_kwh gas_usage_kwh
    ## 437 2022-12-11      65.18066         67.17
    ## 438 2022-12-12      82.03405         80.90
    ## 439 2022-12-13      68.17657         70.51
    ## 440 2022-12-14      88.62056         87.62
    ## 442 2022-12-16      81.30471         82.85
    ## 443 2022-12-17      73.00144         71.27
    ## 452 2022-12-26      65.52850         66.81
    ## 461 2023-01-04      36.01823         37.79
    ## 462 2023-01-05      44.24296         43.02
    ## 473 2023-01-16      58.81856         60.21
    ## 481 2023-01-24      60.75973         62.20
    ## 482 2023-01-25      61.59006         60.55
    ## 538 2023-03-22      22.48615         29.81
    ## 539 2023-03-23      34.94105         29.81
    ## 553 2023-04-06      31.30557         33.12
    ## 554 2023-04-07      34.35758         32.92

The days where supplier and n3rgy data disagree most for electricity
consumption coincide with days when the supplier provided estimates and
where the n3rgy dataset was missing records. This does not seem to be
the case for gas data.Possibly the conversion

Compare monthly totals with daily dataset from EDF

``` r
# EDF data
daily_data$yr_mth <- paste(year(daily_data$date), month(daily_data$date), sep='-')
EDF_summary <-
  daily_data %>%
  group_by(yr_mth) %>%
  summarise(EDF_elec_import_kwh=sum(elec_usage_kwh),
            EDF_gas_import_kwh=sum(gas_usage_kwh),
           # EDF_count=n() 
            )

# n3rgy data
n3rgy$yr_mth <- paste(year(n3rgy$UTC_time),
                      month(n3rgy$UTC_time), sep='-')
n3rgy_summary <-
  n3rgy %>%
  group_by(yr_mth) %>%
  summarise(n3rgy_elec_import_kwh=sum(elec_import_kwh),
            n3rgy_gas_kwh=sum(gas_kwh),
          #  n3rgy_count=n() 
            )


monthly_summary <- merge(n3rgy_summary, 
             EDF_summary, by="yr_mth")
monthly_summary$elec_diff <- monthly_summary$EDF_elec_import_kwh -
  monthly_summary$n3rgy_elec_import_kwh

monthly_summary$gas_diff <- monthly_summary$EDF_gas_import_kwh -
  monthly_summary$n3rgy_gas_kwh

# looking for discrepancies > 1 %
monthly_summary[which(abs(monthly_summary$elec_diff/monthly_summary$EDF_elec_import_kwh) > 0.01), c("yr_mth","elec_diff","EDF_elec_import_kwh")]
```

    ##    yr_mth elec_diff EDF_elec_import_kwh
    ## 18 2023-3     5.927               144.5

``` r
monthly_summary[which(abs(monthly_summary$gas_diff/monthly_summary$EDF_gas_import_kwh) > 0.01),c("yr_mth","gas_diff","EDF_gas_import_kwh")]
```

    ##    yr_mth gas_diff EDF_gas_import_kwh
    ## 20 2023-5 2.285004             213.39
    ## 21 2023-6 2.412497             160.50

The EDF and n3rgy data are not identical but monthly totals agree to
within a few KWh - less than 1% on electric for all months bar March
2023 which is missing 1.5 day’s data in the n3rgy dataset.

Total difference (Kwh) between supplier and n3rgy electric data (Oct
2021 - June 2023):

``` r
# Total difference between supplier and n3rgy electric data 
round(sum(daily_summary$n3rgy_elec_import_kwh) - sum(daily_summary$elec_usage_kwh),2)
```

    ## [1] -19.74

## Aggregate by hour in local time

``` r
# convert to local time and aggregate by hour
n3rgy_hourly <- n3rgy %>%
   mutate(local_time=format(as.POSIXct(with_tz(n3rgy$UTC_time, "Europe/London") ), "%Y-%m-%d %H")) %>%
  # aggregate st 01:00 + 01:30 => 02:00 if BST else => 01:00 (centre of range assuming data is hour ending)
  group_by(local_time) %>%
  summarise(elec_import_kwh=sum(elec_import_kwh),
            gas_kwh=round(sum(gas_kwh),3)) %>%
  mutate(local_time=as.POSIXct(local_time, tz="Europe/London", format = "%Y-%m-%d %H")) 
```

Doing a check to be sure the aggregation is as expected and timezones
are dealt with correctly..

``` r
# Check aggregation as expected and timezones dealt with correctly
# (NB normally not a good idea to test floats for equality
# Here testing result is expected sum)

# BST ended 31/10/21 and 30/10/22 => 
# TEST BST: 31/10/21 00:00 local time (BST) is sum of 30/10/21 23:00 and 23:30 in UTC
n3rgy_hourly$elec_import_kwh[which(
  n3rgy_hourly$local_time == as.POSIXct("2021-10-31 00:00:00", tz="Europe/London")
  )] == 
n3rgy$elec_import_kwh[which(n3rgy$UTC_time == as.POSIXct("2021-10-30 23:00:00", tz="UCT"))] + n3rgy$elec_import_kwh[which(n3rgy$UTC_time == as.POSIXct("2021-10-30 23:30:00", tz="UCT"))]
```

    ## [1] TRUE

``` r
# TEST Autumn clock change: 31/10/21 01:00 local time is sum of 00:00-01:30 in UTC,
n3rgy_hourly$elec_import_kwh[which(
  n3rgy_hourly$local_time == as.POSIXct("2021-10-31 01:00:00", tz="Europe/London")
  )] == 
sum( n3rgy$elec_import_kwh[which(
  n3rgy$UTC_time >= as.POSIXct("2021-10-31 00:00:00", tz="UCT") & 
  n3rgy$UTC_time <= as.POSIXct("2021-10-31 01:30:00", tz="UCT")
  )] 
)
```

    ## [1] TRUE

``` r
# TEST GMT: 31/10/21 02:00 local time (after clock put back to GMT) is 
# sum of 02:00 and 02:30 UTC
n3rgy_hourly$elec_import_kwh[which(
  n3rgy_hourly$local_time == as.POSIXct("2021-10-31 02:00:00", tz="Europe/London")
  )] ==  
n3rgy$elec_import_kwh[which(n3rgy$UTC_time == as.POSIXct("2021-10-31 02:00:00", tz="UCT"))] + n3rgy$elec_import_kwh[which(n3rgy$UTC_time == as.POSIXct("2021-10-31 02:30:00", tz="UCT"))]
```

    ## [1] TRUE

``` r
## show the records for a visual check
n3rgy_hourly[which(
  n3rgy_hourly$local_time >= as.POSIXct("2021-10-30 23:00:00", tz="UCT") &
  n3rgy_hourly$local_time < as.POSIXct("2021-10-31 03:00:00", tz="UCT")),]
```

    ## # A tibble: 3 × 3
    ##   local_time          elec_import_kwh gas_kwh
    ##   <dttm>                        <dbl>   <dbl>
    ## 1 2021-10-31 00:00:00           0.59       NA
    ## 2 2021-10-31 01:00:00           0.75       NA
    ## 3 2021-10-31 02:00:00           0.078      NA

``` r
n3rgy[which(
  n3rgy$UTC_time >= as.POSIXct("2021-10-30 23:00:00", tz="UCT") & 
  n3rgy$UTC_time < as.POSIXct("2021-10-31 03:00:00", tz="UCT")),]
```

    ##                 UTC_time elec_import_kwh gas_m3 gas_kwh       date  yr_mth
    ## 1438 2021-10-30 23:00:00           0.120     NA      NA 2021-10-30 2021-10
    ## 1439 2021-10-30 23:30:00           0.470     NA      NA 2021-10-30 2021-10
    ## 1440 2021-10-31 00:00:00           0.071     NA      NA 2021-10-31 2021-10
    ## 1441 2021-10-31 00:30:00           0.085     NA      NA 2021-10-31 2021-10
    ## 1442 2021-10-31 01:00:00           0.423     NA      NA 2021-10-31 2021-10
    ## 1443 2021-10-31 01:30:00           0.171     NA      NA 2021-10-31 2021-10
    ## 1444 2021-10-31 02:00:00           0.041     NA      NA 2021-10-31 2021-10
    ## 1445 2021-10-31 02:30:00           0.037     NA      NA 2021-10-31 2021-10

``` r
# BST started 27/3/22, 26/3/22 =>
# there will be no record for 2022-03-27 01:00:00 in local time
# NB when requested directly R appears to repeat the record for 00:00, even tho the record is not in the table!
# TEST Spring clock change: Sum of 27/3/22 00:00 and 01:00 in local time is same as 00:00 
sum(n3rgy_hourly$elec_import_kwh[which(
  n3rgy_hourly$local_time >= as.POSIXct("2022-03-27 00:00:00", tz="Europe/London") & 
  n3rgy_hourly$local_time <= as.POSIXct("2022-03-27 01:00:00", tz="Europe/London")
  )] 
) == n3rgy_hourly$elec_import_kwh[which(
  n3rgy_hourly$local_time == as.POSIXct("2022-03-27 00:00:00", tz="Europe/London")
  )] 
```

    ## [1] TRUE

``` r
sum(n3rgy_hourly$gas_kwh[which(
    n3rgy_hourly$local_time >= as.POSIXct("2023-03-26 00:00:00", tz="Europe/London") & 
    n3rgy_hourly$local_time <= as.POSIXct("2023-03-26 01:00:00", tz="Europe/London")
  )] 
) == 
  n3rgy_hourly$gas_kwh[which(
  n3rgy_hourly$local_time == as.POSIXct("2023-03-26 00:00:00", tz="Europe/London")
  )] 
```

    ## [1] TRUE

``` r
## show the records for a visual check
n3rgy_hourly[which(
  n3rgy_hourly$local_time >= as.POSIXct("2023-03-26 00:00:00", tz="UCT") & 
  n3rgy_hourly$local_time < as.POSIXct("2023-03-26 03:00:00", tz="UCT")),]
```

    ## # A tibble: 3 × 3
    ##   local_time          elec_import_kwh gas_kwh
    ##   <dttm>                        <dbl>   <dbl>
    ## 1 2023-03-26 00:00:00           0.493   0.314
    ## 2 2023-03-26 02:00:00           0.182   0.135
    ## 3 2023-03-26 03:00:00           0.112   0

``` r
n3rgy[which(
  n3rgy$UTC_time >= as.POSIXct("2023-03-26 00:00:00", tz="UCT") & 
  n3rgy$UTC_time < as.POSIXct("2023-03-26 03:00:00", tz="UCT")),]
```

    ##                  UTC_time elec_import_kwh gas_m3   gas_kwh       date yr_mth
    ## 25968 2023-03-26 00:00:00           0.329  0.028 0.3141777 2023-03-26 2023-3
    ## 25969 2023-03-26 00:30:00           0.164  0.000 0.0000000 2023-03-26 2023-3
    ## 25970 2023-03-26 01:00:00           0.103  0.012 0.1346476 2023-03-26 2023-3
    ## 25971 2023-03-26 01:30:00           0.079  0.000 0.0000000 2023-03-26 2023-3
    ## 25972 2023-03-26 02:00:00           0.080  0.000 0.0000000 2023-03-26 2023-3
    ## 25973 2023-03-26 02:30:00           0.032  0.000 0.0000000 2023-03-26 2023-3

Looks OK so write to csv and tidy up

``` r
write.csv(n3rgy_hourly, file="Data/230630_hourly.csv", row.names=FALSE)

# tidy up some temporary files
rm(elec_halfhr, gas_halfhr, filled_elec_halfhr, filled_gas_halfhr, EDF_summary, n3rgy_summary, diffs, ixs)
```

# 3. Load and prepare solar yield estimate data

Take a quick visual check of loaded data

``` r
solar_estimate_pvgis <- read.csv(
  "Data/solar estimate/PVdata_SA2_crystSi_3kWp_14_30deg_-35deg.csv",
                                 skip=9, nrows=12, sep='\t')

solar_estimate <- solar_estimate_pvgis %>% 
  select('Month' ,'E_d', 'E_m','SD_m' ) %>% 
  rename( 'month' = 'Month') %>%
  rename( 'avg_daily_yield_kwh' = 'E_d') %>%
  rename( 'avg_monthly_yield_kwh' = 'E_m') %>%
  rename( 'sd_monthly_yield_kwh' = 'SD_m') %>%
  arrange('month')


solar_estimate
```

    ##    month avg_daily_yield_kwh avg_monthly_yield_kwh sd_monthly_yield_kwh
    ## 1      1                3.06                 94.84                12.60
    ## 2      2                6.04                169.01                34.62
    ## 3      3                9.56                296.42                38.12
    ## 4      4               13.36                400.76                55.23
    ## 5      5               14.78                458.33                56.71
    ## 6      6               14.73                441.97                50.29
    ## 7      7               14.17                439.34                61.58
    ## 8      8               12.47                386.62                29.25
    ## 9      9               10.39                311.67                28.75
    ## 10    10                6.54                202.67                23.92
    ## 11    11                4.33                130.04                19.31
    ## 12    12                2.94                 91.18                16.05

``` r
#write.csv(solar_estimate, file="Data/PVGIS_solar_estimate_mthly.csv", row.names=FALSE)
```
