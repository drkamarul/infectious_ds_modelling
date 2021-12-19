Practical: Introduction to Spatial Data
================
Kamarul Imran Musa
8/6/2019

-   [Workflow](#workflow)
-   [Preparing environment for
    analysis](#preparing-environment-for-analysis)
-   [Read polygons data](#read-polygons-data)
-   [Dengue data](#dengue-data)
-   [Data cleaning](#data-cleaning)
-   [Convert the dengue data to spatial
    data](#convert-the-dengue-data-to-spatial-data)
-   [Convert shapefile to RSO
    (https://epsg.io/3168)](#convert-shapefile-to-rso-httpsepsgio3168)
-   [Dealing with dates](#dealing-with-dates)
-   [Spatial plots](#spatial-plots)
-   [Density of dengue cases](#density-of-dengue-cases)
-   [Plot incidence of dengue for
    mukim](#plot-incidence-of-dengue-for-mukim)

# Workflow

-   Prepare environment :
    -   directory
    -   load packages
    -   spatial data (Malaysia and Dengue)
-   Read data
    -   polygons
    -   points
-   Analyze

# Preparing environment for analysis

The packages for

-   running spatial analysis
-   data wrangling
-   import data
-   deals with regular expressions

``` r
library(sf)
```

    ## Linking to GEOS 3.9.1, GDAL 3.2.1, PROJ 7.2.1

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(here)
```

    ## here() starts at C:/Users/drkim/OneDrive - Universiti Sains Malaysia/NIH_Infectious_Ds_Modelling/KIM/infectious_ds_modelling.git

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(DT)
library(stringr)
library(readxl)
library(broom)
library(tmap)
library(mapview)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

# Read polygons data

``` r
kel <- st_read(here("data", 
                    "Map_Kelantan",
                    "kelantan.shp")) 
```

    ## Reading layer `kelantan' from data source 
    ##   `C:\Users\drkim\OneDrive - Universiti Sains Malaysia\NIH_Infectious_Ds_Modelling\KIM\infectious_ds_modelling.git\data\Map_Kelantan\kelantan.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 66 features and 6 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 371629.6 ymin: 503028.2 xmax: 519479.6 ymax: 690232.8
    ## Projected CRS: Kertau (RSO) / RSO Malaya (m)

``` r
kel %>% datatable()
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The geometry is *Projected CRS: Kertau (RSO) / RSO Malaya (m)*

``` r
st_geometry(kel)
```

    ## Geometry set for 66 features 
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 371629.6 ymin: 503028.2 xmax: 519479.6 ymax: 690232.8
    ## Projected CRS: Kertau (RSO) / RSO Malaya (m)
    ## First 5 geometries:

    ## POLYGON ((485501.8 669698.8, 485717.3 669694.6,...

    ## POLYGON ((487716.5 665649.5, 487615.4 665445.1,...

    ## POLYGON ((482744.8 660223.4, 482823.6 660137.8,...

    ## POLYGON ((486936.9 677358.5, 486990.5 677333.7,...

    ## POLYGON ((490841.7 668783.4, 490906.1 668691, 4...

Make plot

``` r
plot(kel[,2]) #daerah
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
plot(kel[,3]) #mukim
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
plot(kel[,6]) #population
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

Or using geom_sf from **ggplot** package

``` r
kel %>% 
  ggplot() +
  geom_sf(aes(fill = JUM_JANTIN)) +
  ggtitle('Population by subdistrict') +
  theme_bw()
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Another way of plotting (by changing the color palette)

``` r
ggplot() + 
  geom_sf(data = kel, aes(fill = JUM_JANTIN)) +
  scale_fill_gradientn(colors = sf.colors(20)) +
  ggtitle('Population by subdistrict') +
  theme_bw()
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

or using the **tmap** package

In a non-interactive mode

``` r
tm_shape(kel) +
  tm_polygons("JUM_JANTIN")
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Adding facets (by splitting the map based on gender)

``` r
tm_shape(kel) +
  tm_polygons(c("LELAKI", "PEREMPUAN")) +
  tm_facets(ncol = 2)
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Adding a variable for are using `sr_area()`

``` r
kel <- kel %>% mutate(area_muk = st_area(kel),
                      area_muk_km2 = st_area(kel)/1000000)
kel %>% datatable()
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# Dengue data

-   This is vector data in points format.
-   The dengue file comes in `xlsx` format.
-   So we will read using **read_xl** package

``` r
den <- read_xlsx(here('data',
                      'dengue_2019.xlsx')) %>%
  clean_names()
glimpse(den)
```

    ## Rows: 2,567
    ## Columns: 13
    ## $ notifikasi_no           <dbl> 4090641, 4092674, 4222794, 4030932, 4273311, 4~
    ## $ kes_no                  <chr> "2019/32780", "2019/33141", "2019/59670", "201~
    ## $ tarikh_onset            <chr> "17-03-2019", "15-03-2019", "19-06-2019", "13-~
    ## $ tkh_notifikasi          <chr> "20-03-2019", "21-03-2019", "22-06-2019", "16-~
    ## $ lokaliti_alamat_semasa  <chr> "KELAIK", "BLAU", "HANIR", "PPRT TANAH PUTIH",~
    ## $ mukim_zon_alamat_semasa <chr> "BERTAM", "BERTAM", "BERTAM", "GALAS", "GALAS"~
    ## $ umur_tahun              <dbl> 11, 10, 9, 27, 41, 27, 38, 18, 14, 12, 22, 30,~
    ## $ epid_minggu_tkh_onset   <dbl> 12, 11, 25, 7, 29, 6, 52, 8, 52, 30, 8, 5, 26,~
    ## $ latitud_iso             <dbl> 4.722550, 4.761049, 4.763850, 4.819074, 4.8279~
    ## $ lngitud_iso             <dbl> 101.6293, 101.7543, 101.4727, 101.9701, 101.96~
    ## $ status_akhir_kes        <chr> "Hidup", "Hidup", "Hidup", "Hidup", "Hidup", "~
    ## $ jantina                 <chr> "Lelaki", "Lelaki", "Lelaki", "Lelaki", "Perem~
    ## $ sub_diagnosis           <chr> "DENGUE FEVER", "DENGUE FEVER", "DENGUE FEVER"~

# Data cleaning

-   select the first 8 letters of variable name

``` r
den2 <- den %>% select_all(~str_sub(., end = 8L))
glimpse(den2)
```

    ## Rows: 2,567
    ## Columns: 13
    ## $ notifika <dbl> 4090641, 4092674, 4222794, 4030932, 4273311, 4025693, 3940032~
    ## $ kes_no   <chr> "2019/32780", "2019/33141", "2019/59670", "2019/20659", "2019~
    ## $ tarikh_o <chr> "17-03-2019", "15-03-2019", "19-06-2019", "13-02-2019", "15-0~
    ## $ tkh_noti <chr> "20-03-2019", "21-03-2019", "22-06-2019", "16-02-2019", "18-0~
    ## $ lokaliti <chr> "KELAIK", "BLAU", "HANIR", "PPRT TANAH PUTIH", "TAMAN WAWASAN~
    ## $ mukim_zo <chr> "BERTAM", "BERTAM", "BERTAM", "GALAS", "GALAS", "GALAS", "GAL~
    ## $ umur_tah <dbl> 11, 10, 9, 27, 41, 27, 38, 18, 14, 12, 22, 30, 39, 34, 13, 33~
    ## $ epid_min <dbl> 12, 11, 25, 7, 29, 6, 52, 8, 52, 30, 8, 5, 26, NA, 7, 6, 27, ~
    ## $ latitud_ <dbl> 4.722550, 4.761049, 4.763850, 4.819074, 4.827947, 4.831150, 4~
    ## $ lngitud_ <dbl> 101.6293, 101.7543, 101.4727, 101.9701, 101.9633, 101.9496, 1~
    ## $ status_a <chr> "Hidup", "Hidup", "Hidup", "Hidup", "Hidup", "Hidup", "Hidup"~
    ## $ jantina  <chr> "Lelaki", "Lelaki", "Lelaki", "Lelaki", "Perempuan", "Perempu~
    ## $ sub_diag <chr> "DENGUE FEVER", "DENGUE FEVER", "DENGUE FEVER", "DENGUE FEVER~

Need to remove missing data (NA) in coordinate columns

``` r
den2 <- den2 %>% 
  filter(!is.na(latitud_),
         !is.na(lngitud_))
```

# Convert the dengue data to spatial data

-   The excel is in WGS84 format so `CRS = 4326`.
-   Then let’s describe `den` (now is spatial format)

``` r
loc_den <- st_as_sf(den2, 
                    coords = c("lngitud_", "latitud_"), 
                    crs = 4326)
loc_den %>% datatable()
```

    ## Warning in instance$preRenderHook(instance): It seems your data is too big
    ## for client-side DataTables. You may consider server-side processing: https://
    ## rstudio.github.io/DT/server.html

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Let’s confirm the `CRS` in WGS84 (EPSG:4326)

``` r
st_crs(loc_den)
```

    ## Coordinate Reference System:
    ##   User input: EPSG:4326 
    ##   wkt:
    ## GEOGCRS["WGS 84",
    ##     DATUM["World Geodetic System 1984",
    ##         ELLIPSOID["WGS 84",6378137,298.257223563,
    ##             LENGTHUNIT["metre",1]]],
    ##     PRIMEM["Greenwich",0,
    ##         ANGLEUNIT["degree",0.0174532925199433]],
    ##     CS[ellipsoidal,2],
    ##         AXIS["geodetic latitude (Lat)",north,
    ##             ORDER[1],
    ##             ANGLEUNIT["degree",0.0174532925199433]],
    ##         AXIS["geodetic longitude (Lon)",east,
    ##             ORDER[2],
    ##             ANGLEUNIT["degree",0.0174532925199433]],
    ##     USAGE[
    ##         SCOPE["Horizontal component of 3D system."],
    ##         AREA["World."],
    ##         BBOX[-90,-180,90,180]],
    ##     ID["EPSG",4326]]

Make plots to see if map is alright (no outliers)

``` r
ggplot() +
  geom_sf(data = loc_den) +
  ggtitle("Map of Dengue 2019") +
  theme_bw()
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

# Convert shapefile to RSO (<https://epsg.io/3168>)

Reasons

-   the kel polygons is in RSO
-   so the dengue points should also be in RSO

``` r
loc_den2 <- st_transform(loc_den, 3168)
loc_den2 %>% datatable()
```

    ## Warning in instance$preRenderHook(instance): It seems your data is too big
    ## for client-side DataTables. You may consider server-side processing: https://
    ## rstudio.github.io/DT/server.html

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Now select points that fall only in Kelantan. Experince tells us there
are always points outside the required boundary

``` r
den_kel <- loc_den2 %>% 
  mutate(within_kel = lengths(st_within(loc_den2, kel)))
den_kel %>% datatable()
```

    ## Warning in instance$preRenderHook(instance): It seems your data is too big
    ## for client-side DataTables. You may consider server-side processing: https://
    ## rstudio.github.io/DT/server.html

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
den_kel <- den_kel %>% 
  filter(within_kel == 1)
```

# Dealing with dates

-   variable `tarikh_o` and `tkh_noti` are in `chr` format
-   need to change chr to the correct `dates` format
-   <https://lubridate.tidyverse.org/>
-   <http://daniellequinn.github.io/RLessons/FormattingDates/FormattingDates.html>

``` r
den_kel <- den_kel %>% 
  mutate_at(vars(tarikh_o, tkh_noti), ~dmy(.))
```

We can split dates into

-   month
-   day
-   year

``` r
den_kel <- den_kel %>% 
  mutate(year_on =  year(tarikh_o),
         month_on = month(tarikh_o),
         day_on = day(tarikh_o))
den_kel %>% datatable()
```

    ## Warning in instance$preRenderHook(instance): It seems your data is too big
    ## for client-side DataTables. You may consider server-side processing: https://
    ## rstudio.github.io/DT/server.html

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Generate epiweek

``` r
den_kel <- den_kel %>% 
  mutate(epiweek_on = epiweek(tarikh_o))
```

Now we have

-   polygon data
-   point data

``` r
glimpse(den_kel)
```

    ## Rows: 2,562
    ## Columns: 17
    ## $ notifika   <dbl> 4090641, 4092674, 4222794, 4030932, 4273311, 4025693, 39400~
    ## $ kes_no     <chr> "2019/32780", "2019/33141", "2019/59670", "2019/20659", "20~
    ## $ tarikh_o   <date> 2019-03-17, 2019-03-15, 2019-06-19, 2019-02-13, 2019-07-15~
    ## $ tkh_noti   <date> 2019-03-20, 2019-03-21, 2019-06-22, 2019-02-16, 2019-07-18~
    ## $ lokaliti   <chr> "KELAIK", "BLAU", "HANIR", "PPRT TANAH PUTIH", "TAMAN WAWAS~
    ## $ mukim_zo   <chr> "BERTAM", "BERTAM", "BERTAM", "GALAS", "GALAS", "GALAS", "G~
    ## $ umur_tah   <dbl> 11, 10, 9, 27, 41, 27, 38, 18, 14, 12, 22, 30, 39, 34, 13, ~
    ## $ epid_min   <dbl> 12, 11, 25, 7, 29, 6, 52, 8, 52, 30, 8, 5, 26, NA, 7, 6, 27~
    ## $ status_a   <chr> "Hidup", "Hidup", "Hidup", "Hidup", "Hidup", "Hidup", "Hidu~
    ## $ jantina    <chr> "Lelaki", "Lelaki", "Lelaki", "Lelaki", "Perempuan", "Perem~
    ## $ sub_diag   <chr> "DENGUE FEVER", "DENGUE FEVER", "DENGUE FEVER", "DENGUE FEV~
    ## $ geometry   <POINT [m]> POINT (404312.2 522488.3), POINT (418189.6 526709.4),~
    ## $ within_kel <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,~
    ## $ year_on    <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2018, 2019, 2018, 2019,~
    ## $ month_on   <dbl> 3, 3, 6, 2, 7, 2, 12, 2, 12, 7, 2, 1, 6, 7, 2, 2, 7, 1, 7, ~
    ## $ day_on     <int> 17, 15, 19, 13, 15, 8, 23, 21, 26, 21, 17, 27, 29, 3, 13, 3~
    ## $ epiweek_on <dbl> 12, 11, 25, 7, 29, 6, 52, 8, 52, 30, 8, 5, 26, 27, 7, 6, 27~

``` r
glimpse(loc_den2)
```

    ## Rows: 2,567
    ## Columns: 12
    ## $ notifika <dbl> 4090641, 4092674, 4222794, 4030932, 4273311, 4025693, 3940032~
    ## $ kes_no   <chr> "2019/32780", "2019/33141", "2019/59670", "2019/20659", "2019~
    ## $ tarikh_o <chr> "17-03-2019", "15-03-2019", "19-06-2019", "13-02-2019", "15-0~
    ## $ tkh_noti <chr> "20-03-2019", "21-03-2019", "22-06-2019", "16-02-2019", "18-0~
    ## $ lokaliti <chr> "KELAIK", "BLAU", "HANIR", "PPRT TANAH PUTIH", "TAMAN WAWASAN~
    ## $ mukim_zo <chr> "BERTAM", "BERTAM", "BERTAM", "GALAS", "GALAS", "GALAS", "GAL~
    ## $ umur_tah <dbl> 11, 10, 9, 27, 41, 27, 38, 18, 14, 12, 22, 30, 39, 34, 13, 33~
    ## $ epid_min <dbl> 12, 11, 25, 7, 29, 6, 52, 8, 52, 30, 8, 5, 26, NA, 7, 6, 27, ~
    ## $ status_a <chr> "Hidup", "Hidup", "Hidup", "Hidup", "Hidup", "Hidup", "Hidup"~
    ## $ jantina  <chr> "Lelaki", "Lelaki", "Lelaki", "Lelaki", "Perempuan", "Perempu~
    ## $ sub_diag <chr> "DENGUE FEVER", "DENGUE FEVER", "DENGUE FEVER", "DENGUE FEVER~
    ## $ geometry <POINT [m]> POINT (404312.2 522488.3), POINT (418189.6 526709.4), P~

# Spatial plots

The overall plot for the year 2019

``` r
den_kel <- den_kel %>% 
  filter(year_on == 2019) 
```

The plots

``` r
den_plot <- ggplot() +
  geom_sf(data = kel) +
  geom_sf(data = den_kel) +
  ggtitle("Map of Dengue Cases for 2019") +
  theme_bw()
den_plot
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

Now let’s make plots based on

-   jantina
-   year of onset
-   month of onset

``` r
den_plot + 
  facet_wrap(~jantina)
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
den_plot + 
  facet_wrap(~ year_on + month_on, ncol = 3)
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
den_plot + 
  facet_wrap(~ DAERAH, ncol = 3)
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

Further analysis for year 2019

-   number of dengue cases (column n) based on year, month and epid week
-   bar chart for the total number per epi week
-   line chart for the trend of dengue for each epid week

``` r
# count den per epiweek
den_kel %>% 
  filter(year_on == 2019) %>% 
  group_by(year_on, month_on, epiweek_on) %>% 
  count() %>% 
  print(n = 52) %>%
  ggplot(aes(x = epiweek_on, y = n)) + 
  geom_bar(stat = 'identity', fill = 'white') +
  geom_line(colour = "red") +
  ggtitle('Number of dengue cases based on epid week') +
  ylab('Number') +
  xlab('Epid week based on date of onset') +
  theme_bw()
```

    ## Simple feature collection with 36 features and 4 fields
    ## Geometry type: MULTIPOINT
    ## Dimension:     XY
    ## Bounding box:  xmin: 386963.3 ymin: 522488.3 xmax: 504175.9 ymax: 689489.7
    ## Projected CRS: Kertau (RSO) / RSO Malaya (m)
    ## # A tibble: 36 x 5
    ##    year_on month_on epiweek_on     n                                    geometry
    ##  *   <dbl>    <dbl>      <dbl> <int>                            <MULTIPOINT [m]>
    ##  1    2019        1          1    58 ((435336 633662.3), (443415.3 664929.2), (~
    ##  2    2019        1          2    82 ((417437.5 636433.1), (420844.8 629604.8),~
    ##  3    2019        1          3    85 ((416959.5 632635.4), (432553.9 639824.2),~
    ##  4    2019        1          4    72 ((435471.3 643990.7), (441513.4 540349.9),~
    ##  5    2019        1          5    51 ((433551.9 638523.4), (434902.6 604965.7),~
    ##  6    2019        2          5    20 ((442406.1 642597.6), (456045.3 688892.8),~
    ##  7    2019        2          6   107 ((414031.3 629252), (417508.4 635945.3), (~
    ##  8    2019        2          7   109 ((430535.8 635203.2), (433182.8 638900.5),~
    ##  9    2019        2          8    93 ((430457.8 636583.7), (430950 637789.1), (~
    ## 10    2019        2          9    66 ((414037.1 636278.2), (428840.3 630323.9),~
    ## 11    2019        3          9    25 ((433245.5 638903.7), (443940.3 665169.4),~
    ## 12    2019        3         10    76 ((433182.8 638900.5), (442435 565576.9), (~
    ## 13    2019        3         11    77 ((418189.6 526709.4), (434194.4 637892.2),~
    ## 14    2019        3         12    71 ((404312.2 522488.3), (421600.1 541297.9),~
    ## 15    2019        3         13    57 ((433188.9 638997.6), (450335.1 636529.1),~
    ## 16    2019        3         14    10 ((430687.9 635949.5), (447914.8 666573.9),~
    ## 17    2019        4         14    53 ((420911.8 629391.9), (445832.4 663836.8),~
    ## 18    2019        4         15    56 ((442021.2 541085), (453457.6 671445.9), (~
    ## 19    2019        4         16    50 ((455935.5 688218), (461169.4 668977.3), (~
    ## 20    2019        4         17    34 ((450122.9 642672.6), (460674.8 671090.5),~
    ## 21    2019        4         18     9 ((457412.1 684372.7), (464097.6 658086.8),~
    ## 22    2019        5         18    12 ((435184.1 643997.5), (442032.9 541396.6),~
    ## 23    2019        5         19    65 ((417885.5 636390.4), (427277.4 627849.4),~
    ## 24    2019        5         20    24 ((449936.4 652676.4), (450730.5 569114.9),~
    ## 25    2019        5         21    25 ((429126.3 627872.8), (461713.9 685087.4),~
    ## 26    2019        5         22    34 ((442640.9 665603), (443102.2 637561.4), (~
    ## 27    2019        6         22     3 ((445698.3 664001.5), (446357 646864.9), (~
    ## 28    2019        6         23    40 ((431488.6 623976.5), (433391.8 638447.5),~
    ## 29    2019        6         24    63 ((421066.6 629885), (430773 637435.6), (44~
    ## 30    2019        6         25    96 ((386963.3 527102.7), (433171.7 638772.7),~
    ## 31    2019        6         26    91 ((429051.5 627524.6), (429087.1 540727.3),~
    ## 32    2019        6         27    16 ((438626.3 647702.6), (443420.2 597738.1),~
    ## 33    2019        7         27   131 ((432873.8 644871.4), (440416.7 541971.6),~
    ## 34    2019        7         28   226 ((427014.6 628390.2), (428468.9 629540.1),~
    ## 35    2019        7         29   270 ((412699.2 628141.4), (428953.3 629635.1),~
    ## 36    2019        7         30   129 ((413806.4 629251.6), (428210.8 630454.4),~

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

Using the **tmaps** package

All data for all years

``` r
tm_shape(kel) +
  tm_polygons("DAERAH") + 
  tm_shape(den_kel) +
  tm_dots()
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

Split plots

``` r
tm_shape(kel) +
  tm_polygons("NEGERI") +
  tm_facets('DAERAH') + 
  tm_shape(den_kel) +
  tm_dots(shape = 'jantina', size = 0.1) 
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

# Density of dengue cases

Aim : to calculate dengue cases per area

Steps:

-   Join points `den_kel` to polygons `kel` shapefiles
-   Calculate dengue points in each mukim (polygons)

``` r
den_in_muk <- st_join(den_kel, kel, 
                      join = st_within)
glimpse(den_in_muk)
```

    ## Rows: 2,486
    ## Columns: 25
    ## $ notifika     <dbl> 4090641, 4092674, 4222794, 4030932, 4273311, 4025693, 404~
    ## $ kes_no       <chr> "2019/32780", "2019/33141", "2019/59670", "2019/20659", "~
    ## $ tarikh_o     <date> 2019-03-17, 2019-03-15, 2019-06-19, 2019-02-13, 2019-07-~
    ## $ tkh_noti     <date> 2019-03-20, 2019-03-21, 2019-06-22, 2019-02-16, 2019-07-~
    ## $ lokaliti     <chr> "KELAIK", "BLAU", "HANIR", "PPRT TANAH PUTIH", "TAMAN WAW~
    ## $ mukim_zo     <chr> "BERTAM", "BERTAM", "BERTAM", "GALAS", "GALAS", "GALAS", ~
    ## $ umur_tah     <dbl> 11, 10, 9, 27, 41, 27, 18, 12, 22, 30, 39, 34, 13, 33, 51~
    ## $ epid_min     <dbl> 12, 11, 25, 7, 29, 6, 8, 30, 8, 5, 26, NA, 7, 6, 27, 2, 2~
    ## $ status_a     <chr> "Hidup", "Hidup", "Hidup", "Hidup", "Hidup", "Hidup", "Hi~
    ## $ jantina      <chr> "Lelaki", "Lelaki", "Lelaki", "Lelaki", "Perempuan", "Per~
    ## $ sub_diag     <chr> "DENGUE FEVER", "DENGUE FEVER", "DENGUE FEVER", "DENGUE F~
    ## $ geometry     <POINT [m]> POINT (404312.2 522488.3), POINT (418189.6 526709.4~
    ## $ within_kel   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
    ## $ year_on      <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 201~
    ## $ month_on     <dbl> 3, 3, 6, 2, 7, 2, 2, 7, 2, 1, 6, 7, 2, 2, 7, 1, 7, 1, 1, ~
    ## $ day_on       <int> 17, 15, 19, 13, 15, 8, 21, 21, 17, 27, 29, 3, 13, 3, 4, 1~
    ## $ epiweek_on   <dbl> 12, 11, 25, 7, 29, 6, 8, 30, 8, 5, 26, 27, 7, 6, 27, 2, 2~
    ## $ NEGERI       <chr> "KELANTAN", "KELANTAN", "KELANTAN", "KELANTAN", "KELANTAN~
    ## $ DAERAH       <chr> "GUA MUSANG", "GUA MUSANG", "GUA MUSANG", "GUA MUSANG", "~
    ## $ MUKIM        <chr> "BERTAM", "BERTAM", "BERTAM", "GALAS", "GALAS", "GALAS", ~
    ## $ LELAKI       <int> 12006, 12006, 12006, 19644, 19644, 19644, 19644, 19644, 1~
    ## $ PEREMPUAN    <dbl> 11135, 11135, 11135, 17311, 17311, 17311, 17311, 17311, 1~
    ## $ JUM_JANTIN   <dbl> 23141, 23141, 23141, 36955, 36955, 36955, 36955, 36955, 3~
    ## $ area_muk     [m^2] 4233714250 [m^2], 4233714250 [m^2], 4233714250 [m^2], 948~
    ## $ area_muk_km2 [m^2] 4233.7143 [m^2], 4233.7143 [m^2], 4233.7143 [m^2], 948.99~

``` r
count_den_mukim_mth_yr <- den_in_muk %>% 
  count(DAERAH, MUKIM, year_on, 
        month_on, JUM_JANTIN) %>% 
  ungroup()
count_den_mukim_mth_yr %>% datatable()
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

Calculate incidence of dengue per 1000 population for

-   each mukim
-   year
-   month

``` r
count_den_muk_m_y_1000 <- count_den_mukim_mth_yr %>% 
  mutate(incidence_den = (n/JUM_JANTIN)*1000)
count_den_muk_m_y_1000 %>% datatable()
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

# Plot incidence of dengue for mukim

join polygon to point to enable plot polygon of Kelantan

``` r
count_den_mukim <- den_in_muk %>% 
  count(DAERAH, MUKIM, JUM_JANTIN) %>% 
  ungroup()
count_den_mukim_1000 <- count_den_mukim %>% 
  mutate(inc_1000 = (n/JUM_JANTIN)*1000)
```

``` r
kel_with_den <- st_join(kel, count_den_mukim_1000)
glimpse(kel_with_den)
```

    ## Rows: 66
    ## Columns: 14
    ## $ NEGERI       <chr> "KELANTAN", "KELANTAN", "KELANTAN", "KELANTAN", "KELANTAN~
    ## $ DAERAH.x     <chr> "BACHOK", "BACHOK", "BACHOK", "BACHOK", "BACHOK", "BACHOK~
    ## $ MUKIM.x      <chr> "BEKLAM", "GUNONG (GUNONG TIMOR)", "MAHLIGAI", "PERUPOK",~
    ## $ LELAKI       <int> 4859, 11100, 4564, 8777, 9227, 14140, 5863, 4929, 18064, ~
    ## $ PEREMPUAN    <dbl> 4813, 10884, 4600, 8614, 8672, 13632, 6634, 5042, 17893, ~
    ## $ JUM_JANTIN.x <dbl> 9672, 21984, 9164, 17391, 17899, 27772, 12497, 9971, 3595~
    ## $ area_muk     [m^2] 23687815 [m^2], 56043029 [m^2], 18933859 [m^2], 17493217 ~
    ## $ area_muk_km2 [m^2] 23.68781 [m^2], 56.04303 [m^2], 18.93386 [m^2], 17.49322 ~
    ## $ DAERAH.y     <chr> "BACHOK", "BACHOK", "BACHOK", "BACHOK", "BACHOK", "BACHOK~
    ## $ MUKIM.y      <chr> "BEKLAM", "GUNONG (GUNONG TIMOR)", "MAHLIGAI", "PERUPOK",~
    ## $ JUM_JANTIN.y <dbl> 9672, 21984, 9164, 17391, 17899, 27772, 12497, 9971, 3595~
    ## $ n            <int> 13, 22, 1, 63, 44, 58, 29, 25, 110, 23, 8, 3, 131, 180, 3~
    ## $ inc_1000     <dbl> 1.3440860, 1.0007278, 0.1091227, 3.6225634, 2.4582379, 2.~
    ## $ geometry     <POLYGON [m]> POLYGON ((485501.8 669698.8..., POLYGON ((487716.~

Plot

``` r
ggplot() + 
  geom_sf(data = kel_with_den, aes(fill = inc_1000)) +
  scale_fill_gradientn(colors = sf.colors(20)) +
  ggtitle('Dengue Incidence per 1000 population') +
  theme_bw()
```

![](dengue_2021_idm_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->
