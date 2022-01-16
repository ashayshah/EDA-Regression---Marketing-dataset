EDA & Regression on Marketing Dataset
================

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

getwd()

``` r
Ch3_marketing <- read.csv('Ch3_marketing.csv')
Ch4_marketing <- read.csv('Ch4_marketing.csv')
```

``` r
library(data.table)
mark = copy(Ch3_marketing) #create a data table 'mark' , to avoid working on the original table
str(mark)
```

    ## 'data.frame':    172 obs. of  7 variables:
    ##  $ google_adwords : num  65.7 39.1 174.8 34.4 78.2 ...
    ##  $ facebook       : num  47.9 55.2 52 62 40.9 ...
    ##  $ twitter        : num  52.5 77.4 68 86.9 30.4 ...
    ##  $ marketing_total: num  166 172 295 183 150 ...
    ##  $ revenues       : num  39.3 38.9 49.5 40.6 40.2 ...
    ##  $ employees      : int  5 7 11 7 9 3 10 6 6 4 ...
    ##  $ pop_density    : chr  "High" "Medium" "Medium" "High" ...

``` r
setDT(mark)
```

``` r
# Cleaning



unique(mark$pop_density) #to check the unique values in the only column that is of character datatype
```

    ## [1] "High"   "Medium" "Low"

``` r
#we see the output as 'high', 'medium' and 'low'... which means 'pop_density' is a ordinal column. So, we convert it into a factor.
mark[,pop_density:=factor(pop_density,levels=c('Low','Medium','High'),ordered=T)]

grep('NA',mark)
```

    ## integer(0)

``` r
#see the output... integer(0) means there are no null values

#the 5 number summary
summary(mark$google_adwords)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   23.65   97.25  169.47  169.87  243.10  321.00

``` r
min(mark$google_adwords) #if you just want the min.
```

    ## [1] 23.65

``` r
max(mark$twitter) #if you just want the max.
```

    ## [1] 122.19

``` r
#the above example was a numerical data column. But we can also use summary() on a categorical column.
mark[,summary(pop_density)]
```

    ##    Low Medium   High 
    ##     68     52     52

``` r
#the output shows we have 68 iterations of value 'Low', 52 iterations of 'Medium' and 52 of 'High'
```

``` r
# EDA




library(ggplot2)



#let's map the pop_density column (categorical data) on a bar graph
ggplot(mark,aes(x=pop_density)) + geom_bar()
```

![](EDA---Regression-R-Markdown_files/figure-gfm/EDA-1.png)<!-- -->

``` r
#now lets plot numerical data. lets start with plotting google_adwords on box plot.
ggplot(mark,aes(x=google_adwords)) + geom_boxplot()
```

![](EDA---Regression-R-Markdown_files/figure-gfm/EDA-2.png)<!-- -->

``` r
#our distribution is almost normally distributed.


#now plot the same column on a histogram.
ggplot(mark,aes(x=google_adwords)) + geom_histogram(fill='yellow',color='black',binwidth=50)
```

![](EDA---Regression-R-Markdown_files/figure-gfm/EDA-3.png)<!-- -->

``` r
#very few values in the google_adwords column ranging from 0 to 50. And there are more than #
#35 values (see y-axis) in the google_adwords column that range from 250 to 300.

#our data is skewing towards the max value side
ggplot(mark,aes(y=twitter)) + geom_boxplot(fill='yellow')
```

![](EDA---Regression-R-Markdown_files/figure-gfm/EDA-4.png)<!-- -->

``` r
ggplot(mark,aes(x=twitter)) + geom_histogram(fill='green',color='black',bins=10)
```

![](EDA---Regression-R-Markdown_files/figure-gfm/EDA-5.png)<!-- -->

``` r
#this histogram in the output clearly shows the 'distribution' is uneven.. and towards the right, we see the anomaly outliers around 120 range.

#lets find the relationship between employees and pop_density.. pop_density is categorical column, employees is numerical value column.#
#lets convert employee into categorical.. so first, cut the employees into 2 groups using cut().
mark[,empFactor:=cut(employees,2)]
str(mark)
```

    ## Classes 'data.table' and 'data.frame':   172 obs. of  8 variables:
    ##  $ google_adwords : num  65.7 39.1 174.8 34.4 78.2 ...
    ##  $ facebook       : num  47.9 55.2 52 62 40.9 ...
    ##  $ twitter        : num  52.5 77.4 68 86.9 30.4 ...
    ##  $ marketing_total: num  166 172 295 183 150 ...
    ##  $ revenues       : num  39.3 38.9 49.5 40.6 40.2 ...
    ##  $ employees      : int  5 7 11 7 9 3 10 6 6 4 ...
    ##  $ pop_density    : Ord.factor w/ 3 levels "Low"<"Medium"<..: 3 2 2 3 1 3 1 3 2 1 ...
    ##  $ empFactor      : Factor w/ 2 levels "(2.99,7.5]","(7.5,12]": 1 1 2 1 2 1 2 1 1 1 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
table(mark$empFactor) #78 values in the range (2.99 to 7.5) , and 94 in (7.5 to 12)
```

    ## 
    ## (2.99,7.5]   (7.5,12] 
    ##         78         94

``` r
#now we can find the relationship between empFactor and pop_density, as both are categorical now.
table(mark$empFactor, mark$pop_density)
```

    ##             
    ##              Low Medium High
    ##   (2.99,7.5]  35     21   22
    ##   (7.5,12]    33     31   30

``` r
#companies where the employees are a bit lower (2.99 to 7.5), they are focusing more on the 'Low' population densities (35).#
# and companies with more employees (7.5 to 12) are focusing equally on low, medium and high population densities (33,31,30)

#now lets compare numerical to numerical.. lets find the relation between google_adwords and revenues. We do this using Scatter Plots.
ggplot(mark,aes(x=revenues,y=google_adwords)) + geom_point(color='purple')
```

![](EDA---Regression-R-Markdown_files/figure-gfm/EDA-6.png)<!-- -->

``` r
#we see a strong correlation in the start, and less strong towards the end.

# calculating the correlation
mark[,cor(google_adwords,revenues)]
```

    ## [1] 0.7662461

``` r
#now, to test whether the correlation is significant, means whether it is due to random chance, or whether it actually is significant, ##
##we use the cor.test() function to check the significance..
cor.test(mark$google_adwords,mark$revenues)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  mark$google_adwords and mark$revenues
    ## t = 15.548, df = 170, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.6964662 0.8216704
    ## sample estimates:
    ##       cor 
    ## 0.7662461

``` r
# t is 15.5, which is a large number, But p-value is 2.2e-16, which is almost 0.0000002.. which is almost 0.
# Hence, our alternative hypothesis is correct.. and the correlation is statistically significant,

#lets test the correlation between twitter & revenue columns.
cor.test(mark$twitter,mark$revenues)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  mark$twitter and mark$revenues
    ## t = 3.6516, df = 170, p-value = 0.0003467
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.1250993 0.4030549
    ## sample estimates:
    ##       cor 
    ## 0.2696854

``` r
#the correlation is 0.29... the t-score is 3.6, p-value is 0.003 (which is not very very less as the previous example above). ##
##The question to ask by seeing this correlation (0.29 is less) is why are they spending money on twitter advertising if revenue's ###
###relation with twitter is less?

#pairs() function creates a scatterplot of every column against every column in the datatable.
pairs(mark)
```

![](EDA---Regression-R-Markdown_files/figure-gfm/EDA-7.png)<!-- -->

``` r
#---#
```

``` r
advert = copy(Ch4_marketing)
setDT(advert)
str(advert)
```

    ## Classes 'data.table' and 'data.frame':   172 obs. of  5 variables:
    ##  $ google_adwords : num  65.7 39.1 174.8 34.4 78.2 ...
    ##  $ facebook       : num  47.9 55.2 52 62 40.9 ...
    ##  $ twitter        : num  52.5 77.4 68 86.9 30.4 ...
    ##  $ marketing_total: num  166 172 295 183 150 ...
    ##  $ revenues       : num  39.3 38.9 49.5 40.6 40.2 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
#say we want to compare the distributions of 3 columns : google, twitter and facebook
#First lets create a vector with all the data from the 3 columns:
g=advert[,google_adwords]
f=advert[,facebook]
tw=advert[,twitter]
nv=c(g,f,tw)
nv
```

    ##   [1]  65.66  39.10 174.81  34.36  78.21  34.19 225.71  90.03 238.40  43.53
    ##  [11] 118.24 224.64 219.51  89.79 306.09  91.98 242.72 263.64  35.16 253.04
    ##  [21]  88.14 163.68 263.96 268.98 314.07 134.90 284.99 117.68 316.09 288.58
    ##  [31]  97.37  66.60 250.75 223.10 202.09 317.58 231.76  45.16 199.47 262.08
    ##  [41] 224.85 124.23 241.18 205.77 285.71  32.13 155.66 233.44 235.13  77.65
    ##  [51] 283.94 128.03 153.47  90.01  51.29 159.30 258.91 239.73 133.73  48.95
    ##  [61] 155.09 234.76  50.01 142.13  29.26 137.11 262.45  99.96  88.29 238.93
    ##  [71] 214.87 101.48 132.43 109.94 132.43 159.85 243.05 272.93 132.14 189.32
    ##  [81] 219.94 313.99 157.30 243.98 321.00 300.92 262.48 158.05  46.02 110.70
    ##  [91]  34.08 275.11 249.60 267.13 200.45 234.48 100.69  97.27 164.18  99.93
    ## [101]  43.28 163.07 245.19 149.35 253.23 107.99  28.01 102.85 246.07  83.09
    ## [111]  26.83 289.62  30.20  59.22  68.64  44.99 298.01  63.02 207.32  97.18
    ## [121] 219.94 243.27 127.29 116.71 164.88 262.58 266.08  65.45 301.60 144.47
    ## [131] 194.62 207.64  23.65 117.70 175.60  35.19 154.93 195.48 111.90 186.35
    ## [141] 141.28 258.01  38.87 227.91 239.81 306.77 185.03  44.13 188.43 247.75
    ## [151] 300.50 193.40 298.30 188.07 179.40 239.07  79.21 307.43 275.04 225.79
    ## [161] 160.79 216.67 308.42  43.46  64.96  97.69  36.95 188.45 174.07  59.86
    ## [171] 117.91 308.78  47.86  55.20  52.01  61.96  40.91  15.09  15.91  17.13
    ## [181]  35.10  42.23  15.74  40.84  59.21  45.99  51.69  30.68  39.42  18.74
    ## [191]  25.26  29.04  25.84  37.48  27.96  34.68  36.87  26.80  26.79  10.78
    ## [201]  16.89  52.88  59.47  37.60  47.85  30.30  45.89  39.08  20.66  33.16
    ## [211]  34.27  51.08  15.55  20.83  53.88  56.77  39.21  40.33  26.06  59.64
    ## [221]  41.33  13.55  52.74  42.33  52.57  17.71  31.79  21.90  36.41  54.23
    ## [231]  25.63  42.55  18.79  33.36  11.51  37.53  41.16  16.21  14.15  32.36
    ## [241]  51.79  55.83  27.47  40.08  49.73  34.54  57.83  17.85  46.25  45.93
    ## [251]  26.14  45.02  13.24  53.99  51.20  13.28  48.30  18.22  45.98  53.95
    ## [261]  19.42   8.00   8.78  34.01  19.40  50.83  27.55  32.88  56.69  44.57
    ## [271]  26.68  11.73  27.28  35.97  10.99  48.25  43.43  19.99  46.51  10.05
    ## [281]  62.17  22.89  53.13  14.72  36.40  48.32  54.74  45.79  40.61  33.32
    ## [291]  53.72  28.18  49.04  43.37  15.79  22.71  13.88  17.18  59.28  33.95
    ## [301]  22.20  19.27  50.42  28.34  18.55  54.70  14.50  47.79  29.03  28.48
    ## [311]  49.40  47.05  26.18  14.31  45.97  13.71  35.41  20.47  28.83  32.03
    ## [321]  14.53  16.15  59.90  18.40  11.30  19.87  12.80  13.37  16.11  50.23
    ## [331]  29.94  53.29  10.79  41.67  23.72  24.26  53.96  20.39  11.25  51.05
    ## [341]  47.37  12.76  16.01  54.58  52.46  77.40  68.01  86.86  30.41  12.79
    ## [351]  33.31  34.33  13.90  71.83  14.14  52.74  63.21 122.19  66.69  27.28
    ## [361]  63.92  35.94  57.76  37.14  30.34  19.58  32.96  29.28  50.57  46.80
    ## [371]   5.89  15.58  20.09  12.88  54.57  44.80  40.95  38.40  49.99  11.98
    ## [381]  37.46  49.56  42.07  26.88  45.85  13.63  50.58  68.07  25.11  52.43
    ## [391]  22.26  46.54  19.93  31.75  63.54  19.93  37.47   8.11   8.19  16.40
    ## [401]  18.71  36.33  41.83  27.65  43.19  20.66  29.41  22.03  19.46  30.41
    ## [411]  45.75  43.36  41.69  45.43  73.57  27.38  71.13  81.24  60.23  21.05
    ## [421]  70.55  80.53  21.84  65.12  14.44  61.69  54.20  57.58 111.70  28.32
    ## [431]  15.78  65.35  36.92  29.70  32.78  11.41  66.50  34.83  13.35  21.78
    ## [441]  43.19  61.07  36.78  24.53  32.38  54.17  22.99  24.85  84.13  32.89
    ## [451]  57.01  18.05  15.17  52.79  21.03  53.62  10.10  74.12  15.04  14.89
    ## [461]  70.21  26.72  10.32  22.88  88.04  46.87  43.29  45.61  19.78  17.38
    ## [471]  53.38  27.55  44.10  58.37  47.22  15.54  11.45  60.50  36.30  54.89
    ## [481]  44.03  39.88  61.70  16.45  15.68  94.51  28.77  26.71  68.21  15.07
    ## [491]  54.13  27.73  19.03  24.65  51.60  44.60  31.50  26.27  17.30  34.17
    ## [501]  38.91  77.83  37.44  26.59  34.09  29.97  12.32  34.36  17.46  14.39
    ## [511]  37.55  11.45  16.57  21.66  18.01  77.58

``` r
#basically we created vector nv, with data of all 3 columns stacked one below the other in it.

#Now we need to create a vector that labels each piece of data according to what column belongs to (our categorical data):
nc=c(rep('g',NROW(g)),rep('f',NROW(f)),rep('tw',NROW(tw)))
advert2=data.table(nv ,nc)

ggplot(advert2, aes(x=nc ,y=nv)) + geom_boxplot (fill='pink')
```

![](EDA---Regression-R-Markdown_files/figure-gfm/Regression-1.png)<!-- -->

``` r
#the plot shows google is more expensive. There are 2 outlier prices in twitter marketing.
```

``` r
#regression




model1=lm(revenues~marketing_total , data = advert)
#we made a model where the marketing total predicts revenue
model1
```

    ## 
    ## Call:
    ## lm(formula = revenues ~ marketing_total, data = advert)
    ## 
    ## Coefficients:
    ##     (Intercept)  marketing_total  
    ##        32.00670          0.05193

``` r
#the 32.0067 in y-intercept, means that if you dont spend any money on marketing (basically X is zero dollars),
#then we can say the revenue would be $32007 (unit of the data in thousands)..
#also, revenue increases by $51.93 (slope) for every $1000 increase in total marketing

#so by using this data,this is how we can make a prediction model :
#Revenue = 32.0067 + (0.05193 * marketing_total)

# L.I.N.E assumptions must be satisfied by the data, in order to use linear regression.
#To find whether the 'N' , i.e. normal distribution of residuals is satisfied by the data, we run these series of tests :-
# Z-score is calculated by subtracting each data point in the column by it's mean, and dividing that number by the SD of that column.


str(model1)
```

    ## List of 12
    ##  $ coefficients : Named num [1:2] 32.0067 0.0519
    ##   ..- attr(*, "names")= chr [1:2] "(Intercept)" "marketing_total"
    ##  $ residuals    : Named num [1:172] -1.366 -2.023 2.193 -0.959 0.438 ...
    ##   ..- attr(*, "names")= chr [1:172] "1" "2" "3" "4" ...
    ##  $ effects      : Named num [1:172] -585.067 65.094 2.206 -0.783 0.663 ...
    ##   ..- attr(*, "names")= chr [1:172] "(Intercept)" "marketing_total" "" "" ...
    ##  $ rank         : int 2
    ##  $ fitted.values: Named num [1:172] 40.6 40.9 47.3 41.5 39.8 ...
    ##   ..- attr(*, "names")= chr [1:172] "1" "2" "3" "4" ...
    ##  $ assign       : int [1:2] 0 1
    ##  $ qr           :List of 5
    ##   ..$ qr   : num [1:172, 1:2] -13.1149 0.0762 0.0762 0.0762 0.0762 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:172] "1" "2" "3" "4" ...
    ##   .. .. ..$ : chr [1:2] "(Intercept)" "marketing_total"
    ##   .. ..- attr(*, "assign")= int [1:2] 0 1
    ##   ..$ qraux: num [1:2] 1.08 1.05
    ##   ..$ pivot: int [1:2] 1 2
    ##   ..$ tol  : num 1e-07
    ##   ..$ rank : int 2
    ##   ..- attr(*, "class")= chr "qr"
    ##  $ df.residual  : int 170
    ##  $ xlevels      : Named list()
    ##  $ call         : language lm(formula = revenues ~ marketing_total, data = advert)
    ##  $ terms        :Classes 'terms', 'formula'  language revenues ~ marketing_total
    ##   .. ..- attr(*, "variables")= language list(revenues, marketing_total)
    ##   .. ..- attr(*, "factors")= int [1:2, 1] 0 1
    ##   .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. ..$ : chr [1:2] "revenues" "marketing_total"
    ##   .. .. .. ..$ : chr "marketing_total"
    ##   .. ..- attr(*, "term.labels")= chr "marketing_total"
    ##   .. ..- attr(*, "order")= int 1
    ##   .. ..- attr(*, "intercept")= int 1
    ##   .. ..- attr(*, "response")= int 1
    ##   .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. ..- attr(*, "predvars")= language list(revenues, marketing_total)
    ##   .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
    ##   .. .. ..- attr(*, "names")= chr [1:2] "revenues" "marketing_total"
    ##  $ model        :'data.frame':   172 obs. of  2 variables:
    ##   ..$ revenues       : num [1:172] 39.3 38.9 49.5 40.6 40.2 ...
    ##   ..$ marketing_total: num [1:172] 166 172 295 183 150 ...
    ##   ..- attr(*, "terms")=Classes 'terms', 'formula'  language revenues ~ marketing_total
    ##   .. .. ..- attr(*, "variables")= language list(revenues, marketing_total)
    ##   .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
    ##   .. .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. .. ..$ : chr [1:2] "revenues" "marketing_total"
    ##   .. .. .. .. ..$ : chr "marketing_total"
    ##   .. .. ..- attr(*, "term.labels")= chr "marketing_total"
    ##   .. .. ..- attr(*, "order")= int 1
    ##   .. .. ..- attr(*, "intercept")= int 1
    ##   .. .. ..- attr(*, "response")= int 1
    ##   .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
    ##   .. .. ..- attr(*, "predvars")= language list(revenues, marketing_total)
    ##   .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
    ##   .. .. .. ..- attr(*, "names")= chr [1:2] "revenues" "marketing_total"
    ##  - attr(*, "class")= chr "lm"

``` r
model1$residuals #to find the residual values of our model
```

    ##           1           2           3           4           5           6 
    ## -1.36586950 -2.02290331  2.19308039 -0.95904809  0.43836236  2.86007165 
    ##           7           8           9          10          11          12 
    ## -2.07353275  0.87587142  1.86891277 -3.56018529 -1.55841777 -1.29153864 
    ##          13          14          15          16          17          18 
    ##  4.44722505 -3.21281711  2.74100638  1.98707147  0.04275833 -2.09673154 
    ##          19          20          21          22          23          24 
    ## -2.88366387 -0.64346763  3.73891238  0.01049912 -1.41738570 -2.51593334 
    ##          25          26          27          28          29          30 
    ## -2.58670406 -1.23389007 -4.01297770 -0.30654750 -4.45126387  4.37278631 
    ##          31          32          33          34          35          36 
    ##  2.08499657  1.55588472  2.31081605 -2.25957478  2.41002337  1.23020178 
    ##          37          38          39          40          41          42 
    ## -1.59987112 -2.38737505  0.64076974  3.41537084 -2.72136889  1.98269283 
    ##          43          44          45          46          47          48 
    ##  5.12456851  2.89506120  0.72660025 -0.46210738 -2.23917104  4.49718102 
    ##          49          50          51          52          53          54 
    ##  3.13207117  1.55863486  1.75028434  5.14173680  3.41807606 -0.01163075 
    ##          55          56          57          58          59          60 
    ##  0.24374520 -1.16786410 -0.20395618  3.77167847  1.57571083  0.45596501 
    ##          61          62          63          64          65          66 
    ##  1.11107850 -0.94274923  0.38140106  1.04974634  0.18592649 -1.73760904 
    ##          67          68          69          70          71          72 
    ## -6.09600536  2.53041932 -0.25582822  5.15758149 -3.84158268  4.10042070 
    ##          73          74          75          76          77          78 
    ##  0.27021055 -1.48810724  2.01561172  2.12241756  1.75665759 -0.81661460 
    ##          79          80          81          82          83          84 
    ##  2.57985461  3.06264918 -3.12535208  3.07097564  1.35155878 -7.37601866 
    ##          85          86          87          88          89          90 
    ## -0.88453932 -6.82994225  3.83584885  0.64079910 -1.40214728 -3.01295789 
    ##          91          92          93          94          95          96 
    ## -1.95460472 -1.44149460 -4.52887155  4.60327624  2.01024267  1.45855633 
    ##          97          98          99         100         101         102 
    ##  2.36790681 -0.07360703  1.05218910  1.55109506  0.82773391 -0.18563513 
    ##         103         104         105         106         107         108 
    ## -6.01371166  5.59170055 -0.65073542 -1.27051237 -4.32691586  0.34320295 
    ##         109         110         111         112         113         114 
    ##  9.36894991  0.63853851 -1.82100456 -5.77519663 -0.78964944 -0.62011603 
    ##         115         116         117         118         119         120 
    ##  0.44529341 -0.90403223 -0.42682425 -1.07707733  4.72185374  2.67535075 
    ##         121         122         123         124         125         126 
    ##  2.59361662  1.24446713 -0.89470156 -2.00511672  0.26332265 -4.05687557 
    ##         127         128         129         130         131         132 
    ##  4.30571900 -0.04908109 -6.41137077 -0.77064360  2.83653789 -1.92787514 
    ##         133         134         135         136         137         138 
    ## -4.34268627  2.69904262 -0.16341896 -0.67614564 -0.01598605 -0.62764027 
    ##         139         140         141         142         143         144 
    ##  3.61313960  3.56884627  2.16302907 -7.94580675 -1.23634839 -4.93080143 
    ##         145         146         147         148         149         150 
    ## -0.63066843 -4.61250796 -3.19314557  2.42840137 -4.10441212 -2.44080556 
    ##         151         152         153         154         155         156 
    ##  4.89856086 -2.72128860 -8.61967389 -1.39898435 -1.88581906 -6.42006333 
    ##         157         158         159         160         161         162 
    ##  0.43287587 -0.89125107 -3.24822015  3.21017016 -3.39693103  3.59165739 
    ##         163         164         165         166         167         168 
    ## -3.87415529  1.85239284  4.87122855  0.90427089 -3.10960679  3.91172439 
    ##         169         170         171         172 
    ##  5.00368549  0.05743895  1.21373278  3.47573595

``` r
#now, to plot the Q-Q plot, we need the residuals in a data table, and the Z-scores are calculated by ggplot() itself...
resdf = data.table('res'=model1$residuals) #data table 'resdf' with column 'res' which basically are our residuals

#first lets plot the histogram plot of residuals
ggplot(resdf ,aes(x=res)) + geom_histogram(bins=10,fill='purple',color='black')
```

![](EDA---Regression-R-Markdown_files/figure-gfm/Simple%20Regression-1.png)<!-- -->

``` r
#the shape looks like normal distribution, because majority residual values are near 0.

#and now we can check the mean
mean(model1$residuals)
```

    ## [1] 4.05832e-17

``` r
#the value is 4.05832 * e ^ -17, in the console... which basically is : 0.000000000000000000040583... which is essentially zero.

#now, let's also plot the Q-Q plot
ggplot(resdf ,aes(sample=res)) + stat_qq(color='blue') + stat_qq_line()
```

![](EDA---Regression-R-Markdown_files/figure-gfm/Simple%20Regression-2.png)<!-- -->

``` r
#We see that majority of the points lie on the straight line, and are clustered in the center... so we can say it is normally distributed with ##
# mean 0.

#To find whether the 'E' (equal variance) of residuals is satisfied by the data, we have to show that there is no pattern between to the residual errors.

#just like residuals, predicted Y values are in 'model1' which we created. We can add a 'pred' column in the resdf table, to plot it further.
resdf[,pred:= model1$fitted.values]

ggplot(resdf ,aes(x=pred ,y=res)) + geom_point(color='purple') + geom_smooth(method='lm')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](EDA---Regression-R-Markdown_files/figure-gfm/Simple%20Regression-3.png)<!-- -->

``` r
#there is a horizontal line in the output, and it means, as the predicted Y values increase or decrease, it has zero effects on the residuals.
# which shows there is no relationship between the 2 variables.
```

``` r
#PREDICTING OUTPUTS


#if you predict outputs using SLR, it works best if you use the inputs within the range of the existing data. Eg:
summary(advert$marketing_total)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   53.65  158.41  245.56  242.72  322.62  481.00

``` r
#so, predicted output (revenue) will be best if it is in the range of values [53.65 to 481] of the marketing_total data (input variable)

#Problem: predict the revenues if you spend $460,000 in marketing.
#firstly, we should check there are how many data points more than 460 in the marketing column
advert[marketing_total >460, marketing_total]
```

    ## [1] 481

``` r
#only 1 value... i.e. 481

#we can create 3 values, 460, 465 and 470 as inputs.
##we create a data table with these inputs, and then use the 'predict.lm()' function to estimate the outputs.
newrev = data.table(marketing_total=seq (460 ,470 ,5))
predict.lm(model1 ,newrev ,interval = 'predict')
```

    ##        fit      lwr      upr
    ## 1 55.89403 49.75781 62.03025
    ## 2 56.15368 50.01331 62.29404
    ## 3 56.41332 50.26873 62.55791

``` r
#we in parameters, pass the regression model (model1), the table which we want as input, and interval = 'predict' says we want to predict
# based on the model and table passed.

#For the value of $460,000 you get an estimate of revenue of $55,894.


#now, let's sample a smaller table 'liladvert' from our main data table 'advert', using the sample() function
liladvert = advert[sample (.N,.3*.N)]

#now, let's go ahead and create the same regression model using lm() function, but now on the 'liladvert' table.
set.seed (4510)
liladvert = advert[sample (.N,.3*.N)]
samp_model = lm(revenues~marketing_total ,data=liladvert)
samp_model
```

    ## 
    ## Call:
    ## lm(formula = revenues ~ marketing_total, data = liladvert)
    ## 
    ## Coefficients:
    ##     (Intercept)  marketing_total  
    ##        32.97301          0.04662

``` r
#Multiple regression:




model2 = lm(revenues ~ google_adwords + facebook + twitter , data=advert)

#create a new data table to put the residuals and predicted values in the table in order to plot graphs
resdf2 = data.table(res = model2$residuals, pred = model2$fitted.values)

plot(advert) #we do this to check individual variables plot against revenue variable to check linearity
```

![](EDA---Regression-R-Markdown_files/figure-gfm/Outputs-1.png)<!-- -->

``` r
#test normality
ggplot(resdf2 ,aes(x=res)) + geom_histogram(bins=10,fill='blue',color='white')
```

![](EDA---Regression-R-Markdown_files/figure-gfm/Outputs-2.png)<!-- -->

``` r
ggplot(resdf2 ,aes(sample=res)) + stat_qq(color="blue") + stat_qq_line()
```

![](EDA---Regression-R-Markdown_files/figure-gfm/Outputs-3.png)<!-- -->

``` r
#test equal variance
ggplot(resdf2 ,aes(x=pred ,y=res)) + geom_point(color='purple') + geom_smooth(method = 'lm')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](EDA---Regression-R-Markdown_files/figure-gfm/Outputs-4.png)<!-- -->

``` r
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = revenues ~ google_adwords + facebook + twitter, 
    ##     data = advert)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.9971 -1.4566  0.2791  1.7428  4.3711 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    29.545988   0.533523   55.38   <2e-16 ***
    ## google_adwords  0.048384   0.001947   24.85   <2e-16 ***
    ## facebook        0.197651   0.011871   16.65   <2e-16 ***
    ## twitter         0.003889   0.008270    0.47    0.639    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.214 on 168 degrees of freedom
    ## Multiple R-squared:  0.8585, Adjusted R-squared:  0.856 
    ## F-statistic: 339.8 on 3 and 168 DF,  p-value: < 2.2e-16

``` r
#we see that twitter isn't statistically significant and isnt contributing a lot to the rev, as it's p-value is > 0.05
```
