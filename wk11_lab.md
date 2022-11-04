pm566_wk11_lab
================
Yiping Li
2022-11-04

``` r
library(RSQLite)
library(DBI)
```

``` r
# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")

# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)

dbListTables(con)
```

    ## [1] "actor"    "customer" "payment"  "rental"

``` sql
PRAGMA table_info(actor)
```

| cid | name        | type    | notnull | dflt_value |  pk |
|:----|:------------|:--------|--------:|:-----------|----:|
| 0   | actor_id    | INTEGER |       0 | NA         |   0 |
| 1   | first_name  | TEXT    |       0 | NA         |   0 |
| 2   | last_name   | TEXT    |       0 | NA         |   0 |
| 3   | last_update | TEXT    |       0 | NA         |   0 |

4 records

An alternative would be:

``` r
dbGetQuery(con, "
           pragma table_info(actor)
           ")
```

    ##   cid        name    type notnull dflt_value pk
    ## 1   0    actor_id INTEGER       0         NA  0
    ## 2   1  first_name    TEXT       0         NA  0
    ## 3   2   last_name    TEXT       0         NA  0
    ## 4   3 last_update    TEXT       0         NA  0

\#exercise1: retrive the actor ID, first name and last name for all
actors using the actor table. Sort by last name and then by first name.

``` r
dbGetQuery(con, "
SELECT  actor_id, first_name, last_name
FROM  actor
ORDER by last_name, first_name
           ")
```

    ##     actor_id  first_name    last_name
    ## 1         58   CHRISTIAN       AKROYD
    ## 2        182      DEBBIE       AKROYD
    ## 3         92     KIRSTEN       AKROYD
    ## 4        118        CUBA        ALLEN
    ## 5        145         KIM        ALLEN
    ## 6        194       MERYL        ALLEN
    ## 7         76    ANGELINA      ASTAIRE
    ## 8        112     RUSSELL       BACALL
    ## 9        190      AUDREY       BAILEY
    ## 10        67     JESSICA       BAILEY
    ## 11       115    HARRISON         BALE
    ## 12       187       RENEE         BALL
    ## 13        47       JULIA    BARRYMORE
    ## 14       158      VIVIEN     BASINGER
    ## 15       174     MICHAEL       BENING
    ## 16       124    SCARLETT       BENING
    ## 17        14      VIVIEN       BERGEN
    ## 18       121        LIZA      BERGMAN
    ## 19        91 CHRISTOPHER        BERRY
    ## 20        60       HENRY        BERRY
    ## 21        12        KARL        BERRY
    ## 22       189        CUBA        BIRCH
    ## 23        25       KEVIN        BLOOM
    ## 24       185     MICHAEL       BOLGER
    ## 25        37         VAL       BOLGER
    ## 26        98       CHRIS      BRIDGES
    ## 27        39      GOLDIE        BRODY
    ## 28       159       LAURA        BRODY
    ## 29       167    LAURENCE      BULLOCK
    ## 30        40      JOHNNY         CAGE
    ## 31        11        ZERO         CAGE
    ## 32       181     MATTHEW       CARREY
    ## 33        86        GREG      CHAPLIN
    ## 34         3          ED        CHASE
    ## 35       176         JON        CHASE
    ## 36       183     RUSSELL        CLOSE
    ## 37        16        FRED      COSTNER
    ## 38       129       DARYL     CRAWFORD
    ## 39        26         RIP     CRAWFORD
    ## 40        49        ANNE       CRONYN
    ## 41       104    PENELOPE       CRONYN
    ## 42       105      SIDNEY        CROWE
    ## 43        57        JUDE       CRUISE
    ## 44       201         TOM       CRUISE
    ## 45       203         TOM       CRUISE
    ## 46       205         TOM       CRUISE
    ## 47       207         TOM       CRUISE
    ## 48        80       RALPH         CRUZ
    ## 49        81    SCARLETT        DAMON
    ## 50         4    JENNIFER        DAVIS
    ## 51       101       SUSAN        DAVIS
    ## 52       110       SUSAN        DAVIS
    ## 53        48     FRANCES    DAY-LEWIS
    ## 54        35        JUDY         DEAN
    ## 55       143       RIVER         DEAN
    ## 56       148       EMILY          DEE
    ## 57       138     LUCILLE          DEE
    ## 58       107        GINA    DEGENERES
    ## 59        41       JODIE    DEGENERES
    ## 60       166        NICK    DEGENERES
    ## 61        89    CHARLIZE        DENCH
    ## 62       123    JULIANNE        DENCH
    ## 63       160       CHRIS         DEPP
    ## 64       100     SPENCER         DEPP
    ## 65       109   SYLVESTER         DERN
    ## 66       173        ALAN     DREYFUSS
    ## 67        36        BURT      DUKAKIS
    ## 68       188        ROCK      DUKAKIS
    ## 69       106     GROUCHO        DUNST
    ## 70        19         BOB      FAWCETT
    ## 71       199       JULIA      FAWCETT
    ## 72        10   CHRISTIAN        GABLE
    ## 73       165          AL      GARLAND
    ## 74       184    HUMPHREY      GARLAND
    ## 75       127       KEVIN      GARLAND
    ## 76       154       MERYL       GIBSON
    ## 77        46      PARKER     GOLDBERG
    ## 78       139        EWAN      GOODING
    ## 79       191     GREGORY      GOODING
    ## 80        71        ADAM        GRANT
    ## 81       179          ED      GUINESS
    ## 82         1    PENELOPE      GUINESS
    ## 83        90        SEAN      GUINESS
    ## 84        32         TIM      HACKMAN
    ## 85       175     WILLIAM      HACKMAN
    ## 86       202         TOM        HANKS
    ## 87       204         TOM        HANKS
    ## 88       206         TOM        HANKS
    ## 89       208         TOM        HANKS
    ## 90       152         BEN       HARRIS
    ## 91       141        CATE       HARRIS
    ## 92        56         DAN       HARRIS
    ## 93        97         MEG        HAWKE
    ## 94       151    GEOFFREY       HESTON
    ## 95       169     KENNETH      HOFFMAN
    ## 96        79         MAE      HOFFMAN
    ## 97        28       WOODY      HOFFMAN
    ## 98       161      HARVEY         HOPE
    ## 99       134        GENE      HOPKINS
    ## 100      113      MORGAN      HOPKINS
    ## 101       50     NATALIE      HOPKINS
    ## 102      132        ADAM       HOPPER
    ## 103      170        MENA       HOPPER
    ## 104       65      ANGELA       HUDSON
    ## 105       52      CARMEN         HUNT
    ## 106      140      WHOOPI         HURT
    ## 107      131        JANE      JACKMAN
    ## 108      119      WARREN      JACKMAN
    ## 109      146      ALBERT    JOHANSSON
    ## 110        8     MATTHEW    JOHANSSON
    ## 111       64         RAY    JOHANSSON
    ## 112       82       WOODY        JOLIE
    ## 113       43        KIRK     JOVOVICH
    ## 114      130       GRETA       KEITEL
    ## 115      198        MARY       KEITEL
    ## 116       74       MILLA       KEITEL
    ## 117       55         FAY       KILMER
    ## 118      153      MINNIE       KILMER
    ## 119      162       OPRAH       KILMER
    ## 120       45       REESE       KILMER
    ## 121       23      SANDRA       KILMER
    ## 122      103     MATTHEW        LEIGH
    ## 123        5      JOHNNY LOLLOBRIGIDA
    ## 124      157       GRETA       MALDEN
    ## 125      136          ED    MANSFIELD
    ## 126       22       ELVIS         MARX
    ## 127       77        CARY  MCCONAUGHEY
    ## 128       70    MICHELLE  MCCONAUGHEY
    ## 129      114      MORGAN    MCDORMAND
    ## 130      177        GENE     MCKELLEN
    ## 131       38         TOM     MCKELLEN
    ## 132      128        CATE      MCQUEEN
    ## 133       27       JULIA      MCQUEEN
    ## 134       42         TOM      MIRANDA
    ## 135      178        LISA       MONROE
    ## 136      120    PENELOPE       MONROE
    ## 137        7       GRACE       MOSTEL
    ## 138       99         JIM       MOSTEL
    ## 139       61   CHRISTIAN       NEESON
    ## 140       62       JAYNE       NEESON
    ## 141        6       BETTE    NICHOLSON
    ## 142      125      ALBERT        NOLTE
    ## 143      150       JAYNE        NOLTE
    ## 144      122       SALMA        NOLTE
    ## 145      108      WARREN        NOLTE
    ## 146       34      AUDREY      OLIVIER
    ## 147       15        CUBA      OLIVIER
    ## 148       69     KENNETH      PALTROW
    ## 149       21     KIRSTEN      PALTROW
    ## 150       33       MILLA         PECK
    ## 151       30      SANDRA         PECK
    ## 152       87     SPENCER         PECK
    ## 153       73        GARY         PENN
    ## 154      133     RICHARD         PENN
    ## 155       88     KENNETH        PESCI
    ## 156      171     OLYMPIA     PFEIFFER
    ## 157       51        GARY      PHOENIX
    ## 158       54    PENELOPE      PINKETT
    ## 159       84       JAMES         PITT
    ## 160       75        BURT        POSEY
    ## 161       93       ELLEN      PRESLEY
    ## 162      135        RITA     REYNOLDS
    ## 163      142        JADA        RYDER
    ## 164      195       JAYNE  SILVERSTONE
    ## 165      180        JEFF  SILVERSTONE
    ## 166       78     GROUCHO      SINATRA
    ## 167       31       SISSY     SOBIESKI
    ## 168       44        NICK     STALLONE
    ## 169       24     CAMERON       STREEP
    ## 170      116         DAN       STREEP
    ## 171      192        JOHN       SUVARI
    ## 172        9         JOE        SWANK
    ## 173      155         IAN        TANDY
    ## 174       66        MARY        TANDY
    ## 175       59      DUSTIN       TAUTOU
    ## 176      193        BURT       TEMPLE
    ## 177       53        MENA       TEMPLE
    ## 178      149     RUSSELL       TEMPLE
    ## 179      200       THORA       TEMPLE
    ## 180      126     FRANCES        TOMEI
    ## 181       18         DAN         TORN
    ## 182       94     KENNETH         TORN
    ## 183      102      WALTER         TORN
    ## 184       20     LUCILLE        TRACY
    ## 185      117       RENEE        TRACY
    ## 186       17       HELEN       VOIGHT
    ## 187       95       DARYL     WAHLBERG
    ## 188        2        NICK     WAHLBERG
    ## 189      196        BELA       WALKEN
    ## 190       29        ALEC        WAYNE
    ## 191      163 CHRISTOPHER         WEST
    ## 192      197       REESE         WEST
    ## 193      172     GROUCHO     WILLIAMS
    ## 194      137      MORGAN     WILLIAMS
    ## 195       72        SEAN     WILLIAMS
    ## 196       83         BEN       WILLIS
    ## 197       96        GENE       WILLIS
    ## 198      164    HUMPHREY       WILLIS
    ## 199      168        WILL       WILSON
    ## 200      147         FAY      WINSLET
    ## 201       68         RIP      WINSLET
    ## 202      144      ANGELA  WITHERSPOON
    ## 203      156         FAY         WOOD
    ## 204       13         UMA         WOOD
    ## 205       63     CAMERON         WRAY
    ## 206      111     CAMERON    ZELLWEGER
    ## 207      186       JULIA    ZELLWEGER
    ## 208       85      MINNIE    ZELLWEGER

try in SQL directly

``` sql
SELECT actor_id, first_name, last_name
FROM actor 
ORDER BY last_name, first_name
```

| actor_id | first_name | last_name |
|---------:|:-----------|:----------|
|       58 | CHRISTIAN  | AKROYD    |
|      182 | DEBBIE     | AKROYD    |
|       92 | KIRSTEN    | AKROYD    |
|      118 | CUBA       | ALLEN     |
|      145 | KIM        | ALLEN     |
|      194 | MERYL      | ALLEN     |
|       76 | ANGELINA   | ASTAIRE   |
|      112 | RUSSELL    | BACALL    |
|      190 | AUDREY     | BAILEY    |
|       67 | JESSICA    | BAILEY    |

Displaying records 1 - 10

\#exercise2: retrive the actor ID, first name, and last name for actors
whose last name equals ‘WILLIAMS’ or ‘DAVIS’.

``` sql
SELECT actor_id, first_name, last_name
FROM actor
WHERE last_name IN ('WILLIAMS', 'DAVIS')
```

| actor_id | first_name | last_name |
|---------:|:-----------|:----------|
|        4 | JENNIFER   | DAVIS     |
|       72 | SEAN       | WILLIAMS  |
|      101 | SUSAN      | DAVIS     |
|      110 | SUSAN      | DAVIS     |
|      137 | MORGAN     | WILLIAMS  |
|      172 | GROUCHO    | WILLIAMS  |

6 records

\#exercise3: write a query against the `rental` table that returns the
IDs of the customers who rented a film on July 5, 2005 (use the
rental.rental_date column, and you can use the date() function to ignore
the time component). Include a single row for each distinct customer ID.

``` sql
PRAGMA table_info(rental)
```

| cid | name         | type    | notnull | dflt_value |  pk |
|:----|:-------------|:--------|--------:|:-----------|----:|
| 0   | rental_id    | INTEGER |       0 | NA         |   0 |
| 1   | rental_date  | TEXT    |       0 | NA         |   0 |
| 2   | inventory_id | INTEGER |       0 | NA         |   0 |
| 3   | customer_id  | INTEGER |       0 | NA         |   0 |
| 4   | return_date  | TEXT    |       0 | NA         |   0 |
| 5   | staff_id     | INTEGER |       0 | NA         |   0 |
| 6   | last_update  | TEXT    |       0 | NA         |   0 |

7 records

``` sql
SELECT DISTINCT customer_id, rental_date
FROM rental
WHERE date(rental_date) = '2005-07-05'
```

| customer_id | rental_date         |
|------------:|:--------------------|
|         565 | 2005-07-05 22:49:24 |
|         242 | 2005-07-05 22:51:44 |
|          37 | 2005-07-05 22:56:33 |
|          60 | 2005-07-05 22:57:34 |
|         594 | 2005-07-05 22:59:53 |
|           8 | 2005-07-05 23:01:21 |
|         490 | 2005-07-05 23:02:37 |
|         476 | 2005-07-05 23:05:17 |
|         322 | 2005-07-05 23:05:44 |
|         298 | 2005-07-05 23:08:53 |

Displaying records 1 - 10

``` sql
SELECT DISTINCT customer_id,COUNT(*) AS N
FROM rental
WHERE date(rental_date) = '2005-07-05'
GROUP BY customer_id
```

| customer_id |   N |
|------------:|----:|
|           8 |   1 |
|          37 |   1 |
|          60 |   1 |
|         111 |   1 |
|         114 |   1 |
|         138 |   1 |
|         142 |   1 |
|         169 |   1 |
|         242 |   1 |
|         295 |   1 |

Displaying records 1 - 10

\#exercise4

``` sql
PRAGMA table_info(payment)
```

| cid | name         | type    | notnull | dflt_value |  pk |
|:----|:-------------|:--------|--------:|:-----------|----:|
| 0   | payment_id   | INTEGER |       0 | NA         |   0 |
| 1   | customer_id  | INTEGER |       0 | NA         |   0 |
| 2   | staff_id     | INTEGER |       0 | NA         |   0 |
| 3   | rental_id    | INTEGER |       0 | NA         |   0 |
| 4   | amount       | REAL    |       0 | NA         |   0 |
| 5   | payment_date | TEXT    |       0 | NA         |   0 |

6 records

\#4.1: construct a query that retrives all rows from the `payment` table
where the amount is either 1.99, 7.99, 9.99.

``` sql
SELECT *
FROM payment
WHERE amount IN (1.99, 7.99, 9.99)
```

| payment_id | customer_id | staff_id | rental_id | amount | payment_date               |
|-----------:|------------:|---------:|----------:|-------:|:---------------------------|
|      16050 |         269 |        2 |         7 |   1.99 | 2007-01-24 21:40:19.996577 |
|      16056 |         270 |        1 |       193 |   1.99 | 2007-01-26 05:10:14.996577 |
|      16081 |         282 |        2 |        48 |   1.99 | 2007-01-25 04:49:12.996577 |
|      16103 |         294 |        1 |       595 |   1.99 | 2007-01-28 12:28:20.996577 |
|      16133 |         307 |        1 |       614 |   1.99 | 2007-01-28 14:01:54.996577 |
|      16158 |         316 |        1 |      1065 |   1.99 | 2007-01-31 07:23:22.996577 |
|      16160 |         318 |        1 |       224 |   9.99 | 2007-01-26 08:46:53.996577 |
|      16161 |         319 |        1 |        15 |   9.99 | 2007-01-24 23:07:48.996577 |
|      16180 |         330 |        2 |       967 |   7.99 | 2007-01-30 17:40:32.996577 |
|      16206 |         351 |        1 |      1137 |   1.99 | 2007-01-31 17:48:40.996577 |

Displaying records 1 - 10

\#4.2: Construct a query that retrives all rows from the `payment` table
where the amount is greater then 5

``` sql
SELECT *
FROM payment
WHERE amount > 5
```

| payment_id | customer_id | staff_id | rental_id | amount | payment_date               |
|-----------:|------------:|---------:|----------:|-------:|:---------------------------|
|      16052 |         269 |        2 |       678 |   6.99 | 2007-01-28 21:44:14.996577 |
|      16058 |         271 |        1 |      1096 |   8.99 | 2007-01-31 11:59:15.996577 |
|      16060 |         272 |        1 |       405 |   6.99 | 2007-01-27 12:01:05.996577 |
|      16061 |         272 |        1 |      1041 |   6.99 | 2007-01-31 04:14:49.996577 |
|      16068 |         274 |        1 |       394 |   5.99 | 2007-01-27 09:54:37.996577 |
|      16073 |         276 |        1 |       860 |  10.99 | 2007-01-30 01:13:42.996577 |
|      16074 |         277 |        2 |       308 |   6.99 | 2007-01-26 20:30:05.996577 |
|      16082 |         282 |        2 |       282 |   6.99 | 2007-01-26 17:24:52.996577 |
|      16086 |         284 |        1 |      1145 |   6.99 | 2007-01-31 18:42:11.996577 |
|      16087 |         286 |        2 |        81 |   6.99 | 2007-01-25 10:43:45.996577 |

Displaying records 1 - 10

\#4.3: Construct a query that retrives all rows from the `payment` table
where the amount is greater then 5 and less then 8

``` {{sql,connection=con}
SELECT *
FROM payment
WHERE amount > 5 AND amount < 8
```

\#exercise5: Retrive all the payment IDs and their amount from the
customers whose last name is ‘DAVIS’.

``` sql
PRAGMA table_info(customer)
```

| cid | name        | type    | notnull | dflt_value |  pk |
|:----|:------------|:--------|--------:|:-----------|----:|
| 0   | customer_id | INTEGER |       0 | NA         |   0 |
| 1   | store_id    | INTEGER |       0 | NA         |   0 |
| 2   | first_name  | TEXT    |       0 | NA         |   0 |
| 3   | last_name   | TEXT    |       0 | NA         |   0 |
| 4   | email       | TEXT    |       0 | NA         |   0 |
| 5   | address_id  | INTEGER |       0 | NA         |   0 |
| 6   | activebool  | TEXT    |       0 | NA         |   0 |
| 7   | create_date | TEXT    |       0 | NA         |   0 |
| 8   | last_update | TEXT    |       0 | NA         |   0 |
| 9   | active      | INTEGER |       0 | NA         |   0 |

Displaying records 1 - 10

``` sql
SELECT c.customer_id, payment_id, amount
FROM payment AS p
  INNER JOIN customer AS c
  ON p.customer_id=c.customer_id
WHERE last_name ='DAVIS'
```

| customer_id | payment_id | amount |
|------------:|-----------:|-------:|
|           6 |      16685 |   4.99 |
|           6 |      16686 |   2.99 |
|           6 |      16687 |   0.99 |

3 records

\#exercise6: \#6.1: Use COUNT(\*) to count the number of rows in rental

``` sql
SELECT COUNT(*) AS N
FROM rental
```

|     N |
|------:|
| 16044 |

1 records

\#6.2: Use COUNT(\*) and GROUP BY to count the number of rentals for
each customer_id

``` sql
SELECT customer_id, COUNT(*) AS N
FROM rental
GROUP BY customer_id
LIMIT 8
```

| customer_id |   N |
|:------------|----:|
| 1           |  32 |
| 2           |  27 |
| 3           |  26 |
| 4           |  22 |
| 5           |  38 |
| 6           |  28 |
| 7           |  33 |
| 8           |  24 |

8 records

\#6.3: Repeat the previous query and sort by the count in descending
order

``` sql
SELECT customer_id, COUNT(*) AS N
FROM rental
GROUP BY customer_id
ORDER BY N DESC
LIMIT 8
```

| customer_id |   N |
|------------:|----:|
|         148 |  46 |
|         526 |  45 |
|         236 |  42 |
|         144 |  42 |
|          75 |  41 |
|         469 |  40 |
|         197 |  40 |
|         468 |  39 |

8 records

\#6.4: Repeat the previous query but use HAVING to only keep the groups
with 40 or more.

``` sql
SELECT customer_id, COUNT(*) AS N
FROM rental
GROUP BY customer_id
HAVING N >=40 /* have to be followed by group*/
ORDER BY N DESC
```

| customer_id |   N |
|------------:|----:|
|         148 |  46 |
|         526 |  45 |
|         236 |  42 |
|         144 |  42 |
|          75 |  41 |
|         469 |  40 |
|         197 |  40 |

7 records

\#exercise7: The following query calculates a number of summary
statistics for the payment table using MAX, MIN, AVG and SUM

``` sql
SELECT MAX(amount) AS maxpayment,
       MIN(amount) AS minpayment,
       AVG(amount) AS avgpayment,
       SUM(amount) AS sumpayment
FROM payment
```

| maxpayment | minpayment | avgpayment | sumpayment |
|-----------:|-----------:|-----------:|-----------:|
|      11.99 |       0.99 |   4.169775 |    4824.43 |

1 records

\#7.1: Modify the above query to do those calculations for each
customer_id

``` sql
SELECT customer_id,
       MAX(amount) AS maxpayment,
       MIN(amount) AS minpayment,
       AVG(amount) AS avgpayment,
       SUM(amount) AS sumpayment
FROM payment
GROUP BY customer_id
LIMIT 10
```

| customer_id | maxpayment | minpayment | avgpayment | sumpayment |
|------------:|-----------:|-----------:|-----------:|-----------:|
|           1 |       2.99 |       0.99 |   1.990000 |       3.98 |
|           2 |       4.99 |       4.99 |   4.990000 |       4.99 |
|           3 |       2.99 |       1.99 |   2.490000 |       4.98 |
|           5 |       6.99 |       0.99 |   3.323333 |       9.97 |
|           6 |       4.99 |       0.99 |   2.990000 |       8.97 |
|           7 |       5.99 |       0.99 |   4.190000 |      20.95 |
|           8 |       6.99 |       6.99 |   6.990000 |       6.99 |
|           9 |       4.99 |       0.99 |   3.656667 |      10.97 |
|          10 |       4.99 |       4.99 |   4.990000 |       4.99 |
|          11 |       6.99 |       6.99 |   6.990000 |       6.99 |

Displaying records 1 - 10

\#7.2: Modify the above query to only keep the customer_ids that have
more then 5 payments

``` sql
SELECT customer_id,
       MAX(amount) AS maxpayment,
       MIN(amount) AS minpayment,
       AVG(amount) AS avgpayment,
       SUM(amount) AS sumpayment,
       COUNT(*) AS N
FROM payment
GROUP BY customer_id
HAVING N >5
```

| customer_id | maxpayment | minpayment | avgpayment | sumpayment |   N |
|------------:|-----------:|-----------:|-----------:|-----------:|----:|
|          19 |       9.99 |       0.99 |   4.490000 |      26.94 |   6 |
|          53 |       9.99 |       0.99 |   4.490000 |      26.94 |   6 |
|         109 |       7.99 |       0.99 |   3.990000 |      27.93 |   7 |
|         161 |       5.99 |       0.99 |   2.990000 |      17.94 |   6 |
|         197 |       3.99 |       0.99 |   2.615000 |      20.92 |   8 |
|         207 |       6.99 |       0.99 |   2.990000 |      17.94 |   6 |
|         239 |       7.99 |       2.99 |   5.656667 |      33.94 |   6 |
|         245 |       8.99 |       0.99 |   4.823333 |      28.94 |   6 |
|         251 |       4.99 |       1.99 |   3.323333 |      19.94 |   6 |
|         269 |       6.99 |       0.99 |   3.156667 |      18.94 |   6 |

Displaying records 1 - 10

# clean up

``` r
dbDisconnect(con)
```
