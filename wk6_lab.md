wk6_lab
================
Yiping Li
2022-09-28

``` r
library(data.table)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()   masks data.table::between()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::first()     masks data.table::first()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::last()      masks data.table::last()
    ## ✖ purrr::transpose() masks data.table::transpose()

``` r
library(dplyr)
library(dtplyr)
library(tidytext)
library(ggplot2)
library(forecast)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
if (!file.exists("mtsamples.csv")){
  download.file(
    "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv",
    "mtsamples.csv",
    method   = "libcurl",
    timeout  = 60)
  }

mts <- read.csv("mtsamples.csv")
str(mts)
```

    ## 'data.frame':    4999 obs. of  6 variables:
    ##  $ X                : int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ description      : chr  " A 23-year-old white female presents with complaint of allergies." " Consult for laparoscopic gastric bypass." " Consult for laparoscopic gastric bypass." " 2-D M-Mode. Doppler.  " ...
    ##  $ medical_specialty: chr  " Allergy / Immunology" " Bariatrics" " Bariatrics" " Cardiovascular / Pulmonary" ...
    ##  $ sample_name      : chr  " Allergic Rhinitis " " Laparoscopic Gastric Bypass Consult - 2 " " Laparoscopic Gastric Bypass Consult - 1 " " 2-D Echocardiogram - 1 " ...
    ##  $ transcription    : chr  "SUBJECTIVE:,  This 23-year-old white female presents with complaint of allergies.  She used to have allergies w"| __truncated__ "PAST MEDICAL HISTORY:, He has difficulty climbing stairs, difficulty with airline seats, tying shoes, used to p"| __truncated__ "HISTORY OF PRESENT ILLNESS: , I have seen ABC today.  He is a very pleasant gentleman who is 42 years old, 344 "| __truncated__ "2-D M-MODE: , ,1.  Left atrial enlargement with left atrial diameter of 4.7 cm.,2.  Normal size right and left "| __truncated__ ...
    ##  $ keywords         : chr  "allergy / immunology, allergic rhinitis, allergies, asthma, nasal sprays, rhinitis, nasal, erythematous, allegr"| __truncated__ "bariatrics, laparoscopic gastric bypass, weight loss programs, gastric bypass, atkin's diet, weight watcher's, "| __truncated__ "bariatrics, laparoscopic gastric bypass, heart attacks, body weight, pulmonary embolism, potential complication"| __truncated__ "cardiovascular / pulmonary, 2-d m-mode, doppler, aortic valve, atrial enlargement, diastolic function, ejection"| __truncated__ ...

``` r
mts <- as_tibble(mts)
mts
```

    ## # A tibble: 4,999 × 6
    ##        X description                             medic…¹ sampl…² trans…³ keywo…⁴
    ##    <int> <chr>                                   <chr>   <chr>   <chr>   <chr>  
    ##  1     0 " A 23-year-old white female presents … " Alle… " Alle… "SUBJE… "aller…
    ##  2     1 " Consult for laparoscopic gastric byp… " Bari… " Lapa… "PAST … "baria…
    ##  3     2 " Consult for laparoscopic gastric byp… " Bari… " Lapa… "HISTO… "baria…
    ##  4     3 " 2-D M-Mode. Doppler.  "               " Card… " 2-D … "2-D M… "cardi…
    ##  5     4 " 2-D Echocardiogram"                   " Card… " 2-D … "1.  T… "cardi…
    ##  6     5 " Morbid obesity.  Laparoscopic anteco… " Bari… " Lapa… "PREOP… "baria…
    ##  7     6 " Liposuction of the supraumbilical ab… " Bari… " Lipo… "PREOP… "baria…
    ##  8     7 " 2-D Echocardiogram"                   " Card… " 2-D … "2-D E… "cardi…
    ##  9     8 " Suction-assisted lipectomy - lipodys… " Bari… " Lipe… "PREOP… "baria…
    ## 10     9 " Echocardiogram and Doppler"           " Card… " 2-D … "DESCR… "cardi…
    ## # … with 4,989 more rows, and abbreviated variable names ¹​medical_specialty,
    ## #   ²​sample_name, ³​transcription, ⁴​keywords
    ## # ℹ Use `print(n = ...)` to see more rows

\#Question 1: What specialties do we have? We can use count() from dplyr
to figure out how many different catagories do we have? Are these
catagories related? overlapping? evenly distributed?

``` r
specialties <- mts %>%
  count(medical_specialty)

specialties %>%
  arrange(desc(n)) %>%
  knitr::kable()
```

| medical_specialty             |    n |
|:------------------------------|-----:|
| Surgery                       | 1103 |
| Consult - History and Phy.    |  516 |
| Cardiovascular / Pulmonary    |  372 |
| Orthopedic                    |  355 |
| Radiology                     |  273 |
| General Medicine              |  259 |
| Gastroenterology              |  230 |
| Neurology                     |  223 |
| SOAP / Chart / Progress Notes |  166 |
| Obstetrics / Gynecology       |  160 |
| Urology                       |  158 |
| Discharge Summary             |  108 |
| ENT - Otolaryngology          |   98 |
| Neurosurgery                  |   94 |
| Hematology - Oncology         |   90 |
| Ophthalmology                 |   83 |
| Nephrology                    |   81 |
| Emergency Room Reports        |   75 |
| Pediatrics - Neonatal         |   70 |
| Pain Management               |   62 |
| Psychiatry / Psychology       |   53 |
| Office Notes                  |   51 |
| Podiatry                      |   47 |
| Dermatology                   |   29 |
| Cosmetic / Plastic Surgery    |   27 |
| Dentistry                     |   27 |
| Letters                       |   23 |
| Physical Medicine - Rehab     |   21 |
| Sleep Medicine                |   20 |
| Endocrinology                 |   19 |
| Bariatrics                    |   18 |
| IME-QME-Work Comp etc.        |   16 |
| Chiropractic                  |   14 |
| Diets and Nutritions          |   10 |
| Rheumatology                  |   10 |
| Speech - Language             |    9 |
| Autopsy                       |    8 |
| Lab Medicine - Pathology      |    8 |
| Allergy / Immunology          |    7 |
| Hospice - Palliative Care     |    6 |

\#There are ’nrows(specialities)” medical specialities

``` r
specialties %>%
  top_n(10) %>%
  ggplot(aes(x=n, y=fct_reorder(medical_specialty,n))) +
  geom_col()
```

    ## Selecting by n

![](wk6_lab_files/figure-gfm/barplot-of-speciality-counts-1.png)<!-- -->

\#The distribution is not at all uniform

\#Question 2 Tokenize the the words in the transcription column Count
the number of times each token appears Visualize the top 20 most
frequent words Explain what we see from this result. Does it makes
sense? What insights (if any) do we get?

``` r
mts %>%
  unnest_tokens(word, transcription) %>%
  #anti_join(stop_words, by=c("word")) %>%
  count(word, sort= TRUE) %>%
  top_n(20, n) %>%
  ggplot(aes(n, fct_reorder(word,n))) +
  geom_col()
```

![](wk6_lab_files/figure-gfm/token-transcription-1.png)<!-- --> \#There
are a lot of stopwords here, non-specific to medical text. We do see
“patient”

\#Question 3 Redo visualization but remove stopwords before Bonus points
if you remove numbers as well What do we see know that we have removed
stop words? Does it give us a better idea of what the text is about?

``` r
mts %>%
  unnest_tokens(word, transcription) %>%
  anti_join(stop_words, by=c("word")) %>%
  count(word, sort= TRUE) %>%
  #use regular expression to filter out numbers
  filter(!grepl(pattern = "^[0-9]+$", x = word)) %>%
  top_n(20, n) %>%
  ggplot(aes(n, fct_reorder(word,n))) +
  geom_col()
```

![](wk6_lab_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

\#Question 4 repeat question 2, but this time tokenize into bi-grams.
how does the result change if you look at tri-grams?

\#Question 5 Using the results you got from questions 4. Pick a word and
count the words that appears after and before it.

\#Question 6 Which words are most used in each of the specialties. you
can use group_by() and top_n() from dplyr to have the calculations be
done within each specialty. Remember to remove stopwords. How about the
most 5 used words?
