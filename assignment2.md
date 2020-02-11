Data analysis assignment 2
================
\[Krystof Burysek\]
\[11.2.2020\]

In this assignment you will work with relational data, i.e. data coming
from different data tables that you can combine using keys. Please read
ch.13 from R for Data Science before completing this assignment –
<https://r4ds.had.co.nz/relational-data.html>.

## Read data

We will work with three different tables: household roster from wave 8
(*h\_egoalt*), stable characteristics of individuals (*xwavedat*), and
household data from wave 8 (*h\_hhresp*).

``` r
library(tidyverse)
# You need to complete the paths to these files on your computer.
Egoalt8 <- read_tsv("C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w8/h_egoalt.tab")
Stable <- read_tsv("C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
Hh8 <- read_tsv("C:/Users/kryst/OneDrive/Documents/UK.U/UKDA-6614-tab/tab/ukhls_w8/h_hhresp.tab")
```

## Filter household roster data (10 points)

The **egoalt8** data table contains data on the kin and other
relationships between people in the same household. In each row in this
table you will have a pair of individuals in the same household: ego
(identified by *pidp*) and alter (identified by *apidp*).
*h\_relationship\_dv* shows the type of relationship between ego and
alter. You can check the codes in the Understanding Society codebooks
here –
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation>.

First we want to select only pairs of individuals who are husbands and
wives or cohabiting partners (codes 1 and 2). For convenience, we also
want to keep only the variables *pidp*, *apidp*, *h\_hidp* (household
identifier), *h\_relationship\_dv*, *h\_esex* (ego’s sex), and *h\_asex*
(alter’s sex).

``` r
Partners8 <- Egoalt8 %>%
        filter(h_relationship_dv == '1' | h_relationship_dv == '2') %>%
        select(pidp, apidp, h_hidp, h_relationship_dv, h_sex, h_asex)
```

Each couple now appears in the data twice: 1) with one partner as ego
and the other as alter, 2) the other way round. Now we will only focus
on heterosexual couples, and keep one observation per couple with women
as egos and men as their alters.

``` r
Hetero8 <- Partners8 %>%
        # filter out same-sex couples
        filter(h_sex != '1' & h_asex == '1' | h_sex != '2' & h_asex == '2') %>%
        # keep only one observation per couple with women as egos
        filter(h_sex == 2)

Partners8 %>% 
  count(h_sex == '1' & h_asex == '1')
```

    ## # A tibble: 2 x 2
    ##   `h_sex == "1" & h_asex == "1"`     n
    ##   <lgl>                          <int>
    ## 1 FALSE                          27324
    ## 2 TRUE                             106

## Recode data on ethnicity (10 points)

In this assignment we will explore ethnic endogamy, i.e. marriages and
partnerships within the same ethnic group. First, let us a create a
version of the table with stable individual characteristics with two
variables only: *pidp* and *racel\_dv* (ethnicity).

``` r
Stable2 <- Stable %>%
        select(pidp, racel_dv)
```

Let’s code missing values on ethnicity (-9) as NA.

``` r
Stable2 <- Stable2 %>%
        mutate(racel_dv = recode(racel_dv, `-9` = NA_real_))
```

Now let us recode the variable on ethnicity into a new binary variable
with the following values: “White” (codes 1 to 4) and “non-White” (all
other codes).

``` r
Stable2 <- Stable2 %>%
        mutate(race = recode(racel_dv, 
                             '1' = 'White',
                             '2' = 'White',
                             '3' = 'White',
                             '4' = 'White',
                             .default = 'non-White'))

## Join data (30 points)

#Now we want to join data from the household roster (*Hetero8*) and the data table with ethnicity (*Stable2*). First let us merge in the data on ego's ethnicity. We want to keep all the observations we have in *Hetero8*, but we don't want to add any other individuals from *Stable2*.
```

``` r
JoinedEthn <- Hetero8 %>%
  inner_join(Stable2, by = 'pidp')

# Let us rename the variables for ethnicity to clearly indicate that they refer to egos.
```

``` r
JoinedEthn <- JoinedEthn %>%
        rename(egoRacel_dv = racel_dv) %>%
        rename(egoRace = race)
```

Now let us merge in the data on alter’s ethnicity. Note that in this
case the key variables have different names in two data tables; please
refer to the documentation for your join function (or the relevant
section from R for Data Science) to check the solution for this problem.

?join

``` r
JoinedEthn <- JoinedEthn %>%
        left_join(Stable2, by = c('apidp' = 'pidp'))
```

Renaming the variables for alters.

``` r
JoinedEthn <- JoinedEthn %>%
        rename(alterRacel_dv = racel_dv) %>%
        rename(alterRace = race)
```

## Explore probabilities of racial endogamy (20 points)

Let us start by looking at the joint distribution of race (White
vs. non-White) of both partners.

?count

``` r
TableRace <- JoinedEthn %>%
        # filter out observations with missing data
        filter(egoRace != "NA" & alterRace != "NA") %>%
        count(egoRace, alterRace)
TableRace
```

    ## # A tibble: 4 x 3
    ##   egoRace   alterRace     n
    ##   <chr>     <chr>     <int>
    ## 1 non-White non-White  1790
    ## 2 non-White White       326
    ## 3 White     non-White   266
    ## 4 White     White      9694

Now calculate the following probabilities: 1) for a White woman to have
a White partner, 2) for a White woman to have a non-White partner, 3)
for a non-White woman to have a White partner, 4) for a non-White woman
to have a non-White partner.

Of course, you can simply calculate these numbers manually. However, the
code will not be reproducible: if the data change the code will need to
be changed, too. Your task is to write reproducible code producing a
table with the required four probabilities.

?group\_by

``` r
## The code for question below


## Just the variable with the household and kids


c.Hh8 <- Hh8 %>%
  select(h_hidp, h_nkids_dv)


## Joining with the inidividaul data file - based on race

## just identifier and race 
c.Stable <- Stable %>%
  select(pidp, racel_dv) %>%
  mutate(racel_dv = recode(racel_dv, `-9` = NA_real_))

## joining egolat and household level 

c.Joined <- Egoalt8 %>%
  left_join(c.Hh8, by = "h_hidp")

## then join with the race 

c.Joined <- c.Joined %>%
  left_join(c.Stable, by = "pidp")

## Renaming race_dv on h_sex. race

c.Joined <- c.Joined %>%
  rename(h_sex.race = "racel_dv")

## joing the race for the alter 

c.Joined <- c.Joined %>%
  left_join(c.Stable, by = c("apidp" = "pidp"))

## Renaming race_dv on h_asex.race

c.Joined <- c.Joined %>%
  rename(h_asex.race = "racel_dv")

#### Joined 

#### Selecting racially endogemous couples 

## marriage or cohabiting filtering


c.Joined <- c.Joined %>%
  filter(h_relationship_dv == '1' | h_relationship_dv == '2')

## race filtering 

c.Joined <- c.Joined %>%
  filter(h_sex.race == 1 & h_asex.race == 1 |
           h_sex.race == 9 & h_asex.race == 9 |
           h_sex.race == 10 & h_asex.race == 10)


## summarising the kids 


Table.kids <- c.Joined %>%
  group_by(h_sex.race) %>%
  summarise(meanKids = mean(h_nkids_dv, na.rm = TRUE))

Table.kids.m <- c.Joined %>%
  group_by(h_sex.race) %>%
  summarise(meanKids = median(h_nkids_dv, na.rm = TRUE))

Table.kids <- Table.kids %>%
  left_join(Table.kids.m, by = "h_sex.race")

Table.kids <- Table.kids %>%
  rename(median = "meanKids.y")

Table.kids <- Table.kids %>%
  rename(mean = "meanKids.x")

Table.kids <- Table.kids %>%
  rename(race = "h_sex.race")

Table.kids <- Table.kids %>%
  mutate(race = recode(race, '1' = "White British", 
                       '9' = 'Indian', 
                       '10' = 'Pakistani'))


# Interpratation: White British only have 0.56 kids on average which amount to zero in median. This places it at the bottom showing the narrative of British population growing more cosmpolitan in future may be true. Indian then rank second with one kid as median and then Pakisatni with 2 kids as median. Of course, mean and median numbers differ in all cases , most in the case of White British inidcating the spread of the data. The data may be influenced by the number of factors like the fact that White British population is still the most numerous and therefore the data are more easily represented fro them than other groups. Other factors responisble but unnaccounted for may include socioeconomic background of these groups as immigrant background is said to correlate with the less well being but also greater birth rate.
```

## Join with household data and calculate mean and median number of children by ethnic group (30 points)

1)  Join the individual-level file with the household-level data from
    wave 8 (specifically, we want the variable for the number of
    children in the household).
2)  Select only couples that are ethnically endogamous (i.e. partners
    come from the same ethnic group) for the following groups: White
    British, Indian, and Pakistani.
3)  Produce a table showing the mean and median number of children in
    these households by ethnic group (make sure the table has meaningful
    labels for ethnic groups, not just numerical codes).
4)  Write a short interpretation of your results. What could affect your
    findings?
