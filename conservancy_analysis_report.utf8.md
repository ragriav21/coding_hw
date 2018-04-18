---
title: "Conservancy Analysis"
author: "BEACN"
date: "March 23, 2018"
output:
  pdf_document: default
  html_document: default
---

## Setup

Downloading the data


```r
library(tidyverse)
```

```
## -- Attaching packages ----------------------------------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1.9000     v purrr   0.2.4     
## v tibble  1.4.2          v dplyr   0.7.4     
## v tidyr   0.7.2          v stringr 1.2.0     
## v readr   1.1.1          v forcats 0.2.0
```

```
## -- Conflicts -------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
## x dplyr::vars()   masks ggplot2::vars()
```

```r
library(ggplot2)
library(stringr)
library(gridExtra)
```

```
## Warning: package 'gridExtra' was built under R version 3.4.4
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
library(grid)

conservancy_names <- function(name) {
  return(paste0("conservancy_", name, ".csv")) 
}

ucnrs_names <- function(name) {
  return(paste0("ucnrs_", name, ".csv"))
}

conservancyNames <- c("birds", "herpetofauna", "invertebrates", "mammals", "plants")

ucnrsNames <- c("animal_list", "plant_list")

fullConservancyNames <- conservancy_names(conservancyNames)
fullUCNRSNames <- ucnrs_names(ucnrsNames)

allFiles <- c(fullConservancyNames, fullUCNRSNames)
path <- paste0("./ucnrs_data/", allFiles)

data <- lapply(path, read_csv) 
```

```
## Parsed with column specification:
## cols(
##   species = col_character()
## )
```

```
## Parsed with column specification:
## cols(
##   species = col_character()
## )
```

```
## Parsed with column specification:
## cols(
##   Species = col_character()
## )
```

```
## Parsed with column specification:
## cols(
##   species = col_character()
## )
## Parsed with column specification:
## cols(
##   species = col_character()
## )
```

```
## Parsed with column specification:
## cols(
##   Reserve = col_character(),
##   Taxon = col_character(),
##   Family = col_character(),
##   `Scientific Name` = col_character(),
##   `Accepted Name` = col_character(),
##   `Common Name` = col_character()
## )
```

```
## Warning: Missing column names filled in: 'X17' [17], 'X18' [18],
## 'X19' [19], 'X20' [20], 'X21' [21], 'X22' [22]
```

```
## Parsed with column specification:
## cols(
##   .default = col_character(),
##   `Reserve count (values)` = col_integer(),
##   `Reserve count (formula)` = col_integer()
## )
```

```
## See spec(...) for full column specifications.
```

```r
head(data)
```

## Combining UCNRS and Conservancy into four datasets



```r
# Separating out columns of data for conservancy data and removing irrelevant values

conservancyAnimalData <- bind_rows(data[[1]], data[[2]], data[[3]], data[[4]]) %>% 
  distinct(species) %>% 
  separate(species, into = c("genus", "species", "sublabel1", "sublabel2"), sep = " ", extra = "merge") %>% 
  filter(!(genus %in% c("Tejon", "Undefined")))
```

```
## Warning: Too few values at 351 locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
## 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...
```

```r
conservancyPlantData <- data[[5]] %>% distinct(species) %>% 
  separate(species, into = c("genus", "species", "separator_label", "sublabel"), sep = " ", extra = "merge") %>% 
  filter(!(genus %in% c("Tejon", "Undefined")))
```

```
## Warning: Too few values at 652 locations: 2, 4, 5, 6, 11, 12, 13, 14, 15,
## 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, ...
```

```r
# Loading Plant and Animal data from UCNRS

ucnrsPlantData <- data[[7]]
ucnrsAnimalData <- data[[6]]

# Relabeling data

names(ucnrsPlantData)[7:8] <- c("separator_label", "sublabel")
names(ucnrsAnimalData)[4] <- "scientific_name"

# Cleaning UCNRS Animal data

ucnrsAnimalDataSubsetTemp <- ucnrsAnimalData %>% 
  select(Reserve, scientific_name) %>% 
  separate(scientific_name, into = c("genus", "species", "sublabel1", "sublabel2"), sep = " ", extra = "merge") %>%
  distinct()
```

```
## Warning: Too few values at 8696 locations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
## 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...
```

```r
ucnrsAnimalDataSubset <-  mutate(ucnrsAnimalDataSubsetTemp, 
                                 sublabel1 = gsub("\\(", "", sublabel1), sublabel2 = gsub("\\(", "", sublabel2), 
         sublabel1 = gsub("\\)", "", sublabel1), sublabel2 = gsub("\\)", "", sublabel2),
         sublabel1 = gsub("\\.", "", sublabel1), sublabel2 = gsub("\\.", "", sublabel2), 
         species = tolower(species))

# Cleaning UCNRS Plant Data

ucnrsPlantDataSubset <- ucnrsPlantData %>% 
  select(Reserve, Genus, Species, separator_label, sublabel) %>%
  distinct()

# Relabeling column headers

names(ucnrsPlantDataSubset)[1:3] <- c("reserve", "genus", "species")
names(ucnrsAnimalDataSubset)[1] <- c("reserve")

# Print results

head(ucnrsAnimalDataSubset)
```

```
## # A tibble: 6 x 5
##   reserve                    genus         species     sublabel1 sublabel2
##   <chr>                      <chr>         <chr>       <chr>     <chr>    
## 1 Angelo Coast Range Reserve Ambystoma     gracile     <NA>      <NA>     
## 2 Angelo Coast Range Reserve Aneides       ferreus     <NA>      <NA>     
## 3 Angelo Coast Range Reserve Aneides       flavipunct~ <NA>      <NA>     
## 4 Angelo Coast Range Reserve Aneides       vagrans     <NA>      <NA>     
## 5 Angelo Coast Range Reserve Ascaphus      truei       <NA>      <NA>     
## 6 Angelo Coast Range Reserve Bactrachoseps attenuatus  <NA>      <NA>
```

```r
head(ucnrsPlantDataSubset)
```

```
## # A tibble: 6 x 5
##   reserve                     genus   species   separator_label sublabel 
##   <chr>                       <chr>   <chr>     <chr>           <chr>    
## 1 Landells-Hill Big Creek     Abies   bracteata <NA>            <NA>     
## 2 James San Jacinto Mountains Abies   concolor  <NA>            <NA>     
## 3 Valentine                   Abies   concolor  <NA>            <NA>     
## 4 Valentine                   Abies   magnifica var.            magnifica
## 5 Bodega                      Abronia latifolia <NA>            <NA>     
## 6 Younger Lagoon              Abronia latifolia <NA>            <NA>
```

```r
head(conservancyAnimalData)
```

```
## # A tibble: 6 x 4
##   genus species    sublabel1 sublabel2
##   <chr> <chr>      <chr>     <chr>    
## 1 Aix   sponsa     <NA>      <NA>     
## 2 Anas  acuta      <NA>      <NA>     
## 3 Anas  americana  <NA>      <NA>     
## 4 Anas  clypeata   <NA>      <NA>     
## 5 Anas  crecca     <NA>      <NA>     
## 6 Anas  cyanoptera <NA>      <NA>
```

```r
head(conservancyPlantData)
```

```
## # A tibble: 6 x 4
##   genus        species      separator_label sublabel    
##   <chr>        <chr>        <chr>           <chr>       
## 1 Sambucus     nigra        subsp.          caerulea    
## 2 Apocynum     cannabinum   <NA>            <NA>        
## 3 Chlorogalum  pomeridianum var.            pomeridianum
## 4 Hesperoyucca whipplei     <NA>            <NA>        
## 5 Yucca        brevifolia   <NA>            <NA>        
## 6 Allium       burlewii     <NA>            <NA>
```

## Match Ratio of Plants per Reserve


```r
# Find shared genies and species of plants
matchingPlants <- semi_join(ucnrsPlantDataSubset, conservancyPlantData, by = c("genus", "species", "sublabel")) %>%
  select(reserve, genus, species, sublabel)

# Calculate how similar plant species composition is by reserve
matchingPlantsCount <- matchingPlants %>% 
  group_by(reserve) %>% 
  summarize(count = n())

totalPlantsCount <- ucnrsPlantDataSubset %>% 
  group_by(reserve) %>% 
  summarize(count = n())

plantsComparisonTable <- left_join(matchingPlantsCount, totalPlantsCount, 
                                   by = "reserve", suffix = c("_matching", "_total")) %>% 
  mutate(match_ratio = count_matching / count_total) %>% 
  arrange(desc(match_ratio))

plantsComparisonTable
```

```
## # A tibble: 30 x 4
##    reserve                          count_matching count_total match_ratio
##    <chr>                                     <int>       <int>       <dbl>
##  1 Sedgewick                                   193         417       0.463
##  2 Motte Rimrock                                85         192       0.443
##  3 Blue Oak Ranch                              193         462       0.418
##  4 Stebbins Cold Canyon                        148         372       0.398
##  5 Stunt Ranch Santa Monica Mounta~            113         297       0.380
##  6 Quail Ridge                                 107         287       0.373
##  7 Burns Pinon Ridge                           100         272       0.368
##  8 San Joaquin Freshwater Marsh                 94         258       0.364
##  9 Hastings                                    229         646       0.354
## 10 Dawson Los Monos Canyon                     104         294       0.354
## # ... with 20 more rows
```


## Where each plant is found


```r
# Find places where each matched species shows up in other reserves

matchingPlantsArranged <- matchingPlants %>% 
  arrange(genus, species, sublabel, reserve) 

matchingPlantsReserves <- matchingPlantsArranged  %>% 
  group_by(genus, species, sublabel) %>% 
  summarize(count = n()) 

# Add names of reserves each species is represented in at end:

matchingPlantsArranged$reserve_list <- ""

matchingPlantsArranged$reserve_list[1] <- paste0(matchingPlantsArranged$reserve[1])
idx = 2
idx_max = nrow(matchingPlantsArranged)

while(idx < idx_max) {
  n = 1
  while((setequal(matchingPlantsArranged[idx, 2:4], matchingPlantsArranged[(idx - 1), 2:4])) & (idx < idx_max)) {
    n = n + 1
    idx = idx + 1
  }
  matchingPlantsArranged$reserve_list[(idx - n):(idx - 1)] <- 
    paste(matchingPlantsArranged$reserve[(idx - n):(idx - 1)], collapse = ", ")
  idx = idx + 1
}

matchingPlantsArranged$reserve_list[idx_max] <- 
    paste(matchingPlantsArranged$reserve[idx_max])

# Combine the data

matchingPlantsReservesCombined <- matchingPlantsArranged %>% 
  distinct(genus, species, sublabel, reserve_list) %>% 
  right_join(matchingPlantsReserves, by = c("genus", "species", "sublabel"))

matchingPlantsReservesCombined
```

```
## # A tibble: 603 x 5
##    genus         species         sublabel  reserve_list              count
##    <chr>         <chr>           <chr>     <chr>                     <int>
##  1 Abies         concolor        <NA>      James San Jacinto Mounta~     2
##  2 Acamptopappus sphaerocephalus hirtellus Sweeney Granite Mountains     1
##  3 Acer          macrophyllum    <NA>      Angelo Coast Range, Blue~     9
##  4 Achillea      millefolium     <NA>      Angelo Coast Range, Blue~    17
##  5 Achyrachaena  mollis          <NA>      Blue Oak Ranch, Hastings~     8
##  6 Acourtia      microcephala    <NA>      Dawson Los Monos Canyon,~     5
##  7 Aesculus      californica     <NA>      Angelo Coast Range, Blue~     8
##  8 Agoseris      grandiflora     <NA>      Angelo Coast Range, Bode~    11
##  9 Agoseris      retrorsa        <NA>      Boyd Deep Canyon, Chicke~     4
## 10 Ailanthus     altissima       <NA>      McLaughlin, Quail Ridge,~     3
## # ... with 593 more rows
```

## Match Ratio of Animals
per Reserve


```r
# Find shared genies and species of plants
matchingAnimals <- semi_join(ucnrsAnimalDataSubset, conservancyAnimalData, by = c("genus", "species", "sublabel1")) %>%
  select(reserve, genus, species, sublabel1, sublabel2)

# Calculate how similar plant species composition is by reserve
matchingAnimalsCount <- matchingAnimals %>% 
  group_by(reserve) %>% 
  summarize(count = n())

totalAnimalsCount <- ucnrsAnimalDataSubset %>% 
  group_by(reserve) %>% 
  summarize(count = n())

animalsComparisonTable <- left_join(matchingAnimalsCount, totalAnimalsCount, 
                                   by = "reserve", suffix = c("_matching", "_total")) %>% 
  mutate(match_ratio = count_matching / count_total) %>% 
  arrange(desc(match_ratio))

animalsComparisonTable
```

```
## # A tibble: 31 x 4
##    reserve                          count_matching count_total match_ratio
##    <chr>                                     <int>       <int>       <dbl>
##  1 <NA>                                          1           1       1.00 
##  2 Dawson Los Monos Canyon Reserve              96         126       0.762
##  3 Kendall-Frost Missions Bay Mars~            104         151       0.689
##  4 Elliot Chaparral Reserve                    107         158       0.677
##  5 Boyd Deep Canyon Desert Researc~            207         325       0.637
##  6 Carpinteria Salt Marsh Reserve              130         205       0.634
##  7 Scripps Coastal Reserve                     118         187       0.631
##  8 Box Springs Reserve                          13          21       0.619
##  9 Stunt Ranch Reserve                          84         139       0.604
## 10 Motte Rimrock Reserve                       144         249       0.578
## # ... with 21 more rows
```


## Where each plant is found


```r
# Find places where each matched species shows up in other reserves

matchingAnimalsArranged <- matchingAnimals %>% 
  arrange(genus, species, sublabel1, reserve) 

matchingAnimalsReserves <- matchingAnimalsArranged  %>% 
  group_by(genus, species, sublabel1) %>% 
  summarize(count = n()) 

# Add names of reserves each species is represented in at end:

matchingAnimalsArranged$reserve_list <- ""

matchingAnimalsArranged$reserve_list[1] <- paste0(matchingAnimalsArranged$reserve[1])
idx = 2
idx_max = nrow(matchingAnimalsArranged)

while(idx < idx_max) {
  n = 1
  while((setequal(matchingAnimalsArranged[idx, 2:4], matchingAnimalsArranged[(idx - 1), 2:4])) & (idx < idx_max)) {
    n = n + 1
    idx = idx + 1
  }
  matchingAnimalsArranged$reserve_list[(idx - n):(idx - 1)] <- 
    paste(matchingAnimalsArranged$reserve[(idx - n):(idx - 1)], collapse = ", ")
  idx = idx + 1
}

matchingAnimalsArranged$reserve_list[idx_max] <- 
    paste(matchingAnimalsArranged$reserve[idx_max])

# Combine the data

matchingAnimalsReservesCombined <- matchingAnimalsArranged %>% 
  distinct(genus, species, sublabel1, reserve_list) %>% 
  right_join(matchingAnimalsReserves, by = c("genus", "species", "sublabel1"))

matchingAnimalsReservesCombined
```

```
## # A tibble: 320 x 5
##    genus        species      sublabel1 reserve_list                  count
##    <chr>        <chr>        <chr>     <chr>                         <int>
##  1 Accipiter    cooperii     <NA>      "Angelo Coast Range Reserve,~    20
##  2 Accipiter    gentilis     <NA>      "Angelo Coast Range Reserve,~     6
##  3 Accipiter    striatus     <NA>      "Angelo Coast Range Reserve,~    19
##  4 Actinemys    marmorata    <NA>      Angelo Coast Range Reserve, ~     3
##  5 Actitis      macularia    <NA>      "Angelo Coast Range Reserve,~    10
##  6 Aechmophorus clarkii      <NA>      Bodega Marine Reserve, Carpi~     7
##  7 Aechmophorus occidentalis <NA>      Angelo Coast Range Reserve, ~     9
##  8 Aegolius     acadicus     <NA>      Angelo Coast Range Reserve, ~    11
##  9 Aeronautes   saxatalis    <NA>      "Blue Oak Ranch Reserve, Box~    19
## 10 Agelaius     phoeniceus   <NA>      "Angelo Coast Range Reserve,~    17
## # ... with 310 more rows
```

## Visuals & Summary Statistics


```r
plantsComparisonTable %>% 
  filter(match_ratio > 0.25) %>% 
  ggplot(aes(x = reorder(reserve, match_ratio), y = match_ratio)) + 
  geom_bar(stat = "identity") +
  coord_flip()
```

![](conservancy_analysis_report_files/figure-latex/unnamed-chunk-7-1.pdf)<!-- --> 

```r
animalsComparisonTable %>% 
  filter(match_ratio > 0.25) %>% 
  ggplot(aes(x = reorder(reserve, match_ratio), y = match_ratio)) + 
  geom_bar(stat = "identity") +
  coord_flip()
```

![](conservancy_analysis_report_files/figure-latex/unnamed-chunk-7-2.pdf)<!-- --> 

## Unique Plants and Animals



```r
uniquePlants <- conservancyPlantData %>% anti_join(ucnrsPlantDataSubset, by = c("genus", "species", "sublabel"))
uniqueAnimals <- conservancyAnimalData %>% anti_join(ucnrsAnimalDataSubset, by = c("genus", "species", "sublabel1"))


totalPlantsNum <- nrow(conservancyPlantData)
uniquePlantsNum <- nrow(uniquePlants)

totalAnimalsNum <- nrow(conservancyAnimalData)
uniqueAnimalsNum <- nrow(uniqueAnimals)

ratioUniquePlants <- uniquePlantsNum / totalPlantsNum
ratioUniqueAnimals <- uniqueAnimalsNum / totalAnimalsNum

rarePlants <- matchingPlantsReservesCombined %>% filter(count < 4)
rarePlantsNum <- nrow(rarePlants)
rareOrUniquePlantsNum <- uniquePlantsNum + rarePlantsNum

rareAnimals <- matchingAnimalsReservesCombined %>% filter(count < 4)
rareAnimalsNum <- nrow(rareAnimals)
rareOrUniquePlantsNum <- uniqueAnimalsNum + rareAnimalsNum

ratioRareOrUniquePlants <- rareOrUniquePlantsNum / totalPlantsNum
ratioRareOrUniqueAnimals <- rareOrUniquePlantsNum / totalAnimalsNum


ratioUniquePlants
```

```
## [1] 0.3744813
```

```r
ratioRareOrUniquePlants
```

```
## [1] 0.08921162
```

```r
ratioUniqueAnimals
```

```
## [1] 0.09631728
```

```r
ratioRareOrUniqueAnimals
```

```
## [1] 0.2436261
```

```r
uniquePlants
```

```
## # A tibble: 361 x 4
##    genus       species       separator_label sublabel   
##    <chr>       <chr>         <chr>           <chr>      
##  1 Allium      howellii      var.            howellii   
##  2 Allium      lacunosum     var.            davisiae   
##  3 Allium      peninsulare   var.            peninsulare
##  4 Rhus        aromatica     <NA>            <NA>       
##  5 Cicuta      douglasii     <NA>            <NA>       
##  6 Perideridia pringlei      <NA>            <NA>       
##  7 Chaenactis  glabriuscula  var.            megacephala
##  8 Chaenactis  santolinoides <NA>            <NA>       
##  9 Chaenactis  xantiana      <NA>            <NA>       
## 10 Crepis      acuminata     <NA>            <NA>       
## # ... with 351 more rows
```

```r
uniqueAnimals
```

```
## # A tibble: 34 x 4
##    genus         species      sublabel1    sublabel2
##    <chr>         <chr>        <chr>        <chr>    
##  1 Colinus       virginianus  <NA>         <NA>     
##  2 Falco         peregrinus   anatum       <NA>     
##  3 Porzana       Carolina     <NA>         <NA>     
##  4 Recurvirostra american     <NA>         <NA>     
##  5 Hydroprogne   caspia       <NA>         <NA>     
##  6 Strix         occidentalis occidentalis <NA>     
##  7 Aeronautes    vauxi        <NA>         <NA>     
##  8 Hylocichla    mustelina    <NA>         <NA>     
##  9 Myadestes     townsendii   <NA>         <NA>     
## 10 Setophaga     coronata     coronata     group    
## # ... with 24 more rows
```

```r
rarePlants
```

```
## # A tibble: 304 x 5
##    genus           species         sublabel   reserve_list           count
##    <chr>           <chr>           <chr>      <chr>                  <int>
##  1 Abies           concolor        <NA>       James San Jacinto Mou~     2
##  2 Acamptopappus   sphaerocephalus hirtellus  Sweeney Granite Mount~     1
##  3 Ailanthus       altissima       <NA>       McLaughlin, Quail Rid~     3
##  4 Allium          burlewii        <NA>       Boyd Deep Canyon, Jam~     2
##  5 Allium          fimbriatum      fimbriatum Boyd Deep Canyon, Bur~     3
##  6 Ambrosia        acanthicarpa    <NA>       Boyd Deep Canyon, Bur~     3
##  7 Amsinckia       intermedia      <NA>       Landells-Hill Big Cre~     1
##  8 Amsinckia       tessellata      tessellata Boyd Deep Canyon, Swe~     2
##  9 Ancistrocarphus filagineus      <NA>       McLaughlin, Motte Rim~     3
## 10 Anisocoma       acaulis         <NA>       Boyd Deep Canyon, Bur~     3
## # ... with 294 more rows
```

```r
rareAnimals
```

```
## # A tibble: 52 x 5
##    genus            species      sublabel1 reserve_list              count
##    <chr>            <chr>        <chr>     <chr>                     <int>
##  1 Actinemys        marmorata    <NA>      Angelo Coast Range Reser~     3
##  2 Alectoris        chukar       <NA>      Motte Rimrock Reserve, S~     2
##  3 Ammospermophilus leucurus     <NA>      Boyd Deep Canyon Desert ~     1
##  4 Amphispiza       bilineata    <NA>      Boyd Deep Canyon Desert ~     3
##  5 Anas             penelope     <NA>      Bodega Marine Reserve, C~     3
##  6 Aythya           marila       <NA>      Bodega Marine Reserve         1
##  7 Batrachoseps     nigriventris <NA>      Elliot Chaparral Reserve~     2
##  8 Botaurus         lentiginosus <NA>      Boyd Deep Canyon Desert ~     3
##  9 Bubulcus         ibis         <NA>      Boyd Deep Canyon Desert ~     3
## 10 Callisaurus      draconoides  <NA>      Boyd Deep Canyon Desert ~     2
## # ... with 42 more rows
```

