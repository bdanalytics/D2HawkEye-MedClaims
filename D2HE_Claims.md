# D2HawkEye Medical Claims: PoorCare classification
bdanalytics  

**  **    
**Date: (Tue) Mar 24, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/quality.csv  
    New:        <prdct_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(caTools))
suppressPackageStartupMessages(require(plyr))

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_predict_dataset <- FALSE    # or TRUE
glb_split_entity_predict_datasets <- TRUE   # or FALSE
glb_sample_predict_obs <- "sample"          # or "condition"
glb_sample_size_split <- 0.25               # > 0 & < 1 

glb_predct_var <- "PoorCare"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- c("MemberID")                # or NULL

glb_exclude_vars_as_features <- glb_id_vars     # or NULL                      
# List chrs converted into factors; num/int transformed  
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
    c("PoorCare.fctr", "PoorCare.predict", "PoorCare.predict.proba",
      "StartedOnCombination")     # or NULL
                                      )
# List feats that shd be excluded due to known causation by prediction variable
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("<col_name>")     # or NULL
#                                       )

glb_is_regression <- FALSE; glb_is_classification <- TRUE

glb_mdl <- glb_sel_mdl <- NULL
glb_models_df <- data.frame()

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/quality.csv", 
    comment="glb_entity_df", force_header=TRUE, 
    print_diagn=(glb_is_separate_predict_dataset | 
                !glb_split_entity_predict_datasets))
```

```
## [1] "Reading file ./data/quality.csv..."
## [1] "dimensions of data in ./data/quality.csv: 131 rows x 14 cols"
```

```r
if (glb_is_separate_predict_dataset) {
    glb_predct_df <- myimport_data(
        url="<prdct_url>", 
        comment="glb_predct_df", force_header=TRUE, print_diagn=TRUE)
} else {
    if (!glb_split_entity_predict_datasets) {
        stop("Not implemented yet") 
        glb_predct_df <- glb_entity_df[sample(1:nrow(glb_entity_df),
                                          max(2, nrow(glb_entity_df) / 1000)),]                    
    } else {
        if (glb_sample_predict_obs == "condition") {
            stop("Not implemented yet") 
#     glb_predct_df <- subset(glb_entity_df, <condition>)        
        } else { 
            if (glb_sample_predict_obs == "sample") {
                set.seed(88)
                split <- sample.split(glb_entity_df[, glb_predct_var], 
                                      SplitRatio=(1-glb_sample_size_split))
                glb_predct_df <- glb_entity_df[!split, ] 
                glb_entity_df <- glb_entity_df[split ,]
            }
        }    
    }
    comment(glb_predct_df) <- "glb_predct_df"
    myprint_df(glb_predct_df)
    str(glb_predct_df)

    if (glb_split_entity_predict_datasets) {
        myprint_df(glb_entity_df)
        str(glb_entity_df)        
    }
}         
```

```
##    MemberID InpatientDays ERVisits OfficeVisits Narcotics
## 5         5             8        2           19         3
## 7         7            16        1            8         1
## 9         9             2        1            4         3
## 10       10             4        2            0         2
## 12       12             0        0            7         4
## 25       25             0        0           14         1
##    DaysSinceLastERVisit Pain TotalVisits ProviderCount MedicalClaims
## 5              449.0000   10          29            24            51
## 7              173.9583    4          25            19            40
## 9               45.0000    5           7            28            20
## 10             104.0000    2           6            21            17
## 12             731.0000    0           7             8            23
## 25             731.0000    0          14            18            33
##    ClaimLines StartedOnCombination AcuteDrugGapSmall PoorCare
## 5         204                FALSE                 0        0
## 7         261                FALSE                 0        0
## 9          98                FALSE                 0        1
## 10         66                FALSE                 0        0
## 12         41                FALSE                 2        0
## 25         82                FALSE                 2        0
##     MemberID InpatientDays ERVisits OfficeVisits Narcotics
## 10        10             4        2            0         2
## 37        37             2        2            9         0
## 84        84             1        7           26        46
## 110      110             0        0            7        21
## 114      114             0        0           14        25
## 124      124             5        1           17         0
##     DaysSinceLastERVisit Pain TotalVisits ProviderCount MedicalClaims
## 10                   104    2           6            21            17
## 37                    72    4          13            31            50
## 84                    87   53          34            82           165
## 110                  731    0           7            24            18
## 114                  731    2          14            14            31
## 124                  380   20          23            20            32
##     ClaimLines StartedOnCombination AcuteDrugGapSmall PoorCare
## 10          66                FALSE                 0        0
## 37         217                FALSE                 0        0
## 84         559                FALSE                17        1
## 110        151                FALSE                 8        1
## 114        107                FALSE                13        1
## 124         84                FALSE                 1        0
##     MemberID InpatientDays ERVisits OfficeVisits Narcotics
## 114      114             0        0           14        25
## 118      118             0        2           16         3
## 121      121             0        0            9         1
## 124      124             5        1           17         0
## 127      127             1        1            5         3
## 131      131            30        1           22         3
##     DaysSinceLastERVisit Pain TotalVisits ProviderCount MedicalClaims
## 114             731.0000    2          14            14            31
## 118             247.9583   49          18            49            41
## 121             731.0000    3           9            21            41
## 124             380.0000   20          23            20            32
## 127             444.0000    0           7            11            11
## 131             452.0000   38          53            20           103
##     ClaimLines StartedOnCombination AcuteDrugGapSmall PoorCare
## 114        107                FALSE                13        1
## 118        120                FALSE                 0        0
## 121        118                FALSE                 2        0
## 124         84                FALSE                 1        0
## 127         36                FALSE                 0        0
## 131        189                FALSE                13        0
## 'data.frame':	32 obs. of  14 variables:
##  $ MemberID            : int  5 7 9 10 12 25 30 31 32 33 ...
##  $ InpatientDays       : int  8 16 2 4 0 0 13 5 10 0 ...
##  $ ERVisits            : int  2 1 1 2 0 0 5 2 4 0 ...
##  $ OfficeVisits        : int  19 8 4 0 7 14 21 2 45 6 ...
##  $ Narcotics           : int  3 1 3 2 4 1 6 0 0 0 ...
##  $ DaysSinceLastERVisit: num  449 174 45 104 731 ...
##  $ Pain                : int  10 4 5 2 0 0 60 0 32 17 ...
##  $ TotalVisits         : int  29 25 7 6 7 14 39 9 59 6 ...
##  $ ProviderCount       : int  24 19 28 21 8 18 36 24 63 14 ...
##  $ MedicalClaims       : int  51 40 20 17 23 33 45 19 133 35 ...
##  $ ClaimLines          : int  204 261 98 66 41 82 248 108 577 83 ...
##  $ StartedOnCombination: logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ AcuteDrugGapSmall   : int  0 0 0 0 2 2 1 0 0 1 ...
##  $ PoorCare            : int  0 0 1 0 0 0 1 0 0 0 ...
##  - attr(*, "comment")= chr "glb_predct_df"
##   MemberID InpatientDays ERVisits OfficeVisits Narcotics
## 1        1             0        0           18         1
## 2        2             1        1            6         1
## 3        3             0        0            5         3
## 4        4             0        1           19         0
## 6        6             2        0            9         2
## 8        8             2        0            8         0
##   DaysSinceLastERVisit Pain TotalVisits ProviderCount MedicalClaims
## 1                  731   10          18            21            93
## 2                  411    0           8            27            19
## 3                  731   10           5            16            27
## 4                  158   34          20            14            59
## 6                  731    6          11            40            53
## 8                  731    5          10            11            28
##   ClaimLines StartedOnCombination AcuteDrugGapSmall PoorCare
## 1        222                FALSE                 0        0
## 2        115                FALSE                 1        0
## 3        148                FALSE                 5        0
## 4        242                FALSE                 0        0
## 6        156                FALSE                 4        1
## 8         87                FALSE                 0        0
##     MemberID InpatientDays ERVisits OfficeVisits Narcotics
## 21        21             5        4           14         1
## 68        68             0        0            9         2
## 69        69             0        0            6         0
## 87        87             1        0            7         4
## 97        97             1        1           18         4
## 106      106             2        0           28        59
##     DaysSinceLastERVisit Pain TotalVisits ProviderCount MedicalClaims
## 21                   132   38          23            27            49
## 68                   731    3           9            15            21
## 69                   731    3           6            31            27
## 87                   731   10           8            15            37
## 97                   330   18          20            30            81
## 106                  731   10          30            33            47
##     ClaimLines StartedOnCombination AcuteDrugGapSmall PoorCare
## 21         148                 TRUE                 0        1
## 68          47                FALSE                 0        0
## 69          89                 TRUE                 0        0
## 87         148                FALSE                 2        0
## 97         270                FALSE                 0        0
## 106         89                FALSE                71        1
##     MemberID InpatientDays ERVisits OfficeVisits Narcotics
## 123      123             2        0            7         1
## 125      125             0        0           23         0
## 126      126             0        0            6         0
## 128      128             1        0            3         2
## 129      129            15       11            5         9
## 130      130             0        2           14         1
##     DaysSinceLastERVisit Pain TotalVisits ProviderCount MedicalClaims
## 123             731.0000    2           9            24            18
## 125             731.0000    0          23            34            58
## 126             731.0000    0           6            21            25
## 128             731.0000    0           4            35            18
## 129             180.9583   95          31            56            43
## 130             216.9583    5          16            26            41
##     ClaimLines StartedOnCombination AcuteDrugGapSmall PoorCare
## 123         79                FALSE                 0        0
## 125        121                FALSE                 3        1
## 126         51                FALSE                 5        0
## 128        106                FALSE                 2        0
## 129        265                FALSE                 3        0
## 130        138                FALSE                 1        1
## 'data.frame':	99 obs. of  14 variables:
##  $ MemberID            : int  1 2 3 4 6 8 11 13 14 15 ...
##  $ InpatientDays       : int  0 1 0 0 2 2 6 0 1 6 ...
##  $ ERVisits            : int  0 1 0 1 0 0 5 1 1 2 ...
##  $ OfficeVisits        : int  18 6 5 19 9 8 20 3 20 31 ...
##  $ Narcotics           : int  1 1 3 0 2 0 2 1 3 3 ...
##  $ DaysSinceLastERVisit: num  731 411 731 158 731 ...
##  $ Pain                : int  10 0 10 34 6 5 9 23 16 70 ...
##  $ TotalVisits         : int  18 8 5 20 11 10 31 4 22 39 ...
##  $ ProviderCount       : int  21 27 16 14 40 11 19 13 18 28 ...
##  $ MedicalClaims       : int  93 19 27 59 53 28 43 18 48 101 ...
##  $ ClaimLines          : int  222 115 148 242 156 87 126 70 133 233 ...
##  $ StartedOnCombination: logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ AcuteDrugGapSmall   : int  0 1 5 0 4 0 2 0 0 0 ...
##  $ PoorCare            : int  0 0 0 0 1 0 0 0 0 0 ...
##  - attr(*, "comment")= chr "glb_entity_df"
```

```r
script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# MemberID numbers the patients from 1 to 131, and is just an identifying number.
# InpatientDays is the number of inpatient visits, or number of days the person spent in the hospital.
# ERVisits is the number of times the patient visited the emergency room.
# OfficeVisits is the number of times the patient visited any doctor's office.
# Narcotics is the number of prescriptions the patient had for narcotics.
# DaysSinceLastERVisit is the number of days between the patient's last emergency room visit and the end of the study period (set to the length of the study period if they never visited the ER). 
# Pain is the number of visits for which the patient complained about pain.
# TotalVisits is the total number of times the patient visited any healthcare provider.
# ProviderCount is the number of providers that served the patient.
# MedicalClaims is the number of days on which the patient had a medical claim.
# ClaimLines is the total number of medical claims.
# StartedOnCombination is whether or not the patient was started on a combination of drugs to treat their diabetes (TRUE or FALSE).
# AcuteDrugGapSmall is the fraction of acute drugs that were refilled quickly after the prescription ran out.
# PoorCare is the outcome or dependent variable, and is equal to 1 if the patient had poor care, and equal to 0 if the patient had good care.

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, obs_twin_df) {
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>)
        StartedOnCombination.fctr=as.factor(StartedOnCombination),

        PoorCare.fctr=factor(PoorCare, 
                    as.factor(union(obs_df$PoorCare, obs_twin_df$PoorCare))) 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<max_n_val>") 

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(<col.name>)        
                        )

    # If levels of a factor are different across obs_df & glb_predct_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df, glb_predct_df)
```

```
##     MemberID     InpatientDays       ERVisits       OfficeVisits 
##  Min.   :  1.0   Min.   : 0.000   Min.   : 0.000   Min.   : 0.0  
##  1st Qu.: 35.5   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 7.0  
##  Median : 69.0   Median : 0.000   Median : 1.000   Median :10.0  
##  Mean   : 66.7   Mean   : 2.283   Mean   : 1.475   Mean   :12.8  
##  3rd Qu.: 96.5   3rd Qu.: 3.000   3rd Qu.: 2.000   3rd Qu.:18.0  
##  Max.   :130.0   Max.   :19.000   Max.   :11.000   Max.   :46.0  
##    Narcotics      DaysSinceLastERVisit      Pain         TotalVisits   
##  Min.   : 0.000   Min.   :  6.0        Min.   :  0.00   Min.   : 0.00  
##  1st Qu.: 0.000   1st Qu.:242.5        1st Qu.:  1.00   1st Qu.: 8.00  
##  Median : 1.000   Median :649.0        Median :  9.00   Median :15.00  
##  Mean   : 4.566   Mean   :489.4        Mean   : 16.06   Mean   :16.56  
##  3rd Qu.: 3.000   3rd Qu.:731.0        3rd Qu.: 24.00   3rd Qu.:22.00  
##  Max.   :59.000   Max.   :731.0        Max.   :104.00   Max.   :65.00  
##  ProviderCount   MedicalClaims      ClaimLines    StartedOnCombination
##  Min.   : 5.00   Min.   : 12.00   Min.   : 20.0   Mode :logical       
##  1st Qu.:15.00   1st Qu.: 25.00   1st Qu.: 79.0   FALSE:95            
##  Median :20.00   Median : 36.00   Median :122.0   TRUE :4             
##  Mean   :23.35   Mean   : 41.88   Mean   :137.1   NA's :0             
##  3rd Qu.:30.00   3rd Qu.: 48.50   3rd Qu.:176.5                       
##  Max.   :79.00   Max.   :194.00   Max.   :410.0                       
##  AcuteDrugGapSmall    PoorCare      StartedOnCombination.fctr
##  Min.   : 0.000    Min.   :0.0000   FALSE:95                 
##  1st Qu.: 0.000    1st Qu.:0.0000   TRUE : 4                 
##  Median : 1.000    Median :0.0000                            
##  Mean   : 2.828    Mean   :0.2525                            
##  3rd Qu.: 3.000    3rd Qu.:0.5000                            
##  Max.   :71.000    Max.   :1.0000                            
##  PoorCare.fctr
##  0:74         
##  1:25         
##               
##               
##               
##               
##                  MemberID             InpatientDays 
##                         0                         0 
##                  ERVisits              OfficeVisits 
##                         0                         0 
##                 Narcotics      DaysSinceLastERVisit 
##                         0                         0 
##                      Pain               TotalVisits 
##                         0                         0 
##             ProviderCount             MedicalClaims 
##                         0                         0 
##                ClaimLines      StartedOnCombination 
##                         0                         0 
##         AcuteDrugGapSmall                  PoorCare 
##                         0                         0 
## StartedOnCombination.fctr             PoorCare.fctr 
##                         0                         0
```

```r
glb_predct_df <- add_new_diag_feats(glb_predct_df, glb_entity_df)
```

```
##     MemberID      InpatientDays       ERVisits      OfficeVisits  
##  Min.   :  5.00   Min.   : 0.000   Min.   :0.000   Min.   : 0.00  
##  1st Qu.: 31.75   1st Qu.: 0.000   1st Qu.:0.000   1st Qu.: 7.00  
##  Median : 50.50   Median : 0.500   Median :1.000   Median :14.00  
##  Mean   : 63.84   Mean   : 4.062   Mean   :1.562   Mean   :14.56  
##  3rd Qu.:108.50   3rd Qu.: 4.250   3rd Qu.:2.000   3rd Qu.:19.50  
##  Max.   :131.00   Max.   :30.000   Max.   :8.000   Max.   :45.00  
##    Narcotics      DaysSinceLastERVisit      Pain     TotalVisits   
##  Min.   : 0.000   Min.   : 30.00       Min.   : 0   Min.   : 3.00  
##  1st Qu.: 0.000   1st Qu.: 99.75       1st Qu.: 0   1st Qu.: 8.50  
##  Median : 1.000   Median :516.98       Median : 4   Median :16.00  
##  Mean   : 4.594   Mean   :453.09       Mean   :14   Mean   :20.19  
##  3rd Qu.: 3.000   3rd Qu.:731.00       3rd Qu.:20   3rd Qu.:25.00  
##  Max.   :46.000   Max.   :731.00       Max.   :60   Max.   :69.00  
##  ProviderCount   MedicalClaims      ClaimLines     StartedOnCombination
##  Min.   : 8.00   Min.   : 11.00   Min.   : 36.00   Mode :logical       
##  1st Qu.:17.00   1st Qu.: 30.00   1st Qu.: 89.75   FALSE:30            
##  Median :20.50   Median : 40.50   Median :114.50   TRUE :2             
##  Mean   :25.94   Mean   : 47.47   Mean   :160.62   NA's :0             
##  3rd Qu.:28.75   3rd Qu.: 50.00   3rd Qu.:192.75                       
##  Max.   :82.00   Max.   :165.00   Max.   :577.00                       
##  AcuteDrugGapSmall    PoorCare    StartedOnCombination.fctr PoorCare.fctr
##  Min.   : 0.000    Min.   :0.00   FALSE:30                  0:24         
##  1st Qu.: 0.000    1st Qu.:0.00   TRUE : 2                  1: 8         
##  Median : 0.000    Median :0.00                                          
##  Mean   : 2.281    Mean   :0.25                                          
##  3rd Qu.: 2.000    3rd Qu.:0.25                                          
##  Max.   :17.000    Max.   :1.00                                          
##                  MemberID             InpatientDays 
##                         0                         0 
##                  ERVisits              OfficeVisits 
##                         0                         0 
##                 Narcotics      DaysSinceLastERVisit 
##                         0                         0 
##                      Pain               TotalVisits 
##                         0                         0 
##             ProviderCount             MedicalClaims 
##                         0                         0 
##                ClaimLines      StartedOnCombination 
##                         0                         0 
##         AcuteDrugGapSmall                  PoorCare 
##                         0                         0 
## StartedOnCombination.fctr             PoorCare.fctr 
##                         0                         0
```

```r
#pairs(subset(glb_entity_df, select=-c(col_symbol)))

#   Histogram of predictor in glb_entity_df & glb_predct_df
# Check for glb_predct_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(glb_entity_df[which.max(glb_entity_df$<col_name>),])

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(xtabs(~ <col1_name>, glb_entity_df))
# print(xtabs(~ <col1_name> + <col2_name>, glb_entity_df))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>.NA, glb_entity_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
print(myplot_histogram(glb_entity_df, glb_predct_var))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](D2HE_Claims_files/figure-html/inspect_explore_data_1-1.png) 

```r
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))
print(myplot_scatter(glb_entity_df, "OfficeVisits", "Narcotics", colorcol_name="PoorCare.fctr"))
```

![](D2HE_Claims_files/figure-html/inspect_explore_data_1-2.png) 

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_predct_df), function(col) sum(is.na(glb_predct_df[, col]))))
# glb_entity_df <- na.omit(glb_entity_df)
# glb_predct_df <- na.omit(glb_predct_df)
# df[is.na(df)] <- 0

script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_predct_df <- mymap_codes(glb_predct_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_entity_df$<col_name>.fctr <- factor(glb_entity_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_predct_df$<col_name>)))
# glb_predct_df$<col_name>.fctr <- factor(glb_predct_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_predct_df$<col_name>)))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
```

## Step `3`: extract features

```r
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_entity_df$<col_name>), -2, na.pad=TRUE)
# glb_entity_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_predct_df$<col_name>), -2, na.pad=TRUE)
# glb_predct_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_predct_df[1, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df) - 1, 
#                                                    "<col_name>"]
# glb_predct_df[2, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     <new_col_name>=
#                     )

# glb_predct_df <- mutate(glb_predct_df,
#     <new_col_name>=
#                     )

# print(summary(glb_entity_df))
# print(summary(glb_predct_df))

# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_predct_df), function(col) sum(is.na(glb_predct_df[, col]))))

# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
## 7      select_features                4                0
```

## Step `4`: select features

```r
print(glb_feats_df <- myselect_features())
```

```
##                                        id       cor.y  cor.y.abs
## Narcotics                       Narcotics  0.40237389 0.40237389
## OfficeVisits                 OfficeVisits  0.40236225 0.40236225
## TotalVisits                   TotalVisits  0.36664685 0.36664685
## AcuteDrugGapSmall       AcuteDrugGapSmall  0.33364848 0.33364848
## ProviderCount               ProviderCount  0.22328055 0.22328055
## MedicalClaims               MedicalClaims  0.16082837 0.16082837
## DaysSinceLastERVisit DaysSinceLastERVisit -0.10704558 0.10704558
## ERVisits                         ERVisits  0.08633538 0.08633538
## ClaimLines                     ClaimLines  0.08194897 0.08194897
## InpatientDays               InpatientDays  0.05592793 0.05592793
## Pain                                 Pain  0.04336044 0.04336044
```

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, 
                    merge(glb_feats_df, mydelete_cor_features(), all.x=TRUE)))
```

```
## Loading required package: reshape2
```

```
##                        Narcotics OfficeVisits TotalVisits
## Narcotics             1.00000000   0.34081434   0.2258990
## OfficeVisits          0.34081434   1.00000000   0.8797902
## TotalVisits           0.22589896   0.87979017   1.0000000
## AcuteDrugGapSmall     0.70954318   0.21746834   0.1431560
## ProviderCount         0.23947333   0.28761079   0.4116145
## MedicalClaims         0.13969543   0.39845398   0.4337810
## DaysSinceLastERVisit  0.09825464  -0.14566059  -0.3512199
## ERVisits             -0.08679816   0.24807968   0.5742296
## ClaimLines            0.08287002   0.32042099   0.4824285
## InpatientDays        -0.10287053   0.02838842   0.4677934
## Pain                  0.06851293   0.29747261   0.4261948
##                      AcuteDrugGapSmall ProviderCount MedicalClaims
## Narcotics                   0.70954318     0.2394733    0.13969543
## OfficeVisits                0.21746834     0.2876108    0.39845398
## TotalVisits                 0.14315601     0.4116145    0.43378103
## AcuteDrugGapSmall           1.00000000     0.1223287    0.01014624
## ProviderCount               0.12232870     1.0000000    0.39733383
## MedicalClaims               0.01014624     0.3973338    1.00000000
## DaysSinceLastERVisit        0.13865706    -0.1753613   -0.16794022
## ERVisits                   -0.10232145     0.3356405    0.25009653
## ClaimLines                 -0.10263352     0.4734476    0.79506595
## InpatientDays              -0.04080193     0.3103759    0.16252006
## Pain                       -0.07699869     0.2792510    0.18052936
##                      DaysSinceLastERVisit    ERVisits  ClaimLines
## Narcotics                      0.09825464 -0.08679816  0.08287002
## OfficeVisits                  -0.14566059  0.24807968  0.32042099
## TotalVisits                   -0.35121994  0.57422962  0.48242854
## AcuteDrugGapSmall              0.13865706 -0.10232145 -0.10263352
## ProviderCount                 -0.17536127  0.33564048  0.47344760
## MedicalClaims                 -0.16794022  0.25009653  0.79506595
## DaysSinceLastERVisit           1.00000000 -0.74242241 -0.42122483
## ERVisits                      -0.74242241  1.00000000  0.48139110
## ClaimLines                    -0.42122483  0.48139110  1.00000000
## InpatientDays                 -0.23192101  0.48181472  0.35131396
## Pain                          -0.31331042  0.47574158  0.44394824
##                      InpatientDays        Pain
## Narcotics              -0.10287053  0.06851293
## OfficeVisits            0.02838842  0.29747261
## TotalVisits             0.46779339  0.42619476
## AcuteDrugGapSmall      -0.04080193 -0.07699869
## ProviderCount           0.31037595  0.27925100
## MedicalClaims           0.16252006  0.18052936
## DaysSinceLastERVisit   -0.23192101 -0.31331042
## ERVisits                0.48181472  0.47574158
## ClaimLines              0.35131396  0.44394824
## InpatientDays           1.00000000  0.24650846
## Pain                    0.24650846  1.00000000
##                       Narcotics OfficeVisits TotalVisits AcuteDrugGapSmall
## Narcotics            0.00000000   0.34081434   0.2258990        0.70954318
## OfficeVisits         0.34081434   0.00000000   0.8797902        0.21746834
## TotalVisits          0.22589896   0.87979017   0.0000000        0.14315601
## AcuteDrugGapSmall    0.70954318   0.21746834   0.1431560        0.00000000
## ProviderCount        0.23947333   0.28761079   0.4116145        0.12232870
## MedicalClaims        0.13969543   0.39845398   0.4337810        0.01014624
## DaysSinceLastERVisit 0.09825464   0.14566059   0.3512199        0.13865706
## ERVisits             0.08679816   0.24807968   0.5742296        0.10232145
## ClaimLines           0.08287002   0.32042099   0.4824285        0.10263352
## InpatientDays        0.10287053   0.02838842   0.4677934        0.04080193
## Pain                 0.06851293   0.29747261   0.4261948        0.07699869
##                      ProviderCount MedicalClaims DaysSinceLastERVisit
## Narcotics                0.2394733    0.13969543           0.09825464
## OfficeVisits             0.2876108    0.39845398           0.14566059
## TotalVisits              0.4116145    0.43378103           0.35121994
## AcuteDrugGapSmall        0.1223287    0.01014624           0.13865706
## ProviderCount            0.0000000    0.39733383           0.17536127
## MedicalClaims            0.3973338    0.00000000           0.16794022
## DaysSinceLastERVisit     0.1753613    0.16794022           0.00000000
## ERVisits                 0.3356405    0.25009653           0.74242241
## ClaimLines               0.4734476    0.79506595           0.42122483
## InpatientDays            0.3103759    0.16252006           0.23192101
## Pain                     0.2792510    0.18052936           0.31331042
##                        ERVisits ClaimLines InpatientDays       Pain
## Narcotics            0.08679816 0.08287002    0.10287053 0.06851293
## OfficeVisits         0.24807968 0.32042099    0.02838842 0.29747261
## TotalVisits          0.57422962 0.48242854    0.46779339 0.42619476
## AcuteDrugGapSmall    0.10232145 0.10263352    0.04080193 0.07699869
## ProviderCount        0.33564048 0.47344760    0.31037595 0.27925100
## MedicalClaims        0.25009653 0.79506595    0.16252006 0.18052936
## DaysSinceLastERVisit 0.74242241 0.42122483    0.23192101 0.31331042
## ERVisits             0.00000000 0.48139110    0.48181472 0.47574158
## ClaimLines           0.48139110 0.00000000    0.35131396 0.44394824
## InpatientDays        0.48181472 0.35131396    0.00000000 0.24650846
## Pain                 0.47574158 0.44394824    0.24650846 0.00000000
## [1] "cor(OfficeVisits, TotalVisits)=0.8798"
```

![](D2HE_Claims_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(PoorCare, OfficeVisits)=0.4024"
## [1] "cor(PoorCare, TotalVisits)=0.3666"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## Warning in mydelete_cor_features(): Dropping TotalVisits as a feature
```

![](D2HE_Claims_files/figure-html/remove_correlated_features-2.png) 

```
##                                        id       cor.y  cor.y.abs
## Narcotics                       Narcotics  0.40237389 0.40237389
## OfficeVisits                 OfficeVisits  0.40236225 0.40236225
## AcuteDrugGapSmall       AcuteDrugGapSmall  0.33364848 0.33364848
## ProviderCount               ProviderCount  0.22328055 0.22328055
## MedicalClaims               MedicalClaims  0.16082837 0.16082837
## DaysSinceLastERVisit DaysSinceLastERVisit -0.10704558 0.10704558
## ERVisits                         ERVisits  0.08633538 0.08633538
## ClaimLines                     ClaimLines  0.08194897 0.08194897
## InpatientDays               InpatientDays  0.05592793 0.05592793
## Pain                                 Pain  0.04336044 0.04336044
##                        Narcotics OfficeVisits AcuteDrugGapSmall
## Narcotics             1.00000000   0.34081434        0.70954318
## OfficeVisits          0.34081434   1.00000000        0.21746834
## AcuteDrugGapSmall     0.70954318   0.21746834        1.00000000
## ProviderCount         0.23947333   0.28761079        0.12232870
## MedicalClaims         0.13969543   0.39845398        0.01014624
## DaysSinceLastERVisit  0.09825464  -0.14566059        0.13865706
## ERVisits             -0.08679816   0.24807968       -0.10232145
## ClaimLines            0.08287002   0.32042099       -0.10263352
## InpatientDays        -0.10287053   0.02838842       -0.04080193
## Pain                  0.06851293   0.29747261       -0.07699869
##                      ProviderCount MedicalClaims DaysSinceLastERVisit
## Narcotics                0.2394733    0.13969543           0.09825464
## OfficeVisits             0.2876108    0.39845398          -0.14566059
## AcuteDrugGapSmall        0.1223287    0.01014624           0.13865706
## ProviderCount            1.0000000    0.39733383          -0.17536127
## MedicalClaims            0.3973338    1.00000000          -0.16794022
## DaysSinceLastERVisit    -0.1753613   -0.16794022           1.00000000
## ERVisits                 0.3356405    0.25009653          -0.74242241
## ClaimLines               0.4734476    0.79506595          -0.42122483
## InpatientDays            0.3103759    0.16252006          -0.23192101
## Pain                     0.2792510    0.18052936          -0.31331042
##                         ERVisits  ClaimLines InpatientDays        Pain
## Narcotics            -0.08679816  0.08287002   -0.10287053  0.06851293
## OfficeVisits          0.24807968  0.32042099    0.02838842  0.29747261
## AcuteDrugGapSmall    -0.10232145 -0.10263352   -0.04080193 -0.07699869
## ProviderCount         0.33564048  0.47344760    0.31037595  0.27925100
## MedicalClaims         0.25009653  0.79506595    0.16252006  0.18052936
## DaysSinceLastERVisit -0.74242241 -0.42122483   -0.23192101 -0.31331042
## ERVisits              1.00000000  0.48139110    0.48181472  0.47574158
## ClaimLines            0.48139110  1.00000000    0.35131396  0.44394824
## InpatientDays         0.48181472  0.35131396    1.00000000  0.24650846
## Pain                  0.47574158  0.44394824    0.24650846  1.00000000
##                       Narcotics OfficeVisits AcuteDrugGapSmall
## Narcotics            0.00000000   0.34081434        0.70954318
## OfficeVisits         0.34081434   0.00000000        0.21746834
## AcuteDrugGapSmall    0.70954318   0.21746834        0.00000000
## ProviderCount        0.23947333   0.28761079        0.12232870
## MedicalClaims        0.13969543   0.39845398        0.01014624
## DaysSinceLastERVisit 0.09825464   0.14566059        0.13865706
## ERVisits             0.08679816   0.24807968        0.10232145
## ClaimLines           0.08287002   0.32042099        0.10263352
## InpatientDays        0.10287053   0.02838842        0.04080193
## Pain                 0.06851293   0.29747261        0.07699869
##                      ProviderCount MedicalClaims DaysSinceLastERVisit
## Narcotics                0.2394733    0.13969543           0.09825464
## OfficeVisits             0.2876108    0.39845398           0.14566059
## AcuteDrugGapSmall        0.1223287    0.01014624           0.13865706
## ProviderCount            0.0000000    0.39733383           0.17536127
## MedicalClaims            0.3973338    0.00000000           0.16794022
## DaysSinceLastERVisit     0.1753613    0.16794022           0.00000000
## ERVisits                 0.3356405    0.25009653           0.74242241
## ClaimLines               0.4734476    0.79506595           0.42122483
## InpatientDays            0.3103759    0.16252006           0.23192101
## Pain                     0.2792510    0.18052936           0.31331042
##                        ERVisits ClaimLines InpatientDays       Pain
## Narcotics            0.08679816 0.08287002    0.10287053 0.06851293
## OfficeVisits         0.24807968 0.32042099    0.02838842 0.29747261
## AcuteDrugGapSmall    0.10232145 0.10263352    0.04080193 0.07699869
## ProviderCount        0.33564048 0.47344760    0.31037595 0.27925100
## MedicalClaims        0.25009653 0.79506595    0.16252006 0.18052936
## DaysSinceLastERVisit 0.74242241 0.42122483    0.23192101 0.31331042
## ERVisits             0.00000000 0.48139110    0.48181472 0.47574158
## ClaimLines           0.48139110 0.00000000    0.35131396 0.44394824
## InpatientDays        0.48181472 0.35131396    0.00000000 0.24650846
## Pain                 0.47574158 0.44394824    0.24650846 0.00000000
## [1] "cor(MedicalClaims, ClaimLines)=0.7951"
```

![](D2HE_Claims_files/figure-html/remove_correlated_features-3.png) 

```
## [1] "cor(PoorCare, MedicalClaims)=0.1608"
## [1] "cor(PoorCare, ClaimLines)=0.0819"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## Warning in mydelete_cor_features(): Dropping ClaimLines as a feature
```

![](D2HE_Claims_files/figure-html/remove_correlated_features-4.png) 

```
##                                        id       cor.y  cor.y.abs
## Narcotics                       Narcotics  0.40237389 0.40237389
## OfficeVisits                 OfficeVisits  0.40236225 0.40236225
## AcuteDrugGapSmall       AcuteDrugGapSmall  0.33364848 0.33364848
## ProviderCount               ProviderCount  0.22328055 0.22328055
## MedicalClaims               MedicalClaims  0.16082837 0.16082837
## DaysSinceLastERVisit DaysSinceLastERVisit -0.10704558 0.10704558
## ERVisits                         ERVisits  0.08633538 0.08633538
## InpatientDays               InpatientDays  0.05592793 0.05592793
## Pain                                 Pain  0.04336044 0.04336044
##                        Narcotics OfficeVisits AcuteDrugGapSmall
## Narcotics             1.00000000   0.34081434        0.70954318
## OfficeVisits          0.34081434   1.00000000        0.21746834
## AcuteDrugGapSmall     0.70954318   0.21746834        1.00000000
## ProviderCount         0.23947333   0.28761079        0.12232870
## MedicalClaims         0.13969543   0.39845398        0.01014624
## DaysSinceLastERVisit  0.09825464  -0.14566059        0.13865706
## ERVisits             -0.08679816   0.24807968       -0.10232145
## InpatientDays        -0.10287053   0.02838842       -0.04080193
## Pain                  0.06851293   0.29747261       -0.07699869
##                      ProviderCount MedicalClaims DaysSinceLastERVisit
## Narcotics                0.2394733    0.13969543           0.09825464
## OfficeVisits             0.2876108    0.39845398          -0.14566059
## AcuteDrugGapSmall        0.1223287    0.01014624           0.13865706
## ProviderCount            1.0000000    0.39733383          -0.17536127
## MedicalClaims            0.3973338    1.00000000          -0.16794022
## DaysSinceLastERVisit    -0.1753613   -0.16794022           1.00000000
## ERVisits                 0.3356405    0.25009653          -0.74242241
## InpatientDays            0.3103759    0.16252006          -0.23192101
## Pain                     0.2792510    0.18052936          -0.31331042
##                         ERVisits InpatientDays        Pain
## Narcotics            -0.08679816   -0.10287053  0.06851293
## OfficeVisits          0.24807968    0.02838842  0.29747261
## AcuteDrugGapSmall    -0.10232145   -0.04080193 -0.07699869
## ProviderCount         0.33564048    0.31037595  0.27925100
## MedicalClaims         0.25009653    0.16252006  0.18052936
## DaysSinceLastERVisit -0.74242241   -0.23192101 -0.31331042
## ERVisits              1.00000000    0.48181472  0.47574158
## InpatientDays         0.48181472    1.00000000  0.24650846
## Pain                  0.47574158    0.24650846  1.00000000
##                       Narcotics OfficeVisits AcuteDrugGapSmall
## Narcotics            0.00000000   0.34081434        0.70954318
## OfficeVisits         0.34081434   0.00000000        0.21746834
## AcuteDrugGapSmall    0.70954318   0.21746834        0.00000000
## ProviderCount        0.23947333   0.28761079        0.12232870
## MedicalClaims        0.13969543   0.39845398        0.01014624
## DaysSinceLastERVisit 0.09825464   0.14566059        0.13865706
## ERVisits             0.08679816   0.24807968        0.10232145
## InpatientDays        0.10287053   0.02838842        0.04080193
## Pain                 0.06851293   0.29747261        0.07699869
##                      ProviderCount MedicalClaims DaysSinceLastERVisit
## Narcotics                0.2394733    0.13969543           0.09825464
## OfficeVisits             0.2876108    0.39845398           0.14566059
## AcuteDrugGapSmall        0.1223287    0.01014624           0.13865706
## ProviderCount            0.0000000    0.39733383           0.17536127
## MedicalClaims            0.3973338    0.00000000           0.16794022
## DaysSinceLastERVisit     0.1753613    0.16794022           0.00000000
## ERVisits                 0.3356405    0.25009653           0.74242241
## InpatientDays            0.3103759    0.16252006           0.23192101
## Pain                     0.2792510    0.18052936           0.31331042
##                        ERVisits InpatientDays       Pain
## Narcotics            0.08679816    0.10287053 0.06851293
## OfficeVisits         0.24807968    0.02838842 0.29747261
## AcuteDrugGapSmall    0.10232145    0.04080193 0.07699869
## ProviderCount        0.33564048    0.31037595 0.27925100
## MedicalClaims        0.25009653    0.16252006 0.18052936
## DaysSinceLastERVisit 0.74242241    0.23192101 0.31331042
## ERVisits             0.00000000    0.48181472 0.47574158
## InpatientDays        0.48181472    0.00000000 0.24650846
## Pain                 0.47574158    0.24650846 0.00000000
## [1] "cor(DaysSinceLastERVisit, ERVisits)=-0.7424"
```

![](D2HE_Claims_files/figure-html/remove_correlated_features-5.png) 

```
## [1] "cor(PoorCare, DaysSinceLastERVisit)=-0.1070"
## [1] "cor(PoorCare, ERVisits)=0.0863"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## Warning in mydelete_cor_features(): Dropping ERVisits as a feature
```

![](D2HE_Claims_files/figure-html/remove_correlated_features-6.png) 

```
##                                        id       cor.y  cor.y.abs
## Narcotics                       Narcotics  0.40237389 0.40237389
## OfficeVisits                 OfficeVisits  0.40236225 0.40236225
## AcuteDrugGapSmall       AcuteDrugGapSmall  0.33364848 0.33364848
## ProviderCount               ProviderCount  0.22328055 0.22328055
## MedicalClaims               MedicalClaims  0.16082837 0.16082837
## DaysSinceLastERVisit DaysSinceLastERVisit -0.10704558 0.10704558
## InpatientDays               InpatientDays  0.05592793 0.05592793
## Pain                                 Pain  0.04336044 0.04336044
##                        Narcotics OfficeVisits AcuteDrugGapSmall
## Narcotics             1.00000000   0.34081434        0.70954318
## OfficeVisits          0.34081434   1.00000000        0.21746834
## AcuteDrugGapSmall     0.70954318   0.21746834        1.00000000
## ProviderCount         0.23947333   0.28761079        0.12232870
## MedicalClaims         0.13969543   0.39845398        0.01014624
## DaysSinceLastERVisit  0.09825464  -0.14566059        0.13865706
## InpatientDays        -0.10287053   0.02838842       -0.04080193
## Pain                  0.06851293   0.29747261       -0.07699869
##                      ProviderCount MedicalClaims DaysSinceLastERVisit
## Narcotics                0.2394733    0.13969543           0.09825464
## OfficeVisits             0.2876108    0.39845398          -0.14566059
## AcuteDrugGapSmall        0.1223287    0.01014624           0.13865706
## ProviderCount            1.0000000    0.39733383          -0.17536127
## MedicalClaims            0.3973338    1.00000000          -0.16794022
## DaysSinceLastERVisit    -0.1753613   -0.16794022           1.00000000
## InpatientDays            0.3103759    0.16252006          -0.23192101
## Pain                     0.2792510    0.18052936          -0.31331042
##                      InpatientDays        Pain
## Narcotics              -0.10287053  0.06851293
## OfficeVisits            0.02838842  0.29747261
## AcuteDrugGapSmall      -0.04080193 -0.07699869
## ProviderCount           0.31037595  0.27925100
## MedicalClaims           0.16252006  0.18052936
## DaysSinceLastERVisit   -0.23192101 -0.31331042
## InpatientDays           1.00000000  0.24650846
## Pain                    0.24650846  1.00000000
##                       Narcotics OfficeVisits AcuteDrugGapSmall
## Narcotics            0.00000000   0.34081434        0.70954318
## OfficeVisits         0.34081434   0.00000000        0.21746834
## AcuteDrugGapSmall    0.70954318   0.21746834        0.00000000
## ProviderCount        0.23947333   0.28761079        0.12232870
## MedicalClaims        0.13969543   0.39845398        0.01014624
## DaysSinceLastERVisit 0.09825464   0.14566059        0.13865706
## InpatientDays        0.10287053   0.02838842        0.04080193
## Pain                 0.06851293   0.29747261        0.07699869
##                      ProviderCount MedicalClaims DaysSinceLastERVisit
## Narcotics                0.2394733    0.13969543           0.09825464
## OfficeVisits             0.2876108    0.39845398           0.14566059
## AcuteDrugGapSmall        0.1223287    0.01014624           0.13865706
## ProviderCount            0.0000000    0.39733383           0.17536127
## MedicalClaims            0.3973338    0.00000000           0.16794022
## DaysSinceLastERVisit     0.1753613    0.16794022           0.00000000
## InpatientDays            0.3103759    0.16252006           0.23192101
## Pain                     0.2792510    0.18052936           0.31331042
##                      InpatientDays       Pain
## Narcotics               0.10287053 0.06851293
## OfficeVisits            0.02838842 0.29747261
## AcuteDrugGapSmall       0.04080193 0.07699869
## ProviderCount           0.31037595 0.27925100
## MedicalClaims           0.16252006 0.18052936
## DaysSinceLastERVisit    0.23192101 0.31331042
## InpatientDays           0.00000000 0.24650846
## Pain                    0.24650846 0.00000000
## [1] "cor(Narcotics, AcuteDrugGapSmall)=0.7095"
```

![](D2HE_Claims_files/figure-html/remove_correlated_features-7.png) 

```
## [1] "cor(PoorCare, Narcotics)=0.4024"
## [1] "cor(PoorCare, AcuteDrugGapSmall)=0.3336"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## Warning in mydelete_cor_features(): Dropping AcuteDrugGapSmall as a
## feature
```

![](D2HE_Claims_files/figure-html/remove_correlated_features-8.png) 

```
##                                        id       cor.y  cor.y.abs
## Narcotics                       Narcotics  0.40237389 0.40237389
## OfficeVisits                 OfficeVisits  0.40236225 0.40236225
## ProviderCount               ProviderCount  0.22328055 0.22328055
## MedicalClaims               MedicalClaims  0.16082837 0.16082837
## DaysSinceLastERVisit DaysSinceLastERVisit -0.10704558 0.10704558
## InpatientDays               InpatientDays  0.05592793 0.05592793
## Pain                                 Pain  0.04336044 0.04336044
##                        Narcotics OfficeVisits ProviderCount MedicalClaims
## Narcotics             1.00000000   0.34081434     0.2394733     0.1396954
## OfficeVisits          0.34081434   1.00000000     0.2876108     0.3984540
## ProviderCount         0.23947333   0.28761079     1.0000000     0.3973338
## MedicalClaims         0.13969543   0.39845398     0.3973338     1.0000000
## DaysSinceLastERVisit  0.09825464  -0.14566059    -0.1753613    -0.1679402
## InpatientDays        -0.10287053   0.02838842     0.3103759     0.1625201
## Pain                  0.06851293   0.29747261     0.2792510     0.1805294
##                      DaysSinceLastERVisit InpatientDays        Pain
## Narcotics                      0.09825464   -0.10287053  0.06851293
## OfficeVisits                  -0.14566059    0.02838842  0.29747261
## ProviderCount                 -0.17536127    0.31037595  0.27925100
## MedicalClaims                 -0.16794022    0.16252006  0.18052936
## DaysSinceLastERVisit           1.00000000   -0.23192101 -0.31331042
## InpatientDays                 -0.23192101    1.00000000  0.24650846
## Pain                          -0.31331042    0.24650846  1.00000000
##                       Narcotics OfficeVisits ProviderCount MedicalClaims
## Narcotics            0.00000000   0.34081434     0.2394733     0.1396954
## OfficeVisits         0.34081434   0.00000000     0.2876108     0.3984540
## ProviderCount        0.23947333   0.28761079     0.0000000     0.3973338
## MedicalClaims        0.13969543   0.39845398     0.3973338     0.0000000
## DaysSinceLastERVisit 0.09825464   0.14566059     0.1753613     0.1679402
## InpatientDays        0.10287053   0.02838842     0.3103759     0.1625201
## Pain                 0.06851293   0.29747261     0.2792510     0.1805294
##                      DaysSinceLastERVisit InpatientDays       Pain
## Narcotics                      0.09825464    0.10287053 0.06851293
## OfficeVisits                   0.14566059    0.02838842 0.29747261
## ProviderCount                  0.17536127    0.31037595 0.27925100
## MedicalClaims                  0.16794022    0.16252006 0.18052936
## DaysSinceLastERVisit           0.00000000    0.23192101 0.31331042
## InpatientDays                  0.23192101    0.00000000 0.24650846
## Pain                           0.31331042    0.24650846 0.00000000
##                      id       cor.y  cor.y.abs cor.low
## 7             Narcotics  0.40237389 0.40237389       1
## 8          OfficeVisits  0.40236225 0.40236225       1
## 11          TotalVisits  0.36664685 0.36664685      NA
## 1     AcuteDrugGapSmall  0.33364848 0.33364848      NA
## 10        ProviderCount  0.22328055 0.22328055       1
## 6         MedicalClaims  0.16082837 0.16082837       1
## 4              ERVisits  0.08633538 0.08633538      NA
## 2            ClaimLines  0.08194897 0.08194897      NA
## 5         InpatientDays  0.05592793 0.05592793       1
## 9                  Pain  0.04336044 0.04336044       1
## 3  DaysSinceLastERVisit -0.10704558 0.10704558       1
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
## 9                 run_models                5                0
```

## Step `5`: run models

```r
max_cor_y_x_var <- subset(glb_feats_df, cor.low == 1)[1, "id"]

#   Regression:
if (glb_is_regression) {
    #   Linear:
    myrun_mdl_fn <- myrun_mdl_lm
}    

#   Classification:
if (glb_is_classification) {
    #   Logit Regression:
    myrun_mdl_fn <- myrun_mdl_glm
}    
    
# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## Loading required package: ROCR
```

```
## Warning: package 'ROCR' was built under R version 3.1.3
```

```
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9261  -0.6674  -0.6119  -0.2146   1.8803  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.58064    0.28932  -5.463 4.67e-08 ***
## Narcotics    0.09604    0.03077   3.122   0.0018 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  97.341  on 97  degrees of freedom
## AIC: 101.34
## 
## Number of Fisher Scoring iterations: 4
## 
##       feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit
## 1 Narcotics    99       NA       NA           NA 646.6943      NA 101.3409
##     auc.fit   auc.OOB
## 1 0.6762162 0.8567708
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
if (nrow(subset(glb_feats_df, is.na(cor.low))) > 0)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)    
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.5299  -0.7013  -0.5767  -0.1183   2.2312  
## 
## Coefficients:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                 -1.2773800  0.3110671  -4.106 4.02e-05 ***
## Narcotics                   -0.7444180  0.4246497  -1.753   0.0796 .  
## Narcotics:TotalVisits        0.0202579  0.0124301   1.630   0.1032    
## Narcotics:AcuteDrugGapSmall  0.0375859  0.0159713   2.353   0.0186 *  
## Narcotics:ERVisits          -0.0257155  0.0261365  -0.984   0.3252    
## Narcotics:ClaimLines         0.0011282  0.0007754   1.455   0.1457    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  87.753  on 93  degrees of freedom
## AIC: 99.753
## 
## Number of Fisher Scoring iterations: 8
## 
##                                                                                                     feats
## 1                                                                                               Narcotics
## 2 Narcotics, Narcotics:TotalVisits, Narcotics:AcuteDrugGapSmall, Narcotics:ERVisits, Narcotics:ClaimLines
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 1    99       NA       NA           NA 646.6943      NA 101.3409 0.6762162
## 2    99       NA       NA           NA 720.1554      NA  99.7530 0.6913514
##     auc.OOB
## 1 0.8567708
## 2 0.6015625
```

```r
# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, cor.low == 1)[, "id"],
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.76488  -0.65881  -0.44678  -0.00084   2.11058  
## 
## Coefficients:
##                       Estimate Std. Error z value Pr(>|z|)  
## (Intercept)          -2.239032   0.941990  -2.377   0.0175 *
## Narcotics             0.083287   0.033709   2.471   0.0135 *
## OfficeVisits          0.100613   0.041795   2.407   0.0161 *
## ProviderCount         0.019113   0.026951   0.709   0.4782  
## MedicalClaims        -0.010897   0.015282  -0.713   0.4758  
## InpatientDays         0.071491   0.075358   0.949   0.3428  
## Pain                 -0.017456   0.014452  -1.208   0.2271  
## DaysSinceLastERVisit -0.001271   0.001096  -1.160   0.2460  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  85.099  on 91  degrees of freedom
## AIC: 101.1
## 
## Number of Fisher Scoring iterations: 5
## 
##                                                                                                     feats
## 1                                                                                               Narcotics
## 3        Narcotics, OfficeVisits, ProviderCount, MedicalClaims, InpatientDays, Pain, DaysSinceLastERVisit
## 2 Narcotics, Narcotics:TotalVisits, Narcotics:AcuteDrugGapSmall, Narcotics:ERVisits, Narcotics:ClaimLines
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 1    99       NA       NA           NA 646.6943      NA 101.3409 0.6762162
## 3    99       NA       NA           NA 627.1277      NA 101.0986 0.8194595
## 2    99       NA       NA           NA 720.1554      NA  99.7530 0.6913514
##     auc.OOB
## 1 0.8567708
## 3 0.7760417
## 2 0.6015625
```

```r
# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(setdiff(names(glb_entity_df),
                                                        glb_predct_var),
                                                glb_exclude_vars_as_features),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.49502  -0.53888  -0.30366  -0.06124   2.08980  
## 
## Coefficients: (1 not defined because of singularities)
##                                Estimate Std. Error z value Pr(>|z|)   
## (Intercept)                   -1.527211   1.248868  -1.223  0.22138   
## InpatientDays                  0.139784   0.097246   1.437  0.15060   
## ERVisits                      -0.326455   0.248564  -1.313  0.18906   
## OfficeVisits                   0.098519   0.046867   2.102  0.03555 * 
## Narcotics                      0.046951   0.045670   1.028  0.30392   
## DaysSinceLastERVisit          -0.003885   0.001883  -2.063  0.03912 * 
## Pain                           0.002268   0.015376   0.147  0.88274   
## TotalVisits                          NA         NA      NA       NA   
## ProviderCount                  0.028466   0.033303   0.855  0.39267   
## MedicalClaims                  0.015683   0.024407   0.643  0.52051   
## ClaimLines                    -0.010551   0.009240  -1.142  0.25351   
## AcuteDrugGapSmall              0.268079   0.102462   2.616  0.00889 **
## StartedOnCombination.fctrTRUE  2.345867   1.667029   1.407  0.15936   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  72.527  on 87  degrees of freedom
## AIC: 96.527
## 
## Number of Fisher Scoring iterations: 6
## 
##                                                                                                                                                                               feats
## 4 InpatientDays, ERVisits, OfficeVisits, Narcotics, DaysSinceLastERVisit, Pain, TotalVisits, ProviderCount, MedicalClaims, ClaimLines, AcuteDrugGapSmall, StartedOnCombination.fctr
## 1                                                                                                                                                                         Narcotics
## 3                                                                                  Narcotics, OfficeVisits, ProviderCount, MedicalClaims, InpatientDays, Pain, DaysSinceLastERVisit
## 2                                                                           Narcotics, Narcotics:TotalVisits, Narcotics:AcuteDrugGapSmall, Narcotics:ERVisits, Narcotics:ClaimLines
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB   AIC.fit
## 4    99       NA       NA           NA 516.2256      NA  96.52702
## 1    99       NA       NA           NA 646.6943      NA 101.34094
## 3    99       NA       NA           NA 627.1277      NA 101.09858
## 2    99       NA       NA           NA 720.1554      NA  99.75300
##     auc.fit   auc.OOB
## 4 0.8756757 0.8645833
## 1 0.6762162 0.8567708
## 3 0.8194595 0.7760417
## 2 0.6913514 0.6015625
```

```r
glb_sel_mdl <- glb_mdl

# User specified
ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("Narcotics", "OfficeVisits"),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -2.06303  -0.63155  -0.50503  -0.09689   2.16686  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -2.64613    0.52357  -5.054 4.33e-07 ***
## Narcotics     0.07630    0.03205   2.381  0.01728 *  
## OfficeVisits  0.08212    0.03055   2.688  0.00718 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  89.127  on 96  degrees of freedom
## AIC: 95.127
## 
## Number of Fisher Scoring iterations: 4
## 
##                                                                                                                                                                               feats
## 4 InpatientDays, ERVisits, OfficeVisits, Narcotics, DaysSinceLastERVisit, Pain, TotalVisits, ProviderCount, MedicalClaims, ClaimLines, AcuteDrugGapSmall, StartedOnCombination.fctr
## 1                                                                                                                                                                         Narcotics
## 5                                                                                                                                                           Narcotics, OfficeVisits
## 3                                                                                  Narcotics, OfficeVisits, ProviderCount, MedicalClaims, InpatientDays, Pain, DaysSinceLastERVisit
## 2                                                                           Narcotics, Narcotics:TotalVisits, Narcotics:AcuteDrugGapSmall, Narcotics:ERVisits, Narcotics:ClaimLines
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB   AIC.fit
## 4    99       NA       NA           NA 516.2256      NA  96.52702
## 1    99       NA       NA           NA 646.6943      NA 101.34094
## 5    99       NA       NA           NA 760.1428      NA  95.12656
## 3    99       NA       NA           NA 627.1277      NA 101.09858
## 2    99       NA       NA           NA 720.1554      NA  99.75300
##     auc.fit   auc.OOB
## 4 0.8756757 0.8645833
## 1 0.6762162 0.8567708
## 5 0.7745946 0.7994792
## 3 0.8194595 0.7760417
## 2 0.6913514 0.6015625
```

```r
ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("StartedOnCombination", "ProviderCount"),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.61826  -0.72782  -0.64555  -0.08407   1.94662  
## 
## Coefficients:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -2.00097    0.55097  -3.632 0.000282 ***
## StartedOnCombinationTRUE  1.95230    1.22342   1.596 0.110541    
## ProviderCount             0.03366    0.01983   1.697 0.089706 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.89  on 98  degrees of freedom
## Residual deviance: 104.37  on 96  degrees of freedom
## AIC: 110.37
## 
## Number of Fisher Scoring iterations: 4
## 
##                                                                                                                                                                               feats
## 4 InpatientDays, ERVisits, OfficeVisits, Narcotics, DaysSinceLastERVisit, Pain, TotalVisits, ProviderCount, MedicalClaims, ClaimLines, AcuteDrugGapSmall, StartedOnCombination.fctr
## 1                                                                                                                                                                         Narcotics
## 5                                                                                                                                                           Narcotics, OfficeVisits
## 3                                                                                  Narcotics, OfficeVisits, ProviderCount, MedicalClaims, InpatientDays, Pain, DaysSinceLastERVisit
## 6                                                                                                                                               StartedOnCombination, ProviderCount
## 2                                                                           Narcotics, Narcotics:TotalVisits, Narcotics:AcuteDrugGapSmall, Narcotics:ERVisits, Narcotics:ClaimLines
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB   AIC.fit
## 4    99       NA       NA           NA 516.2256      NA  96.52702
## 1    99       NA       NA           NA 646.6943      NA 101.34094
## 5    99       NA       NA           NA 760.1428      NA  95.12656
## 3    99       NA       NA           NA 627.1277      NA 101.09858
## 6    99       NA       NA           NA 610.6529      NA 110.36502
## 2    99       NA       NA           NA 720.1554      NA  99.75300
##     auc.fit   auc.OOB
## 4 0.8756757 0.8645833
## 1 0.6762162 0.8567708
## 5 0.7745946 0.7994792
## 3 0.8194595 0.7760417
## 6 0.6321622 0.7630208
## 2 0.6913514 0.6015625
```

```r
# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)

plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 20))
if (glb_is_regression)
    print(myplot_scatter(plot_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))

if (glb_is_classification) {
    # Lower AIC is better
    plot_models_df[, "inv.AIC.fit"] <- 1.0 / plot_models_df[, "AIC.fit"] 
    print(myplot_scatter(plot_models_df, "inv.AIC.fit", "auc.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}    
```

![](D2HE_Claims_files/figure-html/run_models-1.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_training.all", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
```

## Step `6`: fit training.all

```r
print(mdl_feats_df <- myextract_mdl_feats())
```

```
##                                                  id        Pr.z
## AcuteDrugGapSmall                 AcuteDrugGapSmall 0.008886686
## OfficeVisits                           OfficeVisits 0.035545500
## DaysSinceLastERVisit           DaysSinceLastERVisit 0.039120312
## InpatientDays                         InpatientDays 0.150597914
## StartedOnCombination.fctr StartedOnCombination.fctr 0.159363803
## ERVisits                                   ERVisits 0.189060612
## ClaimLines                               ClaimLines 0.253506422
## Narcotics                                 Narcotics 0.303922305
## ProviderCount                         ProviderCount 0.392674890
## MedicalClaims                         MedicalClaims 0.520512872
## Pain                                           Pain 0.882742847
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    glb_entity_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_entity_df[, glb_predct_var_name] - glb_entity_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_entity_df)))                             
}    

if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_entity_df, type="response")
    
#     for (clf_threshold in c(0.3, 0.5, 0.7))
#         print(xtabs(reformulate(paste(glb_predct_var, 
#             paste0("(", glb_predct_var_name, ".proba >= ", 
#                    sprintf("%f", clf_threshold), ")"), 
#                                       sep=" + ")),
#                     glb_entity_df))
    require(ROCR)
    ROCRpred <- prediction(glb_entity_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_entity_df[, glb_predct_var])
    ROCRperf <- performance(ROCRpred, "tpr", "fpr")
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))
    
    # 0 & 1 does not generate outcomes for certain categories
    thresholds_df <- data.frame(threshold=seq(0.1, 0.9, 0.1))
    thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix) 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     thresholds_df[row_ix, "threshold"]))
    print(thresholds_df)
    print(myplot_line(thresholds_df, "threshold", "f.score"))
    
    glb_clf_proba_threshold <- thresholds_df[which.max(thresholds_df$f.score), 
                                             "threshold"]
    # This should change to maximize f.score.OOB ???
    print(sprintf("Classifier Probability Threshold: %0.4f to maximize f.score.fit",
                  glb_clf_proba_threshold))

    glb_entity_df[, glb_predct_var_name] <- 
        (glb_entity_df[, paste0(glb_predct_var_name, ".proba")] >= 
             glb_clf_proba_threshold) * 1.0
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_entity_df))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, glb_clf_proba_threshold)))    
}    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.49502  -0.53888  -0.30366  -0.06124   2.08980  
## 
## Coefficients:
##                                Estimate Std. Error z value Pr(>|z|)   
## (Intercept)                   -1.527211   1.248868  -1.223  0.22138   
## AcuteDrugGapSmall              0.268079   0.102462   2.616  0.00889 **
## OfficeVisits                   0.098519   0.046867   2.102  0.03555 * 
## DaysSinceLastERVisit          -0.003885   0.001883  -2.063  0.03912 * 
## InpatientDays                  0.139784   0.097246   1.437  0.15060   
## StartedOnCombination.fctrTRUE  2.345867   1.667029   1.407  0.15936   
## ERVisits                      -0.326455   0.248564  -1.313  0.18906   
## ClaimLines                    -0.010551   0.009240  -1.142  0.25351   
## Narcotics                      0.046951   0.045670   1.028  0.30392   
## ProviderCount                  0.028466   0.033303   0.855  0.39267   
## MedicalClaims                  0.015683   0.024407   0.643  0.52051   
## Pain                           0.002268   0.015376   0.147  0.88274   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  72.527  on 87  degrees of freedom
## AIC: 96.527
## 
## Number of Fisher Scoring iterations: 6
## 
##                                                                                                                                                                               feats
## 4 InpatientDays, ERVisits, OfficeVisits, Narcotics, DaysSinceLastERVisit, Pain, TotalVisits, ProviderCount, MedicalClaims, ClaimLines, AcuteDrugGapSmall, StartedOnCombination.fctr
## 1                                                                                                                                                                         Narcotics
## 5                                                                                                                                                           Narcotics, OfficeVisits
## 3                                                                                  Narcotics, OfficeVisits, ProviderCount, MedicalClaims, InpatientDays, Pain, DaysSinceLastERVisit
## 6                                                                                                                                               StartedOnCombination, ProviderCount
## 2                                                                           Narcotics, Narcotics:TotalVisits, Narcotics:AcuteDrugGapSmall, Narcotics:ERVisits, Narcotics:ClaimLines
## 7              AcuteDrugGapSmall, OfficeVisits, DaysSinceLastERVisit, InpatientDays, StartedOnCombination.fctr, ERVisits, ClaimLines, Narcotics, ProviderCount, MedicalClaims, Pain
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB   AIC.fit
## 4    99       NA       NA           NA 516.2256      NA  96.52702
## 1    99       NA       NA           NA 646.6943      NA 101.34094
## 5    99       NA       NA           NA 760.1428      NA  95.12656
## 3    99       NA       NA           NA 627.1277      NA 101.09858
## 6    99       NA       NA           NA 610.6529      NA 110.36502
## 2    99       NA       NA           NA 720.1554      NA  99.75300
## 7    99       NA       NA           NA 516.2256      NA  96.52702
##     auc.fit   auc.OOB
## 4 0.8756757 0.8645833
## 1 0.6762162 0.8567708
## 5 0.7745946 0.7994792
## 3 0.8194595 0.7760417
## 6 0.6321622 0.7630208
## 2 0.6913514 0.6015625
## 7 0.8756757        NA
```

![](D2HE_Claims_files/figure-html/fit_training.all-1.png) 

```
##   threshold   f.score
## 1       0.1 0.6097561
## 2       0.2 0.5937500
## 3       0.3 0.6206897
## 4       0.4 0.5531915
## 5       0.5 0.5238095
## 6       0.6 0.5000000
## 7       0.7 0.5294118
## 8       0.8 0.4375000
## 9       0.9 0.4375000
```

![](D2HE_Claims_files/figure-html/fit_training.all-2.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##         PoorCare.predict
## PoorCare  0  1
##        0 59 15
##        1  7 18
## [1] "f.score=0.6207"
```

```r
print(glb_feats_df <- mymerge_feats_Pr.z())
```

```
##                           id       cor.y  cor.y.abs cor.low        Pr.z
## 1          AcuteDrugGapSmall  0.33364848 0.33364848      NA 0.008886686
## 8               OfficeVisits  0.40236225 0.40236225       1 0.035545500
## 3       DaysSinceLastERVisit -0.10704558 0.10704558       1 0.039120312
## 5              InpatientDays  0.05592793 0.05592793       1 0.150597914
## 11 StartedOnCombination.fctr          NA         NA      NA 0.159363803
## 4                   ERVisits  0.08633538 0.08633538      NA 0.189060612
## 2                 ClaimLines  0.08194897 0.08194897      NA 0.253506422
## 7                  Narcotics  0.40237389 0.40237389       1 0.303922305
## 10             ProviderCount  0.22328055 0.22328055       1 0.392674890
## 6              MedicalClaims  0.16082837 0.16082837       1 0.520512872
## 9                       Pain  0.04336044 0.04336044       1 0.882742847
## 12               TotalVisits  0.36664685 0.36664685      NA          NA
```

```r
# Most of this code is used again in predict_newdata chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))
    }
    
    if (glb_is_regression) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_regression(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                           plot_vars_df$id[1])
#               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1,2] is a factor                                                         
#               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
              )
    }    
    
    if (glb_is_classification) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_classification(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"),
                                               plot_vars_df$id[1])
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(glb_entity_df)
```

![](D2HE_Claims_files/figure-html/fit_training.all-3.png) ![](D2HE_Claims_files/figure-html/fit_training.all-4.png) ![](D2HE_Claims_files/figure-html/fit_training.all-5.png) ![](D2HE_Claims_files/figure-html/fit_training.all-6.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
## 11            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
if (glb_is_regression)
    glb_predct_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_predct_df, type="response")

if (glb_is_classification) {
    glb_predct_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_predct_df, type="response")
    glb_predct_df[, glb_predct_var_name] <- 
        (predict(glb_sel_mdl, newdata=glb_predct_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0
}    
    
myprint_df(glb_predct_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##    MemberID PoorCare PoorCare.predict
## 5         5        0                0
## 7         7        0                0
## 9         9        1                0
## 10       10        0                0
## 12       12        0                0
## 25       25        0                0
##     MemberID PoorCare PoorCare.predict
## 5          5        0                0
## 10        10        0                0
## 37        37        0                0
## 66        66        0                0
## 114      114        1                1
## 124      124        0                1
##     MemberID PoorCare PoorCare.predict
## 114      114        1                1
## 118      118        0                1
## 121      121        0                0
## 124      124        0                1
## 127      127        0                0
## 131      131        0                1
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_predct_df[, glb_predct_var_name] - 
                        glb_predct_df[, glb_predct_var]) ^ 2)))
    print(sprintf("RMSE: %0.4f", 
                  (sum((glb_predct_df[, glb_predct_var_name] - 
                        glb_predct_df[, glb_predct_var]) ^ 2) / nrow(glb_predct_df)) ^ 0.5))                        
    print(myplot_scatter(glb_predct_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
                         
    glb_predct_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_predct_df[, glb_predct_var_name] - glb_predct_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_predct_df)))                                                      

#     glb_predct_df[, "<Output Pred variable>"] <- func(glb_predct_df[, glb_pred_var_name])                         
}                         

if (glb_is_classification) {
    ROCRpred <- prediction(glb_predct_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_predct_df[, glb_predct_var])
    print(sprintf("auc=%0.4f", auc <- as.numeric(performance(ROCRpred, "auc")@y.values)))
    print(sprintf("probability threshold=%0.4f", glb_clf_proba_threshold))
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_predct_df))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_predct_df, glb_clf_proba_threshold)))
}    
```

```
## [1] "auc=0.8646"
## [1] "probability threshold=0.3000"
##         PoorCare.predict
## PoorCare  0  1
##        0 19  5
##        1  2  6
## [1] "f.score=0.6316"
```

```r
glb_analytics_diag_plots(glb_predct_df)
```

![](D2HE_Claims_files/figure-html/predict_newdata-1.png) ![](D2HE_Claims_files/figure-html/predict_newdata-2.png) ![](D2HE_Claims_files/figure-html/predict_newdata-3.png) ![](D2HE_Claims_files/figure-html/predict_newdata-4.png) 

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ROCR_1.0-6      gplots_2.16.0   reshape2_1.4.1  plyr_1.8.1     
## [5] caTools_1.17.1  doBy_4.5-13     survival_2.38-1 ggplot2_1.0.0  
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6       colorspace_1.2-5   digest_0.6.8      
##  [4] evaluate_0.5.5     formatR_1.0        gdata_2.13.3      
##  [7] grid_3.1.2         gtable_0.1.2       gtools_3.4.1      
## [10] htmltools_0.2.6    KernSmooth_2.23-14 knitr_1.9         
## [13] labeling_0.3       lattice_0.20-30    MASS_7.3-39       
## [16] Matrix_1.1-5       munsell_0.4.2      proto_0.3-10      
## [19] Rcpp_0.11.4        rmarkdown_0.5.1    scales_0.2.4      
## [22] splines_3.1.2      stringr_0.6.2      tcltk_3.1.2       
## [25] tools_3.1.2        yaml_2.1.13
```
