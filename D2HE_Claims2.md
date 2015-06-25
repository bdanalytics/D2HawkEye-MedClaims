# Medical Claims: D2HawkEye : PoorCare classification:: Claims2
bdanalytics  

**  **    
**Date: (Thu) Jun 25, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/quality.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

Regression results:
First run:
    <glb_sel_mdl_id>: 
        OOB_RMSE=<0.4f>; new_RMSE=<0.4f>; <feat1>=<imp>; <feat2>=<imp>

Classification results:
First run:
    <glb_sel_mdl_id>: Leaderboard: <accuracy>
        newobs_tbl=[0=, 1=]; submit_filename=
        OOB_conf_mtrx=[YN=, NY=]=; max.Accuracy.OOB=; opt.prob.threshold.OOB=
            <feat1>=<imp>; <feat1>=<imp>; <feat1>=<imp>; 
            <txt.feat1>=<imp>; <txt.feat1>=<imp>; <txt.feat1>=<imp>; 

### Prediction Accuracy Enhancement Options:
- import.data chunk:
    - which obs should be in fit vs. OOB (currently dirty.0 vs .1 is split 50%)
    
- inspect.data chunk:
    - For date variables
        - Appropriate factors ?
        - Different / More last* features ?
        
- scrub.data chunk:        
- transform.data chunk:
    - derive features from multiple features
    
- manage.missing.data chunk:
    - Not fill missing vars
    - Fill missing numerics with a different algorithm
    - Fill missing chars with data based on clusters 
    
- extract.features chunk:
    - Text variables: move to date extraction chunk ???
        - Mine acronyms
        - Mine places

- Review set_global_options chunk after features are finalized

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")
#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/quality.csv"
glb_newdt_url <- "<newdt_url>"
glb_out_pfx <- "Claims2_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newobs_dataset <- FALSE    # or TRUE
    glb_split_entity_newobs_datasets <- TRUE   # or FALSE
    glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
    glb_split_newdata_condition <- NULL # or "is.na(<var>)"; "<var> <condition_operator> <value>"
    glb_split_newdata_size_ratio <- 0.25               # > 0 & < 1
    glb_split_sample.seed <- 88               # or any integer

glb_max_fitobs <- NULL # or any integer                         
glb_is_regression <- FALSE; glb_is_classification <- !glb_is_regression; 
    glb_is_binomial <- TRUE # or TRUE or FALSE

glb_rsp_var_raw <- "PoorCare"

# for classification, the response variable has to be a factor
glb_rsp_var <- "PoorCare.fctr"

# if the response factor is based on numbers/logicals e.g (0/1 OR TRUE/FALSE vs. "A"/"B"), 
#   or contains spaces (e.g. "Not in Labor Force")
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- function(raw) {
#     return(log(raw))
    ret_vals <- rep_len(NA, length(raw)); ret_vals[!is.na(raw)] <- ifelse(raw[!is.na(raw)] == 1, "Y", "N"); return(relevel(as.factor(ret_vals), ref="N"))
#     #as.factor(paste0("B", raw))
#     #as.factor(gsub(" ", "\\.", raw))    
}
glb_map_rsp_raw_to_var(c(1, 1, 0, 0, NA))
```

```
## [1] Y    Y    N    N    <NA>
## Levels: N Y
```

```r
glb_map_rsp_var_to_raw <- function(var) {
#     return(exp(var))
    as.numeric(var) - 1
#     #as.numeric(var)
#     #gsub("\\.", " ", levels(var)[as.numeric(var)])
#     c("<=50K", " >50K")[as.numeric(var)]
#     #c(FALSE, TRUE)[as.numeric(var)]
}
glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, NA)))
```

```
## [1]  1  1  0  0 NA
```

```r
if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

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

# If multiple vars are parts of id, consider concatenating them to create one id var
# If glb_id_var == NULL, ".rownames <- row.names()" is the default
glb_id_var <- c("MemberID")
glb_category_vars <- NULL # or c("<var1>", "<var2>")
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_map_vars <- NULL # or c("<var1>", "<var2>")
glb_map_urls <- list();
# glb_map_urls[["<var1>"]] <- "<var1.url>"

glb_assign_pairs_lst <- NULL; 
# glb_assign_pairs_lst[["<var1>"]] <- list(from=c(NA),
#                                            to=c("NA.my"))
glb_assign_vars <- names(glb_assign_pairs_lst)

# Derived features
glb_derive_lst <- NULL;
# glb_derive_lst[["Week.bgn"]] <- list(
#     mapfn=function(Week) { return(substr(Week, 1, 10)) }
#     , args=c("Week"))

# require(zoo)
# # If glb_allobs_df is not sorted in the desired manner
# glb_derive_lst[["ILI.2.lag"]] <- list(
#     mapfn=function(Week) { return(coredata(lag(zoo(orderBy(~Week, glb_allobs_df)$ILI), -2, na.pad=TRUE))) }
#     , args=c("Week"))
# glb_derive_lst[["ILI.2.lag"]] <- list(
#     mapfn=function(ILI) { return(coredata(lag(zoo(ILI), -2, na.pad=TRUE))) }
#     , args=c("ILI"))
# glb_derive_lst[["ILI.2.lag.log"]] <- list(
#     mapfn=function(ILI.2.lag) { return(log(ILI.2.lag)) }
#     , args=c("ILI.2.lag"))

#     mapfn=function(PTS, oppPTS) { return(PTS - oppPTS) }
#     , args=c("PTS", "oppPTS"))

# Add logs of numerics that are not distributed normally ->  do automatically ???

#     mapfn=function(raw) { tfr_raw <- as.character(cut(raw, 5)); 
#                           tfr_raw[is.na(tfr_raw)] <- "NA.my";
#                           return(as.factor(tfr_raw)) }

# glb_derive_lst[["<txt_var>.niso8859.log"]] <- list(
#     mapfn=function(<txt_var>) { match_lst <- gregexpr("&#[[:digit:]]{3};", <txt_var>)
#                         match_num_vctr <- unlist(lapply(match_lst, 
#                                                         function(elem) length(elem)))
#                         return(log(1 + match_num_vctr)) }
#     , args=c("<txt_var>"))

#     mapfn=function(raw) { mod_raw <- raw;
#         mod_raw <- gsub("&#[[:digit:]]{3};", " ", mod_raw);
#         # Modifications for this exercise only
#         mod_raw <- gsub("\\bgoodIn ", "good In", mod_raw);
#                           return(mod_raw)

#         # Create user-specified pattern vectors 
# #sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
#         if (txt_var %in% c("Snippet", "Abstract")) {
#             txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
#                 as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
#                                                    glb_allobs_df[, txt_var]))
#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])

# args_lst <- NULL; for (arg in glb_derive_lst[["Week.bgn"]]$args) args_lst[[arg]] <- glb_allobs_df[, arg]; do.call(mapfn, args_lst)

# glb_derive_lst[["<var1>"]] <- glb_derive_lst[["<var2>"]]
glb_derive_vars <- names(glb_derive_lst)

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- NULL # or c("<txt_var1>", "<txt_var2>")   
#Sys.setlocale("LC_ALL", "C") # For english

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms 

glb_sprs_thresholds <- NULL # or c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

# User-specified exclusions  
glb_exclude_vars_as_features <- NULL # or c("<var_name>") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- FALSE # or TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](D2HE_Claims2_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 9.534  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
#glb_chunks_df <- myadd_chunk(NULL, "import.data")

glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/quality.csv..."
## [1] "dimensions of data in ./data/quality.csv: 131 rows x 14 cols"
##   MemberID InpatientDays ERVisits OfficeVisits Narcotics
## 1        1             0        0           18         1
## 2        2             1        1            6         1
## 3        3             0        0            5         3
## 4        4             0        1           19         0
## 5        5             8        2           19         3
## 6        6             2        0            9         2
##   DaysSinceLastERVisit Pain TotalVisits ProviderCount MedicalClaims
## 1                  731   10          18            21            93
## 2                  411    0           8            27            19
## 3                  731   10           5            16            27
## 4                  158   34          20            14            59
## 5                  449   10          29            24            51
## 6                  731    6          11            40            53
##   ClaimLines StartedOnCombination AcuteDrugGapSmall PoorCare
## 1        222                FALSE                 0        0
## 2        115                FALSE                 1        0
## 3        148                FALSE                 5        0
## 4        242                FALSE                 0        0
## 5        204                FALSE                 0        0
## 6        156                FALSE                 4        1
##     MemberID InpatientDays ERVisits OfficeVisits Narcotics
## 5          5             8        2           19         3
## 20        20             0        1           20         0
## 67        67             0        1            2         1
## 93        93             0        2           15         0
## 95        95             2        0            8         0
## 128      128             1        0            3         2
##     DaysSinceLastERVisit Pain TotalVisits ProviderCount MedicalClaims
## 5               449.0000   10          29            24            51
## 20              263.9583   34          21            27            53
## 67              694.0000    0           3            17           194
## 93              547.9583   11          17            13            30
## 95              731.0000    0          10            11            17
## 128             731.0000    0           4            35            18
##     ClaimLines StartedOnCombination AcuteDrugGapSmall PoorCare
## 5          204                FALSE                 0        0
## 20         153                FALSE                 0        0
## 67         376                FALSE                 0        0
## 93          65                FALSE                 0        0
## 95          53                FALSE                 2        0
## 128        106                FALSE                 2        0
##     MemberID InpatientDays ERVisits OfficeVisits Narcotics
## 126      126             0        0            6         0
## 127      127             1        1            5         3
## 128      128             1        0            3         2
## 129      129            15       11            5         9
## 130      130             0        2           14         1
## 131      131            30        1           22         3
##     DaysSinceLastERVisit Pain TotalVisits ProviderCount MedicalClaims
## 126             731.0000    0           6            21            25
## 127             444.0000    0           7            11            11
## 128             731.0000    0           4            35            18
## 129             180.9583   95          31            56            43
## 130             216.9583    5          16            26            41
## 131             452.0000   38          53            20           103
##     ClaimLines StartedOnCombination AcuteDrugGapSmall PoorCare
## 126         51                FALSE                 5        0
## 127         36                FALSE                 0        0
## 128        106                FALSE                 2        0
## 129        265                FALSE                 3        0
## 130        138                FALSE                 1        1
## 131        189                FALSE                13        0
## 'data.frame':	131 obs. of  14 variables:
##  $ MemberID            : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ InpatientDays       : int  0 1 0 0 8 2 16 2 2 4 ...
##  $ ERVisits            : int  0 1 0 1 2 0 1 0 1 2 ...
##  $ OfficeVisits        : int  18 6 5 19 19 9 8 8 4 0 ...
##  $ Narcotics           : int  1 1 3 0 3 2 1 0 3 2 ...
##  $ DaysSinceLastERVisit: num  731 411 731 158 449 ...
##  $ Pain                : int  10 0 10 34 10 6 4 5 5 2 ...
##  $ TotalVisits         : int  18 8 5 20 29 11 25 10 7 6 ...
##  $ ProviderCount       : int  21 27 16 14 24 40 19 11 28 21 ...
##  $ MedicalClaims       : int  93 19 27 59 51 53 40 28 20 17 ...
##  $ ClaimLines          : int  222 115 148 242 204 156 261 87 98 66 ...
##  $ StartedOnCombination: logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
##  $ AcuteDrugGapSmall   : int  0 1 5 0 0 4 0 0 0 0 ...
##  $ PoorCare            : int  0 0 0 0 0 1 0 0 1 0 ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
## NULL
```

```r
# glb_trnobs_df <- read.delim("data/hygiene.txt", header=TRUE, fill=TRUE, sep="\t",
#                             fileEncoding='iso-8859-1')
# glb_trnobs_df <- read.table("data/hygiene.dat.labels", col.names=c("dirty"),
#                             na.strings="[none]")
# glb_trnobs_df$review <- readLines("data/hygiene.dat", n =-1)
# comment(glb_trnobs_df) <- "glb_trnobs_df"                                

# glb_trnobs_df <- data.frame()
# for (symbol in c("Boeing", "CocaCola", "GE", "IBM", "ProcterGamble")) {
#     sym_trnobs_df <- 
#         myimport_data(url=gsub("IBM", symbol, glb_trnng_url), comment="glb_trnobs_df", 
#                                     force_header=TRUE)
#     sym_trnobs_df$Symbol <- symbol
#     glb_trnobs_df <- myrbind_df(glb_trnobs_df, sym_trnobs_df)
# }
                                
# glb_trnobs_df <- 
#     glb_trnobs_df %>% dplyr::filter(Year >= 1999)
                                
if (glb_is_separate_newobs_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newobs_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newobs_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
    }
}         
```

```
## Loading required package: caTools
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
##  - attr(*, "comment")= chr "glb_newobs_df"
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
##  - attr(*, "comment")= chr "glb_trnobs_df"
```

```r
if ((num_nas <- sum(is.na(glb_trnobs_df[, glb_rsp_var_raw]))) > 0)
    stop("glb_trnobs_df$", glb_rsp_var_raw, " contains NAs for ", num_nas, " obs")

if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Combine trnent & newobs into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"

# Check for duplicates in glb_id_var
if (length(glb_id_var) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_trnobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Train"))
    glb_newobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Test"))    
    glb_id_var <- ".rownames"
}
if (sum(duplicated(glb_allobs_df[, glb_id_var, FALSE])) > 0)
    stop(glb_id_var, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_var)

glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 1  import.data          1          0  9.534 10.007   0.473
## 2 inspect.data          2          0 10.008     NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    mycheck_problem_data(glb_allobs_df)
}
glb_chk_data()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Loading required package: reshape2
```

![](D2HE_Claims2_files/figure-html/inspect.data-1.png) 

```
##       PoorCare.0 PoorCare.1
## Test          24          8
## Train         74         25
##       PoorCare.0 PoorCare.1
## Test   0.7500000  0.2500000
## Train  0.7474747  0.2525253
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##        InpatientDays             ERVisits         OfficeVisits 
##                   67                   61                    3 
##            Narcotics                 Pain          TotalVisits 
##                   49                   28                    1 
## StartedOnCombination    AcuteDrugGapSmall             PoorCare 
##                  125                   65                   98 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
## named list()
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: tcltk
```

```
##   PoorCare PoorCare.fctr .n
## 1        0             N 98
## 2        1             Y 33
```

![](D2HE_Claims2_files/figure-html/inspect.data-2.png) 

```
##       PoorCare.fctr.N PoorCare.fctr.Y
## Test               24               8
## Train              74              25
##       PoorCare.fctr.N PoorCare.fctr.Y
## Test        0.7500000       0.2500000
## Train       0.7474747       0.2525253
```

```r
# check distribution of all numeric data
dsp_numeric_feats_dstrb <- function(feats_vctr) {
    for (feat in feats_vctr) {
        print(sprintf("feat: %s", feat))
        if (glb_is_regression)
            gp <- myplot_scatter(df=glb_allobs_df, ycol_name=glb_rsp_var, xcol_name=feat,
                                 smooth=TRUE)
        if (glb_is_classification)
            gp <- myplot_box(df=glb_allobs_df, ycol_names=feat, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, feat], "factor"))
            gp <- gp + facet_wrap(reformulate(feat))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

#         <col_name> = trunc(<col2_name> / 100),

        .rnorm = rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Merge some <descriptor>
# glb_allobs_df$<descriptor>.my <- glb_allobs_df$<descriptor>
# glb_allobs_df[grepl("\\bAIRPORT\\b", glb_allobs_df$<descriptor>.my),
#               "<descriptor>.my"] <- "AIRPORT"
# glb_allobs_df$<descriptor>.my <-
#     plyr::revalue(glb_allobs_df$<descriptor>.my, c(
#         "ABANDONED BUILDING" = "OTHER",
#         "##"                      = "##"
#     ))
# print(<descriptor>_freq_df <- mycreate_sqlxtab_df(glb_allobs_df, c("<descriptor>.my")))
# # print(dplyr::filter(<descriptor>_freq_df, grepl("(MEDICAL|DENTAL|OFFICE)", <descriptor>.my)))
# # print(dplyr::filter(dplyr::select(glb_allobs_df, -<var.zoo>), 
# #                     grepl("STORE", <descriptor>.my)))
# glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, "<descriptor>")

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_feats_dstrb(feats_vctr=setdiff(names(glb_allobs_df), 
        c(myfind_chr_cols_df(glb_allobs_df), glb_rsp_var_raw, glb_rsp_var, 
          glb_exclude_vars_as_features)))
```

```
## [1] "feat: InpatientDays"
```

![](D2HE_Claims2_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: ERVisits"
```

![](D2HE_Claims2_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: OfficeVisits"
```

![](D2HE_Claims2_files/figure-html/inspect.data-5.png) 

```
## [1] "feat: Narcotics"
```

![](D2HE_Claims2_files/figure-html/inspect.data-6.png) 

```
## [1] "feat: DaysSinceLastERVisit"
```

![](D2HE_Claims2_files/figure-html/inspect.data-7.png) 

```
## [1] "feat: Pain"
```

![](D2HE_Claims2_files/figure-html/inspect.data-8.png) 

```
## [1] "feat: TotalVisits"
```

![](D2HE_Claims2_files/figure-html/inspect.data-9.png) 

```
## [1] "feat: ProviderCount"
```

![](D2HE_Claims2_files/figure-html/inspect.data-10.png) 

```
## [1] "feat: MedicalClaims"
```

![](D2HE_Claims2_files/figure-html/inspect.data-11.png) 

```
## [1] "feat: ClaimLines"
```

![](D2HE_Claims2_files/figure-html/inspect.data-12.png) 

```
## [1] "feat: StartedOnCombination"
```

![](D2HE_Claims2_files/figure-html/inspect.data-13.png) 

```
## [1] "feat: AcuteDrugGapSmall"
```

![](D2HE_Claims2_files/figure-html/inspect.data-14.png) 

```
## [1] "feat: .rnorm"
```

![](D2HE_Claims2_files/figure-html/inspect.data-15.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 
# print(mycreate_sqlxtab_df(glb_allobs_df, c("<col1_name>", "<col2_name>")))

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("CocaCola", "ProcterGamble")), 
#                   "Date.POSIX", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.POSIXlt("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1983-01-01")))        
#         )
# print(myplot_line(subset(glb_trnobs_df, Date.POSIX > as.POSIXct("2004-01-01")), 
#                   "Date.POSIX", "StockPrice") +
#     geom_line(aes(color=Symbol)) + 
#     coord_cartesian(xlim=c(as.POSIXct("1990-01-01"),
#                            as.POSIXct("2000-01-01"))) +     
#     coord_cartesian(ylim=c(0, 250)) +     
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-09-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-11-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5) +
#         geom_vline(xintercept=84))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "scrub.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0 10.008 17.863   7.855
## 3   scrub.data          2          1 17.864     NA      NA
```

### Step `2.1: scrub data`

```r
mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##        InpatientDays             ERVisits         OfficeVisits 
##                   67                   61                    3 
##            Narcotics                 Pain          TotalVisits 
##                   49                   28                    1 
## StartedOnCombination    AcuteDrugGapSmall             PoorCare 
##                  125                   65                   98 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
## named list()
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

# sel_obs <- function(Popular=NULL, 
#                     NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
#         Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
#         Headline.pfx=NULL, NewsDesk.nb=NULL, .clusterid=NULL, myCategory=NULL,
#         perl=FALSE) {
sel_obs <- function(vars_lst) {
    tmp_df <- glb_allobs_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
            tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_df <- tmp_df[tmp_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_df <- tmp_df[tmp_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_df <- 
            tmp_df[grep(Headline.contains, tmp_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_df <- 
            tmp_df[grep(Snippet.contains, tmp_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_df <- 
            tmp_df[grep(Abstract.contains, tmp_df$Abstract, perl=perl), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_df), fixed=TRUE, value=TRUE))
            > 0) tmp_df <- 
                tmp_df[tmp_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_allobs_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_allobs_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    if (!is.null(.clusterid)) {
        if (any(grepl(".clusterid", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$clusterid == clusterid, ] else
        warning("glb_allobs_df does not contain clusterid; ignoring that filter")                       }
    if (!is.null(myCategory)) {    
        if (!(myCategory %in% names(glb_allobs_df)))
            tmp_df <-
                tmp_df[tmp_df$myCategory == myCategory, ] else
        warning("glb_allobs_df does not contain myCategory; ignoring that filter")                    
    }    
    
    return(glb_allobs_df$UniqueID %in% tmp_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_var, "Popular")],
                    sel_df[, c(glb_id_var, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 17.864 19.508   1.645
## 4 transform.data          2          2 19.509     NA      NA
```

```r
### Mapping dictionary
#sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_map_vars)) {
    for (feat in glb_map_vars) {
        map_df <- myimport_data(url=glb_map_urls[[feat]], 
                                            comment="map_df", 
                                           print_diagn=TRUE)
        glb_allobs_df <- mymap_codes(glb_allobs_df, feat, names(map_df)[2], 
                                     map_df, map_join_col_name=names(map_df)[1], 
                                     map_tgt_col_name=names(map_df)[2])
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_map_vars)
}

### Forced Assignments
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_assign_vars) {
    new_feat <- paste0(feat, ".my")
    print(sprintf("Forced Assignments for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_allobs_df[, feat]
    
    pairs <- glb_assign_pairs_lst[[feat]]
    for (pair_ix in 1:length(pairs$from)) {
        if (is.na(pairs$from[pair_ix]))
            nobs <- nrow(filter(glb_allobs_df, 
                                is.na(eval(parse(text=feat),
                                            envir=glb_allobs_df)))) else
            nobs <- sum(glb_allobs_df[, feat] == pairs$from[pair_ix])
        #nobs <- nrow(filter(glb_allobs_df, is.na(Married.fctr)))    ; print(nobs)
        
        if ((is.na(pairs$from[pair_ix])) && (is.na(pairs$to[pair_ix])))
            stop("what are you trying to do ???")
        if (is.na(pairs$from[pair_ix]))
            glb_allobs_df[is.na(glb_allobs_df[, feat]), new_feat] <- 
                pairs$to[pair_ix] else
            glb_allobs_df[glb_allobs_df[, feat] == pairs$from[pair_ix], new_feat] <- 
                pairs$to[pair_ix]
                    
        print(sprintf("    %s -> %s for %s obs", 
                      pairs$from[pair_ix], pairs$to[pair_ix], format(nobs, big.mark=",")))
    }

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_assign_vars)
}

### Derivations using mapping functions
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (new_feat in glb_derive_vars) {
    print(sprintf("Creating new feature: %s...", new_feat))
    args_lst <- NULL 
    for (arg in glb_derive_lst[[new_feat]]$args) 
        args_lst[[arg]] <- glb_allobs_df[, arg]
    glb_allobs_df[, new_feat] <- do.call(glb_derive_lst[[new_feat]]$mapfn, args_lst)
}
```

## Step `2.2: transform data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 4   transform.data          2          2 19.509 19.578   0.069
## 5 extract.features          3          0 19.578     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 19.587  NA      NA
```

```r
# Options:
#   Select Tf, log(1 + Tf), Tf-IDF or BM25Tf-IDf

# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

#stop(here"); sav_allobs_df <- glb_allobs_df #; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, 
                           id_vars=glb_id_var, rsp_var=glb_rsp_var))
    for (sfx in c("", ".POSIX"))
        glb_exclude_vars_as_features <- 
            union(glb_exclude_vars_as_features, 
                    paste(glb_date_vars, sfx, sep=""))

    for (feat in glb_date_vars) {
        glb_allobs_df <- orderBy(reformulate(paste0(feat, ".POSIX")), glb_allobs_df)
#         print(myplot_scatter(glb_allobs_df, xcol_name=paste0(feat, ".POSIX"),
#                              ycol_name=glb_rsp_var, colorcol_name=glb_rsp_var))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[, paste0(feat, ".POSIX")] >=
                                               strptime("2012-12-01", "%Y-%m-%d"), ], 
                             xcol_name=paste0(feat, ".POSIX"),
                             ycol_name=glb_rsp_var, colorcol_name=paste0(feat, ".wkend")))

        # Create features that measure the gap between previous timestamp in the data
        require(zoo)
        z <- zoo(as.numeric(as.POSIXlt(glb_allobs_df[, paste0(feat, ".POSIX")])))
        glb_allobs_df[, paste0(feat, ".zoo")] <- z
        print(head(glb_allobs_df[, c(glb_id_var, feat, paste0(feat, ".zoo"))]))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[,  paste0(feat, ".POSIX")] >
                                            strptime("2012-10-01", "%Y-%m-%d"), ], 
                            xcol_name=paste0(feat, ".zoo"), ycol_name=glb_rsp_var,
                            colorcol_name=glb_rsp_var))
        b <- zoo(, seq(nrow(glb_allobs_df)))
        
        last1 <- as.numeric(merge(z-lag(z, -1), b, all=TRUE)); last1[is.na(last1)] <- 0
        glb_allobs_df[, paste0(feat, ".last1.log")] <- log(1 + last1)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last1.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last1.log"), 
                               xcol_name=glb_rsp_var))
        
        last2 <- as.numeric(merge(z-lag(z, -2), b, all=TRUE)); last2[is.na(last2)] <- 0
        glb_allobs_df[, paste0(feat, ".last2.log")] <- log(1 + last2)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last2.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last2.log"), 
                               xcol_name=glb_rsp_var))
        
        last10 <- as.numeric(merge(z-lag(z, -10), b, all=TRUE)); last10[is.na(last10)] <- 0
        glb_allobs_df[, paste0(feat, ".last10.log")] <- log(1 + last10)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last10.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last10.log"), 
                               xcol_name=glb_rsp_var))
        
        last100 <- as.numeric(merge(z-lag(z, -100), b, all=TRUE)); last100[is.na(last100)] <- 0
        glb_allobs_df[, paste0(feat, ".last100.log")] <- log(1 + last100)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last100.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last100.log"), 
                               xcol_name=glb_rsp_var))
        
        glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                                c(paste0(feat, ".zoo")))
        # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
        # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
        # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
        # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
        # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
        # 
        # 
        # # order table
        # all2 = all2[order(all2$id),]
        # 
        # ## fill in NAs
        # # count averages
        # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
        #     last1=mean(last1, na.rm=TRUE),
        #     last3=mean(last3, na.rm=TRUE),
        #     last5=mean(last5, na.rm=TRUE),
        #     last10=mean(last10, na.rm=TRUE),
        #     last20=mean(last20, na.rm=TRUE),
        #     last50=mean(last50, na.rm=TRUE)
        # )
        # 
        # # fill in averages
        # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
        # na.merge = na.merge[order(na.merge$id),]
        # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
        #     y = paste0(i, ".y")
        #     idx = is.na(all2[[i]])
        #     all2[idx,][[i]] <- na.merge[idx,][[y]]
        # }
        # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
    }
}
rm(last1, last10, last100)
```

```
## Warning in rm(last1, last10, last100): object 'last1' not found
```

```
## Warning in rm(last1, last10, last100): object 'last10' not found
```

```
## Warning in rm(last1, last10, last100): object 'last100' not found
```

```r
#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 1                extract.features_bgn          1          0 19.587 19.602
## 2 extract.features_factorize.str.vars          2          0 19.603     NA
##   elapsed
## 1   0.016
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##   .src 
## ".src"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               c(glb_exclude_vars_as_features, glb_txt_vars))) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- 
            relevel(factor(glb_allobs_df[, var]),
                    names(which.max(table(glb_allobs_df[, var], useNA = "ifany"))))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}

if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(rex_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(rex_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }

#     match_lst <- gregexpr("\\bok(?!ay)", txt_vctr[746], ignore.case = FALSE, perl=TRUE); print(match_lst)
    dsp_pattern <- function(rex_str, ignore.case=TRUE, print.all=TRUE) {
        match_lst <- gregexpr(rex_str, txt_vctr, ignore.case = ignore.case, perl=TRUE)
        match_lst <- regmatches(txt_vctr, match_lst)
        match_df <- data.frame(matches=sapply(match_lst, 
                                              function (elems) paste(elems, collapse="#")))
        match_df <- subset(match_df, matches != "")
        if (print.all)
            print(match_df)
        return(match_df)
    }
    
    dsp_matches <- function(rex_str, ix) {
        print(match_pos <- gregexpr(rex_str, txt_vctr[ix], perl=TRUE))
        print(str_sub(txt_vctr[ix], (match_pos[[1]] / 100) *  99 +   0, 
                                    (match_pos[[1]] / 100) * 100 + 100))        
    }

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], 
                        glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#chk.equal( 1, 100)
#dsp.equal(86, 90)
    
    glb_txt_map_df <- read.csv("mytxt_map.csv", comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[163, "rex_str"])
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rpl_str == "versus", "rex_str"])             
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining OK in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "(?<!(BO|HO|LO))OK(?!(E\\!|ED|IE|IN|S ))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "Ok(?!(a\\.|ay|in|ra|um))", ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "(?<!( b| B| c| C| g| G| j| M| p| P| w| W| r| Z|\\(b|ar|bo|Bo|co|Co|Ew|gk|go|ho|ig|jo|kb|ke|Ke|ki|lo|Lo|mo|mt|no|No|po|ra|ro|sm|Sm|Sp|to|To))ok(?!(ay|bo|e |e\\)|e,|e\\.|eb|ed|el|en|er|es|ey|i |ie|in|it|ka|ke|ki|ly|on|oy|ra|st|u |uc|uy|yl|yo))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))
    }    
    # txt_vctr <- glb_txt_lst[[glb_txt_vars[1]]]
    # print(chk_pattern_freq(rex_str <- "(?<!( b| c| C| p|\\(b|bo|co|lo|Lo|Sp|to|To))ok(?!(ay|e |e\\)|e,|e\\.|ed|el|en|es|ey|ie|in|on|ra))", ignore.case=FALSE))
    # print(chk_pattern_freq(rex_str <- "ok(?!(ay|el|on|ra))", ignore.case=FALSE))
    # dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
    # dsp_matches(rex_str, ix=8)
    # substr(txt_vctr[86], 5613, 5620)
    # substr(glb_allobs_df[301, "review"], 550, 650)

#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "([[:upper:]]\\.( *)){2,}", ignore.case=FALSE))
        
        # Check for names
        print(subset(chk_pattern_freq(rex_str <- "(([[:upper:]]+)\\.( *)){1}",
                                      ignore.case=FALSE),
                     .n > 1))
        # dsp_pattern(rex_str="(OK\\.( *)){1}", ignore.case=FALSE)
        # dsp_matches(rex_str="(OK\\.( *)){1}", ix=557)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)(\\B)", ix=461)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)", ix=461)        
        #print(str_sub(txt_vctr[676], 10100, 10200))
        #print(str_sub(txt_vctr[74], 1, -1))        
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, 
                             preserve_intra_word_dashes=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        filter_df <- read.csv("mytxt_compound.csv", comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, tolower) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english"))) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#sav_allobs_df <- glb_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)
        txt_X_df[is.nan(txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")]),
                 paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 0

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_var, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
    rm(log_X_df, txt_X_df)
}

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_DTM' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_full_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_sprs_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_corpus' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 19.603 19.623
## 3                extract.features_end          3          0 19.623     NA
##   elapsed
## 2    0.02
## 3      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 19.603 19.623
## 1                extract.features_bgn          1          0 19.587 19.602
##   elapsed duration
## 2   0.020    0.020
## 1   0.016    0.015
## [1] "Total Elapsed Time: 19.623 secs"
```

![](D2HE_Claims2_files/figure-html/extract.features-1.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](D2HE_Claims2_files/figure-html/extract.features-2.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 19.578 21.169   1.591
## 6     cluster.data          4          0 21.169     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn   end elapsed
## 6        cluster.data          4          0 21.169 21.54   0.371
## 7 manage.missing.data          4          1 21.540    NA      NA
```

```r
# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##        InpatientDays             ERVisits         OfficeVisits 
##                   67                   61                    3 
##            Narcotics                 Pain          TotalVisits 
##                   49                   28                    1 
## StartedOnCombination    AcuteDrugGapSmall             PoorCare 
##                  125                   65                   98 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
## named list()
```

```r
# glb_allobs_df <- na.omit(glb_allobs_df)

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    # complete(mice()) changes attributes of factors even though values don't change
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col], inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (length(myfind_numerics_missing(glb_allobs_df)) > 0) &&
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
    
mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##        InpatientDays             ERVisits         OfficeVisits 
##                   67                   61                    3 
##            Narcotics                 Pain          TotalVisits 
##                   49                   28                    1 
## StartedOnCombination    AcuteDrugGapSmall             PoorCare 
##                  125                   65                   98 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
## named list()
```

## Step `4.1: manage missing data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 7 manage.missing.data          4          1 21.540 21.602   0.062
## 8     select.features          5          0 21.603     NA      NA
```

## Step `5.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##                                        id       cor.y exclude.as.feat
## PoorCare                         PoorCare  1.00000000               1
## Narcotics                       Narcotics  0.40237389               0
## OfficeVisits                 OfficeVisits  0.40236225               0
## TotalVisits                   TotalVisits  0.36664685               0
## AcuteDrugGapSmall       AcuteDrugGapSmall  0.33364848               0
## StartedOnCombination StartedOnCombination  0.23495709               0
## ProviderCount               ProviderCount  0.22328055               0
## .rnorm                             .rnorm -0.19502924               0
## MedicalClaims               MedicalClaims  0.16082837               0
## DaysSinceLastERVisit DaysSinceLastERVisit -0.10704558               0
## ERVisits                         ERVisits  0.08633538               0
## ClaimLines                     ClaimLines  0.08194897               0
## MemberID                         MemberID  0.06501932               1
## InpatientDays               InpatientDays  0.05592793               0
## Pain                                 Pain  0.04336044               0
##                       cor.y.abs
## PoorCare             1.00000000
## Narcotics            0.40237389
## OfficeVisits         0.40236225
## TotalVisits          0.36664685
## AcuteDrugGapSmall    0.33364848
## StartedOnCombination 0.23495709
## ProviderCount        0.22328055
## .rnorm               0.19502924
## MedicalClaims        0.16082837
## DaysSinceLastERVisit 0.10704558
## ERVisits             0.08633538
## ClaimLines           0.08194897
## MemberID             0.06501932
## InpatientDays        0.05592793
## Pain                 0.04336044
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## [1] "cor(OfficeVisits, TotalVisits)=0.8798"
## [1] "cor(PoorCare.fctr, OfficeVisits)=0.4024"
## [1] "cor(PoorCare.fctr, TotalVisits)=0.3666"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified TotalVisits as highly correlated with
## OfficeVisits
```

```
## [1] "cor(AcuteDrugGapSmall, Narcotics)=0.7095"
## [1] "cor(PoorCare.fctr, AcuteDrugGapSmall)=0.3336"
## [1] "cor(PoorCare.fctr, Narcotics)=0.4024"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified AcuteDrugGapSmall as highly correlated with
## Narcotics
```

```
##                      id       cor.y exclude.as.feat  cor.y.abs
## 12             PoorCare  1.00000000               1 1.00000000
## 9             Narcotics  0.40237389               0 0.40237389
## 10         OfficeVisits  0.40236225               0 0.40236225
## 15          TotalVisits  0.36664685               0 0.36664685
## 2     AcuteDrugGapSmall  0.33364848               0 0.33364848
## 14 StartedOnCombination  0.23495709               0 0.23495709
## 13        ProviderCount  0.22328055               0 0.22328055
## 7         MedicalClaims  0.16082837               0 0.16082837
## 5              ERVisits  0.08633538               0 0.08633538
## 3            ClaimLines  0.08194897               0 0.08194897
## 8              MemberID  0.06501932               1 0.06501932
## 6         InpatientDays  0.05592793               0 0.05592793
## 11                 Pain  0.04336044               0 0.04336044
## 4  DaysSinceLastERVisit -0.10704558               0 0.10704558
## 1                .rnorm -0.19502924               0 0.19502924
##      cor.high.X freqRatio percentUnique zeroVar   nzv myNearZV
## 12         <NA>  2.960000      2.020202   FALSE FALSE    FALSE
## 9          <NA>  1.590909     18.181818   FALSE FALSE    FALSE
## 10         <NA>  1.000000     30.303030   FALSE FALSE    FALSE
## 15 OfficeVisits  1.833333     34.343434   FALSE FALSE    FALSE
## 2     Narcotics  3.357143     15.151515   FALSE FALSE    FALSE
## 14         <NA> 23.750000      2.020202   FALSE  TRUE    FALSE
## 13         <NA>  1.000000     39.393939   FALSE FALSE    FALSE
## 7          <NA>  1.000000     50.505051   FALSE FALSE    FALSE
## 5          <NA>  2.400000     10.101010   FALSE FALSE    FALSE
## 3          <NA>  1.000000     79.797980   FALSE FALSE    FALSE
## 8          <NA>  1.000000    100.000000   FALSE FALSE    FALSE
## 6          <NA>  4.250000     14.141414   FALSE FALSE    FALSE
## 11         <NA>  2.714286     40.404040   FALSE FALSE    FALSE
## 4          <NA> 24.000000     49.494949   FALSE FALSE    FALSE
## 1          <NA>  1.000000    100.000000   FALSE FALSE    FALSE
##    is.cor.y.abs.low
## 12            FALSE
## 9             FALSE
## 10            FALSE
## 15            FALSE
## 2             FALSE
## 14            FALSE
## 13            FALSE
## 7              TRUE
## 5              TRUE
## 3              TRUE
## 8              TRUE
## 6              TRUE
## 11             TRUE
## 4              TRUE
## 1             FALSE
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning: Removed 9 rows containing missing values (geom_point).
```

```
## Warning: Removed 9 rows containing missing values (geom_point).
```

```
## Warning: Removed 9 rows containing missing values (geom_point).
```

![](D2HE_Claims2_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##  [1] id               cor.y            exclude.as.feat  cor.y.abs       
##  [5] cor.high.X       freqRatio        percentUnique    zeroVar         
##  [9] nzv              myNearZV         is.cor.y.abs.low
## <0 rows> (or 0-length row.names)
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
## named integer(0)
## [1] "numeric data w/ 0s in : "
##        InpatientDays             ERVisits         OfficeVisits 
##                   67                   61                    3 
##            Narcotics                 Pain          TotalVisits 
##                   49                   28                    1 
## StartedOnCombination    AcuteDrugGapSmall             PoorCare 
##                  125                   65                   98 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
## named list()
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 21.603 22.245   0.643
## 9 partition.data.training          6          0 22.246     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newobs_df) * 1.1 / nrow(glb_trnobs_df)))
    glb_fitobs_df <- glb_trnobs_df[split, ] 
    glb_OOBobs_df <- glb_trnobs_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## [1] "Newdata contains non-NA data for PoorCare.fctr; setting OOB to Newdata"
```

```r
if (!is.null(glb_max_fitobs) && (nrow(glb_fitobs_df) > glb_max_fitobs)) {
    warning("glb_fitobs_df restricted to glb_max_fitobs: ", 
            format(glb_max_fitobs, big.mark=","))
    org_fitobs_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitobs_df[split <- sample.split(org_fitobs_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitobs), ]
    org_fitobs_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_fitobs_df[, glb_id_var], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_OOBobs_df[, glb_id_var], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_vars)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_vars)
    glb_ctgry_df <- merge(newobs_ctgry_df, OOBobs_ctgry_df, by=glb_category_vars
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 15 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_var) && glb_id_var != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_var, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                          id exclude.as.feat rsp_var
## PoorCare.fctr PoorCare.fctr            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
if (glb_id_var != ".rownames")
    print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var)) else
    print(subset(glb_feats_df, rsp_var_raw | rsp_var))    
```

```
##                          id      cor.y exclude.as.feat  cor.y.abs
## 12                 PoorCare 1.00000000            TRUE 1.00000000
## 8                  MemberID 0.06501932            TRUE 0.06501932
## PoorCare.fctr PoorCare.fctr         NA            TRUE         NA
##               cor.high.X freqRatio percentUnique zeroVar   nzv myNearZV
## 12                  <NA>      2.96      2.020202   FALSE FALSE    FALSE
## 8                   <NA>      1.00    100.000000   FALSE FALSE    FALSE
## PoorCare.fctr       <NA>        NA            NA      NA    NA       NA
##               is.cor.y.abs.low interaction.feat rsp_var_raw id_var rsp_var
## 12                       FALSE               NA        TRUE     NA      NA
## 8                         TRUE               NA       FALSE   TRUE      NA
## PoorCare.fctr               NA               NA          NA     NA    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
## character(0)
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 131  18
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 99 17
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 99 17
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 32 17
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 32 17
```

```r
# # Does not handle NULL or length(glb_id_var) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_trnobs_df[, glb_id_var], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_fitobs_df[, glb_id_var], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_OOBobs_df[, glb_id_var], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_newobs_df[, glb_id_var], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 22.246 22.589   0.343
## 10              fit.models          7          0 22.589     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] N Y
## Levels: N Y
## [1] "unique.prob:"
## y
##         N         Y 
## 0.7474747 0.2525253 
## [1] "MFO.val:"
## [1] "N"
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.7474747 0.2525253
## 2 0.7474747 0.2525253
## 3 0.7474747 0.2525253
## 4 0.7474747 0.2525253
## 5 0.7474747 0.2525253
## 6 0.7474747 0.2525253
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.MFO.myMFO_classfr.N
## 1             N                                        74
## 2             Y                                        25
##          Prediction
## Reference  N  Y
##         N 74  0
##         Y 25  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.474747e-01   0.000000e+00   6.501833e-01   8.294361e-01   7.474747e-01 
## AccuracyPValue  McnemarPValue 
##   5.534838e-01   1.586656e-06 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.7474747 0.2525253
## 2 0.7474747 0.2525253
## 3 0.7474747 0.2525253
## 4 0.7474747 0.2525253
## 5 0.7474747 0.2525253
## 6 0.7474747 0.2525253
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.MFO.myMFO_classfr.N
## 1             N                                        24
## 2             Y                                         8
##          Prediction
## Reference  N  Y
##         N 24  0
##         Y  8  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.75000000     0.00000000     0.56595063     0.88538399     0.75000000 
## AccuracyPValue  McnemarPValue 
##     0.59351165     0.01332833 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.299                 0.002         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.7474747
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.6501833             0.8294361             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0             0.75
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5659506              0.885384             0
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Random.myrandom_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      table      numeric  
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "in Random.Classifier$prob"
```

![](D2HE_Claims2_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.4032258
## 2        0.1 0.4032258
## 3        0.2 0.4032258
## 4        0.3 0.1509434
## 5        0.4 0.1509434
## 6        0.5 0.1509434
## 7        0.6 0.1509434
## 8        0.7 0.1509434
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.Random.myrandom_classfr.Y
## 1             N                                              74
## 2             Y                                              25
##          Prediction
## Reference  N  Y
##         N  0 74
##         Y  0 25
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   2.525253e-01   0.000000e+00   1.705639e-01   3.498167e-01   7.474747e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.137287e-17 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in Random.Classifier$prob"
```

![](D2HE_Claims2_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.4000000
## 2        0.1 0.4000000
## 3        0.2 0.4000000
## 4        0.3 0.2666667
## 5        0.4 0.2666667
## 6        0.5 0.2666667
## 7        0.6 0.2666667
## 8        0.7 0.2666667
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.Random.myrandom_classfr.Y
## 1             N                                              24
## 2             Y                                               8
##          Prediction
## Reference  N  Y
##         N  0 24
##         Y  0  8
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   2.500000e-01   0.000000e+00   1.146160e-01   4.340494e-01   7.500000e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.667955e-06 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.229                 0.001   0.4178378
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4032258        0.2525253
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.1705639             0.3498167             0   0.5208333
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2             0.4             0.25
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.114616             0.4340494             0
```

```r
# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: Narcotics, OfficeVisits"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.28 on full training set
```

```
## Loading required package: rpart.plot
```

![](D2HE_Claims2_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 99 
## 
##     CP nsplit rel error
## 1 0.28      0         1
## 
## Node number 1: 99 observations
##   predicted class=N  expected loss=0.2525253  P(node) =1
##     class counts:    74    25
##    probabilities: 0.747 0.253 
## 
## n= 99 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 99 25 N (0.7474747 0.2525253) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1             N                                           74
## 2             Y                                           25
##          Prediction
## Reference  N  Y
##         N 74  0
##         Y 25  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.474747e-01   0.000000e+00   6.501833e-01   8.294361e-01   7.474747e-01 
## AccuracyPValue  McnemarPValue 
##   5.534838e-01   1.586656e-06 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1             N                                           24
## 2             Y                                            8
##          Prediction
## Reference  N  Y
##         N 24  0
##         Y  8  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.75000000     0.00000000     0.56595063     0.88538399     0.75000000 
## AccuracyPValue  McnemarPValue 
##     0.59351165     0.01332833 
##               model_id model_method                   feats
## 1 Max.cor.Y.cv.0.rpart        rpart Narcotics, OfficeVisits
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      1.164                 0.009
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1         0.5                    0.5               0        0.7474747
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.6501833             0.8294361             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0             0.75
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5659506              0.885384             0
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: Narcotics, OfficeVisits"
## Fitting cp = 0 on full training set
```

![](D2HE_Claims2_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 99 
## 
##     CP nsplit rel error
## 1 0.28      0      1.00
## 2 0.02      1      0.72
## 3 0.00      3      0.68
## 
## Variable importance
##    Narcotics OfficeVisits 
##           68           32 
## 
## Node number 1: 99 observations,    complexity param=0.28
##   predicted class=N  expected loss=0.2525253  P(node) =1
##     class counts:    74    25
##    probabilities: 0.747 0.253 
##   left son=2 (90 obs) right son=3 (9 obs)
##   Primary splits:
##       Narcotics    < 19.5 to the left,  improve=8.018182, (0 missing)
##       OfficeVisits < 12.5 to the left,  improve=6.871246, (0 missing)
## 
## Node number 2: 90 observations,    complexity param=0.02
##   predicted class=N  expected loss=0.1888889  P(node) =0.9090909
##     class counts:    73    17
##    probabilities: 0.811 0.189 
##   left son=4 (56 obs) right son=5 (34 obs)
##   Primary splits:
##       OfficeVisits < 12.5 to the left,  improve=2.941223, (0 missing)
##       Narcotics    < 2.5  to the right, improve=0.800000, (0 missing)
##   Surrogate splits:
##       Narcotics < 5.5  to the left,  agree=0.633, adj=0.029, (0 split)
## 
## Node number 3: 9 observations
##   predicted class=Y  expected loss=0.1111111  P(node) =0.09090909
##     class counts:     1     8
##    probabilities: 0.111 0.889 
## 
## Node number 4: 56 observations
##   predicted class=N  expected loss=0.08928571  P(node) =0.5656566
##     class counts:    51     5
##    probabilities: 0.911 0.089 
## 
## Node number 5: 34 observations,    complexity param=0.02
##   predicted class=N  expected loss=0.3529412  P(node) =0.3434343
##     class counts:    22    12
##    probabilities: 0.647 0.353 
##   left son=10 (25 obs) right son=11 (9 obs)
##   Primary splits:
##       OfficeVisits < 21   to the left,  improve=1.004967, (0 missing)
##       Narcotics    < 2.5  to the right, improve=0.778089, (0 missing)
##   Surrogate splits:
##       Narcotics < 7.5  to the left,  agree=0.765, adj=0.111, (0 split)
## 
## Node number 10: 25 observations
##   predicted class=N  expected loss=0.28  P(node) =0.2525253
##     class counts:    18     7
##    probabilities: 0.720 0.280 
## 
## Node number 11: 9 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.09090909
##     class counts:     4     5
##    probabilities: 0.444 0.556 
## 
## n= 99 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 99 25 N (0.74747475 0.25252525)  
##    2) Narcotics< 19.5 90 17 N (0.81111111 0.18888889)  
##      4) OfficeVisits< 12.5 56  5 N (0.91071429 0.08928571) *
##      5) OfficeVisits>=12.5 34 12 N (0.64705882 0.35294118)  
##       10) OfficeVisits< 21 25  7 N (0.72000000 0.28000000) *
##       11) OfficeVisits>=21 9  4 Y (0.44444444 0.55555556) *
##    3) Narcotics>=19.5 9  1 Y (0.11111111 0.88888889) *
## [1] "    calling mypredict_mdl for fit:"
```

![](D2HE_Claims2_files/figure-html/fit.models_0-7.png) 

```
##    threshold   f.score
## 1        0.0 0.4032258
## 2        0.1 0.5882353
## 3        0.2 0.5882353
## 4        0.3 0.6046512
## 5        0.4 0.6046512
## 6        0.5 0.6046512
## 7        0.6 0.4705882
## 8        0.7 0.4705882
## 9        0.8 0.4705882
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1             N                                                69
## 2             Y                                                12
##   PoorCare.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                 5
## 2                                                13
##          Prediction
## Reference  N  Y
##         N 69  5
##         Y 12 13
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.82828283     0.49865952     0.73936108     0.89666668     0.74747475 
## AccuracyPValue  McnemarPValue 
##     0.03739789     0.14561010 
## [1] "    calling mypredict_mdl for OOB:"
```

![](D2HE_Claims2_files/figure-html/fit.models_0-9.png) 

```
##    threshold   f.score
## 1        0.0 0.4000000
## 2        0.1 0.5000000
## 3        0.2 0.5000000
## 4        0.3 0.5555556
## 5        0.4 0.5555556
## 6        0.5 0.5555556
## 7        0.6 0.5454545
## 8        0.7 0.5454545
## 9        0.8 0.5454545
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1             N                                                19
## 2             Y                                                 3
##   PoorCare.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                 5
## 2                                                 5
##          Prediction
## Reference  N  Y
##         N 19  5
##         Y  3  5
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.7500000      0.3846154      0.5659506      0.8853840      0.7500000 
## AccuracyPValue  McnemarPValue 
##      0.5935117      0.7236736 
##                    model_id model_method                   feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart Narcotics, OfficeVisits
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.465                 0.008
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8056757                    0.5       0.6046512        0.8282828
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7393611             0.8966667     0.4986595   0.7838542
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5       0.5555556             0.75
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5659506              0.885384     0.3846154
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: Narcotics, OfficeVisits"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.28 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](D2HE_Claims2_files/figure-html/fit.models_0-11.png) ![](D2HE_Claims2_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 99 
## 
##     CP nsplit rel error
## 1 0.28      0         1
## 
## Node number 1: 99 observations
##   predicted class=N  expected loss=0.2525253  P(node) =1
##     class counts:    74    25
##    probabilities: 0.747 0.253 
## 
## n= 99 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 99 25 N (0.7474747 0.2525253) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.Max.cor.Y.rpart.N
## 1             N                                      74
## 2             Y                                      25
##          Prediction
## Reference  N  Y
##         N 74  0
##         Y 25  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.474747e-01   0.000000e+00   6.501833e-01   8.294361e-01   7.474747e-01 
## AccuracyPValue  McnemarPValue 
##   5.534838e-01   1.586656e-06 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.Max.cor.Y.rpart.N
## 1             N                                      24
## 2             Y                                       8
##          Prediction
## Reference  N  Y
##         N 24  0
##         Y  8  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.75000000     0.00000000     0.56595063     0.88538399     0.75000000 
## AccuracyPValue  McnemarPValue 
##     0.59351165     0.01332833 
##          model_id model_method                   feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart Narcotics, OfficeVisits               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.066                 0.009         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.7171717
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.6501833             0.8294361   0.004975124         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0             0.75
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5659506              0.885384             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.06998185     0.008617168
```

```r
# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.glm"
## [1] "    indep_vars: Narcotics, OfficeVisits"
## Aggregating results
## Fitting final model on full training set
```

![](D2HE_Claims2_files/figure-html/fit.models_0-13.png) ![](D2HE_Claims2_files/figure-html/fit.models_0-14.png) ![](D2HE_Claims2_files/figure-html/fit.models_0-15.png) ![](D2HE_Claims2_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## NULL
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
## [1] "    calling mypredict_mdl for fit:"
```

![](D2HE_Claims2_files/figure-html/fit.models_0-17.png) 

```
##    threshold   f.score
## 1        0.0 0.4032258
## 2        0.1 0.4210526
## 3        0.2 0.5245902
## 4        0.3 0.5777778
## 5        0.4 0.5365854
## 6        0.5 0.5128205
## 7        0.6 0.5405405
## 8        0.7 0.4705882
## 9        0.8 0.1428571
## 10       0.9 0.1481481
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_0-18.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.Max.cor.Y.glm.N
## 1             N                                    67
## 2             Y                                    12
##   PoorCare.fctr.predict.Max.cor.Y.glm.Y
## 1                                     7
## 2                                    13
##          Prediction
## Reference  N  Y
##         N 67  7
##         Y 12 13
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.8080808      0.4555716      0.7166324      0.8803156      0.7474747 
## AccuracyPValue  McnemarPValue 
##      0.0991555      0.3587954 
## [1] "    calling mypredict_mdl for OOB:"
```

![](D2HE_Claims2_files/figure-html/fit.models_0-19.png) 

```
##    threshold   f.score
## 1        0.0 0.4000000
## 2        0.1 0.4324324
## 3        0.2 0.5217391
## 4        0.3 0.6315789
## 5        0.4 0.4615385
## 6        0.5 0.5000000
## 7        0.6 0.3636364
## 8        0.7 0.2000000
## 9        0.8 0.2222222
## 10       0.9 0.2222222
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_0-20.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.Max.cor.Y.glm.N
## 1             N                                    19
## 2             Y                                     2
##   PoorCare.fctr.predict.Max.cor.Y.glm.Y
## 1                                     5
## 2                                     6
##          Prediction
## Reference  N  Y
##         N 19  5
##         Y  2  6
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.7812500      0.4814815      0.6002717      0.9072285      0.7500000 
## AccuracyPValue  McnemarPValue 
##      0.4324708      0.4496918 
##        model_id model_method                   feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm Narcotics, OfficeVisits               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.895                  0.01   0.7745946
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.3       0.5777778        0.8080808
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7166324             0.8803156     0.3783979   0.7994792
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.6315789          0.78125
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.6002717             0.9072285     0.4814815    95.12656
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.03499093       0.2143272
```

```r
if (!is.null(glb_date_vars) && 
    (sum(grepl(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
               names(glb_allobs_df))) > 0)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
            grep(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
                        names(glb_allobs_df), value=TRUE)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.glm"
## [1] "    indep_vars: Narcotics, OfficeVisits, Narcotics:OfficeVisits, Narcotics:Narcotics"
## Aggregating results
## Fitting final model on full training set
```

![](D2HE_Claims2_files/figure-html/fit.models_0-21.png) ![](D2HE_Claims2_files/figure-html/fit.models_0-22.png) ![](D2HE_Claims2_files/figure-html/fit.models_0-23.png) ![](D2HE_Claims2_files/figure-html/fit.models_0-24.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.82094  -0.64557  -0.48623   0.03981   2.18076  
## 
## Coefficients:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -2.865354   0.592384  -4.837 1.32e-06 ***
## Narcotics                 0.164955   0.105692   1.561  0.11859    
## OfficeVisits              0.093530   0.033775   2.769  0.00562 ** 
## `Narcotics:OfficeVisits` -0.004276   0.004585  -0.933  0.35097    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  88.328  on 95  degrees of freedom
## AIC: 96.328
## 
## Number of Fisher Scoring iterations: 4
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](D2HE_Claims2_files/figure-html/fit.models_0-25.png) 

```
##    threshold    f.score
## 1        0.0 0.40322581
## 2        0.1 0.45283019
## 3        0.2 0.51612903
## 4        0.3 0.53061224
## 5        0.4 0.53658537
## 6        0.5 0.51282051
## 7        0.6 0.52631579
## 8        0.7 0.51428571
## 9        0.8 0.32258065
## 10       0.9 0.07692308
## 11       1.0 0.00000000
```

![](D2HE_Claims2_files/figure-html/fit.models_0-26.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.Interact.High.cor.Y.glm.N
## 1             N                                              69
## 2             Y                                              14
##   PoorCare.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                               5
## 2                                              11
##          Prediction
## Reference  N  Y
##         N 69  5
##         Y 14 11
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.80808081     0.42282909     0.71663237     0.88031565     0.74747475 
## AccuracyPValue  McnemarPValue 
##     0.09915550     0.06645742 
## [1] "    calling mypredict_mdl for OOB:"
```

![](D2HE_Claims2_files/figure-html/fit.models_0-27.png) 

```
##    threshold   f.score
## 1        0.0 0.4000000
## 2        0.1 0.4444444
## 3        0.2 0.5000000
## 4        0.3 0.5714286
## 5        0.4 0.6666667
## 6        0.5 0.6153846
## 7        0.6 0.5000000
## 8        0.7 0.3636364
## 9        0.8 0.2222222
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_0-28.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.Interact.High.cor.Y.glm.N
## 1             N                                              22
## 2             Y                                               3
##   PoorCare.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                               2
## 2                                               5
##          Prediction
## Reference  N  Y
##         N 22  2
##         Y  3  5
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.8437500      0.5652174      0.6721212      0.9472494      0.7500000 
## AccuracyPValue  McnemarPValue 
##      0.1530031      1.0000000 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                  feats
## 1 Narcotics, OfficeVisits, Narcotics:OfficeVisits, Narcotics:Narcotics
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.006                 0.007
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7805405                    0.4       0.5365854        0.7878788
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7166324             0.8803156     0.3036687   0.7994792
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.6666667          0.84375
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.6721212             0.9472494     0.5652174    96.32839
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.03030303       0.1761507
```

```r
# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.glm"
## [1] "    indep_vars: Narcotics, OfficeVisits, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm"
## Aggregating results
## Fitting final model on full training set
```

![](D2HE_Claims2_files/figure-html/fit.models_0-29.png) ![](D2HE_Claims2_files/figure-html/fit.models_0-30.png) ![](D2HE_Claims2_files/figure-html/fit.models_0-31.png) ![](D2HE_Claims2_files/figure-html/fit.models_0-32.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.91016  -0.63358  -0.38098  -0.03883   2.18847  
## 
## Coefficients:
##                           Estimate Std. Error z value Pr(>|z|)   
## (Intercept)              -1.041241   1.174013  -0.887  0.37513   
## Narcotics                 0.113998   0.043616   2.614  0.00896 **
## OfficeVisits              0.080484   0.045269   1.778  0.07542 . 
## StartedOnCombinationTRUE  1.542598   1.753659   0.880  0.37905   
## ProviderCount             0.037269   0.033465   1.114  0.26542   
## MedicalClaims             0.017201   0.024309   0.708  0.47919   
## ERVisits                 -0.278985   0.247024  -1.129  0.25874   
## ClaimLines               -0.014086   0.009103  -1.547  0.12177   
## InpatientDays             0.157996   0.091904   1.719  0.08559 . 
## Pain                     -0.002224   0.016742  -0.133  0.89431   
## DaysSinceLastERVisit     -0.003257   0.001803  -1.807  0.07077 . 
## .rnorm                   -0.459905   0.294492  -1.562  0.11836   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  77.309  on 87  degrees of freedom
## AIC: 101.31
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](D2HE_Claims2_files/figure-html/fit.models_0-33.png) 

```
##    threshold   f.score
## 1        0.0 0.4032258
## 2        0.1 0.5217391
## 3        0.2 0.6153846
## 4        0.3 0.5882353
## 5        0.4 0.5000000
## 6        0.5 0.5128205
## 7        0.6 0.5128205
## 8        0.7 0.5555556
## 9        0.8 0.4242424
## 10       0.9 0.2758621
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_0-34.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.Low.cor.X.glm.N
## 1             N                                    54
## 2             Y                                     5
##   PoorCare.fctr.predict.Low.cor.X.glm.Y
## 1                                    20
## 2                                    20
##          Prediction
## Reference  N  Y
##         N 54 20
##         Y  5 20
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##    0.747474747    0.441939121    0.650183274    0.829436096    0.747474747 
## AccuracyPValue  McnemarPValue 
##    0.553483804    0.005110261 
## [1] "    calling mypredict_mdl for OOB:"
```

![](D2HE_Claims2_files/figure-html/fit.models_0-35.png) 

```
##    threshold   f.score
## 1        0.0 0.4000000
## 2        0.1 0.4705882
## 3        0.2 0.5925926
## 4        0.3 0.5833333
## 5        0.4 0.5263158
## 6        0.5 0.4705882
## 7        0.6 0.4615385
## 8        0.7 0.3636364
## 9        0.8 0.3636364
## 10       0.9 0.2000000
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_0-36.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.Low.cor.X.glm.N
## 1             N                                    13
## 2             Y                                    NA
##   PoorCare.fctr.predict.Low.cor.X.glm.Y
## 1                                    11
## 2                                     8
##          Prediction
## Reference  N  Y
##         N 13 11
##         Y  0  8
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##    0.656250000    0.371428571    0.468068964    0.814280908    0.750000000 
## AccuracyPValue  McnemarPValue 
##    0.919569586    0.002568832 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                  feats
## 1 Narcotics, OfficeVisits, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.917                 0.011
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8551351                    0.2       0.6153846        0.6969697
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.6501833             0.8294361     0.2127595   0.8072917
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.5925926          0.65625
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1              0.468069             0.8142809     0.3714286    101.3093
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.09090909      0.03073747
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 22.589 44.367  21.779
## 11 fit.models          7          1 44.368     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor    bgn end elapsed
## 1 fit.models_1_bgn          1          0 50.392  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 50.392 50.408   0.016
## 2 fit.models_1_glm          2          0 50.409     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm"
## Aggregating results
## Fitting final model on full training set
```

![](D2HE_Claims2_files/figure-html/fit.models_1-1.png) ![](D2HE_Claims2_files/figure-html/fit.models_1-2.png) ![](D2HE_Claims2_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.65164  -0.53424  -0.32405  -0.05238   2.15660  
## 
## Coefficients: (1 not defined because of singularities)
##                           Estimate Std. Error z value Pr(>|z|)   
## (Intercept)              -1.593724   1.279009  -1.246  0.21274   
## Narcotics                 0.066497   0.050182   1.325  0.18513   
## OfficeVisits             -0.082001   0.097735  -0.839  0.40146   
## TotalVisits               0.168408   0.101096   1.666  0.09575 . 
## AcuteDrugGapSmall         0.278584   0.106011   2.628  0.00859 **
## StartedOnCombinationTRUE  2.140366   1.952199   1.096  0.27291   
## ProviderCount             0.029512   0.033563   0.879  0.37923   
## MedicalClaims             0.016256   0.024718   0.658  0.51075   
## ERVisits                 -0.537541   0.325498  -1.651  0.09865 . 
## ClaimLines               -0.011584   0.009414  -1.231  0.21851   
## InpatientDays                   NA         NA      NA       NA   
## Pain                      0.005414   0.016051   0.337  0.73590   
## DaysSinceLastERVisit     -0.003692   0.001945  -1.898  0.05765 . 
## .rnorm                   -0.503638   0.319494  -1.576  0.11494   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  69.917  on 86  degrees of freedom
## AIC: 95.917
## 
## Number of Fisher Scoring iterations: 6
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](D2HE_Claims2_files/figure-html/fit.models_1-4.png) ![](D2HE_Claims2_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.4032258
## 2        0.1 0.6075949
## 3        0.2 0.6666667
## 4        0.3 0.6181818
## 5        0.4 0.5531915
## 6        0.5 0.5581395
## 7        0.6 0.5263158
## 8        0.7 0.5142857
## 9        0.8 0.4848485
## 10       0.9 0.4375000
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.All.X.glm.N
## 1             N                                57
## 2             Y                                 4
##   PoorCare.fctr.predict.All.X.glm.Y
## 1                                17
## 2                                21
##          Prediction
## Reference  N  Y
##         N 57 17
##         Y  4 21
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##    0.787878788    0.520636385    0.694212957    0.863638064    0.747474747 
## AccuracyPValue  McnemarPValue 
##    0.211012636    0.008828761 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](D2HE_Claims2_files/figure-html/fit.models_1-6.png) ![](D2HE_Claims2_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.4000000
## 2        0.1 0.5517241
## 3        0.2 0.6400000
## 4        0.3 0.5833333
## 5        0.4 0.5555556
## 6        0.5 0.5714286
## 7        0.6 0.5714286
## 8        0.7 0.6153846
## 9        0.8 0.5000000
## 10       0.9 0.3636364
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.All.X.glm.N
## 1             N                                15
## 2             Y                                NA
##   PoorCare.fctr.predict.All.X.glm.Y
## 1                                 9
## 2                                 8
##          Prediction
## Reference  N  Y
##         N 15  9
##         Y  0  8
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##    0.718750000    0.454545455    0.532528900    0.862543097    0.750000000 
## AccuracyPValue  McnemarPValue 
##    0.736659037    0.007660761 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                  feats
## 1 Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.125                 0.013
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8848649                    0.2       0.6666667        0.7070707
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.694213             0.8636381     0.1930697   0.8541667
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2            0.64          0.71875
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.5325289             0.8625431     0.4545455    95.91667
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.03499093       0.2172201
##                   label step_major step_minor    bgn    end elapsed
## 2      fit.models_1_glm          2          0 50.409 56.408   5.999
## 3 fit.models_1_bayesglm          3          0 56.408     NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm"
```

```
## Loading required package: arm
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: Matrix
## Loading required package: lme4
## 
## arm (Version 1.8-5, built: 2015-05-13)
## 
## Working directory is /Users/bbalaji-2012/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Lectures/LCTR3_D2HawkEye_MedClaims
```

![](D2HE_Claims2_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.56847  -0.58420  -0.40249  -0.07502   2.05612  
## 
## Coefficients:
##                            Estimate Std. Error z value Pr(>|z|)  
## (Intercept)              -2.0351408  1.1460498  -1.776   0.0758 .
## Narcotics                 0.0538456  0.0402935   1.336   0.1814  
## OfficeVisits              0.0436625  0.0766877   0.569   0.5691  
## TotalVisits               0.0332410  0.0743855   0.447   0.6550  
## AcuteDrugGapSmall         0.2202023  0.0885618   2.486   0.0129 *
## StartedOnCombinationTRUE  1.2828101  1.3187011   0.973   0.3307  
## ProviderCount             0.0205619  0.0277738   0.740   0.4591  
## MedicalClaims             0.0063483  0.0179182   0.354   0.7231  
## ERVisits                 -0.2212218  0.2158220  -1.025   0.3054  
## ClaimLines               -0.0061446  0.0064662  -0.950   0.3420  
## InpatientDays             0.0800453  0.1075530   0.744   0.4567  
## Pain                      0.0005311  0.0143102   0.037   0.9704  
## DaysSinceLastERVisit     -0.0022221  0.0015027  -1.479   0.1392  
## .rnorm                   -0.4477182  0.2915976  -1.535   0.1247  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  71.222  on 85  degrees of freedom
## AIC: 99.222
## 
## Number of Fisher Scoring iterations: 14
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](D2HE_Claims2_files/figure-html/fit.models_1-9.png) 

```
##    threshold   f.score
## 1        0.0 0.4032258
## 2        0.1 0.5681818
## 3        0.2 0.6562500
## 4        0.3 0.6296296
## 5        0.4 0.5777778
## 6        0.5 0.5128205
## 7        0.6 0.5405405
## 8        0.7 0.4705882
## 9        0.8 0.4848485
## 10       0.9 0.3870968
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.All.X.bayesglm.N
## 1             N                                     56
## 2             Y                                      4
##   PoorCare.fctr.predict.All.X.bayesglm.Y
## 1                                     18
## 2                                     21
##          Prediction
## Reference  N  Y
##         N 56 18
##         Y  4 21
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##    0.777777778    0.503419973    0.683109156    0.855188123    0.747474747 
## AccuracyPValue  McnemarPValue 
##    0.285995041    0.005577994 
## [1] "    calling mypredict_mdl for OOB:"
```

![](D2HE_Claims2_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.4000000
## 2        0.1 0.5000000
## 3        0.2 0.6153846
## 4        0.3 0.5217391
## 5        0.4 0.6250000
## 6        0.5 0.5333333
## 7        0.6 0.5000000
## 8        0.7 0.5000000
## 9        0.8 0.5000000
## 10       0.9 0.2000000
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.All.X.bayesglm.N
## 1             N                                     21
## 2             Y                                      3
##   PoorCare.fctr.predict.All.X.bayesglm.Y
## 1                                      3
## 2                                      5
##          Prediction
## Reference  N  Y
##         N 21  3
##         Y  3  5
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.8125000      0.5000000      0.6356077      0.9279238      0.7500000 
## AccuracyPValue  McnemarPValue 
##      0.2778717      1.0000000 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                  feats
## 1 Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      3.261                 0.033
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8881081                    0.2         0.65625        0.7373737
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.6831092             0.8551881     0.2244709   0.8541667
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4           0.625           0.8125
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.6356077             0.9279238           0.5    99.22172
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.01749546       0.2448595
##                   label step_major step_minor    bgn    end elapsed
## 3 fit.models_1_bayesglm          3          0 56.408 63.938   7.531
## 4    fit.models_1_rpart          4          0 63.940     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.28 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](D2HE_Claims2_files/figure-html/fit.models_1-13.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 99 
## 
##     CP nsplit rel error
## 1 0.28      0         1
## 
## Node number 1: 99 observations
##   predicted class=N  expected loss=0.2525253  P(node) =1
##     class counts:    74    25
##    probabilities: 0.747 0.253 
## 
## n= 99 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 99 25 N (0.7474747 0.2525253) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.All.X.no.rnorm.rpart.N
## 1             N                                           74
## 2             Y                                           25
##          Prediction
## Reference  N  Y
##         N 74  0
##         Y 25  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.474747e-01   0.000000e+00   6.501833e-01   8.294361e-01   7.474747e-01 
## AccuracyPValue  McnemarPValue 
##   5.534838e-01   1.586656e-06 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.All.X.no.rnorm.rpart.N
## 1             N                                           24
## 2             Y                                            8
##          Prediction
## Reference  N  Y
##         N 24  0
##         Y  8  0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.75000000     0.00000000     0.56595063     0.88538399     0.75000000 
## AccuracyPValue  McnemarPValue 
##     0.59351165     0.01332833 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                          feats
## 1 Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      0.933                 0.015
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1         0.5                    0.5               0        0.7272727
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.6501833             0.8294361   0.002008032         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0             0.75
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5659506              0.885384             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.03030303     0.003478014
##                label step_major step_minor    bgn    end elapsed
## 4 fit.models_1_rpart          4          0 63.940 66.748   2.808
## 5    fit.models_1_rf          5          0 66.749     NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](D2HE_Claims2_files/figure-html/fit.models_1-14.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 2 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: mtry
```

![](D2HE_Claims2_files/figure-html/fit.models_1-15.png) ![](D2HE_Claims2_files/figure-html/fit.models_1-16.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted         99   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes            198   matrix     numeric  
## oob.times         99   -none-     numeric  
## classes            2   -none-     character
## importance        12   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                 99   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            12   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](D2HE_Claims2_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.4032258
## 2        0.1 0.6849315
## 3        0.2 0.9090909
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 0.8372093
## 9        0.8 0.5294118
## 10       0.9 0.2142857
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.All.X.no.rnorm.rf.N
## 1             N                                        74
## 2             Y                                        NA
##   PoorCare.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                        NA
## 2                                        25
##          Prediction
## Reference  N  Y
##         N 74  0
##         Y  0 25
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.634243e-01   1.000000e+00   7.474747e-01 
## AccuracyPValue  McnemarPValue 
##   3.062358e-13            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](D2HE_Claims2_files/figure-html/fit.models_1-19.png) 

```
##    threshold   f.score
## 1        0.0 0.4000000
## 2        0.1 0.4000000
## 3        0.2 0.4827586
## 4        0.3 0.5714286
## 5        0.4 0.4210526
## 6        0.5 0.4285714
## 7        0.6 0.4000000
## 8        0.7 0.2222222
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](D2HE_Claims2_files/figure-html/fit.models_1-20.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   PoorCare.fctr PoorCare.fctr.predict.All.X.no.rnorm.rf.N
## 1             N                                        17
## 2             Y                                         2
##   PoorCare.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                         7
## 2                                         6
##          Prediction
## Reference  N  Y
##         N 17  7
##         Y  2  6
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.7187500      0.3793103      0.5325289      0.8625431      0.7500000 
## AccuracyPValue  McnemarPValue 
##      0.7366590      0.1824224 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                          feats
## 1 Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.733                 0.098
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.6               1        0.7272727
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9634243                     1    0.08894493   0.7682292
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.5714286          0.71875
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5325289             0.8625431     0.3793103
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.05248639       0.1050316
```

```r
# User specified
#   Ensure at least 2 vars in each regression; else varImp crashes
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df

    # easier to exclude features
#model_id <- "";
# indep_vars_vctr <- head(subset(glb_models_df, grepl("All\\.X\\.", model_id), select=feats), 1)
# indep_vars_vctr <- setdiff(indep_vars_vctr, ".rnorm")

    # easier to include features
#model_id <- "Rank9.2"; indep_vars_vctr <- c(NULL
#    ,"<feat1>"
#    ,"<feat1>*<feat2>"
#    ,"<feat1>:<feat2>"
#                                            )
# for (method in c("bayesglm")) {
#     ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                     n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
#     csm_mdl_id <- paste0(model_id, ".", method)
#     csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
# }

# Ntv.1.lm <- lm(reformulate(indep_vars_vctr, glb_rsp_var), glb_trnobs_df); print(summary(Ntv.1.lm))

#print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
##                                                                                                                                                                                                          feats
## MFO.myMFO_classfr                                                                                                                                                                                       .rnorm
## Random.myrandom_classfr                                                                                                                                                                                 .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                   Narcotics, OfficeVisits
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                              Narcotics, OfficeVisits
## Max.cor.Y.rpart                                                                                                                                                                        Narcotics, OfficeVisits
## Max.cor.Y.glm                                                                                                                                                                          Narcotics, OfficeVisits
## Interact.High.cor.Y.glm                                                                                                                   Narcotics, OfficeVisits, Narcotics:OfficeVisits, Narcotics:Narcotics
## Low.cor.X.glm                                             Narcotics, OfficeVisits, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm
## All.X.glm                 Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm
## All.X.bayesglm            Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm
## All.X.no.rnorm.rpart              Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit
## All.X.no.rnorm.rf                 Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                       0                      0.299
## Random.myrandom_classfr                 0                      0.229
## Max.cor.Y.cv.0.rpart                    0                      1.164
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.465
## Max.cor.Y.rpart                         3                      1.066
## Max.cor.Y.glm                           1                      0.895
## Interact.High.cor.Y.glm                 1                      1.006
## Low.cor.X.glm                           1                      0.917
## All.X.glm                               1                      1.125
## All.X.bayesglm                          1                      3.261
## All.X.no.rnorm.rpart                    3                      0.933
## All.X.no.rnorm.rf                       3                      1.733
##                           min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                         0.002   0.5000000
## Random.myrandom_classfr                   0.001   0.4178378
## Max.cor.Y.cv.0.rpart                      0.009   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.008   0.8056757
## Max.cor.Y.rpart                           0.009   0.5000000
## Max.cor.Y.glm                             0.010   0.7745946
## Interact.High.cor.Y.glm                   0.007   0.7805405
## Low.cor.X.glm                             0.011   0.8551351
## All.X.glm                                 0.013   0.8848649
## All.X.bayesglm                            0.033   0.8881081
## All.X.no.rnorm.rpart                      0.015   0.5000000
## All.X.no.rnorm.rf                         0.098   1.0000000
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.2       0.4032258
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.5       0.6046512
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.3       0.5777778
## Interact.High.cor.Y.glm                      0.4       0.5365854
## Low.cor.X.glm                                0.2       0.6153846
## All.X.glm                                    0.2       0.6666667
## All.X.bayesglm                               0.2       0.6562500
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.6       1.0000000
##                           max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                0.7474747             0.6501833
## Random.myrandom_classfr          0.2525253             0.1705639
## Max.cor.Y.cv.0.rpart             0.7474747             0.6501833
## Max.cor.Y.cv.0.cp.0.rpart        0.8282828             0.7393611
## Max.cor.Y.rpart                  0.7171717             0.6501833
## Max.cor.Y.glm                    0.8080808             0.7166324
## Interact.High.cor.Y.glm          0.7878788             0.7166324
## Low.cor.X.glm                    0.6969697             0.6501833
## All.X.glm                        0.7070707             0.6942130
## All.X.bayesglm                   0.7373737             0.6831092
## All.X.no.rnorm.rpart             0.7272727             0.6501833
## All.X.no.rnorm.rf                0.7272727             0.9634243
##                           max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                     0.8294361   0.000000000   0.5000000
## Random.myrandom_classfr               0.3498167   0.000000000   0.5208333
## Max.cor.Y.cv.0.rpart                  0.8294361   0.000000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8966667   0.498659517   0.7838542
## Max.cor.Y.rpart                       0.8294361   0.004975124   0.5000000
## Max.cor.Y.glm                         0.8803156   0.378397889   0.7994792
## Interact.High.cor.Y.glm               0.8803156   0.303668706   0.7994792
## Low.cor.X.glm                         0.8294361   0.212759506   0.8072917
## All.X.glm                             0.8636381   0.193069668   0.8541667
## All.X.bayesglm                        0.8551881   0.224470934   0.8541667
## All.X.no.rnorm.rpart                  0.8294361   0.002008032   0.5000000
## All.X.no.rnorm.rf                     1.0000000   0.088944927   0.7682292
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.2       0.4000000
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.5       0.5555556
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.3       0.6315789
## Interact.High.cor.Y.glm                      0.4       0.6666667
## Low.cor.X.glm                                0.2       0.5925926
## All.X.glm                                    0.2       0.6400000
## All.X.bayesglm                               0.4       0.6250000
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.3       0.5714286
##                           max.Accuracy.OOB max.AccuracyLower.OOB
## MFO.myMFO_classfr                  0.75000             0.5659506
## Random.myrandom_classfr            0.25000             0.1146160
## Max.cor.Y.cv.0.rpart               0.75000             0.5659506
## Max.cor.Y.cv.0.cp.0.rpart          0.75000             0.5659506
## Max.cor.Y.rpart                    0.75000             0.5659506
## Max.cor.Y.glm                      0.78125             0.6002717
## Interact.High.cor.Y.glm            0.84375             0.6721212
## Low.cor.X.glm                      0.65625             0.4680690
## All.X.glm                          0.71875             0.5325289
## All.X.bayesglm                     0.81250             0.6356077
## All.X.no.rnorm.rpart               0.75000             0.5659506
## All.X.no.rnorm.rf                  0.71875             0.5325289
##                           max.AccuracyUpper.OOB max.Kappa.OOB
## MFO.myMFO_classfr                     0.8853840     0.0000000
## Random.myrandom_classfr               0.4340494     0.0000000
## Max.cor.Y.cv.0.rpart                  0.8853840     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart             0.8853840     0.3846154
## Max.cor.Y.rpart                       0.8853840     0.0000000
## Max.cor.Y.glm                         0.9072285     0.4814815
## Interact.High.cor.Y.glm               0.9472494     0.5652174
## Low.cor.X.glm                         0.8142809     0.3714286
## All.X.glm                             0.8625431     0.4545455
## All.X.bayesglm                        0.9279238     0.5000000
## All.X.no.rnorm.rpart                  0.8853840     0.0000000
## All.X.no.rnorm.rf                     0.8625431     0.3793103
##                           max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                         NA              NA          NA
## Random.myrandom_classfr                   NA              NA          NA
## Max.cor.Y.cv.0.rpart                      NA              NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA              NA          NA
## Max.cor.Y.rpart                   0.06998185     0.008617168          NA
## Max.cor.Y.glm                     0.03499093     0.214327175    95.12656
## Interact.High.cor.Y.glm           0.03030303     0.176150724    96.32839
## Low.cor.X.glm                     0.09090909     0.030737468   101.30927
## All.X.glm                         0.03499093     0.217220064    95.91667
## All.X.bayesglm                    0.01749546     0.244859488    99.22172
## All.X.no.rnorm.rpart              0.03030303     0.003478014          NA
## All.X.no.rnorm.rf                 0.05248639     0.105031564          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5  fit.models_1_rf          5          0 66.749 82.977  16.228
## 6 fit.models_1_end          6          0 82.977     NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 11 fit.models          7          1 44.368 82.984  38.616
## 12 fit.models          7          2 82.984     NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id",
                                    grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df),
                                    grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
##                                                                                                                                                                                                          feats
## MFO.myMFO_classfr                                                                                                                                                                                       .rnorm
## Random.myrandom_classfr                                                                                                                                                                                 .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                   Narcotics, OfficeVisits
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                              Narcotics, OfficeVisits
## Max.cor.Y.rpart                                                                                                                                                                        Narcotics, OfficeVisits
## Max.cor.Y.glm                                                                                                                                                                          Narcotics, OfficeVisits
## Interact.High.cor.Y.glm                                                                                                                   Narcotics, OfficeVisits, Narcotics:OfficeVisits, Narcotics:Narcotics
## Low.cor.X.glm                                             Narcotics, OfficeVisits, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm
## All.X.glm                 Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm
## All.X.bayesglm            Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit, .rnorm
## All.X.no.rnorm.rpart              Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit
## All.X.no.rnorm.rf                 Narcotics, OfficeVisits, TotalVisits, AcuteDrugGapSmall, StartedOnCombination, ProviderCount, MedicalClaims, ERVisits, ClaimLines, InpatientDays, Pain, DaysSinceLastERVisit
##                           max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                       0   0.5000000
## Random.myrandom_classfr                 0   0.4178378
## Max.cor.Y.cv.0.rpart                    0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart               0   0.8056757
## Max.cor.Y.rpart                         3   0.5000000
## Max.cor.Y.glm                           1   0.7745946
## Interact.High.cor.Y.glm                 1   0.7805405
## Low.cor.X.glm                           1   0.8551351
## All.X.glm                               1   0.8848649
## All.X.bayesglm                          1   0.8881081
## All.X.no.rnorm.rpart                    3   0.5000000
## All.X.no.rnorm.rf                       3   1.0000000
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.2       0.4032258
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.5       0.6046512
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.3       0.5777778
## Interact.High.cor.Y.glm                      0.4       0.5365854
## Low.cor.X.glm                                0.2       0.6153846
## All.X.glm                                    0.2       0.6666667
## All.X.bayesglm                               0.2       0.6562500
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.6       1.0000000
##                           max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                0.7474747   0.000000000   0.5000000
## Random.myrandom_classfr          0.2525253   0.000000000   0.5208333
## Max.cor.Y.cv.0.rpart             0.7474747   0.000000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart        0.8282828   0.498659517   0.7838542
## Max.cor.Y.rpart                  0.7171717   0.004975124   0.5000000
## Max.cor.Y.glm                    0.8080808   0.378397889   0.7994792
## Interact.High.cor.Y.glm          0.7878788   0.303668706   0.7994792
## Low.cor.X.glm                    0.6969697   0.212759506   0.8072917
## All.X.glm                        0.7070707   0.193069668   0.8541667
## All.X.bayesglm                   0.7373737   0.224470934   0.8541667
## All.X.no.rnorm.rpart             0.7272727   0.002008032   0.5000000
## All.X.no.rnorm.rf                0.7272727   0.088944927   0.7682292
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.2       0.4000000
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.5       0.5555556
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.3       0.6315789
## Interact.High.cor.Y.glm                      0.4       0.6666667
## Low.cor.X.glm                                0.2       0.5925926
## All.X.glm                                    0.2       0.6400000
## All.X.bayesglm                               0.4       0.6250000
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.3       0.5714286
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                  0.75000     0.0000000
## Random.myrandom_classfr            0.25000     0.0000000
## Max.cor.Y.cv.0.rpart               0.75000     0.0000000
## Max.cor.Y.cv.0.cp.0.rpart          0.75000     0.3846154
## Max.cor.Y.rpart                    0.75000     0.0000000
## Max.cor.Y.glm                      0.78125     0.4814815
## Interact.High.cor.Y.glm            0.84375     0.5652174
## Low.cor.X.glm                      0.65625     0.3714286
## All.X.glm                          0.71875     0.4545455
## All.X.bayesglm                     0.81250     0.5000000
## All.X.no.rnorm.rpart               0.75000     0.0000000
## All.X.no.rnorm.rf                  0.71875     0.3793103
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                          3.3444816             500.00000
## Random.myrandom_classfr                    4.3668122            1000.00000
## Max.cor.Y.cv.0.rpart                       0.8591065             111.11111
## Max.cor.Y.cv.0.cp.0.rpart                  2.1505376             125.00000
## Max.cor.Y.rpart                            0.9380863             111.11111
## Max.cor.Y.glm                              1.1173184             100.00000
## Interact.High.cor.Y.glm                    0.9940358             142.85714
## Low.cor.X.glm                              1.0905125              90.90909
## All.X.glm                                  0.8888889              76.92308
## All.X.bayesglm                             0.3066544              30.30303
## All.X.no.rnorm.rpart                       1.0718114              66.66667
## All.X.no.rnorm.rf                          0.5770340              10.20408
##                           inv.aic.fit
## MFO.myMFO_classfr                  NA
## Random.myrandom_classfr            NA
## Max.cor.Y.cv.0.rpart               NA
## Max.cor.Y.cv.0.cp.0.rpart          NA
## Max.cor.Y.rpart                    NA
## Max.cor.Y.glm             0.010512311
## Interact.High.cor.Y.glm   0.010381156
## Low.cor.X.glm             0.009870765
## All.X.glm                 0.010425717
## All.X.bayesglm            0.010078439
## All.X.no.rnorm.rpart               NA
## All.X.no.rnorm.rf                  NA
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 4 rows containing missing values (geom_path).
```

```
## Warning: Removed 87 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

![](D2HE_Claims2_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## Warning: max.AccuracyUpper.fit already exists in glb_models_df
```

```
## [1] "var:max.KappaSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

![](D2HE_Claims2_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 7    Interact.High.cor.Y.glm          0.84375   0.7994792     0.5652174
## 10            All.X.bayesglm          0.81250   0.8541667     0.5000000
## 6              Max.cor.Y.glm          0.78125   0.7994792     0.4814815
## 4  Max.cor.Y.cv.0.cp.0.rpart          0.75000   0.7838542     0.3846154
## 1          MFO.myMFO_classfr          0.75000   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart          0.75000   0.5000000     0.0000000
## 5            Max.cor.Y.rpart          0.75000   0.5000000     0.0000000
## 11      All.X.no.rnorm.rpart          0.75000   0.5000000     0.0000000
## 9                  All.X.glm          0.71875   0.8541667     0.4545455
## 12         All.X.no.rnorm.rf          0.71875   0.7682292     0.3793103
## 8              Low.cor.X.glm          0.65625   0.8072917     0.3714286
## 2    Random.myrandom_classfr          0.25000   0.5208333     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 7     96.32839                    0.4
## 10    99.22172                    0.4
## 6     95.12656                    0.3
## 4           NA                    0.5
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 11          NA                    0.5
## 9     95.91667                    0.2
## 12          NA                    0.3
## 8    101.30927                    0.2
## 2           NA                    0.2
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 38 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually if you must have them.
```

![](D2HE_Claims2_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~-max.Accuracy.OOB - max.auc.OOB - max.Kappa.OOB + min.aic.fit - 
##     opt.prob.threshold.OOB
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: Interact.High.cor.Y.glm"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
#     if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
#         warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
#         glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
#     }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](D2HE_Claims2_files/figure-html/fit.models_2-4.png) ![](D2HE_Claims2_files/figure-html/fit.models_2-5.png) ![](D2HE_Claims2_files/figure-html/fit.models_2-6.png) ![](D2HE_Claims2_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.82094  -0.64557  -0.48623   0.03981   2.18076  
## 
## Coefficients:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -2.865354   0.592384  -4.837 1.32e-06 ***
## Narcotics                 0.164955   0.105692   1.561  0.11859    
## OfficeVisits              0.093530   0.033775   2.769  0.00562 ** 
## `Narcotics:OfficeVisits` -0.004276   0.004585  -0.933  0.35097    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  88.328  on 95  degrees of freedom
## AIC: 96.328
## 
## Number of Fisher Scoring iterations: 4
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

#stop(here"); #sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                          importance Interact.High.cor.Y.glm.importance
## OfficeVisits              100.00000                          100.00000
## Narcotics                  34.19471                           34.19471
## `Narcotics:OfficeVisits`    0.00000                            0.00000
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
    
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))

#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_var)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_var,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

![](D2HE_Claims2_files/figure-html/fit.models_2-8.png) ![](D2HE_Claims2_files/figure-html/fit.models_2-9.png) ![](D2HE_Claims2_files/figure-html/fit.models_2-10.png) 

```
## [1] "Min/Max Boundaries: "
##    MemberID PoorCare.fctr
## 41       41             Y
## 10       10             N
## 31       31             N
## 84       84             Y
## 52       52             N
## 32       32             N
##    PoorCare.fctr.predict.Interact.High.cor.Y.glm.prob
## 41                                         0.18810096
## 10                                         0.07341038
## 31                                         0.06426642
## 84                                         0.88492610
## 52                                         0.43868496
## 32                                         0.79306781
##    PoorCare.fctr.predict.Interact.High.cor.Y.glm
## 41                                             N
## 10                                             N
## 31                                             N
## 84                                             Y
## 52                                             Y
## 32                                             Y
##    PoorCare.fctr.predict.Interact.High.cor.Y.glm.accurate
## 41                                                  FALSE
## 10                                                   TRUE
## 31                                                   TRUE
## 84                                                   TRUE
## 52                                                  FALSE
## 32                                                  FALSE
##    PoorCare.fctr.predict.Interact.High.cor.Y.glm.error .label
## 41                                         -0.21189904     41
## 10                                          0.00000000     10
## 31                                          0.00000000     31
## 84                                          0.00000000     84
## 52                                          0.03868496     52
## 32                                          0.39306781     32
## [1] "Inaccurate: "
##    MemberID PoorCare.fctr
## 9         9             Y
## 41       41             Y
## 30       30             Y
## 52       52             N
## 32       32             N
##    PoorCare.fctr.predict.Interact.High.cor.Y.glm.prob
## 9                                           0.1142866
## 41                                          0.1881010
## 30                                          0.3892902
## 52                                          0.4386850
## 32                                          0.7930678
##    PoorCare.fctr.predict.Interact.High.cor.Y.glm
## 9                                              N
## 41                                             N
## 30                                             N
## 52                                             Y
## 32                                             Y
##    PoorCare.fctr.predict.Interact.High.cor.Y.glm.accurate
## 9                                                   FALSE
## 41                                                  FALSE
## 30                                                  FALSE
## 52                                                  FALSE
## 32                                                  FALSE
##    PoorCare.fctr.predict.Interact.High.cor.Y.glm.error
## 9                                          -0.28571341
## 41                                         -0.21189904
## 30                                         -0.01070981
## 52                                          0.03868496
## 32                                          0.39306781
```

![](D2HE_Claims2_files/figure-html/fit.models_2-11.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
# FN_OOB_ids <- c(4721, 4020, 693, 92)
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_feats_df$id[1:5]])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
write.csv(glb_OOBobs_df[, c(glb_id_var, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

# write.csv(glb_chunks_df, paste0(glb_out_pfx, tail(glb_chunks_df, 1)$label, "_",
#                                 tail(glb_chunks_df, 1)$step_minor,  "_chunks1.csv"),
#           row.names=FALSE)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn    end elapsed
## 12 fit.models          7          2  82.984 104.21  21.227
## 13 fit.models          7          3 104.211     NA      NA
```


```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "PoorCare.fctr.predict.Interact.High.cor.Y.glm.prob"    
## [2] "PoorCare.fctr.predict.Interact.High.cor.Y.glm"         
## [3] "PoorCare.fctr.predict.Interact.High.cor.Y.glm.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](D2HE_Claims2_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 104.211 108.608   4.397
## 14 fit.data.training          8          0 108.608      NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## [1] "fitting model: Final.glm"
## [1] "    indep_vars: Narcotics, OfficeVisits, Narcotics:OfficeVisits, Narcotics:Narcotics"
## Aggregating results
## Fitting final model on full training set
```

![](D2HE_Claims2_files/figure-html/fit.data.training_0-1.png) ![](D2HE_Claims2_files/figure-html/fit.data.training_0-2.png) ![](D2HE_Claims2_files/figure-html/fit.data.training_0-3.png) ![](D2HE_Claims2_files/figure-html/fit.data.training_0-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.82094  -0.64557  -0.48623   0.03981   2.18076  
## 
## Coefficients:
##                           Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              -2.865354   0.592384  -4.837 1.32e-06 ***
## Narcotics                 0.164955   0.105692   1.561  0.11859    
## OfficeVisits              0.093530   0.033775   2.769  0.00562 ** 
## `Narcotics:OfficeVisits` -0.004276   0.004585  -0.933  0.35097    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 111.888  on 98  degrees of freedom
## Residual deviance:  88.328  on 95  degrees of freedom
## AIC: 96.328
## 
## Number of Fisher Scoring iterations: 4
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](D2HE_Claims2_files/figure-html/fit.data.training_0-5.png) 

```
##    threshold    f.score
## 1        0.0 0.40322581
## 2        0.1 0.45283019
## 3        0.2 0.51612903
## 4        0.3 0.53061224
## 5        0.4 0.53658537
## 6        0.5 0.51282051
## 7        0.6 0.52631579
## 8        0.7 0.51428571
## 9        0.8 0.32258065
## 10       0.9 0.07692308
## 11       1.0 0.00000000
```

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   PoorCare.fctr PoorCare.fctr.predict.Final.glm.N
## 1             N                                69
## 2             Y                                14
##   PoorCare.fctr.predict.Final.glm.Y
## 1                                 5
## 2                                11
##          Prediction
## Reference  N  Y
##         N 69  5
##         Y 14 11
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.80808081     0.42282909     0.71663237     0.88031565     0.74747475 
## AccuracyPValue  McnemarPValue 
##     0.09915550     0.06645742
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](D2HE_Claims2_files/figure-html/fit.data.training_0-6.png) 

```
##    model_id model_method
## 1 Final.glm          glm
##                                                                  feats
## 1 Narcotics, OfficeVisits, Narcotics:OfficeVisits, Narcotics:Narcotics
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.889                 0.006
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.7805405                    0.4       0.5365854        0.7878788
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit min.aic.fit
## 1             0.7166324             0.8803156     0.3036687    96.32839
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.03030303       0.1761507
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 108.608 113.379   4.771
## 15 fit.data.training          8          1 113.379      NA      NA
```


```r
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id, : Using default probability threshold: 0.4
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                          Interact.High.cor.Y.glm.importance importance
## OfficeVisits                                      100.00000  100.00000
## Narcotics                                          34.19471   34.19471
## `Narcotics:OfficeVisits`                            0.00000    0.00000
##                          Final.glm.importance
## OfficeVisits                        100.00000
## Narcotics                            34.19471
## `Narcotics:OfficeVisits`              0.00000
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](D2HE_Claims2_files/figure-html/fit.data.training_1-1.png) ![](D2HE_Claims2_files/figure-html/fit.data.training_1-2.png) ![](D2HE_Claims2_files/figure-html/fit.data.training_1-3.png) 

```
## [1] "Min/Max Boundaries: "
##     MemberID PoorCare.fctr PoorCare.fctr.predict.Final.glm.prob
## 85        85             Y                           0.11674931
## 64        64             Y                           0.16118220
## 125      125             Y                           0.32868178
## 101      101             Y                           0.39327614
## 4          4             N                           0.25194321
## 35        35             Y                           0.79795156
## 43        43             N                           0.05389303
## 106      106             Y                           0.91844626
##     PoorCare.fctr.predict.Final.glm
## 85                                N
## 64                                N
## 125                               N
## 101                               N
## 4                                 N
## 35                                Y
## 43                                N
## 106                               Y
##     PoorCare.fctr.predict.Final.glm.accurate
## 85                                     FALSE
## 64                                     FALSE
## 125                                    FALSE
## 101                                    FALSE
## 4                                       TRUE
## 35                                      TRUE
## 43                                      TRUE
## 106                                     TRUE
##     PoorCare.fctr.predict.Final.glm.error .label
## 85                           -0.283250690     85
## 64                           -0.238817801     64
## 125                          -0.071318220    125
## 101                          -0.006723861    101
## 4                             0.000000000      4
## 35                            0.000000000     35
## 43                            0.000000000     43
## 106                           0.000000000    106
## [1] "Inaccurate: "
##     MemberID PoorCare.fctr PoorCare.fctr.predict.Final.glm.prob
## 28        28             Y                           0.09274989
## 48        48             Y                           0.10807509
## 24        24             Y                           0.11654347
## 85        85             Y                           0.11674931
## 6          6             Y                           0.14546228
## 64        64             Y                           0.16118220
## 21        21             Y                           0.18987219
## 103      103             Y                           0.18987219
## 130      130             Y                           0.18987219
## 99        99             Y                           0.22084031
## 60        60             Y                           0.25146385
## 18        18             Y                           0.28591335
## 125      125             Y                           0.32868178
## 101      101             Y                           0.39327614
## 119      119             N                           0.45765316
## 15        15             N                           0.53276830
## 59        59             N                           0.62295927
## 36        36             N                           0.64765306
## 58        58             N                           0.80946438
##     PoorCare.fctr.predict.Final.glm
## 28                                N
## 48                                N
## 24                                N
## 85                                N
## 6                                 N
## 64                                N
## 21                                N
## 103                               N
## 130                               N
## 99                                N
## 60                                N
## 18                                N
## 125                               N
## 101                               N
## 119                               Y
## 15                                Y
## 59                                Y
## 36                                Y
## 58                                Y
##     PoorCare.fctr.predict.Final.glm.accurate
## 28                                     FALSE
## 48                                     FALSE
## 24                                     FALSE
## 85                                     FALSE
## 6                                      FALSE
## 64                                     FALSE
## 21                                     FALSE
## 103                                    FALSE
## 130                                    FALSE
## 99                                     FALSE
## 60                                     FALSE
## 18                                     FALSE
## 125                                    FALSE
## 101                                    FALSE
## 119                                    FALSE
## 15                                     FALSE
## 59                                     FALSE
## 36                                     FALSE
## 58                                     FALSE
##     PoorCare.fctr.predict.Final.glm.error
## 28                           -0.307250106
## 48                           -0.291924911
## 24                           -0.283456528
## 85                           -0.283250690
## 6                            -0.254537725
## 64                           -0.238817801
## 21                           -0.210127811
## 103                          -0.210127811
## 130                          -0.210127811
## 99                           -0.179159689
## 60                           -0.148536147
## 18                           -0.114086653
## 125                          -0.071318220
## 101                          -0.006723861
## 119                           0.057653161
## 15                            0.132768298
## 59                            0.222959266
## 36                            0.247653062
## 58                            0.409464378
```

![](D2HE_Claims2_files/figure-html/fit.data.training_1-4.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

# print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])

print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "PoorCare.fctr.predict.Final.glm.prob"
## [2] "PoorCare.fctr.predict.Final.glm"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](D2HE_Claims2_files/figure-html/fit.data.training_1-5.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 113.379 116.618   3.239
## 16  predict.data.new          9          0 116.619      NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
# sav_newobs_df <- glb_newobs_df
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(glb_newobs_df, mdl_id = glb_fin_mdl_id,
## rsp_var_out = glb_rsp_var_out, : Using default probability threshold: 0.4
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](D2HE_Claims2_files/figure-html/predict.data.new-1.png) ![](D2HE_Claims2_files/figure-html/predict.data.new-2.png) ![](D2HE_Claims2_files/figure-html/predict.data.new-3.png) 

```
## [1] "Min/Max Boundaries: "
##    MemberID PoorCare.fctr PoorCare.fctr.predict.Final.glm.prob
## 41       41             Y                           0.18810096
## 10       10             N                           0.07341038
## 31       31             N                           0.06426642
## 84       84             Y                           0.88492610
## 52       52             N                           0.43868496
## 32       32             N                           0.79306781
##    PoorCare.fctr.predict.Final.glm
## 41                               N
## 10                               N
## 31                               N
## 84                               Y
## 52                               Y
## 32                               Y
##    PoorCare.fctr.predict.Final.glm.accurate
## 41                                    FALSE
## 10                                     TRUE
## 31                                     TRUE
## 84                                     TRUE
## 52                                    FALSE
## 32                                    FALSE
##    PoorCare.fctr.predict.Final.glm.error .label
## 41                           -0.21189904     41
## 10                            0.00000000     10
## 31                            0.00000000     31
## 84                            0.00000000     84
## 52                            0.03868496     52
## 32                            0.39306781     32
## [1] "Inaccurate: "
##    MemberID PoorCare.fctr PoorCare.fctr.predict.Final.glm.prob
## 9         9             Y                            0.1142866
## 41       41             Y                            0.1881010
## 30       30             Y                            0.3892902
## 52       52             N                            0.4386850
## 32       32             N                            0.7930678
##    PoorCare.fctr.predict.Final.glm
## 9                                N
## 41                               N
## 30                               N
## 52                               Y
## 32                               Y
##    PoorCare.fctr.predict.Final.glm.accurate
## 9                                     FALSE
## 41                                    FALSE
## 30                                    FALSE
## 52                                    FALSE
## 32                                    FALSE
##    PoorCare.fctr.predict.Final.glm.error
## 9                            -0.28571341
## 41                           -0.21189904
## 30                           -0.01070981
## 52                            0.03868496
## 32                            0.39306781
```

![](D2HE_Claims2_files/figure-html/predict.data.new-4.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
#     submit_df <- glb_newobs_df[, c(paste0(glb_rsp_var_out, glb_fin_mdl_id)), FALSE]
#     names(submit_df)[1] <- "BDscience"
#     submit_df$BDscience <- as.numeric(submit_df$BDscience) - 1
#     #submit_df <-rbind(submit_df, data.frame(bdanalytics=c(" ")))
#     print("Submission Stats:")
#     print(table(submit_df$BDscience, useNA = "ifany"))
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
submit_fname <- paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
                    "_submit.csv")
write.csv(submit_df, submit_fname, quote=FALSE, row.names=FALSE)
#cat(" ", "\n", file=submit_fn, append=TRUE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
```

```
## [1] 0.4
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: Interact.High.cor.Y.glm"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.glm"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 99 17
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 7    Interact.High.cor.Y.glm          0.84375   0.7994792     0.5652174
## 10            All.X.bayesglm          0.81250   0.8541667     0.5000000
## 6              Max.cor.Y.glm          0.78125   0.7994792     0.4814815
## 4  Max.cor.Y.cv.0.cp.0.rpart          0.75000   0.7838542     0.3846154
## 1          MFO.myMFO_classfr          0.75000   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart          0.75000   0.5000000     0.0000000
## 5            Max.cor.Y.rpart          0.75000   0.5000000     0.0000000
## 11      All.X.no.rnorm.rpart          0.75000   0.5000000     0.0000000
## 9                  All.X.glm          0.71875   0.8541667     0.4545455
## 12         All.X.no.rnorm.rf          0.71875   0.7682292     0.3793103
## 8              Low.cor.X.glm          0.65625   0.8072917     0.3714286
## 2    Random.myrandom_classfr          0.25000   0.5208333     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 7     96.32839                    0.4
## 10    99.22172                    0.4
## 6     95.12656                    0.3
## 4           NA                    0.5
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 11          NA                    0.5
## 9     95.91667                    0.2
## 12          NA                    0.3
## 8    101.30927                    0.2
## 2           NA                    0.2
```

```r
if (glb_is_regression) {
    print(sprintf("%s OOB RMSE: %0.4f", glb_sel_mdl_id,
                  glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "min.RMSE.OOB"]))

    if (!is.null(glb_category_vars)) {
        stop("not implemented yet")
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
            pred_stats_df <- 
                mypredict_mdl(mdl=glb_models_lst[[glb_fin_mdl_id]], 
                              df=glb_newobs_df, 
                              rsp_var=glb_rsp_var, 
                              rsp_var_out=glb_rsp_var_out, 
                              model_id_method=glb_fin_mdl_id, 
                              label="new",
						      model_summaryFunction=glb_sel_mdl$control$summaryFunction, 
						      model_metric=glb_sel_mdl$metric,
						      model_metric_maximize=glb_sel_mdl$maximize,
						      ret_type="stats")        
            print(sprintf("%s prediction stats for glb_newobs_df:", glb_fin_mdl_id))
            print(pred_stats_df)
    }    
}    
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    if (!is.null(glb_category_vars)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
        print(sprintf("%s new confusion matrix & accuracy: ", glb_fin_mdl_id))
        print(t(confusionMatrix(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)], 
                                glb_newobs_df[, glb_rsp_var])$table))
    }    

}    
```

```
## [1] "Interact.High.cor.Y.glm OOB confusion matrix & accuracy: "
##          Prediction
## Reference  N  Y
##         N 22  2
##         Y  3  5
## [1] "Final.glm new confusion matrix & accuracy: "
##          Prediction
## Reference  N  Y
##         N 22  2
##         Y  3  5
```

```r
dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_var]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

# if (glb_is_classification) {
#     print("FN_OOB_ids:")
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         glb_txt_vars])
#     print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
#                                 union(myfind_chr_cols_df(glb_OOBobs_df),
#                     grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
# }

dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##                          Interact.High.cor.Y.glm.importance importance
## OfficeVisits                                      100.00000  100.00000
## Narcotics                                          34.19471   34.19471
## `Narcotics:OfficeVisits`                            0.00000    0.00000
##                          Final.glm.importance
## OfficeVisits                        100.00000
## Narcotics                            34.19471
## `Narcotics:OfficeVisits`              0.00000
```

```r
# players_df <- data.frame(id=c("Chavez", "Giambi", "Menechino", "Myers", "Pena"),
#                          OBP=c(0.338, 0.391, 0.369, 0.313, 0.361),
#                          SLG=c(0.540, 0.450, 0.374, 0.447, 0.500),
#                         cost=c(1400000, 1065000, 295000, 800000, 300000))
# players_df$RS.predict <- predict(glb_models_lst[[csm_mdl_id]], players_df)
# print(orderBy(~ -RS.predict, players_df))

if (length(diff <- setdiff(names(glb_trnobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

if (length(diff <- setdiff(names(glb_fitobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
if (length(diff <- setdiff(names(glb_OOBobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
if (length(diff <- setdiff(names(glb_newobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)
```

```
## Warning in rm(submit_df, tmp_OOBobs_df): object 'tmp_OOBobs_df' not found
```

```r
# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor     bgn     end elapsed
## 16     predict.data.new          9          0 116.619 119.181   2.562
## 17 display.session.info         10          0 119.181      NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor     bgn     end elapsed
## 11              fit.models          7          1  44.368  82.984  38.616
## 10              fit.models          7          0  22.589  44.367  21.779
## 12              fit.models          7          2  82.984 104.210  21.227
## 2             inspect.data          2          0  10.008  17.863   7.855
## 14       fit.data.training          8          0 108.608 113.379   4.771
## 13              fit.models          7          3 104.211 108.608   4.397
## 15       fit.data.training          8          1 113.379 116.618   3.239
## 16        predict.data.new          9          0 116.619 119.181   2.562
## 3               scrub.data          2          1  17.864  19.508   1.645
## 5         extract.features          3          0  19.578  21.169   1.591
## 8          select.features          5          0  21.603  22.245   0.643
## 1              import.data          1          0   9.534  10.007   0.473
## 6             cluster.data          4          0  21.169  21.540   0.371
## 9  partition.data.training          6          0  22.246  22.589   0.343
## 4           transform.data          2          2  19.509  19.578   0.069
## 7      manage.missing.data          4          1  21.540  21.602   0.062
##    duration
## 11   38.616
## 10   21.778
## 12   21.226
## 2     7.855
## 14    4.771
## 13    4.397
## 15    3.239
## 16    2.562
## 3     1.644
## 5     1.591
## 8     0.642
## 1     0.473
## 6     0.371
## 9     0.343
## 4     0.069
## 7     0.062
## [1] "Total Elapsed Time: 119.181 secs"
```

![](D2HE_Claims2_files/figure-html/display.session.info-1.png) 

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
##  [1] tcltk     grid      parallel  stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
##  [1] gdata_2.16.1        randomForest_4.6-10 arm_1.8-5          
##  [4] lme4_1.1-8          Matrix_1.2-1        MASS_7.3-41        
##  [7] rpart.plot_1.5.2    rpart_4.1-9         ROCR_1.0-7         
## [10] gplots_2.17.0       dplyr_0.4.2         plyr_1.8.3         
## [13] sqldf_0.4-10        RSQLite_1.0.0       DBI_0.3.1          
## [16] gsubfn_0.6-6        proto_0.3-10        reshape2_1.4.1     
## [19] caTools_1.17.1      doMC_1.3.3          iterators_1.0.7    
## [22] foreach_1.4.2       doBy_4.5-13         survival_2.38-2    
## [25] caret_6.0-47        ggplot2_1.0.1       lattice_0.20-31    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.6         class_7.3-12        gtools_3.5.0       
##  [4] assertthat_0.1      digest_0.6.8        R6_2.0.1           
##  [7] BradleyTerry2_1.0-6 chron_2.3-47        coda_0.17-1        
## [10] evaluate_0.7        e1071_1.6-4         lazyeval_0.1.10    
## [13] minqa_1.2.4         SparseM_1.6         car_2.0-25         
## [16] nloptr_1.0.4        rmarkdown_0.7       labeling_0.3       
## [19] splines_3.2.0       stringr_1.0.0       munsell_0.4.2      
## [22] compiler_3.2.0      mgcv_1.8-6          htmltools_0.2.6    
## [25] nnet_7.3-9          codetools_0.2-11    brglm_0.5-9        
## [28] bitops_1.0-6        nlme_3.1-120        gtable_0.1.2       
## [31] magrittr_1.5        formatR_1.2         scales_0.2.5       
## [34] KernSmooth_2.23-14  stringi_0.5-2       RColorBrewer_1.1-2 
## [37] tools_3.2.0         abind_1.4-3         pbkrtest_0.4-2     
## [40] yaml_2.1.13         colorspace_1.2-6    knitr_1.10.5       
## [43] quantreg_5.11
```
