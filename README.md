# InjurySeverityScore
Translate ICD-9 to Injury Severity Score. This is rewritten from STATA into R package based on ICDPIC. The reference was seen here:
https://ideas.repec.org/c/boc/bocode/s457028.html. The dictionary of mapping ICD9 to body region and severity score were also downloaded from [ICDPIC](https://ideas.repec.org/c/boc/bocode/s457028.html).

## Input Dataset
Function `injury_score` requires the input dataset has at least two variables. One is patient id and the other is the icd9 code. The icd9 code must be **character**. The icd9 code could be with or without dot and is indicated through `has_dot` parameter.
## Output
Output dataset will use the patient id as the primary key and contains variable `iss` which is the injury severity score. For the definition of injury severity score, go to https://en.wikipedia.org/wiki/Injury_Severity_Score. Other variables `br1-br6` are the maximum score for each body region. `9` indicated unknown score and `0` indicated no valid injury code under that body region. And max1-max3 are the top 3 scores out of all.

A score of **99** for `iss` indicates that valid injury ICD-9 exists but none had known injury score.
## Example 1 (ICD code with dot) 
1. If you don't have the package, install through one of the following:
* ~~`install.packages('InjurySeverityScore')`~~
* `install.packages("devtools"); devtools::install_github("dajuntian/InjurySeverityScore")`
2. Generate sample dataset  
``` R
pat_id <- c(2,2,2,2,2,1,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
icd9 <- c('874.2', '874.8', '900.81', '900.82', '900.89', '805.06', 
          'E966', '805.07', 'V14.0', '807.02', 'V70.4', '821.01', '823.20', 
          '860.0', '861.01', '861.21', '861.22', '863.84', '864.04', '865.04', 
          '865.09', '866.02', '868.04', '958.4')
sample_data <- data.frame(subj = pat_id, code = icd9, stringsAsFactors = FALSE)
````
3. Load the package and calculate injury severity score.
```R
library(InjurySeverityScore)
?InjurySeverityScore::injury_score #see help
injury_score(sample_data, subj, code, has_dot = T)
```
## Example 2 (ICD code without dot)
```R
pat_id <- c(2,2,2,2,2,1,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1)
icd9 <- c('8742', '8748', '90081', '90082', '90089', '80506', 
          'E966', '80507', 'V140', '80702', 'V704', '82101', '82320', 
          '8600', '86101', '86121', '86122', '86384', '86404', '86504', 
          '86509', '86602', '86804', '9584')
sample_data <- data.frame(subj = pat_id, code = icd9, stringsAsFactors = FALSE)
injury_score(sample_data, subj, code, has_dot = F)
```
## Both examples would have the following output:
```R
#  subj br_1 br_2 br_3 br_4 br_5 br_6 max_1 max_2 max_3 iss
#1    2    3    0    0    0    0    1     3     1     0  10
#2    1    2    0    3    5    3    0     5     3     3  43
```          
## Example 3 (Data in wide form instead of tall form)
```R
data2 <- data.frame(pid = c(1,2), diag1 = c('900.89', '805.06'),
                    diag2 = c('863.84', '865.04'),stringsAsFactors = FALSE)
#  pid  diag1  diag2
#1   1 900.89 863.84
#2   2 805.06 865.04                
injury_score(data2, pid, diag, tall = FALSE)
```
