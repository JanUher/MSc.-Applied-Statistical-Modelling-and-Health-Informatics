// Candidate number: AG39349

cap log close

use "C:\Users\xxx\KCL\Introduction to Statistical Modelling\Coursework\original_pollution_data", clear

// Dataset consists of 10 variables (11th only for Greenwich)
// A: PID = ID (1 - 1834)
// B: Borough (32, Greenwich has an exceptional 11th variable)
// C: Age
// D: Socioeconomic status (1 - best, 5 - worst)
// E: Employed (either office or manual - binary)
// F: Gender (M/F - binary)
// G: Excercise: How many times a week do they excericise
// H: Smoker? (Non-smoker, smoker - binary)
// I: Pollution concentration
// J: Depression score (0 - No symptoms, 27 - All symptoms)
// K: Has received diagnosis of depressive disorder? (Y/N, only for Greenwich)


* Encode original string variables into new numeric categorical versions
encode borough, gen(borough_cat)
encode employment, gen(employment_cat)
encode gender, gen(gender_cat)
encode smoker, gen(smoker_cat)

* Drop the original string variables
drop borough employment gender smoker

* Rename encoded variables back to original names
rename borough_cat borough
rename employment_cat employment
rename gender_cat gender
rename smoker_cat smoker


*****************************************************
* Convert "NA" string values to genuine missing
*****************************************************
foreach var of varlist ses exercise pm25 phq9 {
    replace `var' = "" if `var' == "NA"
}

*****************************************************
* Tag individuals with ANY missing in selected vars
*****************************************************
egen anymiss = rowmiss(ses exercise pm25 phq9)

*****************************************************
* Save dataset with missing values
*****************************************************
preserve
keep if anymiss > 0
save "C:\Users\xxx\KCL\Introduction to Statistical Modelling\Coursework\dataset_missing", replace
restore

*****************************************************
* Save dataset with complete cases
*****************************************************
preserve
keep if anymiss == 0
save "C:\Users\xxx\KCL\Introduction to Statistical Modelling\Coursework\dataset_complete", replace
restore





