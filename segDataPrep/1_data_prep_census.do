
************************************************************
*** UK school segretation: data prep ***
************************************************************

do 0_uk_master.do


************************************************************
*** Import pupil characteristics data into Stata ***
************************************************************

/* Check full dataset
insheet using "$ukdata/2023_24\supporting-files\spc_school_level_underlying_data.csv", clear
des headcountof *fsm* *eligible*, full
br urn headcountof *fsm* *eligible* fulltimepupils parttimepupils
// Part-time pupils are not included in "number of pupils used for FSM calculation"
fre v263 // % of pupils whose first language is known or believed to be other than English - could be relevant
*/

local f2023_24 "2023_24\supporting-files\spc_school_level_underlying_data.csv"
local f2022_23 "2022_23\supporting-files\spc_school_level_underlying_data_23112023.csv"
local f2021_22 "2021_22\supporting-files\spc_school_level_underlying_data_20230302.csv"
local f2020_21 "2020_21\supporting-files\spc_school_level_underlying_data_220216.csv"
local f2019_20 "2019_20\supporting-files\spc_school_level_underlying_data.csv" // harmonized up to here
local f2018_19 "2018_19\Schools_Pupils_and_their_Characteristics_2019_pupil_characteristics_UD.csv"
local f2017_18 "2017_18\Schools_Pupils_and_their_Characteristics_2018_Schools_Pupils_UD.csv"
local f2016_17 "2016_17\SFR28_2017_Schools_Pupils_UD.csv"
local f2015_16 "2015_16\SFR20_2016_Schools_Pupils_UD.csv"
local f2014_15 "2014_15\SFR16_2015_Schools_Pupils_UD.csv"
local f2013_14 "2013_14\SFR15_2014_school_level_pupils_UD.csv"
local f2012_13 "2012_13\School_level_schools_pupil_2013.csv"
local f2011_12 "2011_12\School_level_schools_pupils_2012.csv"
local f2010_11 "2010_11\School_level_schools_pupils_2011.csv"
local f2009_10 "2009_10\school_level_census.csv"
local f2008_09 "2002_09\schoolfinal09#.csv" // NB: 2002-2009 data has quite a few 'suppressed' racial data points to prevent disclosure - need to look into
local f2007_08 "2002_09\schoolfinal08#.csv"
local f2006_07 "2002_09\schoolfinal07#.csv"
local f2005_06 "2002_09\schoolfinal06#.csv"
local f2004_05 "2002_09\schoolfinal05#.csv"
local f2003_04 "2002_09\schoolfinal04#.csv"
local f2002_03 "2002_09\schoolfinal03#.csv"
local f2001_02 "2002_09\schoolfinal02#.csv"
/*
foreach v in 2001_02 2002_03 2003_04 2004_05 {
		import delim using "$ukdata/`f`v''", varnames(5) rowrange(6) case(lower) clear
		// This data has pupil numbers by race, but not other pupils characteristics - need to import from other file
		save "$tmp/schooldata_raw_`v'.dta", replace
}
foreach v in 2005_06 {
		import delim using "$ukdata/`f`v''", varnames(2) rowrange(3) case(lower) clear
		save "$tmp/schooldata_raw_`v'.dta", replace
}
foreach v in 2006_07   {
		import delim using "$ukdata/`f`v''", varnames(3) rowrange(4) case(lower) clear
		save "$tmp/schooldata_raw_`v'.dta", replace
}
foreach v in 2007_08 2008_09  {
		import delim using "$ukdata/`f`v''", varnames(6) rowrange(7) case(lower) clear
		save "$tmp/schooldata_raw_`v'.dta", replace
}
foreach v in 2009_10 2010_11 2011_12 2012_13 2013_14 2014_15 2015_16 2016_17 2017_18 2018_19 2019_20 2020_21 2021_22 2022_23 2023_24 {
		insheet using "$ukdata/`f`v''", clear
		save "$tmp/schooldata_raw_`v'.dta", replace
}
*/

use "$tmp/schooldata_raw_2012_13.dta", clear


************************************************************
*** Clean and harmonize data (last 5 years only) ***
************************************************************
 
foreach v in 2019_20 2020_21 2021_22 2022_23 2023_24 {
		use "$tmp/schooldata_raw_`v'.dta", clear
		di `v'
		keep region* urn la* old* new* school* phase* type* denom* district* ward* ///
		headcountofpupils numberoffsmeligible* numberofpupilsknowntobeeligiblef* ///
		numberofpupilsusedforfsmcalculat numberofpupilsclassified* urban* academy* estab urn 
		capture rename *_* ** // ensure consistent naming of variables across waves: remove dash
		capture rename *_* ** // remove second dash
		capture rename regionname region
		rename numberofpupilsclassifiedas* *
		rename numberofpupilsknowntobeeligiblef fsm_eligible
		rename numberoffsmeligiblepupilstakinga fsm_taking 
		rename numberofpupilsusedforfsmcalculat fsm_total
		foreach x in whiteb irishe travel anyoth gypsyr whitea indian pakist bangla caribb africa chines ///
		fsm_eligible fsm_taking fsm_total {
			capture replace `x'="." if `x'=="z"
		}
		destring whiteb irishe travel anyoth gypsyr whitea indian pakist bangla caribb africa chines fsm*, replace 
		gen year = "`v'"
	save "$tmp/schooldata`v'.dta", replace
}
use "$tmp/schooldata2023_24.dta", clear
foreach v in 2019_20 2020_21 2021_22 2022_23 {
	append using "$tmp/schooldata`v'.dta"
}
sort year
des, full

* School name and ID
lab var urn "School Unique Reference Number"  // consistent across years
lab var schoolname "School name" // some schools change name (as well as location)

* Geographic units
fre 	urbanrural
replace region = "Yorkshire and the Humber" if region=="Yorkshire and The Humber"

replace districtadministrativename="." if districtadministrativename=="Unknown"
encode  districtadministrativename, gen(district)
recode  district (1=.) // unknown
tab 	district year, mi // mostly consistent, but some districts appear or disappear across years
drop 	districtadministrativename
rename  districtadministrativecode districtcode

fre laname, all
fre newlacode // none missing
replace laname="Bristol, City of" if laname=="Bristol City of"
replace laname="Bournemouth, Christchurch and Poole" if laname=="Bournemouth, Christchurch and Poole Council"
replace laname="Herefordshire" if laname=="Herefordshire, County of"
replace laname="Isles of Scilly" if laname=="Isles Of Scilly"
replace laname="Kingston upon Hull, City of" if laname=="Kingston upon Hull City of"
replace laname="" if laname==""
replace laname="" if laname==""

* Phase / type
fre 	academyflag
replace phasetypegrouping = "Pupil referral unit" if phasetypegrouping=="State-funded AP school"
encode  phasetypegrouping, gen(type)
drop 	phasetypegrouping

* 2020-21 had a separate category for Arabic students: assign to other White
egen 	tmp = rowtotal(anyoth arabet) if year=="2020_21"
replace anyoth = tmp if year=="2020_21"
drop 	arabet

* Private schools have no info on race and FSM: assign to missing
foreach x in whiteb irishe travel anyoth gypsyr whitea indian pakist bangla caribb africa chines ///
			 fsm_eligible fsm_taking fsm_total {
		replace `x' = . if type==1 // Independent school
}

* Aggregate racial categories
egen asian = rowtotal(indian pakist bangla chines)
egen black = rowtotal(caribb africa whitea)
egen white = rowtotal(whiteb irishe travel anyoth gypsyr)
egen totrace = rowtotal(asian black white)
// br totrace headcount fsm_total // Sum of racial categories<headcount: presumably some pupils have missing race? - need to look into

* Race percentages
foreach v in asian black white {
	gen perc_`v' = `v'/totrace*100
	lab var perc_`v' "Percentage of `v' pupils"
	format perc* %10.1f	
}

* Free school meals variable
gen fsm_non_eligible = headcount - fsm_eligible
gen perc_fsm_elig 	 = fsm_eligible/headcount*100 // use headcount as the baseline for %, same as in the original data
gen fsm_non_taking   = headcount - fsm_taking
gen perc_fsm_taking	 = fsm_taking/headcount*100
br headcount *fsm*
// Need to investigate what is causing discrepancy between fsm_total and headcount, and whether my percentages correspond to the percentages in the raw data
lab var perc_fsm_elig   "Percentage of pupils eligible for free school meals in the last 6 years"
lab var perc_fsm_taking "Percentage of pupils currently taking free school meals"

// need to look up what fsm_total, fsm_elible and fsm_taking actually means

order region regioncode district districtcode wardname wardcode type schoolname year denom headcount fsm*
sort regioncode districtcode wardcode type schoolname year
save "$posted/ukdata_full.dta", replace


*** Export Bristol data ***
/*
keep if laname=="Bristol, City of" // 
save "$posted/bristoldata.dta", replace
export excel using "$posted/bristoldata.xlsx", replace firstrow(varlabels)
*/

********************************************I*******
**** Create long dataset (race) (for export to R) ***
********************************************I*******

use "$posted/ukdata_full.dta", clear

* Exclude independent schools (no data), nurseries, special schools, referral unit
keep if inlist(type, 5, 6) // State-funded primary / State-funded secondary
keep white asian black totrace headcount perc* schoolname type region* district* ward* year

* Create long file 
rename white n1
rename asian n2
rename black n3
gen tmpid=_n
reshape long n, i(tmpid) j(race)
lab def race 1 "White" 2 "Asian" 3 "Black", replace
lab val race race 
sort schoolname year

preserve
keep if type==5 // "State-funded primary"
save "$posted/primary_long.dta", replace
restore

preserve
keep if type==6 // "State-funded secondary"
save "$posted/secondary_long.dta", replace
restore



********************************************I*******
**** Create long dataset (FSM) (for export to R) ***
********************************************I*******

use "$posted/ukdata_full.dta", clear
keep if inlist(type, 5, 6) // State-funded primary / State-funded secondary
keep fsm* headcount perc* schoolname type region* district* ward* year //

gen n1 = fsm_eligible
gen n2 = fsm_non_eligible
drop fsm_* 
gen tmpid=_n

reshape long n, i(tmpid) j(fsm)
lab def fsm 1 "FSM eligible" 2 "Not FSM eligible", replace
lab val fsm fsm
drop  tmpid
sort  schoolname year fsm
order schoolname year fsm n

preserve
keep if type==5 // "State-funded primary"
save "$posted/fsm_primary_long.dta", replace
restore

preserve
keep if type==6 // "State-funded secondary"
save "$posted/fsm_secondary_long.dta", replace
restore


