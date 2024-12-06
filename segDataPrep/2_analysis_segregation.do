
************************************************************
*** UK school segretation: data analysis ***
************************************************************

do 0_uk_master.do
cd  "$tmp"

********************************************I*******
**** Calculate H-index for 3 racial groups ***
********************************************I*******

*** Generate H indices for primary schools (by region/LA/district/ward) ***
use "$posted/ukdata_full.dta", clear
codebook regioncode newlacode districtcode wardcode // 9, 160, 325, 9413
keep if type==5 // "State-funded primary"
seg white black asian, h by(year regioncode) 	file(H_region_prim) replace 
seg white black asian, h by(year newlacode) 	file(H_la_prim) replace 
seg white black asian, h by(year districtcode)  file(H_district_prim) replace 
seg white black asian, h by(year wardcode)  	file(H_ward_prim) replace 

use "$tmp/H_district_prim.dta", clear
egen p=sum(Hseg*Total)
egen T=total(Total) // Total N students: 4.1 million
gen meanHw = p/T // population weighted mean N across districts: .17

*** Generate H indices for primary schools (by region/LA/district/ward) ***
use "$posted/ukdata_full.dta", clear
sort district
keep if type==6 // "State-funded secondary"
seg white black asian, h by(year regioncode) 	file(H_region_sec) replace 
seg white black asian, h by(year newlacode) 	file(H_la_sec) replace 
seg white black asian, h by(year districtcode)  file(H_district_sec) replace 
seg white black asian, h by(year wardcode)  	file(H_ward_sec) replace 

use "$tmp/H_district_sec.dta", clear
egen p=sum(Hseg*Total)
egen T=total(Total) // Total N students: 3.2 million
gen meanHw = p/T // population weighted mean N across districts: .11


*************************************************************************
**** Calculate D-index by FSM status ***
*************************************************************************

// https://ffteducationdatalab.org.uk/2021/10/how-free-school-meal-eligibility-has-been-changing-and-why-we-might-need-new-measures-of-disadvantage/

use "$posted/ukdata_full.dta", clear
keep if type==5 // "State-funded primary"
seg fsm_eligible fsm_non_eligible, d by(year regioncode) 	file(D_fsm_region_prim) replace 
seg fsm_eligible fsm_non_eligible, d by(year newlacode) 	file(D_fsm_la_prim) replace 
seg fsm_eligible fsm_non_eligible, d by(year districtcode)  file(D_fsm_district_prim) replace 
seg fsm_eligible fsm_non_eligible, d by(year wardcode)  	file(D_fsm_ward_prim) replace 

use "$posted/ukdata_full.dta", clear
keep if type==6 // "State-funded secondary"
seg fsm_eligible fsm_non_eligible, d by(year regioncode) 	file(D_fsm_region_sec) replace 
seg fsm_eligible fsm_non_eligible, d by(year newlacode) 	file(D_fsm_la_sec) replace 
seg fsm_eligible fsm_non_eligible, d by(year districtcode)  file(D_fsm_district_sec) replace 
seg fsm_eligible fsm_non_eligible, d by(year wardcode)  	file(D_fsm_ward_sec) replace 

*************************************************************************
*** Create single datasets at the region, LA, district, and ward levels ***
*************************************************************************

* Region
use "$tmp/H_region_prim.dta", clear
rename nunits nunits_prim
rename Hseg Hseg_prim_3race
drop Total
merge 1:1 regioncode year using "$tmp/H_region_sec.dta", nogen
rename nunits nunits_sec
rename Hseg Hseg_sec_3race
drop Total
merge 1:1 regioncode year using "$tmp/D_fsm_region_prim.dta", nogen
rename Total tot_prim
rename Dseg Dseg_prim_fsm
merge 1:1 regioncode year using "$tmp/D_fsm_region_sec.dta", nogen
rename Total tot_sec
rename Dseg Dseg_sec_fsm
drop Ediv Idiv 
save "$posted/seg_indices_region.dta", replace

* Local authorities
use "$tmp/H_la_prim.dta", clear
rename nunits nunits_prim
rename Hseg Hseg_prim_3race
drop Total
merge 1:1 newlacode year using "$tmp/H_la_sec.dta", nogen
rename nunits nunits_sec
rename Hseg Hseg_sec_3race
drop Total
merge 1:1 newlacode year using "$tmp/D_fsm_la_prim.dta", nogen
rename Total tot_prim
rename Dseg Dseg_prim_fsm
merge 1:1 newlacode year using "$tmp/D_fsm_la_sec.dta", nogen
rename Total tot_sec
rename Dseg Dseg_sec_fsm
drop Ediv Idiv 
save "$posted/seg_indices_la.dta", replace // v small number not matched

* District
use "$tmp/H_district_prim.dta", clear
rename nunits nunits_prim
rename Hseg Hseg_prim_3race
drop Total
merge 1:1 districtcode year using "$tmp/H_district_sec.dta", nogen
rename nunits nunits_sec
rename Hseg Hseg_sec_3race
drop Total
merge 1:1 districtcode year using "$tmp/D_fsm_district_prim.dta", nogen
rename Total tot_prim
rename Dseg Dseg_prim_fsm
merge 1:1 districtcode year using "$tmp/D_fsm_district_sec.dta", nogen
rename Total tot_sec
rename Dseg Dseg_sec_fsm
drop Ediv Idiv 
replace Hseg_prim_3race=. if nunits_prim==1 // Only one school in the district - cannot calculate segregation
replace Hseg_sec_3race =. if nunits_sec==1 
replace Dseg_prim_fsm  =. if nunits_prim==1 
replace Dseg_sec_fsm   =. if nunits_sec==1 
drop if districtcode=="Unknown"
clonevar LAD23CD = districtcode 
merge m:1 LAD23CD using "$mapdata/district_names.dta", keep(3) nogen // some old districts not matched, this is map for 2023-24
save "$posted/seg_indices_district.dta", replace 

* Ward
use "$tmp/H_ward_prim.dta", clear
rename nunits nunits_prim
rename Hseg Hseg_prim_3race
drop Total
merge 1:1 wardcode year using "$tmp/H_ward_sec.dta", nogen // many not matched, maybe ward that do not have sec schools?
rename nunits nunits_sec
rename Hseg Hseg_sec_3race
drop Total
merge 1:1 wardcode year using "$tmp/D_fsm_ward_prim.dta", nogen
rename Total tot_prim
rename Dseg Dseg_prim_fsm
merge 1:1 wardcode year using "$tmp/D_fsm_ward_sec.dta", nogen
rename Total tot_sec
rename Dseg Dseg_sec_fsm
drop Ediv Idiv 
save "$posted/seg_indices_ward.dta", replace 



************************************************************
*** Mapping school segretation: by district ***
************************************************************

* Create map for primary school racial segregation by district
use "$posted/seg_indices_district.dta", clear
keep if year=="2023_24"
spmap Hseg_prim_3race using "$mapdata/district_coordinates.dta", id(_ID) title("Primary H (3 racial groups)", size(medium))  ///
	fcolor(Reds2) ocolor(white ..) osize(0.1 ..) clmethod(custom) clbreaks(0 .05 .1 .15 .2 .3 1) ndocolor(black) ndfcolor(white)  /// 
	legend(order(1 "No data" 2 "0-.05" 3 ".05-.1" 4 ".1-.15" 5 ".15-.2" 6 ".2-.3" 7 ">.3") ring(0) bplacement(10) size(*1.4)) ///
	name(primary_race, replace) plotregion(margin(0 0 0 0)) norescaling
graph export "$figures/primary_map_H_districts.png", replace width(4000)

* Create map for secondary school racial segregation by district
spmap Hseg_sec_3race using "$mapdata/district_coordinates.dta", id(_ID) title("Secondary H (3 racial groups)", size(medium))  ///
	fcolor(Reds2) ocolor(white ..) osize(0.1 ..) clmethod(custom) clbreaks(0 .05 .1 .15 .2 .3 1) ndocolor(black) ndfcolor(white)  /// 
	legend(order(1 "No data" 2 "0-.05" 3 ".05-.1" 4 ".1-.15" 5 ".15-.2" 6 ".2-.3" 7 ">.3") ring(0) bplacement(10) size(*1.4)) ///
	name(sec_race, replace) plotregion(margin(0 0 0 0)) norescaling
graph export "$figures/secondary_map_H_districts.png", replace width(4000)

* Create map for primary school FSM segregation by district
spmap Dseg_prim_fsm using "$mapdata/district_coordinates.dta", id(_ID) title("Primary D-index (FSM)", size(medium))  ///
	fcolor(Reds2) ocolor(white ..) osize(0.1 ..) clmethod(custom) clbreaks(0 .1 .2 .3 .4 1) ndocolor(black) ndfcolor(white)  /// 
	legend(order(1 "No data" 2 "0-.1" 3 ".1-.2" 4 ".2-.3" 5 ".3-.4" 6 ">.4") ring(0) bplacement(10) size(*1.4)) ///
	name(prim_fsm, replace) plotregion(margin(0 0 0 0)) norescaling
graph export "$figures/prim_D_fsm_districts.png", replace width(4000)

* Create map for secondary school FSM segregation by district
spmap Dseg_sec_fsm using "$mapdata/district_coordinates.dta", id(_ID) title("Secondary D-index (FSM)", size(medium))  ///
	fcolor(Reds2) ocolor(white ..) osize(0.1 ..) clmethod(custom) clbreaks(0 .1 .2 .3 .4 1) ndocolor(black) ndfcolor(white)  /// 
	legend(order(1 "No data" 2 "0-.1" 3 ".1-.2" 4 ".2-.3" 5 ".3-.4" 6 ">.4") ring(0) bplacement(10) size(*1.4)) ///
	name(sec_fsm, replace) plotregion(margin(0 0 0 0)) norescaling
graph export "$figures/sec_D_fsm_districts.png", replace width(4000)


