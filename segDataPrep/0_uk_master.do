

************************************************************
*** UK school segregation: master ***
************************************************************

*** Set directories ***
clear
global workingDir "C:\Users\nt23395\Dropbox\11d_segregation_UK" // Change to different directory if needed

global posted	  "${workingDir}\1_posted"
global figures	  "${workingDir}\2_figures"
global tables	  "${workingDir}\3_tables"
global mapdata    "${workingDir}\4_mapdata"
global ukdata	  "${workingDir}\5_DfE data\Schools Pupils and their Characteristics"
global tmp        "${workingDir}\99_tmp"



***************************************************************************
*** Download & unzip "Schools, Pupils and their Characteristics" data 
*** from academic year 2019-20 - 2023-24 (only need to do this once)
***************************************************************************

/* Download data from the DfE website for each academic year (last 5 years available)
// Source: https://www.gov.uk/government/collections/statistics-school-and-pupil-numbers
local d2023_24 https://content.explore-education-statistics.service.gov.uk/api/releases/6a45c262-aaca-4bda-a548-cc9d1dc63137/files
local d2022_23 https://content.explore-education-statistics.service.gov.uk/api/releases/5597d8c4-ae2c-43cd-ba21-c6cbe80c5e4a/files
local d2021_22 https://content.explore-education-statistics.service.gov.uk/api/releases/b71871a9-3207-4e83-8c5f-15eefdd2f458/files
local d2020_21 https://content.explore-education-statistics.service.gov.uk/api/releases/18afd52c-ef75-406c-993e-e6459781dbcc/files
local d2019_20 https://content.explore-education-statistics.service.gov.uk/api/releases/759ddb6d-b508-4f92-8c64-c517c473d75f/files
local d2018_19 https://assets.publishing.service.gov.uk/media/5d5bcd81ed915d08d612d802/Schools_Pupils_and_their_Characteristics_2019_Underlying_Data.zip
local d2017_18 https://assets.publishing.service.gov.uk/media/5bb73f48ed915d23980938e9/schools_pupils_and_their_characteristics_2018_underlying_data.zip
local d2016_17 https://assets.publishing.service.gov.uk/media/5a824241ed915d74e340292a/SFR28_2017_Underlying_Data.zip
local d2015_16 https://assets.publishing.service.gov.uk/media/5a7f3f4ced915d74e62294a7/SFR20_2016_Underlying_Data.zip
local d2014_15 https://assets.publishing.service.gov.uk/media/5a7f22f6ed915d74e62289c9/SFR16_2015_Underlying_Data.zip
local d2013_14 https://assets.publishing.service.gov.uk/media/5a80260de5274a2e8ab4e77a/SFR15_2014_Underlying_data_v102.zip
local d2012_13 https://assets.publishing.service.gov.uk/media/5a7c1750e5274a1f5cc75bb1/SFR21-2013_UD.zip
local d2011_12 https://assets.publishing.service.gov.uk/media/5a7a100ee5274a319e777952/sfr10-2012ud.zip
local d2010_11 https://assets.publishing.service.gov.uk/media/5a7af937ed915d71db8b3d79/sfr12-2011udv4.zip
local d2009_10 https://assets.publishing.service.gov.uk/media/5a7a2d68e5274a319e77865a/underlying_20data_20sfr092010.zip
local d2002_09 https://assets.publishing.service.gov.uk/media/5b6d50dce5274a29795b48f3/SPC_UD_files_2002_to_2009.zip // NB: From 2002 to 2009! 


cd "$ukdata"
foreach v in 2002_09 2009_10 2010_11 2011_12 2012_13 2013_14 2014_15 2015_16 2016_17 2017_18 2018_19 2019_20 2020_21 2021_22 2022_23 2023_24 {
	mkdir `v'
	cd "$ukdata/`v'"
	copy `d`v'' `v'.zip
	unzipfile `v'
	cd "$ukdata"
}

***************************************************************************
*** Download & unzip map data from gov.uk ***
***************************************************************************
// Local Authority Districts (December 2023) Boundaries UK BFE
// Local Authority Districts (May 2023) Boundaries UK BGC
cd "$mapdata"
copy "https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/3f29d2c4a5834360a540ff206718c4f2/shapefile?layers=0" lamaps.zip
unzipfile lamaps
copy "https://open-geography-portalx-ons.hub.arcgis.com/api/download/v1/items/1292e3031e214641a023012a8fae2afa/shapefile?layers=0" districtmaps.zip
unzipfile districtmaps

* Create .dta files of countries & coordinates from the shapefile
// ssc install shp2dta
shp2dta using LAD_DEC_2023_UK_BFE.shp,    database(LA_names) coordinates(LA_coordinates)
shp2dta using LAD_MAY_2023_UK_BGC_V2.shp, database(district_names) coordinates(district_coordinates)
cd "$workingDir"