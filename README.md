# School Segregation App

This is a *development version* of a web application that visualizes school
segregation in the United Kingdom. It allows users to
select a local authority and view the segregation 
plots. Segplots are a method for visualising school segregation 
(see [here](https://www.sciencedirect.com/science/article/pii/S027656242300104X)). 
For some example segplots, and examples on how to interpret them
please see [`example_segplots.pdf`](example_segplots.pdf).

This tool could, in theory, be adapted/extended to visualise other types of 
segregation in other locations. 

## For Developers

In the current setup we have:

1. **R-scripts** (📁`segDataPrep/`): A series of R-scripts and Stata scripts used to process data on school segregation (credit to [Stuart Lane](https://www.bristol.ac.uk/people/person/Stuart-Lane-394f9fd8-ea4d-43cf-b7bc-409fd5bac2f0/) and [Rob Gruijters](https://www.bristol.ac.uk/people/person/Rob-Gruijters-b1007ebe-9659-4717-a9c8-2da935993d0d/)). All outputs are written to 📁`segDataPrep/outputs/` folder in this directory. These outputs contain all segplots, but also metadata describing: which plots are generated; where they are stored; the legends associated with these plots. The scripts are designed to be run in numerical order. Dependencies are managed using `renv`.

2. **React App** (📁`app/`): A React app that displays the segregation plots. The application, hosted on github pages, uses shapefiles from the ONS geoportal to generate a map of local authority districts. A short form allows users to filter by school time, segregation type, and time frame. Using this query, the application fetches the corresponding segplot stored in the 📁`segDataPrep/outputs/` folder. The application is recompiled and deployed to github pages using github actions, see `.github/workflows/deploy.yml` for more details.

