# School Segregation App

This is a *development version* of a web application that visualizes school
segregation in the United Kingdom. It allows users to
select a local authority and view the segregation 
plots. Segplots are a method for visualising school segregation 
(see [here](https://www.sciencedirect.com/science/article/pii/S027656242300104X)). 
For some example segplots, and examples on how to interpret them
please see [`example_segplots.pdf`](example_segplots.pdf).

This tool could be adapted/extended to visualise other types of 
segregation in other locations. 

## For Developers

Plots for this tool are generated using the
"segregation" package (available in the R-programming)
language. In the current setup we have:

1. **R-scripts** (📁`segDataPrep/`): A series of R-scripts used to process data on school segregation (credit to [Stuart Lane](https://www.bristol.ac.uk/people/person/Stuart-Lane-394f9fd8-ea4d-43cf-b7bc-409fd5bac2f0/) and [Rob Gruijters](https://www.bristol.ac.uk/people/person/Rob-Gruijters-b1007ebe-9659-4717-a9c8-2da935993d0d/)). All outputs are written to 📁`segDataPrep/outputs/` folder in this directory. These outputs contain all segplots, but also metadata describing: which plots are generated; where they are stored; the legends associated with these plots. 

2. **React App** (📁`app/`): A React app that displays the segregation plots. The application, hosted on github pages, uses shapefi 

Each script, see [`segDataPrep/README.md`](segDataPrep/README.md), has a description of its purpose and how to run it. When this application is pushed to github, the application is recompiled and deployed to github pages, see `.github/workflows/deploy.yml` for more details.