# Perinatal COVID-19 vaccine effectiveness
You can download this repository as a zip folder using Code -> Download ZIP.
## Diagnostics
In the folder **Diagnostics** there is code for running diagnostics and for deploying the shiny with the results.
1) **PhenotypeR:** work through the `CodeToRun.R` script to execute the diagnostics. After running you should then have zip file with the results.
2) **Shiny:** paste the "zip" file in the "data" folder, and run the app (script `global.R`).

## Study
The folder **Study** contains the analytical code for the study. Please open the script `CodeToRun.R` (this is the only script you should interact with) and fill the information about your database connection and database-specific study parametres. Once complete, you can execute this study to start the study analysis. After running you should then have zip file with the results.

## ! Note
When opening an R project (for all projects in this repository*) follow this 3 steps:
1. Execute `renv::activate()` --> this will activate the renv library.
2. Execute `renv::restore()` --> this will load the relevant package with the target version.
3. Restart your R sesion `.rs.restartR()` --> make changes effective.

*phenotypeR.Rproj, ShinyPhenotypeR.Rproj, Shiny.Rproj, and Study.Rproj
