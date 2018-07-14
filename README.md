This repository contains code associated with the manuscript titled "Modeling phenological controls on carbon dynamics in dryland sagebrush ecosystems", which is currently in review.

Notes:

The analysis associated with this project was complex and involved
numerous different steps. Provided here is all new code that was
generated as part of the project. Several scripts require the LPJ-GUESS
model, which must be requested from the original authors. Other scripts
were run on Hyalite, a high-performance computing cluster at Montana
State University, and are designed to run many tasks in parallel.

Files with the .ins extension are instructional files that set key
parameters for each LPJ-GUESS model run.

Model output and reference data (GPP and LAI) are provided here so that
the analysis can be recreated.


1.  Data Processing
Files in the folder code-processing:
-   merge\_RC\_flux\_data.R
    -   Reads in 2 years of flux data, combines into one file, and
        converts units so compatible with LPJ-GUESS output
    -   Output: data/RCflux\_15\_16.csv
-   convert\_modis\_monthly.R
    -   Reads in all MODIS variables and converts data to monthly time
        step
    -   Output: data/ReynoldsC/MODIS/lai\_gpp.csv

2.  Latin Hypercube Sampling
Files in the folder code-processing/LHC\_scripts:
-   latin\_hypercube.R
    -   Creates 640 separate ins files with different sets of parameters
    -   Reads in automate\_tests/LHC.ins
    -   Calls automate\_tests/split\_into\_subsets.sh to create
        bundles of 32 files to run on separate nodes on Hyalite
        -   Output gets copied to Hyalite
-   LHC\_newphen.R
    -   Creates 420 separate ins files with different sets of parameters
    -   Reads in automate\_tests/LHC\_newphen.ins
    -   Calls automate\_tests/split\_into\_subsets.sh to create bundles
        of 32 files to run on separate nodes on hyalite
        -   Output gets copied to Hyalite
-   LHC.sh\*
    -   Copies ins files created by latin\_hypercube.R to compute nodes
        and runs LPJ-GUESS
-   LHC\_newphen.sh\*
    -   Copies ins files created by LHC\_newphen.R to compute nodes and
        runs LPJ-GUESS
Files in the folder automate\_tests:
-   split\_into\_substes.sh
    -   Called in LHC scripts, divides ins files into folders
-   LHC.ins
    -   Read in and modified via latin\_hypercube.R
-   LHC\_newphen.ins
    -   Read in and modified via LHC\_newphen.R
    
3.  Parameter Optimization
Files in the optim\_hyalite\_bundle folder:
-   DE\_paroptim.R & DE\_paroptim\_newphen.R
    -   Run optimization function on hyalite for the original model and
        new phenology models, respectively
    -   Output: .RData file containing model object with run history,
        diagnostics, and final parameter estimates
-   optim1\_summergreen.ins & optim2\_newphen.ins
    -   Ins files with dummy parameters that get replaced in each
        iteration of the optimization function
-   DEparoptim\_slurm.sh & DEparoptim\_newphen\_slurm.sh
    -   Initiates job on hyalite: copies files to compute nodes, runs
        the R scripts controlling the optimization function, and
        transfers output into the \$STORE directory on hyalite
-   runRDEpar.sh & runRnewphen.sh
    -   Tiny scripts called by the slurm scripts that in turn call the R
        scripts- this is a work-around for some hyalite quirks.
        Basically R needed to be called from the compute node to set the
        working directory to whatever compute node was being used,
        whereas since the slurm scripts run on the head node they
        couldn't figure out relative path names
        
4.  Analysis and Plots
Files in the code-analyze folder (alphabetical, several dependencies):
-   Calc\_percover\_lai.R
    -   Calculates percent of total LAI that is sagebrush in each model
        run
    -   Output: perclai.tex, **Table 4** in the manuscript
-   ESA\_plots.R
    -   Makes pretty color plots sized for powerpoint
-   fxn\_RPCC.R
    -   Several functions to calculate RPCC for annual, seasonal, and
        monthly model output, from both original model runs and runs
        with the new phenology
-   plot\_climate\_RC.R
    -   Reads in daymet climate data
    -   Output: TempPrecip.pdf, **Figure 1** in the manuscript
-   plot\_compare\_mod\_out.R
    -   Dependency: run plot\_gpp\_lai\_DEoptimparms.R first to generate
        model output
    -   Reads in output from all model runs and compares to reference
        data
    -   Output: GPP\_LAI\_newmods.pdf, **Figure 3** in the manuscript
    -   Output: GPP\_LAI\_newmods\_xy.pdf, **Figure S3** in the
        manuscript
-   plot\_gpp\_lai\_DEoptimparms.R^2^
    -   Reads in output from optimization run (must copy from Hyalite)
    -   Reads in blank ins
        file (optim\_hyalite\_bundle/summergreen\_optim1\_LMpar.ins) and
        modifies with optimized parameters
    -   Runs LPJ-GUESS then reads in the output
    -   Repeats previous steps for the new phenology model and compares
        output
    -   Output: LPJ-GUESS output from model runs with optimized
        parameters
-   plot\_modis\_vs\_field\_LAI.R
    -   Compares field LAI measurements to MODIS data
    -   Output: LAI\_field\_vs\_MODIS.pdf, **Figure S1** in the
        manuscript
-   plot\_summer\_ever\_flux.R
    -   Plots standard parameter runs (evergreen,
        summergreen, raingreen) against flux data
    -   Output: GPP\_LAI\_origpheno.pdf, **Figure 2** in the manuscript
-   R2\_MAE\_table.R
    -   Compares output from all model runs to EC GPP and MODIS LAI
    -   Calculates R2 and MAE
    -   Output: R2MAE2.tex, **Table 2** in the manuscript
-   sensitivity\_newphen\_parms.R
    -   Calculates RPCC for variables in sensitivity analysis of new
        model
    -   Output: RPCCecosagenewphen.R, **Table S3** in the manuscript
    -   Output: RPCC\_seas\_newphen.R, **Table S4** in the manuscript
-   table\_parm\_estimates.R
    -   Output: parm\_estimates.tex, **Table 3** in the manuscript
-   table\_RPCC\_total.R
    -   Calculates RPCC for variables in sensitivity analysis of
        original model
    -   Output: RPCCecosage.tex, **Table S1** in the manuscript
    -   Output: RPCC\_seas.tex, **Table S2** in the manuscript
-   tables\_RPCC\_grass.R
    -   Exploratory analysis: looks at sensitivity of grass biomass to
        variation in sage and grass parameters
    -   Creates several tables not used in manuscript

NOTES

^1^Must be run on Hyalite or a similar high-performance computing
cluster

^2^Requires LPJ-GUESS

