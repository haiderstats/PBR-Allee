# PBR-Allee

This repository serves as a supplement to the Incorporating Allee effects into the Potential Biological
Removal criterion paper submitted to Natural Resource Modeling. In the [Data folder](https://github.com/haiderstats/PBR-Allee/tree/master/Data) can be found the data used to generate Figure 5,6, and 7 (The 100 year performance, 20 year performance, and the carrying capacity performance respectively). 

The files for data generation can be found in [DataGenerationFiles folder](https://github.com/haiderstats/PBR-Allee/tree/master/DataGenerationFiles). The notes for each file can be found at the beginning of each of in the comments.

Figure generation files can be found in Performance-CarryingCapacityFigureGeneration.R file in  [FigureGenerationFiles folder](https://github.com/haiderstats/PBR-Allee/tree/master/FigureGenerationFiles). 

The initial simulations (Figures 3,4) are simulated in BaseSimulationsAndFigureGeneration.R. This is also where the figures are generated.

The data construction for the process outlined in Section 2.4 of the paper can be found in the FigureGenerationFiles folder in PBRComparison.R

Figure 1 generation can be found in PopulationTrajectoriesFigureGen.R. Figure 2 and the data for it can be found in LogisticAndAlleePBRComparision.R.

Note that all the functions constructed can be found in HelperFunctions.R in the [FunctionFiles folder](https://github.com/haiderstats/PBR-Allee/tree/master/FunctionFiles). However there are some functions in specific files where the function is only pertinent for the specific file. Explanations for each function can be found in the comments.

