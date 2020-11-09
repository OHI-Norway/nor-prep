
# Code and example tables to make an OHI flowerplot

This folder containts the code to make a flowerplot, provided by the Eleanore Campbell at Baltic Health Index.
There are also two tables - scores and plot configuration, that we can use to test the code.

Some explanations from Eleanore: 
The flower plot code has undergone multiple iterations and I’ve moved it around quite a bit. I believe this is the code you are looking for: https://github.com/OHI-Science/bhi/commit/c331d817a1b31220f3586d03e51a01bc4642dc9f  (load the flowerplot.R diff). This old code has some file path dependencies, so to make things easier for you I’ve copied the relevant code to an R script (attached).  
 
The script is broken into multiple functions for doing data wrangling, configuration, and plotting. Run the entire script to define the functions; then you only need to run the flowerwgradient function with scores.csv and goals.csv dataframes and some other arguments which are defined near the top of the script. Also at the top of the script are some theme elements/color palettes you can adjust as you want. Note: the code for the plot with gradients along the petals will only plot one region’s flowerplot at a time; you can give a vector of region_id’s to the rgns argument but it will take the first one. I did this because it is quite slow to plot…
 
Let me know if it does not work, or if you need more info on how to calculate the plot or table. I am including the bhi rgn_scores and plot_config datasets also in case it’s helpful to see how the input data looks like (though not all their columns are used by the functions).
 
**How to cite the code for flowerplot**
