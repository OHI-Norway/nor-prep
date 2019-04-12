###  This folder is for the output data - tables with calculation of scores that we make

For example, the data tables with ready-made indices of sustainable fisheries, which we will upload on the Coastal barometer website, will be storted here.

**Data tables in the folder**:

Filename  | Explanation
------------- | -------------
catch_weighted_score | proportions of each each per municipality and catch weighted score
sustain_index  | fisheries sustainabylity index for each stock and municipallity
fishery_final_score| final score of  fisheries sustainability per municipality (all stocks).

**Explanation of calculations**:

*Stock score*:
$$ SSB/Bmsy $$


*Catch weighted score*:
$$ (SSB/Bmsy)*catch.prop $$

*Final sustainability score*:

If $SSB/Bmsy >= 1$, $score = 1$

If $SSB/Bmsy < 1$, $score = [SSB/Bmsy - Blim/Bmsy]/(Blim/Bmsy)$
 
If $SSB/Bmsy <=Blim.Bmsy$, $score = 0$
                                   



