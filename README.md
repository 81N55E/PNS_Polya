# PNS_Polya's Urn Simulation (the Clinical Trial Dilemma)
Polya's Urn Simulation in R + Shiny for PNS-Course@UvA 

This is code for a Polya's Urn Simulation (https://en.wikipedia.org/wiki/P%C3%B3lya_urn_model) written in R+Shiny.

It was created for a course assignement at a stats course of the University of Amsterdam. 

This Simulation uses the Clinical Trial Dilemma (proposed by Wei, 1979¹) that can be represented by a Polya's Urn Simulation. The patients represent the number of trials. The number of treatments represents the number of balls (different treatments = different colors) in the urn. The number of simulations represents the number of urns.

The ui.R file contains the user interface for the Shiny app.

The server.R file contains the background calculation pipeline for the Shiny app.

The R folder contains all necessary functions for the app to run. 

The PolyasShinyUrn.tar.gz file contains the whole package that can be installed and then loaded into R. 

# When you run the app you can choose between two main options: 

[1] simple: you can choose your number of treatments, the number of patients and the number of simulations. In addition you have the option to indicate the success rate (0-100%) of the treatments (one option for all treatments). The plot represents maximum 3 treatments. You can choose which treatments should be displayed as well as how many (max 3) treatments should be displayed.

[2] advanced: In advanced mode you have the same features as in simple mode but also additional ones. The additional once are: (1) choose different treatment success rate for the first two treatments; (2) choose different number of start rate for the first two treatments; (3) choose different number of return rate for the first two treatments and (4) choose different relapse rate for the frst two treatments. 

Please see the manual for more thorough documentation including installation guide, how to use the app and an example. 
Please use the Issues-Section here on Github for any questions or remarks.

# References
¹ Wei, L. J. (1979). The generalized Polya's urn design for sequential medical trials. The Annals of Statistics, 291-296.
