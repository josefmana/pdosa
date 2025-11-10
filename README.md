# hippoosa
My humble addition to Filip Růžička's project about the association of obstruction
sleep apnea with subcortical brain volume in early PD.

The [targets](https://docs.ropensci.org/targets/) package was used to create a reproducible
analysis pipeline.

To run the analyses, the following is needed:
- put all input files to the `data-raw` folder,
- then the following code needs to be run:

```
#install.packages("targets")
targets::tar_make()
```
