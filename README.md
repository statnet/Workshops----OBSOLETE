# Workshops
Source materials for Statnet Workshops

Please see our [wiki](https://github.com/statnet/Workshops/wiki) for our online training materials and software installation instructions.

**Notes for end users:**

If you would like to suggest edits/modifications/improvements/additions to any of our workshop materials, please file an issue in this GitHub repository (or, if you're familiar with GitHub and markdown, you can also fork, modify and submit a pull request).

**Notes for contributors on using this repository:**

1. source materials for the workshops (.Rmd files) should be stored in a folder with the workshop name at the top level.

* to get the standard `statnet` project headers, please follow the format shown at the top of [`ergm.Rmd`](https://github.com/statnet/Workshops/blob/a4ddaca30a9f4c8f16d004d8112e7acd264e6291/ergm/ergm_tutorial.Rmd#L13-L31).

2. to display the workshop materials on the [wiki](https://github.com/statnet/Workshops/wiki):

* knit the .Rmd file to html output (ideally -- though pdf can also be chosen)
* purl the .Rmd file to extract the `R` code
* copy the html (or pdf) and the `R` code file into the _docs_ folder
* modify the Workshops repo wiki as needed to display these two files
