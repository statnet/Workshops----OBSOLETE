# Workshops (now Obsolete)
This repository holds source materials for Statnet Workshops prior to 2022.

Current Statnet workshops are now held in a separate repositories, with a name like `workshop-ergm`.  There are two ways to access the public materials from these repositories:

* For a list of all workshops and links to their public materials, please see the [workshops page](https://statnet.org/workshops/) on the Statnet website.

* If you know which specific workshop you are looking for, you can navigate directly to that workshop with a url like [https://statnet.org/workshop-ergm/](https://statnet.org/workshop-ergm) (this example is for the ergm workshop).

Members of the statnet development team can find the complete list of workshop repositories by filtering from the [main organization page](https://github.com/statnet) for `workshop`.

We are leaving this respository online for the convenience of users who have links to it.

__ALL INFORMATION BELOW THIS LINE IS OBSOLETE__
____

Please see [statnet.github.io](https://github.com/statnet/Workshops/wiki) for our online training materials and software installation instructions.

**Notes for end users:**

If you would like to suggest edits/modifications/improvements/additions to any of our workshop materials, please file an issue in this GitHub repository (or, if you're familiar with GitHub and markdown, you can also fork, modify and submit a pull request).

**Notes for contributors on using this repository:**

1. source materials for the workshops (.Rmd files) should be stored in a folder (folder name = workshop name) at the top level.

* to get the standard `statnet` project headers, please follow the format shown at the top of [`ergm.Rmd`](https://github.com/statnet/Workshops/blob/a4ddaca30a9f4c8f16d004d8112e7acd264e6291/ergm/ergm_tutorial.Rmd#L13-L31).

2. to display the workshop materials on the [wiki](https://github.com/statnet/Workshops/wiki):

* knit the .Rmd file to html output (ideally -- though pdf can also be used)
* purl the .Rmd file to extract the `R` code
* copy the html (or pdf) and the `R` code file into the _docs_ folder
* modify the Workshops repo wiki as needed to display these two files
