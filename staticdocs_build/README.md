````{R}
# devtools::install_github('hadley/staticdocs)
library(staticdocs)
 dir.create('staticdocs_build/staticdocs')
build_site(getwd(), site_path = "staticdocs_build/staticdocs")
````

So Far 

````{R}
Loading FSelectorRcpp
Error in `$<-.data.frame`(`*tmp*`, "title", value = "") : 
  replacement has 1 row, data has 0
````
