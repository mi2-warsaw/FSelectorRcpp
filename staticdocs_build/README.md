````{R}
# devtools::install_github('hadley/staticdocs)
library(staticdocs)
dir.create('staticdocs_build/staticdocs')
build_site(getwd(), site_path = "staticdocs_build/staticdocs")
````
