
# coconatfly

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of coconatfly is to provide access to highly experimental code for
comparative/integrative connectomics across Drosophila datasets.

## Installation

You can install the development version of coconatfly like so:

``` r
install.packages('natmanager')
natmanager::install(pkgs = 'flyconnectome/coconatfly')
```

Some of the datasets exposed by coconatfly require authentication for access or
are still being annotated in private pre-release. Please consult individual 
package dependencies for authentication details and do not be surprised if you
do not have access to all datasets if you are not a collaborator of the
[fly connectome group](https://flyconnecto.me).

For installation of private packages (currently restricted to in preparation 
datasets being developed with our collaborators at the 
[FlyEM Team at Janelia](https://www.janelia.org/project-team/flyem))
you will need a GITHUB_PAT. This should work

```
natmanager::check_pat()
```

