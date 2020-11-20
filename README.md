
<!-- README.md is generated from README.Rmd. Please edit that file -->

Property Allocation <img src='man/figures/logo.png' align="right" height="30.5" />
==================================================================================

**propalloc** is an R Package housing the codebase for the Oliver Wyman
R Shiny Property Allocation Demo Application.

-   Date: 2020-10-01
-   Copyright 2020 [Oliver Wyman Actuarial,
    Inc.](https://www.oliverwyman.com/index.html)

Badges
------

<!-- badges: start -->

![pkgdown](https://github.com/jimbrig/propalloc/workflows/pkgdown/badge.svg)
[![lifecycle](https://img.shields.io/badge/Lifecycle-Maturing-darkgreen.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![package%20version](https://img.shields.io/badge/Package%20Version-1.1.0-orange.svg)](https://github.com/jimbrig/propalloc/blob/master/commits/master)
[![github](https://img.shields.io/badge/Github-jimbrig/propalloc-black.svg)](https://github.com/jimbrig/propalloc)
[![bitbucket](https://img.shields.io/badge/Bitbucket-owac/propalloc-blue.svg)](https://bitbucket.org/owac/propalloc/src/master/)
<!-- [![R build status](https://github.com/jimbrig/propalloc/workflows/R-CMD-check/badge.svg)](https://github.com/jimbrig/propalloc/actions) -->
<!-- badges: end -->

Purpose
-------

The goal of this project is to create a model in R Shiny that allocates
an insured’s prospective year property insurance costs in an equitable,
easily understood manner that saves the client time, provides the option
to use an “actuarial” methodology, while still allowing for maximum
flexibility and edits.

Team
----

-   Project Leader: [Adam Lewis](mailto:adam.lewis@oliverwyman.com)  
-   Developer: [Jimmy Briggs](mailto:jimmy.briggs@oliverwyman.com)
-   Support: [Jack Reardon](mailto:jack.readron@oliverwyman.com) and
    [Reza Milani](mailto:reza.milani@oliverwyman.com)
-   Peer Reviewer: [Brian Settle](mailto:brian.settle@oliverwyman.com)

Demo
----

To demo the app, you have a couple of options:

-   If you want to run the application locally, simply install the
    package and run the code below:

<!-- -->

    library(propalloc)
    propalloc::run_app()

*Note that the run\_app function will automatically install package
dependencies for you.*

-   Or to view a live **test** version of the shiny application running
    on the Oliver Wyman shiny server
    [here](https://oliverwymanapps.mmc.com/actuarial/ow-innovations/test/property-allocation-test/).

Resources
---------

View all package documentation by installing and running:

    propalloc::open_docs()

#### Links

-   Browse source code from the [BitBucket
    Repository](https://bitbucket.org/owac/property-allocation-shiny-app/src/master/)
    or the [GitHub
    Repository](https://github.com/jimbrig2011/propalloc).
-   Browse various reference material on the [GoogleDrive Project
    Folder](https://drive.google.com/drive/u/1/folders/1m-HPRYoyQ2kehT03javhLEOsfS4gaw0H).
-   Track Project’s progress from [GitKraken Glo
    Board](https://app.gitkraken.com/glo/board/XpZXjYmFCQARJGOT) or the
    [GitHub Project
    Board](https://github.com/jimbrig/propalloc/projects/1#card-44700781).
-   Visit the [Package Documentation
    Website](https://jimbrig.github.io/propalloc/) for R-Package
    specific information.
-   Scan the [Github Project
    Wiki](https://github.com/jimbrig/propalloc/wiki) for useful
    material.

#### Documents

-   [Changelog](inst/reports/changelog.md)
-   [Contributor Code of Conduct](CODE_OF_CONDUCT.md)
-   [License](LICENSE.md)
-   View the [Contributing](inst/reports/contributing.md) document for
    on-boarding resource.  
-   View the [Roadmap](inst/reports/roadmap.md) for schedule and planned
    future actions.

Installation
------------

**propalloc** is setup as an R Package making it simple to house all the
various dependencies that the final application relies on.

Make sure you have the latest and stable version of
[devtools](https://github.com/hadley/devtools).

### BitBucket

Install from [BitBucket](https://bitbucket.org) via:

    library(devtools)
    devtools::install_bitbucket("owac/property-allocation-shiny-app",
                                "<username>", 
                                "<password>")

For example, if your username is `john.doe@oliverwyman.com` and your
password is `password123` you’ll run this code:

    devtools::install_bitbucket(
      "owac/property-allocation-shiny-app", 
      auth_user = "john.doe@oliverwyman.com", 
      password = "password123"
    )

Your bitbucket password may be different than your windows password.
Your username is probably your Oliver Wyman email address.

### GitHub

Install from [GitHub](https://github.com) via:

For information on setting up your GitHub PAT (Personal Access Token)
visit [this webpage](https://github.com/settings/tokens).

    library(devtools)

    # using username/password
    devtools::install_github("jimbrig2011/propalloc",
                             "<username>", 
                             "<password>")

    # using GitHub PAT
    # library(usethis)
    # usethis::browse_github_pat()

    devtools::install_github("jimbrig2011/propalloc",
                             auth_token = github_pat(quiet))

------------------------------------------------------------------------

If you have any trouble, contact [Jimmy
Briggs](jimmy.briggs@oliverwyman.com) to make sure you have read access
to the repository and for further troubleshooting.

Technical
---------

This project is set up as an R Package. To view important details
regarding the setup visit the [Technical API Vignette]().

It also utilized GIT LFS. To install run **git lfs install** in git
bash.

Scope of Services
-----------------

Upon completion, this model will be used as a demo to sell future work,
similar to the R Shiny casualty cost allocation model. Once sold, this
model will provide a template for billable projects.

Specifically, the property allocation model will allocate the premium,
taxes, fees, surcharges and any related costs (collectively, the
“property insurance costs”) to the desired level. This level can include
up to six hierarchies (e.g. Country, Business Unit, Division, Region,
Location, and Intra-location Department).

The tool will enable the client to incorporate the following criteria
into its allocation process:

-   Total Insured Value (TIV)
-   Market AOP Rates (provided by Marsh or Client)
-   Market CAT Rates
-   Wind Tier
-   Earthquake Exposure
-   Flood Exposure
-   Terrorism Rates
-   Claim Frequency
-   Claim Severity
-   Risk Engineering Credits/Surcharges
-   Other Manual Adjustments
-   Relativity Adjustments
-   Combustible vs. Non-Combustible
-   Sprinkler Type/Status
-   Business Unit (BU)

In addition, the model will allow a client to select various thresholds
and caps on premium/rate changes.

The tool will also provide multiple summaries, facilitating the
comparison of the resulting allocation in up to three different
scenarios. For example, the client can compare the resulting to
allocation based upon: proposed market rates without adjustment, rates
used in the prior year allocation, proposed increases for budget
guidance applied to the rates used in the prior year.

The tool will also give the user the ability to:

-   Edit the TIV, properties, or other aspects of the exposures in the
    statement of values (SOV) between the scenarios outlined above (this
    is of importance as information often changes throughout the
    allocation process)
-   Force the final allocation for a specific or a set of specific
    locations to be a fixed number
-   Change the different loss severity buckets considered in the model
-   Add extra fees after the allocation is complete, while still
    maintaining the allocation of the other property insurance costs

Lastly, the tool will provide a summary by any combination of the six
hierarchies that will calculate the dollar change in premium
attributable to any of the following:

-   change in TIV,
-   change in the initial implied rates,
-   claims charges,
-   manual “all other risk” adjustment, and/or
-   cap placed on the increase in rate and the resulting off-balance
    associated with a cap.

Company Policy
==============

Contributing
------------

Please note that the ‘propalloc’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

Circulation or Publication
--------------------------

This model is not intended for general circulation or publication, nor
is it to be used, quoted or distributed to others for any purpose other
than those that may be set forth herein or in the written agreement
pursuant to which this model has been issued without the prior written
consent of [Oliver Wyman](https://www.oliverwyman.com/index.html).

Third Party Reliance and Due Diligence
--------------------------------------

Oliver Wyman’s consent to any distribution of this model (whether herein
or in the written agreement pursuant to which this model has been
issued) to parties other than CLIENT XYZ, does not constitute advice by
Oliver Wyman to any such third parties and shall be solely for
informational purposes and not for purposes of reliance by any such
third parties. Oliver Wyman assumes no liability related to third party
use of this model or any actions taken or decisions made as a
consequence of the results, advice or recommendations set forth herein.
This model should not replace the due diligence on behalf of any such
third party.

Public Dissemination
--------------------

Neither all nor any part of the contents of this model, any opinions
expressed herein, or the firm with which this model is connected, shall
be disseminated to the public through advertising media, public
relations, news media, sales media, mail, direct transmittal, or any
other public means of communications, without the prior written consent
of Oliver Wyman.

### Avoiding Tax Penalty

The actuarial findings contained in this model are not intended to be
used, and cannot be used, by the taxpayer for the purpose of avoiding
tax penalties that may be imposed on the taxpayer.
