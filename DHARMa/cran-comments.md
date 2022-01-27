# Version 0.4.5

## Submission 1, 16.1.2022

This is a minor update of DHARMa, including various smaller improvements and bugfixes. See NEWS for details.

This release was tested without apparent problems under

* local MAC OS 12.0.1 (Monterey), R 4.1.0
* GitHub Actions - Windows / Mac / Ubuntu (release), Ubuntu (devel)
* http://win-builder.r-project.org/ - oldrelease / devel / release

# Version 0.4.4

## Submission 1, 28.9.2021

This is a minor update of DHARMa, including various smaller improvements and bugfixes. See NEWS for details.

This release was tested without apparent problems under

* local MAC OS 11.6 (Big Sur), R 4.1.0
* GitHub Actions - Windows / Mac / Ubuntu (release), Ubuntu (devel)
* http://win-builder.r-project.org/ - oldrelease / devel / release

# Version 0.4.3

## Submission 1, 6.7.2021

This is a bugfix release, attempting to fix the issue with the dependency on glmmTMB (which does not complie on some OS), which was apparently exacerbated by the 0.4.2 update 2 days ago.

I have for the moment (hopefully) fully removed glmmTMB from DHARMa, and will re-introduce it once glmmTMB is fixed and back on CRAN.

This release was tested without apparent problems under

* local MAC OS 11.4 (Big Sur), R 4.1.0
* Linux (Travis CI) - release
* GitHub Actions - Windows / Mac / Ubuntu (release), Ubuntu (devel)
* http://win-builder.r-project.org/ - oldrelease / devel / release

# Version 0.4.2

## Submission 1, 5.7.2021

This is a bugfix release. See NEWS for details.

This release was tested without apparent problems under

* local MAC OS 11.4 (Big Sur), R 4.1.0
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel
* GitHub Actions - Windows / Mac / Ubuntu (release), Ubuntu (devel)

# Version 0.4.1

## Submission 1, 24.3.2021

This is a bugfix release. See NEWS for details.

This release was tested without apparent problems under

* local MAC OS 10.16 (Big Sur), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel


# Version 0.4.0

## Submission 1, 24.3.2021

This is a bugfix release for 0.3.4, but on reflection I decided that 0.4.0 should have been a minor release, so I pushed the version number up to 0.4.0

This release was tested without apparent problems under

* local MAC OS 10.16 (Big Sur), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel

## Submission 2, 27.3.2021

This is a bugfix release for 0.3.4, but on reflection I decided that 0.4.0 should have been a minor release, so I pushed the version number up to 0.4.0

This release was tested without apparent problems under

* local MAC OS 10.16 (Big Sur), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel

Notes: this is the second submission, pre-submission checks from earlier version complained about lazy data in the descriptin, which I have removed, although I don't see the problem to set lazy data even if there is no data in the package, it's just my default description setting for packages.  

## Submission 3, 27.3.2021

This is a bugfix release for 0.3.4, but on reflection I decided that 0.4.0 should have been a minor release, so I pushed the version number up to 0.4.0

This release was tested without apparent problems under

* local MAC OS 10.16 (Big Sur), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel

Notes: this is the third submission, pre-submission checks from earlier version complained about lazy data in the descriptin, which I have now really removed.

# Version 0.3.4

## Submission 1, 23.3.2021

This is a relatively substantial update of DHARMa, with various smaller new features and bugfixes. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.16 (Big Sur), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel

Issues:

Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-ix86+x86_64
Check: CRAN incoming feasibility, Result: Note_to_CRAN_maintainers
  Maintainer: 'Florian Hartig <florian.hartig@biologie.uni-regensburg.de>'

Flavor: r-devel-windows-ix86+x86_64
Check: running examples for arch 'i386', Result: NOTE
  Examples with CPU (user + system) or elapsed time > 10s
                user system elapsed
  hist.DHARMa   8.86   0.20   10.42
  plotResiduals 8.48   0.08   10.84

Flavor: r-devel-windows-ix86+x86_64
Check: running examples for arch 'x64', Result: NOTE
  Examples with CPU (user + system) or elapsed time > 10s
                 user system elapsed
  hist.DHARMa   10.94   0.12   11.73
  plotResiduals  9.64   0.00   10.17

## Submission 2, 23.3.2021

This is a relatively substantial update of DHARMa, with various smaller new features and bugfixes. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.16 (Big Sur), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel

Notes: this is the second submission, pre-submission checks from earlier version complained about slow help files, I have reduced some of the help calculations in response




# Version 0.3.3

## Submission 1

## Submission 1, 22.8.20

This is a minor update of DHARMa. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.15.4 (Catalina), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel


# Version 0.3.2

## Submission 1

## Submission 1, 16.6.20

This is a minor update of DHARMa, with a few stability improvement and bugfixes. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.15.4 (Catalina), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel

# Version 0.3.1

## Submission 1

## Submission 1, 11.5.20

This is a minor update of DHARMa, with a few stability improvement and bugfixes. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.15.4 (Catalina), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel

# Version 0.3.0

## Submission 1

## Submission 1, 20.4.20

This is a substantial update of DHARMa, introducing various new functions and bugfixes. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.15.4 (Catalina), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel


# Version 0.2.7

## Submission 1, 4.2.20

This is a minor update of DHARMa, mainly enhancing compatibility with glmmTMB. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.14.1 (Mojave), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel

# Version 0.2.6

## Submission 1, 26.11.19

This release fixes a bug in the vignette that appeared after last release last week. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.14.1 (Mojave), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel


# Version 0.2.5

## Submission 1

This is a minor bugfix release. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.14.1 (Mojave), R 3.6.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel

# Version 0.2.4

## Submission 1

This is a bugfix release, responding to an email from CRAN about a problem building the vignette. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.14.1 (Mojave), R 3.5.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel

# Version 0.2.3

## Submission 1

This is a maintenance release with a few smaller bug fixes and a few improvements to the package. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.14.1 (Mojave), R 3.5.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel


# Version 0.2.2

## Submission 1

This is bugfix release for version 0.2.1, which was published on CRAN a few days ago. This new release 0.2.2 fixes an issue in the vignette (due to some old header command, title, abstract and TOC were not shown). Apologies for the oversight.

This release was tested without apparent problems under

* local MAC OS 10.14.1 (Mojave), R 3.5.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel


# Version 0.2.1

## Submission 1

This is minor release, adding a few new functionalities and support for the package spaMM.

This release was tested without apparent problems under

* local MAC OS 10.14.1 (Mojave), R 3.5.1
* http://win-builder.r-project.org/ - oldrelease / devel / release
* Linux (Travis CI) - oldrel / release / devel


# Version 0.2.0

## Submission 1

This is major release with a number of important changes to the package. See DHARMa NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.13.4 (High Sierra), R 3.3.2
* http://win-builder.r-project.org/ - oldrelease / devel / realease
* Linux (Travis CI) - oldrel / release / devel


# Version 0.1.6

## Submission 1

This is a maintenance release with a few smaller bug fixes and a few improvements to the package. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.13.3, R 3.3.2
* http://win-builder.r-project.org/ - oldrelease / devel / realease
* Linux (Travis CI) - oldrel / release / devel

# Version 0.1.5

## Submission 1

This release fixes a bug in version 0.1.4 that occurred when running simulateResiduals with refit = T. Apologies for any inconvenience.

This release was tested without apparent problems under

* local MAC OS 10.10.5, R 3.3.1
* http://win-builder.r-project.org/, current settings
* Linux (Travis CI)
 * oldrel / release / devel

# Version 0.1.4

## Submission 1

This is a minor update. See NEWS for changes.

This release was tested without apparent problems under

* local MAC OS 10.10.5, R 3.3.1
* http://win-builder.r-project.org/, current settings
* Linux (Travis CI)
 * oldrel / release / devel

# Version 0.1.3

## Submission 1

This is a minor update. Important changes

- includes support for model class 'gam' from package 'mgcv'. Required overwriting the 'fitted' function for gam, see https://github.com/florianhartig/DHARMa/issues/12

- plotResiduals includes support for factors

- updates to the help

This release was tested without apparent problems under

* local MAC OS 10.10.5, R 3.3.1
* http://win-builder.r-project.org/
 * R-release: R-3.3.1
 * R-devel: 2016-11-14 r71659
* Linux (Travis CI)
 * oldrel / release / devel

# Version 0.1.2

## Submission 1

This is a bugfix release that fixes an issue with backwards compatibility introduced in the 0.1.1 release of DHARMa. 0.1.1 used the 'startsWith' function that is only available in R base since 3.3.0. Apologies for this oversight. I replaced all occurences of 'startsWith' with 'grepl', which should restore the compatibility with older R versions.

This release was tested without apparent problems under

* local MAC OS 10.10.5, R 3.3.1
* http://win-builder.r-project.org/
 * R-release: R-3.3.1
 * R-devel: 2016-11-14 r71659
* Linux (Travis CI)

# Version 0.1.1

## Submission 1

This is a minor update of the DHARMa package

- including now the negative binomial models from MASS and lme4, as well as the possibility to create synthetic data from the negative binomial family

- includes a createDHARMa function that allows using the plot functions of DHARMa also with externally created simulations, for example for Bayesian predictive simulations

### Tested under

* local MAC OS 10.10.5, R 3.3.1
* http://win-builder.r-project.org/
 * R-release: R-3.3.1
 * R-devel: 2016-11-14 r71659

### R CMD check results

#### local

R CMD check results
0 errors | 0 warnings | 0 notes

R CMD check succeeded

#### win-builder

only some spelling problems that seem fine to me.



# Version 0.1.0


## Submission 2

Hi,

thanks for the comments. I hope I have fixed everything now, see notes below.

Best,
Florian

//

We're seeing the notes you saw, and some others:

> Possibly mis-spelled words in DESCRIPTION:
>   DHARMa (7:18)
>   HierArchical (2:33)

Please put single quotes around software names, and use standard capitalization on English words.

* changed DHARMa to 'DHARMa' (quotation marks)
* changed HierArchical to Hierarchical (although HierArchical was on purpose because of the abbreviation DHARMa)

>
> The Title field should be in title case, current version then in title case:
> 'Residual Diagnostics for HierArchical (Multi-level / Mixed) Regression Models'
> 'Residual Diagnostics for HierArchical (Multi-Level / Mixed) Regression Models'

The issue was changing  "Multi-level" to "Multi-Level"? If that is so, it's changed.

> The Date field is not in ISO 8601 yyyy-mm-dd format.

Please fix.

* changed the date field to 2016-08-25

> ** running examples for arch 'i386' ... [17s] NOTE
> Examples with CPU or elapsed time > 10s
>                    user system elapsed
> simulateResiduals 12.67   0.03   12.72
> ** running examples for arch 'x64' ... [17s] NOTE
> Examples with CPU or elapsed time > 10s
>                    user system elapsed
> simulateResiduals 11.48   0.03   11.45

Please shorten the example code so it runs in 5 sec.

* changed the settings in the help of simulateResiduals() - I hope it is below 5 second now.

Resubmit when you have made these changes and passed checks without these notes.

Duncan Murdoch

## Submission 1

### Tested under

* local MAC OS 10.10.5, R 3.3.1
* local Windows 2012 Server, R 3.3.0
* http://win-builder.r-project.org/
 * R-release: R-release, currently R-3.3.1
 * R-devel: R-devel, to be R-3.4.0

### R CMD check results

#### local

R CMD check results
0 errors | 0 warnings | 0 notes

R CMD check succeeded

#### win-builder

Notes:

Possibly mis-spelled words in DESCRIPTION:
  DHARMa (7:18)
  HierArchical (2:33)
  Multi (2:47)
  autocorrelation (14:5)
  interpretable (8:13)
  misspecification (12:73)
  underdispersion (13:27)

The Title field should be in title case, current version then in title case:
'Residual Diagnostics for HierArchical (Multi-level / Mixed) Regression Models'
'Residual Diagnostics for HierArchical (Multi-Level / Mixed) Regression Models'

--> This seems fine to me. The spelling is as intendend, and the title is in title case
