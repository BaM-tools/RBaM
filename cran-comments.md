## Resubmission

This is a resubmission. In this version:

* I explained the acronym MCMC when first used in the DESCRIPTION text.
* I used an immediate call of on.exit() to reset the working directory to its initial value in function runExe().
* I removed default paths in writing functions BaM() and downloadBaM(), so that these paths are now explicitly defined by the user. I modified doc, examples and README accordingly.
* As a consequence of the previous change, I am now using tools::R_user_dir() to determine the configuration folder where I can persistently store a user-defined installation path. This information is stored in a very small single-line configuration text file.
* I implemented a new function setPathToBaM() enabling the user to modify the configuration file above, or to delete the configuration folder altogether (using setPathToBaM(NULL)).
* I added a dependency to R (>= 4.0.0) in the DESCRIPTION file, due to the use of tools::R_user_dir()
* I increased the package version from 1.0.0 to 1.0.1.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
Possibly misspelled words in DESCRIPTION:
  Coz (26:8)
  Darienzo (28:5)
  Mansanarez (25:5)
  Perret (27:5, 29:5)
  Renard (23:5)
  al (25:19, 26:15, 27:15, 28:17, 29:15)
  et (25:16, 26:12, 27:12, 28:14, 29:12)
  
All words are spelled correctly
