## Test environments

* local Windows 10, R 4.2.1 + devel
* Github Actions "windows-latest (release)"
* Github Actions "macOS-latest (release)"
* Github Actions "ubuntu-22.04-latest (release)"
* Github Actions "ubuntu-22.04-latest (devel)"
* Github Actions "ubuntu-22.04-latest (oldrel-1)"
* Windows Server 2022, R-devel, 64 bit
* r-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC
* r-hub Fedora Linux, R-devel, clang, gfortran
* win-builder.r-project.org

## R CMD check results

There were no ERRORs or WARNINGs, only a NOTE about mistaken spelling errors in DESCRIPTION: 

_Dimitra (21:16), Furtbauer (20:24), Garnier (20:57), Georgopoulou (21:3), O'Bryan (20:40), Papadopoulou (20:3), discretizing (16:28)_. 

These words are not misspelled (the first 6 are proper nouns and the last one is correctly spelled).


## Downstream dependencies

There are currently no downstream dependencies for this package.

## CRAN team comments

* Please do not start the description with "This package", package name,
title or similar. -> You can simply omit "A function library that ...".

Fixed.

*  If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking. (If you want to add a title as well please put it in
quotes: "Title")

A reference is added in the form authors (year) <doi:...>.

* Unless "swarm space" is a package or software name, please remove the
single quotes around it.

Fixed.

* We see:
Warning: Unexecutable code in man/group_metrics_per_set.Rd:
   set = rep('', ),:
Warning: Unexecutable code in man/nn_metrics.Rd:
   set = rep('', ),:
Warning: Unexecutable code in man/pairwise_metrics.Rd:
   set = rep('', ),:

Fixed, also given the next remark.

* \dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user. Does not seem necessary. 
Please replace \dontrun with \donttest. Please unwrap the examples if they are executable in < 5 sec, or replace
dontrun{} with \donttest{}.

Most examples are now executable and unwrapped. The set_data_format example was executable in > 5 sec on the Fedora and Ubuntu Linux test, so it was wrapped in \donttest{}.

* You write information messages to the console that cannot be easily
suppressed. 
It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of print() rather use message()/warning() or if(verbose)cat(..)
(or maybe stop()) if you really have to write text to the console.
(except for print, summary, interactive functions)-> R/events_metrics.R

The console writing has been revised, all print() have been replaced with cat() and message() as proposed, except in the interactive function 'pick_threshold'. 