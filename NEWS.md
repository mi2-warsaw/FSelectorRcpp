If something is missing or not clear - please chat with us on our [slack](https://fselectorrcpp.slack.com/messages/general/)?

FSelectorRcpp 0.3.9
----------------------------------------------------------------

Fix CRAN warnings. No new functionality.

FSelectorRcpp 0.3.8
----------------------------------------------------------------

Fix CRAN tests. No new functionality.

FSelectorRcpp 0.3.5
----------------------------------------------------------------
- copied RreliefF scoring algorithm from FSelector [#78](https://github.com/mi2-warsaw/FSelectorRcpp/issues/78) [#86](https://github.com/mi2-warsaw/FSelectorRcpp/issues/86)

FSelectorRcpp 0.3.4
----------------------------------------------------------------
- fixed conditional/joint distribution confusion in the documentation of information_gain
[#85](https://github.com/mi2-warsaw/FSelectorRcpp/issues/85)

FSelectorRcpp 0.3.3
----------------------------------------------------------------

- turned off the OpenMP parallel backend in information_gain function - I could cause segfault sometimes. Better safe than sorry.
- fix parameters mismatch in init.c

FSelectorRcpp 0.3.1
----------------------------------------------------------------

- remove strip debug info.

FSelectorRcpp 0.3.0
----------------------------------------------------------------

The purpose of this release is to make more compatible with `FSelector` package. We changed some of the default behaviors, so the results might be different between version `2.*` and `3.0`.

- `information_gain` and `discretize` get new parameter `discIntegers` to control if integer columns should be discretized. Default value is `TRUE`, so it means that they're treated like numerics. For more information please refer to `vignette("integer-variables", package = "FSelectorRcpp")`.
- FSelectorRcpp tries to mimic the behavior of the FSelector when there are NAs in the dependent variable passed to `information_gain` (remove only those rows which contain NAs dependent variable, NAs in independents variables are removed column-wise).

FSelectorRcpp 0.2.0
----------------------------------------------------------------

- Changed `discretize` argument `all` to `TRUE`.
- Added `customBreaksControl` for creating custom breaks in `discretize` function.
- `discretize` can be now evaluated with data as a first argument in the formula interface
  - You can now use `discretize(iris, Species ~ .)` or `discretize(Species ~ ., iris)`.
  - `discretize(iris, Species ~ .)` seems to be more pipe friendly.
- `discretize_transform` allows applying the discretization cut points to the new data set.
- `extract_discretize_transformer` produces small object containing all cutpoints. It can be also used to transform the new data set.
  - `extract_discretize_transformer` can be useful in ML pipelines where the training data needs to be discarded to save memory.

FSelectorRcpp 0.1.8
----------------------------------------------------------------

- Removed support for the doSnow progress bar. We now use doParallel
  in our examples, which is more recommended than doSnow.

Bug fixes:
- Fixed build using Rcpp 0.12.12
- feature_search now returns proper structure.


FSelectorRcpp 0.1.3
----------------------------------------------------------------

Bug fixes:

- Export C++ routine for tests.
- Use dontrun to hide problematic example (for CRAN).


FSelectorRcpp 0.1.2
----------------------------------------------------------------

Bug fixes:

- Removed problematic links in vignettes.

FSelectorRcpp 0.1.1
----------------------------------------------------------------

Bug fixes:

- Skip benchmark if the `RTCGA.rnaseq` package is not available.
- Minor fixes in vignettes.


FSelectorRcpp 0.1.0
----------------------------------------------------------------

Rcpp (free of Java/Weka) implementation of [FSelector](https://cran.r-project.org/web/packages/FSelector/index.html) entropy-based feature selection algorithms with sparse matrix support.

Provided functions

- `discretize()` with additional `equalsizeControl()` and `mdlControl` - discretize a range of numeric attributes in the dataset into nominal attributes. **Minimum Description Length (MDL)** method is set as the default control. There is also available `equalsizeControl()` method.
- `information_gain()` - algorithms that find ranks of importance of discrete attributes, basing on their  entropy with a continous class attribute,
- `feature_search()` - a convenience wrapper for \code{greedy} and \code{exhaustive} feature selection algorithms that extract valuable attributes depending on the evaluation method (called evaluator),
- `cut_attrs()` - select attributes by their score/rank/weights, depending on the cutoff that may be specified by the percentage of the highest ranked attributes or by the number of the highest ranked attributes,
- `to_formula()` (misc) - create a `formula` object from a vector.
