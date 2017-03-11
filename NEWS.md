If something is missing or not clear - please chat with us on our [slack](https://fselectorrcpp.slack.com/messages/general/)?

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
