# GLMMcosinor (development version | rOpenSci review)

* Create a `sigma()` function which gets the dispersion parameter from a given
  `cglmm` model (by calling `glmmTMB::sigma()`).

* Improve introduction to cosinor modelling in the getting-started vignette.

* Fix small typos in vignettes/docs and make plot legend title consistent across
`autoplot()` and `polar_plot()`.

* Rename functions to be `snake_case` and all S3 class names to `camelCase`.

# GLMMcosinor 0.1.0

* First development version of `{GLMMcosinor}` for submission to rOpenSci.

* Includes functions for fitting a cosinor model, similarly to the {cosinor}
R package but using the `{glmmTMB}` modelling framework to allow more 
flexibility in terms of fitting **generalised** linear **mixed** cosinor models.
