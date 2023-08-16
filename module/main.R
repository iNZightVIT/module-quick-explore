DemoModule <- setRefClass(
    "QuickExploreModule",
    contains = "iNZModule",
    fields = list(
        exp_type = "ANY",
    ),
    methods = list(
        initialize = function(gui, which = 1L, ...) {
            callSuper(gui, ...)

            exp_type <<- gcombobox(c("Univariate", "Bivariate", "Pairs"))

            add_body(exp_type)
        }
    )
)
