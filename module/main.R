DemoModule <- setRefClass(
    "QuickExploreModule",
    contains = "iNZModule",
    fields = list(
        data = "ANY",
        explore_type = "ANY",
        ctrl_group = "ANY",
        plot_handler = "ANY",
        handle_click = "function",
        updatePlot = "function",
        variables = "character",
        page = "integer",
        click_in_progress = "logical"
    ),
    methods = list(
        initialize = function(gui, which = 1L, ...) {
            callSuper(gui, ...)

            initFields(
                data = get_data(),
                ctrl_group = NULL,
                variables = names(get_data()),
                handle_click = function() {},
                updatePlot = function() {},
                click_in_progress = FALSE
            )

            explore_type <<- gcombobox(
                c("Univariate", "Bivariate", "Pairs", "Missing Values"),
                handler = function(h, ...) {
                    switch(h$obj$get_value(),
                        Univariate = body_univariate(),
                        Bivariate = body_bivariate(),
                        Pairs = body_pairs(),
                        "Missing Values" = body_missing_values()
                    )
                }
            )
            add_body(explore_type)
            body_univariate()

            plot_handler <<- addHandlerClicked(
                GUI$plotWidget$plotNb$children[[1]],
                function(h, ...) {
                    if (click_in_progress) {
                        return()
                    }
                    click_in_progress <<- TRUE
                    dev.hold()
                    on.exit({
                        click_in_progress <<- FALSE
                        dev.flush()
                    })
                    handle_click()
                }
            )
        },
        clear_body = function() {
            if (!is.null(ctrl_group)) {
                .self$modwin$body$remove_child(ctrl_group)
            }
            ctrl_group <<- gvbox()
            add_body(ctrl_group)

            while ("Text Info" %in% names(GUI$plotWidget$plotNb)) {
                ind <- which(names(GUI$plotWidget$plotNb) == "Text Info")
                GUI$plotWidget$plotNb$remove_page_by_index(ind)
            }

            updatePlot <<- function() {}
            handle_click <<- function() {}
        },
        body_univariate = function() {
            clear_body()
            visible(ctrl_group) <<- FALSE
            on.exit(visible(ctrl_group) <<- TRUE)

            glabel("Explore all 1 variable plots",
                container = ctrl_group,
                anchor = c(-1, 0)
            )

            page <<- 1L

            addSpace(ctrl_group, 10)

            ctrl_tbl <- glayout(container = ctrl_group)

            # plot grid size
            # [1x1, 2x2, 3x3, 4x4]
            # grid_size <- gradio(
            #     c("1x1", "2x2", "3x3", "4x4"),
            #     selected = 1L,
            #     horizontal = TRUE,
            #     handler = function(h, ...) updatePlot()
            # )

            # ctrl_tbl[1L, 1L] <- glabel("Grid size: ")
            # ctrl_tbl[1L, 2L, expand = TRUE] <- grid_size


            tbl <- glayout(container = ctrl_group)
            tbl[1L, 1L] <- glabel("Variable: ")

            var_lbl <- glabel(variables[page])
            tbl[1L, 2L] <- var_lbl

            tbl[2, 1:2] <- glabel("Click the plot to change variable")

            updatePlot <<- function() {
                eval(parse(
                    text = sprintf(
                        "inzplot(~ %s, data = get_data())",
                        variables[page]
                    )
                ))

                return()
                # n <- grid_size$get_index()
                # n_plot <- n^2
                # var_index <- seq(
                #     (page - 1L) * n_plot + 1L,
                #     min(page * n_plot, length(variables))
                # )

                # plots <- lapply(var_index, function(i) {
                #     ggplot2::ggplot(
                #         data,
                #         ggplot2::aes(x = .data[[variables[i]]])
                #     ) +
                #         if (iNZightTools::vartype(data[[i]]) == "factor") {
                #             ggplot2::geom_bar()
                #         } else {
                #             ggplot2::geom_histogram(
                #                 stat = "count",
                #                 bins = 30L
                #             )
                #         }
                # })
                # print(
                #     Reduce("+", plots) +
                #         patchwork::plot_layout(ncol = n, nrow = n)
                # )
            }

            handle_click <<- function() {
                page <<- page + 1L
                n_plot <- 1L # grid_size$get_index()^2
                n_page <- length(variables) # ceiling(length(variables) / n_plot)
                if (page > n_page) page <<- 1L

                svalue(var_lbl) <- variables[page]

                updatePlot()
            }

            updatePlot()
        },
        body_bivariate = function() {
            clear_body()

            glabel("Explore all 2 variable plots",
                container = ctrl_group,
                anchor = c(-1, 0)
            )

            page <<- 2L

            v1 <- gcombobox(
                variables,
                container = ctrl_group,
                handler = function(h, ...) {
                    page <<- ifelse(h$obj$get_index() == 1L, 2L, 1L)
                    updatePlot()
                }
            )

            addSpace(ctrl_group, 10)

            tbl <- glayout(container = ctrl_group)
            tbl[1L, 1L] <- glabel("Variable: ")

            var_lbl <- glabel(variables[page])
            tbl[1L, 2L] <- var_lbl

            tbl[2, 1:2] <- glabel("Click the plot to change variable")

            updatePlot <<- function() {
                eval(parse(
                    text = sprintf(
                        "inzplot(%s ~ %s, data = get_data())",
                        svalue(v1),
                        variables[page]
                    )
                ))
            }

            handle_click <<- function() {
                page <<- page + 1L
                if (page == v1$get_index()) page <<- page + 1L
                n_plot <- 1L
                n_page <- length(variables) - 1
                if (page > n_page) page <<- 1L
                updatePlot()
            }

            updatePlot()
        },
        body_pairs = function() {
            clear_body()

            glabel("Pairs plot",
                container = ctrl_group,
                anchor = c(-1, 0)
            )

            addSpace(ctrl_group, 10)

            vars <- gtable(
                variables,
                multiple = TRUE,
                container = ctrl_group
            )
            addHandlerSelectionChanged(
                vars,
                handler = function(h, ...) {
                    Sys.sleep(0.1)
                    updatePlot()
                }
            )
            size(vars) <- c(-1, 200)


            glabel("Group by: ",
                container = ctrl_group,
                anchor = c(-1, 0)
            )
            grp_var <- gcombobox(
                c("", variables[iNZightTools::vartypes(data) == "cat"]),
                selected = 1L,
                container = ctrl_group,
                handler = function(h, ...) {
                    updatePlot()
                }
            )

            updatePlot <<- function() {
                print(svalue(vars))
                if (length(svalue(vars)) <= 1L) {
                    # blank plot with text
                    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
                    text(
                        1, 1,
                        "Select at least 2 variables (use SHIFT or CTRL)",
                        cex = 2,
                        col = "grey50"
                    )
                    return()
                }

                p <- GGally::ggpairs(
                    data,
                    mapping = if (svalue(grp_var) == "") {
                        NULL
                    } else {
                        ggplot2::aes(colour = .data[[svalue(grp_var)]])
                    },
                    columns = svalue(vars, index = TRUE),
                    progress = FALSE
                )
                dev.hold()
                on.exit(dev.flush())
                print(p)
            }

            updatePlot()
        },
        body_missing_values = function() {
            clear_body()

            glabel("Missing values",
                container = ctrl_group,
                anchor = c(-1, 0)
            )

            iNZightMR::plotcombn(data)

            miss_text <- capture.output(
                print(
                    iNZightMR::calcmissing(data)
                )
            )
            miss_info <- gtext(
                text = paste(miss_text, collapse = "\n"),
                expand = TRUE,
                wrap = FALSE,
                font.attr = list(family = "monospace")
            )

            if (GUI$popOut) {
                w <- gwindow(parent = GUI$win, size = c(600, 400))
                add(w, miss_info)
            } else {
                add(GUI$plotWidget$plotNb,
                    miss_info,
                    label = "Text Info"
                )
                svalue(GUI$plotWidget$plotNb) <<- 1L
            }
        },
        close = function() {
            clear_body()
            removeHandler(GUI$plotWidget$plotNb$children[[1]], plot_handler)
            callSuper()
        }
    )
)
