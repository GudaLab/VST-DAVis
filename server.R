if (!exists("vstdavis_valid_paths", mode = "function", inherits = TRUE)) {
  vstdavis_valid_paths <- function(x) {
    if (is.null(x) || !length(x)) {
      return(character())
    }
    x <- as.character(x)
    x[!is.na(x) & nzchar(x)]
  }
}

if (!exists("vstdavis_normalize_dir", mode = "function", inherits = TRUE)) {
  vstdavis_normalize_dir <- function(path) {
    path <- vstdavis_valid_paths(path)
    if (!length(path)) {
      return(character())
    }
    normalized <- tryCatch(
      suppressWarnings(normalizePath(path[[1]], winslash = "/", mustWork = FALSE)),
      error = function(e) path[[1]]
    )
    vstdavis_valid_paths(normalized)
  }
}

if (!exists("vstdavis_has_scripts_dir", mode = "function", inherits = TRUE)) {
  vstdavis_has_scripts_dir <- function(path) {
    path <- vstdavis_valid_paths(path)
    if (!length(path)) {
      return(FALSE)
    }
    isTRUE(dir.exists(file.path(path[[1]], "scripts")))
  }
}

if (!exists("vstdavis_find_app_dir", mode = "function", inherits = TRUE)) {
  vstdavis_find_app_dir <- function() {
    frame_paths <- unlist(lapply(sys.frames(), function(frame) {
      vstdavis_valid_paths(frame$ofile)
    }), use.names = FALSE)

    server_paths <- frame_paths[grepl("server\\.R$|global\\.R$", frame_paths, ignore.case = TRUE)]
    candidate_dirs <- vstdavis_valid_paths(dirname(server_paths))
    current_root <- tryCatch(getwd(), error = function(e) character())
    candidate_dirs <- unique(c(candidate_dirs, vstdavis_valid_paths(current_root), "."))

    for (candidate in candidate_dirs) {
      normalized <- vstdavis_normalize_dir(candidate)
      if (length(normalized) && vstdavis_has_scripts_dir(normalized)) {
        return(normalized[[1]])
      }
    }

    fallback <- vstdavis_normalize_dir(vstdavis_valid_paths(current_root))
    if (length(fallback)) fallback[[1]] else "."
  }
}

vstdavis_app_dir <- vstdavis_find_app_dir()

if (!exists("vstdavis_app_file", mode = "function", inherits = TRUE)) {
  vstdavis_app_file <- function(...) {
    root <- vstdavis_valid_paths(vstdavis_app_dir)
    if (!length(root) || !vstdavis_has_scripts_dir(root)) {
      root <- vstdavis_valid_paths(vstdavis_find_app_dir())
    }
    if (!length(root)) {
      root <- "."
    }
    normalizePath(file.path(root, ...), winslash = "/", mustWork = FALSE)
  }
}

if (!exists("vstdavis_safe_getwd", mode = "function", inherits = TRUE)) {
  vstdavis_safe_getwd <- function() {
    current <- tryCatch(getwd(), error = function(e) character())
    current <- vstdavis_valid_paths(current)
    if (length(current) && isTRUE(dir.exists(current[[1]]))) {
      return(normalizePath(current[[1]], winslash = "/", mustWork = TRUE))
    }

    root <- vstdavis_valid_paths(vstdavis_app_dir)
    if (length(root) && isTRUE(dir.exists(root[[1]]))) {
      return(root[[1]])
    }

    tempdir()
  }
}

if (!exists("vstdavis_restore_wd", mode = "function", inherits = TRUE)) {
  vstdavis_restore_wd <- function(path) {
    path <- vstdavis_valid_paths(path)
    if (length(path) && isTRUE(dir.exists(path[[1]]))) {
      tryCatch(setwd(path[[1]]), error = function(e) NULL)
    }
    invisible(NULL)
  }
}

if (!exists("source_app_script", mode = "function", inherits = TRUE)) {
  source_app_script <- function(script_path, local = parent.frame()) {
    resolved_path <- if (grepl("^[A-Za-z]:|^/", script_path)) script_path else vstdavis_app_file(script_path)
    if (!file.exists(resolved_path)) {
      stop("Required script file not found: ", resolved_path)
    }
    source(resolved_path, local = local)
  }
}

#source("global.R")
server <- function(input, output, session) {
  #Timeout
  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  
    # Increment exactly once when the session first renders
  session$onFlushed(function() {
    current <- tryCatch(increment_count(), error = function(e) NA_integer_)
    output$view_count <- renderText({
      if (is.na(current)) "Unavailable" else format(current, big.mark = ",")
    })
  }, once = TRUE)
  
 ######session info
  
  # --- server ---
  # Show session info in the tab and enable download as .txt
  sess_txt <- reactive({
    paste(capture.output(utils::sessionInfo()), collapse = "\n")
  })
  
  output$sess <- renderPrint({
    cat(sess_txt())
  })
  
  output$download_sess <- downloadHandler(
    filename = function() {
      paste0("VST-DAVis_session-info_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".txt")
    },
    content = function(file) {
      writeLines(sess_txt(), con = file, useBytes = TRUE)
    }
  )

  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  app_root <- vstdavis_app_dir
  current_root <- tryCatch(getwd(), error = function(e) "")
  if (!vstdavis_has_scripts_dir(app_root) && vstdavis_has_scripts_dir(current_root)) {
    app_root <- normalizePath(current_root, winslash = "/", mustWork = TRUE)
  }

  app_file <- function(...) {
    normalizePath(file.path(app_root, ...), winslash = "/", mustWork = FALSE)
  }

  pretty_input_label <- function(input_id) {
    label <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", input_id)
    label <- gsub("_", " ", label, fixed = TRUE)
    label <- gsub("\\s+", " ", trimws(label))
    tools::toTitleCase(label)
  }

  get_call_arg <- function(call_expr, arg_name = NULL, position = NULL) {
    args <- as.list(call_expr)[-1]
    arg_names <- names(args)

    if (!is.null(arg_name) && !is.null(arg_names) && arg_name %in% arg_names) {
      return(args[[match(arg_name, arg_names)]])
    }

    if (!is.null(position) && length(args) >= position) {
      return(args[[position]])
    }

    NULL
  }

  extract_string_literal <- function(expr) {
    if (is.null(expr)) {
      return(NULL)
    }

    if ((is.character(expr) || is.numeric(expr) || is.integer(expr) || is.logical(expr)) && length(expr) >= 1) {
      return(as.character(expr[[1]]))
    }

    if (is.call(expr)) {
      args <- as.list(expr)[-1]
      for (arg in args) {
        val <- extract_string_literal(arg)
        if (!is.null(val) && nzchar(val)) {
          return(val)
        }
      }
    }

    NULL
  }

  flatten_choice_expr <- function(expr) {
    choice_rows <- data.frame(
      label = character(),
      value = character(),
      stringsAsFactors = FALSE
    )

    append_choice <- function(label, value) {
      choice_rows <<- rbind(
        choice_rows,
        data.frame(
          label = as.character(label),
          value = as.character(value),
          stringsAsFactors = FALSE
        )
      )
    }

    walk_choices <- function(node) {
      if (is.null(node)) {
        return(invisible(NULL))
      }

      if (is.atomic(node) && length(node) >= 1) {
        node_values <- as.character(node)
        node_names <- names(node_values)
        if (is.null(node_names)) {
          node_names <- rep("", length(node_values))
        }
        for (i in seq_along(node_values)) {
          display_label <- node_names[[i]]
          if (is.null(display_label) || !nzchar(display_label)) {
            display_label <- node_values[[i]]
          }
          append_choice(display_label, node_values[[i]])
        }
        return(invisible(NULL))
      }

      if (is.call(node) && as.character(node[[1]])[1] %in% c("c", "list")) {
        node_args <- as.list(node)[-1]
        node_names <- names(node_args)
        if (is.null(node_names)) {
          node_names <- rep("", length(node_args))
        }
        for (i in seq_along(node_args)) {
          child <- node_args[[i]]
          child_name <- node_names[[i]]
          if (is.call(child) && as.character(child[[1]])[1] %in% c("c", "list")) {
            walk_choices(child)
          } else {
            child_value <- extract_string_literal(child)
            if (is.null(child_value) || !nzchar(child_value)) {
              child_value <- paste(deparse(child, nlines = 1), collapse = " ")
            }
            child_label <- child_name
            if (is.null(child_label) || !nzchar(child_label)) {
              child_label <- child_value
            }
            append_choice(child_label, child_value)
          }
        }
      }

      invisible(NULL)
    }

    walk_choices(expr)

    unique(choice_rows)
  }

  build_input_metadata <- local({
    cache <- NULL

    function(source_files = c("ui.R", "server.R")) {
      if (!is.null(cache)) {
        return(cache)
      }

      supported_functions <- c(
        "selectInput", "numericInput", "textInput", "textAreaInput",
        "fileInput", "checkboxInput", "checkboxGroupInput", "radioButtons",
        "sliderInput", "dateInput", "dateRangeInput", "pickerInput",
        "passwordInput"
      )

      metadata <- list()

      register_call <- function(call_expr) {
        fn_name <- as.character(call_expr[[1]])[1]
        fn_name <- sub("^.*::", "", fn_name)
        if (!fn_name %in% supported_functions) {
          return(invisible(NULL))
        }

        input_id <- extract_string_literal(get_call_arg(call_expr, "inputId", 1))
        if (is.null(input_id) || !nzchar(input_id)) {
          return(invisible(NULL))
        }

        label_text <- extract_string_literal(get_call_arg(call_expr, "label", 2)) %||% pretty_input_label(input_id)
        choice_rows <- flatten_choice_expr(get_call_arg(call_expr, "choices", 3))

        metadata[[input_id]] <<- list(
          label = label_text,
          choices = choice_rows
        )

        invisible(NULL)
      }

      walk_calls <- function(node) {
        if (is.call(node)) {
          register_call(node)
          lapply(as.list(node)[-1], walk_calls)
        } else if (is.expression(node) || is.pairlist(node) || is.list(node)) {
          lapply(as.list(node), walk_calls)
        }
      }

      for (source_file in source_files) {
        source_path <- if (grepl("^[A-Za-z]:|^/", source_file)) source_file else app_file(source_file)
        parsed_source <- tryCatch(parse(file = source_path, keep.source = FALSE), error = function(e) NULL)
        if (!is.null(parsed_source)) {
          walk_calls(parsed_source)
        }
      }

      cache <<- metadata
      metadata
    }
  })

  input_metadata <- build_input_metadata()
  input_metadata$s_gsea6 <- list(
    label = "Collection (from MSigDB)",
    choices = unique(rbind(
      flatten_choice_expr(quote(c(
        "Hallmark gene sets (H)" = "H",
        "Positional gene sets (C1)" = "C1",
        "Curated gene sets (C2)" = "C2",
        "Regulatory target gene sets (C3)" = "C3",
        "Computational gene sets (C4)" = "C4",
        "Ontology gene sets (C5)" = "C5",
        "Oncogenic signature gene sets (C6)" = "C6",
        "Immunologic signature gene sets (C7)" = "C7",
        "Cell type signature gene sets (C8)" = "C8",
        "Computational perturbation signature gene sets (C9)" = "C9"
      ))),
      flatten_choice_expr(quote(c(
        "Mouse hallmark gene sets (MH)" = "MH",
        "Mouse positional gene sets (M1)" = "M1",
        "Mouse curated gene sets (M2)" = "M2",
        "Mouse regulatory target gene sets (M3)" = "M3",
        "Mouse ontology gene sets (M5)" = "M5",
        "Mouse immunologic signature gene sets (M7)" = "M7",
        "Mouse cell type signature gene sets (M8)" = "M8"
      )))
    ))
  )

  run_log_counter <- reactiveVal(0L)
  run_log_state <- reactiveValues(
    entries = data.frame(
      run_id = character(),
      started_at = character(),
      finished_at = character(),
      section = character(),
      action = character(),
      status = character(),
      parameters = character(),
      message = character(),
      stringsAsFactors = FALSE
    ),
    current = list(
      section = "Idle",
      action = "Waiting for user input",
      status = "Idle",
      detail = "No analysis is currently running.",
      progress = 0,
      started_at = "",
      finished_at = "",
      params_text = "No parameters captured yet."
    )
  )

  format_run_timestamp <- function(x = Sys.time()) {
    format(x, "%Y-%m-%d %H:%M:%S")
  }

  format_run_value <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return("")
    }

    if (inherits(x, "POSIXt")) {
      return(format_run_timestamp(x))
    }

    if (is.data.frame(x)) {
      if ("name" %in% colnames(x)) {
        file_names <- as.character(x$name)
        file_names <- file_names[!is.na(file_names) & nzchar(file_names)]
        return(paste(file_names, collapse = ", "))
      }
      return(sprintf("<data.frame: %d x %d>", nrow(x), ncol(x)))
    }

    if (is.list(x) && !is.atomic(x)) {
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
    }

    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]

    if (!length(x)) {
      return("")
    }

    if (length(x) > 12) {
      return(paste0(paste(head(x, 12), collapse = ", "), " ... (", length(x), " values)"))
    }

    paste(x, collapse = ", ")
  }

  format_choice_value <- function(x, choice_rows) {
    if (is.null(choice_rows) || !nrow(choice_rows)) {
      return(format_run_value(x))
    }

    if (is.data.frame(x) || inherits(x, "POSIXt")) {
      return(format_run_value(x))
    }

    if (is.list(x) && !is.atomic(x)) {
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
    }

    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]

    if (!length(x)) {
      return("")
    }

    choice_lookup <- stats::setNames(choice_rows$label, choice_rows$value)
    display_values <- unname(choice_lookup[x])
    missing_values <- is.na(display_values) | !nzchar(display_values)
    display_values[missing_values] <- x[missing_values]

    if (length(display_values) > 12) {
      return(paste0(paste(head(display_values, 12), collapse = ", "), " ... (", length(display_values), " values)"))
    }

    paste(display_values, collapse = ", ")
  }

  compact_run_params <- function(params) {
    if (!length(params)) {
      return(list())
    }

    keep <- vapply(params, function(x) {
      !(is.null(x) || length(x) == 0 || identical(x, ""))
    }, logical(1))

    params[keep]
  }

  format_run_params <- function(params) {
    params <- compact_run_params(params)

    if (!length(params)) {
      return("No parameters captured.")
    }

    format_param_line <- function(param_id, param_value) {
      meta <- input_metadata[[param_id]]
      display_label <- meta$label %||% pretty_input_label(param_id)
      display_value <- format_choice_value(param_value, meta$choices %||% NULL)
      sprintf("%s: %s", display_label, display_value)
    }

    paste(vapply(names(params), function(param_id) {
      format_param_line(param_id, params[[param_id]])
    }, character(1)), collapse = "\n")
  }

  capture_run_inputs <- function(prefixes = character()) {
    all_inputs <- isolate(reactiveValuesToList(input, all.names = TRUE))
    input_ids <- names(all_inputs)

    matched_ids <- unique(unlist(lapply(prefixes, function(prefix) {
      input_ids[startsWith(input_ids, prefix)]
    }), use.names = FALSE))

    if (!length(matched_ids)) {
      return(list())
    }

    matched_ids <- matched_ids[!grepl("^(download_|info_btn|timeOut$|recalc$)", matched_ids)]
    matched_ids <- matched_ids[!grepl("_plot_(height|width|dpi|type)$", matched_ids)]
    matched_ids <- matched_ids[!grepl("_downloadoutput$", matched_ids)]

    compact_run_params(all_inputs[matched_ids])
  }

  append_run_log_entry <- function(section, action, status = "Running", params = list(), message = "") {
    next_id <- run_log_counter() + 1L
    run_log_counter(next_id)
    run_id <- sprintf("RUN-%04d", next_id)
    params_text <- format_run_params(params)
    started_at <- format_run_timestamp()

    new_entry <- data.frame(
      run_id = run_id,
      started_at = started_at,
      finished_at = "",
      section = as.character(section),
      action = as.character(action),
      status = as.character(status),
      parameters = params_text,
      message = as.character(message),
      stringsAsFactors = FALSE
    )

    run_log_state$entries <- rbind(new_entry, run_log_state$entries)
    rownames(run_log_state$entries) <- NULL

    run_id
  }

  update_run_log_entry <- function(run_id, status, message = NULL) {
    idx <- match(run_id, run_log_state$entries$run_id)
    if (is.na(idx)) {
      return(invisible(NULL))
    }

    run_log_state$entries$status[idx] <- status
    run_log_state$entries$finished_at[idx] <- format_run_timestamp()

    if (!is.null(message) && nzchar(message)) {
      run_log_state$entries$message[idx] <- message
    }
  }

  set_current_run <- function(section = "Idle",
                              action = "Waiting for user input",
                              status = "Idle",
                              detail = "No analysis is currently running.",
                              progress = 0,
                              started_at = "",
                              finished_at = "",
                              params_text = "No parameters captured yet.") {
    run_log_state$current <- list(
      section = section,
      action = action,
      status = status,
      detail = detail,
      progress = progress,
      started_at = started_at,
      finished_at = finished_at,
      params_text = params_text
    )
  }

  format_analysis_error <- function(e) {
    error_message <- conditionMessage(e)
    if (is.null(error_message) || !length(error_message) || !nzchar(error_message[[1]])) {
      return("Unknown analysis error.")
    }

    error_message <- paste(error_message, collapse = "\n")
    if (grepl("required R package installation appears to be corrupt|installed igraph package.*corrupt", error_message, ignore.case = TRUE)) {
      return(error_message)
    }

    package_corruption_patterns <- c(
      "lazy-load database",
      "\\.rdb.*corrupt",
      "internal error -3 in R_decompress1",
      "package or namespace load failed"
    )

    if (any(vapply(package_corruption_patterns, grepl, logical(1), x = error_message, ignore.case = TRUE))) {
      package_name <- ""
      if (grepl("lazy-load database '", error_message, fixed = TRUE)) {
        database_path <- sub(".*lazy-load database '([^']+)'.*", "\\1", error_message)
        package_name <- basename(dirname(dirname(gsub("\\\\", "/", database_path))))
      }

      package_hint <- if (nzchar(package_name)) {
        paste0("reinstall the '", package_name, "' package with install.packages('", package_name, "')")
      } else {
        "reinstall the package reported in the original error"
      }

      return(paste0(
        "A required R package installation appears to be corrupt on this server. ",
        "Restart the R session and ", package_hint, ", then rerun the analysis.\n",
        "Original error: ", error_message
      ))
    }

    error_message
  }

  run_logged_analysis <- function(section, action, params = list(), expr) {
    expr_sub <- substitute(expr)
    expr_env <- parent.frame()
    params <- compact_run_params(params)
    params_text <- format_run_params(params)
    run_id <- append_run_log_entry(section, action, status = "Running", params = params, message = "Run started.")
    started_at <- run_log_state$entries$started_at[match(run_id, run_log_state$entries$run_id)]
    log_messages <- character(0)

    set_current_run(
      section = section,
      action = action,
      status = "Running",
      detail = "Initializing analysis...",
      progress = 5,
      started_at = started_at,
      finished_at = "",
      params_text = params_text
    )

    withProgress(message = paste(section, action, sep = " - "), detail = "Initializing analysis...", value = 0, {
      tryCatch(
        {
          incProgress(0.15, detail = "Loading inputs and scripts...")
          set_current_run(section, action, "Running", "Loading inputs and scripts...", 15, started_at, "", params_text)

          incProgress(0.2, detail = "Running analysis...")
          set_current_run(section, action, "Running", "Running analysis...", 35, started_at, "", params_text)

          result <- withCallingHandlers(
            eval(expr_sub, envir = expr_env),
            message = function(m) {
              log_messages <<- c(log_messages, paste("Message:", conditionMessage(m)))
            },
            warning = function(w) {
              log_messages <<- c(log_messages, paste("Warning:", conditionMessage(w)))
            }
          )

          incProgress(0.85, detail = "Analysis completed successfully.")
          completion_message <- "Completed successfully."
          if (length(log_messages)) {
            completion_message <- paste(completion_message, paste(head(unique(log_messages), 20), collapse = "\n"), sep = "\n")
          }

          update_run_log_entry(run_id, "Completed", completion_message)
          set_current_run(section, action, "Completed", "Analysis completed successfully.", 100, started_at, format_run_timestamp(), params_text)
          result
        },
        error = function(e) {
          failure_message <- format_analysis_error(e)
          if (length(log_messages)) {
            failure_message <- paste(failure_message, paste(head(unique(log_messages), 20), collapse = "\n"), sep = "\n")
          }
          update_run_log_entry(run_id, "Failed", failure_message)
          set_current_run(section, action, "Failed", failure_message, 100, started_at, format_run_timestamp(), params_text)
          stop(failure_message, call. = FALSE)
        }
      )
    })
  }

  format_download_size <- function(size_bytes) {
    if (is.null(size_bytes) || !length(size_bytes) || !is.finite(size_bytes) || size_bytes < 0) {
      return("")
    }

    units <- c("B", "KB", "MB", "GB", "TB")
    unit_index <- 1L
    scaled_size <- as.numeric(size_bytes)

    while (scaled_size >= 1024 && unit_index < length(units)) {
      scaled_size <- scaled_size / 1024
      unit_index <- unit_index + 1L
    }

    paste0(format(round(scaled_size, 2), trim = TRUE, scientific = FALSE), " ", units[[unit_index]])
  }

  create_object_download_handler <- function(section, action, filename_text, object_expr) {
    object_sub <- substitute(object_expr)
    object_env <- parent.frame()

    downloadHandler(
      filename = function() {
        filename_text
      },
      content = function(file) {
        params <- list(download_file = filename_text)
        params_text <- paste0("Download file: ", filename_text)
        run_id <- append_run_log_entry(
          section = section,
          action = action,
          status = "Running",
          params = params,
          message = "Preparing download."
        )
        started_at <- run_log_state$entries$started_at[match(run_id, run_log_state$entries$run_id)]

        set_current_run(
          section = section,
          action = action,
          status = "Running",
          detail = "Preparing object for download...",
          progress = 5,
          started_at = started_at,
          finished_at = "",
          params_text = params_text
        )

        withProgress(message = paste(section, action, sep = " - "), detail = "Preparing object for download...", value = 0, {
          tryCatch(
            {
              incProgress(0.2, detail = "Collecting object from memory...")
              set_current_run(section, action, "Running", "Collecting object from memory...", 20, started_at, "", params_text)

              object_to_save <- eval(object_sub, envir = object_env)
              if (is.null(object_to_save)) {
                stop("No object is available to download yet.")
              }

              incProgress(0.55, detail = "Writing compressed RDS file...")
              set_current_run(section, action, "Running", "Writing compressed RDS file...", 75, started_at, "", params_text)
              saveRDS(object_to_save, file = file, compress = TRUE)

              file_size <- tryCatch(file.info(file)$size, error = function(e) NA_real_)
              size_label <- format_download_size(file_size)
              completion_detail <- if (nzchar(size_label)) {
                paste0("Download completed successfully (", size_label, ").")
              } else {
                "Download completed successfully."
              }

              incProgress(0.25, detail = "Download is ready.")
              update_run_log_entry(run_id, "Completed", completion_detail)
              set_current_run(section, action, "Completed", completion_detail, 100, started_at, format_run_timestamp(), params_text)
            },
            error = function(e) {
              failure_message <- paste0("Download failed: ", conditionMessage(e))
              update_run_log_entry(run_id, "Failed", failure_message)
              set_current_run(section, action, "Failed", failure_message, 100, started_at, format_run_timestamp(), params_text)
              stop(e)
            }
          )
        })
      }
    )
  }

  bulk_download_state <- reactiveValues(
    status = "Idle",
    detail = "No bulk download has started.",
    progress = 0,
    completed_at = ""
  )

  set_bulk_download_state <- function(status = "Idle", detail = "", progress = 0, completed_at = "") {
    progress <- suppressWarnings(as.numeric(progress))
    if (!is.finite(progress)) {
      progress <- 0
    }
    bulk_download_state$status <- status
    bulk_download_state$detail <- detail
    bulk_download_state$progress <- max(0, min(100, round(progress)))
    bulk_download_state$completed_at <- completed_at
  }

  normalize_bulk_image_format <- function(image_format) {
    image_format <- as.character(image_format %||% ".jpg")[[1]]
    if (!nzchar(image_format)) {
      image_format <- ".jpg"
    }
    if (!startsWith(image_format, ".")) {
      image_format <- paste0(".", image_format)
    }
    image_format <- tolower(image_format)
    allowed_formats <- c(".png", ".jpg", ".jpeg", ".tif", ".tiff", ".pdf", ".svg", ".bmp", ".eps", ".ps")
    if (!image_format %in% allowed_formats) {
      image_format <- ".jpg"
    }
    image_format
  }

  sanitize_bulk_component <- function(x, fallback = "item") {
    x <- as.character(x %||% fallback)[[1]]
    x <- gsub("[/\\\\:*?\"<>|]+", "_", x)
    x <- gsub("\\s+", " ", trimws(x))
    if (!nzchar(x)) {
      x <- fallback
    }
    x
  }

  sanitize_bulk_file_stem <- function(x, fallback = "item") {
    x <- sanitize_bulk_component(x, fallback)
    x <- gsub("[^A-Za-z0-9._-]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    if (!nzchar(x)) {
      x <- fallback
    }
    x
  }

  get_analysis_pdf_path <- function(pdf_name, root_dir = getwd()) {
    if (is.null(pdf_name) || !nzchar(pdf_name)) {
      return(NULL)
    }

    if (file.exists(pdf_name)) {
      return(normalizePath(pdf_name, winslash = "/", mustWork = FALSE))
    }

    candidate_paths <- c(
      file.path(root_dir %||% getwd(), pdf_name),
      file.path(root_dir %||% getwd(), "www", pdf_name),
      file.path(getwd(), "www", pdf_name),
      file.path(getwd(), pdf_name)
    )
    candidate_paths <- unique(candidate_paths)
    existing_paths <- candidate_paths[file.exists(candidate_paths)]
    if (length(existing_paths) == 0) {
      return(candidate_paths[[1]])
    }

    existing_paths[[1]]
  }

  www_url <- function(...) {
    parts <- vapply(list(...), URLencode, character(1), reserved = TRUE)
    paste(parts, collapse = "/")
  }

  local_resource_url <- function(file_path, prefix_seed = "vstdavis_resource") {
    resource_dir <- dirname(file_path)
    resource_id <- paste(basename(dirname(resource_dir)), basename(resource_dir), sep = "_")
    resource_prefix <- sanitize_bulk_file_stem(
      paste(prefix_seed, resource_id, sep = "_"),
      "vstdavis_resource"
    )
    shiny::addResourcePath(resource_prefix, resource_dir)
    www_url(resource_prefix, basename(file_path))
  }

  render_pdf_preview <- function(pdf_name, root_dir = getwd(), title = "PDF preview", height = "650px") {
    pdf_path <- get_analysis_pdf_path(pdf_name, root_dir)
    if (is.null(pdf_path) || !file.exists(pdf_path)) {
      return(shiny::tags$p("PDF file not found. Please rerun the analysis."))
    }

    pdf_mtime <- as.integer(file.info(pdf_path)$mtime)
    pdf_url <- paste0(local_resource_url(pdf_path, "hdwgcna_pdf"), "?v=", pdf_mtime)
    preview_dir <- file.path(dirname(pdf_path), "hdwgcna_pdf_previews")
    dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)

    preview_tags <- NULL
    if (requireNamespace("pdftools", quietly = TRUE)) {
      preview_tags <- tryCatch(
        {
          pdf_info <- pdftools::pdf_info(pdf_path)
          page_count <- max(1L, as.integer(pdf_info$pages %||% 1L))
          pdf_stem <- sanitize_bulk_file_stem(tools::file_path_sans_ext(basename(pdf_path)), "pdf_preview")
          preview_files <- file.path(preview_dir, sprintf("%s_page_%03d.png", pdf_stem, seq_len(page_count)))
          preview_stale <- !all(file.exists(preview_files)) ||
            any(file.info(preview_files[file.exists(preview_files)])$mtime < file.info(pdf_path)$mtime)

          if (preview_stale) {
            unlink(Sys.glob(file.path(preview_dir, paste0(pdf_stem, "_page_*.png"))))
            preview_files <- pdftools::pdf_convert(
              pdf = pdf_path,
              format = "png",
              pages = seq_len(page_count),
              filenames = preview_files,
              dpi = 144,
              verbose = FALSE
            )
          }

          preview_files <- preview_files[file.exists(preview_files)]
          if (length(preview_files) == 0) {
            return(NULL)
          }

          shiny::tags$div(
            lapply(seq_along(preview_files), function(page_index) {
              preview_file <- preview_files[[page_index]]
              preview_url <- paste0(
                local_resource_url(preview_file, "hdwgcna_pdf_preview"),
                "?v=",
                as.integer(file.info(preview_file)$mtime)
              )
              shiny::tagList(
                if (length(preview_files) > 1) {
                  shiny::tags$p(shiny::tags$b(paste("Page", page_index)))
                },
                shiny::tags$img(
                  src = preview_url,
                  alt = paste(title, "page", page_index),
                  style = "width:100%; max-width:1100px; border:1px solid #d9d9d9; margin-bottom:14px; background-color:#ffffff;"
                )
              )
            })
          )
        },
        error = function(e) NULL
      )
    }

    shiny::tagList(
      shiny::tags$div(
        style = "margin: 8px 0 12px 0;",
        shiny::tags$a(href = pdf_url, target = "_blank", "Open PDF in new tab"),
        shiny::tags$span(" | "),
        shiny::tags$a(href = pdf_url, download = basename(pdf_path), "Download PDF")
      ),
      if (!is.null(preview_tags)) {
        preview_tags
      } else {
        shiny::tags$object(
          data = pdf_url,
          type = "application/pdf",
          style = paste0("width:100%; height:", height, "; border:1px solid #d9d9d9; background-color:#ffffff;"),
          shiny::tags$a(href = pdf_url, target = "_blank", "Open PDF in new tab")
        )
      }
    )
  }

  draw_hdwgcna_shiny_plot <- function(plot_obj) {
    if (inherits(plot_obj, "recordedplot")) {
      grDevices::replayPlot(plot_obj)
      return(invisible(NULL))
    }

    if (inherits(plot_obj, c("gg", "ggplot", "patchwork", "trellis"))) {
      print(plot_obj)
      return(invisible(NULL))
    }

    if (inherits(plot_obj, "pheatmap") && !is.null(plot_obj$gtable)) {
      grid::grid.newpage()
      grid::grid.draw(plot_obj$gtable)
      return(invisible(NULL))
    }

    if (inherits(plot_obj, c("grob", "gtable"))) {
      grid::grid.newpage()
      grid::grid.draw(plot_obj)
      return(invisible(NULL))
    }

    if (inherits(plot_obj, c("Heatmap", "HeatmapList")) && requireNamespace("ComplexHeatmap", quietly = TRUE)) {
      ComplexHeatmap::draw(plot_obj)
      return(invisible(NULL))
    }

    if (is.list(plot_obj) && !is.data.frame(plot_obj)) {
      plot_candidates <- Filter(function(x) {
        inherits(x, c("gg", "ggplot", "patchwork", "trellis", "grob", "gtable", "recordedplot"))
      }, plot_obj)
      if (length(plot_candidates)) {
        if (all(vapply(plot_candidates, inherits, logical(1), what = c("gg", "ggplot", "patchwork")))) {
          print(patchwork::wrap_plots(plot_candidates))
        } else {
          invisible(lapply(plot_candidates, draw_hdwgcna_shiny_plot))
        }
        return(invisible(NULL))
      }
    }

    print(plot_obj)
    invisible(NULL)
  }

  bulk_group <- function(folder, trigger, result, labels = character(), sizes = list(), pdf_files = character()) {
    list(
      folder = as.character(folder),
      trigger = trigger,
      result = result,
      labels = labels,
      sizes = sizes,
      pdf_files = pdf_files
    )
  }

  bulk_download_groups <- function() {
    bulk_group_list <- list(
      bulk_group(
        folder = c("Single or Multiple Samples Analysis", "Stats"),
        trigger = "multiple_sample_submit",
        result = function() datainput_multiple_sample_level(),
        labels = c(
          plot1 = "QC_before_filtering",
          Plot2 = "spatial_feature_plot",
          Plot3 = "feature_feature_relationships_plot",
          data1 = "Number_of_cells"
        ),
        sizes = list(plot1 = c(width = 8, height = 12), Plot2 = c(width = 12, height = 12), Plot3 = c(width = 12, height = 5))
      ),
      bulk_group(
        folder = c("Single or Multiple Samples Analysis", "Sample Groups and QC Filtering"),
        trigger = "multiple_sample_qc_filtering",
        result = function() datainput_multiple_qc_filter_level(),
        labels = c(
          plot1 = "QC_after_filtering_sample_based",
          plot2 = "QC_after_filtering_group_based",
          plot3 = "Bar_plot_sample_based",
          plot4 = "Bar_plot_group_based",
          plot5 = "Spatial_plot_after_filtering",
          data1 = "Cell_count_after_QC",
          data2 = "Cell_count_by_group_after_QC"
        ),
        sizes = list(
          plot1 = c(width = 8, height = 12),
          plot2 = c(width = 5, height = 12),
          plot3 = c(width = 8, height = 10),
          plot4 = c(width = 5, height = 8),
          plot5 = c(width = 8, height = 12)
        )
      ),
      bulk_group(
        folder = c("Single or Multiple Samples Analysis", "Normalization and PCA Analysis"),
        trigger = "multiple_sample_normalization",
        result = function() datainput_multiple_normalization_pca_level(),
        labels = c(plot1 = "After_normalization_PCA_heatmap", plot2 = "After_normalization_elbow_plot", plot3 = "PCA_plot_sample_based", plot4 = "PCA_plot_group_based"),
        sizes = list(plot1 = c(width = 8, height = 5), plot2 = c(width = 8, height = 5), plot3 = c(width = 8, height = 5), plot4 = c(width = 8, height = 5))
      ),
      bulk_group(
        folder = c("Single or Multiple Samples Analysis", "Clustering"),
        trigger = "multiple_sample_clustering",
        result = function() datainput_multiple_clustering_level(),
        labels = c(
          plot1 = "Cluster_plot",
          plot2 = "Cluster_based_bar_plot",
          plot3 = "Condition_based_plot",
          plot4 = "Condition_based_bar_plot",
          plot5 = "Sample_based_plot",
          plot6 = "Sample_based_bar_plot",
          plot7 = "Spatial_plot",
          plot8 = "Spatial_plot_split_by_clusters",
          plot9 = "Cluster_based_plot_split_by_condition",
          plot10 = "Cluster_based_plot_split_by_samples",
          plot11 = "Cluster_split_by_condition",
          data1 = "Number_of_cells_in_clusters",
          data2 = "Number_of_cells_in_clusters_based_on_condition",
          data3 = "Number_of_cells_in_clusters_based_on_samples"
        ),
        sizes = list(
          plot1 = c(width = 8, height = 8),
          plot2 = c(width = 8, height = 8),
          plot3 = c(width = 8, height = 8),
          plot4 = c(width = 8, height = 8),
          plot5 = c(width = 8, height = 8),
          plot6 = c(width = 8, height = 8),
          plot7 = c(width = 12, height = 8),
          plot8 = c(width = 12, height = 20),
          plot9 = c(width = 20, height = 8),
          plot10 = c(width = 20, height = 8),
          plot11 = c(width = 20, height = 8)
        )
      ),
      bulk_group(
        folder = c("Single or Multiple Samples Analysis", "Markers Identification"),
        trigger = "multiple_sample_marker",
        result = function() datainput_multiple_marker_level(),
        labels = c(plot1 = "Heatmap_with_Top5_expressed_genes", data1 = "Identified_markers_or_differentially_expressed_genes"),
        sizes = list(plot1 = c(width = 12, height = 8))
      ),
      bulk_group(
        folder = c("Single or Multiple Samples Analysis", "Cell Type Prediction"),
        trigger = "multiple_sample_celltype",
        result = function() datainput_multiple_celltype_level(),
        labels = c(plot1 = "Dimplot_with_celltype", plot2 = "Spatial_Dimplot_with_celltype", plot3 = "score_plot", plot4 = "delta_distribution_plot", table1 = "predicted_celltype_scores"),
        sizes = list(plot1 = c(width = 16, height = 8), plot2 = c(width = 16, height = 8), plot3 = c(width = 20, height = 8), plot4 = c(width = 8, height = 8))
      ),
      bulk_group(
        folder = c("Single or Multiple Samples Analysis", "Cluster-Based Plots"),
        trigger = "multiple_sample_clusterbased",
        result = function() datainput_multiple_clusterbased_level(),
        labels = c(plot1 = "Plots_for_top_or_selected_markers", data2 = "Top_or_selected_Cell_counts_proportion"),
        sizes = list(plot1 = c(width = 20, height = 20))
      ),
      bulk_group(
        folder = c("Single or Multiple Samples Analysis", "Condition Based Analysis"),
        trigger = "multiple_sample_conditionbased",
        result = function() datainput_multiple_conditionbased_level(),
        labels = c(plot1 = "Plots_for_top_selected_markers", data1 = "Differentially_expressed_genes_sample_based"),
        sizes = list(plot1 = c(width = 20, height = 20))
      ),
      bulk_group(
        folder = c("Subclustering", "Stats"),
        trigger = "subclustering_multiple_sample_submit",
        result = function() datainput_subclustering_multiple_sample_level(),
        labels = c(plot = "QC_for_the_selected_subclusters", plot2 = "QC_for_the_selected_subclusters_with_spatial_image", data1 = "Number_of_cells"),
        sizes = list(plot = c(width = 5, height = 8), plot2 = c(width = 12, height = 6))
      ),
      bulk_group(
        folder = c("Subclustering", "Normalization and PCA Analysis"),
        trigger = "subclustering_multiple_sample_normalization",
        result = function() datainput_subclustering_multiple_normalization_pca_level(),
        labels = c(plot1 = "After_normalization_PCA_heatmap", plot2 = "After_normalization_elbow_plot", plot3 = "After_normalization_PCA_plot_sample_based", plot4 = "After_normalization_PCA_plot_group_based"),
        sizes = list(plot1 = c(width = 8, height = 5), plot2 = c(width = 8, height = 5), plot3 = c(width = 8, height = 5), plot4 = c(width = 8, height = 5))
      ),
      bulk_group(
        folder = c("Subclustering", "Clustering"),
        trigger = "subclustering_multiple_sample_clustering",
        result = function() datainput_subclustering_multiple_clustering_level(),
        labels = c(
          plot1 = "Cluster_plot",
          plot2 = "Cluster_bar_plot",
          plot3 = "Condition_based_plot",
          plot4 = "Condition_based_bar_plot",
          plot5 = "Sample_based_plot",
          plot6 = "Sample_based_bar_plot",
          plot7 = "Sample_based_plot_split_by_clusters",
          plot8 = "Spatial_plot_split_by_clusters",
          plot9 = "Cluster_based_plot_split_by_condition",
          plot10 = "Cluster_based_plot_split_by_samples",
          plot11 = "Cluster_split_by_condition",
          data1 = "Number_of_cells_in_clusters",
          data2 = "Number_of_cells_in_clusters_based_on_condition",
          data3 = "Number_of_cells_in_clusters_based_on_samples"
        ),
        sizes = list(
          plot1 = c(width = 8, height = 8),
          plot2 = c(width = 8, height = 8),
          plot3 = c(width = 8, height = 8),
          plot4 = c(width = 8, height = 8),
          plot5 = c(width = 8, height = 8),
          plot6 = c(width = 8, height = 8),
          plot7 = c(width = 15, height = 12),
          plot8 = c(width = 8, height = 20),
          plot9 = c(width = 20, height = 8),
          plot10 = c(width = 20, height = 8),
          plot11 = c(width = 20, height = 8)
        )
      ),
      bulk_group(
        folder = c("Subclustering", "Markers Identification"),
        trigger = "subclustering_multiple_sample_marker",
        result = function() datainput_subclustering_multiple_marker_level(),
        labels = c(plot1 = "Heatmap_with_Top5_expressed_genes", data1 = "Identified_markers_or_differentially_expressed_genes"),
        sizes = list(plot1 = c(width = 12, height = 8))
      ),
      bulk_group(
        folder = c("Subclustering", "Cell Type Prediction"),
        trigger = "subclustering_multiple_sample_celltype",
        result = function() datainput_subclustering_multiple_celltype_level(),
        labels = c(plot1 = "Dimplot_with_celltype", plot2 = "Spatial_Dimplot_with_celltype", plot3 = "score_plot", plot4 = "delta_distribution_plot", table1 = "predicted_celltype_scores"),
        sizes = list(plot1 = c(width = 16, height = 8), plot2 = c(width = 16, height = 8), plot3 = c(width = 20, height = 8), plot4 = c(width = 8, height = 8))
      ),
      bulk_group(
        folder = c("Subclustering", "Cluster-Based Plots"),
        trigger = "subclustering_multiple_sample_clusterbased",
        result = function() datainput_subclustering_multiple_clusterbased_level(),
        labels = c(plot1 = "Plots_for_top_or_selected_markers", data2 = "Top_or_selected_Cell_counts_proportion"),
        sizes = list(plot1 = c(width = 20, height = 20))
      ),
      bulk_group(
        folder = c("Subclustering", "Condition Based Analysis"),
        trigger = "subclustering_multiple_sample_conditionbased",
        result = function() datainput_subclustering_multiple_conditionbased_level(),
        labels = c(plot1 = "Plots_for_top_selected_markers", data1 = "Differentially_expressed_genes_sample_based"),
        sizes = list(plot1 = c(width = 20, height = 20))
      ),
      bulk_group(
        folder = c("Correlation Network"),
        trigger = "single_multiple_sample_cccn",
        result = function() datainput_single_multiple_sample_cccn_level(),
        labels = c(plot1 = "Cluster_based_correlation_matrix_plot", plot2 = "Cluster_based_correlation_network_plot", data1 = "Cluster_based_correlation_table"),
        sizes = list(plot1 = c(width = 8, height = 8), plot2 = c(width = 8, height = 8))
      ),
      bulk_group(
        folder = c("GO Terms"),
        trigger = "single_multiple_sample_go",
        result = function() datainput_single_multiple_sample_go_level(),
        labels = c(plot1 = "GO_terms_plot", data1 = "GO_terms_summary_table"),
        sizes = list(plot1 = c(width = 8, height = 8))
      ),
      bulk_group(
        folder = c("Pathway Analysis"),
        trigger = "single_multiple_sample_pathway",
        result = function() datainput_single_multiple_sample_pathway_level(),
        labels = c(plot1 = "Pathway_plot", data1 = "Pathway_summary_table"),
        sizes = list(plot1 = c(width = 8, height = 8))
      ),
      bulk_group(
        folder = c("GSEA Analysis"),
        trigger = "single_multiple_sample_gsea",
        result = function() datainput_single_multiple_sample_gsea_level(),
        labels = c(plot1 = "GSEA_plot", data1 = "GSEA_summary_table"),
        sizes = list(plot1 = c(width = 15, height = 10))
      ),
      bulk_group(
        folder = c("Cell-Cell Communication", "Communication Inference"),
        trigger = "single_multiple_sample_cellchat1",
        result = function() datainput_single_multiple_sample_cellchat1_level(),
        labels = c(
          plot1 = "Number_of_interactions_circle_plot",
          plot2 = "Interaction_weights_or_strength_circle_plot",
          plot3 = "Interactions_heatmap",
          plot4 = "Signaling_patterns",
          plot5 = "Communication_patterns",
          data1 = "CellChat_summary_table",
          data3 = "CellChat_signaling_pathways"
        ),
        sizes = list(plot1 = c(width = 8, height = 8), plot2 = c(width = 8, height = 8), plot3 = c(width = 10, height = 8), plot4 = c(width = 10, height = 12), plot5 = c(width = 10, height = 12))
      ),
      bulk_group(
        folder = c("Cell-Cell Communication", "Selected Pathway Visualization"),
        trigger = "single_multiple_sample_cellchat2",
        result = function() datainput_single_multiple_sample_cellchat2_level(),
        labels = c(
          plot1 = "Selected_pathway_circle_plot",
          plot2 = "Selected_pathway_chord_plot",
          plot3 = "Selected_pathway_heatmap",
          plot4 = "Selected_pathway_hierarchy_plot",
          plot5 = "Bubble_plot",
          plot6 = "Network_analysis_contribution_bar_plot",
          plot7 = "Gene_expression_plot",
          plot8 = "Interactions_spatial_plot",
          data1 = "Selected_pathway_interaction_table"
        ),
        sizes = list(plot1 = c(width = 8, height = 8), plot2 = c(width = 8, height = 8), plot3 = c(width = 10, height = 8), plot4 = c(width = 10, height = 8), plot5 = c(width = 10, height = 8), plot6 = c(width = 10, height = 8), plot7 = c(width = 10, height = 10), plot8 = c(width = 8, height = 8))
      ),
      bulk_group(
        folder = c("Trajectory and Pseudotime Analysis", "Build Trajectory"),
        trigger = "single_multiple_sample_trajectory1",
        result = function() datainput_single_multiple_sample_trajectory1_level(),
        labels = c(plot1 = "Trajectory_plot"),
        sizes = list(plot1 = c(width = 8, height = 8))
      ),
      bulk_group(
        folder = c("Trajectory and Pseudotime Analysis", "Pseudotime Ordering"),
        trigger = "single_multiple_sample_trajectory2",
        result = function() datainput_single_multiple_sample_trajectory2_level(),
        labels = c(plot1 = "Cells_in_pseudotime", plot2 = "Cells_ordered_by_Monocle3_pseudotime"),
        sizes = list(plot1 = c(width = 8, height = 8), plot2 = c(width = 16, height = 8))
      ),
      bulk_group(
        folder = c("Trajectory and Pseudotime Analysis", "Pseudotime Feature Analysis"),
        trigger = "single_multiple_sample_trajectory3",
        result = function() datainput_single_multiple_sample_trajectory3_level(),
        labels = c(plot1 = "FeaturePlot_with_pseudotime", plot2 = "Spatial_plot_with_pseudotime", data1 = "Genes_that_change_as_a_function_of_pseudotime"),
        sizes = list(plot1 = c(width = 8, height = 8), plot2 = c(width = 8, height = 8))
      ),
      bulk_group(
        folder = c("Trajectory and Pseudotime Analysis", "Pseudotime Gene Plots"),
        trigger = "single_multiple_sample_trajectory4",
        result = function() datainput_single_multiple_sample_trajectory4_level(),
        labels = c(plot1 = "FeaturePlot_with_pseudotime_for_selected_genes", plot2 = "FeaturePlot_with_pseudotime_for_selected_genes_with_spatial_images"),
        sizes = list(plot1 = c(width = 8, height = 10), plot2 = c(width = 8, height = 10))
      ),
      bulk_group(
        folder = c("Co-expression and TF Analysis", "Co-expression Network Analysis"),
        trigger = "single_multiple_sample_hdwgcna",
        result = function() datainput_single_multiple_sample_hdwgcna_level(),
        labels = c(
          plot801 = "UMAP_plot",
          plot802 = "Soft_power_threshold_plots",
          plot804 = "Module_ranked_by_eigengene_based_connectivity_kME",
          plot805 = "Module_feature_plots",
          plot807 = "Module_with_Seurat_dot_plot",
          plot810 = "Module_feature_plots_with_spatial_image",
          data1 = "Soft_power_threshold_table",
          data2 = "Module_assignment_table",
          data3 = "Top_N_hub_genes"
        ),
        sizes = list(plot801 = c(width = 8, height = 8), plot802 = c(width = 8, height = 8), plot804 = c(width = 8, height = 8), plot805 = c(width = 8, height = 8), plot807 = c(width = 8, height = 8), plot810 = c(width = 8, height = 8)),
        pdf_files = function(result) {
          pdf_dir <- character()
          if (is.list(result)) {
            pdf_dir <- result[["text_summary"]] %||% character()
            if (!length(pdf_dir) && length(result) >= 6) {
              pdf_dir <- result[[6]]
            }
          }

          pdf_dir <- as.character(pdf_dir %||% character())
          pdf_dir <- pdf_dir[!is.na(pdf_dir) & nzchar(pdf_dir)]
          pdf_names <- c(
            result[["dendrogram_file"]] %||% "PlotDendrogram.pdf",
            result[["correlogram_file"]] %||% "PlotModuleCorrelogram.pdf",
            result[["module_networks_file"]] %||% "combined_output.pdf",
            result[["module_umap_file"]] %||% "ModuleUMAPPlot.pdf"
          )
          pdf_names <- as.character(pdf_names)
          pdf_names <- pdf_names[!is.na(pdf_names) & nzchar(pdf_names)]

          if (length(pdf_dir)) {
            file.path(pdf_dir[[1]], pdf_names)
          } else {
            character()
          }
        }
      ),
      bulk_group(
        folder = c("Co-expression and TF Analysis", "Transcription Factor Regulatory Network Analysis"),
        trigger = "single_multiple_sample_tfrn1",
        result = function() datainput_single_multiple_sample_tfrn1_level(),
        labels = c(
          plot1 = "Module_regulatory_network_plot_positive",
          plot2 = "Module_regulatory_network_plot_negative",
          plot3 = "Module_regulatory_network_plot_both",
          plot4 = "Module_regulatory_network_plot_module_UMAP",
          data1 = "TF_network_table"
        ),
        sizes = list(plot1 = c(width = 8, height = 8), plot2 = c(width = 8, height = 8), plot3 = c(width = 8, height = 8), plot4 = c(width = 8, height = 8))
      ),
      bulk_group(
        folder = c("Co-expression and TF Analysis", "Transcription Factor Regulatory Network Analysis", "Selected TF"),
        trigger = "single_multiple_sample_tfrn2",
        result = function() datainput_single_multiple_sample_tfrn2_level(),
        labels = c(
          plot1 = "Feature_plot_of_selected_TF",
          plot2 = "Top_target_genes_within_TF_regulons",
          plot3 = "TF_network_plot_positive",
          plot4 = "TF_network_plot_negative",
          plot5 = "TF_network_plot_both",
          plot6 = "Feature_plot_of_selected_TF_with_spatial_image"
        ),
        sizes = list(plot1 = c(width = 8, height = 8), plot2 = c(width = 8, height = 8), plot3 = c(width = 8, height = 8), plot4 = c(width = 8, height = 8), plot5 = c(width = 8, height = 8), plot6 = c(width = 8, height = 8))
      )
    )

    bulk_group_list
  }

  bulk_trigger_completed <- function(trigger) {
    trigger_value <- input[[trigger]]
    trigger_value <- suppressWarnings(as.numeric(trigger_value %||% 0))
    is.finite(trigger_value) && trigger_value > 0
  }

  bulk_safe_get_result <- function(group) {
    result <- tryCatch(
      isolate(group$result()),
      error = function(e) NULL
    )

    if (!is.null(result) && is.list(result) && length(result)) {
      return(result)
    }

    if (!bulk_trigger_completed(group$trigger)) {
      return(NULL)
    }

    result
  }

  bulk_is_table <- function(x) {
    inherits(x, c("data.frame", "data.table", "tbl_df", "tbl")) || is.matrix(x) || is.table(x)
  }

  bulk_is_plot <- function(x) {
    if (is.null(x) || bulk_is_table(x)) {
      return(FALSE)
    }

    if (inherits(x, c("gg", "ggplot", "patchwork", "grob", "gtable", "recordedplot", "trellis", "Heatmap", "HeatmapList", "pheatmap"))) {
      return(TRUE)
    }

    if (is.list(x) && !is.data.frame(x) && length(x) && length(x) <= 50) {
      return(tryCatch(any(vapply(x, bulk_is_plot, logical(1))), error = function(e) FALSE))
    }

    FALSE
  }

  bulk_named_plot_output <- function(output_name) {
    grepl("^plot", output_name %||% "", ignore.case = TRUE)
  }

  bulk_should_save_plot <- function(output_name, output_object) {
    if (is.null(output_object) || bulk_is_table(output_object)) {
      return(FALSE)
    }

    isTRUE(tryCatch(bulk_is_plot(output_object), error = function(e) FALSE)) || bulk_named_plot_output(output_name)
  }

  bulk_output_label <- function(group, output_name, output_type) {
    labels <- group$labels %||% character()
    if (!is.null(names(labels)) && output_name %in% names(labels)) {
      return(sanitize_bulk_component(labels[[output_name]], fallback = output_name))
    }

    sanitize_bulk_component(paste(output_type, output_name, sep = "_"), fallback = output_type)
  }

  bulk_output_size <- function(group, output_name) {
    size <- group$sizes[[output_name]]
    if (is.null(size)) {
      size <- c(width = 8, height = 8)
    }

    size_names <- names(size) %||% character()
    width_value <- if ("width" %in% size_names) size[["width"]] else if (length(size) >= 1) size[[1]] else 8
    height_value <- if ("height" %in% size_names) size[["height"]] else if (length(size) >= 2) size[[2]] else 8
    width <- suppressWarnings(as.numeric(width_value %||% 8))
    height <- suppressWarnings(as.numeric(height_value %||% 8))
    if (!is.finite(width)) {
      width <- 8
    }
    if (!is.finite(height)) {
      height <- 8
    }
    c(width = max(3, min(49, width)), height = max(3, min(49, height)))
  }

  bulk_plot_panel_count <- function(plot_object) {
    if (inherits(plot_object, "patchwork") && !is.null(plot_object$patches$plots)) {
      return(max(1L, length(plot_object$patches$plots) + 1L))
    }

    if (inherits(plot_object, c("gg", "ggplot", "grob", "gtable", "recordedplot", "trellis", "Heatmap", "HeatmapList", "pheatmap"))) {
      return(1L)
    }

    if (is.list(plot_object) && !is.data.frame(plot_object)) {
      plot_candidates <- Filter(bulk_is_plot, plot_object)
      if (length(plot_candidates)) {
        return(length(plot_candidates))
      }
    }

    1L
  }

  bulk_adjust_plot_size <- function(plot_object, size) {
    panel_count <- bulk_plot_panel_count(plot_object)
    if (panel_count <= 1) {
      return(size)
    }

    columns <- min(3L, ceiling(sqrt(panel_count)))
    rows <- ceiling(panel_count / columns)
    size[["width"]] <- max(size[["width"]], columns * 6)
    size[["height"]] <- max(size[["height"]], rows * 5)
    c(width = max(3, min(49, size[["width"]])), height = max(3, min(49, size[["height"]])))
  }

  bulk_folder_path <- function(root_dir, folder, leaf = NULL) {
    folder_parts <- vapply(folder, sanitize_bulk_component, character(1), fallback = "folder")
    path_parts <- c(root_dir, folder_parts, leaf %||% character())
    path <- do.call(file.path, as.list(path_parts))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    path
  }

  bulk_unique_path <- function(dir_path, file_name) {
    file_name <- sanitize_bulk_component(file_name, fallback = "output")
    candidate <- file.path(dir_path, file_name)
    if (!file.exists(candidate)) {
      return(candidate)
    }

    ext <- tools::file_ext(file_name)
    stem <- tools::file_path_sans_ext(file_name)
    for (i in seq_len(999)) {
      suffix <- if (nzchar(ext)) paste0("_", i, ".", ext) else paste0("_", i)
      candidate <- file.path(dir_path, paste0(stem, suffix))
      if (!file.exists(candidate)) {
        return(candidate)
      }
    }

    file.path(dir_path, paste0(stem, "_", as.integer(Sys.time()), if (nzchar(ext)) paste0(".", ext) else ""))
  }

  bulk_open_plot_device <- function(file_path, extension, width, height, dpi) {
    extension <- normalize_bulk_image_format(extension)
    if (extension %in% c(".jpg", ".jpeg")) {
      grDevices::jpeg(filename = file_path, width = width, height = height, units = "in", res = dpi, quality = 95)
    } else if (extension %in% c(".tif", ".tiff")) {
      grDevices::tiff(filename = file_path, width = width, height = height, units = "in", res = dpi, compression = "lzw")
    } else if (extension == ".png") {
      grDevices::png(filename = file_path, width = width, height = height, units = "in", res = dpi)
    } else if (extension == ".pdf") {
      grDevices::pdf(file = file_path, width = width, height = height, onefile = FALSE)
    } else if (extension == ".svg") {
      grDevices::svg(filename = file_path, width = width, height = height)
    } else if (extension == ".bmp") {
      grDevices::bmp(filename = file_path, width = width, height = height, units = "in", res = dpi)
    } else if (extension == ".eps") {
      grDevices::postscript(file = file_path, width = width, height = height, onefile = FALSE, paper = "special", horizontal = FALSE)
    } else if (extension == ".ps") {
      grDevices::postscript(file = file_path, width = width, height = height, onefile = TRUE, paper = "special", horizontal = FALSE)
    } else {
      grDevices::jpeg(filename = file_path, width = width, height = height, units = "in", res = dpi, quality = 95)
    }
  }

  bulk_draw_plot <- function(plot_object) {
    if (inherits(plot_object, "recordedplot")) {
      grDevices::replayPlot(plot_object)
      return(invisible(TRUE))
    }

    if (inherits(plot_object, c("gg", "ggplot", "patchwork", "trellis"))) {
      print(plot_object)
      return(invisible(TRUE))
    }

    if (inherits(plot_object, "pheatmap") && !is.null(plot_object$gtable)) {
      grid::grid.newpage()
      grid::grid.draw(plot_object$gtable)
      return(invisible(TRUE))
    }

    if (inherits(plot_object, c("grob", "gtable"))) {
      grid::grid.newpage()
      grid::grid.draw(plot_object)
      return(invisible(TRUE))
    }

    if (inherits(plot_object, c("Heatmap", "HeatmapList")) && requireNamespace("ComplexHeatmap", quietly = TRUE)) {
      ComplexHeatmap::draw(plot_object)
      return(invisible(TRUE))
    }

    if (is.list(plot_object) && !is.data.frame(plot_object)) {
      plot_candidates <- Filter(bulk_is_plot, plot_object)
      if (length(plot_candidates)) {
        if (requireNamespace("patchwork", quietly = TRUE)) {
          print(patchwork::wrap_plots(plot_candidates))
        } else {
          print(plot_candidates[[1]])
        }
        return(invisible(TRUE))
      }
    }

    print(plot_object)
    invisible(TRUE)
  }

  bulk_save_plot <- function(plot_object, file_path, extension, width, height, dpi) {
    ggsave_candidate <- inherits(plot_object, c("gg", "ggplot", "patchwork"))
    if (ggsave_candidate) {
      saved <- tryCatch(
        {
          ggplot2::ggsave(filename = file_path, plot = plot_object, width = width, height = height, dpi = dpi, units = "in", limitsize = FALSE)
          TRUE
        },
        error = function(e) FALSE
      )

      if (isTRUE(saved) && file.exists(file_path) && file.info(file_path)$size > 0) {
        return(invisible(TRUE))
      }
    }

    bulk_open_plot_device(file_path, extension, width, height, dpi)
    on.exit({
      if (grDevices::dev.cur() > 1) {
        grDevices::dev.off()
      }
    }, add = TRUE)
    bulk_draw_plot(plot_object)
    invisible(TRUE)
  }

  bulk_save_table <- function(table_object, file_path) {
    row_names <- FALSE
    if (!is.null(rownames(table_object))) {
      rn <- rownames(table_object)
      row_names <- any(!is.na(rn) & nzchar(rn) & !grepl("^[0-9]+$", rn))
    }
    utils::write.csv(table_object, file = file_path, row.names = row_names)
  }

  bulk_resolve_extra_files <- function(group, result = NULL) {
    extra_files <- group$pdf_files %||% character()
    if (is.function(extra_files)) {
      extra_files <- tryCatch(extra_files(result), error = function(e) character())
    }

    extra_files <- as.character(extra_files %||% character())
    extra_files[!is.na(extra_files) & nzchar(extra_files)]
  }

  bulk_copy_extra_files <- function(root_dir, group, result = NULL) {
    extra_files <- bulk_resolve_extra_files(group, result)
    if (!length(extra_files)) {
      return(character())
    }

    group_dir <- bulk_folder_path(root_dir, group$folder)
    copied <- character()

    for (extra_file in extra_files) {
      source_path <- if (grepl("^[A-Za-z]:|^/", extra_file)) extra_file else vstdavis_app_file(extra_file)
      if (!file.exists(source_path) || isTRUE(file.info(source_path)$size == 0)) {
        next
      }

      target_path <- bulk_unique_path(group_dir, basename(source_path))
      if (file.copy(source_path, target_path, overwrite = TRUE)) {
        copied <- c(copied, target_path)
      }
    }

    copied
  }

  bulk_export_group <- function(root_dir, group, image_format, dpi) {
    result <- bulk_safe_get_result(group)
    if (is.null(result) || !is.list(result)) {
      return(list(files = character(), messages = character()))
    }

    result_names <- names(result)
    if (is.null(result_names)) {
      result_names <- paste0("item", seq_along(result))
    }
    missing_names <- is.na(result_names) | !nzchar(result_names)
    result_names[missing_names] <- paste0("item", which(missing_names))
    created_files <- character()
    messages <- character()

    group_dir <- bulk_folder_path(root_dir, group$folder)

    for (i in seq_along(result)) {
      output_object <- result[[i]]
      original_name <- result_names[[i]]

      if (bulk_should_save_plot(original_name, output_object)) {
        base_name <- bulk_output_label(group, original_name, "plot")
        size <- bulk_adjust_plot_size(output_object, bulk_output_size(group, original_name))
        plot_path <- bulk_unique_path(group_dir, paste0(base_name, image_format))
        saved <- tryCatch(
          {
            bulk_save_plot(output_object, plot_path, image_format, width = size[["width"]], height = size[["height"]], dpi = dpi)
            TRUE
          },
          error = function(e) {
            messages <<- c(messages, paste0("Could not save plot ", paste(group$folder, collapse = " / "), " / ", original_name, ": ", conditionMessage(e)))
            FALSE
          }
        )
        if (isTRUE(saved) && file.exists(plot_path) && file.info(plot_path)$size > 0) {
          created_files <- c(created_files, plot_path)
        }
      } else if (bulk_is_table(output_object)) {
        base_name <- bulk_output_label(group, original_name, "table")
        table_path <- bulk_unique_path(group_dir, paste0(base_name, ".csv"))
        saved <- tryCatch(
          {
            bulk_save_table(output_object, table_path)
            TRUE
          },
          error = function(e) {
            messages <<- c(messages, paste0("Could not save table ", paste(group$folder, collapse = " / "), " / ", original_name, ": ", conditionMessage(e)))
            FALSE
          }
        )
        if (isTRUE(saved) && file.exists(table_path)) {
          created_files <- c(created_files, table_path)
        }
      }
    }

    extra_files <- tryCatch(
      bulk_copy_extra_files(root_dir, group, result),
      error = function(e) {
        messages <<- c(messages, paste0("Could not copy extra files for ", paste(group$folder, collapse = " / "), ": ", conditionMessage(e)))
        character()
      }
    )

    list(files = c(created_files, extra_files), messages = messages)
  }

  bulk_zip_directory <- function(source_dir, zip_file) {
    files <- list.files(source_dir, all.files = FALSE, recursive = TRUE, full.names = FALSE, include.dirs = FALSE)
    if (!length(files)) {
      stop("No files were prepared for the bulk download.")
    }

    source_dir <- normalizePath(source_dir, winslash = "/", mustWork = TRUE)
    zip_file <- normalizePath(zip_file, winslash = "/", mustWork = FALSE)
    temp_zip <- tempfile(fileext = ".zip")
    on.exit(unlink(temp_zip, force = TRUE), add = TRUE)

    zip_created <- FALSE
    nested_files_exist <- any(grepl("[/\\\\]", files))

    archive_preserves_folders <- function(archive_path) {
      if (!nested_files_exist) {
        return(TRUE)
      }

      archive_listing <- tryCatch(utils::unzip(archive_path, list = TRUE), error = function(e) NULL)
      if (is.null(archive_listing) || !nrow(archive_listing)) {
        return(FALSE)
      }

      any(grepl("[/\\\\]", archive_listing$Name))
    }

    if (.Platform$OS.type == "windows") {
      ps_file <- tempfile(fileext = ".ps1")
      on.exit(unlink(ps_file, force = TRUE), add = TRUE)
      writeLines(
        c(
          "param([string]$SourceDir, [string]$ZipFile)",
          "Add-Type -AssemblyName System.IO.Compression.FileSystem",
          "if (Test-Path -LiteralPath $ZipFile) { Remove-Item -LiteralPath $ZipFile -Force }",
          "$sourceRoot = [System.IO.Path]::GetFullPath($SourceDir)",
          "if (-not $sourceRoot.EndsWith([System.IO.Path]::DirectorySeparatorChar)) {",
          "  $sourceRoot = $sourceRoot + [System.IO.Path]::DirectorySeparatorChar",
          "}",
          "$sourceUri = New-Object System.Uri($sourceRoot)",
          "$archive = [System.IO.Compression.ZipFile]::Open($ZipFile, [System.IO.Compression.ZipArchiveMode]::Create)",
          "try {",
          "  $directories = Get-ChildItem -LiteralPath $SourceDir -Directory -Recurse -Force",
          "  foreach ($directory in $directories) {",
          "    $dirUri = New-Object System.Uri($directory.FullName)",
          "    $entryName = [System.Uri]::UnescapeDataString($sourceUri.MakeRelativeUri($dirUri).ToString()).Replace('\\', '/')",
          "    if ($entryName.Length -gt 0) {",
          "      if (-not $entryName.EndsWith('/')) { $entryName = $entryName + '/' }",
          "      [void]$archive.CreateEntry($entryName)",
          "    }",
          "  }",
          "  $sourceFiles = Get-ChildItem -LiteralPath $SourceDir -File -Recurse -Force",
          "  foreach ($sourceFile in $sourceFiles) {",
          "    $fileUri = New-Object System.Uri($sourceFile.FullName)",
          "    $entryName = [System.Uri]::UnescapeDataString($sourceUri.MakeRelativeUri($fileUri).ToString()).Replace('\\', '/')",
          "    if ($entryName.Length -gt 0) {",
          "      [void][System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($archive, $sourceFile.FullName, $entryName, [System.IO.Compression.CompressionLevel]::Optimal)",
          "    }",
          "  }",
          "} finally {",
          "  $archive.Dispose()",
          "}"
        ),
        con = ps_file,
        useBytes = TRUE
      )
      status <- tryCatch(
        system2(
          "powershell",
          args = c("-NoProfile", "-NonInteractive", "-ExecutionPolicy", "Bypass", "-File", ps_file, source_dir, temp_zip),
          stdout = TRUE,
          stderr = TRUE
        ),
        error = function(e) structure(conditionMessage(e), status = 1L)
      )
      zip_created <- file.exists(temp_zip) && file.info(temp_zip)$size > 0 && is.null(attr(status, "status")) && archive_preserves_folders(temp_zip)
      if (!zip_created && file.exists(temp_zip)) {
        unlink(temp_zip, force = TRUE)
      }
    }

    if (!zip_created && requireNamespace("zip", quietly = TRUE)) {
      if (file.exists(temp_zip)) {
        unlink(temp_zip, force = TRUE)
      }
      old_wd <- vstdavis_safe_getwd()
      on.exit(vstdavis_restore_wd(old_wd), add = TRUE)
      setwd(source_dir)
      zip::zipr(zipfile = temp_zip, files = files)
      zip_created <- file.exists(temp_zip) && file.info(temp_zip)$size > 0 && archive_preserves_folders(temp_zip)
      if (!zip_created && file.exists(temp_zip)) {
        unlink(temp_zip, force = TRUE)
      }
    }

    if (!zip_created) {
      if (file.exists(temp_zip)) {
        unlink(temp_zip, force = TRUE)
      }
      old_wd <- vstdavis_safe_getwd()
      on.exit(vstdavis_restore_wd(old_wd), add = TRUE)
      setwd(source_dir)
      utils::zip(zipfile = temp_zip, files = files, flags = "-r9X")
      zip_created <- file.exists(temp_zip) && file.info(temp_zip)$size > 0 && archive_preserves_folders(temp_zip)
    }

    if (!zip_created) {
      stop("Could not create the ZIP archive.")
    }

    if (!archive_preserves_folders(temp_zip)) {
      stop("ZIP archive was created without folders. Please retry the bulk download.")
    }

    if (!file.copy(temp_zip, zip_file, overwrite = TRUE)) {
      stop("Could not finalize the ZIP archive.")
    }
  }

  bulk_download_summary_rows <- function() {
    groups <- bulk_download_groups()
    rows <- lapply(groups, function(group) {
      result <- bulk_safe_get_result(group)
      completed <- !is.null(result) && is.list(result)
      plot_count <- if (completed) sum(vapply(seq_along(result), function(i) {
        output_names <- names(result) %||% character()
        candidate_name <- if (length(output_names) >= i) output_names[[i]] else ""
        output_name <- if (!is.na(candidate_name) && nzchar(candidate_name)) candidate_name else paste0("item", i)
        bulk_should_save_plot(output_name, result[[i]])
      }, logical(1))) else 0L
      table_count <- if (completed) sum(vapply(result, bulk_is_table, logical(1))) else 0L
      extra_file_count <- 0L

      if (completed) {
        resolved_extra_files <- bulk_resolve_extra_files(group, result)
        extra_file_count <- sum(vapply(resolved_extra_files, function(extra_file) {
          source_path <- if (grepl("^[A-Za-z]:|^/", extra_file)) extra_file else vstdavis_app_file(extra_file)
          file.exists(source_path) && file.info(source_path)$size > 0
        }, logical(1)))
      }

      data.frame(
        Section = paste(group$folder, collapse = " / "),
        Status = if (completed) "Completed" else "Not completed",
        Images = plot_count,
        Tables = table_count,
        Files = extra_file_count,
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, rows)
  }

  selected_run_log_entry <- reactive({
    entries <- run_log_state$entries
    if (!nrow(entries)) {
      return(NULL)
    }

    selected_idx <- input$run_log_table_rows_selected
    if (length(selected_idx)) {
      entries[selected_idx[1], , drop = FALSE]
    } else {
      entries[1, , drop = FALSE]
    }
  })

  output$run_log_current_section <- renderText(run_log_state$current$section %||% "Idle")
  output$run_log_current_action <- renderText(run_log_state$current$action %||% "Waiting for user input")
  output$run_log_current_status <- renderText(run_log_state$current$status %||% "Idle")
  output$run_log_current_started <- renderText(run_log_state$current$started_at %||% "")
  output$run_log_current_finished <- renderText(run_log_state$current$finished_at %||% "")
  output$run_log_current_detail <- renderText(run_log_state$current$detail %||% "No analysis is currently running.")
  output$run_log_latest_params <- renderText(run_log_state$current$params_text %||% "No parameters captured yet.")

  output$run_log_progress_ui <- renderUI({
    current <- run_log_state$current
    progress_value <- suppressWarnings(as.numeric(current$progress %||% 0))
    if (!is.finite(progress_value)) {
      progress_value <- 0
    }
    progress_value <- max(0, min(100, progress_value))

    progress_class <- switch(
      current$status %||% "Idle",
      Running = "progress-bar progress-bar-striped active",
      Completed = "progress-bar progress-bar-success",
      Failed = "progress-bar progress-bar-danger",
      "progress-bar progress-bar-info"
    )

    shiny::tags$div(
      class = "progress",
      shiny::tags$div(
        class = progress_class,
        role = "progressbar",
        `aria-valuenow` = progress_value,
        `aria-valuemin` = 0,
        `aria-valuemax` = 100,
        style = paste0("width: ", progress_value, "%; min-width: 3em;"),
        paste0(progress_value, "%")
      )
    )
  })

  output$run_log_table <- renderDataTable({
    log_df <- run_log_state$entries
    if (!nrow(log_df)) {
      log_df <- data.frame(
        run_id = character(),
        started_at = character(),
        finished_at = character(),
        section = character(),
        action = character(),
        status = character(),
        message = character(),
        stringsAsFactors = FALSE
      )
    } else {
      log_df <- log_df[, c("run_id", "started_at", "finished_at", "section", "action", "status", "message")]
    }

    DT::datatable(
      log_df,
      options = list(scrollX = TRUE, pageLength = 10, dom = "Blfrtip"),
      rownames = FALSE,
      selection = "single"
    )
  })

  output$run_log_selected_params <- renderText({
    entry <- selected_run_log_entry()
    if (is.null(entry)) {
      return("No run has been logged yet.")
    }
    entry$parameters[[1]]
  })

  output$run_log_selected_message <- renderText({
    entry <- selected_run_log_entry()
    if (is.null(entry)) {
      return("No run has been logged yet.")
    }
    entry$message[[1]]
  })

  output$download_run_log <- downloadHandler(
    filename = function() {
      paste0("VST-DAVis_run_log_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".csv")
    },
    content = function(file) {
      write.csv(run_log_state$entries, file, row.names = FALSE)
    }
  )

  bulk_download_summary_data <- reactive({
    input$refresh_bulk_download_table
    bulk_download_summary_rows()
  })

  output$bulk_download_progress_ui <- renderUI({
    progress_value <- max(0, min(100, suppressWarnings(as.numeric(bulk_download_state$progress %||% 0))))
    progress_class <- switch(
      bulk_download_state$status %||% "Idle",
      Running = "progress-bar progress-bar-striped active",
      Completed = "progress-bar progress-bar-success",
      Failed = "progress-bar progress-bar-danger",
      "progress-bar progress-bar-info"
    )

    shiny::tags$div(
      class = "progress",
      shiny::tags$div(
        class = progress_class,
        role = "progressbar",
        `aria-valuenow` = progress_value,
        `aria-valuemin` = 0,
        `aria-valuemax` = 100,
        style = paste0("width: ", progress_value, "%; min-width: 3em;"),
        paste0(progress_value, "%")
      )
    )
  })

  output$bulk_download_status <- renderText({
    status_text <- bulk_download_state$status %||% "Idle"
    detail_text <- bulk_download_state$detail %||% "No bulk download has started."
    completed_at <- bulk_download_state$completed_at %||% ""
    if (nzchar(completed_at)) {
      paste(status_text, detail_text, paste("Completed at:", completed_at), sep = "\n")
    } else {
      paste(status_text, detail_text, sep = "\n")
    }
  })

  output$bulk_download_summary <- renderText({
    rows <- bulk_download_summary_data()
    completed_rows <- rows[rows$Status == "Completed", , drop = FALSE]
    if (!nrow(completed_rows)) {
      return("No completed analyses are available for bulk download yet.")
    }

    paste0(
      nrow(completed_rows),
      " completed section(s) available: ",
      sum(completed_rows$Images),
      " image(s), ",
      sum(completed_rows$Tables),
      " table(s), ",
      sum(completed_rows$Files),
      " additional file(s)."
    )
  })

  output$bulk_download_table <- renderDataTable({
    DT::datatable(
      bulk_download_summary_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        paging = TRUE,
        dom = "lfrtip"
      ),
      rownames = FALSE,
      selection = "none"
    )
  })

  output$download_bulk_results <- downloadHandler(
    filename = function() {
      paste0("VST-DAVis_bulk_results_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".zip")
    },
    contentType = "application/zip",
    content = function(file) {
      image_format <- normalize_bulk_image_format(input$bulk_image_format %||% ".jpg")
      dpi <- suppressWarnings(as.numeric(input$bulk_image_dpi %||% 300))
      if (!is.finite(dpi)) {
        dpi <- 300
      }
      dpi <- max(72, min(600, dpi))
      params <- list(image_format = image_format, image_dpi = dpi)
      params_text <- format_run_params(params)
      run_id <- append_run_log_entry(
        section = "Bulk Download",
        action = "Download completed images and tables",
        status = "Running",
        params = params,
        message = "Preparing bulk ZIP download."
      )
      started_at <- run_log_state$entries$started_at[match(run_id, run_log_state$entries$run_id)]

      set_current_run(
        section = "Bulk Download",
        action = "Download completed images and tables",
        status = "Running",
        detail = "Preparing bulk download workspace...",
        progress = 5,
        started_at = started_at,
        finished_at = "",
        params_text = params_text
      )
      set_bulk_download_state("Running", "Preparing bulk download workspace...", 5)

      withProgress(message = "Bulk Download", detail = "Preparing bulk download workspace...", value = 0, {
        temp_root <- file.path(tempdir(), paste0("VST-DAVis_bulk_", session$token, "_", as.integer(Sys.time())))
        unlink(temp_root, recursive = TRUE, force = TRUE)
        dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
        on.exit(unlink(temp_root, recursive = TRUE, force = TRUE), add = TRUE)

        tryCatch(
          {
            writeLines(sess_txt(), con = file.path(temp_root, "sessionInfo.txt"), useBytes = TRUE)

            groups <- bulk_download_groups()
            total_groups <- max(1, length(groups))
            created_files <- file.path(temp_root, "sessionInfo.txt")
            export_messages <- character()

            incProgress(0.05, detail = "Extracting completed outputs...")

            for (i in seq_along(groups)) {
              group <- groups[[i]]
              detail_text <- paste0("Extracting ", paste(group$folder, collapse = " / "))
              progress_value <- 10 + (i - 1) / total_groups * 75
              incProgress(0.75 / total_groups, detail = detail_text)
              set_bulk_download_state("Running", detail_text, progress_value)
              set_current_run(
                section = "Bulk Download",
                action = "Download completed images and tables",
                status = "Running",
                detail = detail_text,
                progress = progress_value,
                started_at = started_at,
                finished_at = "",
                params_text = params_text
              )

              exported <- bulk_export_group(temp_root, group, image_format = image_format, dpi = dpi)
              created_files <- c(created_files, exported$files)
              export_messages <- c(export_messages, exported$messages)
            }

            if (length(export_messages)) {
              writeLines(unique(export_messages), con = file.path(temp_root, "bulkExportLog.txt"), useBytes = TRUE)
              created_files <- c(created_files, file.path(temp_root, "bulkExportLog.txt"))
            }

            created_files <- created_files[file.exists(created_files)]
            if (!length(created_files)) {
              stop("No completed images, tables, additional files, or session information could be prepared.")
            }

            incProgress(0.15, detail = "Compressing ZIP file...")
            set_bulk_download_state("Running", "Compressing ZIP file...", 90)
            set_current_run(
              section = "Bulk Download",
              action = "Download completed images and tables",
              status = "Running",
              detail = "Compressing ZIP file...",
              progress = 90,
              started_at = started_at,
              finished_at = "",
              params_text = params_text
            )

            bulk_zip_directory(temp_root, file)
            file_size <- tryCatch(file.info(file)$size, error = function(e) NA_real_)
            size_label <- format_download_size(file_size)
            completed_detail <- if (nzchar(size_label)) {
              paste0("Bulk download completed successfully (", size_label, ").")
            } else {
              "Bulk download completed successfully."
            }

            incProgress(0.05, detail = "Download is ready.")
            update_run_log_entry(run_id, "Completed", completed_detail)
            set_current_run(
              section = "Bulk Download",
              action = "Download completed images and tables",
              status = "Completed",
              detail = completed_detail,
              progress = 100,
              started_at = started_at,
              finished_at = format_run_timestamp(),
              params_text = params_text
            )
            set_bulk_download_state("Completed", completed_detail, 100, format_run_timestamp())
          },
          error = function(e) {
            failure_message <- paste0("Bulk download failed: ", conditionMessage(e))
            update_run_log_entry(run_id, "Failed", failure_message)
            set_current_run(
              section = "Bulk Download",
              action = "Download completed images and tables",
              status = "Failed",
              detail = failure_message,
              progress = 100,
              started_at = started_at,
              finished_at = format_run_timestamp(),
              params_text = params_text
            )
            set_bulk_download_state("Failed", failure_message, 100, format_run_timestamp())
            stop(e)
          }
        )
      })
    }
  )

  observeEvent(input$menu_tabs, {
    append_run_log_entry(
      section = "Navigation",
      action = "Top-level tab changed",
      status = "Viewed",
      params = list(selected_tab = input$menu_tabs),
      message = "User switched the top-level navigation tab."
    )
  }, ignoreInit = TRUE)

  observeEvent(input$multiple_tabsets, {
    append_run_log_entry(
      section = "Navigation",
      action = "Single or Multiple Samples tab changed",
      status = "Viewed",
      params = list(selected_tab = input$multiple_tabsets),
      message = "User switched the analysis sub-tab."
    )
  }, ignoreInit = TRUE)

  observeEvent(input$subclustering_multiple_tabsets, {
    append_run_log_entry(
      section = "Navigation",
      action = "Subclustering tab changed",
      status = "Viewed",
      params = list(selected_tab = input$subclustering_multiple_tabsets),
      message = "User switched the subclustering sub-tab."
    )
  }, ignoreInit = TRUE)

  observeEvent(input$Coexpression_tabsets, {
    append_run_log_entry(
      section = "Navigation",
      action = "Co-expression and TF tab changed",
      status = "Viewed",
      params = list(selected_tab = input$Coexpression_tabsets),
      message = "User switched the co-expression/TF analysis tab."
    )
  }, ignoreInit = TRUE)
  
  
  
############################################################################################################################################################
                                                                  ##    Multiple Input     ##
############################################################################################################################################################
  
  ##########multiple sidebar hide##########
  observeEvent(input[["multiple_tabsets"]], {
    if(input[["multiple_tabsets"]] == "Stats"){
      showElement(selector = "#multiple_sidebar")
      removeCssClass("multiple_main_menu", "col-sm-12")
      addCssClass("multiple_main_menu", "col-sm-8")
    }else{
      hideElement(selector = "#multiple_sidebar")
      removeCssClass("multiple_main_menu", "col-sm-8")
      addCssClass("multiple_main_menu", "col-sm-12")
    }
  })
  
  ################multiple hide tabs########
  hideTab(inputId = "multiple_tabsets", target = "Sample Groups and QC Filtering")
  hideTab(inputId = "multiple_tabsets", target = "Normalization and PCA Analysis")
  hideTab(inputId = "multiple_tabsets", target = "Clustering")
  hideTab(inputId = "multiple_tabsets", target = "Markers Identification")
  hideTab(inputId = "multiple_tabsets", target = "Cell Type Prediction")
  hideTab(inputId = "multiple_tabsets", target = "Cluster-Based Plots")
  hideTab(inputId = "multiple_tabsets", target = "Condition Based Analysis")
  
  ################showtabbutton#############     
  observeEvent(input$link_m_qc_filtering, {
    showTab(inputId = "multiple_tabsets", target = "Sample Groups and QC Filtering")
  })
  
  observeEvent(input$link_m_normalization, {
    showTab(inputId = "multiple_tabsets", target = "Normalization and PCA Analysis")
  })
  
  observeEvent(input$link_m_clustering, {
    showTab(inputId = "multiple_tabsets", target = "Clustering")
  })

  observeEvent(input$link_m_marker, {
    showTab(inputId = "multiple_tabsets", target = "Markers Identification")
  })
  
  observeEvent(input$link_m_prediction, {
    showTab(inputId = "multiple_tabsets", target = "Cell Type Prediction")
  })
  
  observeEvent(input$link_m_clusterbased, {
    showTab(inputId = "multiple_tabsets", target = "Cluster-Based Plots")
  })
  
  observeEvent(input$link_m_conditionbased, {
    showTab(inputId = "multiple_tabsets", target = "Condition Based Analysis")
  })
  

  
  ################ multiple input###########   
  output$multiple_sample_file_ui <- renderUI({
    format_choice <- input$multiple_sample_format %||% "h5"

    if (identical(format_choice, "exampledata") || identical(format_choice, "MFB")) {
      return(NULL)
    }

    upload_label <- if (identical(format_choice, "visium_bin")) {
      "Upload multiple files at once (Visium HD Bin data with binned_outputs and spatial folder ZIP format)"
    } else {
      "Upload multiple files at once (SpaceRanger h5 and spatial image in zip format)"
    }

    fileInput(
      "multiple_sample_file",
      label = upload_label,
      multiple = TRUE,
      accept = ".zip"
    )
  })

  observe({
    if (input$multiple_sample_format == "h5") {
      shinyjs::hide("multiple_sample_file_mfb")
      shinyjs::hide("multiple_sample_spatial_mode")
      shinyjs::hide("multiple_sample_hd_bin_size")
      shinyjs::hide("multiple_sample_hd_hint")

    }
    else if (input$multiple_sample_format == "MFB") {
      shinyjs::show("multiple_sample_file_mfb")
      shinyjs::hide("multiple_sample_spatial_mode")
      shinyjs::hide("multiple_sample_hd_bin_size")
      shinyjs::hide("multiple_sample_hd_hint")

    }
    else if (input$multiple_sample_format == "visium_bin") {
      shinyjs::hide("multiple_sample_file_mfb")
      shinyjs::hide("multiple_sample_spatial_mode")
      shinyjs::show("multiple_sample_hd_bin_size")
      shinyjs::show("multiple_sample_hd_hint")

    }
    else if (input$multiple_sample_format == "exampledata") {
      shinyjs::hide("multiple_sample_file_mfb")
      shinyjs::hide("multiple_sample_spatial_mode")
      shinyjs::hide("multiple_sample_hd_bin_size")
      shinyjs::hide("multiple_sample_hd_hint")

    }
    
  })
  
  observeEvent(input$multiple_sample_format, {
    if (identical(input$multiple_sample_format, "visium_bin")) {
      updateSelectInput(
        session,
        "multiple_sample_spatial_mode",
        choices = list(
          "Auto detect Visium HD bins or standard spatial" = "auto",
          "Visium HD binned outputs" = "visium_hd"
        ),
        selected = "visium_hd"
      )
      updateSelectInput(
        session,
        "multiple_sample_hd_bin_size",
        choices = list("8 um" = "8", "16 um" = "16", "2 um" = "2"),
        selected = if (input$multiple_sample_hd_bin_size %in% c("8", "16", "2")) input$multiple_sample_hd_bin_size else "8"
      )
    } else {
      updateSelectInput(
        session,
        "multiple_sample_spatial_mode",
        choices = list(
          "Auto detect Visium HD bins or standard spatial" = "auto",
          "Visium HD binned outputs" = "visium_hd"
        ),
        selected = "auto"
      )
      updateSelectInput(
        session,
        "multiple_sample_hd_bin_size",
        choices = list("8 um" = "8", "16 um" = "16", "2 um" = "2"),
        selected = "8"
      )
    }
  }, ignoreInit = FALSE)
  
  
  ########multiple hide qc filtering######## 
  shinyjs::show("m_bf_box0")
  shinyjs::hide("m_bf_box1")
  shinyjs::hide("m_bf_box2")
  shinyjs::hide("m_bf_box3")
  shinyjs::hide("m_bf_box4")
  shinyjs::hide("m_bf_box5")
  shinyjs::hide("m_bf_box6")
  
  observeEvent(input$multiple_sample_submit,{
    #shinyjs::hide("m_bf_box0")
    shinyjs::show("m_bf_box1")
    shinyjs::show("m_bf_box2")
    shinyjs::show("m_bf_box3")
    shinyjs::show("m_bf_box4")
    shinyjs::show("m_bf_box5")
    shinyjs::show("m_bf_box6")
  })
  
  ########multiple hide qc filtering######## 
  shinyjs::hide("m_bf_box1")
  shinyjs::hide("m_bf_box2")
  shinyjs::hide("m_bf_box3")
  shinyjs::hide("m_bf_box4")
  shinyjs::hide("m_bf_box5")
  
  observeEvent(input$multiple_sample_submit,{
    shinyjs::show("m_bf_box1")
    shinyjs::show("m_bf_box2")
    shinyjs::show("m_bf_box3")
    shinyjs::show("m_bf_box4")
    shinyjs::show("m_bf_box5")
  })
  
  
  ########multiple hide qc filtering########  
  shinyjs::hide("m_qc_filter_box1")
  shinyjs::hide("m_qc_filter_box2")
  shinyjs::hide("m_qc_filter_box3")
  shinyjs::hide("m_qc_filter_box4")
  shinyjs::hide("m_qc_filter_box5")
  shinyjs::hide("m_qc_filter_box6")
  shinyjs::hide("m_qc_filter_box7")
  
  observeEvent(input$multiple_sample_qc_filtering,{
    shinyjs::show("m_qc_filter_box1")
    shinyjs::show("m_qc_filter_box2")
    shinyjs::show("m_qc_filter_box3")
    shinyjs::show("m_qc_filter_box4")
    shinyjs::show("m_qc_filter_box5")
    shinyjs::show("m_qc_filter_box6")
    shinyjs::show("m_qc_filter_box7")
  })
  
  observe({
    if (input$multiple_group_count == 1) {
      shinyjs::show("group1_name")
      shinyjs::show("group1_samples")
      shinyjs::hide("group2_name")
      shinyjs::hide("group2_samples")
      shinyjs::hide("group3_name")
      shinyjs::hide("group3_samples")
      shinyjs::hide("group4_name")
      shinyjs::hide("group4_samples")
      shinyjs::hide("group5_name")
      shinyjs::hide("group5_samples")
      shinyjs::hide("group6_name")
      shinyjs::hide("group6_samples")
      shinyjs::hide("group2_samples2")
      shinyjs::hide("group3_samples3")
      shinyjs::hide("group4_samples4")
      shinyjs::hide("group5_samples5")
      shinyjs::hide("group6_samples6")
      hideTab(inputId = "multiple_tabsets", target = "Condition Based Analysis")
      hideTab(inputId = "subclustering_multiple_tabsets", target = "Condition Based Analysis")
      shinyjs::hide("link_m_conditionbased")
      shinyjs::hide("link_m_subclustering_conditionbased")
    }
    else if (input$multiple_group_count == 2) {
      shinyjs::show("group1_name")
      shinyjs::show("group1_samples")
      shinyjs::show("group2_name")
      shinyjs::show("group2_samples")
      shinyjs::hide("group3_name")
      shinyjs::hide("group3_samples")
      shinyjs::hide("group4_name")
      shinyjs::hide("group4_samples")
      shinyjs::hide("group5_name")
      shinyjs::hide("group5_samples")
      shinyjs::hide("group6_name")
      shinyjs::hide("group6_samples")
      shinyjs::show("group2_samples2")
      shinyjs::hide("group3_samples3")
      shinyjs::hide("group4_samples4")
      shinyjs::hide("group5_samples5")
      shinyjs::hide("group6_samples6")
      #showTab(inputId = "multiple_tabsets", target = "Condition based analysis")
      #showTab(inputId = "subclustering_multiple_tabsets", target = "Condition based analysis")
      shinyjs::show("link_m_conditionbased")
      shinyjs::show("link_m_subclustering_conditionbased")
    }
    else if (input$multiple_group_count == 3) {
      shinyjs::show("group1_name")
      shinyjs::show("group1_samples")
      shinyjs::show("group2_name")
      shinyjs::show("group2_samples")
      shinyjs::show("group3_name")
      shinyjs::show("group3_samples")
      shinyjs::hide("group4_name")
      shinyjs::hide("group4_samples")
      shinyjs::hide("group5_name")
      shinyjs::hide("group5_samples")
      shinyjs::hide("group6_name")
      shinyjs::hide("group6_samples")
      shinyjs::show("group2_samples2")
      shinyjs::show("group3_samples3")
      shinyjs::hide("group4_samples4")
      shinyjs::hide("group5_samples5")
      shinyjs::hide("group6_samples6")
      shinyjs::show("link_m_conditionbased")
      shinyjs::show("link_m_subclustering_conditionbased")
    }
    else if (input$multiple_group_count == 4) {
      shinyjs::show("group1_name")
      shinyjs::show("group1_samples")
      shinyjs::show("group2_name")
      shinyjs::show("group2_samples")
      shinyjs::show("group3_name")
      shinyjs::show("group3_samples")
      shinyjs::show("group4_name")
      shinyjs::show("group4_samples")
      shinyjs::hide("group5_name")
      shinyjs::hide("group5_samples")
      shinyjs::hide("group6_name")
      shinyjs::hide("group6_samples")
      shinyjs::show("group2_samples2")
      shinyjs::show("group3_samples3")
      shinyjs::show("group4_samples4")
      shinyjs::hide("group5_samples5")
      shinyjs::hide("group6_samples6")
      shinyjs::show("link_m_conditionbased")
      shinyjs::show("link_m_subclustering_conditionbased")
    }
    else if (input$multiple_group_count == 5) {
      shinyjs::show("group1_name")
      shinyjs::show("group1_samples")
      shinyjs::show("group2_name")
      shinyjs::show("group2_samples")
      shinyjs::show("group3_name")
      shinyjs::show("group3_samples")
      shinyjs::show("group4_name")
      shinyjs::show("group4_samples")
      shinyjs::show("group5_name")
      shinyjs::show("group5_samples")
      shinyjs::hide("group6_name")
      shinyjs::hide("group6_samples")
      shinyjs::show("group2_samples2")
      shinyjs::show("group3_samples3")
      shinyjs::show("group4_samples4")
      shinyjs::show("group5_samples5")
      shinyjs::hide("group6_samples6")
      shinyjs::show("link_m_conditionbased")
      shinyjs::show("link_m_subclustering_conditionbased")
    }
    else if (input$multiple_group_count == 6) {
      shinyjs::show("group1_name")
      shinyjs::show("group1_samples")
      shinyjs::show("group2_name")
      shinyjs::show("group2_samples")
      shinyjs::show("group3_name")
      shinyjs::show("group3_samples")
      shinyjs::show("group4_name")
      shinyjs::show("group4_samples")
      shinyjs::show("group5_name")
      shinyjs::show("group5_samples")
      shinyjs::show("group6_name")
      shinyjs::show("group6_samples")
      shinyjs::show("group2_samples2")
      shinyjs::show("group3_samples3")
      shinyjs::show("group4_samples4")
      shinyjs::show("group5_samples5")
      shinyjs::show("group6_samples6")
      shinyjs::show("link_m_conditionbased")
      shinyjs::show("link_m_subclustering_conditionbased")
    }
  })
  
  
  ########multiple hide normalization########     
  shinyjs::hide("m_pca_box1")
  shinyjs::hide("m_elbow_box")
  shinyjs::hide("m_pca_box2")
  shinyjs::hide("m_pca_box3")
  shinyjs::hide("m_pca_box4")
  
  observeEvent(input$multiple_sample_normalization,{
    shinyjs::show("m_pca_box1")
    shinyjs::show("m_elbow_box")
    shinyjs::show("m_pca_box2")
    shinyjs::show("m_pca_box3")
    shinyjs::show("m_pca_box4")
  })
  
  observe({
    if (input$multiple_sample_normalization_method == "LogNormalize") {
      current_assay <- isolate(input$multiple_sample_assay) %||% "auto"
      updateSelectInput(
        session,
        "multiple_sample_assay",
        choices = c("Auto detect" = "auto", "RNA" = "RNA", "Spatial" = "Spatial"),
        selected = if (identical(current_assay, "SCT")) "auto" else current_assay
      )
      shinyjs::show("multiple_sample_scale_factor")
      shinyjs::show("multiple_sample_normalization_variable_genes")
      shinyjs::show("multiple_sample_var_genes")
      shinyjs::hide("multiple_sample_var_genes1")
      shinyjs::show("multiple_sample_normalization_method1")
    }
    else if (input$multiple_sample_normalization_method  == "SCTransform") {
      current_assay <- isolate(input$multiple_sample_assay) %||% "auto"
      updateSelectInput(
        session,
        "multiple_sample_assay",
        choices = c("Auto detect" = "auto", "RNA" = "RNA", "Spatial" = "Spatial", "SCT" = "SCT"),
        selected = current_assay %||% "auto"
      )
      shinyjs::hide("multiple_sample_scale_factor")
      shinyjs::hide("multiple_sample_normalization_variable_genes")
      shinyjs::hide("multiple_sample_var_genes")
      shinyjs::show("multiple_sample_var_genes1")
      shinyjs::show("multiple_sample_normalization_method1")
    }
  })
  
  ########multiple hide clustering########     
  shinyjs::hide("m_clustering_box1")
  shinyjs::hide("m_clustering_box2")
  shinyjs::hide("m_clustering_box3")
  shinyjs::hide("m_clustering_box4")
  shinyjs::hide("m_clustering_box5")
  shinyjs::hide("m_clustering_box6") 
  shinyjs::hide("m_clustering_box7") 
  shinyjs::hide("m_clustering_box8")   
  shinyjs::hide("m_clustering_box9") 
  shinyjs::hide("m_clustering_box10") 
  shinyjs::hide("m_clustering_box11") 
  shinyjs::hide("m_clustering_box12")
  shinyjs::hide("m_clustering_box13")
  shinyjs::hide("m_clustering_box14")
  shinyjs::hide("m_clustering_box15")
  
  observeEvent(input$multiple_sample_clustering,{
    shinyjs::show("m_clustering_box1")
    shinyjs::show("m_clustering_box2")
    shinyjs::show("m_clustering_box3")
    shinyjs::show("m_clustering_box4")
    shinyjs::show("m_clustering_box5")
    shinyjs::show("m_clustering_box6") 
    shinyjs::show("m_clustering_box7") 
    shinyjs::show("m_clustering_box8") 
    shinyjs::show("m_clustering_box9") 
    shinyjs::show("m_clustering_box10") 
    shinyjs::show("m_clustering_box11") 
    shinyjs::show("m_clustering_box12") 
    shinyjs::show("m_clustering_box13") 
    shinyjs::show("m_clustering_box14") 
    shinyjs::show("m_clustering_box15") 
  })
  
  
  observe({
    if (input$m_clustering6 == "umap") {
      shinyjs::show("m_umap_box")
      shinyjs::hide("m_tsne_box")
    }
    else if (input$m_clustering6  == "tsne") {
      shinyjs::hide("m_umap_box")
      shinyjs::show("m_tsne_box")
    }
  })
  
  ########multiple hide markers box########  
  shinyjs::hide("m_marker_box5")
  shinyjs::hide("m_marker_box6")
  shinyjs::hide("m_marker_box7")
  shinyjs::hide("m_marker10")
  shinyjs::hide("m_marker11")
  shinyjs::hide("m_marker12")
  
  
  observe({
    if (input$m_marker1 == 1) {
      shinyjs::hide("m_marker_6")
      shinyjs::hide("m_marker_7")
      shinyjs::hide("m_marker_8")
      shinyjs::hide("m_marker_9")
      shinyjs::hide("m_marker6")
      shinyjs::hide("m_marker7")
      shinyjs::hide("m_marker8")
      shinyjs::hide("m_marker9")
      shinyjs::hide("m_marker10")
      shinyjs::show("m_marker11")
      shinyjs::hide("m_marker12")
    }
    else if (input$m_marker1 == 2) {
      shinyjs::show("m_marker_6")
      shinyjs::hide("m_marker_7")
      shinyjs::hide("m_marker_8")
      shinyjs::hide("m_marker_9")
      shinyjs::show("m_marker6")
      shinyjs::hide("m_marker7")
      shinyjs::hide("m_marker8")
      shinyjs::hide("m_marker9")
      shinyjs::hide("m_marker_box6")
      shinyjs::hide("m_marker10")
      shinyjs::show("m_marker11")
      shinyjs::hide("m_marker12")
      
    }
    else if (input$m_marker1 == 3) {
      shinyjs::show("m_marker_6")
      shinyjs::show("m_marker_7")
      shinyjs::hide("m_marker_8")
      shinyjs::hide("m_marker_9")
      shinyjs::show("m_marker6")
      shinyjs::show("m_marker7")
      shinyjs::hide("m_marker8")
      shinyjs::hide("m_marker9")
      shinyjs::hide("m_marker_box6")
      shinyjs::hide("m_marker10")
      shinyjs::show("m_marker11")
      shinyjs::hide("m_marker12")
    }
    else if (input$m_marker1 == 4) {
      shinyjs::hide("m_marker_6")
      shinyjs::hide("m_marker_7")
      shinyjs::show("m_marker_8")
      shinyjs::hide("m_marker_9")
      shinyjs::hide("m_marker6")
      shinyjs::hide("m_marker7")
      shinyjs::show("m_marker8")
      shinyjs::hide("m_marker9")
      shinyjs::hide("m_marker_box6")
      shinyjs::show("m_marker10")
      shinyjs::hide("m_marker11")
      shinyjs::show("m_marker12")
    }
    
    else if (input$m_marker1 == 5) {
      shinyjs::hide("m_marker_6")
      shinyjs::hide("m_marker_7")
      shinyjs::show("m_marker_8")
      shinyjs::show("m_marker_9")
      shinyjs::hide("m_marker6")
      shinyjs::hide("m_marker7")
      shinyjs::show("m_marker8")
      shinyjs::show("m_marker9")
      shinyjs::hide("m_marker_box6")
      shinyjs::show("m_marker10")
      shinyjs::hide("m_marker11")
      shinyjs::show("m_marker12")
    }
 })
  
  
  ########multiple hide celltype box########  
  shinyjs::hide("m_celltype_box3")
  shinyjs::hide("m_celltype_box4")
  shinyjs::hide("m_celltype_box5")
  shinyjs::hide("m_celltype_box7")
  shinyjs::hide("m_celltype_box8")
  shinyjs::hide("m_celltype_box9")
  shinyjs::hide("m_celltype_box10")
  shinyjs::hide("m_celltype_box11")
  
  observe({
    if (input$m_celltype1 == 1) {
      shinyjs::show("m_celltype_box2")
      shinyjs::show("m_celltype2")
      shinyjs::hide("m_celltype_box3")
      shinyjs::hide("m_celltype3")
      shinyjs::hide("m_celltype4")
      shinyjs::hide("m_celltype_box4")
      shinyjs::hide("m_celltype5")
      shinyjs::hide("m_celltype6")
      shinyjs::hide("m_celltype_box5")
      shinyjs::hide("m_celltype7")
    }
    else if (input$m_celltype1 == 2) {
      shinyjs::hide("m_celltype_box2")
      shinyjs::hide("m_celltype2")
      shinyjs::show("m_celltype_box3")
      shinyjs::show("m_celltype3")
      shinyjs::show("m_celltype4")
      shinyjs::hide("m_celltype_box4")
      shinyjs::hide("m_celltype5")
      shinyjs::hide("m_celltype6")
      shinyjs::hide("m_celltype_box5")
      shinyjs::hide("m_celltype7")
    }
    else if (input$m_celltype1 == 3) {
      shinyjs::hide("m_celltype_box2")
      shinyjs::hide("m_celltype2")
      shinyjs::hide("m_celltype_box3")
      shinyjs::hide("m_celltype3")
      shinyjs::hide("m_celltype4")
      shinyjs::show("m_celltype_box4")
      shinyjs::show("m_celltype5")
      shinyjs::show("m_celltype6")
      shinyjs::hide("m_celltype_box5")
      shinyjs::hide("m_celltype7")
    }
    else if (input$m_celltype1 == 4) {
      shinyjs::hide("m_celltype_box2")
      shinyjs::hide("m_celltype2")
      shinyjs::hide("m_celltype_box3")
      shinyjs::hide("m_celltype3")
      shinyjs::hide("m_celltype4")
      shinyjs::hide("m_celltype_box4")
      shinyjs::hide("m_celltype5")
      shinyjs::hide("m_celltype6")
      shinyjs::show("m_celltype_box5")
      shinyjs::show("m_celltype7")
    }
  })     
  
  ##################multiple Cluster-based plots####################### 
  shinyjs::hide("m_clusterbased2")
  shinyjs::hide("m_clusterbased_box2")
  shinyjs::hide("m_clusterbased_box3")     
  shinyjs::hide("m_clusterbased_box4")
  
  ##################multiple conditionbased####################### 
  shinyjs::hide("m_conditionbased_box3")
  shinyjs::hide("m_conditionbased_box4")     
  shinyjs::hide("m_conditionbased_box5")      
  
  
  #####################Tab1##############################
  ######################data Input##################
  # datainput_multiple_sample_level<- eventReactive(input$multiple_sample_submit,{
  #   
  #   if (input$multiple_sample_format == "h5") {
  #     file1 <- input$multiple_sample_file[['datapath']]
  #     filesdir = dirname(file1)
  #     
  #     file.rename(file1, paste0(filesdir,'/',input$multiple_sample_file$name))
  #     upload_multiple_sample_file <- filesdir
  #     upload_multiple_sample_file_names <- input$multiple_sample_file$name
  #     }
  #   else if (input$multiple_sample_format == "MFB") {
  #     
  #     file1 <- input$multiple_sample_file_mfb[['datapath']]
  #     filesdir = dirname(file1)
  #     
  #     file.rename(file1, paste0(filesdir,'/',input$multiple_sample_file_mfb$name))
  #     upload_multiple_sample_file <- filesdir
  #     upload_multiple_sample_file_names <- input$multiple_sample_file_mfb$name
  #   }
  datainput_multiple_sample_level <- eventReactive(input$multiple_sample_submit, {
    run_logged_analysis(
      section = "Multiple Samples",
      action = "Load input data",
      params = capture_run_inputs(c("multiple_sample_")),
      expr = {
        format <- input$multiple_sample_format
        
        if (format == "h5" || format == "visium_bin") {
          req(input$multiple_sample_file)
          file1 <- input$multiple_sample_file[['datapath']]
          filesdir <- dirname(file1)
          renamed_paths <- file.path(filesdir, input$multiple_sample_file$name)
          file.rename(file1, renamed_paths)
          
          upload_multiple_sample_file <- input$multiple_sample_file
          upload_multiple_sample_file_paths <- renamed_paths
          upload_multiple_sample_file_names <- input$multiple_sample_file$name
          
        } else if (format == "MFB") {
          req(input$multiple_sample_file_mfb)
          file1 <- input$multiple_sample_file_mfb[['datapath']]
          filesdir <- dirname(file1)
          renamed_paths <- file.path(filesdir, input$multiple_sample_file_mfb$name)
          file.rename(file1, renamed_paths)
          
          upload_multiple_sample_file <- input$multiple_sample_file_mfb
          upload_multiple_sample_file_paths <- renamed_paths
          upload_multiple_sample_file_names <- input$multiple_sample_file_mfb$name
          
        } else if (format == "exampledata") {
          upload_multiple_sample_file <- NULL
          upload_multiple_sample_file_paths <- NULL
          upload_multiple_sample_file_names <- NULL
          filesdir <- NULL
        }
        source_app_script("scripts/multiple_file_upload.R")
        datainput_multiple_sample(
          index_multiple_sample_file = upload_multiple_sample_file_paths,
          index_multiple_sample_file_names = upload_multiple_sample_file_names,
          index_multiple_sample_file1 = filesdir,
          index_multiple_sample_format = input$multiple_sample_format,
          index_multiple_sample_name = "",
          index_multiple_sample_spatial_mode = if (identical(input$multiple_sample_format, "visium_bin")) "visium_hd" else input$multiple_sample_spatial_mode,
          index_multiple_sample_hd_bin_size = input$multiple_sample_hd_bin_size
        )
      }
    )
  })
  observeEvent(datainput_multiple_sample_level(), {
    req(datainput_multiple_sample_level())
    
    result <- datainput_multiple_sample_level()
    
    if (result$is_valid) {
      shinyjs::hide("m_bf_box0")
      shinyjs::show("m_bf_box1")
      shinyjs::show("m_bf_box2")
      shinyjs::show("m_bf_box3")
      shinyjs::show("m_bf_box4")
      shinyjs::show("m_bf_box5")
      shinyjs::show("m_bf_box6")
    } else {
      shinyjs::show("m_bf_box0")
      shinyjs::show("m_bf_box1")
      shinyjs::hide("m_bf_box2")
      shinyjs::hide("m_bf_box3")
      shinyjs::hide("m_bf_box4")
      shinyjs::hide("m_bf_box5")
      shinyjs::hide("m_bf_box6")
    }
    
    output$text_level<- renderText({
      paste(datainput_multiple_sample_level()[[2]], collapse = "\n")
    })
    
    
  output$m_qc_before_filtering <- renderPlot({
    datainput_multiple_sample_level()[[3]]
  })
  ################m_QCplot############################
  observeEvent(input$download_m_qc_before_filtering, {
    showModal(modalDialog(
      title = strong("Download QC plot"),
      numericInput("m_qc_before_filtering_plot_height", label = h5("Figure height (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_qc_before_filtering_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_qc_before_filtering_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_qc_before_filtering_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_qc_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  
  output$m_qc_downloadoutput<- downloadHandler(
    filename = function(){
      paste("QC_before_filtering", input$m_qc_before_filtering_plot_type, sep = "")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_sample_level()[[3]], width = input$m_qc_before_filtering_plot_width, height = input$m_qc_before_filtering_plot_height, dpi = input$m_qc_before_filtering_plot_dpi, units = "in")
    }
  )
  
  
  output$multiple_cell_table<- renderDataTable(DT::datatable((datainput_multiple_sample_level()[[9]]),
                                                             options = list(
                                                               scrollX = TRUE,
                                                               pageLength = 10,
                                                               bFilter=0
                                                             ),rownames= FALSE, selection = "none"))
  
  output$download_multiple_cell_table <- downloadHandler(
    filename = function() { 
      paste("Number of cells", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_multiple_sample_level()[[9]], file)
    }
  )
  
  
  output$m_sf_before_filtering <- renderPlot({
    datainput_multiple_sample_level()[[8]]
  })
  
  #######################m_sf_plot#########################
  observeEvent(input$download_m_sf_before_filtering, {
    showModal(modalDialog(
      title = strong("Download Spatial plot"),
      numericInput("m_sf_before_filtering_plot_height", label = h5("Figure height (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_sf_before_filtering_plot_width", label = h5("Figure width (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_sf_before_filtering_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_sf_before_filtering_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_sf_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  
  output$m_sf_downloadoutput<- downloadHandler(
    filename = function(){
      paste("spatial_feature_plot", input$m_sf_before_filtering_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_sample_level()[[8]], width = input$m_sf_before_filtering_plot_width, height = input$m_sf_before_filtering_plot_height, dpi = input$m_sf_before_filtering_plot_dpi, units = "in")
    }
  )
  
  output$m_ff_before_filtering <- renderPlot({
    datainput_multiple_sample_level()[[4]]
  })
  
  #######################m_ff_plot#########################
  observeEvent(input$download_m_ff_before_filtering, {
    showModal(modalDialog(
      title = strong("Download feature-feature plot"),
      numericInput("m_ff_before_filtering_plot_height", label = h5("Figure height (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_ff_before_filtering_plot_width", label = h5("Figure width (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_ff_before_filtering_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_ff_before_filtering_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_ff_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  
  output$m_ff_downloadoutput<- downloadHandler(
    filename = function(){
      paste("feature_feature_relationships_plot", input$m_ff_before_filtering_plot_type, sep = "")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_sample_level()[[4]], width = input$m_ff_before_filtering_plot_width, height = input$m_ff_before_filtering_plot_height, dpi = input$m_ff_before_filtering_plot_dpi, units = "in")
    }
  )
  
  

  
  
  output$m_so_before_filtering <- create_object_download_handler(
    section = "Multiple Samples",
    action = "Download Seurat Object (Before QC)",
    filename_text = "seuart_object_before_qc.RDS",
    object_expr = datainput_multiple_sample_level()[[5]]
  )
  })
  ###############link to next tab###########################      
  observeEvent(input$link_m_qc_filtering, {
    newvalue <- "Sample Groups and QC Filtering"
    updateTabsetPanel(session, "multiple_tabsets", newvalue)
  }) 
  
  ##########################Tab1.2###############################          
  ####################groupnames##############################  
  output$group1_samples1 <- renderUI ({
    
    samples <- req(datainput_multiple_sample_level()[[6]])
    #samples <- samples[!samples == input$group2_samples]
    
    shinyWidgets::pickerInput(
      inputId = "group1_samples",
      label = "Select group1 sample(s)",
      choices = sort(samples),
      multiple = T,
      selected = sort(samples)[1],
      #options = list(`actions-box` = TRUE)
    )
    
  })
  
  output$group2_samples2 <- renderUI ({
    samples <- req(datainput_multiple_sample_level()[[6]])
    remaining_samples <- samples[!samples %in% input$group1_samples]
    
    shinyWidgets::pickerInput(
      inputId = "group2_samples",
      label = "Select group2 sample(s)",
      choices = sort(remaining_samples),
      multiple = T,
      selected = if (length(remaining_samples) > 0) sort(remaining_samples)[1] else NULL,
      #options = list(`actions-box` = TRUE)
    )
    
  })
  
  output$group3_samples3 <- renderUI ({
    samples <- req(datainput_multiple_sample_level()[[6]])
    remaining_samples <- samples[!samples %in% c(input$group1_samples, input$group2_samples)]
    
    shinyWidgets::pickerInput(
      inputId = "group3_samples",
      label = "Select group3 sample(s)",
      choices = sort(remaining_samples),
      multiple = T,
      selected = if (length(remaining_samples) > 0) sort(remaining_samples)[1] else NULL,
      #options = list(`actions-box` = TRUE)
    )
    
  })
  
  output$group4_samples4 <- renderUI ({
    samples <- req(datainput_multiple_sample_level()[[6]])
    remaining_samples <- samples[!samples %in% c(input$group1_samples, input$group2_samples, input$group3_samples)]
    
    shinyWidgets::pickerInput(
      inputId = "group4_samples",
      label = "Select group4 sample(s)",
      choices = sort(remaining_samples),
      multiple = T,
      selected = if (length(remaining_samples) > 0) sort(remaining_samples)[1] else NULL,
      #options = list(`actions-box` = TRUE)
    )
    
  })
  
  output$group5_samples5 <- renderUI ({
    samples <- req(datainput_multiple_sample_level()[[6]])
    remaining_samples <- samples[!samples %in% c(input$group1_samples, input$group2_samples, input$group3_samples, input$group4_samples)]
    
    shinyWidgets::pickerInput(
      inputId = "group5_samples",
      label = "Select group5 sample(s)",
      choices = sort(remaining_samples),
      multiple = T,
      selected = if (length(remaining_samples) > 0) sort(remaining_samples)[1] else NULL,
      #options = list(`actions-box` = TRUE)
    )
    
  })
  
  
  output$group6_samples6 <- renderUI ({
    samples <- req(datainput_multiple_sample_level()[[6]])
    remaining_samples <- samples[!samples %in% c(input$group1_samples, input$group2_samples, input$group3_samples, input$group4_samples, input$group5_samples)]
    
    shinyWidgets::pickerInput(
      inputId = "group6_samples",
      label = "Select group6 sample(s)",
      choices = sort(remaining_samples),
      multiple = T,
      selected = if (length(remaining_samples) > 0) sort(remaining_samples)[1] else NULL,
      #options = list(`actions-box` = TRUE)
    )
    
  })
  
  
  
  
  ##############multiple QC after filtering###################   
  datainput_multiple_qc_filter_level <- eventReactive(input$multiple_sample_qc_filtering,{
    run_logged_analysis(
      section = "Multiple Samples",
      action = "QC filtering",
      params = capture_run_inputs(c("multiple_sample_", "multiple_group_", "group")),
      expr = {
        source_app_script("scripts/multiple_qc_filter.R")
        datainput_multiple_qc_filter(
          index_multiple_qc_input = datainput_multiple_sample_level()[[5]],
          index_multiple_qc_input1 = datainput_multiple_sample_level()[[7]],
          index_multiple_group_count = input$multiple_group_count,
          index_group1_name = input$group1_name,
          index_group1_samples = input$group1_samples,
          index_group2_name = input$group2_name,
          index_group2_samples = input$group2_samples,
          index_group3_name = input$group3_name,
          index_group3_samples = input$group3_samples,
          index_group4_name = input$group4_name,
          index_group4_samples = input$group4_samples,
          index_group5_name = input$group5_name,
          index_group5_samples = input$group5_samples,
          index_group6_name = input$group6_name,
          index_group6_samples = input$group6_samples,
          index_multiple_sample_min_count = input$multiple_sample_min_count,
          index_multiple_sample_max_count = input$multiple_sample_max_count,
          index_multiple_sample_min_ncount = input$multiple_sample_min_ncount,
          index_multiple_sample_max_ncount = input$multiple_sample_max_ncount,
          index_multiple_sample_max_mito_perc = input$multiple_sample_max_mito_perc
        )
      }
    )
  })
  
  
  
  output$m_qc_after_filtering<- renderPlot({
    datainput_multiple_qc_filter_level()[1]
  })
  
  
  #################m_QC_after_filtering_plot############
  observeEvent(input$download_m_qc_after_filtering, {
    showModal(modalDialog(
      title = strong("Download samples QC plot"),
      numericInput("m_qc_after_filtering_plot_height", label = h5("Figure height (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_qc_after_filtering_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_qc_after_filtering_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_qc_after_filtering_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_qc_after_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_qc_after_downloadoutput<- downloadHandler(
    filename = function(){
      paste("QC_after_filtering_sample_based", input$m_qc_after_filtering_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_qc_filter_level()[[1]], width = input$m_qc_after_filtering_plot_width, height = input$m_qc_after_filtering_plot_height, dpi = input$m_qc_after_filtering_plot_dpi, units = "in")
    }
  )
  
  
  
  output$m_qc_after_filtering2<- renderPlot({
    datainput_multiple_qc_filter_level()[2]
  })
  
  observeEvent(input$download_m_qc_after_filtering2, {
    showModal(modalDialog(
      title = strong("Download group QC plot"),
      numericInput("m_qc_after_filtering2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_qc_after_filtering2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_qc_after_filtering2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_qc_after_filtering2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_qc_after_downloadoutput2", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_qc_after_downloadoutput2<- downloadHandler(
    filename = function(){
      paste("QC_after_filtering_group_based", input$m_qc_after_filtering2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_qc_filter_level()[[2]], width = input$m_qc_after_filtering2_plot_width, height = input$m_qc_after_filtering2_plot_height, dpi = input$m_qc_after_filtering2_plot_dpi, units = "in")
    }
  )
  
  
  
  output$m_qc_after_filtering3<- renderPlot({
    datainput_multiple_qc_filter_level()[3]
  })
  
  
  observeEvent(input$download_m_qc_after_filtering3, {
    showModal(modalDialog(
      title = strong("Download samples bar plot"),
      numericInput("m_qc_after_filtering3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 10, width = "300px"),
      numericInput("m_qc_after_filtering3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_qc_after_filtering3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_qc_after_filtering3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_qc_after_downloadoutput3", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_qc_after_downloadoutput3<- downloadHandler(
    filename = function(){
      paste("Bar_plot_sample_based", input$m_qc_after_filtering3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_qc_filter_level()[[3]], width = input$m_qc_after_filtering3_plot_width, height = input$m_qc_after_filtering3_plot_height, dpi = input$m_qc_after_filtering3_plot_dpi, units = "in")
    }
  )
  
  
  output$m_qc_after_filtering4<- renderPlot({
    datainput_multiple_qc_filter_level()[4]
  })
  
  observeEvent(input$download_m_qc_after_filtering4, {
    showModal(modalDialog(
      title = strong("Download groups bar plot"),
      numericInput("m_qc_after_filtering4_plot_height", label = h5("Figure height (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_qc_after_filtering4_plot_width", label = h5("Figure width (upto 49 inces)"), value = 6, width = "300px"),
      numericInput("m_qc_after_filtering4_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_qc_after_filtering4_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_qc_after_downloadoutput4", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_qc_after_downloadoutput4<- downloadHandler(
    filename = function(){
      paste("Bar_plot_group_based", input$m_qc_after_filtering4_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_qc_filter_level()[[4]], width = input$m_qc_after_filtering4_plot_width, height = input$m_qc_after_filtering4_plot_height, dpi = input$m_qc_after_filtering4_plot_dpi, units = "in")
    }
  )
  
  
  output$multiple_cell_table_after_qc<- renderDataTable(DT::datatable((datainput_multiple_qc_filter_level()[[5]]),
                                                                      options = list(
                                                                        scrollX = TRUE,
                                                                        pageLength = 10,
                                                                        bFilter=0
                                                                      ),rownames= FALSE, selection = "none"))
  
  output$download_multiple_cell_table_after_qc <- downloadHandler(
    filename = function() { 
      paste("Number of cells in samples after qc", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_multiple_qc_filter_level()[[5]], file)
    }
  )
  
  output$multiple_cell_table_after_qc2<- renderDataTable(DT::datatable((datainput_multiple_qc_filter_level()[[6]]),
                                                                       options = list(
                                                                         scrollX = TRUE,
                                                                         pageLength = 10,
                                                                         bFilter=0
                                                                       ),rownames= FALSE, selection = "none"))
  
  output$download_multiple_cell_table_after_qc2 <- downloadHandler(
    filename = function() { 
      paste("Number of cells in groups after qc", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_multiple_qc_filter_level()[[6]], file)
    }
  )  
  
  
  output$m_qc_after_filtering5<- renderPlot({
    datainput_multiple_qc_filter_level()[8]
  })
  
  observeEvent(input$download_m_qc_after_filtering5, {
    showModal(modalDialog(
      title = strong("Download group QC plot"),
      numericInput("m_qc_after_filtering5_plot_height", label = h5("Figure height (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_qc_after_filtering5_plot_width", label = h5("Figure width (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_qc_after_filtering5_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_qc_after_filtering5_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_qc_after_downloadoutput5", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_qc_after_downloadoutput5<- downloadHandler(
    filename = function(){
      paste("Spatial_QC_after_filtering_group_based", input$m_qc_after_filtering5_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_qc_filter_level()[[8]], width = input$m_qc_after_filtering5_plot_width, height = input$m_qc_after_filtering5_plot_height, dpi = input$m_qc_after_filtering5_plot_dpi, units = "in")
    }
  )
  
  
  ###################save seurat object after qc###################
  output$m_so_after_filtering <- create_object_download_handler(
    section = "Multiple Samples",
    action = "Download Seurat Object (After QC)",
    filename_text = "multiple_sample_seuart_object_after_qc.RDS",
    object_expr = datainput_multiple_qc_filter_level()[[7]]
  )
  
  ###############link to next tab###########################      
  observeEvent(input$link_m_normalization, {
    newvalue <- "Normalization and PCA Analysis"
    updateTabsetPanel(session, "multiple_tabsets", newvalue)
  })       
  
  
  
  ##########################Tab1.3###############################      
  ##############multiple Normalization & PCA###################      
  datainput_multiple_normalization_pca_level <- eventReactive(input$multiple_sample_normalization,{
    run_logged_analysis(
      section = "Multiple Samples",
      action = "Normalization and PCA",
      params = capture_run_inputs(c("multiple_sample_")),
      expr = {
        source_app_script("scripts/multiple_normalization_pca.R")
        datainput_multiple_normalization_pca(index_multiple_normalization_pca_input = datainput_multiple_qc_filter_level()[[7]], index_multiple_sample_normalization_method = input$multiple_sample_normalization_method, multiple_sample_normalization_method1 = input$multiple_sample_normalization_method1, index_multiple_sample_scale_factor=input$multiple_sample_scale_factor, index_multiple_sample_var_genes = input$multiple_sample_var_genes,  index_multiple_sample_var_genes1 = input$multiple_sample_var_genes1, index_multiple_sample_normalization_variable_genes=input$multiple_sample_normalization_variable_genes, index_multiple_sample_pca_dim=input$multiple_sample_pca_dim, index_multiple_sample_assay=input$multiple_sample_assay)
      }
    )
  })
  
  
  output$m_pca_plot<-renderPlot({
    datainput_multiple_normalization_pca_level()[1]
  })
  
  observeEvent(input$download_m_pca_plot, {
    showModal(modalDialog(
      title = strong("Download PCA Plot"),
      numericInput("m_pca_plot_height", label = h5("Figure height (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_pca_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_pca_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_pca_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_pca_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_pca_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("After_normalization_PCA_plot", input$m_pca_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_normalization_pca_level()[[1]], width = input$m_pca_plot_width, height = input$m_pca_plot_height, dpi = input$m_pca_plot_dpi, units = "in")
    }
  )
  
  output$m_elbow_plot<-renderPlot({
    datainput_multiple_normalization_pca_level()[2]
  })
  
  observeEvent(input$download_m_elbow_plot, {
    showModal(modalDialog(
      title = strong("Download Variable Features Plot"),
      numericInput("m_elbow_plot_height", label = h5("Figure height (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_elbow_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_elbow_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_elbow_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_elbow_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_elbow_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("After_normalization_Elbow", input$m_elbow_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_normalization_pca_level()[[2]], width = input$m_elbow_plot_width, height = input$m_elbow_plot_height, dpi = input$m_elbow_plot_dpi, units = "in")
    }
  )
  
  output$m_pca2_plot<-renderPlot({
    datainput_multiple_normalization_pca_level()[3]
  })
  
  observeEvent(input$download_m_pca2_plot, {
    showModal(modalDialog(
      title = strong("Download PCA Plot"),
      numericInput("m_pca2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_pca2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_pca2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_pca2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_pca2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_pca2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("After_normalization_PCA_plot_sample_based", input$m_pca2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_normalization_pca_level()[[3]], width = input$m_pca2_plot_width, height = input$m_pca2_plot_height, dpi = input$m_pca2_plot_dpi, units = "in")
    }
  )
  
  
  output$m_pca3_plot<-renderPlot({
    datainput_multiple_normalization_pca_level()[4]
  })
  
  
  observeEvent(input$download_m_pca3_plot, {
    showModal(modalDialog(
      title = strong("Download PCA Plot"),
      numericInput("m_pca3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_pca3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_pca3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_pca3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_pca3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_pca3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("After_normalization_PCA_plot_group_based", input$m_pca3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_normalization_pca_level()[[4]], width = input$m_pca3_plot_width, height = input$m_pca3_plot_height, dpi = input$m_pca3_plot_dpi, units = "in")
    }
  )
  
  
  
  ###################save seurat object after normalization###################
  output$m_normalization <- create_object_download_handler(
    section = "Multiple Samples",
    action = "Download Seurat Object (After Normalization)",
    filename_text = "multiple_sample_seuart_object_after_normalization.RDS",
    object_expr = datainput_multiple_normalization_pca_level()[[5]]
  )
  
  #####################################link to next tab###########################      
  observeEvent(input$link_m_clustering, {
    newvalue <- "Clustering"
    updateTabsetPanel(session, "multiple_tabsets", newvalue)
  })       
  
  
  
  #####################################################Tab1.4####################      
  ########################################multiple Clustering###################      
  datainput_multiple_clustering_level <- eventReactive(input$multiple_sample_clustering,{
    run_logged_analysis(
      section = "Multiple Samples",
      action = "Clustering",
      params = capture_run_inputs(c("multiple_sample_", "m_clustering")),
      expr = {
        source_app_script("scripts/multiple_clustering.R")
        datainput_multiple_clustering(index_multiple_clustering_input = datainput_multiple_normalization_pca_level()[[5]], index_multiple_sample_normalization_method = input$multiple_sample_normalization_method, index_m_clustering1 = input$m_clustering1, index_m_clustering2 = input$m_clustering2, index_m_clustering3 = input$m_clustering3, index_m_clustering4 = input$m_clustering4, index_m_clustering5 = input$m_clustering5, index_m_clustering6 = input$m_clustering6, index_m_clustering7 = input$m_clustering7, index_m_clustering8 = input$m_clustering8, index_m_clustering9 = input$m_clustering9, index_m_clustering10 = input$m_clustering10, index_m_clustering11 = input$m_clustering11, index_m_clustering12 = input$m_clustering12)
      }
    )
  })
  
  output$m_umap_tsne1_plot<-renderPlot({
    datainput_multiple_clustering_level()[1]
  })
  observeEvent(input$download_m_umap_tsne1_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_plot", input$m_umap_tsne1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[1]], width = input$m_umap_tsne1_plot_width, height = input$m_umap_tsne1_plot_height, dpi = input$m_umap_tsne1_plot_dpi, units = "in")
    }
  )
  
  
  output$m_umap_tsne_bar1_plot<-renderPlot({
    datainput_multiple_clustering_level()[2]
  }) 
  observeEvent(input$download_m_umap_tsne_bar1_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne_bar1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne_bar1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne_bar1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne_bar1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne_bar1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne_bar1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_based_bar_plot", input$m_umap_tsne_bar1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[2]], width = input$m_umap_tsne_bar1_plot_width, height = input$m_umap_tsne_bar1_plot_height, dpi = input$m_umap_tsne_bar1_plot_dpi, units = "in")
    }
  )
  
  
  output$m_umap_tsne2_plot<-renderPlot({
    datainput_multiple_clustering_level()[3]
  })
  observeEvent(input$download_m_umap_tsne2_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Condition_based_plot", input$m_umap_tsne2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[3]], width = input$m_umap_tsne2_plot_width, height = input$m_umap_tsne2_plot_height, dpi = input$m_umap_tsne2_plot_dpi, units = "in")
    }
  )
  
  
  output$m_umap_tsne_bar2_plot<-renderPlot({
    datainput_multiple_clustering_level()[4]
  })
  observeEvent(input$download_m_umap_tsne_bar2_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne_bar2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne_bar2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne_bar2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne_bar2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne_bar2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne_bar2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Condition_based_bar_plot", input$m_umap_tsne_bar2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[4]], width = input$m_umap_tsne_bar2_plot_width, height = input$m_umap_tsne_bar2_plot_height, dpi = input$m_umap_tsne_bar2_plot_dpi, units = "in")
    }
  )
  
  
  output$m_umap_tsne3_plot<-renderPlot({
    datainput_multiple_clustering_level()[5]
  })
  observeEvent(input$download_m_umap_tsne3_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Sample_based_plot", input$m_umap_tsne3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[5]], width = input$m_umap_tsne3_plot_width, height = input$m_umap_tsne3_plot_height, dpi = input$m_umap_tsne3_plot_dpi, units = "in")
    }
  )	  
  
  
  output$m_umap_tsne_bar3_plot<-renderPlot({
    datainput_multiple_clustering_level()[6]
  })
  observeEvent(input$download_m_umap_tsne_bar3_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne_bar3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne_bar3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne_bar3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne_bar3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne_bar3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne_bar3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Sample_based_bar_plot", input$m_umap_tsne_bar3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[6]], width = input$m_umap_tsne_bar3_plot_width, height = input$m_umap_tsne_bar3_plot_height, dpi = input$m_umap_tsne_bar3_plot_dpi, units = "in")
    }
  )
  
  output$m_clustering_table1<- renderDataTable(DT::datatable((datainput_multiple_clustering_level()[[7]]),
                                                             options = list(
                                                               scrollX = TRUE,
                                                               pageLength = 10,
                                                               bFilter=0
                                                             ),rownames= FALSE, selection = "none"))
  
  output$download_m_clustering_table1 <- downloadHandler(
    filename = function() { 
      paste("Number of cells in clusters", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_multiple_clustering_level()[[7]], file)
    }
  ) 
  
  output$m_clustering_table2<- renderDataTable(DT::datatable((datainput_multiple_clustering_level()[[8]]),
                                                             options = list(
                                                               scrollX = TRUE,
                                                               pageLength = 10,
                                                               bFilter=0
                                                             ),rownames= FALSE, selection = "none"))
  
  output$download_m_clustering_table2 <- downloadHandler(
    filename = function() { 
      paste("Number of cells in clusters based on condition", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_multiple_clustering_level()[[8]], file)
    }
  ) 
  
  output$m_clustering_table3<- renderDataTable(DT::datatable((datainput_multiple_clustering_level()[[9]]),
                                                             options = list(
                                                               scrollX = TRUE,
                                                               pageLength = 10,
                                                               bFilter=0
                                                             ),rownames= FALSE, selection = "none"))
  
  output$download_m_clustering_table3 <- downloadHandler(
    filename = function() { 
      paste("Number of cells in clusters based on samples", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_multiple_clustering_level()[[9]], file)
    }
  ) 
  
  
  output$m_umap_tsne3_plot<-renderPlot({
    datainput_multiple_clustering_level()[5]
  })
  observeEvent(input$download_m_umap_tsne3_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Sample_based_plot", input$m_umap_tsne3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[5]], width = input$m_umap_tsne3_plot_width, height = input$m_umap_tsne3_plot_height, dpi = input$m_umap_tsne3_plot_dpi, units = "in")
    }
  )	  
  
  
  
  output$m_umap_tsne4_plot<-renderPlot({
    datainput_multiple_clustering_level()[15]
  })
  observeEvent(input$download_m_umap_tsne4_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne4_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne4_plot_width", label = h5("Figure width (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_umap_tsne4_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne4_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne4_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne4_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Spatial_plot", input$m_umap_tsne4_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[15]], width = input$m_umap_tsne4_plot_width, height = input$m_umap_tsne4_plot_height, dpi = input$m_umap_tsne4_plot_dpi, units = "in")
    }
  )	  
  
  output$m_umap_tsne5_plot<-renderPlot({
    datainput_multiple_clustering_level()[16]
  })
  observeEvent(input$download_m_umap_tsne5_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne5_plot_height", label = h5("Figure height (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_umap_tsne5_plot_width", label = h5("Figure width (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_umap_tsne5_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne5_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne5_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne5_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("SPatial_plot_split_by_clusters", input$m_umap_tsne5_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[16]], width = input$m_umap_tsne5_plot_width, height = input$m_umap_tsne5_plot_height, dpi = input$m_umap_tsne5_plot_dpi, units = "in")
    }
  )	  
  
  output$m_umap_tsne6_plot<-renderPlot({
    datainput_multiple_clustering_level()[17]
  })
  observeEvent(input$download_m_umap_tsne6_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne6_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne6_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_umap_tsne6_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne6_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne6_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne6_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_based_plot_split_by_condition", input$m_umap_tsne6_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[17]], width = input$m_umap_tsne6_plot_width, height = input$m_umap_tsne6_plot_height, dpi = input$m_umap_tsne6_plot_dpi, units = "in")
    }
  )	  
  
  output$m_umap_tsne7_plot<-renderPlot({
    datainput_multiple_clustering_level()[18]
  })
  observeEvent(input$download_m_umap_tsne7_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne7_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne7_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_umap_tsne7_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne7_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne7_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne7_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_based_plot_split_by_samples", input$m_umap_tsne3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[18]], width = input$m_umap_tsne7_plot_width, height = input$m_umap_tsne7_plot_height, dpi = input$m_umap_tsne7_plot_dpi, units = "in")
    }
  )	 
  
  output$m_umap_tsne8_plot<-renderPlot({
    datainput_multiple_clustering_level()[19]
  })
  observeEvent(input$download_m_umap_tsne8_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_umap_tsne8_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_umap_tsne8_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_umap_tsne8_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_umap_tsne8_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_umap_tsne8_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_umap_tsne8_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_split_by_condition", input$m_umap_tsne8_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clustering_level()[[19]], width = input$m_umap_tsne8_plot_width, height = input$m_umap_tsne8_plot_height, dpi = input$m_umap_tsne8_plot_dpi, units = "in")
    }
  )	  
  
  ###################save seurat object after clustering###################
  output$m_clustering <- create_object_download_handler(
    section = "Multiple Samples",
    action = "Download Seurat Object (After Clustering)",
    filename_text = "multiple_sample_seuart_object_after_clustering.RDS",
    object_expr = datainput_multiple_clustering_level()[[10]]
  )
  
  
  
  #####################################link to next tab###########################   
  observeEvent(input$link_m_marker, {
    newvalue <- "Markers Identification"
    updateTabsetPanel(session, "multiple_tabsets", newvalue)
  })  
  
  ##########################Tab1.6###############################      
  ##############multiple Marker identification###################    
  observeEvent(input$multiple_sample_marker,{
    if(input$m_marker1 == 1){
      shinyjs::show("m_marker_box5")
      shinyjs::show("m_marker_box6")
      shinyjs::show("m_marker_box7")
    }
    else{
      shinyjs::show("m_marker_box5")
      shinyjs::hide("m_marker_box6")
      shinyjs::show("m_marker_box7")
    }
    
  })
  
  output$m_marker_6 <- renderUI ({
    clusters <- req(datainput_multiple_clustering_level()[[11]])
    
    shinyWidgets::pickerInput(
      inputId = "m_marker6",
      label = "Select one cluster for analsysis",
      choices = sort(clusters),
      multiple = F,
      options = list(`actions-box` = TRUE))
  })
  
  output$m_marker_7 <- renderUI ({
    clusters <- req(datainput_multiple_clustering_level()[[11]])
    clusters <- clusters[!clusters == input$m_marker6]
    shinyWidgets::pickerInput(
      inputId = "m_marker7",
      label = "Identify markers distinguishing a cluster from other selected clusters",
      choices = sort(clusters),
      multiple = T,
      selected = sort(clusters)[1],
      options = list(`actions-box` = TRUE))
  })
  
  output$m_marker_8 <- renderUI ({
    clusters <- req(datainput_multiple_clustering_level()[[11]])
    
    shinyWidgets::pickerInput(
      inputId = "m_marker8",
      label = "Select one cluster to define markers",
      choices = sort(clusters),
      multiple = F,
      options = list(`actions-box` = TRUE))
  })
  
  output$m_marker_9 <- renderUI ({
    clusters <- req(datainput_multiple_clustering_level()[[11]])
    clusters <- clusters[!clusters == input$m_marker8]
    shinyWidgets::pickerInput(
      inputId = "m_marker9",
      label = "Select the cluster to find the conserved markers between two clusters",
      choices = sort(clusters),
      multiple = T,
      selected = sort(clusters)[1],
      options = list(`actions-box` = TRUE))
  })
  
  
  datainput_multiple_marker_level <- eventReactive(input$multiple_sample_marker,{
    run_logged_analysis(
      section = "Multiple Samples",
      action = "Marker identification",
      params = capture_run_inputs(c("multiple_sample_", "m_marker")),
      expr = {
        source_app_script("scripts/multiple_marker.R")
        datainput_multiple_marker(index_multiple_marker_input = datainput_multiple_clustering_level()[[10]], index_m_marker1 = input$m_marker1, index_m_marker2 = input$m_marker2, index_m_marker3 = input$m_marker3, index_m_marker4 = input$m_marker4, index_m_marker5 = input$m_marker5, index_m_marker6 = input$m_marker6, index_m_marker7 = input$m_marker7, index_m_marker8 = input$m_marker8, index_m_marker9 = input$m_marker9, index_m_marker10 = input$m_marker10, index_multiple_sample_normalization_method = input$multiple_sample_normalization_method)
      }
    )
  })
  
  
  output$m_marker1_table<- renderDataTable(DT::datatable((datainput_multiple_marker_level()[[1]]),
                                                         options = list(
                                                           scrollX = TRUE,
                                                           pageLength = 10,
                                                           dom = "Blfrtip"
                                                           #bFilter=0
                                                         ),rownames= FALSE, selection = "none"))
  
  
  output$download_m_marker1_table <- downloadHandler(
    filename = function() {
      paste("Number_of_identified_markers_or_differentially_expressed_genes", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_multiple_marker_level()[[1]], file)
    }
  )
  
  
  output$m_marker1_plot<-renderPlot({
    datainput_multiple_marker_level()[3]
  })
  
  
  observeEvent(input$download_m_marker1_plot, {
    showModal(modalDialog(
      title = strong("Download Heatmap"),
      numericInput("m_marker1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_marker1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_marker1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_marker1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_marker1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_marker1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste( "Heatmap_with_Top5_expressed_genes", input$m_marker1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_marker_level()[[3]], width = input$m_marker1_plot_width, height = input$m_marker1_plot_height, dpi = input$m_marker1_plot_dpi, units = "in")
    }
  )
  
  
  ###################save seurat object after doublet removal###################
  output$m_marker <- create_object_download_handler(
    section = "Multiple Samples",
    action = "Download Seurat Object (After Marker Identification)",
    filename_text = "multiple_sample_seuart_object_after_marker_identification.RDS",
    object_expr = datainput_multiple_marker_level()[[2]]
  )
  #####################################link to next tab###########################     
  observeEvent(input$link_m_prediction, {
    newvalue <- "Cell Type Prediction"
    updateTabsetPanel(session, "multiple_tabsets", newvalue)
  })  
  
  ##########################Tab1.7###############################      
  ##############multiple cell type################### 
  output$m_celltype7 <- renderUI({
    numberofclusters <- as.integer(length(levels(datainput_multiple_marker_level()[[2]])))
    lapply(1:numberofclusters, function(i) {
      column(3, textInput(paste("mcelltypenames", levels(datainput_multiple_marker_level()[[2]])[i], sep = ""),
                          paste("Cluster", levels(datainput_multiple_marker_level()[[2]])[i]), value = paste("Cluster", levels(datainput_multiple_marker_level()[[2]])[i])))
    })
  })
  
  observeEvent(input$multiple_sample_celltype,{
    if(input$m_celltype1 == 1){
      shinyjs::show("m_celltype_box7")
      shinyjs::show("m_celltype_box8")
      shinyjs::hide("m_celltype_box9")
      shinyjs::show("m_celltype_box10")
      shinyjs::show("m_celltype_box11")
      shinyjs::show("m_celltype9")
      shinyjs::hide("m_celltype10")
    }
    else if (input$m_celltype1 == 2){
      shinyjs::show("m_celltype_box7")
      shinyjs::show("m_celltype_box8")
      shinyjs::show("m_celltype_box9")
      shinyjs::show("m_celltype_box10")
      shinyjs::show("m_celltype_box11")
      shinyjs::hide("m_celltype9")
      shinyjs::show("m_celltype10")
    }
    else if (input$m_celltype1 == 3){
      shinyjs::show("m_celltype_box7")
      shinyjs::hide("m_celltype_box8")
      shinyjs::hide("m_celltype_box9")
      shinyjs::show("m_celltype_box10")
      shinyjs::show("m_celltype_box11")
      shinyjs::hide("m_celltype9")
      shinyjs::hide("m_celltype10")
    }
    else if (input$m_celltype1 == 4){
      shinyjs::show("m_celltype_box7")
      shinyjs::hide("m_celltype_box8")
      shinyjs::hide("m_celltype_box9")
      shinyjs::show("m_celltype_box10")
      shinyjs::show("m_celltype_box11")
      shinyjs::hide("m_celltype9")
      shinyjs::hide("m_celltype10")
    }
    
  })
  
  
  datainput_multiple_celltype_level <- eventReactive(input$multiple_sample_celltype,{
    run_logged_analysis(
      section = "Multiple Samples",
      action = "Cell type annotation",
      params = capture_run_inputs(c("multiple_sample_", "m_celltype", "mcelltypenames")),
      expr = {
        source_app_script("scripts/multiple_celltype.R")
        datainput_multiple_celltype(index_multiple_celltype_input = datainput_multiple_marker_level()[[2]], index_cell_markers = datainput_multiple_marker_level()[[1]], index_m_celltype1 = input$m_celltype1, index_m_celltype2 = input$m_celltype2, index_m_celltype3 = input$m_celltype3, index_m_celltype4 = input$m_celltype4, index_m_celltype5 = input$m_celltype5, index_m_celltype6 = input$m_celltype6, index_m_celltype7 = c(input$mcelltypenames0,input$mcelltypenames1,input$mcelltypenames2,input$mcelltypenames3,input$mcelltypenames4,input$mcelltypenames5,input$mcelltypenames6,input$mcelltypenames7,input$mcelltypenames8,input$mcelltypenames9,input$mcelltypenames10,input$mcelltypenames11,input$mcelltypenames12,input$mcelltypenames13,input$mcelltypenames14,input$mcelltypenames15,input$mcelltypenames16,input$mcelltypenames17,input$mcelltypenames18,input$mcelltypenames19,input$mcelltypenames20,input$mcelltypenames21,input$mcelltypenames22,input$mcelltypenames23,input$mcelltypenames24,input$mcelltypenames25,input$mcelltypenames26,input$mcelltypenames27,input$mcelltypenames28,input$mcelltypenames29,input$mcelltypenames30,input$mcelltypenames31,input$mcelltypenames32,input$mcelltypenames33,input$mcelltypenames34,input$mcelltypenames35,input$mcelltypenames36,input$mcelltypenames37,input$mcelltypenames38,input$mcelltypenames39,input$mcelltypenames40,input$mcelltypenames41,input$mcelltypenames42,input$mcelltypenames43,input$mcelltypenames44,input$mcelltypenames45,input$mcelltypenames46,input$mcelltypenames47,input$mcelltypenames48,input$mcelltypenames49,input$mcelltypenames50,input$mcelltypenames51,input$mcelltypenames52,input$mcelltypenames53,input$mcelltypenames54,input$mcelltypenames55,input$mcelltypenames56,input$mcelltypenames57,input$mcelltypenames58,input$mcelltypenames59,input$mcelltypenames60,input$mcelltypenames61,input$mcelltypenames62,input$mcelltypenames63,input$mcelltypenames64,input$mcelltypenames65,input$mcelltypenames66,input$mcelltypenames67,input$mcelltypenames68,input$mcelltypenames69,input$mcelltypenames70,input$mcelltypenames71,input$mcelltypenames72,input$mcelltypenames73,input$mcelltypenames74,input$mcelltypenames75,input$mcelltypenames76,input$mcelltypenames77,input$mcelltypenames78,input$mcelltypenames79,input$mcelltypenames80,input$mcelltypenames81,input$mcelltypenames82,input$mcelltypenames83,input$mcelltypenames84,input$mcelltypenames85,input$mcelltypenames86,input$mcelltypenames87,input$mcelltypenames88,input$mcelltypenames89,input$mcelltypenames90,input$mcelltypenames91,input$mcelltypenames92,input$mcelltypenames93,input$mcelltypenames94,input$mcelltypenames95,input$mcelltypenames96,input$mcelltypenames97,input$mcelltypenames98,input$mcelltypenames99), index_m_celltype8 = input$m_celltype8, index_m_celltype9 = input$m_celltype_splitby, index_m_clustering6 = input$m_clustering6, index_multiple_sample_normalization_method = input$multiple_sample_normalization_method)
      }
    )
  })
  output$m_celltype1_plot<-renderPlot({
    datainput_multiple_celltype_level()[[5]]
  })
  
  observeEvent(input$download_m_celltype1_plot, {
    showModal(modalDialog(
      title = strong("Download Celltype"),
      numericInput("m_celltype1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_celltype1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 16, width = "300px"),
      numericInput("m_celltype1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_celltype1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_celltype1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_celltype1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Dimplot_with_celltype", input$m_celltype1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_celltype_level()[[5]], width = input$m_celltype1_plot_width, height = input$m_celltype1_plot_height, dpi = input$m_celltype1_plot_dpi, units = "in")
    }
  )
  
  
  output$m_celltype4_plot<-renderPlot({
    datainput_multiple_celltype_level()[[6]]
  })
  
  observeEvent(input$download_m_celltype4_plot, {
    showModal(modalDialog(
      title = strong("Download Celltype"),
      numericInput("m_celltype4_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_celltype4_plot_width", label = h5("Figure width (upto 49 inces)"), value = 16, width = "300px"),
      numericInput("m_celltype4_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_celltype4_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_celltype4_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_celltype4_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Spatial_Dimplot_with_celltype", input$m_celltype4_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_celltype_level()[[6]], width = input$m_celltype4_plot_width, height = input$m_celltype4_plot_height, dpi = input$m_celltype4_plot_dpi, units = "in")
    }
  )
  
  output$m_celltype1_table<- renderDataTable(DT::datatable((datainput_multiple_celltype_level()[[7]]),
                                                           options = list(
                                                             scrollX = TRUE,
                                                             pageLength = 10,
                                                             dom = "Blfrtip"
                                                             #bFilter=0
                                                           ),rownames= TRUE, selection = "none"))
  
  output$download_m_celltype1_table <- downloadHandler(
    filename = function() { 
      paste("predicted_celltype_Scores", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_multiple_celltype_level()[[7]], file)
    }
  )
  
  output$m_celltype2_plot<-renderPlot({
    datainput_multiple_celltype_level()[[8]]
  })
  
  observeEvent(input$download_m_celltype2_plot, {
    showModal(modalDialog(
      title = strong("Download Heatmap"),
      numericInput("m_celltype2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_celltype2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_celltype2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_celltype2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_celltype2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_celltype2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("score_plots", input$m_celltype2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_celltype_level()[[8]], width = input$m_celltype2_plot_width, height = input$m_celltype2_plot_height, dpi = input$m_celltype2_plot_dpi, units = "in")
    }
  )
  
  
  output$m_celltype3_plot<-renderPlot({
    datainput_multiple_celltype_level()[[9]]
  })
  
  observeEvent(input$download_m_celltype3_plot, {
    showModal(modalDialog(
      title = strong("Download"),
      numericInput("m_celltype3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_celltype3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_celltype3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_celltype3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_celltype3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_celltype3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("score_plots", input$m_celltype3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_celltype_level()[[9]], width = input$m_celltype3_plot_width, height = input$m_celltype3_plot_height, dpi = input$m_celltype3_plot_dpi, units = "in")
    }
  )
  
 
  
  ###################save seurat object after doublet###################
  output$m_celltype <- create_object_download_handler(
    section = "Multiple Samples",
    action = "Download Seurat Object (After Cell Type Annotation)",
    filename_text = "multiple_sample_seuart_object_after_celltypes.RDS",
    object_expr = datainput_multiple_celltype_level()[[1]]
  )
  #####################################link to next tab###########################     
  observeEvent(input$link_m_clusterbased, {
    newvalue <- "Cluster-Based Plots"
    updateTabsetPanel(session, "multiple_tabsets", newvalue)
  })  
  
  
  ###################################Tab1.8###############################  
  ###########################Cluster-based plots####################### 
  observe({
    if (input$m_clusterbased1 == "gene_name_list") {
      shinyjs::show("m_clusterbased2")
    } else {
      shinyjs::hide("m_clusterbased2")
    }
  })
  
  observeEvent(input$multiple_sample_clusterbased, {
    if (input$m_clusterbased4 == "seurat_clusters") {
      shinyjs::show("m_clusterbased_box3")
    } else {
      shinyjs::hide("m_clusterbased_box3")
    }
  })
  
  observeEvent(input$multiple_sample_clusterbased, {
    shinyjs::show("m_clusterbased_box2")
    shinyjs::show("m_clusterbased_box4")
  })
  
  observe({
    if (input$m_clusterbased4 == "seurat_clusters") {
      output$m_clusterbased_6 <- renderUI({
        clusters <- req(datainput_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "m_clusterbased6",
          label = "Select one or multiple cluster(s) for plotting",
          choices = sort(clusters),
          selected = sort(clusters),
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      })
    }
    
    plot_type <- input$m_clusterbased3
    grouping <- input$m_clusterbased4
    
    if (plot_type %in% c("Dot Plot", "VlnPlot", "RidgePlot")) {
      if (grouping == "seurat_clusters") {
        shinyjs::show("m_clusterbased_6")
        shinyjs::show("m_clusterbased6")
      } else {
        shinyjs::hide("m_clusterbased_6")
        shinyjs::hide("m_clusterbased6")
      }
      
      if (plot_type  %in% c("FeaturePlot", "spatial_plot")) {
        shinyjs::hide("m_clusterbased5")
      } else {
        shinyjs::show("m_clusterbased5")
      }
    } else if (plot_type  %in% c("FeaturePlot", "spatial_plot")) {
      shinyjs::hide("m_clusterbased_6")
      shinyjs::hide("m_clusterbased6")
      shinyjs::hide("m_clusterbased5")
    }
  })
  
  
  
    # Uncomment and modify the block below if "predicted" behavior is required:
    # else if (input$m_clusterbased4 == "predicted") {
    #   output$m_clusterbased_6 <- renderUI({
    #     clusters <- req(datainput_multiple_celltype_level()[[3]])
    #     shinyWidgets::pickerInput(
    #       inputId = "m_clusterbased6",
    #       label = "Select one or multiple cluster(s) for analysis",
    #       choices = sort(clusters),
    #       selected = sort(clusters),
    #       multiple = TRUE,
    #       options = list(`actions-box` = TRUE)
    #     )
    #   })
    # }
    # })
  
  
  datainput_multiple_clusterbased_level <- eventReactive(input$multiple_sample_clusterbased,{
    run_logged_analysis(
      section = "Multiple Samples",
      action = "Cluster-based plots",
      params = capture_run_inputs(c("m_clusterbased")),
      expr = {
        source_app_script("scripts/multiple_clusterbased.R")
        datainput_multiple_clusterbased(index_multiple_clusterbased_input = datainput_multiple_celltype_level()[[1]], index_multiple_clusterbased_features = datainput_multiple_marker_level()[[1]], index_m_celltype_method = datainput_multiple_celltype_level()[[4]], index_m_clusterbased1 = input$m_clusterbased1, index_m_clusterbased2 = input$m_clusterbased2, index_m_clusterbased3 = input$m_clusterbased3, index_m_clusterbased4 = input$m_clusterbased4, index_m_clusterbased5 = input$m_clusterbased5, index_m_clusterbased6 = input$m_clusterbased6)
      }
    )
  })  
  
  output$m_clusterbased1_plot<-renderPlot({
    datainput_multiple_clusterbased_level()[1]
  })
  
  observeEvent(input$download_m_clusterbased1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("m_clusterbased1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_clusterbased1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_clusterbased1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_clusterbased1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_clusterbased1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_clusterbased1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Plots_for_top_or_selected_markers",  input$m_clusterbased1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_clusterbased_level()[[1]], width = input$m_clusterbased1_plot_width, height = input$m_clusterbased1_plot_height, dpi = input$m_clusterbased1_plot_dpi, units = "in")
    }
  )
  
  
  
  output$m_clusterbased1_table<- renderDataTable(DT::datatable((datainput_multiple_clusterbased_level()[[3]]),
                                                               options = list(
                                                                 scrollX = TRUE,
                                                                 pageLength = 10,
                                                                 dom = "Blfrtip"
                                                                 #bFilter=0
                                                               ),rownames= FALSE, selection = "none"))

  output$download_m_clusterbased1_table <- downloadHandler(
    filename = function() {
      paste("Top_or_selected_Cell_counts_proportion", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_multiple_clusterbased_level()[[3]], file)
    }
  )
  
  
  ###################save seurat object after doublet###################
  output$m_clusterbased <- create_object_download_handler(
    section = "Multiple Samples",
    action = "Download Seurat Object (After Cluster-Based Plots)",
    filename_text = "multiple_sample_seuart_object_after_plots.RDS",
    object_expr = datainput_multiple_clusterbased_level()[[2]]
  )
  
  #####################################link to next tab###########################     
  observeEvent(input$link_m_conditionbased, {
    newvalue <- "Condition Based Analysis"
    updateTabsetPanel(session, "multiple_tabsets", newvalue)
  })  
  
  
  ##########################Tab1.9###############################      
  ##############Condition-based analysis###################    
  observe({
    if(input$m_conditionbased9 == "gene_name_list"){
      shinyjs::show("m_conditionbased10")
    }
    else {
      shinyjs::hide("m_conditionbased10")
    }
  })  
  
  observe({
    if(input$m_conditionbased7 == "VolcanoPlot"){
      shinyjs::hide("m_conditionbased8")
      shinyjs::hide("m_conditionbased9")
      #shinyjs::hide("m_conditionbased_box4")
    }
    else{
      shinyjs::show("m_conditionbased8")
      shinyjs::show("m_conditionbased9")
      #shinyjs::show("m_conditionbased_box4")
    }
  })
  
  observeEvent(input$multiple_sample_conditionbased,{
    shinyjs::show("m_conditionbased_box3")
    shinyjs::show("m_conditionbased_box4")
    shinyjs::show("m_conditionbased_box5")
  })
  
  
  
  output$m_conditionbased_1 <- renderUI ({
    clusters <- req(datainput_multiple_clustering_level()[[13]])
    
    shinyWidgets::pickerInput(
      inputId = "m_conditionbased1",
      label = "Select the Condition1",
      choices = sort(clusters),
      multiple = F,
      options = list(`actions-box` = TRUE))
  })
  
  output$m_conditionbased_2 <- renderUI ({
    clusters <- req(datainput_multiple_clustering_level()[[13]])
    clusters <- clusters[!clusters == input$m_conditionbased1]
    shinyWidgets::pickerInput(
      inputId = "m_conditionbased2",
      label = "Select the Condition2",
      choices = sort(clusters),
      selected = sort(clusters)[1],
      multiple = F,
      options = list(`actions-box` = TRUE))
  })  
  
  
  
  datainput_multiple_conditionbased_level <- eventReactive(input$multiple_sample_conditionbased,{
    run_logged_analysis(
      section = "Multiple Samples",
      action = "Condition-based comparison",
      params = capture_run_inputs(c("multiple_sample_", "m_conditionbased")),
      expr = {
        source_app_script("scripts/multiple_conditionbased.R")
        datainput_multiple_conditionbased(index_multiple_conditionbased_input = datainput_multiple_celltype_level()[[1]], index_multiple_sample_normalization_method = input$multiple_sample_normalization_method, index_m_conditionbased1 = input$m_conditionbased1, index_m_conditionbased2 = input$m_conditionbased2, index_m_conditionbased3 = input$m_conditionbased3, index_m_conditionbased4 = input$m_conditionbased4, index_m_conditionbased5 = input$m_conditionbased5, index_m_conditionbased6 = input$m_conditionbased6, index_m_conditionbased7 = input$m_conditionbased7, index_m_conditionbased8 = input$m_conditionbased8, index_m_conditionbased9 = input$m_conditionbased9, index_m_conditionbased10 = input$m_conditionbased10)
      }
    )
  })  
  
  output$m_conditionbased1_plot<-renderPlot({
    datainput_multiple_conditionbased_level()[1]
  })
  
  observeEvent(input$download_m_conditionbased1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("m_conditionbased1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_conditionbased1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_conditionbased1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_conditionbased1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_conditionbased1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_conditionbased1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Plots_for_top_selected_markers",  input$m_conditionbased1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_multiple_conditionbased_level()[[1]], width = input$m_conditionbased1_plot_width, height = input$m_conditionbased1_plot_height, dpi = input$m_conditionbased1_plot_dpi, units = "in")
    }
  )
  
  
  
  output$m_conditionbased1_table<- renderDataTable(DT::datatable((datainput_multiple_conditionbased_level()[[2]]),
                                                                 options = list(
                                                                   scrollX = TRUE,
                                                                   pageLength = 10,
                                                                   dom = "Blfrtip"
                                                                   #bFilter=0
                                                                 ),rownames= FALSE, selection = "none"))
  
  output$download_m_conditionbased1_table <- downloadHandler(
    filename = function() { 
      paste("Differentially_expressed genes_sample_based", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_multiple_conditionbased_level()[[2]], file)
    }
  )
  
  
  ###################save seurat object after doublet###################
  output$m_conditionbased <- create_object_download_handler(
    section = "Multiple Samples",
    action = "Download Seurat Object (After Condition-Based Analysis)",
    filename_text = "multiple_sample_seuart_object_after_plots.RDS",
    object_expr = datainput_multiple_conditionbased_level()[[3]]
  )
  
  
  #################################################################Tab2#################################################################
  #################################################Multiple samples subclustering######################################################      
  
  ##########multiple sidebar hide##########
  observeEvent(input[["subclustering_multiple_tabsets"]], {
    if(input[["subclustering_multiple_tabsets"]] == "Cell Stats"){
      showElement(selector = "#subclustering_multiple_sidebar")
      removeCssClass("subclustering_multiple_main_menu", "col-sm-12")
      addCssClass("subclustering_multiple_main_menu", "col-sm-8")
    }else{
      hideElement(selector = "#subclustering_multiple_sidebar")
      removeCssClass("subclustering_multiple_main_menu", "col-sm-8")
      addCssClass("subclustering_multiple_main_menu", "col-sm-12")
    }
  })
  
  ############subclustering multiple hide tabs########
  hideTab(inputId = "subclustering_multiple_tabsets", target = "Normalization and PCA Analysis")
  hideTab(inputId = "subclustering_multiple_tabsets", target = "Clustering")
  hideTab(inputId = "subclustering_multiple_tabsets", target = "Markers Identification")
  hideTab(inputId = "subclustering_multiple_tabsets", target = "Cell Type Prediction")
  hideTab(inputId = "subclustering_multiple_tabsets", target = "Cluster-Based Plots")
  hideTab(inputId = "subclustering_multiple_tabsets", target = "Condition Based Analysis")
  
  
  ################hide menu bar######################
  #hideTab(inputId = "menu_tabs", target = "Multiple samples Subclustering")
  
  ##########subclustering showtabbutton#############     
  observeEvent(input$link_m_subclustering_normalization, {
    showTab(inputId = "subclustering_multiple_tabsets", target = "Normalization and PCA Analysis")
  })
  
  observeEvent(input$link_m_subclustering_clustering, {
    showTab(inputId = "subclustering_multiple_tabsets", target = "Clustering")
  })
  
  observeEvent(input$link_m_subclustering_marker, {
    showTab(inputId = "subclustering_multiple_tabsets", target = "Markers Identification")
  })
  
  observeEvent(input$link_m_subclustering_prediction, {
    showTab(inputId = "subclustering_multiple_tabsets", target = "Cell Type Prediction")
  })
  
  observeEvent(input$link_m_subclustering_clusterbased, {
    showTab(inputId = "subclustering_multiple_tabsets", target = "Cluster-Based Plots")
  })    
  
  observeEvent(input$link_m_subclustering_conditionbased, {
    showTab(inputId = "subclustering_multiple_tabsets", target = "Condition Based Analysis")
  })
  
  ################show menu bar######################
  observeEvent(input$multiple_sample_celltype, {
    #showTab(inputId = "menu_tabs", target = "Multiple samples subclustering")
    shinyjs::hide("m_subclustering0")
    shinyjs::show("m_subclustering1")
    shinyjs::show("subclustering_multiple_sample_submit")
  })   
  
  ########multiple samples subclustering hide stats########  
  shinyjs::hide("m_subclustering1")
  shinyjs::hide("m_subclustering_2")
  shinyjs::hide("m_subclustering_3")
  shinyjs::hide("m_subclustering3")
  shinyjs::hide("subclustering_multiple_sample_submit")
  shinyjs::hide("m_subclustering_box1")
  shinyjs::hide("m_subclustering_box2")
  shinyjs::hide("m_subclustering_box3")
  shinyjs::hide("m_subclustering_box4")
  
  ##########multiple sidebar hide##########
  observeEvent(input[["subclustering_multiple_tabsets"]], {
    if(input[["subclustering_multiple_tabsets"]] == "Cell Stats"){
      showElement(selector = "#subclustering_multiple_sidebar")
      removeCssClass("subclustering_multiple_main_menu", "col-sm-12")
      addCssClass("subclustering_multiple_main_menu", "col-sm-8")
    }else{
      hideElement(selector = "#subclustering_multiple_sidebar")
      removeCssClass("subclustering_multiple_main_menu", "col-sm-8")
      addCssClass("subclustering_multiple_main_menu", "col-sm-12")
    }
  })
  
  
  ########multiple hide normalization########     
  shinyjs::hide("m_subclustering_pca_box1")
  shinyjs::hide("m_subclustering_elbow_box")
  shinyjs::hide("m_subclustering_pca_box2")
  shinyjs::hide("m_subclustering_pca_box3")
  shinyjs::hide("m_subclustering_pca_box4")
  
  observeEvent(input$subclustering_multiple_sample_normalization,{
    shinyjs::show("m_subclustering_pca_box1")
    shinyjs::show("m_subclustering_elbow_box")
    shinyjs::show("m_subclustering_pca_box2")
    shinyjs::show("m_subclustering_pca_box3")
    shinyjs::show("m_subclustering_pca_box4")
  })
  
  observe({
    if (input$subclustering_multiple_sample_normalization_method == "LogNormalize") {
      current_assay <- isolate(input$subclustering_multiple_sample_assay) %||% "auto"
      updateSelectInput(
        session,
        "subclustering_multiple_sample_assay",
        choices = c("Auto detect" = "auto", "RNA" = "RNA", "Spatial" = "Spatial"),
        selected = if (identical(current_assay, "SCT")) "auto" else current_assay
      )
      shinyjs::show("subclustering_multiple_sample_scale_factor")
      shinyjs::show("subclustering_multiple_sample_normalization_variable_genes")
      shinyjs::show("subclustering_multiple_sample_var_genes")
      shinyjs::hide("subclustering_multiple_sample_var_genes1")
      #shinyjs::show("subclustering_multiple_sample_normalization_method1")
    }
    else if (input$subclustering_multiple_sample_normalization_method  == "SCTransform") {
      current_assay <- isolate(input$subclustering_multiple_sample_assay) %||% "auto"
      updateSelectInput(
        session,
        "subclustering_multiple_sample_assay",
        choices = c("Auto detect" = "auto", "RNA" = "RNA", "Spatial" = "Spatial", "SCT" = "SCT"),
        selected = current_assay %||% "auto"
      )
      shinyjs::hide("subclustering_multiple_sample_scale_factor")
      shinyjs::hide("subclustering_multiple_sample_normalization_variable_genes")
      shinyjs::hide("subclustering_multiple_sample_var_genes")
      shinyjs::show("subclustering_multiple_sample_var_genes1")
      #shinyjs::hide("multiple_sample_normalization_method1")
    }
  })
  
  ########multiple hide clustering########     
  shinyjs::hide("m_subclustering_clustering_box1")
  shinyjs::hide("m_subclustering_clustering_box2")
  shinyjs::hide("m_subclustering_clustering_box3")
  shinyjs::hide("m_subclustering_clustering_box4")
  shinyjs::hide("m_subclustering_clustering_box5")
  shinyjs::hide("m_subclustering_clustering_box6") 
  shinyjs::hide("m_subclustering_clustering_box7") 
  shinyjs::hide("m_subclustering_clustering_box8")   
  shinyjs::hide("m_subclustering_clustering_box9") 
  shinyjs::hide("m_subclustering_clustering_box10") 
  shinyjs::hide("m_subclustering_clustering_box11")
  shinyjs::hide("m_subclustering_clustering_box12")
  shinyjs::hide("m_subclustering_clustering_box13")
  shinyjs::hide("m_subclustering_clustering_box14")
  shinyjs::hide("m_subclustering_clustering_box15")
  
  observeEvent(input$subclustering_multiple_sample_clustering,{
    shinyjs::show("m_subclustering_clustering_box1")
    shinyjs::show("m_subclustering_clustering_box2")
    shinyjs::show("m_subclustering_clustering_box3")
    shinyjs::show("m_subclustering_clustering_box4")
    shinyjs::show("m_subclustering_clustering_box5")
    shinyjs::show("m_subclustering_clustering_box6") 
    shinyjs::show("m_subclustering_clustering_box7") 
    shinyjs::show("m_subclustering_clustering_box8") 
    shinyjs::show("m_subclustering_clustering_box9") 
    shinyjs::show("m_subclustering_clustering_box10") 
    shinyjs::show("m_subclustering_clustering_box11")
    shinyjs::show("m_subclustering_clustering_box12")
    shinyjs::show("m_subclustering_clustering_box13")
    shinyjs::show("m_subclustering_clustering_box14")
    shinyjs::show("m_subclustering_clustering_box15") 	
  })
  
  
  observe({
    if (input$m_subclustering_clustering6 == "umap") {
      shinyjs::show("m_subclustering_umap_box")
      shinyjs::hide("m_subclustering_tsne_box")
    }
    else if (input$m_subclustering_clustering6  == "tsne") {
      shinyjs::hide("m_subclustering_umap_box")
      shinyjs::show("m_subclustering_tsne_box")
    }
  })
  
  
  ########multiple hide doublet boxes########
  shinyjs::hide("m_subclustering_doublet_box2")
  shinyjs::hide("m_subclustering_doublet_box3")
  shinyjs::hide("m_subclustering_doublet_box4")
  shinyjs::hide("m_subclustering_doublet_box5")
  shinyjs::hide("m_subclustering_doublet_box6")
  shinyjs::hide("m_subclustering_doublet_box7")
  shinyjs::hide("m_subclustering_doublet_box8")
  shinyjs::hide("m_subclustering_doublet_box9")
  shinyjs::hide("m_subclustering_doublet_box10")
  shinyjs::hide("m_subclustering_doublet_box11")
  shinyjs::hide("m_subclustering_doublet_box12")
  shinyjs::hide("m_subclustering_doublet_box13")
  shinyjs::hide("m_subclustering_doublet_box14")
  shinyjs::hide("m_subclustering_doublet_box15")
  shinyjs::hide("m_subclustering_doublet_box16")
  shinyjs::hide("m_subclustering_doublet_box17")
  shinyjs::hide("m_subclustering_doublet_box18")
  
  observeEvent(input$subclustering_multiple_sample_doublet,{
    shinyjs::show("m_subclustering_doublet_box2")
    shinyjs::show("m_subclustering_doublet_box3")
    shinyjs::show("m_subclustering_doublet_box4")
    shinyjs::show("m_subclustering_doublet_box5")
    shinyjs::show("m_subclustering_doublet_box6")
    shinyjs::show("m_subclustering_doublet_box7")
  })
  observeEvent(input$subclustering_multiple_sample_doublet2,{
    
    shinyjs::show("m_subclustering_doublet_box8")
    shinyjs::show("m_subclustering_doublet_box9")
    shinyjs::show("m_subclustering_doublet_box10")
    shinyjs::show("m_subclustering_doublet_box11")
    shinyjs::show("m_subclustering_doublet_box12")
    shinyjs::show("m_subclustering_doublet_box13")
    shinyjs::show("m_subclustering_doublet_box14")
    shinyjs::show("m_subclustering_doublet_box15")
    shinyjs::show("m_subclustering_doublet_box16")
    shinyjs::show("m_subclustering_doublet_box17")
    shinyjs::show("m_subclustering_doublet_box18")
    
  })
  
  
  ########multiple hide markers box########  
  shinyjs::hide("m_subclustering_marker_box5")
  shinyjs::hide("m_subclustering_marker_box6")
  shinyjs::hide("m_subclustering_marker_box7")
  shinyjs::hide("m_subclustering_marker10")
  shinyjs::hide("m_subclustering_marker11")
  shinyjs::hide("m_subclustering_marker12")
  
  
  observe({
    if (input$m_subclustering_marker1 == 1) {
      shinyjs::hide("m_subclustering_marker_6")
      shinyjs::hide("m_subclustering_marker_7")
      shinyjs::hide("m_subclustering_marker_8")
      shinyjs::hide("m_subclustering_marker_9")
      shinyjs::hide("m_subclustering_marker6")
      shinyjs::hide("m_subclustering_marker7")
      shinyjs::hide("m_subclustering_marker8")
      shinyjs::hide("m_subclustering_marker9")
      shinyjs::hide("m_subclustering_marker10")
      shinyjs::show("m_subclustering_marker11")
      shinyjs::hide("m_subclustering_marker12")
    }
    else if (input$m_subclustering_marker1 == 2) {
      shinyjs::show("m_subclustering_marker_6")
      shinyjs::hide("m_subclustering_marker_7")
      shinyjs::hide("m_subclustering_marker_8")
      shinyjs::hide("m_subclustering_marker_9")
      shinyjs::show("m_subclustering_marker6")
      shinyjs::hide("m_subclustering_marker7")
      shinyjs::hide("m_subclustering_marker8")
      shinyjs::hide("m_subclustering_marker9")
      shinyjs::hide("m_subclustering_marker_box6")
      shinyjs::hide("m_subclustering_marker10")
      shinyjs::show("m_subclustering_marker11")
      shinyjs::hide("m_subclustering_marker12")
      
    }
    else if (input$m_subclustering_marker1 == 3) {
      shinyjs::show("m_subclustering_marker_6")
      shinyjs::show("m_subclustering_marker_7")
      shinyjs::hide("m_subclustering_marker_8")
      shinyjs::hide("m_subclustering_marker_9")
      shinyjs::show("m_subclustering_marker6")
      shinyjs::show("m_subclustering_marker7")
      shinyjs::hide("m_subclustering_marker8")
      shinyjs::hide("m_subclustering_marker9")
      shinyjs::hide("m_subclustering_marker_box6")
      shinyjs::hide("m_subclustering_marker10")
      shinyjs::show("m_subclustering_marker11")
      shinyjs::hide("m_subclustering_marker12")
    }
    else if (input$m_subclustering_marker1 == 4) {
      shinyjs::hide("m_subclustering_marker_6")
      shinyjs::hide("m_subclustering_marker_7")
      shinyjs::show("m_subclustering_marker_8")
      shinyjs::hide("m_subclustering_marker_9")
      shinyjs::hide("m_subclustering_marker6")
      shinyjs::hide("m_subclustering_marker7")
      shinyjs::show("m_subclustering_marker8")
      shinyjs::hide("m_subclustering_marker9")
      shinyjs::hide("m_subclustering_marker_box6")
      shinyjs::show("m_subclustering_marker10")
      shinyjs::hide("m_subclustering_marker11")
      shinyjs::show("m_subclustering_marker12")
    }
    
    else if (input$m_subclustering_marker1 == 5) {
      shinyjs::hide("m_subclustering_marker_6")
      shinyjs::hide("m_subclustering_marker_7")
      shinyjs::show("m_subclustering_marker_8")
      shinyjs::show("m_subclustering_marker_9")
      shinyjs::hide("m_subclustering_marker6")
      shinyjs::hide("m_subclustering_marker7")
      shinyjs::show("m_subclustering_marker8")
      shinyjs::show("m_subclustering_marker9")
      shinyjs::hide("m_subclustering_marker_box6")
      shinyjs::show("m_subclustering_marker10")
      shinyjs::hide("m_subclustering_marker11")
      shinyjs::show("m_subclustering_marker12")
    }
    
  })
  
  ########multiple hide celltype box########  
  shinyjs::hide("m_subclustering_celltype_box3")
  shinyjs::hide("m_subclustering_celltype_box4")
  shinyjs::hide("m_subclustering_celltype_box5")
  shinyjs::hide("m_subclustering_celltype_box7")
  shinyjs::hide("m_subclustering_celltype_box8")
  shinyjs::hide("m_subclustering_celltype_box9")
  shinyjs::hide("m_subclustering_celltype_box10")
  shinyjs::hide("m_subclustering_celltype_box11")
  
  observe({
    if (input$m_subclustering_celltype1 == 1) {
      shinyjs::show("m_subclustering_celltype_box2")
      shinyjs::show("m_subclustering_celltype2")
      shinyjs::hide("m_subclustering_celltype_box3")
      shinyjs::hide("m_subclustering_celltype3")
      shinyjs::hide("m_subclustering_celltype4")
      shinyjs::hide("m_subclustering_celltype_box4")
      shinyjs::hide("m_subclustering_celltype5")
      shinyjs::hide("m_subclustering_celltype6")
      shinyjs::hide("m_subclustering_celltype_box5")
      shinyjs::hide("m_subclustering_celltype7")
    }
    else if (input$m_subclustering_celltype1 == 2) {
      shinyjs::hide("m_subclustering_celltype_box2")
      shinyjs::hide("m_subclustering_celltype2")
      shinyjs::show("m_subclustering_celltype_box3")
      shinyjs::show("m_subclustering_celltype3")
      shinyjs::show("m_subclustering_celltype4")
      shinyjs::hide("m_subclustering_celltype_box4")
      shinyjs::hide("m_subclustering_celltype5")
      shinyjs::hide("m_subclustering_celltype6")
      shinyjs::hide("m_subclustering_celltype_box5")
      shinyjs::hide("m_subclustering_celltype7")
    }
    else if (input$m_subclustering_celltype1 == 3) {
      shinyjs::hide("m_subclustering_celltype_box2")
      shinyjs::hide("m_subclustering_celltype2")
      shinyjs::hide("m_subclustering_celltype_box3")
      shinyjs::hide("m_subclustering_celltype3")
      shinyjs::hide("m_subclustering_celltype4")
      shinyjs::show("m_subclustering_celltype_box4")
      shinyjs::show("m_subclustering_celltype5")
      shinyjs::show("m_subclustering_celltype6")
      shinyjs::hide("m_subclustering_celltype_box5")
      shinyjs::hide("m_subclustering_celltype7")
    }
    else if (input$m_subclustering_celltype1 == 4) {
      shinyjs::hide("m_subclustering_celltype_box2")
      shinyjs::hide("m_subclustering_celltype2")
      shinyjs::hide("m_subclustering_celltype_box3")
      shinyjs::hide("m_subclustering_celltype3")
      shinyjs::hide("m_subclustering_celltype4")
      shinyjs::hide("m_subclustering_celltype_box4")
      shinyjs::hide("m_subclustering_celltype5")
      shinyjs::hide("m_subclustering_celltype6")
      shinyjs::show("m_subclustering_celltype_box5")
      shinyjs::show("m_subclustering_celltype7")
    }
  })     
  
  ##################multiple Cluster-based plots####################### 
  shinyjs::hide("m_subclustering_clusterbased2")
  shinyjs::hide("m_subclustering_clusterbased_box2")
  shinyjs::hide("m_subclustering_clusterbased_box3")     
  shinyjs::hide("m_subclustering_clusterbased_box4")
  
  ##################multiple conditionbased####################### 
  shinyjs::hide("m_subclustering_conditionbased_box3")
  shinyjs::hide("m_subclustering_conditionbased_box4")     
  shinyjs::hide("m_subclustering_conditionbased_box5")      
  
  ###################################################
  ######################data Input##################
  
  #######################TAB2.1################################
  ########multiple samples subclustering hide stats########   
  
  observe({
    if (input$m_subclustering1 == "seurat_clusters") {
      shinyjs::show("m_subclustering_2")
      shinyjs::hide("m_subclustering_3")
      shinyjs::hide("m_subclustering_4")
      shinyjs::hide("m_subclustering_5")
      shinyjs::hide("m_subclustering_6")
      shinyjs::show("m_subclustering2")
      shinyjs::hide("m_subclustering3")
      shinyjs::hide("m_subclustering4")
      shinyjs::hide("m_subclustering5")
      shinyjs::hide("m_subclustering6")
    }  
    else if (input$m_subclustering1 == "predicted") {
      shinyjs::hide("m_subclustering_2")
      shinyjs::show("m_subclustering_3")
      shinyjs::hide("m_subclustering_4")
      shinyjs::hide("m_subclustering_5")
      shinyjs::hide("m_subclustering_6")
      shinyjs::hide("m_subclustering2")
      shinyjs::show("m_subclustering3")
      shinyjs::hide("m_subclustering4")
      shinyjs::hide("m_subclustering5")
      shinyjs::hide("m_subclustering6")
    }  
    else if (input$m_subclustering1 == "selected_gene") {
      shinyjs::hide("m_subclustering_2")
      shinyjs::hide("m_subclustering_3")
      shinyjs::show("m_subclustering_4")
      shinyjs::hide("m_subclustering_5")
      shinyjs::hide("m_subclustering_6")
      shinyjs::hide("m_subclustering2")
      shinyjs::hide("m_subclustering3")
      shinyjs::hide("m_subclustering4")
      shinyjs::hide("m_subclustering5")
      shinyjs::hide("m_subclustering6")
    }
    else if (input$m_subclustering1 == "exclude_selected_gene") {
      shinyjs::hide("m_subclustering_2")
      shinyjs::hide("m_subclustering_3")
      shinyjs::hide("m_subclustering_4")
      shinyjs::show("m_subclustering_5")
      shinyjs::hide("m_subclustering_6")
      shinyjs::hide("m_subclustering2")
      shinyjs::hide("m_subclustering3")
      shinyjs::hide("m_subclustering4")
      shinyjs::hide("m_subclustering5")
      shinyjs::hide("m_subclustering6")
    } 
    
  })
  
  observeEvent(input$subclustering_multiple_sample_submit,{
    shinyjs::show("m_subclustering_box1")
    shinyjs::show("m_subclustering_box2")
    shinyjs::show("m_subclustering_box3")
    shinyjs::show("m_subclustering_box4")
  })
  
  output$m_subclustering_2 <- renderUI ({
    clusters <- req(datainput_multiple_celltype_level()[[2]])
    shinyWidgets::pickerInput(
      inputId = "m_subclustering2",
      label = "Select one or multiple cluster(s) for analsysis",
      choices = sort(clusters),
      selected = sort(clusters)[1],
      multiple = T,
      options = list(`actions-box` = TRUE))
  })
  
  output$m_subclustering_3 <- renderUI ({
    clusters <- req(datainput_multiple_celltype_level()[[3]])
    shinyWidgets::pickerInput(
      inputId = "m_subclustering3",
      label = "Select one or multiple cluster(s) for analsysis",
      choices = sort(clusters),
      multiple = T,
      options = list(`actions-box` = TRUE))
  })
  
  
  datainput_subclustering_multiple_sample_level<- eventReactive(input$subclustering_multiple_sample_submit,{
    run_logged_analysis(
      section = "Subclustering",
      action = "Subset selected clusters or cell types",
      params = capture_run_inputs(c("m_subclustering")),
      expr = {
        source_app_script("scripts/subclustering_multiple_stats.R")
        datainput_subclustering_multiple_sample(index_subclustering_multiple_sample_file = datainput_multiple_celltype_level()[[1]], index_subclustering_multiple_sample_celltype = datainput_multiple_celltype_level()[[4]], index_m_subclustering1 = input$m_subclustering1, index_m_subclustering2 = input$m_subclustering2, index_m_subclustering3 = input$m_subclustering3, index_m_subclustering_4 = input$m_subclustering_4, index_m_subclustering_5 = input$m_subclustering_5)
      }
    )
  })
  
  output$m_subclustering_qc <- renderPlot({
    datainput_subclustering_multiple_sample_level()[1]
  })
  
  observeEvent(input$download_m_subclustering_qc, {
    showModal(modalDialog(
      title = strong("Download QC plot"),
      numericInput("m_subclustering_qc_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_qc_plot_width", label = h5("Figure width (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_subclustering_qc_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_qc_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_qc_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  output$m_subclustering_qc_downloadoutput<- downloadHandler(
    filename = function(){
      paste("QC_for_the_selected_subclusters", input$m_subclustering_qc_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_sample_level()[[1]], width = input$m_subclustering_qc_plot_width, height = input$m_subclustering_qc_plot_height, dpi = input$m_subclustering_qc_plot_dpi, units = "in")
    }
  )
  
  output$m_subclustering_qc_sp <- renderPlot({
    datainput_subclustering_multiple_sample_level()[4]
  })
  
  observeEvent(input$download_m_subclustering_qc_sp, {
    showModal(modalDialog(
      title = strong("Download QC plot"),
      numericInput("m_subclustering_qc_sp_plot_height", label = h5("Figure height (upto 49 inces)"), value = 6, width = "300px"),
      numericInput("m_subclustering_qc_sp_plot_width", label = h5("Figure width (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_subclustering_qc_sp_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_qc_sp_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_qc_sp_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  output$m_subclustering_qc_sp_downloadoutput<- downloadHandler(
    filename = function(){
      paste("QC_for_the_selected_subclusters_with_spatial_image", input$m_subclustering_qc_sp_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_sample_level()[[4]], width = input$m_subclustering_qc_sp_plot_width, height = input$m_subclustering_qc_sp_plot_height, dpi = input$m_subclustering_qc_sp_plot_dpi, units = "in")
    }
  )
  
  output$subclustering_multiple_cell_table<- renderDataTable(DT::datatable((datainput_subclustering_multiple_sample_level()[[2]]),
                                                                           options = list(
                                                                             scrollX = TRUE,
                                                                             pageLength = 10,
                                                                             bFilter=0
                                                                           ),rownames= FALSE, selection = "none"))
  
  output$download_subclustering_multiple_cell_table <- downloadHandler(
    filename = function() { 
      paste("Number of cells", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_subclustering_multiple_sample_level()[[2]], file)
    }
  )
  
  
  ###################save seurat object before qc###################
  output$m_subclustering_stats <- create_object_download_handler(
    section = "Subclustering",
    action = "Download Seurat Object (Subclustering Stats)",
    filename_text = "multiple_sample_subclustering_seuart_object.RDS",
    object_expr = datainput_subclustering_multiple_sample_level()[[3]]
  )
  ###############link to next tab###########################      
  observeEvent(input$link_m_subclustering_normalization, {
    newvalue <- "Normalization and PCA Analysis"
    updateTabsetPanel(session, "subclustering_multiple_tabsets", newvalue)
  })       
  
  
  
  
  ##########################Tab2.2###############################      
  ##############multiple Normalization & PCA###################      
  datainput_subclustering_multiple_normalization_pca_level <- eventReactive(input$subclustering_multiple_sample_normalization,{
    run_logged_analysis(
      section = "Subclustering",
      action = "Normalization and PCA",
      params = capture_run_inputs(c("subclustering_multiple_sample_", "m_subclustering")),
      expr = {
        source_app_script("scripts/subclustering_multiple_normalization_pca.R")
        datainput_subclustering_multiple_normalization_pca(index_subclustering_multiple_normalization_pca_input = datainput_subclustering_multiple_sample_level()[[3]], index_subclustering_multiple_sample_normalization_method = input$subclustering_multiple_sample_normalization_method, index_subclustering_multiple_sample_scale_factor=input$subclustering_multiple_sample_scale_factor, index_subclustering_multiple_sample_var_genes = input$subclustering_multiple_sample_var_genes, index_subclustering_multiple_sample_var_genes1 = input$subclustering_multiple_sample_var_genes1, index_subclustering_multiple_sample_normalization_variable_genes=input$subclustering_multiple_sample_normalization_variable_genes, index_subclustering_multiple_sample_pca_dim=input$subclustering_multiple_sample_pca_dim, index_subclustering_multiple_sample_assay=input$subclustering_multiple_sample_assay)
      }
    )
  })
  
  
  output$m_subclustering_pca_plot<-renderPlot({
    datainput_subclustering_multiple_normalization_pca_level()[1]
  })
  
  observeEvent(input$download_m_subclustering_pca_plot, {
    showModal(modalDialog(
      title = strong("Download PCA Plot"),
      numericInput("m_subclustering_pca_plot_height", label = h5("Figure height (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_subclustering_pca_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_pca_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_pca_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_pca_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_subclustering_pca_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("After_normalization_PCA_plot", input$m_subclustering_pca_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_normalization_pca_level()[[1]], width = input$m_subclustering_pca_plot_width, height = input$m_subclustering_pca_plot_height, dpi = input$m_subclustering_pca_plot_dpi, units = "in")
    }
  )
  
  output$m_subclustering_elbow_plot<-renderPlot({
    datainput_subclustering_multiple_normalization_pca_level()[2]
  })
  
  observeEvent(input$download_m_subclustering_elbow_plot, {
    showModal(modalDialog(
      title = strong("Download Variable Features Plot"),
      numericInput("m_subclustering_elbow_plot_height", label = h5("Figure height (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_subclustering_elbow_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_elbow_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_elbow_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_elbow_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_subclustering_elbow_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("After_normalization_Elbow", input$m_subclustering_elbow_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_normalization_pca_level()[[2]], width = input$m_subclustering_elbow_plot_width, height = input$m_subclustering_elbow_plot_height, dpi = input$m_subclustering_elbow_plot_dpi, units = "in")
    }
  )
  
  output$m_subclustering_pca2_plot<-renderPlot({
    datainput_subclustering_multiple_normalization_pca_level()[3]
  })
  
  observeEvent(input$download_m_subclustering_pca2_plot, {
    showModal(modalDialog(
      title = strong("Download PCA Plot"),
      numericInput("m_subclustering_pca2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_subclustering_pca2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_pca2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_pca2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_pca2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_subclustering_pca2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("After_normalization_PCA_plot_sample_based", input$m_subclustering_pca2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_normalization_pca_level()[[3]], width = input$m_subclustering_pca2_plot_width, height = input$m_subclustering_pca2_plot_height, dpi = input$m_subclustering_pca2_plot_dpi, units = "in")
    }
  )
  
  
  output$m_subclustering_pca3_plot<-renderPlot({
    datainput_subclustering_multiple_normalization_pca_level()[4]
  })
  
  
  observeEvent(input$download_m_subclustering_pca3_plot, {
    showModal(modalDialog(
      title = strong("Download PCA Plot"),
      numericInput("m_subclustering_pca3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 5, width = "300px"),
      numericInput("m_subclustering_pca3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_pca3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_pca3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_pca3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_subclustering_pca3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("After_normalization_PCA_plot_group_based", input$m_subclustering_pca3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_normalization_pca_level()[[4]], width = input$m_subclustering_pca3_plot_width, height = input$m_subclustering_pca3_plot_height, dpi = input$m_subclustering_pca3_plot_dpi, units = "in")
    }
  )
  
  
  
  ###################save seurat object after normalization###################
  output$m_subclustering_normalization <- create_object_download_handler(
    section = "Subclustering",
    action = "Download Seurat Object (After Normalization)",
    filename_text = "subclustering_multiple_sample_seuart_object_after_normalization.RDS",
    object_expr = datainput_subclustering_multiple_normalization_pca_level()[[5]]
  )
  
  #####################################link to next tab###########################      
  observeEvent(input$link_m_subclustering_clustering, {
    newvalue <- "Clustering"
    updateTabsetPanel(session, "subclustering_multiple_tabsets", newvalue)
  })       
  
  
  
  #####################################################Tab2.3####################      
  ########################################multiple Clustering###################      
  datainput_subclustering_multiple_clustering_level <- eventReactive(input$subclustering_multiple_sample_clustering,{
    run_logged_analysis(
      section = "Subclustering",
      action = "Clustering",
      params = capture_run_inputs(c("subclustering_multiple_sample_", "m_subclustering_clustering")),
      expr = {
        source_app_script("scripts/subclustering_multiple_clustering.R")
        datainput_subclustering_multiple_clustering(index_subclustering_multiple_clustering_input = datainput_subclustering_multiple_normalization_pca_level()[[5]], index_subclustering_multiple_sample_normalization_method = input$subclustering_multiple_sample_normalization_method, index_m_subclustering_clustering1 = input$m_subclustering_clustering1, index_m_subclustering_clustering2 = input$m_subclustering_clustering2, index_m_subclustering_clustering3 = input$m_subclustering_clustering3, index_m_subclustering_clustering4 = input$m_subclustering_clustering4, index_m_subclustering_clustering5 = input$m_subclustering_clustering5, index_m_subclustering_clustering6 = input$m_subclustering_clustering6, index_m_subclustering_clustering7 = input$m_subclustering_clustering7, index_m_subclustering_clustering8 = input$m_subclustering_clustering8, index_m_subclustering_clustering9 = input$m_subclustering_clustering9, index_m_subclustering_clustering10 = input$m_subclustering_clustering10, index_m_subclustering_clustering11 = input$m_subclustering_clustering11, index_m_subclustering_clustering12 = input$m_subclustering_clustering12)
      }
    )
  })
  
  output$m_subclustering_umap_tsne1_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[1]
  })
  observeEvent(input$download_m_subclustering_umap_tsne1_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_umap_tsne1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_plot", input$m_subclustering_umap_tsne1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[1]], width = input$m_subclustering_umap_tsne1_plot_width, height = input$m_subclustering_umap_tsne1_plot_height, dpi = input$m_subclustering_umap_tsne1_plot_dpi, units = "in")
    }
  )
  
  output$m_subclustering_umap_tsne_bar1_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[2]
  }) 
  observeEvent(input$download_m_subclustering_umap_tsne_bar1_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne_bar1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne_bar1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne_bar1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne_bar1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne_bar1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_umap_tsne_bar1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_bar_plot", input$m_subclustering_umap_tsne_bar1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[2]], width = input$m_subclustering_umap_tsne_bar1_plot_width, height = input$m_subclustering_umap_tsne_bar1_plot_height, dpi = input$m_subclustering_umap_tsne_bar1_plot_dpi, units = "in")
    }
  )
  
  
  
  
  
  output$m_subclustering_umap_tsne2_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[3]
  })
  observeEvent(input$download_m_subclustering_umap_tsne2_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_umap_tsne2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Condition_based_plot", input$m_subclustering_umap_tsne2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[3]], width = input$m_subclustering_umap_tsne2_plot_width, height = input$m_subclustering_umap_tsne2_plot_height, dpi = input$m_subclustering_umap_tsne2_plot_dpi, units = "in")
    }
  )
  
  
  
  output$m_subclustering_umap_tsne_bar2_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[4]
  })
  observeEvent(input$download_m_subclustering_umap_tsne_bar2_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne_bar2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne_bar2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne_bar2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne_bar2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne_bar2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_umap_tsne_bar2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Condition_based_bar_plot", input$m_subclustering_umap_tsne_bar2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[4]], width = input$m_subclustering_umap_tsne_bar2_plot_width, height = input$m_subclustering_umap_tsne_bar2_plot_height, dpi = input$m_subclustering_umap_tsne_bar2_plot_dpi, units = "in")
    }
  )
  
  
  
  output$m_subclustering_umap_tsne3_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[5]
  })
  observeEvent(input$download_m_subclustering_umap_tsne3_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_umap_tsne3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Sample_based_plot", input$m_subclustering_umap_tsne3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[5]], width = input$m_subclustering_umap_tsne3_plot_width, height = input$m_subclustering_umap_tsne3_plot_height, dpi = input$m_subclustering_umap_tsne3_plot_dpi, units = "in")
    }
  )	
  
  
  output$m_subclustering_umap_tsne4_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[15]
  })
  
  observeEvent(input$download_m_subclustering_umap_tsne4_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne4_plot_height", label = h5("Figure height (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_subclustering_umap_tsne4_plot_width", label = h5("Figure width (upto 49 inces)"), value = 15, width = "300px"),
      numericInput("m_subclustering_umap_tsne4_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne4_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne4_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_subclustering_umap_tsne4_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Sample_based_plot_split_by_clusters", input$m_subclustering_umap_tsne4_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[15]], width = input$m_subclustering_umap_tsne4_plot_width, height = input$m_subclustering_umap_tsne4_plot_height, dpi = input$m_subclustering_umap_tsne4_plot_dpi, units = "in")
    }
  )	 
  
  output$m_subclustering_umap_tsne_bar3_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[6]
  })
  observeEvent(input$download_m_subclustering_umap_tsne_bar3_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne_bar3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne_bar3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne_bar3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne_bar3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne_bar3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_umap_tsne_bar3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Sample_based_bar_plot", input$m_subclustering_umap_tsne_bar3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[6]], width = input$m_subclustering_umap_tsne_bar3_plot_width, height = input$m_subclustering_umap_tsne_bar3_plot_height, dpi = input$m_subclustering_umap_tsne_bar3_plot_dpi, units = "in")
    }
  )
  
  output$m_subclustering_clustering_table1<- renderDataTable(DT::datatable((datainput_subclustering_multiple_clustering_level()[[7]]),
                                                                           options = list(
                                                                             scrollX = TRUE,
                                                                             pageLength = 10,
                                                                             bFilter=0
                                                                           ),rownames= FALSE, selection = "none"))
  
  output$download_m_subclustering_clustering_table1 <- downloadHandler(
    filename = function() { 
      paste("Number of cells in clusters", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_subclustering_multiple_clustering_level()[[7]], file)
    }
  ) 
  
  output$m_subclustering_clustering_table2<- renderDataTable(DT::datatable((datainput_subclustering_multiple_clustering_level()[[8]]),
                                                                           options = list(
                                                                             scrollX = TRUE,
                                                                             pageLength = 10,
                                                                             bFilter=0
                                                                           ),rownames= FALSE, selection = "none"))
  
  output$download_m_subclustering_clustering_table2 <- downloadHandler(
    filename = function() { 
      paste("Number of cells in clusters based on condition", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_subclustering_multiple_clustering_level()[[8]], file)
    }
  ) 
  
  output$m_subclustering_clustering_table3<- renderDataTable(DT::datatable((datainput_subclustering_multiple_clustering_level()[[9]]),
                                                                           options = list(
                                                                             scrollX = TRUE,
                                                                             pageLength = 10,
                                                                             bFilter=0
                                                                           ),rownames= FALSE, selection = "none"))
  
  output$download_m_subclustering_clustering_table3 <- downloadHandler(
    filename = function() { 
      paste("Number of cells in clusters based on samples", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_subclustering_multiple_clustering_level()[[9]], file)
    }
  ) 
  
  
  output$m_subclustering_umap_tsne5_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[16]
  })
  observeEvent(input$download_m_subclustering_umap_tsne5_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne5_plot_height", label = h5("Figure height (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_subclustering_umap_tsne5_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne5_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne5_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne5_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_umap_tsne5_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("SPatial_plot_split_by_clusters", input$m_subclustering_umap_tsne5_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[16]], width = input$m_subclustering_umap_tsne5_plot_width, height = input$m_subclustering_umap_tsne5_plot_height, dpi = input$m_subclustering_umap_tsne5_plot_dpi, units = "in")
    }
  )	  
  
  output$m_subclustering_umap_tsne6_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[17]
  })
  observeEvent(input$download_m_subclustering_umap_tsne6_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne6_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne6_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_subclustering_umap_tsne6_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne6_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne6_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_umap_tsne6_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_based_plot_split_by_condition", input$m_subclustering_umap_tsne6_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[17]], width = input$m_subclustering_umap_tsne6_plot_width, height = input$m_subclustering_umap_tsne6_plot_height, dpi = input$m_subclustering_umap_tsne6_plot_dpi, units = "in")
    }
  )	  
  
  output$m_subclustering_umap_tsne7_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[18]
  })
  observeEvent(input$download_m_subclustering_umap_tsne7_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne7_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne7_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_subclustering_umap_tsne7_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne7_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne7_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_umap_tsne7_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_based_plot_split_by_samples", input$m_subclustering_umap_tsne3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[18]], width = input$m_subclustering_umap_tsne7_plot_width, height = input$m_subclustering_umap_tsne7_plot_height, dpi = input$m_subclustering_umap_tsne7_plot_dpi, units = "in")
    }
  )	 
  
  output$m_subclustering_umap_tsne8_plot<-renderPlot({
    datainput_subclustering_multiple_clustering_level()[19]
  })
  observeEvent(input$download_m_subclustering_umap_tsne8_plot, {
    showModal(modalDialog(
      title = strong("Download UMAP/ t-SNE Plot"),
      numericInput("m_subclustering_umap_tsne8_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_umap_tsne8_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_subclustering_umap_tsne8_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_umap_tsne8_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_umap_tsne8_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_umap_tsne8_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_split_by_condition", input$m_subclustering_umap_tsne8_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clustering_level()[[19]], width = input$m_subclustering_umap_tsne8_plot_width, height = input$m_subclustering_umap_tsne8_plot_height, dpi = input$m_subclustering_umap_tsne8_plot_dpi, units = "in")
    }
  )	 
  
  ###################save seurat object after clustering###################
  output$m_subclustering_clustering <- create_object_download_handler(
    section = "Subclustering",
    action = "Download Seurat Object (After Clustering)",
    filename_text = "subclustering_multiple_sample_seuart_object_after_clustering.RDS",
    object_expr = datainput_subclustering_multiple_clustering_level()[[10]]
  )
  
  
  #####################################link to next tab###########################   
  observeEvent(input$link_m_subclustering_marker, {
    newvalue <- "Markers Identification"
    updateTabsetPanel(session, "subclustering_multiple_tabsets", newvalue)
  }) 
  
  
  ##########################Tab2.4###############################      
  ##############multiple Marker identification###################    
  observeEvent(input$subclustering_multiple_sample_marker,{
    if(input$m_subclustering_marker1 == 1){
      shinyjs::show("m_subclustering_marker_box5")
      shinyjs::show("m_subclustering_marker_box6")
      shinyjs::show("m_subclustering_marker_box7")
    }
    else{
      shinyjs::show("m_subclustering_marker_box5")
      shinyjs::hide("m_subclustering_marker_box6")
      shinyjs::show("m_subclustering_marker_box7")
    }
    
  })
  
  output$m_subclustering_marker_6 <- renderUI ({
    clusters <- req(datainput_subclustering_multiple_clustering_level()[[11]])
    
    shinyWidgets::pickerInput(
      inputId = "m_subclustering_marker6",
      label = "Select one cluster for analsysis",
      choices = sort(clusters),
      multiple = F,
      options = list(`actions-box` = TRUE))
  })
  
  output$m_subclustering_marker_7 <- renderUI ({
    clusters <- req(datainput_subclustering_multiple_clustering_level()[[11]])
    clusters <- clusters[!clusters == input$m_subclustering_marker6]
    shinyWidgets::pickerInput(
      inputId = "m_subclustering_marker7",
      label = "Identify markers distinguishing a cluster from other selected clusters",
      choices = sort(clusters),
      multiple = T,
      selected = sort(clusters)[1],
      options = list(`actions-box` = TRUE))
  })
  
  output$m_subclustering_marker_8 <- renderUI ({
    clusters <- req(datainput_subclustering_multiple_clustering_level()[[11]])
    
    shinyWidgets::pickerInput(
      inputId = "m_subclustering_marker8",
      label = "Select one cluster to define markers",
      choices = sort(clusters),
      multiple = F,
      options = list(`actions-box` = TRUE))
  })
  
  output$m_subclustering_marker_9 <- renderUI ({
    clusters <- req(datainput_subclustering_multiple_clustering_level()[[11]])
    clusters <- clusters[!clusters == input$m_subclustering_marker8]
    shinyWidgets::pickerInput(
      inputId = "m_subclustering_marker9",
      label = "Select the cluster to find the conserved markers between two clusters",
      choices = sort(clusters),
      multiple = T,
      selected = sort(clusters)[1],
      options = list(`actions-box` = TRUE))
  })
  
  
  datainput_subclustering_multiple_marker_level <- eventReactive(input$subclustering_multiple_sample_marker,{
    run_logged_analysis(
      section = "Subclustering",
      action = "Marker identification",
      params = capture_run_inputs(c("subclustering_multiple_sample_", "m_subclustering_marker")),
      expr = {
        source_app_script("scripts/subclustering_multiple_marker.R")
        datainput_subclustering_multiple_marker(index_subclustering_multiple_marker_input = datainput_subclustering_multiple_clustering_level()[[10]], index_m_subclustering_marker1 = input$m_subclustering_marker1, index_m_subclustering_marker2 = input$m_subclustering_marker2, index_m_subclustering_marker3 = input$m_subclustering_marker3, index_m_subclustering_marker4 = input$m_subclustering_marker4, index_m_subclustering_marker5 = input$m_subclustering_marker5, index_m_subclustering_marker6 = input$m_subclustering_marker6, index_m_subclustering_marker7 = input$m_subclustering_marker7, index_m_subclustering_marker8 = input$m_subclustering_marker8, index_m_subclustering_marker9 = input$m_subclustering_marker9, index_m_subclustering_marker10 = input$m_subclustering_marker10, index_subclustering_multiple_sample_normalization_method = input$subclustering_multiple_sample_normalization_method)
      }
    )
  })
  
  
  output$m_subclustering_marker1_table<- renderDataTable(DT::datatable((datainput_subclustering_multiple_marker_level()[[1]]),
                                                                       options = list(
                                                                         scrollX = TRUE,
                                                                         pageLength = 10,
                                                                         dom = "Blfrtip"
                                                                         #bFilter=0
                                                                       ),rownames= FALSE, selection = "none"))
  
  
  output$download_m_subclustering_marker1_table <- downloadHandler(
    filename = function() {
      paste("Number_of_identified_markers_or_differentially_expressed_genes", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_subclustering_multiple_marker_level()[[1]], file)
    }
  )
  
  
  output$m_subclustering_marker1_plot<-renderPlot({
    datainput_subclustering_multiple_marker_level()[3]
  })
  
  
  observeEvent(input$download_m_subclustering_marker1_plot, {
    showModal(modalDialog(
      title = strong("Download Heatmap"),
      numericInput("m_subclustering_marker1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_marker1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 12, width = "300px"),
      numericInput("m_subclustering_marker1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_marker1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_marker1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  
  output$m_subclustering_marker1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste( "Heatmap_with_Top5_expressed_genes", input$m_subclustering_marker1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_marker_level()[[3]], width = input$m_subclustering_marker1_plot_width, height = input$m_subclustering_marker1_plot_height, dpi = input$m_subclustering_marker1_plot_dpi, units = "in")
    }
  )
  
  
  ###################save seurat object after doublet removal###################
  output$m_subclustering_marker <- create_object_download_handler(
    section = "Subclustering",
    action = "Download Seurat Object (After Marker Identification)",
    filename_text = "subclustering_multiple_sample_seuart_object_after_marker_identification.RDS",
    object_expr = datainput_subclustering_multiple_marker_level()[[2]]
  )
  
  #####################################link to next tab###########################     
  observeEvent(input$link_m_subclustering_prediction, {
    newvalue <- "Cell Type Prediction"
    updateTabsetPanel(session, "subclustering_multiple_tabsets", newvalue)
  })  
  
  ##########################Tab2.5###############################      
  ##############multiple cell type################### 
  output$m_subclustering_celltype7 <- renderUI({
    numberofclusters <- as.integer(length(levels(datainput_subclustering_multiple_marker_level()[[2]])))
    lapply(1:numberofclusters, function(i) {
      column(3, textInput(paste("subclustering_mcelltypenames", levels(datainput_subclustering_multiple_marker_level()[[2]])[i], sep = ""),
                          paste("Cluster", levels(datainput_subclustering_multiple_marker_level()[[2]])[i]), value = paste("Cluster", levels(datainput_subclustering_multiple_marker_level()[[2]])[i])))
    })
  })
  
  observeEvent(input$subclustering_multiple_sample_celltype,{
    if(input$m_subclustering_celltype1 == 1){
      shinyjs::show("m_subclustering_celltype_box7")
      shinyjs::show("m_subclustering_celltype_box8")
      shinyjs::hide("m_subclustering_celltype_box9")
      shinyjs::show("m_subclustering_celltype_box10")
      shinyjs::show("m_subclustering_celltype_box11")
      shinyjs::show("m_subclustering_celltype9")
      shinyjs::hide("m_subclustering_celltype10")
      
    }
    else if (input$m_subclustering_celltype1 == 2){
      shinyjs::show("m_subclustering_celltype_box7")
      shinyjs::show("m_subclustering_celltype_box8")
      shinyjs::show("m_subclustering_celltype_box9")
      shinyjs::show("m_subclustering_celltype_box10")
      shinyjs::show("m_subclustering_celltype_box11")
      shinyjs::hide("m_subclustering_celltype9")
      shinyjs::show("m_subclustering_celltype10")
    }
    else if (input$m_subclustering_celltype1 == 3){
      shinyjs::show("m_subclustering_celltype_box7")
      shinyjs::hide("m_subclustering_celltype_box8")
      shinyjs::hide("m_subclustering_celltype_box9")
      shinyjs::show("m_subclustering_celltype_box10")
      shinyjs::show("m_subclustering_celltype_box11")
      shinyjs::hide("m_subclustering_celltype9")
      shinyjs::hide("m_subclustering_celltype10")
    }
    else if (input$m_subclustering_celltype1 == 4){
      shinyjs::show("m_subclustering_celltype_box7")
      shinyjs::hide("m_subclustering_celltype_box8")
      shinyjs::hide("m_subclustering_celltype_box9")
      shinyjs::show("m_subclustering_celltype_box10")
      shinyjs::show("m_subclustering_celltype_box11")
      shinyjs::hide("m_subclustering_celltype9")
      shinyjs::hide("m_subclustering_celltype10")
    }
    
  })
  
  
  datainput_subclustering_multiple_celltype_level <- eventReactive(input$subclustering_multiple_sample_celltype,{
    run_logged_analysis(
      section = "Subclustering",
      action = "Cell type annotation",
      params = capture_run_inputs(c("subclustering_multiple_sample_", "m_subclustering_celltype", "subclustering_mcelltypenames")),
      expr = {
        source_app_script("scripts/subclustering_multiple_celltype.R")
        datainput_subclustering_multiple_celltype(index_subclustering_multiple_celltype_input = datainput_subclustering_multiple_marker_level()[[2]], index_cell_markers = datainput_subclustering_multiple_marker_level()[[1]], index_m_subclustering_celltype1 = input$m_subclustering_celltype1, index_m_subclustering_celltype2 = input$m_subclustering_celltype2, index_m_subclustering_celltype3 = input$m_subclustering_celltype3, index_m_subclustering_celltype4 = input$m_subclustering_celltype4, index_m_subclustering_celltype5 = input$m_subclustering_celltype5, index_m_subclustering_celltype6 = input$m_subclustering_celltype6, 
                                                index_m_subclustering_celltype7 = c(input$subclustering_mcelltypenames0,input$subclustering_mcelltypenames1,input$subclustering_mcelltypenames2,input$subclustering_mcelltypenames3,input$subclustering_mcelltypenames4,input$subclustering_mcelltypenames5,input$subclustering_mcelltypenames6,input$subclustering_mcelltypenames7,input$subclustering_mcelltypenames8,input$subclustering_mcelltypenames9,input$subclustering_mcelltypenames10,input$subclustering_mcelltypenames11,input$subclustering_mcelltypenames12,input$subclustering_mcelltypenames13,input$subclustering_mcelltypenames14,input$subclustering_mcelltypenames15,input$subclustering_mcelltypenames16,input$subclustering_mcelltypenames17,input$subclustering_mcelltypenames18,input$subclustering_mcelltypenames19,input$subclustering_mcelltypenames20,input$subclustering_mcelltypenames21,input$subclustering_mcelltypenames22,input$subclustering_mcelltypenames23,input$subclustering_mcelltypenames24,input$subclustering_mcelltypenames25,input$subclustering_mcelltypenames26,input$subclustering_mcelltypenames27,input$subclustering_mcelltypenames28,input$subclustering_mcelltypenames29,input$subclustering_mcelltypenames30,input$subclustering_mcelltypenames31,input$subclustering_mcelltypenames32,input$subclustering_mcelltypenames33,input$subclustering_mcelltypenames34,input$subclustering_mcelltypenames35,input$subclustering_mcelltypenames36,input$subclustering_mcelltypenames37,input$subclustering_mcelltypenames38,input$subclustering_mcelltypenames39,input$subclustering_mcelltypenames40,input$subclustering_mcelltypenames41,input$subclustering_mcelltypenames42,input$subclustering_mcelltypenames43,input$subclustering_mcelltypenames44,input$subclustering_mcelltypenames45,input$subclustering_mcelltypenames46,input$subclustering_mcelltypenames47,input$subclustering_mcelltypenames48,input$subclustering_mcelltypenames49,input$subclustering_mcelltypenames50,input$subclustering_mcelltypenames51,input$subclustering_mcelltypenames52,input$subclustering_mcelltypenames53,input$subclustering_mcelltypenames54,input$subclustering_mcelltypenames55,input$subclustering_mcelltypenames56,input$subclustering_mcelltypenames57,input$subclustering_mcelltypenames58,input$subclustering_mcelltypenames59,input$subclustering_mcelltypenames60,input$subclustering_mcelltypenames61,input$subclustering_mcelltypenames62,input$subclustering_mcelltypenames63,input$subclustering_mcelltypenames64,input$subclustering_mcelltypenames65,input$subclustering_mcelltypenames66,input$subclustering_mcelltypenames67,input$subclustering_mcelltypenames68,input$subclustering_mcelltypenames69,input$subclustering_mcelltypenames70,input$subclustering_mcelltypenames71,input$subclustering_mcelltypenames72,input$subclustering_mcelltypenames73,input$subclustering_mcelltypenames74,input$subclustering_mcelltypenames75,input$subclustering_mcelltypenames76,input$subclustering_mcelltypenames77,input$subclustering_mcelltypenames78,input$subclustering_mcelltypenames79,input$subclustering_mcelltypenames80,input$subclustering_mcelltypenames81,input$subclustering_mcelltypenames82,input$subclustering_mcelltypenames83,input$subclustering_mcelltypenames84,input$subclustering_mcelltypenames85,input$subclustering_mcelltypenames86,input$subclustering_mcelltypenames87,input$subclustering_mcelltypenames88,input$subclustering_mcelltypenames89,input$subclustering_mcelltypenames90,input$subclustering_mcelltypenames91,input$subclustering_mcelltypenames92,input$subclustering_mcelltypenames93,input$subclustering_mcelltypenames94,input$subclustering_mcelltypenames95,input$subclustering_mcelltypenames96,input$subclustering_mcelltypenames97,input$subclustering_mcelltypenames98,input$subclustering_mcelltypenames99), 
                                                index_m_subclustering_celltype8 = input$m_subclustering_celltype8, index_m_subclustering_celltype9 = input$m_subclustering_celltype_splitby, index_m_subclustering_clustering6 = input$m_subclustering_clustering6, index_subclustering_multiple_sample_normalization_method = input$subclustering_multiple_sample_normalization_method)
      }
    )
  })
  output$m_subclustering_celltype1_plot<-renderPlot({
    datainput_subclustering_multiple_celltype_level()[[5]]
  })
  
  observeEvent(input$download_m_subclustering_celltype1_plot, {
    showModal(modalDialog(
      title = strong("Download Celltype"),
      numericInput("m_subclustering_celltype1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_celltype1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 16, width = "300px"),
      numericInput("m_subclustering_celltype1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_celltype1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_celltype1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_celltype1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Dimplot_with_celltype", input$m_subclustering_celltype1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_celltype_level()[[5]], width = input$m_subclustering_celltype1_plot_width, height = input$m_subclustering_celltype1_plot_height, dpi = input$m_subclustering_celltype1_plot_dpi, units = "in")
    }
  )
  
  output$m_subclustering_celltype4_plot<-renderPlot({
    datainput_subclustering_multiple_celltype_level()[[6]]
  })
  
  observeEvent(input$download_m_subclustering_celltype4_plot, {
    showModal(modalDialog(
      title = strong("Download Celltype"),
      numericInput("m_subclustering_celltype4_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_celltype4_plot_width", label = h5("Figure width (upto 49 inces)"), value = 16, width = "300px"),
      numericInput("m_subclustering_celltype4_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_celltype4_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_celltype4_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_celltype4_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Spatial_Dimplot_with_celltype", input$m_subclustering_celltype4_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_celltype_level()[[6]], width = input$m_subclustering_celltype4_plot_width, height = input$m_subclustering_celltype4_plot_height, dpi = input$m_subclustering_celltype4_plot_dpi, units = "in")
    }
  )
  
  output$m_subclustering_celltype1_table<- renderDataTable(DT::datatable((datainput_subclustering_multiple_celltype_level()[[7]]),
                                                                         options = list(
                                                                           scrollX = TRUE,
                                                                           pageLength = 10,
                                                                           dom = "Blfrtip"
                                                                           #bFilter=0
                                                                         ),rownames= FALSE, selection = "none"))
  
  output$download_m_subclustering_celltype1_table <- downloadHandler(
    filename = function() { 
      paste("predicted_celltype_Scores", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_subclustering_multiple_celltype_level()[[7]], file)
    }
  )
  
  output$m_subclustering_celltype2_plot<-renderPlot({
    datainput_subclustering_multiple_celltype_level()[[8]]
  })
  
  observeEvent(input$download_m_subclustering_celltype2_plot, {
    showModal(modalDialog(
      title = strong("Download Heatmap"),
      numericInput("m_subclustering_celltype2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_celltype2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_celltype2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_celltype2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_celltype2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_celltype2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("score_plots", input$m_subclustering_celltype2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_celltype_level()[[8]], width = input$m_subclustering_celltype2_plot_width, height = input$m_subclustering_celltype2_plot_height, dpi = input$m_subclustering_celltype2_plot_dpi, units = "in")
    }
  )
  
  
  output$m_subclustering_celltype3_plot<-renderPlot({
    datainput_subclustering_multiple_celltype_level()[[9]]
  })
  
  observeEvent(input$download_m_subclustering_celltype3_plot, {
    showModal(modalDialog(
      title = strong("Download Heatmap"),
      numericInput("m_subclustering_celltype3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_celltype3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("m_subclustering_celltype3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_celltype3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_celltype3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_celltype3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("score_plots", input$m_subclustering_celltype3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_celltype_level()[[9]], width = input$m_subclustering_celltype3_plot_width, height = input$m_subclustering_celltype3_plot_height, dpi = input$m_subclustering_celltype3_plot_dpi, units = "in")
    }
  )
  
  
  
  ###################save seurat object after doublet###################
  output$m_subclustering_celltype <- create_object_download_handler(
    section = "Subclustering",
    action = "Download Seurat Object (After Cell Type Annotation)",
    filename_text = "subclustering_multiple_sample_seuart_object_after_celltypes.RDS",
    object_expr = datainput_subclustering_multiple_celltype_level()[[1]]
  )
  #####################################link to next tab###########################     
  observeEvent(input$link_m_subclustering_clusterbased, {
    newvalue <- "Cluster-Based Plots"
    updateTabsetPanel(session, "subclustering_multiple_tabsets", newvalue)
  })  
  
  
  ###################################Tab2.6###############################  
  ###########################Cluster-based plots####################### 
  observe({
    if(input$m_subclustering_clusterbased1 == "gene_name_list"){
      shinyjs::show("m_subclustering_clusterbased2")
    }
    else {
      shinyjs::hide("m_subclustering_clusterbased2")
    }
  })  
  
  observeEvent(input$subclustering_multiple_sample_clusterbased, {
   if(input$m_subclustering_clusterbased4 ==  "seurat_clusters"){
     shinyjs::show("m_subclustering_clusterbased_box3")
   }
   else {
     shinyjs::hide("m_subclustering_clusterbased_box3")
   }
  })
  
  
  observeEvent(input$subclustering_multiple_sample_clusterbased,{
    shinyjs::show("m_subclustering_clusterbased_box2")
    shinyjs::show("m_subclustering_clusterbased_box3")
    shinyjs::show("m_subclustering_clusterbased_box4")
  })
  
  observe({
    if (input$m_subclustering_clusterbased4 == "seurat_clusters") {
      output$m_subclustering_clusterbased_6 <- renderUI({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "m_subclustering_clusterbased6",
          label = "Select one or multiple cluster(s) for plotting",
          choices = sort(clusters),
          selected = sort(clusters),
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      })
    }
    
    plot_type <- input$m_subclustering_clusterbased3
    grouping <- input$m_subclustering_clusterbased4
    
    if (plot_type %in% c("Dot Plot", "VlnPlot", "RidgePlot")) {
      if (grouping == "seurat_clusters") {
        shinyjs::show("m_subclustering_clusterbased_6")
        shinyjs::show("m_subclustering_clusterbased6")
      } else {
        shinyjs::hide("m_subclustering_clusterbased_6")
        shinyjs::hide("m_subclustering_clusterbased6")
      }
      
      if (plot_type %in%  c("FeaturePlot", "spatial_plot")) {
        shinyjs::hide("m_subclustering_clusterbased5")
      } else {
        shinyjs::show("m_subclustering_clusterbased5")
      }
    } else if (plot_type %in% c("FeaturePlot", "spatial_plot")) {
      shinyjs::hide("m_subclustering_clusterbased_6")
      shinyjs::hide("m_subclustering_clusterbased6")
      shinyjs::show("m_subclustering_clusterbased5")
    }
  })
  
  
  datainput_subclustering_multiple_clusterbased_level <- eventReactive(input$subclustering_multiple_sample_clusterbased,{
    run_logged_analysis(
      section = "Subclustering",
      action = "Cluster-based plots",
      params = capture_run_inputs(c("m_subclustering_clusterbased")),
      expr = {
        source_app_script("scripts/subclustering_multiple_clusterbased.R")
        datainput_subclustering_multiple_clusterbased(index_subclustering_multiple_clusterbased_input = datainput_subclustering_multiple_celltype_level()[[1]], index_subclustering_multiple_clusterbased_features = datainput_subclustering_multiple_marker_level()[[1]], index_m_subclustering_celltype_method = datainput_subclustering_multiple_celltype_level()[[4]], index_m_subclustering_clusterbased1 = input$m_subclustering_clusterbased1, index_m_subclustering_clusterbased2 = input$m_subclustering_clusterbased2, index_m_subclustering_clusterbased3 = input$m_subclustering_clusterbased3, index_m_subclustering_clusterbased4 = input$m_subclustering_clusterbased4, index_m_subclustering_clusterbased5 = input$m_subclustering_clusterbased5, index_m_subclustering_clusterbased6 = input$m_subclustering_clusterbased6)
      }
    )
  })  
  
  output$m_subclustering_clusterbased1_plot<-renderPlot({
    datainput_subclustering_multiple_clusterbased_level()[1]
  })
  
  observeEvent(input$download_m_subclustering_clusterbased1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("m_subclustering_clusterbased1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_subclustering_clusterbased1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_subclustering_clusterbased1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_clusterbased1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_clusterbased1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_clusterbased1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Plots_for_top_or_selected_markers",  input$m_subclustering_clusterbased1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_clusterbased_level()[[1]], width = input$m_subclustering_clusterbased1_plot_width, height = input$m_subclustering_clusterbased1_plot_height, dpi = input$m_subclustering_clusterbased1_plot_dpi, units = "in")
    }
  )
  
  
  
  output$m_subclustering_clusterbased1_table<- renderDataTable(DT::datatable((datainput_subclustering_multiple_clusterbased_level()[[3]]),
                                                                             options = list(
                                                                               scrollX = TRUE,
                                                                               pageLength = 10,
                                                                               dom = "Blfrtip"
                                                                               #bFilter=0
                                                                             ),rownames= FALSE, selection = "none"))
  
  output$download_m_subclustering_clusterbased1_table <- downloadHandler(
    filename = function() { 
      paste("Top_or_selected_Cell_counts_proportion", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_subclustering_multiple_clusterbased_level()[[3]], file)
    }
  )
  
  
  ###################save seurat object after doublet###################
  output$m_subclustering_clusterbased <- create_object_download_handler(
    section = "Subclustering",
    action = "Download Seurat Object (After Cluster-Based Plots)",
    filename_text = "subclustering_multiple_sample_seuart_object_after_plots.RDS",
    object_expr = datainput_subclustering_multiple_clusterbased_level()[[2]]
  )
  
  #####################################link to next tab###########################     
  observeEvent(input$link_m_subclustering_conditionbased, {
    newvalue <- "Condition Based Analysis"
    updateTabsetPanel(session, "subclustering_multiple_tabsets", newvalue)
  })  
  
  
  ##########################Tab2.7###############################      
  ##############Condition-based analysis###################    
  observe({
    if(input$m_subclustering_conditionbased9 == "gene_name_list"){
      shinyjs::show("m_subclustering_conditionbased10")
    }
    else {
      shinyjs::hide("m_subclustering_conditionbased10")
    }
  })
  
  observe({
    if(input$m_subclustering_conditionbased7 == "VolcanoPlot"){
      shinyjs::hide("m_subclustering_conditionbased8")
      shinyjs::hide("m_subclustering_conditionbased9")
      #shinyjs::hide("m_subclustering_conditionbased_box4")
    }
    else{
      shinyjs::show("m_subclustering_conditionbased8")
      shinyjs::show("m_subclustering_conditionbased9")
      #shinyjs::show("m_subclustering_conditionbased_box4")
    }
  })
  
  observeEvent(input$subclustering_multiple_sample_conditionbased,{
    shinyjs::show("m_subclustering_conditionbased_box3")
    shinyjs::show("m_subclustering_conditionbased_box4")
    shinyjs::show("m_subclustering_conditionbased_box5")
  })
  
  
  
  output$m_subclustering_conditionbased_1 <- renderUI ({
    clusters <- req(datainput_subclustering_multiple_clustering_level()[[13]])
    
    shinyWidgets::pickerInput(
      inputId = "m_subclustering_conditionbased1",
      label = "Select the Condition1",
      choices = sort(clusters),
      multiple = F,
      options = list(`actions-box` = TRUE))
  })
  
  output$m_subclustering_conditionbased_2 <- renderUI ({
    clusters <- req(datainput_subclustering_multiple_clustering_level()[[13]])
    clusters <- clusters[!clusters == input$m_subclustering_conditionbased1]
    shinyWidgets::pickerInput(
      inputId = "m_subclustering_conditionbased2",
      label = "Select the Condition2",
      choices = sort(clusters),
      selected = sort(clusters)[1],
      multiple = F,
      options = list(`actions-box` = TRUE))
  })  
  
  observe({
    if(input$m_subclustering_conditionbased9 == "gene_name_list"){
      shinyjs::show("m_subclustering_conditionbased10")
    }
    else {
      shinyjs::hide("m_subclustering_conditionbased10")
    }
  }) 
  
  datainput_subclustering_multiple_conditionbased_level <- eventReactive(input$subclustering_multiple_sample_conditionbased,{
    run_logged_analysis(
      section = "Subclustering",
      action = "Condition-based comparison",
      params = capture_run_inputs(c("subclustering_multiple_sample_", "m_subclustering_conditionbased")),
      expr = {
        source_app_script("scripts/subclustering_multiple_conditionbased.R")
        datainput_subclustering_multiple_conditionbased(index_subclustering_multiple_conditionbased_input = datainput_subclustering_multiple_celltype_level()[[1]], index_subclustering_multiple_sample_normalization_method = input$subclustering_multiple_sample_normalization_method, index_m_subclustering_conditionbased1 = input$m_subclustering_conditionbased1, index_m_subclustering_conditionbased2 = input$m_subclustering_conditionbased2, index_m_subclustering_conditionbased3 = input$m_subclustering_conditionbased3, index_m_subclustering_conditionbased4 = input$m_subclustering_conditionbased4, index_m_subclustering_conditionbased5 = input$m_subclustering_conditionbased5, index_m_subclustering_conditionbased6 = input$m_subclustering_conditionbased6, index_m_subclustering_conditionbased7 = input$m_subclustering_conditionbased7, index_m_subclustering_conditionbased8 = input$m_subclustering_conditionbased8, index_m_subclustering_conditionbased9 = input$m_subclustering_conditionbased9, index_m_subclustering_conditionbased10 = input$m_subclustering_conditionbased10)
      }
    )
  })  
  
  output$m_subclustering_conditionbased1_plot<-renderPlot({
    datainput_subclustering_multiple_conditionbased_level()[1]
  })
  
  observeEvent(input$download_m_subclustering_conditionbased1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("m_subclustering_conditionbased1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_subclustering_conditionbased1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 20, width = "300px"),
      numericInput("m_subclustering_conditionbased1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("m_subclustering_conditionbased1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("m_subclustering_conditionbased1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$m_subclustering_conditionbased1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Plots_for_top_selected_markers",  input$m_subclustering_conditionbased1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_subclustering_multiple_conditionbased_level()[[1]], width = input$m_subclustering_conditionbased1_plot_width, height = input$m_subclustering_conditionbased1_plot_height, dpi = input$m_subclustering_conditionbased1_plot_dpi, units = "in")
    }
  )
  
  
  
  output$m_subclustering_conditionbased1_table<- renderDataTable(DT::datatable((datainput_subclustering_multiple_conditionbased_level()[[2]]),
                                                                               options = list(
                                                                                 scrollX = TRUE,
                                                                                 pageLength = 10,
                                                                                 dom = "Blfrtip"
                                                                                 #bFilter=0
                                                                               ),rownames= FALSE, selection = "none"))
  
  output$download_m_subclustering_conditionbased1_table <- downloadHandler(
    filename = function() { 
      paste("Differentially_expressed genes_sample_based", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_subclustering_multiple_conditionbased_level()[[2]], file)
    }
  )
  
  
  ###################save seurat object after doublet###################
  output$m_subclustering_conditionbased <- create_object_download_handler(
    section = "Subclustering",
    action = "Download Seurat Object (After Condition-Based Analysis)",
    filename_text = "subclustering_multiple_sample_seuart_object_after_plots.RDS",
    object_expr = datainput_subclustering_multiple_conditionbased_level()[[3]]
  )
  
  
  ##############################################Menu3#####################################################################   
  #####################################Cell Cluster Correlation Network##########################################################
  ###################hidebox#################
  shinyjs::hide("s_cccn_box1")
  shinyjs::hide("s_cccn_box2")
  
  observeEvent(input$multiple_sample_celltype, {
    if(input$m_marker1 == 1){
      shinyjs::hide("s_cccn_box0")
      shinyjs::show("s_cccn_box1")
    }
    else{
      shinyjs::show("s_cccn_box0")
      shinyjs::hide("s_cccn_box1") 
    }
  })
  observeEvent(input$subclustering_multiple_sample_celltype, {
    if(input$m_subclustering_marker1 == 1){
      shinyjs::hide("s_cccn_box0")
      shinyjs::show("s_cccn_box1")
    }
    else{
      shinyjs::show("s_cccn_box0")
      shinyjs::hide("s_cccn_box1") 
    }
  })
  
  observeEvent(input$single_multiple_sample_cccn,{
    shinyjs::show("s_cccn_box2")
  })
  
  
  datainput_single_multiple_sample_cccn_level <- eventReactive(input$single_multiple_sample_cccn,{
    run_logged_analysis(
      section = "Co-expression",
      action = "Cluster-based correlation network",
      params = capture_run_inputs(c("s_cccn", "multiple_sample_normalization_method", "subclustering_multiple_sample_normalization_method")),
      expr = {
        source_app_script("scripts/cccn.R")
        datainput_single_multiple_sample_cccn(index_multiple_sample_cccn_input = datainput_multiple_celltype_level()[[1]], index_subclustering_multiple_sample_cccn_input = datainput_subclustering_multiple_celltype_level()[[1]], index_multiple_sample_cccn_input2 = datainput_multiple_celltype_level()[[4]], index_subclustering_multiple_sample_cccn_input2 = datainput_subclustering_multiple_celltype_level()[[4]], index_multiple_sample_normalization_method_cccn = input$multiple_sample_normalization_method, index_subclustering_multiple_sample_normalization_method_cccn = input$subclustering_multiple_sample_normalization_method, index_s_cccn1 = input$s_cccn1, index_s_cccn2 = input$s_cccn2, index_s_cccn3 = input$s_cccn3)
      }
    )
  })  
  
  output$s_cccn1_plot<-renderPlot({
    datainput_single_multiple_sample_cccn_level()[1]
  })
  
  observeEvent(input$download_s_cccn1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_cccn1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cccn1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cccn1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_cccn1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_cccn1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_cccn1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_based_correlation_matrix_plot",  input$s_cccn1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_cccn_level()[[1]], width = input$s_cccn1_plot_width, height = input$s_cccn1_plot_height, dpi = input$s_cccn1_plot_dpi, units = "in")
    }
  )
  
  
  output$s_cccn2_plot<-renderPlot({
    datainput_single_multiple_sample_cccn_level()[2]
  })
  observeEvent(input$download_s_cccn2_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_cccn2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cccn2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cccn2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_cccn2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_cccn2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_cccn2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cluster_based_correlation_network_plot",  input$s_cccn2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_cccn_level()[[2]], width = input$s_cccn2_plot_width, height = input$s_cccn2_plot_height, dpi = input$s_cccn2_plot_dpi, units = "in")
    }
  )
  
  output$s_cccn1_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_cccn_level()[[3]]),
                                                       options = list(
                                                         scrollX = TRUE,
                                                         pageLength = 10,
                                                         bFilter=0
                                                       ),rownames= TRUE, selection = "none"))
  
  output$download_s_cccn1_table <- downloadHandler(
    filename = function() { 
      paste("Cluster_based_correlation_table", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_cccn_level()[[3]], file)
    }
  )
  
  
  
  
  
  ######################################################Menu4#####################################################################   
  ####################################################GO terms####################################################################  
  ###################hidebox#################
  shinyjs::hide("s_go_box1")
  shinyjs::hide("s_go_box2")
  shinyjs::hide("s_go_box3")
  
  
  observeEvent(input$multiple_sample_celltype, {
    if(input$m_marker1 == 1){
      shinyjs::hide("s_go_box0")
      shinyjs::show("s_go_box1")
      shinyjs::show("s_go_box2")
    }
    else{
      shinyjs::show("s_go_box0")
      shinyjs::hide("s_go_box1") 
      shinyjs::hide("s_go_box2") 
    }
  })
  observeEvent(input$subclustering_multiple_sample_celltype, {
    if(input$m_subclustering_marker1 == 1){
      shinyjs::hide("s_go_box0")
      shinyjs::show("s_go_box1")
      shinyjs::show("s_go_box2")
    }
    else{
      shinyjs::show("s_go_box0")
      shinyjs::hide("s_go_box1") 
      shinyjs::hide("s_go_box2") 
    }
  })
  
  observe({
    if(input$s_go1 == "gene_name_list"){
      shinyjs::show("s_go14")
      shinyjs::hide("s_go2")
      shinyjs::hide("s_go3")
      shinyjs::hide("s_go_3")
      shinyjs::hide("s_go4")
    }
    else {
      shinyjs::hide("s_go14")
      shinyjs::show("s_go2")
      shinyjs::show("s_go3")
      shinyjs::show("s_go_3")
      shinyjs::show("s_go4")
    }
  })
  
  
  observeEvent(input$single_multiple_sample_go,{
    shinyjs::show("s_go_box3")
  })
  
  observe({
    
    if (input$s_go1 == "multiple_sample" & input$s_go2 == "seurat_clusters"){
      output$s_go_3 <- renderUI ({
        clusters <- req(datainput_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "s_go3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }
    else if (input$s_go1 == "multiple_sample" & input$s_go2 == "predicted"){
      output$s_go_3 <- renderUI ({
        clusters <- req(datainput_multiple_celltype_level()[[3]])
        shinyWidgets::pickerInput(
          inputId = "s_go3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }  
    else if (input$s_go1 == "multiple_sample_subclustering" & input$s_go2 == "seurat_clusters"){
      output$s_go_3 <- renderUI ({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "s_go3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }
    else if (input$s_go1 == "multiple_sample_subclustering" & input$s_go2 == "predicted"){
      output$s_go_3 <- renderUI ({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[3]])
        shinyWidgets::pickerInput(
          inputId = "s_go3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }
  })
  
  datainput_single_multiple_sample_go_level <- eventReactive(input$single_multiple_sample_go,{
    run_logged_analysis(
      section = "Functional Analysis",
      action = "GO term enrichment",
      params = capture_run_inputs(c("s_go")),
      expr = {
        source_app_script("scripts/go.R")
        datainput_single_multiple_sample_go(index_multiple_sample_go_input = datainput_multiple_celltype_level()[[1]], index_subclustering_multiple_sample_go_input = datainput_subclustering_multiple_celltype_level()[[1]], index_multiple_sample_go_input2 = datainput_multiple_celltype_level()[[4]], index_subclustering_multiple_sample_go_input2 = datainput_subclustering_multiple_celltype_level()[[4]], index_multiple_sample_go_input3 = datainput_multiple_marker_level()[[1]], index_subclustering_multiple_sample_go_input3 = datainput_subclustering_multiple_marker_level()[[1]], index_s_go1 = input$s_go1, index_s_go2 = input$s_go2, index_s_go3 = input$s_go3, index_s_go4 = input$s_go4, index_s_go5 = input$s_go5, index_s_go6 = input$s_go6, index_s_go7 = input$s_go7, index_s_go8 = input$s_go8, index_s_go9 = input$s_go9, index_s_go10 = input$s_go10, index_s_go11 = input$s_go11,index_s_go12= input$s_go12, index_s_go13 = input$s_go13, index_s_go14 = input$s_go14)
      }
    )
  })  
  
  output$s_go1_plot<-renderPlot({
    datainput_single_multiple_sample_go_level()[[1]]
  })
  
  observeEvent(input$download_s_go1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_go1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_go1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_go1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_go1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_go1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  output$s_go1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Go_terms_", input$s_go12, input$s_go1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_go_level()[[1]], width = input$s_go1_plot_width, height = input$s_go1_plot_height, dpi = input$s_go1_plot_dpi, units = "in")
    }
  )
  
  output$s_go1_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_go_level()[[2]]),
                                                     options = list(
                                                       scrollX = TRUE,
                                                       pageLength = 10,
                                                       dom = "Blfrtip"
                                                       #bFilter=0
                                                     ),rownames= FALSE, selection = "none"))
  
  output$download_s_go1_table <- downloadHandler(
    filename = function() { 
      paste("Go_terms_summary_table", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_go_level()[[2]], file)
    }
  )
  
  #############################################################Menu5#####################################################################   
  ####################################################Pathway analsis####################################################################  
  ###################hidebox#################
  shinyjs::hide("s_pathway_box1")
  shinyjs::hide("s_pathway_box2")
  shinyjs::hide("s_pathway_box3")
  
  
  observeEvent(input$multiple_sample_celltype, {
    if(input$m_marker1 == 1){
      shinyjs::hide("s_pathway_box0")
      shinyjs::show("s_pathway_box1")
      shinyjs::show("s_pathway_box2")
    }
    else{
      shinyjs::show("s_pathway_box0")
      shinyjs::hide("s_pathway_box1") 
      shinyjs::hide("s_pathway_box2")
    }
  })
  observeEvent(input$subclustering_multiple_sample_celltype, {
    if(input$m_subclustering_marker1 == 1){
      shinyjs::hide("s_pathway_box0")
      shinyjs::show("s_pathway_box1")
      shinyjs::show("s_pathway_box2")
    }
    else{
      shinyjs::show("s_pathway_box0")
      shinyjs::hide("s_pathway_box1") 
      shinyjs::hide("s_pathway_box2")
    }
  })
  
  observe({
    if(input$s_pathway1 == "gene_name_list"){
      shinyjs::show("s_pathway14")
      shinyjs::hide("s_pathway2")
      shinyjs::hide("s_pathway3")
      shinyjs::hide("s_pathway_3")
      shinyjs::hide("s_pathway4")
    }
    else {
      shinyjs::hide("s_pathway14")
      shinyjs::show("s_pathway2")
      shinyjs::show("s_pathway3")
      shinyjs::show("s_pathway_3")
      shinyjs::show("s_pathway4")
    }
  })
  
  observeEvent(input$single_multiple_sample_pathway,{
    shinyjs::show("s_pathway_box3")
  })
  
  observe({
    
    if (input$s_pathway1 == "multiple_sample" & input$s_pathway2 == "seurat_clusters"){
      output$s_pathway_3 <- renderUI ({
        clusters <- req(datainput_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "s_pathway3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }
    else if (input$s_pathway1 == "multiple_sample" & input$s_pathway2 == "predicted"){
      output$s_pathway_3 <- renderUI ({
        clusters <- req(datainput_multiple_celltype_level()[[3]])
        shinyWidgets::pickerInput(
          inputId = "s_pathway3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }  
    else if (input$s_pathway1 == "multiple_sample_subclustering" & input$s_pathway2 == "seurat_clusters"){
      output$s_pathway_3 <- renderUI ({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "s_pathway3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }
    else if (input$s_pathway1 == "multiple_sample_subclustering" & input$s_pathway2 == "predicted"){
      output$s_pathway_3 <- renderUI ({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[3]])
        shinyWidgets::pickerInput(
          inputId = "s_pathway3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }
  })
  
  datainput_single_multiple_sample_pathway_level <- eventReactive(input$single_multiple_sample_pathway,{
    run_logged_analysis(
      section = "Functional Analysis",
      action = "Pathway enrichment",
      params = capture_run_inputs(c("s_pathway")),
      expr = {
        source_app_script("scripts/pathway.R")
        datainput_single_multiple_sample_pathway(index_multiple_sample_pathway_input = datainput_multiple_celltype_level()[[1]], index_subclustering_multiple_sample_pathway_input = datainput_subclustering_multiple_celltype_level()[[1]], index_multiple_sample_pathway_input2 = datainput_multiple_celltype_level()[[4]], index_subclustering_multiple_sample_pathway_input2 = datainput_subclustering_multiple_celltype_level()[[4]], index_multiple_sample_pathway_input3 = datainput_multiple_marker_level()[[1]], index_subclustering_multiple_sample_pathway_input3 = datainput_subclustering_multiple_marker_level()[[1]], index_s_pathway1 = input$s_pathway1, index_s_pathway2 = input$s_pathway2, index_s_pathway3 = input$s_pathway3, index_s_pathway4 = input$s_pathway4, index_s_pathway5 = input$s_pathway5, index_s_pathway6 = input$s_pathway6, index_s_pathway7 = input$s_pathway7, index_s_pathway8 = input$s_pathway8, index_s_pathway9 = input$s_pathway9, index_s_pathway10 = input$s_pathway10, index_s_pathway11 = input$s_pathway11,index_s_pathway12= input$s_pathway12, index_s_pathway13 = input$s_pathway13, index_s_pathway14 = input$s_pathway14)
      }
    )
  })  
  
  output$s_pathway1_plot<-renderPlot({
    datainput_single_multiple_sample_pathway_level()[[1]]
  })
  
  observeEvent(input$download_s_pathway1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_pathway1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_pathway1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_pathway1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_pathway1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_pathway1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  output$s_pathway1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("pathway_", input$s_pathway6, "_", input$s_pathway12, input$s_pathway1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_pathway_level()[[1]], width = input$s_pathway1_plot_width, height = input$s_pathway1_plot_height, dpi = input$s_pathway1_plot_dpi, units = "in")
    }
  )
  
  output$s_pathway1_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_pathway_level()[[2]]),
                                                          options = list(
                                                            scrollX = TRUE,
                                                            pageLength = 10,
                                                            dom = "Blfrtip"
                                                            #bFilter=0
                                                          ),rownames= FALSE, selection = "none"))
  
  output$download_s_pathway1_table <- downloadHandler(
    filename = function() { 
      paste("Pathway_summary_table", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_pathway_level()[[2]], file)
    }
  )
  
  ######################################################Menu6#####################################################################   
  ####################################################GSEA terms####################################################################  
  ###################hidebox#################
  shinyjs::hide("s_gsea_box1")
  shinyjs::hide("s_gsea_box2")
  shinyjs::hide("s_gsea_box3")
  
  
  observeEvent(input$multiple_sample_celltype, {
    if(input$m_marker1 == 1){
      shinyjs::hide("s_gsea_box0")
      shinyjs::show("s_gsea_box1")
      shinyjs::show("s_gsea_box2")
    }
    else{
      shinyjs::show("s_gsea_box0")
      shinyjs::hide("s_gsea_box1")
      shinyjs::hide("s_gsea_box2")
    }
  })
  observeEvent(input$subclustering_multiple_sample_celltype, {
    if(input$m_subclustering_marker1 == 1){
      shinyjs::hide("s_gsea_box0")
      shinyjs::show("s_gsea_box1")
      shinyjs::show("s_gsea_box2")
    }
    else{
      shinyjs::show("s_gsea_box0")
      shinyjs::hide("s_gsea_box1")
      shinyjs::hide("s_gsea_box2")
    }
  })
  
  
  
  get_msigdb_collection_choices <- function(species = "Homo sapiens") {
    if (identical(species, "Mus musculus")) {
      return(c(
        "Mouse hallmark gene sets (MH)" = "MH",
        "Mouse positional gene sets (M1)" = "M1",
        "Mouse curated gene sets (M2)" = "M2",
        "Mouse regulatory target gene sets (M3)" = "M3",
        "Mouse ontology gene sets (M5)" = "M5",
        "Mouse immunologic signature gene sets (M7)" = "M7",
        "Mouse cell type signature gene sets (M8)" = "M8"
      ))
    }

    c(
      "Hallmark gene sets (H)" = "H",
      "Positional gene sets (C1)" = "C1",
      "Curated gene sets (C2)" = "C2",
      "Regulatory target gene sets (C3)" = "C3",
      "Computational gene sets (C4)" = "C4",
      "Ontology gene sets (C5)" = "C5",
      "Oncogenic signature gene sets (C6)" = "C6",
      "Immunologic signature gene sets (C7)" = "C7",
      "Cell type signature gene sets (C8)" = "C8",
      "Computational perturbation signature gene sets (C9)" = "C9"
    )
  }

  normalize_msigdb_collection_choice <- function(species, collection_value) {
    human_to_mouse <- c(H = "MH", C1 = "M1", C2 = "M2", C3 = "M3", C5 = "M5", C7 = "M7", C8 = "M8")
    mouse_to_human <- stats::setNames(names(human_to_mouse), human_to_mouse)
    available_values <- unname(get_msigdb_collection_choices(species))

    if (is.null(collection_value) || !length(collection_value) || !nzchar(collection_value)) {
      return(if (identical(species, "Mus musculus")) "M2" else "C2")
    }

    if (collection_value %in% available_values) {
      return(collection_value)
    }

    mapped_value <- if (identical(species, "Mus musculus")) {
      unname(human_to_mouse[collection_value])
    } else {
      unname(mouse_to_human[collection_value])
    }

    if (!is.na(mapped_value) && nzchar(mapped_value) && mapped_value %in% available_values) {
      return(mapped_value)
    }

    if (identical(species, "Mus musculus")) "M2" else "C2"
  }

  observeEvent(input$s_gsea5, {
    current_collection <- isolate(input$s_gsea6)
    selected_species <- input$s_gsea5 %||% "Homo sapiens"
    updateSelectInput(
      session,
      "s_gsea6",
      choices = get_msigdb_collection_choices(selected_species),
      selected = normalize_msigdb_collection_choice(selected_species, current_collection)
    )
  }, ignoreInit = FALSE)

  observeEvent(input$single_multiple_sample_gsea,{
    shinyjs::show("s_gsea_box3")
  })
  
  observe({
    
    if (input$s_gsea1 == "multiple_sample" & input$s_gsea2 == "seurat_clusters"){
      output$s_gsea_3 <- renderUI ({
        clusters <- req(datainput_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "s_gsea3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }
    else if (input$s_gsea1 == "multiple_sample" & input$s_gsea2 == "predicted"){
      output$s_gsea_3 <- renderUI ({
        clusters <- req(datainput_multiple_celltype_level()[[3]])
        shinyWidgets::pickerInput(
          inputId = "s_gsea3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }  
    else if (input$s_gsea1 == "multiple_sample_subclustering" & input$s_gsea2 == "seurat_clusters"){
      output$s_gsea_3 <- renderUI ({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "s_gsea3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }
    else if (input$s_gsea1 == "multiple_sample_subclustering" & input$s_gsea2 == "predicted"){
      output$s_gsea_3 <- renderUI ({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[3]])
        shinyWidgets::pickerInput(
          inputId = "s_gsea3",
          label = "Select one or multiple cluster(s) for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = T,
          options = list(`actions-box` = TRUE))
      })
    }
  })
  
  datainput_single_multiple_sample_gsea_level <- eventReactive(input$single_multiple_sample_gsea,{
    run_logged_analysis(
      section = "Functional Analysis",
      action = "GSEA",
      params = capture_run_inputs(c("s_gsea")),
      expr = {
        source_app_script("scripts/gsea.R")
        datainput_single_multiple_sample_gsea(index_multiple_sample_gsea_input = datainput_multiple_celltype_level()[[1]], index_subclustering_multiple_sample_gsea_input = datainput_subclustering_multiple_celltype_level()[[1]], index_multiple_sample_gsea_input2 = datainput_multiple_celltype_level()[[4]], index_subclustering_multiple_sample_gsea_input2 = datainput_subclustering_multiple_celltype_level()[[4]], index_multiple_sample_gsea_input3 = datainput_multiple_marker_level()[[1]], index_subclustering_multiple_sample_gsea_input3 = datainput_subclustering_multiple_marker_level()[[1]], index_s_gsea1 = input$s_gsea1, index_s_gsea2 = input$s_gsea2, index_s_gsea3 = input$s_gsea3, index_s_gsea4 = input$s_gsea4, index_s_gsea5 = input$s_gsea5, index_s_gsea6 = input$s_gsea6, index_s_gsea7 = input$s_gsea7, index_s_gsea8 = input$s_gsea8, index_s_gsea9 = input$s_gsea9, index_s_gsea10 = input$s_gsea10, index_s_gsea11 = input$s_gsea11,index_s_gsea12= input$s_gsea12)
      }
    )
  })  
  
  output$s_gsea1_plot<-renderPlot({
    datainput_single_multiple_sample_gsea_level()[1]
  })
  
  observeEvent(input$download_s_gsea1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_gsea1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 10, width = "300px"),
      numericInput("s_gsea1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 15, width = "300px"),
      numericInput("s_gsea1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_gsea1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_gsea1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  output$s_gsea1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("gsea_plot",  input$s_gsea1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_gsea_level()[[1]], width = input$s_gsea1_plot_width, height = input$s_gsea1_plot_height, dpi = input$s_gsea1_plot_dpi, units = "in")
    }
  )
  
  output$s_gsea1_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_gsea_level()[[2]]),
                                                       options = list(
                                                         scrollX = TRUE,
                                                         pageLength = 10,
                                                         dom = "Blfrtip"
                                                         #bFilter=0
                                                       ),rownames= FALSE, selection = "none"))
  
  output$download_s_gsea1_table <- downloadHandler(
    filename = function() { 
      paste("GSEA_summary_table", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_gsea_level()[[2]], file)
    }
  )
  
  ##############################################Menu7#####################################################################   
  #####################################Cell-Cell Communication##########################################################
  ###################hidebox#################
  shinyjs::hide("s_cellchat_box1")
  shinyjs::hide("s_cellchat_box2")
  shinyjs::hide("s_cellchat_box3")
  shinyjs::hide("s_cellchat_box4")
  shinyjs::hide("s_cellchat_box5")
  shinyjs::hide("s_cellchat_box6")
  shinyjs::hide("s_cellchat_box7")
  shinyjs::hide("s_cellchat_box8")
  shinyjs::hide("s_cellchat_box9")
  shinyjs::hide("s_cellchat_box10")
  shinyjs::hide("s_cellchat_box11")
  shinyjs::hide("s_cellchat_box12")
  
  
  observeEvent(input$multiple_sample_celltype, {
    if(input$m_marker1 == 1){
      shinyjs::hide("s_cellchat_box0")
      shinyjs::show("s_cellchat_box1")
    }
    else{
      shinyjs::show("s_cellchat_box0")
      shinyjs::hide("s_cellchat_box1") 
    }
  })
  observeEvent(input$subclustering_multiple_sample_celltype, {
    if(input$m_subclustering_marker1 == 1){
      shinyjs::hide("s_cellchat_box0")
      shinyjs::show("s_cellchat_box1")
    }
    else{
      shinyjs::show("s_cellchat_box0")
      shinyjs::hide("s_cellchat_box1") 
    }
  })
  
  observeEvent(input$single_multiple_sample_cellchat1,{
    shinyjs::show("s_cellchat_box2")
    shinyjs::show("s_cellchat_box3")
    shinyjs::show("s_cellchat_box4")
    shinyjs::show("s_cellchat_box5")
    shinyjs::show("s_cellchat_box6")
  })
  
  observeEvent(input$single_multiple_sample_cellchat2,{
    shinyjs::show("s_cellchat_box7")
    shinyjs::show("s_cellchat_box8")
    shinyjs::show("s_cellchat_box9")
    shinyjs::show("s_cellchat_box10")
    shinyjs::show("s_cellchat_box11")
    shinyjs::show("s_cellchat_box12")
  })
  
  
   observe({
    if (input$s_cellchat13 == "FALSE") {
      shinyjs::hide("s_cellchat14")
      }
    else if (input$s_cellchat13  == "TRUE") {
      shinyjs::show("s_cellchat14")
    }
  })
  
  datainput_single_multiple_sample_cellchat1_level <- eventReactive(input$single_multiple_sample_cellchat1,{
    run_logged_analysis(
      section = "CellChat",
      action = "Communication inference",
      params = capture_run_inputs(c("s_cellchat", "multiple_sample_normalization_method", "subclustering_multiple_sample_normalization_method")),
      expr = {
        source_app_script("scripts/cellchat1.R")
        datainput_single_multiple_sample_cellchat1(index_multiple_sample_cellchat1_input = datainput_multiple_celltype_level()[[1]], index_subclustering_multiple_sample_cellchat1_input = datainput_subclustering_multiple_celltype_level()[[1]], index_multiple_sample_cellchat1_input2 = datainput_multiple_celltype_level()[[4]], index_subclustering_multiple_sample_cellchat1_input2 = datainput_subclustering_multiple_celltype_level()[[4]], index_multiple_sample_normalization_method_cellchat1 = input$multiple_sample_normalization_method, index_subclustering_multiple_sample_normalization_method_cellchat1 = input$subclustering_multiple_sample_normalization_method, index_s_cellchat1 = input$s_cellchat1, index_s_cellchat2 = input$s_cellchat2, index_s_cellchat3 = input$s_cellchat3, index_s_cellchat4 = input$s_cellchat4, index_s_cellchat5 = input$s_cellchat5, index_s_cellchat6 = input$s_cellchat6, index_s_cellchat7 = input$s_cellchat7, index_s_cellchat8 = input$s_cellchat8, index_s_cellchat9 = input$s_cellchat9, index_s_cellchat10 = input$s_cellchat10, index_s_cellchat13 = input$s_cellchat13, index_s_cellchat14 = input$s_cellchat14, index_s_cellchat15 = input$s_cellchat15, index_s_cellchat16 = input$s_cellchat16, index_s_cellchat17 = input$s_cellchat17)
      }
    )
  })  
  
  output$s_cellchat1_plot<-renderPlot({
    #grid.newpage() 
    datainput_single_multiple_sample_cellchat1_level()[1]
  })
  
  
  # observeEvent(input$download_s_cellchat1_plot, {
  #   showModal(modalDialog(
  #     title = strong("Download plot"),
  #     numericInput("s_cellchat1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
  #     selectInput("s_cellchat1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
  #     downloadBttn("s_cellchat1_plot_downloadoutput", "Download"),
  #     size = "s",
  #     easyClose = TRUE,
  #     #footer = NULL
  #   ))
  # })
  # output$s_cellchat1_plot_downloadoutput<- downloadHandler(
  #   filename = function(){
  #     paste("Number_of_interactions", input$s_cellchat1_plot_type, sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = datainput_single_multiple_sample_cellchat1_level()[[1]], width = input$s_cellchat1_plot_width, height = input$s_cellchat1_plot_height, dpi = input$s_cellchat1_plot_dpi, units = "in")
  #   }
  # )
  
  
  
  output$s_cellchat2_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat1_level()[2]
  })
  # observeEvent(input$download_s_cellchat2_plot, {
  #   showModal(modalDialog(
  #     title = strong("Download plot"),
  #     numericInput("s_cellchat2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
  #     selectInput("s_cellchat2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
  #     downloadBttn("s_cellchat2_plot_downloadoutput", "Download"),
  #     size = "s",
  #     easyClose = TRUE,
  #     #footer = NULL
  #   ))
  # })
  # output$s_cellchat2_plot_downloadoutput<- downloadHandler(
  #   filename = function(){
  #     paste("Interaction_weights_or_strength", input$s_cellchat2_plot_type, sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = datainput_single_multiple_sample_cellchat1_level()[[2]], width = input$s_cellchat2_plot_width, height = input$s_cellchat2_plot_height, dpi = input$s_cellchat2_plot_dpi, units = "in")
  #   }
  # )
  
  
  output$s_cellchat3_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat1_level()[3]
  })
  # observeEvent(input$download_s_cellchat3_plot, {
  #   showModal(modalDialog(
  #     title = strong("Download plot"),
  #     numericInput("s_cellchat3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
  #     selectInput("s_cellchat3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
  #     downloadBttn("s_cellchat3_plot_downloadoutput", "Download"),
  #     size = "s",
  #     easyClose = TRUE,
  #     #footer = NULL
  #   ))
  # })
  # output$s_cellchat3_plot_downloadoutput<- downloadHandler(
  #   filename = function(){
  #     paste("Interactions_heatmap", input$s_cellchat3_plot_type, sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = datainput_single_multiple_sample_cellchat1_level()[[3]], width = input$s_cellchat3_plot_width, height = input$s_cellchat3_plot_height, dpi = input$s_cellchat3_plot_dpi, units = "in")
  #   }
  # )
  
  
  output$s_cellchat4_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat1_level()[4]
  })
  # observeEvent(input$download_s_cellchat4_plot, {
  #   showModal(modalDialog(
  #     title = strong("Download plot"),
  #     numericInput("s_cellchat4_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat4_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat4_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
  #     selectInput("s_cellchat4_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
  #     downloadBttn("s_cellchat4_plot_downloadoutput", "Download"),
  #     size = "s",
  #     easyClose = TRUE,
  #     #footer = NULL
  #   ))
  # })
  # output$s_cellchat4_plot_downloadoutput<- downloadHandler(
  #   filename = function(){
  #     paste("Signaling_patterns", input$s_cellchat4_plot_type, sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = datainput_single_multiple_sample_cellchat1_level()[[4]], width = input$s_cellchat4_plot_width, height = input$s_cellchat4_plot_height, dpi = input$s_cellchat4_plot_dpi, units = "in")
  #   }
  # )
  
  output$s_cellchat12_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat1_level()[5]
  })
  # observeEvent(input$download_s_cellchat12_plot, {
  #   showModal(modalDialog(
  #     title = strong("Download plot"),
  #     numericInput("s_cellchat12_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat12_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat12_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
  #     selectInput("s_cellchat12_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
  #     downloadBttn("s_cellchat12_plot_downloadoutput", "Download"),
  #     size = "s",
  #     easyClose = TRUE,
  #     #footer = NULL
  #   ))
  # })
  # output$s_cellchat12_plot_downloadoutput<- downloadHandler(
  #   filename = function(){
  #     paste("communication_patterns", input$s_cellchat12_plot_type, sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = datainput_single_multiple_sample_cellchat1_level()[[5]], width = input$s_cellchat12_plot_width, height = input$s_cellchat12_plot_height, dpi = input$s_cellchat12_plot_dpi, units = "in")
  #   }
  # )
  
  
  output$s_cellchat1_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_cellchat1_level()[[6]]),
                                                           options = list(
                                                             scrollX = TRUE,
                                                             pageLength = 10,
                                                             dom = "Blfrtip"
                                                             #bFilter=0
                                                           ),rownames= FALSE, selection = "none"))
  
  output$download_s_cellchat1_table <- downloadHandler(
    filename = function() { 
      paste("cellchat_summary_table", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_cellchat1_level()[[6]], file)
    }
  )
  
  
  ##############################################submenu#########################################
  observe({
    output$s_cellchat_11 <- renderUI ({
      clusters <- req(datainput_single_multiple_sample_cellchat1_level()[[7]])
      shinyWidgets::pickerInput(
        inputId = "s_cellchat11",
        label = "Select one signaligng pathway for vizualization",
        choices = clusters,
        multiple = F,
        options = list(`actions-box` = TRUE))
    })
  })
  
  datainput_single_multiple_sample_cellchat2_level <- eventReactive(input$single_multiple_sample_cellchat2,{
    run_logged_analysis(
      section = "CellChat",
      action = "Pathway-level visualization",
      params = capture_run_inputs(c("s_cellchat")),
      expr = {
        source_app_script("scripts/cellchat2.R")
        datainput_single_multiple_sample_cellchat2(index_single_sample_cellchat2_input = datainput_single_multiple_sample_cellchat1_level()[[8]],  index_s_cellchat11 = input$s_cellchat11, index_s_cellchat12 = input$s_cellchat12)
      }
    )
  })  
  
  output$s_cellchat13_plot<-renderPlot({
    plot_obj <- datainput_single_multiple_sample_cellchat2_level()[[10]]
    if (inherits(plot_obj, "recordedplot")) {
      grDevices::replayPlot(plot_obj)
    } else {
      print(plot_obj)
    }
  })
  observeEvent(input$download_s_cellchat13_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_cellchat13_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cellchat13_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cellchat13_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_cellchat13_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_cellchat13_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_cellchat13_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Interactions_spatial_plot", input$s_cellchat13_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_cellchat2_level()[[10]], width = input$s_cellchat13_plot_width, height = input$s_cellchat13_plot_height, dpi = input$s_cellchat13_plot_dpi, units = "in")
    }
  )
  
  
  output$s_cellchat5_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat2_level()[1]
  })
  # observeEvent(input$download_s_cellchat5_plot, {
  #   showModal(modalDialog(
  #     title = strong("Download plot"),
  #     numericInput("s_cellchat5_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat5_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat5_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
  #     selectInput("s_cellchat5_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
  #     downloadBttn("s_cellchat5_plot_downloadoutput", "Download"),
  #     size = "s",
  #     easyClose = TRUE,
  #     #footer = NULL
  #   ))
  # })
  # output$s_cellchat5_plot_downloadoutput<- downloadHandler(
  #   filename = function(){
  #     paste("Number_of_interactions_circle_plot", input$s_cellchat5_plot_type, sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = datainput_single_multiple_sample_cellchat2_level()[[1]], width = input$s_cellchat5_plot_width, height = input$s_cellchat5_plot_height, dpi = input$s_cellchat5_plot_dpi, units = "in")
  #   }
  # )
  
  output$s_cellchat6_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat2_level()[2]
  })
  # observeEvent(input$download_s_cellchat6_plot, {
  #   showModal(modalDialog(
  #     title = strong("Download plot"),
  #     numericInput("s_cellchat6_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat6_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat6_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
  #     selectInput("s_cellchat6_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
  #     downloadBttn("s_cellchat6_plot_downloadoutput", "Download"),
  #     size = "s",
  #     easyClose = TRUE,
  #     #footer = NULL
  #   ))
  # })
  # output$s_cellchat6_plot_downloadoutput<- downloadHandler(
  #   filename = function(){
  #     paste("Number_of_interactions_chord_plot", input$s_cellchat6_plot_type, sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = datainput_single_multiple_sample_cellchat2_level()[[2]], width = input$s_cellchat6_plot_width, height = input$s_cellchat6_plot_height, dpi = input$s_cellchat6_plot_dpi, units = "in")
  #   }
  # )
  
  
  output$s_cellchat7_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat2_level()[3]
  })
  # observeEvent(input$download_s_cellchat7_plot, {
  #   showModal(modalDialog(
  #     title = strong("Download plot"),
  #     numericInput("s_cellchat7_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat7_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat7_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
  #     selectInput("s_cellchat7_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
  #     downloadBttn("s_cellchat7_plot_downloadoutput", "Download"),
  #     size = "s",
  #     easyClose = TRUE,
  #     #footer = NULL
  #   ))
  # })
  # output$s_cellchat7_plot_downloadoutput<- downloadHandler(
  #   filename = function(){
  #     paste("interaction_heatmap", input$s_cellchat7_plot_type, sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = datainput_single_multiple_sample_cellchat2_level()[[3]], width = input$s_cellchat7_plot_width, height = input$s_cellchat7_plot_height, dpi = input$s_cellchat7_plot_dpi, units = "in")
  #   }
  # )
  
  output$s_cellchat11_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat2_level()[4]
  })
  # observeEvent(input$download_s_cellchat11_plot, {
  #   showModal(modalDialog(
  #     title = strong("Download plot"),
  #     numericInput("s_cellchat11_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat11_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
  #     numericInput("s_cellchat11_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
  #     selectInput("s_cellchat11_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
  #     downloadBttn("s_cellchat11_plot_downloadoutput", "Download"),
  #     size = "s",
  #     easyClose = TRUE,
  #     #footer = NULL
  #   ))
  # })
  # output$s_cellchat11_plot_downloadoutput<- downloadHandler(
  #   filename = function(){
  #     paste("Hierachy_plot", input$s_cellchat11_plot_type, sep="")
  #   },
  #   content = function(file){
  #     ggsave(file,plot = datainput_single_multiple_sample_cellchat2_level()[[4]], width = input$s_cellchat11_plot_width, height = input$s_cellchat11_plot_height, dpi = input$s_cellchat11_plot_dpi, units = "in")
  #   }
  # )
  
  output$s_cellchat8_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat2_level()[5]
  })
  observeEvent(input$download_s_cellchat8_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_cellchat8_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cellchat8_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cellchat8_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_cellchat8_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_cellchat8_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_cellchat8_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Bubble_plot", input$s_cellchat8_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_cellchat2_level()[[5]], width = input$s_cellchat8_plot_width, height = input$s_cellchat8_plot_height, dpi = input$s_cellchat8_plot_dpi, units = "in")
    }
  )
  
  output$s_cellchat9_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat2_level()[6]
  })
  observeEvent(input$download_s_cellchat9_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_cellchat9_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cellchat9_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cellchat9_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_cellchat9_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_cellchat9_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_cellchat9_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Network_Analysis_contribution_Bar_plot", input$s_cellchat9_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_cellchat2_level()[[6]], width = input$s_cellchat9_plot_width, height = input$s_cellchat9_plot_height, dpi = input$s_cellchat9_plot_dpi, units = "in")
    }
  )
  
  output$s_cellchat10_plot<-renderPlot({
    datainput_single_multiple_sample_cellchat2_level()[7]
  })
  observeEvent(input$download_s_cellchat10_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_cellchat10_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cellchat10_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_cellchat10_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_cellchat10_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_cellchat10_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_cellchat10_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Interaction_table_", input$s_cellchat10, input$s_cellchat10_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_cellchat2_level()[[7]], width = input$s_cellchat10_plot_width, height = input$s_cellchat10_plot_height, dpi = input$s_cellchat10_plot_dpi, units = "in")
    }
  )
  
  
  
  output$s_cellchat2_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_cellchat2_level()[[8]]),
                                                           options = list(
                                                             scrollX = TRUE,
                                                             pageLength = 10,
                                                             dom = "Blfrtip"
                                                             #bFilter=0
                                                           ),rownames= FALSE, selection = "none"))
  
  output$download_s_cellchat7_table <- downloadHandler(
    filename = function() { 
      paste("cellchat_summary_table_", input$s_cellchat10, '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_cellchat2_level()[[8]], file)
    }
  )  
  
  ######################################################Menu8#####################################################################   
  #############################################Trajectory and Pseudotime analysis#################################################
  
  hideTab(inputId = "Coexpression_tabsets", target = "Transcription Factor Regulatory Network Analysis")
  observeEvent(input$link_s_tfrn, {
    showTab(inputId = "Coexpression_tabsets", target = "Transcription Factor Regulatory Network Analysis")
  })
  
  #####################################link to next tab###########################     
  observeEvent(input$link_s_tfrn, {
    newvalue <- "Transcription Factor Regulatory Network Analysis"
    updateTabsetPanel(session, "Coexpression_tabsets", newvalue)
  })  
  
  
  ###################hidebox#################
  shinyjs::hide("s_trajectory_box1")
  shinyjs::hide("s_trajectory_box2")
  shinyjs::hide("s_trajectory_box3")
  shinyjs::hide("s_trajectory_box4")
  shinyjs::hide("s_trajectory_box5")
  shinyjs::hide("s_trajectory_box6")
  shinyjs::hide("s_trajectory_box7")
  shinyjs::hide("s_trajectory_box8")
  shinyjs::hide("s_trajectory18")
  
  
  observeEvent(input$multiple_sample_celltype, {
    if(input$m_marker1 == 1){
      shinyjs::hide("s_trajectory_box0")
      shinyjs::show("s_trajectory_box1")
    }
    else{
      shinyjs::show("s_trajectory_box0")
      shinyjs::hide("s_trajectory_box1") 
    }
  })
  observeEvent(input$subclustering_multiple_sample_celltype, {
    if(input$m_subclustering_marker1 == 1){
      shinyjs::hide("s_trajectory_box0")
      shinyjs::show("s_trajectory_box1")
    }
    else{
      shinyjs::show("s_trajectory_box0")
      shinyjs::hide("s_trajectory_box1") 
    }
  })
  
  observeEvent(input$single_multiple_sample_trajectory1,{
    shinyjs::show("s_trajectory_box2")
    shinyjs::show("s_trajectory_box3")
    shinyjs::hide("s_trajectory_box4")
    shinyjs::hide("s_trajectory_box5")
    shinyjs::hide("s_trajectory_box6")
    shinyjs::hide("s_trajectory_box7")
    shinyjs::hide("s_trajectory_box8")
  })
  
  observeEvent(input$single_multiple_sample_trajectory2,{
    shinyjs::show("s_trajectory_box2")
    shinyjs::show("s_trajectory_box3")
    shinyjs::show("s_trajectory_box4")
    shinyjs::show("s_trajectory_box5")
    shinyjs::hide("s_trajectory_box6")
    shinyjs::hide("s_trajectory_box7")
    shinyjs::hide("s_trajectory_box8")
  })
  
  observeEvent(input$single_multiple_sample_trajectory3,{
    shinyjs::show("s_trajectory_box2")
    shinyjs::show("s_trajectory_box3")
    shinyjs::show("s_trajectory_box4")
    shinyjs::show("s_trajectory_box5")
    shinyjs::show("s_trajectory_box6")
    shinyjs::show("s_trajectory_box7")
    shinyjs::hide("s_trajectory_box8")
  })
  
  observeEvent(input$single_multiple_sample_trajectory4,{
    shinyjs::show("s_trajectory_box2")
    shinyjs::show("s_trajectory_box3")
    shinyjs::show("s_trajectory_box4")
    shinyjs::show("s_trajectory_box5")
    shinyjs::show("s_trajectory_box6")
    shinyjs::show("s_trajectory_box7")
    shinyjs::show("s_trajectory_box8")
  })  
  
  
  observe({
    
    if (input$s_trajectory1 == "multiple_sample" & input$s_trajectory2 == "seurat_clusters"){
      output$s_trajectory_10 <- renderUI ({
        clusters <- req(datainput_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "s_trajectory10",
          label = "Select one cluster as root",
          choices = sort(clusters),
          multiple = F,
          options = list(`actions-box` = TRUE))
      })
    }
    else if (input$s_trajectory1 == "multiple_sample" & input$s_trajectory2 == "predicted"){
      output$s_trajectory_10 <- renderUI ({
        clusters <- req(datainput_multiple_celltype_level()[[3]])
        shinyWidgets::pickerInput(
          inputId = "s_trajectory10",
          label = "Select one cluster as root",
          choices = sort(clusters),
          multiple = F,
          options = list(`actions-box` = TRUE))
      })
    }  
    else if (input$s_trajectory1 == "multiple_sample_subclustering" & input$s_trajectory2 == "seurat_clusters"){
      output$s_trajectory_10 <- renderUI ({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "s_trajectory10",
          label = "Select one cluster as root",
          choices = sort(clusters),
          multiple = F,
          options = list(`actions-box` = TRUE))
      })
    }
    else if (input$s_trajectory1 == "multiple_sample_subclustering" & input$s_trajectory2 == "predicted"){
      output$s_trajectory_10 <- renderUI ({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[3]])
        shinyWidgets::pickerInput(
          inputId = "s_trajectory10",
          label = "Select one cluster as root",
          choices = sort(clusters),
          multiple = F,
          options = list(`actions-box` = TRUE))
      })
    }
  })
  
  
  
  datainput_single_multiple_sample_trajectory1_level <- eventReactive(input$single_multiple_sample_trajectory1,{
    run_logged_analysis(
      section = "Trajectory",
      action = "Build trajectory object",
      params = capture_run_inputs(c("s_trajectory", "multiple_sample_normalization_method")),
      expr = {
        source_app_script("scripts/trajectory1.R")
        datainput_single_multiple_sample_trajectory1(index_multiple_sample_input = datainput_multiple_celltype_level()[[1]], index_subclustering_multiple_sample_input = datainput_subclustering_multiple_celltype_level()[[1]], index_multiple_sample_input2 = datainput_multiple_celltype_level()[[4]], index_subclustering_multiple_sample_input2 = datainput_subclustering_multiple_celltype_level()[[4]], index_s_trajectory1 = input$s_trajectory1, index_s_trajectory2 = input$s_trajectory2, index_s_trajectory3 = input$s_trajectory3, index_s_trajectory4 = input$s_trajectory4, index_s_trajectory5 = input$s_trajectory5, index_s_trajectory6 = input$s_trajectory6, index_s_trajectory7 = input$s_trajectory7, index_s_trajectory8 = input$s_trajectory8, index_multiple_sample_normalization_method = input$multiple_sample_normalization_method)
      }
    )
  })  
  
  
  output$s_trajectory1_plot<-renderPlot({
    datainput_single_multiple_sample_trajectory1_level()[3]
  })
  
  observeEvent(input$download_s_trajectory1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_trajectory1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_trajectory1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_trajectory1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_trajectory1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Trajectory_Plot",  input$s_trajectory1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_trajectory1_level()[[3]], width = input$s_trajectory1_plot_width, height = input$s_trajectory1_plot_height, dpi = input$s_trajectory1_plot_dpi, units = "in")
    }
  )
  
  
  ########submenu2#########
  datainput_single_multiple_sample_trajectory2_level <- eventReactive(input$single_multiple_sample_trajectory2,{
    run_logged_analysis(
      section = "Trajectory",
      action = "Order cells along pseudotime",
      params = capture_run_inputs(c("s_trajectory")),
      expr = {
        source_app_script("scripts/trajectory2.R")
        datainput_single_multiple_sample_trajectory2(index_trajectory2_input1 = datainput_single_multiple_sample_trajectory1_level()[[1]], index_trajectory2_input2 = datainput_single_multiple_sample_trajectory1_level()[[2]],  index_trajectory2_multiple_sample_input2 = datainput_multiple_celltype_level()[[4]], index_trajectory2_subclustering_multiple_sample_input2 = datainput_subclustering_multiple_celltype_level()[[4]], index_s_trajectory1 = input$s_trajectory1, index_s_trajectory2 = input$s_trajectory2, index_s_trajectory10 = input$s_trajectory10, index_s_trajectory11 = input$s_trajectory11, index_s_trajectory12 = input$s_trajectory12, index_s_trajectory13 = input$s_trajectory13, index_s_trajectory14 = input$s_trajectory14)
      }
    )
  })  
  
  output$s_trajectory2_plot<-renderPlot({
    datainput_single_multiple_sample_trajectory2_level()[3]
  })
  
  observeEvent(input$download_s_trajectory2_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_trajectory2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_trajectory2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_trajectory2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_trajectory2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("cells_in_Pseudotime",  input$s_trajectory2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_trajectory2_level()[[3]], width = input$s_trajectory2_plot_width, height = input$s_trajectory2_plot_height, dpi = input$s_trajectory2_plot_dpi, units = "in")
    }
  )
  
  output$s_trajectory3_plot<-renderPlot({
    datainput_single_multiple_sample_trajectory2_level()[4]
  })
  
  observeEvent(input$download_s_trajectory3_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_trajectory3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 16, width = "300px"),
      numericInput("s_trajectory3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_trajectory3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_trajectory3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_trajectory3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Cells_ordered_by_Monocle3_Pseudotime",  input$s_trajectory3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_trajectory2_level()[[4]], width = input$s_trajectory3_plot_width, height = input$s_trajectory3_plot_height, dpi = input$s_trajectory3_plot_dpi, units = "in")
    }
  )
  
  ########submenu3#########
  datainput_single_multiple_sample_trajectory3_level <- eventReactive(input$single_multiple_sample_trajectory3,{
    run_logged_analysis(
      section = "Trajectory",
      action = "Pseudotime feature analysis",
      params = capture_run_inputs(c("s_trajectory")),
      expr = {
        source_app_script("scripts/trajectory3.R")
        datainput_single_multiple_sample_trajectory3(index_trajectory3_input1 = datainput_single_multiple_sample_trajectory2_level()[[1]], index_trajectory3_input2 = datainput_single_multiple_sample_trajectory2_level()[[2]], index_s_trajectory15 = input$s_trajectory15, index_s_trajectory16 = input$s_trajectory16)
      }
    )
  })  
  
  output$s_trajectory4_plot<-renderPlot({
    datainput_single_multiple_sample_trajectory3_level()[3]
  })
  
  observeEvent(input$download_s_trajectory4_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_trajectory4_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory4_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory4_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_trajectory4_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_trajectory4_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  output$s_trajectory4_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("FeaturePlot_with_Pseudotime",  input$s_trajectory4_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_trajectory3_level()[[3]], width = input$s_trajectory4_plot_width, height = input$s_trajectory4_plot_height, dpi = input$s_trajectory4_plot_dpi, units = "in")
    }
  )
  
  output$s_trajectory7_plot<-renderPlot({
    datainput_single_multiple_sample_trajectory3_level()[5]
  })
  
  observeEvent(input$download_s_trajectory7_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_trajectory7_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory7_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory7_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_trajectory7_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_trajectory7_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  
  output$s_trajectory7_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Spatial_Plot_with_Pseudotime",  input$s_trajectory7_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_trajectory3_level()[[5]], width = input$s_trajectory7_plot_width, height = input$s_trajectory7_plot_height, dpi = input$s_trajectory7_plot_dpi, units = "in")
    }
  )
  
  output$s_trajectory1_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_trajectory3_level()[[4]]),
                                                             options = list(
                                                               scrollX = TRUE,
                                                               pageLength = 10,
                                                               dom = "Blfrtip"
                                                               #bFilter=0
                                                             ),rownames= FALSE, selection = "none"))
  
  output$download_s_trajectory1_table <- downloadHandler(
    filename = function() { 
      paste("Genes_that_change_as_a_function_of_pseudotime", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_trajectory3_level()[[4]], file)
    }
  )
  
  ########submenu4#########
  observe({
    if(input$s_trajectory17 == "gene_name_list"){
      shinyjs::show("s_trajectory18")
    }
    else {
      shinyjs::hide("s_trajectory18")
    }
  }) 
  
  
  datainput_single_multiple_sample_trajectory4_level <- eventReactive(input$single_multiple_sample_trajectory4,{
    run_logged_analysis(
      section = "Trajectory",
      action = "Pseudotime heatmap and modules",
      params = capture_run_inputs(c("s_trajectory", "multiple_sample_normalization_method")),
      expr = {
        source_app_script("scripts/trajectory4.R")
        datainput_single_multiple_sample_trajectory4(index_trajectory4_input1 = datainput_single_multiple_sample_trajectory3_level()[[1]], index_trajectory4_input2 = datainput_single_multiple_sample_trajectory3_level()[[2]], index_trajectory4_input3 = datainput_single_multiple_sample_trajectory3_level()[[4]], index_s_trajectory17 = input$s_trajectory17, index_s_trajectory18 = input$s_trajectory18, index_multiple_sample_normalization_method = input$multiple_sample_normalization_method)
      }
    )
  })  
  
  output$s_trajectory5_plot<-renderPlot({
    datainput_single_multiple_sample_trajectory4_level()[3]
  })
  
  observeEvent(input$download_s_trajectory5_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_trajectory5_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory5_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory5_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_trajectory5_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_trajectory5_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_trajectory5_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("FeaturePlot_with_Pseudotime_for_selected_genes",  input$s_trajectory5_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_trajectory4_level()[[3]], width = input$s_trajectory5_plot_width, height = input$s_trajectory5_plot_height, dpi = input$s_trajectory5_plot_dpi, units = "in")
    }
  )
 
  
  output$s_trajectory6_plot<-renderPlot({
    datainput_single_multiple_sample_trajectory4_level()[4]
  })
  
  observeEvent(input$download_s_trajectory6_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_trajectory6_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory6_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_trajectory6_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_trajectory6_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_trajectory6_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_trajectory6_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("FeaturePlot_with_Pseudotime_for_selected_genes_with_spatial_images",  input$s_trajectory6_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_trajectory4_level()[[4]], width = input$s_trajectory6_plot_width, height = input$s_trajectory6_plot_height, dpi = input$s_trajectory6_plot_dpi, units = "in")
    }
  ) 
  ######################################################Menu9#####################################################################   
  ####################################################hdwgcna####################################################################  
  ###################hidebox#################
  shinyjs::hide("s_hdwgcna_box1")
  shinyjs::hide("s_hdwgcna_box2")
  shinyjs::hide("s_hdwgcna_box3")
  shinyjs::hide("s_hdwgcna_box4")
  shinyjs::hide("s_hdwgcna_box5")
  shinyjs::hide("s_hdwgcna_box6")
  shinyjs::hide("s_hdwgcna_box7")
  shinyjs::hide("s_hdwgcna_box8")
  shinyjs::hide("s_hdwgcna_box9")
  shinyjs::hide("s_hdwgcna_box10")
  
  observeEvent(input$multiple_sample_celltype, {
    if(input$m_marker1 == 1){
      shinyjs::hide("s_hdwgcna_box0")
      shinyjs::show("s_hdwgcna_box1")
      shinyjs::show("s_hdwgcna_box2")
      shinyjs::show("s_hdwgcna_box3")
      shinyjs::show("s_hdwgcna_box4")
      shinyjs::show("s_hdwgcna_box5")
    }
    else{
      shinyjs::show("s_hdwgcna_box0")
      shinyjs::hide("s_hdwgcna_box1") 
      shinyjs::hide("s_hdwgcna_box2")
      shinyjs::hide("s_hdwgcna_box3")
      shinyjs::hide("s_hdwgcna_box4")
      shinyjs::hide("s_hdwgcna_box5")
    }
  })
  observeEvent(input$subclustering_multiple_sample_celltype, {
    if(input$m_subclustering_marker1 == 1){
      shinyjs::hide("s_hdwgcna_box0")
      shinyjs::show("s_hdwgcna_box1")
      shinyjs::show("s_hdwgcna_box2")
      shinyjs::show("s_hdwgcna_box3")
      shinyjs::show("s_hdwgcna_box4")
      shinyjs::show("s_hdwgcna_box5")
    }
    else{
      shinyjs::show("s_hdwgcna_box0")
      shinyjs::hide("s_hdwgcna_box1") 
      shinyjs::hide("s_hdwgcna_box2")
      shinyjs::hide("s_hdwgcna_box3")
      shinyjs::hide("s_hdwgcna_box4")
      shinyjs::hide("s_hdwgcna_box5")
    }
  })
  
  observeEvent(input$single_multiple_sample_hdwgcna,{
    shinyjs::show("s_hdwgcna_box6")
    shinyjs::show("s_hdwgcna_box7")
    shinyjs::show("s_hdwgcna_box8")
    shinyjs::show("s_hdwgcna_box9")
    shinyjs::show("s_hdwgcna_box10")
  })
  
  observe({
    if (input$s_hdwgcna1 == "multiple_sample" & input$s_hdwgcna2 == "seurat_clusters"){
      output$s_hdwgcna_3 <- renderUI ({
        clusters <- req(datainput_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "s_hdwgcna3",
          label = "Select one cluster for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = F,
          options = list(`actions-box` = TRUE))
      })
    }
    else if (input$s_hdwgcna1 == "multiple_sample" & input$s_hdwgcna2 == "predicted"){
      output$s_hdwgcna_3 <- renderUI ({
        clusters <- req(datainput_multiple_celltype_level()[[3]])
        shinyWidgets::pickerInput(
          inputId = "s_hdwgcna3",
          label = "Select one cluster for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = F,
          options = list(`actions-box` = TRUE))
      })
    }  
    else if (input$s_hdwgcna1 == "multiple_sample_subclustering" & input$s_hdwgcna2 == "seurat_clusters"){
      output$s_hdwgcna_3 <- renderUI ({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[2]])
        shinyWidgets::pickerInput(
          inputId = "s_hdwgcna3",
          label = "Select one cluster for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = F,
          options = list(`actions-box` = TRUE))
      })
    }
    else if (input$s_hdwgcna1 == "multiple_sample_subclustering" & input$s_hdwgcna2 == "predicted"){
      output$s_hdwgcna_3 <- renderUI ({
        clusters <- req(datainput_subclustering_multiple_celltype_level()[[3]])
        shinyWidgets::pickerInput(
          inputId = "s_hdwgcna3",
          label = "Select one cluster for analsysis",
          choices = sort(clusters),
          selected = sort(clusters)[1],
          multiple = F,
          options = list(`actions-box` = TRUE))
      })
    }
  })
  
  observeEvent(input$single_multiple_sample_hdwgcna, {
    files_to_delete <- c(
      "www/combined_output.pdf",
      "www/combined_output.png",
      "www/PlotDendrogram.pdf",
      "www/PlotDendrogram.png",
      "www/ModuleUMAPPlot.pdf",
      "www/ModuleUMAPPlot.png",
      "www/PlotModuleCorrelogram.pdf",
      "www/PlotModuleCorrelogram.png"
    )
    
    for (file in files_to_delete) {
      full_path <- vstdavis_app_file(file)
      if (file.exists(full_path)) {
        unlink(full_path)
        cat("Deleted:", full_path, "\n")
      } else {
        cat("File not found, skipping:", full_path, "\n")
      }
    }
  })
  
  datainput_single_multiple_sample_hdwgcna_level <- eventReactive(input$single_multiple_sample_hdwgcna,{
    run_logged_analysis(
      section = "Co-expression",
      action = "hdWGCNA",
      params = capture_run_inputs(c("s_hdwgcna", "multiple_sample_normalization_method", "subclustering_multiple_sample_normalization_method")),
      expr = {
        source_app_script("scripts/hdwgcna.R")
       datainput_single_multiple_sample_hdwgcna(index_multiple_sample_hdwgcna_input = datainput_multiple_celltype_level()[[1]], index_subclustering_multiple_sample_hdwgcna_input = datainput_subclustering_multiple_celltype_level()[[1]], index_multiple_sample_hdwgcna_input2 = datainput_multiple_celltype_level()[[4]], index_subclustering_multiple_sample_hdwgcna_input2 = datainput_subclustering_multiple_celltype_level()[[4]], index_multiple_sample_normalization_method_hdwgcna = input$multiple_sample_normalization_method, index_subclustering_multiple_sample_normalization_method_hdwgcna = input$subclustering_multiple_sample_normalization_method, index_s_hdwgcna1 = input$s_hdwgcna1, index_s_hdwgcna2 = input$s_hdwgcna2, index_s_hdwgcna3 = input$s_hdwgcna3, index_s_hdwgcna4 = input$s_hdwgcna4, index_s_hdwgcna5 = input$s_hdwgcna5, index_s_hdwgcna6 = input$s_hdwgcna6, index_s_hdwgcna7 = input$s_hdwgcna7, index_s_hdwgcna8 = input$s_hdwgcna8, index_s_hdwgcna9 = input$s_hdwgcna9, index_s_hdwgcna10 = input$s_hdwgcna10, index_s_hdwgcna11 = input$s_hdwgcna11, index_s_hdwgcna12 = input$s_hdwgcna12, index_s_hdwgcna13 = input$s_hdwgcna13, index_s_hdwgcna14 = input$s_hdwgcna14)
      }
    )
  })  
  
  output$s_hdwgcna1_plot<-renderPlot({
    draw_hdwgcna_shiny_plot(datainput_single_multiple_sample_hdwgcna_level()[[1]])
  })
  observeEvent(input$download_s_hdwgcna1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_hdwgcna1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_hdwgcna1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_hdwgcna1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_hdwgcna1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("umap_plot", input$s_hdwgcna1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_hdwgcna_level()[[1]], width = input$s_hdwgcna1_plot_width, height = input$s_hdwgcna1_plot_height, dpi = input$s_hdwgcna1_plot_dpi, units = "in")
    }
  )
  
  output$s_hdwgcna2_plot<-renderPlot({
    draw_hdwgcna_shiny_plot(datainput_single_multiple_sample_hdwgcna_level()[[2]])
  })
  observeEvent(input$download_s_hdwgcna2_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_hdwgcna2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_hdwgcna2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_hdwgcna2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_hdwgcna2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Soft_power_threshold_plots", input$s_hdwgcna2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_hdwgcna_level()[[2]], width = input$s_hdwgcna2_plot_width, height = input$s_hdwgcna2_plot_height, dpi = input$s_hdwgcna2_plot_dpi, units = "in")
    }
  )
  
  # output$text_level_test<- renderText({
  #   paste(datainput_single_multiple_sample_hdwgcna_level()[6])
  # })
  
  output$s_hdwgcna3_plot <- shiny::renderUI({
    hdwgcna_result <- datainput_single_multiple_sample_hdwgcna_level()
    render_pdf_preview(
      hdwgcna_result[["dendrogram_file"]],
      hdwgcna_result[[6]],
      title = "Co-expression network plot"
    )
  })
  
  
  output$s_hdwgcna4_plot<-renderPlot({
    draw_hdwgcna_shiny_plot(datainput_single_multiple_sample_hdwgcna_level()[[3]])
  })
  observeEvent(input$download_s_hdwgcna4_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_hdwgcna4_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna4_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna4_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_hdwgcna4_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_hdwgcna4_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_hdwgcna4_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Module_ranked_by_eigengene_based_connectivity_kME", input$s_hdwgcna4_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_hdwgcna_level()[[3]], width = input$s_hdwgcna4_plot_width, height = input$s_hdwgcna4_plot_height, dpi = input$s_hdwgcna4_plot_dpi, units = "in")
    }
  )
  
  output$s_hdwgcna5_plot<-renderPlot({
    draw_hdwgcna_shiny_plot(datainput_single_multiple_sample_hdwgcna_level()[[4]])
  })
  observeEvent(input$download_s_hdwgcna5_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_hdwgcna5_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna5_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna5_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_hdwgcna5_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_hdwgcna5_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_hdwgcna5_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Module_feature_plots", input$s_hdwgcna5_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_hdwgcna_level()[[4]], width = input$s_hdwgcna5_plot_width, height = input$s_hdwgcna5_plot_height, dpi = input$s_hdwgcna5_plot_dpi, units = "in")
    }
  )
  
  
  output$s_hdwgcna10_plot<-renderPlot({
    draw_hdwgcna_shiny_plot(datainput_single_multiple_sample_hdwgcna_level()[[11]])
  })
  observeEvent(input$download_s_hdwgcna10_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_hdwgcna10_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna10_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna10_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_hdwgcna10_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_hdwgcna10_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_hdwgcna10_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Module_feature_plots_with_spatial_image", input$s_hdwgcna10_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_hdwgcna_level()[[11]], width = input$s_hdwgcna10_plot_width, height = input$s_hdwgcna10_plot_height, dpi = input$s_hdwgcna10_plot_dpi, units = "in")
    }
  )
  
  output$s_hdwgcna6_plot <- renderUI({
    hdwgcna_result <- datainput_single_multiple_sample_hdwgcna_level()
    render_pdf_preview(
      hdwgcna_result[["correlogram_file"]],
      hdwgcna_result[[6]],
      title = "Module correlogram plot"
    )
  })
  
  output$s_hdwgcna7_plot<-renderPlot({
    draw_hdwgcna_shiny_plot(datainput_single_multiple_sample_hdwgcna_level()[[5]])
  })
  observeEvent(input$download_s_hdwgcna7_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_hdwgcna7_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna7_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_hdwgcna7_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_hdwgcna7_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_hdwgcna7_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_hdwgcna7_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Module_with_Seurats_dot_plot", input$s_hdwgcna7_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_hdwgcna_level()[[5]], width = input$s_hdwgcna7_plot_width, height = input$s_hdwgcna7_plot_height, dpi = input$s_hdwgcna7_plot_dpi, units = "in")
    }
  )
  
  
  output$s_hdwgcna7_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Module_with_Seurats_dotPlot", input$s_hdwgcna7_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_hdwgcna_level()[[5]], width = input$s_hdwgcna7_plot_width, height = input$s_hdwgcna7_plot_height, dpi = input$s_hdwgcna7_plot_dpi, units = "in")
    }
  )
  
  output$s_hdwgcna8_plot <- renderUI({
    hdwgcna_result <- datainput_single_multiple_sample_hdwgcna_level()
    render_pdf_preview(
      hdwgcna_result[["module_networks_file"]],
      hdwgcna_result[[6]],
      title = "Individual module network plots"
    )
  })
  
  
  output$s_hdwgcna9_plot <- renderUI({
    hdwgcna_result <- datainput_single_multiple_sample_hdwgcna_level()
    render_pdf_preview(
      hdwgcna_result[["module_umap_file"]],
      hdwgcna_result[[6]],
      title = "UMAP plot for co-expression networks"
    )
  })
  
  
  output$s_hdwgcna1_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_hdwgcna_level()[[7]]),
                                                          options = list(
                                                            scrollX = TRUE,
                                                            pageLength = 10,
                                                            dom = "Blfrtip"
                                                            #bFilter=0
                                                          ),rownames= FALSE, selection = "none"))
  
  output$download_s_hdwgcna1_table <- downloadHandler(
    filename = function() { 
      paste("Soft_power_threshold_table", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_hdwgcna_level()[[7]], file)
    }
  )
  
  output$s_hdwgcna2_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_hdwgcna_level()[[8]]),
                                                          options = list(
                                                            scrollX = TRUE,
                                                            pageLength = 10,
                                                            dom = "Blfrtip"
                                                            #bFilter=0
                                                          ),rownames= FALSE, selection = "none"))
  
  output$download_s_hdwgcna2_table <- downloadHandler(
    filename = function() { 
      paste("Module_assignment_table", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_hdwgcna_level()[[8]], file)
    }
  )
  
  output$s_hdwgcna3_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_hdwgcna_level()[[9]]),
                                                          options = list(
                                                            scrollX = TRUE,
                                                            pageLength = 10,
                                                            dom = "Blfrtip"
                                                            #bFilter=0
                                                          ),rownames= FALSE, selection = "none"))
  
  output$download_s_hdwgcna3_table <- downloadHandler(
    filename = function() { 
      paste("Top_N_hub_genes", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_hdwgcna_level()[[9]], file)
    }
  )
  
  ###################save object file###################
  output$s_hdwgcna <- create_object_download_handler(
    section = "Co-expression Network Analysis",
    action = "Download Object File",
    filename_text = "Co_expression_network_analysis.RDS",
    object_expr = datainput_single_multiple_sample_hdwgcna_level()[[10]]
  )
  
  ######################################################Menu9.2#####################################################################   
  ####################################################TFs####################################################################
  shinyjs::hide("s_tfrn_box5")
  shinyjs::hide("s_tfrn_box6")
  shinyjs::hide("s_tfrn_box7")
  
  observeEvent(input$single_multiple_sample_tfrn1, {
    shinyjs::show("s_tfrn_box5")
    shinyjs::show("s_tfrn_box6")
    shinyjs::hide("s_tfrn_box7")
  })
  observeEvent(input$single_multiple_sample_tfrn2, {
    shinyjs::show("s_tfrn_box7")
  })
  
  datainput_single_multiple_sample_tfrn1_level <- eventReactive(input$single_multiple_sample_tfrn1,{
    run_logged_analysis(
      section = "Transcription Factors",
      action = "TF regulatory network",
      params = capture_run_inputs(c("s_tfrn")),
      expr = {
        source_app_script("scripts/tfrn1.R")
        datainput_single_multiple_sample_tfrn1(index_multiple_sample_tfrn1_input = datainput_single_multiple_sample_hdwgcna_level()[[10]], index_s_tfrn1 = input$s_tfrn1, index_s_tfrn2 = input$s_tfrn2, index_s_tfrn3 = input$s_tfrn3, index_s_tfrn4 = input$s_tfrn4, index_s_tfrn5 = input$s_tfrn5, index_s_tfrn6 = input$s_tfrn6, index_s_tfrn7 = input$s_tfrn7, index_s_tfrn8 = input$s_tfrn8)
      }
    )
  })  
  
  output$s_tfrn1_plot<-renderPlot({
    datainput_single_multiple_sample_tfrn1_level()[1]
  })
  observeEvent(input$download_s_tfrn1_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_tfrn1_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn1_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn1_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_tfrn1_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_tfrn1_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_tfrn1_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Module_regulatory_network_plot_Positive", input$s_tfrn1_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_tfrn1_level()[[1]], width = input$s_tfrn1_plot_width, height = input$s_tfrn1_plot_height, dpi = input$s_tfrn1_plot_dpi, units = "in")
    }
  )
  
  output$s_tfrn2_plot<-renderPlot({
    datainput_single_multiple_sample_tfrn1_level()[2]
  })
  observeEvent(input$download_s_tfrn2_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_tfrn2_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn2_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn2_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_tfrn2_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_tfrn2_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_tfrn2_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Module_regulatory_network_plot_Negative", input$s_tfrn2_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_tfrn1_level()[[2]], width = input$s_tfrn2_plot_width, height = input$s_tfrn2_plot_height, dpi = input$s_tfrn2_plot_dpi, units = "in")
    }
  )
  
  output$s_tfrn3_plot<-renderPlot({
    datainput_single_multiple_sample_tfrn1_level()[3]
  })
  observeEvent(input$download_s_tfrn3_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_tfrn3_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn3_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn3_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_tfrn3_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_tfrn3_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_tfrn3_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Module_regulatory_network_plot_both", input$s_tfrn3_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_tfrn1_level()[[3]], width = input$s_tfrn3_plot_width, height = input$s_tfrn3_plot_height, dpi = input$s_tfrn3_plot_dpi, units = "in")
    }
  )
  
  output$s_tfrn4_plot<-renderPlot({
    datainput_single_multiple_sample_tfrn1_level()[4]
  })
  observeEvent(input$download_s_tfrn4_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_tfrn4_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn4_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn4_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_tfrn4_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_tfrn4_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_tfrn4_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Module_regulatory_network_plot_Module_UMAP", input$s_tfrn4_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_tfrn1_level()[[4]], width = input$s_tfrn4_plot_width, height = input$s_tfrn4_plot_height, dpi = input$s_tfrn4_plot_dpi, units = "in")
    }
  )
  
  output$s_tfrn1_table<- renderDataTable(DT::datatable((datainput_single_multiple_sample_tfrn1_level()[[5]]),
                                                       options = list(
                                                         scrollX = TRUE,
                                                         pageLength = 10,
                                                         dom = "Blfrtip"
                                                         #bFilter=0
                                                       ),rownames= FALSE, selection = "none"))
  
  output$download_s_tfrn1_table <- downloadHandler(
    filename = function() { 
      paste("TF_network_table", '.csv', sep='') },
    content = function(file){
      write.csv(datainput_single_multiple_sample_tfrn1_level()[[5]], file)
    }
  )   
  
  ################################################################################################################################  
  ######################################################submenu################################################################  
  observe({
    output$s_tfrn_11 <- renderUI ({
      clusters <- req(datainput_single_multiple_sample_tfrn1_level()[[6]])
      shinyWidgets::pickerInput(
        inputId = "s_tfrn11",
        label = "Select one TFs",
        choices = clusters,
        multiple = F,
        options = list(`actions-box` = TRUE))
    })
  })
  
  
  datainput_single_multiple_sample_tfrn2_level <- eventReactive(input$single_multiple_sample_tfrn2,{
    run_logged_analysis(
      section = "Transcription Factors",
      action = "TF target exploration",
      params = capture_run_inputs(c("s_tfrn")),
      expr = {
        source_app_script("scripts/tfrn2.R")
        datainput_single_multiple_sample_tfrn2(index_multiple_sample_tfrn2_input = datainput_single_multiple_sample_tfrn1_level()[[7]], index_multiple_sample_tfrn2_input2 = datainput_single_multiple_sample_tfrn1_level()[[8]],index_multiple_sample_tfrn2_input3 = datainput_single_multiple_sample_tfrn1_level()[[9]],index_s_tfrn11 = input$s_tfrn11, index_s_tfrn12 = input$s_tfrn12, index_s_tfrn13 = input$s_tfrn13, index_s_tfrn14 = input$s_tfrn14)
      }
    )
  })  
  
  output$s_tfrn11_plot<-renderPlot({
    datainput_single_multiple_sample_tfrn2_level()[1]
  })
  observeEvent(input$download_s_tfrn11_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_tfrn11_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn11_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn11_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_tfrn11_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_tfrn11_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_tfrn11_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Feature_plot_of_selected_TF", input$s_tfrn11_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_tfrn2_level()[[1]], width = input$s_tfrn11_plot_width, height = input$s_tfrn11_plot_height, dpi = input$s_tfrn11_plot_dpi, units = "in")
    }
  )
  
  output$s_tfrn16_plot<-renderPlot({
    datainput_single_multiple_sample_tfrn2_level()[7]
  })
  observeEvent(input$download_s_tfrn16_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_tfrn16_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn16_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn16_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_tfrn16_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_tfrn16_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_tfrn16_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Feature_plot_of_selected_TF_with_spatial_image", input$s_tfrn16_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_tfrn2_level()[[7]], width = input$s_tfrn16_plot_width, height = input$s_tfrn16_plot_height, dpi = input$s_tfrn16_plot_dpi, units = "in")
    }
  )
  
  output$s_tfrn12_plot<-renderPlot({
    datainput_single_multiple_sample_tfrn2_level()[2]
  })
  observeEvent(input$download_s_tfrn12_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_tfrn12_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn12_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn12_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_tfrn12_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_tfrn12_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_tfrn12_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("Top_target_genes_within_TF_regulons", input$s_tfrn12_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_tfrn2_level()[[2]], width = input$s_tfrn12_plot_width, height = input$s_tfrn12_plot_height, dpi = input$s_tfrn12_plot_dpi, units = "in")
    }
  )
  
  output$s_tfrn13_plot<-renderPlot({
    datainput_single_multiple_sample_tfrn2_level()[3]
  })
  observeEvent(input$download_s_tfrn13_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_tfrn13_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn13_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn13_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_tfrn13_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_tfrn13_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_tfrn13_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("TF_network_plot_positive", input$s_tfrn13_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_tfrn2_level()[[3]], width = input$s_tfrn13_plot_width, height = input$s_tfrn13_plot_height, dpi = input$s_tfrn13_plot_dpi, units = "in")
    }
  )
  
  output$s_tfrn14_plot<-renderPlot({
    datainput_single_multiple_sample_tfrn2_level()[4]
  })
  observeEvent(input$download_s_tfrn14_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_tfrn14_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn14_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn14_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_tfrn14_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_tfrn14_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_tfrn14_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("TF_network_plot_negative", input$s_tfrn14_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_tfrn2_level()[[4]], width = input$s_tfrn14_plot_width, height = input$s_tfrn14_plot_height, dpi = input$s_tfrn14_plot_dpi, units = "in")
    }
  )
  
  output$s_tfrn15_plot<-renderPlot({
    datainput_single_multiple_sample_tfrn2_level()[5]
  })
  observeEvent(input$download_s_tfrn15_plot, {
    showModal(modalDialog(
      title = strong("Download plot"),
      numericInput("s_tfrn15_plot_height", label = h5("Figure height (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn15_plot_width", label = h5("Figure width (upto 49 inces)"), value = 8, width = "300px"),
      numericInput("s_tfrn15_plot_dpi", label = h5("Figure resolution (dpi:72 to 300)"), value = 300, width = "300px"),
      selectInput("s_tfrn15_plot_type", label = "Image format", choices = list("JPG" = ".jpg", "TIFF" =".tiff", "PDF" = ".pdf",  "SVG" = ".svg", "BMP" = ".bmp", "EPS" = ".eps", "PS" = ".ps"), selected = ".jpg"),
      downloadBttn("s_tfrn15_plot_downloadoutput", "Download"),
      size = "s",
      easyClose = TRUE,
      #footer = NULL
    ))
  })
  output$s_tfrn15_plot_downloadoutput<- downloadHandler(
    filename = function(){
      paste("TF_network_plot_both", input$s_tfrn15_plot_type, sep="")
    },
    content = function(file){
      ggsave(file,plot = datainput_single_multiple_sample_tfrn2_level()[[5]], width = input$s_tfrn15_plot_width, height = input$s_tfrn15_plot_height, dpi = input$s_tfrn15_plot_dpi, units = "in")
    }
  )
  
  ###################save object file###################
  output$s_tfrn <- create_object_download_handler(
    section = "Transcription Factor Regulatory Network Analysis",
    action = "Download Object File",
    filename_text = "TF_analysis.RDS",
    object_expr = datainput_single_multiple_sample_tfrn2_level()[[6]]
  ) 
  #########################################################################Help text#########################################
  #################                                             Help                                        #################
  #########################################################################Help text#########################################
  
  observeEvent(input$info_btn1, {
    showModal(modalDialog(
      title = "File upload and Stats",
      HTML("
    <ul>
      <li><b>Standard Space Ranger or Visium HD output</b> can be uploaded as one ZIP file per sample.</li>
<li><b>Standard layout:</b> include filtered_feature_bc_matrix.h5 together with the spatial image folder inside the sample ZIP.</li>
<li><b>Visium HD Bin data layout:</b> include the binned_outputs folder together with the spatial folder inside the sample ZIP. Supported bin folders are square_008um, square_016um, and square_002um; choose 8 um, 16 um, or 2 um in the bin-size selector.</li>
<li><b>Fallback behavior:</b> if a sample does not contain Visium HD bins, the app automatically loads the standard spatial structure instead.</li>
<li><b>Upload Space Ranger Matrix Files (mtx, features, barcodes) and spatial image folder</b>  Space Ranger files: matrix.mtx.gz, feature.tsv.gz, barcode.tsv.gz, spatial image folder and zip it to single folder for each samples.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn2, {
    showModal(modalDialog(
      title = "Sample Groups and QC Filtering",
      HTML("
    <ul>
      <li><b>Number of groups</b> (Default: 1 to 6) – Select up to 6 groups.</li>
<li><b>Group 1 Name</b> (Default: Group1) – Type the group name.</li>
<li><b>Group 2 Name</b> (Default: Group2) – Type the group name.</li>
<li><b>Group 3 Name</b> (Default: Group3) – Type the group name.</li>
<li><b>Group 4 Name</b> (Default: Group4) – Type the group name.</li>
<li><b>Group 5 Name</b> (Default: Group5) – Type the group name.</li>
<li><b>Group 6 Name</b> (Default: Group6) – Type the group name.</li>
<li><b>Min gene count per cell</b> (Default: 0) – Filters out cells with fewer than this number of genes expressed. [Recommended: 200 to 500].</li>
<li><b>Max gene count per cell</b> (Default: 10000) – Filters out cells with more than this number of genes expressed. [Recommended: 5000 to 10000].</li>
<li><b>Max mitochondrial %</b> (Default: 5) – Removes cells with excessive mitochondrial gene expression, often indicating low-quality or dying cells. [Recommended: <10%].</li>
<li><b>Min nCount per spot/bin</b> (Default: 0) - Filters out spots or bins with fewer than this number of total counts.</li>
<li><b>Max nCount per spot/bin</b> (Default: 100000) - Filters out spots or bins with unusually high total counts.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn3, {
    showModal(modalDialog(
      title = "Normalization and PCA Analysis",
      HTML("
      
    <ul>
    <li><b>Normalization method</b>,(Default: SCTransform), or LogNormalize. </li>
      <li><b>Assay to use</b> (Default: Auto detect) - choose RNA, Spatial, or SCT. SCT is available when SCTransform is selected.</li>
      <li><b>Scale factor</b> (Default: 10000, Min: 1, Max: 1e6) – Scale factor used in LogNormalize method for total expression normalization.</li>
<li><b>Variable gene method</b> (Default: vst) – Method for selecting variable features: vst (default), mean.var.plot, or dispersion.</li>
<li><b>Number of variable genes</b> (Default: 2000, Min: 100, Max: 10000) – Number of top variable genes to retain for downstream analysis.</li>
<li><b>PCA dimensions</b> (Default: 30, Min: 2, Max: 100) – Number of principal components computed for dimensionality reduction.</li>
<h4>Integration</h4>
<li><b>Integration method</b> (Default: cca) – integration is performed.</li>
<li><b>CCAIntegration</b> (Default: Reduction = cca; Distance = Euclidean) – Canonical correlation analysis for dataset integration.</li>
<li><b>RPCAIntegration</b> (Default: Reduction = rpca; Distance = Euclidean) – Faster, scalable variant of CCA.</li>
<li><b>HarmonyIntegration</b> - merges normalized samples, runs PCA, and corrects sample-level effects using Harmony with orig.ident.</li>
<li><b>None (merge only)</b> â€“ skip anchor-based integration and merge the normalized samples directly before PCA.</li>

    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn4, {
    showModal(modalDialog(
      title = "Clustering",
      HTML("
    <ul>
      <li><b>Clustering resolution</b> (Default: 0.5,  Min: 0.1, Max: 1) – Resolution used for cluster granularity. Higher = more clusters.</li>
<li><b>Clustering algorithm</b> (Default: Louvain) – Graph-based clustering algorithm: Louvain (1), SLM (3), or Leiden (4).</li>
<li><b>UMAP k-nearest-neighbours</b> (Default: 20, Min: 2, Max: 50) – Number of nearest neighbors considered for UMAP.</li>
<li><b>UMAP dims</b> (Default: 30, Min: 2, Max: 100) – Number of PCs used for UMAP dimensionality reduction.</li>
<li><b>UMAP min.dist</b> (Default: 0.3, Min: 0.001, Max: 0.5) – Controls how tightly UMAP clusters points. Smaller = more tightly packed.</li>
<li><b>tSNE dims</b> (Default: 30, Min: 2, Max: 100) – Number of PCs used for t-SNE dimensionality reduction.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn6, {
    showModal(modalDialog(
      title = "Marker Identification",
      HTML("
    <ul>
<li><b>FindAllMarkers</b> (Default: Select all cluster) – Identifies marker genes for each cluster compared to all other cells.</li>
<li><b>FindMarkers</b> (Default: Select one cluster against another cluster) – Finds differentially expressed genes between two specific groups of cells.</li>
<li><b>FindConservedMarkers</b> (Default: Select one cluster to check for conserved in all clusters) – Identifies markers that are conserved across multiple groups (e.g., conditions or batches).</li>
<li><b>min.pct</b> (Default: 0.25, Min: 0.01, Max: 1.0) – Minimum fraction of cells expressing the gene for it to be tested.</li>
<li><b>logfc.threshold</b> (Default: 0.25, Min: 0.01, Max: ∞) – Minimum log fold change required to consider gene differentially expressed.</li>
<li><b>Statistical test</b> (Default: wilcox) – Statistical test used for differentially expressed gene or marker identification.</li>
<li><b>Return only positive markers</b> (Default: Yes) – Whether to return only genes upregulated in the target group.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn7, {
    showModal(modalDialog(
      title = "Cell Type Prediction",
      HTML("
    <ul>
      <li><b>Cell type method</b> (Default: SingleR),Other methods are  (SingleR, GPTCelltype, Use Own Labels) – Methods for cell type prediction.</li>

<li><b>Reference tissue for SingleR</b> (Default: hpca, blueprint_encode, mouse_rnaseq, immgen, dice, novershtern_hematopoietic, monaco_immune) – Reference data sources for SingleR annotation.</li>
<li><b>DE method for SingleR</b> (Default: classic) – SingleR Differential expression method used for prediction scoring. (classi, wilcox, t test).</li>
<li><b>Reference data for ScType</b> (Default: Immune system) – Selected cell type reference for matching.</li>
<li><b>Top genes for prediction for GPTCelltype</b> (Default: 10) – Number of top genes used for GPTCelltype or other predictions.</li>
<li><b>Modelfor GPTCelltype</b> (Default: gpt-5, gpt-5-mini, gpt-5-nano, gpt-4, gpt-4o, gpt-4-turbo, gpt-3.5-turbo, etc.) – OpenAI models available in GPTCelltype. Available via the web platform. To use it locally, users need to update their API key by setting Sys.setenv(OPENAI_API_KEY = 'your_openai_API_key') in the global.R file</li>
<li><b>Use Own Labels</b> Default: Cluster 0 to Cluster N) — This option allows users to manually assign custom names to clusters. Users may enter identical names for two or more clusters if they wish to merge them into a single group.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn8, {
    showModal(modalDialog(
      title = "Cluster-Based Plots",
      HTML("
    <ul>
      <li><b>No. of features to display</b> (Default: 1) – Show up to 1 genes for every cluster or select the list of gene name from the dropdown and type the specific genes which you are interested in eg: KLk2,KLK3,CTSG,MS4A3.</li>
<li><b>Select one or multiple cluster(s) for plotting</b> (Default: Default all clusters) – User can adjust the cluster to plot.</li>
<li><b>Plot type</b> (Default: Dot Plot) – Types of visualizations for gene expression or differentially expressed genes. (Dot Plot, Violin Plot, Ridge Plot, Feature Plot, Volcano Plot).</li>
<li><b>Dim plot labels</b> (Default: No) – Whether to display labels in dimensionality reduction plots.</li>
<li><b>Group.by</b> (Default: Seurat cluster) – Grouping variable for DE or plotting, e.g., Seurat cluster or Predicted or Own label.</li>
<li><b>Split.by</b> (Default: NULL) – Whether to split plots by condition, sample, or not at all.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn9, {
    showModal(modalDialog(
      title = "Condition Based Plots",
      HTML("
    <ul>
      <li><b>Select the Condition1</b> (Default: Group1) – User can select any one condition.</li>
<li><b>Select the Condition2</b> (Default: Group2) – User can select any one condition.</li>
<li><b>min.pct</b> (Default: 0.25, Min: 0.01, Max: 1.0) – Minimum fraction of cells expressing the gene to be tested in marker analysis.</li>
<li><b>logfc.threshold</b> (Default: 0.25, Min: 0.01, Max: ∞) – Log fold change threshold for identifying differentially expressed genes.</li>
<li><b>Statistical test</b> (Default: wilcox) – Test used for differential expression: e.g., wilcox, wilcox_limma, bimod, roc, t, LR, MAST.</li>
<li><b>Positive markers only</b> (Default: Yes) – If Yes, return only genes upre.g.ulated in the target group.</li>
<li><b>group.by</b> (Default: condition) – Metadata variable to group cells during marker analysis. (Condition and samples).</li>
<li><b>Plot type</b> (Default: Spatial Plot) – Types of visualizations for gene expression or differentially expressed genes. (Spatial Plot, Dot Plot, Violin Plot, Ridge Plot, Feature Plot, Volcano Plot.</li>
<li><b>Number of features to display</b> (Default: 3) – Number of genes to visualize per plot or select the list of gene name from the dropdown and type the specific genes which you are interested in eg: KLk2,KLK3,CTSG,MS4A3.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn10, {
    showModal(modalDialog(
      title = "Subclustering",
      HTML("
    <ul>
      <li><b>Cluster Type Selection</b> (Default: Seurat clusters) – Choose source for subclustering (Seurat, predicted, or gene-based selection.</li>
<li><b>Select cluster(s)</b> (Default: Select the cluster default 0) – Generated based on selected cluster type.</li>
<li><b>Genes to include (positive selection)</b> (Default: Eg: FCN1 or FCN1,PSAP) – Enter comma-separated gene symbols for subsetting.</li>
<li><b>Genes to exclude (negative selection)</b> (Default: Eg: FCN1 or FCN1,PSAP) – Enter comma-separated gene symbols to exclude cells.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn11, {
    showModal(modalDialog(
      title = "Normalization and PCA Analysis",
      HTML("<ul>
      <li><b>Normalization method</b>,(Default: SCTransform), or LogNormalize. </li>
      <li><b>Scale factor</b> (Default: 10000, Min: 1, Max: 1e6) – Scale factor used in LogNormalize method for total expression normalization.</li>
<li><b>Variable gene method</b> (Default: vst) – Method for selecting variable features: vst (default), mean.var.plot, or dispersion.</li>
<li><b>Number of variable genes</b> (Default: 1000, Min: 100, Max: 10000) – Number of top variable genes to retain for downstream analysis.</li>
<li><b>PCA dimensions</b> (Default: 30, Min: 2, Max: 100) – Number of principal components computed for dimensionality reduction.</li>

</ul>
"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn12, {
    showModal(modalDialog(
      title = "Clustering",
      HTML("<ul>
  <li><b>Clustering resolution</b> (Default: 0.5, , Min: 0.1, Max: 1) – Resolution used for cluster granularity. Higher = more clusters.</li>
<li><b>Clustering algorithm</b> (Default: Louvain) – Graph-based clustering algorithm: Louvain (1), SLM (3), or Leiden (4).</li>
<li><b>UMAP k-nearest-neighbours</b> (Default: 20, Min: 2, Max: 50) – Number of nearest neighbors considered for UMAP.</li>
<li><b>UMAP dims</b> (Default: 30, Min: 2, Max: 100) – Number of PCs used for UMAP dimensionality reduction.</li>
<li><b>UMAP min.dist</b> (Default: 0.3, Min: 0.001, Max: 0.5) – Controls how tightly UMAP clusters points. Smaller = more tightly packed.</li>
<li><b>tSNE dims</b> (Default: 30, Min: 2, Max: 100) – Number of PCs used for t-SNE dimensionality reduction.</li>

    </ul>"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn13, {
    showModal(modalDialog(
      title = "Markers Identification",
      HTML("<ul>
      <li><b>FindAllMarkers</b> (Default: Select all cluster) – Identifies marker genes for each cluster compared to all other cells.</li>
<li><b>FindMarkers</b> (Default: Select one cluster against another cluster) – Finds differentially expressed genes between two specific groups of cells.</li>
<li><b>FindConservedMarkers</b> (Default: Select one cluster to check for conserved in all clusters) – Identifies markers that are conserved across multiple groups (e.g., conditions or batches).</li>
<li><b>min.pct</b> (Default: 0.25, Min: 0.01, Max: 1.0) – Minimum fraction of cells expressing the gene for it to be tested.</li>
<li><b>logfc.threshold</b> (Default: 0.25, Min: 0.01, Max:  ∞) – Minimum log fold change required to consider gene differentially expressed.</li>
<li><b>Statistical test</b> (Default: wilcox) – Statistical test used for differentially expressed gene or marker identification.</li>
<li><b>Return only positive markers</b> (Default: Yes) – Whether to return only genes upregulated in the target group.</li>
    </ul>"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn14, {
    showModal(modalDialog(
      title = "Cell Type Prediction",
      HTML("<ul>
    <li><b>Cell type method</b> (Default: SingleR),Other methods are  (SingleR, GPTCelltype, Use Own Labels) – Methods for cell type prediction.</li>

<li><b>Reference tissue for SingleR</b> (Default: hpca, blueprint_encode, mouse_rnaseq, immgen, dice, novershtern_hematopoietic, monaco_immune) – Reference data sources for SingleR annotation.</li>
<li><b>DE method for SingleR</b> (Default: classic) – SingleR Differential expression method used for prediction scoring. (classi, wilcox, t test).</li>
<li><b>Reference data for ScType</b> (Default: Immune system) – Selected cell type reference for matching.</li>
<li><b>Top genes for prediction for GPTCelltype</b> (Default: 10) – Number of top genes used for GPTCelltype or other predictions.</li>
<li><b>Modelfor GPTCelltype</b> (Default: gpt-5, gpt-5-mini, gpt-5-nano, gpt-4, gpt-4o, gpt-4-turbo, gpt-3.5-turbo, etc.) – OpenAI models available in GPTCelltype.</li>
<li><b>Use Own Labels</b> Default: Cluster 0 to Cluster N) — This option allows users to manually assign custom names to clusters. Users may enter identical names for two or more clusters if they wish to merge them into a single group.</li>
   
    </ul>"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn15, {
    showModal(modalDialog(
      title = "Cluster-Based Plots",
      HTML("<ul>
      <li><b>No. of features to display</b> (Default: 3) – Show up to 3 genes for every cluster or select the list of gene name from the dropdown and type the specific genes which you are interested in eg: KLk2,KLK3,CTSG,MS4A3.</li>
<li><b>Select one or multiple cluster(s) for plotting</b> (Default: Default all clusters) – User can adjust the cluster to plot.</li>
<li><b>Plot type</b> (Default: Dot Plot) – Types of visualizations for gene expression or differentially expressed genes. (Dot Plot, Violin Plot, Ridge Plot, Feature Plot, Volcano Plot).</li>
<li><b>Dim plot labels</b> (Default: No) – Whether to display labels in dimensionality reduction plots.</li>
<li><b>Group.by</b> (Default: Seurat cluster) – Grouping variable for DE or plotting, e.g., Seurat cluster or Predicted or Own label.</li>
<li><b>Split.by</b> (Default: NULL) – Whether to split plots by condition, sample, or not at all.</li>
    </ul>"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn16, {
    showModal(modalDialog(
      title = "Condition Based Analysis",
      HTML("<ul>
      <li><b>Select the Condition1</b> (Default: Group1) – User can select any one condition.</li>
<li><b>Select the Condition2</b> (Default: Group2) – User can select any one condition.</li>
<li><b>min.pct</b> (Default: 0.25, Min: 0.01, Max: 1.0) – Minimum fraction of cells expressing the gene to be tested in marker analysis.</li>
<li><b>logfc.threshold</b> (Default: 0.25, Min: 0.01, Max:  ∞) – Log fold change threshold for identifying differentially expressed genes.</li>
<li><b>Statistical test</b> (Default: wilcox) – Test used for differential expression: e.g., wilcox, wilcox_limma, bimod, roc, t, LR, MAST.</li>
<li><b>Positive markers only</b> (Default: Yes) – If Yes, return only genes upre.g.ulated in the target group.</li>
<li><b>group.by</b> (Default: condition) – Metadata variable to group cells during marker analysis. (Condition and samples).</li>
<li><b>Plot type</b> (Default: Spatial Plot) – Types of visualizations for gene expression or differentially expressed genes. (Spatial Plot, Dot Plot, Violin Plot, Ridge Plot, Feature Plot, Volcano Plot.</li>
<li><b>Number of features to display</b> (Default: 3) – Number of genes to visualize per plot or select the list of gene name from the dropdown and type the specific genes which you are interested in eg: KLk2,KLK3,CTSG,MS4A3.</li>
    </ul>"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })  
  observeEvent(input$info_btn17, {
    showModal(modalDialog(
      title = "Correlation",
      HTML("
    <ul>
      <li><b>Input data</b> (Default: Output of single or multiple samples) – Select the input from full dataset or subclustering.</li>
<li><b>Celltype method</b> (Default: Seurat clusters) – Select celltype grouping for correlation. (Seurat clusters or predicted).</li>
<li><b>Correlation method</b> (Default: Spearman) – Method to compute correlation between clusters (Pearson, Spearman, Kendall).</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn18, {
    showModal(modalDialog(
      title = "Gene Ontology",
      HTML("
    <ul>
      <li><b>Input data</b> (Default: Output of single or multiple samples) Users can choose to perform analyses on the full dataset, on a subclustered subset, or by providing a custom gene list. In the latter case, users may directly type the list of genes of interest for analysis.</li>
<li><b>Celltype method</b> (Default: Seurat clusters) – Clustering source for gene selection. (Seurat clusters or predicted).</li>
<li><b>Organism</b> (Default: Human) – Organism-specific annotation package. (Human, Mouse, Rat, Pig, Rhesus).</li>
<li><b>Ontology</b> (Default: BP) – GO ontology cate.g.ories: biological process, etc. (BP, MF, CC, ALL).</li>
<li><b>pAdjustMethod</b> (Default: BH) – Method for p-value adjustment. (holm, bonferroni, BH, BY, fdr, none).</li>
<li><b>pvalueCutoff</b> (Default: 0.05, Min: 0, Max: 1) – Significance threshold for raw p-value.</li>
<li><b>qvalueCutoff</b> (Default: 0.2, Min: 0, Max: 1) – Significance threshold for q-value.</li>
<li><b>Minimal gene size</b> (Default: 10, Min: 1, Max: 500) – Minimum number of genes in a cate.g.ory.</li>
<li><b>Maximal gene size</b> (Default: 500, Min: 10, Max: 5000) – Maximum number of genes in a cate.g.ory.</li>
<li><b>Plot type</b> (Default: Dotplot) – Visualization options for enriched GO terms. (dotplot, barplot, cnetplot, upsetplot).</li>
<li><b>Top categories to plot</b> (Default: 10, Min: 1, Max: 50) – Number of cate.g.ories to include in plots.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn19, {
    showModal(modalDialog(
      title = "Pathway Analysis",
      HTML("
    <ul>
      <li><b>Pathway analysis type</b> (Default: KEGG) – Source of pathway database. (KEGG or Reactome).</li>
<li><b>Input data</b> (Default: Output of single or multiple samples) – Select the input from full dataset or subclustering.</li>
<li><b>Select one or multiple cluster(s) for analsysis</b> (Default:0) – Select one or multiple clusters.</li>
<li><b>Celltype method</b> (Default: Seurat clusters) – Clustering source for gene selection. (Seurat clusters or predicted).</li>
<li><b>Organism</b> (Default: Human) – Organism-specific annotation package. (Human, Mouse, Rat).</li>
<li><b>pAdjustMethod</b> (Default: BH) – Adjustment for multiple testing.(holm, bonferroni, BH, BY, fdr, none).</li>
<li><b>pvalueCutoff</b> (Default: 0.05, Min: 0, Max: 1) – Significance threshold for raw p-value.</li>
<li><b>qvalueCutoff</b> (Default: 0.2, Min: 0, Max: 1) – Significance threshold for q-value.</li>
<li><b>Minimal gene size</b> (Default: 10, Min: 1, Max: 500) – Minimum number of genes in pathway.</li>
<li><b>Maximal gene size</b> (Default: 500, Min: 10, Max: 5000) – Maximum number of genes in pathway.</li>
<li><b>Plot type</b> (Default: Dotplot) – Type of plot for pathway enrichment. (dotplot, barplot, cnetplot, upsetplot).</li>
<li><b>Top categories to plot (Pathway)</b> (Default: 10, Min: 1, Max: 50) – Number of enriched pathways shown.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn20, {
    showModal(modalDialog(
      title = "GSEA Analysis",
      HTML("
    <ul>
      <li><b>Input data</b> (Default: Output of single or multiple samples) – Select the input from full dataset or subclustering.</li>
<li><b>Celltype method</b> (Default: Seurat clusters) – Clustering source for gene selection. (Seurat clusters or predicted).</li>
<li><b>Select one or multiple cluster(s) for analsysis</b> (Default:0) – Select one or multiple clusters.</li>
<li><b>Organism</b> (Default: Homo sapiens) – Species-specific gene set database. (Homo sapiens, Mus musculus).</li>
<li><b>MSigDB collection</b> (Default: Curated gene sets (C2) for human or Mouse curated gene sets (M2) for mouse) – Gene set collection from MSigDB. Human collections use H/C1-C9; mouse collections use MH, M1, M2, M3, M5, M7, and M8.</li>
<li><b>ScoreType</b> (Default: std) – Controls whether to score all, positive or negative enrichment. (std, pos, neg.).</li>
<li><b>Minimal gene size</b> (Default: 15, Min: 5, Max: 500) – Minimum genes per gene set.</li>
<li><b>Maximal gene size</b> (Default: 50, Min: 15, Max: 5000) – Maximum genes per gene set.</li>
<li><b>Permutations</b> (Default: 100, Min: 10, Max: 10000) – Number of random permutations to compute significance.</li>
<li><b>Plot type</b> (Default: GSEA plot) – Style of plot for GSEA results. (GSEA plot, plotGseaTable, barplot).</li>
<li><b>Top significant results to plot</b> (Default: 10, Min: 1, Max: 50) – Number of enriched gene sets to plot.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn21, {
    showModal(modalDialog(
      title = "Cell-Cell Communication Analysis (Cell-chat)",
      HTML("
    <ul>
      <li><b>Input data</b> (Default: Output of single or multiple samples) – Source of expression data for CellChat.</li>
<li><b>Celltype method</b> (Default: Seurat clusters) – Cell grouping used in communication analysis. (Seurat clusters or predicted).</li>
<li><b>Organism</b> (Default: PPI.human) – Organism-specific protein-protein interaction database. (PPI.human, PPI.mouse).</li>
<li><b>Min % cells expressed</b> (Default: 0, Min: 0, Max: 100) – Minimum percent of cells expressing ligand/receptor.</li>
<li><b>LogFC threshold</b> (Default: 0, Min: 0, Max: 10) – Minimum log fold change for expression filter.</li>
<li><b>P-value threshold</b> (Default: 0.05, Min: 0.0001, Max: 1) – Significance cutoff for ligand-receptor pairs.</li>
<li><b>Averaging method</b> (Default: triMean) – Method for averaging gene expression per group. (triMean, truncatedMean, thresholdedMean, median).</li>
<li><b>Minimum cell count</b> (Default: 10, Min: 5, Max: 1000) – Minimum number of cells in a group.</li>
<li><b>Pattern k-value</b> (Default: 2, Min: 2, Max: 20) – Number of communication patterns to infer.</li>
<li><b>Show label</b> (Default: Yes) – Display labels on communication plots.</li>
<li><b>Specific Signaling Pathways</b> (Default: The default 1st one is selected) – Display the communication for the selected.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn22, {
    showModal(modalDialog(
      title = "Trajectory & Pseudotime Analysis (Monocle3)",
      HTML("
    <ul>
      <li><b>Input data</b> (Default: Output of single or multiple samples) – Select the input from full dataset or subclustering.</li>
<li><b>Celltype method</b> (Default: Seurat clusters) – Grouping variable for pseudotime.</li>
<li><b>use_partition</b> (Default: No) – Whether to use partitioned cell sets.</li>
<li><b>close_loop</b> (Default: Yes) – Allow trajectory graph to close loops.</li>
<li><b>label_groups_by_cluster</b> (Default: No) – Whether to show cluster labels.</li>
<li><b>label_branch_points</b> (Default: Yes) – Show pseudotime branch points.</li>
<li><b>label_roots</b> (Default: Yes) – Show root cells in trajectory.</li>
<li><b>label_leaves</b> (Default: No) – Show leaf cells in trajectory.</li>
<li><b>Order cell in Pseudotime</b> (Default: Select one cluster as the root) – Displays all clusters or predicted cell type.</li>
<li><b>Gene functional change (neighbor_graph)</b> (Default: principal_graph) or select knn; Graph type for trajectory inference.</li>
<li><b>Top genes to display in feature plot</b> (Default: 5) – Number or list of genes to plot along pseudotime.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn23, {
    showModal(modalDialog(
      title = "Co-expression & Network Analysis (hdWGCNA)",
      HTML("
    <ul>
      <li><b>Input data</b> (Default: Output of single or multiple samples) – Select the processed input source.</li>
<li><b>Celltype method</b> (Default: Seurat clusters) – Seurat cluster or predicted.</li>
<li><b>Select any one cluster</b> (Default: Default 0) – List all the clusters or names.</li>
<li><b>Input data</b> (Default: Output of single or multiple samples) – Select the processed input source.</li>
<li><b>Reduction type</b> (Default: UMAP) – Dimensionality reduction for module visualization. (UMAP or PCA).</li>
<li><b>Select soft-power Network type</b> (Default: signed) – Type of WGCNA correlation network. (signed, unsigned, signed hybrid).</li>
<li><b>Module eigengenes and connectivity Scale model</b> (Default: linear) – Statistical model for eigengene computation. (linear, poisson, negbinom).</li>
<li><b>Harmonized eigengenes</b> (Default: Yes) – Whether to harmonize eigengenes across datasets.</li>
<li><b>Nearest neighbors (k)</b> (Default: 10, Min: 1, Max: 100) – K for building metacells.</li>
<li><b>Minimum cell group size</b> (Default: 10, Min: 5, Max: 100) – Minimum cells in a group to build a metacell.</li>
<li><b>Max shared cells</b> (Default: 15, Min: 1, Max: 100) – Max overlap between metacells.</li>
<li><b>Target metacells</b> (Default: 1000, Min: 50, Max: 5000) – Max number of metacells to construct.</li>
<li><b>Hub genes per module</b> (Default: 5, Min: 1, Max: 50) – Number of top hub genes labeled.</li>
<li><b>Show inter-module edges</b> (Default: No) – Whether to draw edges across modules.</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$info_btn24, {
    showModal(modalDialog(
      title = "Transcription Factor Regulatory Network Analysis (hdWGCNA)",
      HTML("
    <ul>
      <li><b>Organism</b> (Default: Human) – Reference genome annotation. (Human or Mouse).</li>
<li><b>XGBoost max_depth</b> (Default: 1, Min: 1, Max: 10) – Tree depth for motif-based TF prediction.</li>
<li><b>eta</b> (Default: 0.1, Min: 0.01, Max: 1) – Learning rate in XGBoost.</li>
<li><b>alpha</b> (Default: 0.5, Min: 0, Max: 1) – Re.g.ularization parameter.</li>
<li><b>Regulatory score threshold</b> (Default: 0.01, Min: 0, Max: 1) – Minimum score for defining TF-gene edge.</li>
<li><b>Top TFs per gene</b> (Default: 10, Min: 1, Max: 50) – Top regulators retained per gene.</li>
<li><b>Positive regulon threshold</b> (Default: 0.05, Min: 0, Max: 1) – Minimum expression for positive regulons.</li>
<li><b>Negative regulon threshold</b> (Default: -0.05, Min: -1, Max: 0) – Threshold for defining negative regulons.</li>
<li><b>Color network edge by</b> (Default: Cor) – TF network edge attribute. (Cor, Gain).</li>
<li><b>Extend TF network layers</b> (Default: Primary and secondary) – Depth of TF-target extension. (Primary or Primary and secondary or Primary, secondary and tertiary).</li>
    </ul>
    "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })  
    
  
}
