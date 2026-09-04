gptcelltype_with_key <- function(input,
                                 api_key,
                                 tissuename = NULL,
                                 model = "gpt-5.6-terra",
                                 topgenenumber = 10) {
  if (is.null(api_key) || !length(api_key)) {
    api_key <- ""
  }
  api_key <- trimws(as.character(api_key))
  if (!length(api_key) || !nzchar(api_key[[1]])) {
    stop("Paste your OpenAI API key before running GPTCelltype.", call. = FALSE)
  }
  api_key <- api_key[[1]]
  on.exit(api_key <- "", add = TRUE)

  if (is.list(input) && !is.data.frame(input)) {
    markers_by_cluster <- vapply(input, paste, collapse = ",", FUN.VALUE = character(1))
  } else {
    required_columns <- c("avg_log2FC", "gene", "cluster")
    missing_columns <- setdiff(required_columns, colnames(input))
    if (length(missing_columns)) {
      stop(
        "GPTCelltype marker input is missing: ",
        paste(missing_columns, collapse = ", "),
        call. = FALSE
      )
    }

    positive_markers <- input[input$avg_log2FC > 0, , drop = FALSE]
    markers_by_cluster <- tapply(
      positive_markers$gene,
      positive_markers$cluster,
      function(genes) paste(utils::head(genes, topgenenumber), collapse = ",")
    )
  }

  if (!length(markers_by_cluster)) {
    stop("No positive marker genes are available for GPTCelltype prediction.", call. = FALSE)
  }

  tissue_context <- if (is.null(tissuename) || !nzchar(trimws(tissuename))) {
    ""
  } else {
    paste0(trimws(tissuename), " ")
  }

  batch_ids <- split(
    seq_along(markers_by_cluster),
    ceiling(seq_along(markers_by_cluster) / 30)
  )

  results <- lapply(batch_ids, function(ids) {
    prompt <- paste0(
      "Identify the cell type for each row of ", tissue_context,
      "cells using the marker genes below. Return exactly one cell type name per row, ",
      "in the same order. Do not add row numbers, bullets, explanations, or headings. ",
      "A row may represent a mixture of multiple cell types.\n",
      paste0(names(markers_by_cluster)[ids], ": ", markers_by_cluster[ids], collapse = "\n")
    )

    # Pass the credential explicitly; process-wide environment variables can leak
    # credentials between concurrent Shiny sessions.
    completion <- openai::create_chat_completion(
      model = model,
      messages = list(list(role = "user", content = prompt)),
      openai_api_key = api_key
    )

    response_text <- completion$choices[, "message.content"]
    predictions <- trimws(unlist(strsplit(as.character(response_text[[1]]), "\n", fixed = TRUE)))
    predictions <- predictions[nzchar(predictions)]

    if (length(predictions) != length(ids)) {
      stop(
        "The selected GPT model returned ", length(predictions),
        " predictions for ", length(ids),
        " clusters. Please run the analysis again.",
        call. = FALSE
      )
    }

    stats::setNames(gsub(",$", "", predictions), names(markers_by_cluster)[ids])
  })

  unlist(unname(results), use.names = TRUE)
}
