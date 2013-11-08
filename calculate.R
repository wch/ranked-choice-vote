wrap <- function(..., sep = "", width = 80) {
  paste(strwrap(paste(..., sep = sep)), collapse = "\n")
}

calculate_ranked_choice <- function(dat, round = 1) {
  # Replace any empty entries with NA
  dat$V1[dat$V1 == ""] <- NA
  dat$V2[dat$V2 == ""] <- NA
  dat$V3[dat$V3 == ""] <- NA

  # Get names of all candidates
  all_names <- unique(c(as.character(dat$V1),
                        as.character(dat$V2),
                        as.character(dat$V3)))
  all_names <- all_names[!is.na(all_names)]
  dat$V1 <- factor(dat$V1, levels = all_names)
  dat$V2 <- factor(dat$V2, levels = all_names)
  dat$V3 <- factor(dat$V3, levels = all_names)
  
  # Get counts of first-rank votes for each candidate -------------------------
  counts <- as.data.frame(table(dat$V1))
  names(counts) <- c("name", "votes")
  # Sort by votes
  counts <- counts[order(counts$votes, decreasing = TRUE), ]

  # Print the table -----------------------------------------------------------
  cat("==== Round", round, "====\n")
  print(counts, row.names = FALSE)

  # If we've reached the end
  if (max(counts$votes) > sum(counts$votes)/2) {
    maxrow <- counts$votes == max(counts$votes)
    cat(wrap(
      "Winner is ", counts$name[maxrow], ", with ",
      round(counts$votes[maxrow] / sum(counts$votes) * 100, 2),
      "% of the votes in the last round.", sep = ""
    ))
    cat("\n")
    return(invisible(counts$name[maxrow]))
  }

  # ---------------------------------------------------------------------------
  # Drop those who got the lowest number of first-rank votes
  # (can be more than one)
  lowest_votes <- min(counts$votes)
  drop_names <- counts$name[counts$votes == lowest_votes]
  cat(wrap("Dropping candidate(s) with ", lowest_votes, " votes:"))
  cat("\n")
  cat(wrap(paste(drop_names, collapse = ", ")))
  cat("\n\n")

  # Remove all instances of the lowest vote-getters
  dat$V1[which(dat$V1 %in% drop_names)] <- NA
  dat$V2[which(dat$V2 %in% drop_names)] <- NA
  dat$V3[which(dat$V3 %in% drop_names)] <- NA

  # If any NA's in V1 or V2, shift names over from next column.
  # If a voter's first-rank vote was removed, their second-rank vote is now
  # treated as their first-rank vote.
  na_idx <- is.na(dat$V2)
  dat$V2[na_idx] <- dat$V3[na_idx]
  dat$V3[na_idx] <- NA
  na_idx <- is.na(dat$V1)
  dat$V1[na_idx] <- dat$V2[na_idx]

  # Recurse
  calculate_ranked_choice(dat, round + 1)
}


# Read in a CSV file, calculate winner, and write to file
process_file <- function(filename, outfile = NULL) {
  if (is.null(outfile)) {
    outfile <- sub("\\.csv$", "-results.txt", filename)
  }

  # Save results to file
  if (outfile != stdout()) {
    sink(outfile)
    on.exit(sink())
  }

  # Read data: Columns are named V1, V2, V3, corresponding to 1st, 2nd, 3rd vote
  data <- suppressWarnings(read.csv(filename, header = FALSE,
    fill = TRUE, stringsAsFactors = FALSE))

  calculate_ranked_choice(data)
}

process_file("SampleData.csv")
process_file("SampleDataLarger.csv")
