calculate_ranked_choice <- function(dat, round = 1) {
  # Replace any empty entries with NA
  dat$V1[dat$V1 == ""] <- NA
  dat$V2[dat$V2 == ""] <- NA
  dat$V3[dat$V3 == ""] <- NA

  # Drop those from V2 and V3 who didn't even appear in V1 --------------------
  v1_names <- unique(dat$V1)

  drop_idx <- !(dat$V2 %in% v1_names)
  drop_v2_names <- unique(dat$V2[drop_idx])
  dat$V2[drop_idx] <- NA
  
  drop_idx <- !(dat$V3 %in% v1_names)
  drop_v3_names <- unique(dat$V3[drop_idx])
  dat$V3[drop_idx] <- NA

  # Dropping those with zero votes
  drop_names <- c(drop_v2_names, drop_v3_names)
  # Remove NAs
  drop_names <- drop_names[!is.na(drop_names)]
  if (length(drop_names) > 0) {
    cat("Dropping candidate(s) with zero 1st rank votes, but have some 2nd and 3rd-rank votes:\n",
        paste(drop_names, collapse = ", "), "\n\n")
  }

  # Get counts of first-rank votes for each candidate -------------------------
  counts <- as.data.frame(table(dat$V1), stringsAsFactors = FALSE)
  names(counts) <- c("name", "votes")
  # Sort by votes
  counts <- counts[order(counts$votes, decreasing = TRUE), ]

  # Print the table -----------------------------------------------------------
  cat("==== Round", round, "====\n")
  print(counts, row.names = FALSE)

  # If we've reached the end
  if (nrow(counts) <= 2) {
    cat("Winner is ", counts$name[1], ", with ", counts$votes[1], " votes",
        sep = "")
    return(invisible(counts$name[1]))
  }

  # ---------------------------------------------------------------------------
  # Drop those who got the lowest number of first-rank votes
  # (can be more than one)
  lowest_votes <- counts$votes[nrow(counts)]
  lowest_names <- counts$name[counts$votes == lowest_votes]
  cat("Dropping candidate(s) with", lowest_votes, "votes:",
      paste(lowest_names, collapse = ", "), "\n\n")

  # Remove all instances of the lowest vote-getters
  dat$V1[which(dat$V1 %in% lowest_names)] <- NA
  dat$V2[which(dat$V2 %in% lowest_names)] <- NA
  dat$V3[which(dat$V3 %in% lowest_names)] <- NA

  # If any NA's in V1 or V2, shift names over from next column.
  # If a voter's first-rank vote was removed, their second-rank vote is now
  # treated as their first-rank vote.
  na_idx <- is.na(dat$V2)
  dat$V2[na_idx] <- dat$V3[na_idx]
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
