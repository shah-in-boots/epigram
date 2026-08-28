#!/usr/bin/env Rscript

# Parent file is held in the "mark" repo to make it easy to copy over
# Generally for R packages will use this in the "tools" subfolder
# .Rbuildignore important for this


# Sync selected branding assets from ./mark into ./inst/brand
project_root <- normalizePath('.', winslash = '/', mustWork = TRUE)

source_dir <- file.path(project_root, 'mark')
target_dir <- file.path(project_root, 'inst', 'brand')

if (!dir.exists(source_dir)) {
  stop('Could not find ./mark. Make sure the submodule is initialized.', call. = FALSE)
}

if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

# Default files to copy from mark (no arguments taken)
files_to_copy <- c(
  '_brand.yml',
  'colors.scss',
  'mermaid.scss'
)

missing_files <- files_to_copy[!file.exists(file.path(source_dir, files_to_copy))]
if (length(missing_files) > 0) {
  warning(
    paste0(
      'These files were not found in ./mark and were skipped: ',
      paste(missing_files, collapse = ', ')
    ),
    call. = FALSE
  )
}

existing_targets <- list.files(target_dir, all.files = FALSE, full.names = TRUE, no.. = TRUE)
if (length(existing_targets) > 0) {
  unlink(existing_targets, recursive = TRUE, force = TRUE)
}

copy_candidates <- files_to_copy[file.exists(file.path(source_dir, files_to_copy))]
if (length(copy_candidates) == 0) {
  stop('No requested branding files exist in ./mark; nothing to copy.', call. = FALSE)
}

copied <- file.copy(
  from = file.path(source_dir, copy_candidates),
  to = file.path(target_dir, basename(copy_candidates)),
  overwrite = TRUE
)

if (!all(copied)) {
  failed <- copy_candidates[!copied]
  stop(
    paste0('Failed to copy: ', paste(failed, collapse = ', ')),
    call. = FALSE
  )
}

message('Synced branding files to inst/brand/: ', paste(copy_candidates, collapse = ', '))

