# Header
# Filename:       rbig.R
# Description:    A toolbox for R programmers for working with huge tables and data-frames
# Version History:
# 0.0.1 (05 August 2019)     - Initial Issue
# 1.0.4 (13 October 2020)    - Second Issue WideTable added
# 1.0.5 (13 October 2020)    - wt.R updated
# 1.0.6 (04 November 2020)   - wt.R updated: Method '[.WIDETABLE' changed. Issue fixed.
# 1.0.7 (05 November 2020)   - wt.R updated: Method as.matrix() and as.data.frame() changed.
# 1.0.9 (05 November 2020)   - wt.R updated: Values of each column in meta table are updated for columns in data when a new copy is created 
# 1.1.0 (11 November 2020)   - wt.R updated: Function extract.widetable() added 
# 1.1.1 (12 November 2020)   - wt.R updated: minor issue fixed: n_unique does not become NA in row-subset widetables to avoid errors, but values are unreliable.
# 1.1.2 (15 January 2021)    - io.R updated: parquet tools exported.
# 1.1.7 (17 February 2021)   - wt.R updated: function extract.dataframe() added, '[.WIDETABLE' changed.
# 1.1.8 (05 March 2021)      - io.R updated: function parquet2DataFrame() edited. rbind issue resolved.
# 1.2.1 (25 March 2021)      - wt.R updated: function cor.widetable() added and exported.
# 1.2.7 (22 April 2021)      - wt.R updated: function cor.widetable.multicore() added plus cor.widetable() modified: prints progress info
# 1.3.2 (07 May 2021)        - wt.R updated: function edit_widetable.substitute() added
# 1.3.4 (07 June 2021)       - wt.R updated to version 1.0.7
# Description for Roxygen

#' rbig
#'
#' This package is a tool-box for working with huge tables and data-frames.
#' @author Nicolas Berta
#'
#' @docType package
#' @name rbig
#' 
#' 
#' @include io.R
#' @include wt.R

