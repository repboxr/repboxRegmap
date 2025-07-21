# General plan

- We only use map_reg_run to develop a good mapping. map_inv_reg_run and map_reg_static can at some point later be used for comparision. This means we can use runid as sole map between map_reg_run and the Stata output. No need for code_line and script_num.

- We first generate an rme object with rme_init. It will contain all needed information for later tests.


- If some tests fail, we will later generate a prompt for the AI that describes all problems in order to induce a better version of map_reg_run.

TO DO: 

- Simplif rme_init by focussung only on map_reg_run and ignoring map_inv_reg_run and map_reg_static. Similarly adapt tests in rme_eval.R, e.g. runid will be the only relevant key.

- Add in rme_init. information about which cells are coef / parenthesis pairs. Adapt old functions from rme_reg_cell_hx.R. Also store in rme_init already information which numbers could be mapped with associated regression output.

- Also add already in rme_init information about the cmd_type, of each mapped command which is currently done only in rme_eval (rme_cmd_type function). Best generate a new R script for functions related to cmd_type mapping. For post-regression commands and tests we want to know the associated regression. We use the simple heuristic that it is the regression most closely previously run in the table in the stata_run_cmd parcel. Probably, we can just look at the runid which correspond to the position at which that command was executed in the Stata code.

# Test ideas

Keep tests currently implemented in rme_eval.R. New tests.


*   **1. Unique Summary Statistics:** A single regression can only have one value for any given summary statistic (like R-squared or the number of observations). This is the most reliable and powerful simple check.
    *   **Check:** Identify rows by their labels (e.g., "Observations", "R^2", "Control mean", "Controls"). A proposed mapping for a single regression is incorrect if it includes more than one cell from any of these specific statistic rows. In your example, the "light green" mapping fails this by including cells with different "Control mean" values.

*   **2. Valid Coefficient/Standard Error Structure:** In virtually all tables, coefficients are systematically paired with their standard errors (or t-statistics), which are typically in parentheses.
    *   **Check:** For every cell in a mapping that appears to be a coefficient (a number *not* in parentheses), verify that the cell either **directly below it** OR **directly to its right** is also part of the *same mapping* and contains a number in parentheses. A mapping with many "orphan" coefficients that lack a paired standard error is highly suspect.

*   **3. Spatial Compactness & Connectivity:** The cells of a single regression should be clustered together, not scattered randomly.
    *   **Check (Dimensionality):** A mapping is suspicious if its cells span more than two columns. The two-column case should almost exclusively be for side-by-side coefficients and standard errors.
    *   **Check (Connectivity):** The core results of a regression (coefficients and standard errors) should form a single, connected block of cells (where you can travel from any cell to any other via adjacent mapped cells). A mapping is suspect if this core block is split into multiple, disconnected pieces. Shared statistics (like "Observations") are often separate from this block and can be treated as valid, small, secondary components of the mapping.

*   **4. Non-Overlapping Vertical Blocks:** When multiple regressions are presented in the same column (e.g., in different panels), their primary results should occupy distinct, non-overlapping blocks of rows.
    *   **Check:** For any given column, consider all the regressions mapped to it. For each regression, determine its primary vertical range (min and max row of its coefficients/SEs). The ranges of different regressions within the same column must not overlap. For instance, `Regression A` occupying rows 5-10 and `Regression B` occupying rows 11-16 is a valid stacked structure. `Regression A` in rows 5-10 and `Regression B` in rows 9-14 would be an invalid overlap. (Note: Shared statistics at the bottom of the table are exempt from this check and can be validly mapped to multiple regressions).
