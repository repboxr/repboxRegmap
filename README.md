# repboxRegmap

The repbox toolkit uses AI to generate mappings between regression tables in articles and output of Stata do files.

There are two key challenges:

- AI sometimes makes mistakes in their mappings
- The tables in the articles can suffer from transcription errors and thus contain wrong numbers that don't correspond to the output of the reproduction package.

This package shall help to systematically evaluate the proposed mappings and ideally help to build a "best" mapping, which also correctly identifies the transcription errors.

## Evaluations and tests

Evaluations and tests can have different levers. Examples:

- Heuristically determine coef / se pairs from article table and compare with coef / se outcome from systematically extracted regression output in repdb.

- Determine differences between different mapping versions.

- Check for violations of typical table designs, like regressions are usually shown in columns, cells of different regressions should not be randomly distributed across a table.

The philosophy is to build smaller self contained tests that can provide info for each map version.

## Finding a "best" map / repairing problematic maps

I am not yet sure, how to best do it. Perhaps if a map version has no problems we can declare it a best version.

If there are multiple versions that differ and have problems, maybe we initiate additional AI runs, perhaps with modified prompts. Using a heuristic to "repair" problems in order to get a best version is likely messy and quite complex.

So currently the main philosophy would rather be to use heuristics rather for evaluation AI generated mappings, but not for repairing them. But that might change.
