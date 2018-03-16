# Fragility Index of meta-analyses
==========================================

The Fragility Index for meta-analyses of randomized controlled trials is an intuitive measure for the confidence we have in the conclusions of a meta-analysis.

It is defined as the *minimum number of patients* from one or more trials included in the meta-analysis for which a modification on the event status (ie, changing events to non-events, or non-events to events) would change the statistical significance of the pooled treatment effect. 

Note that after specific event-status modifications, a statistically significant pooled treatment effect could be turned non-significant, and a statistically non-significant treatment effect could be turned significant.

Here we provide functions to evaluate the Fragility Index of:

- Statistically significant meta-analyses (`fragility_sign_ma.R`)
- Statistically non-significant meta-analyses (`fragility_non_sign_ma.R`)

The statistical significance and the fragility index of the meta-analysis are relative to the measure (eg risk ratio, odds ratio or risk difference) and method used (eg mantel-haenszel, inverse variance or peto, as well as fixed or random effects) to pool the treatment effect. The pooled treatment effect according to specific methods is implemented based on the R package `meta` in the function `revman_ma.R`.

- Data should have as columns: `EVENTS\_1, EVENTS\_2, TOTAL\_1, TOTAL\_2`
- method can be: `"Inverse"`, `"MH"` or `"PETO"`
- random can be: `"YES"` or `"NO"`
- measure can be: `"RR"`,`"RD"` or `"OR"`
