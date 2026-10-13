# hmmTMB: potential bugs and limitations

Issues found while building the plague HMM (`r/e_fit_plague_hmm.R` and related scripts), against hmmTMB 1.1.2 (CRAN) and the patched development version pinned in `renv.lock` (github.com/matt-w-rees/hmmTMB, branch `betabinom-dev`). Where it was checked, the upstream master (github.com/TheoMichelot/hmmTMB, last pushed 2026-09-25) has the same code. Each entry gives what happens, how it showed up, and the workaround used here.

Entries are grouped as **likely bugs** (behaviour that looks wrong and could be reported upstream), **silent limitations** (documented or by design, but easy to miss because nothing warns), and **numerical behaviour** (fits that fail or mislead).

---

## Likely bugs

### 1. State-specific formula terms are misallocated when a formula has interactions

- **What happens:** in an observation formula, a term wrapped in `state1()`, `state2()` and so on should enter only that state's formula. `make_formulas()` finds these terms with `terms(form, specials = ...)`, whose `specials` attribute indexes the formula's **variables**, but then uses those positions to pick from the **term labels**. The two lists only line up when the formula has no interactions. Otherwise the wrong term is moved between states, with no warning.
- **Found:** 2026-10-03. With `state1(s(paddock, bs = "re"))` added to the burrow mean (which includes `season * cropping_system`):
  - the paddock random effect appeared in **both** states;
  - `season:cropping_system` was dropped from state 2's formula.

  The chew-card formula, which has no interactions, was built correctly.
- **Workaround:** `fit_plague_hmm()` writes single-state terms first in each formula, where the two lists line up. After building the observation model, it checks that every state's formula holds exactly the intended terms, and stops otherwise.
- **To report upstream:** yes. Minimal example: `~ a * b + state1(s(g, bs = "re"))`, then inspect `obs$formulas()`.

### 2. `HMM$pseudores()` forecasts with the wrong transition matrix

- **What happens:** the forward algorithm (`forward_backward()`) moves from row *r* to row *r* + 1 with `tpms[, , r]`, i.e. the matrix at row *r* links *r* to *r* + 1. `pseudores()` instead builds each row's one-step-ahead forecast from the previous row's forward probabilities times `tpms[, , i]`, the **destination** row's matrix. With covariate-driven transitions, the residuals therefore use the next season's covariates.
- **Found:** 2026-10-03, comparing the two code paths. This project's `randomised_pseudores()` (`r/e_randomised_pseudores.R`) copied the same convention and needs the same fix.
- **Impact:** only on residual diagnostics, not on fits or state probabilities.
- **To report upstream:** yes.

### 3. `pseudores()` isn't randomised for discrete distributions

- **What happens:** quantile residuals are computed as `qnorm(F(y))` with no randomisation between F(y − 1) and F(y). For count data this biases residuals upwards and compresses them, even when the model is correct (mean ≈ 0.47, SD ≈ 0.70 on simulated, correctly specified negative-binomial data).
- **Found:** 2026-09, after many model variants chased an apparent miscalibration that was this bias.
- **Workaround:** `randomised_pseudores()` (`r/e_randomised_pseudores.R`).
- **To report upstream:** yes, as a suggestion (randomised residuals for discrete families).

### 4. AIC/edf counted fixpar-tied coefficients once per copy (CRAN 1.1.2)

- **What happens:** coefficients tied to one shared value through `fixpar` were each counted as a separate parameter (about 520 vs 355 edf in this model); likelihoods and estimates were unaffected.
- **Status:** fixed in the development version (pinned in `renv.lock`).

### 5. `beta` and `zoibeta` families don't evaluate (CRAN 1.1.2)

- **What happens:** `$llk()` returns "Log-likelihood is NaN or infinite at starting parameters" even for a trivial, constant, fully interior response, while other families work under the same setup. Neither family has a help page or an example.
- **Found:** 2026-08, trying a success-rate model for trap data.
- **Workaround:** not used. Traps stay negative binomial with an effort offset.

---

## Silent limitations

### 6. `offset()` terms are dropped without warning

- **What happens:** `make_matrices()` calls `mgcv::gam(fit = FALSE)` and takes only the design matrix, never the offset, so `offset(x)` in a formula has no effect. A model with and without it gives identical coefficients.
- **Workaround:** the effort variable enters as an ordinary covariate whose coefficient is fixed at 1 with `fixpar` (`offset()` in `_targets.R` is translated this way by `fit_plague_hmm()`).

### 7. Random-effect coefficients can't be fixed or tied

- **What happens:** in `HMM$setup()`, `fixpar` reaches fixed effects (`obs`, `hid`), smoothing parameters (`lambda_obs`, `lambda_hid`) and `delta0`, but the random-effect coefficients are listed as `NA` (`usernms <- c("obs", "lambda_obs", "hid", "lambda_hid", "delta0", NA, NA)`). So a random effect (or smooth) can't be shared across states or across streams; every state gets its own coefficients and smoothing parameter.
- **Impact here:**
  - subregion random effects had to be per state, and failed to fit;
  - the four chew-card blocks can't share paddock random effects.
- **Possible fix:** a shared random-effect design in a fork. Tying through TMB's `map` alone would count the penalty's normalising constant twice.

### 8. Binomial `size` is fixed per state and can't vary with covariates

- **What happens:** `size` (the number of trials) is a fixed parameter. Giving it a formula is silently ignored. A scalar `size` mis-broadcasts across states, with only a "number of items to replace" warning; it needs a per-state vector.
- **Workaround:** chew cards are split into fixed blocks of 10 cards, each its own stream with tied coefficients (hmmTMB's own pattern for varying replicate counts).

### 9. No zero-inflated or hurdle family usable with a mean offset

- **What happens:** `zinbinom` and `hnbinom` are parameterised by size/prob, so an effort offset can't act on the mean alone. `zinbinom` also has no pseudo-residuals ("not implemented… Returning NA").

### 10. `fixpar` ties across different families without any check

- **What happens:** a tie between, say, a negative-binomial mean (log link) and a binomial probability (logit link) is accepted silently, forcing one number onto two different link scales. That's user error, but nothing warns.

### 11. Some single-function behaviours worth knowing

- **Missing covariates:** hmmTMB fills missing covariates with the last non-NA value ("Replacing NAs in covariates by last non-NA values"). `-Inf` (e.g. `log(0)`) is not treated as missing and later crashes `$predict(newdata = ...)` inside mgcv.
- **Reloaded models:** `AIC()`/`logLik()` on a model reloaded from disk (e.g. the targets store) segfault, because the TMB object's pointers don't survive serialisation. Fit statistics have to be computed in the fitting process.

---

## Numerical behaviour

### 12. Non-deterministic likelihood evaluations

- **What happens:** with byte-identical inputs, roughly 1 in 4 starts (seen mainly with the three-parameter chew-card families) fail with "NaN or infinite at starting parameters", evaluate to a garbage objective (about ±1e30 to 1e275), or converge with a spurious constant on the objective. A failed start tends to persist within the same R session; several fits at once in parallel processes also fail more often.
- **Workaround:** `fit_plague_hmm_subprocess()` fits in a fresh process each time, retries with jittered starts, and rejects fits with a non-finite or implausible objective, collapsed state shares or runaway coefficients.

### 13. Smoothing parameters can run to infinity

- **What happens:** `HMM$fit()` optimises fixed effects and log smoothing parameters jointly in a single `nlminb()` call. mgcv uses a dedicated outer loop instead. When a smooth wants to be linear, the smoothing parameter walks towards about 1e300, giving crashes, NaN AIC or false convergence, depending on where it overflows.

### 14. AIC/edf implausible for some models

- **What happens:**
  - the 3-state model reports about 2,900 effective parameters;
  - the monthly-smooth model reports 119, fewer than the simpler model it extends (226).

  The package flags AIC as "experimental" for models with random effects or smooths, but these values are far enough off to suggest the effective degrees of freedom are being computed wrongly, not just approximately.
- **Workaround:** compare objectives or log-likelihoods on identical data, and forecast skill, rather than AIC, for these models.

### 15. Convergence messages that mislead

- **What happens:** fits can report convergence code 0 while returning hmmTMB's untouched starting values, or with coefficients sitting at their starting values (e.g. exactly 10, or the default transition intercepts) and a NaN likelihood.
- **Workaround:** never trust the convergence code alone. Check that AIC/objective are finite and that coefficients have moved (part of `fit_plague_hmm_subprocess()`'s validity checks).
