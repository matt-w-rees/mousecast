You are situated inside of an R project source directory. 

The purpose of this project is to integrate data sources and forecast house mouse abundance and plagues in Australian grain growing regions. 
The main management action is to undertake poison-baiting of mice using zinc phosphide (either 25 or 50 dose). 
Providing farmers with early warning of moderate-high or increasing mouse activity is key to help them decide when to bait. 

Mouse data comprises of structured surveys:
a) live-trapping surveys where grids of traps are deployed, usually for 3 or more nights.
b) "rapid assessment" surveys, comprising of active burrow counts (in 100 m transects) and chew card assessments (cards a deployed on same transects, 10 m apart, percentage of card chewed by mice is recorded). 
The same paddock is generally surveyed repeatedly, but there are many cases where another paddock is surveyed instead. 

We also consider citizen observation data of categorical abundance estimates: no, low, moderate of high mouse abundance observed, via the "MouseAlert" website. We consider citizen observation to be less reliable than structured surveys, so often weight it less.

Each survey data type has different detection rates, and mouse detection also likely varies throughout the year for each data type too (namely active burrow counts as soil cracking or dense crop cover can obscure burrows). 

Mice population are generally driven by weather and crop growth patterns. 
It is important to consider there are different cropping schedules across Australia, notably winter-only crop schedule south of Dubbo (approximately) versus summer + winter crops north, in the eastern states. 
Western Australia has winter-only crops, but with slightly different sowing and harvesting dates. 

We consider a crop paddock as a unique "survey site". 
Data sometimes has "site" and "subsite" names, but we use GPS coordinates to determine which paddock the survey was in (although historic data often has coordinates taken from the roadside or a central farm house which makes this challenging, we can use site/subsite names to determine when this is likely the case).
There is also data along fence lines in between paddocks, but we want to discard this data and only keep surveys from within the crop. 
We use the "ePaddock" remotely sensed dataset to determine paddock boundaries, although it is not alway accurate so have to append with hand-drawn paddocks. 

Survey data is currently entered using "ODK" software. Previously "Microsoft Access" was used as the database. Data has also been entered into CSV files, as is MouseAlert data.
ODK Central form definitions (.xlsx, used to build the forms in ODK Central) live in "odk_forms/". Submissions are currently ingested by manually exporting each form from ODK Central ("Export to CSV (with media)") into "raw_data/survey_data/odk/<form_name>.csv/" and reading those files; the live ODK Central API (the "ruODK" package) was used previously but is not currently used.
The "Monitoring" project is the main data source we consider, but we also collate data from an "Ecology" project (live-trap data only in a microsoft access database, mostly experimental studies). Baiting is accounted for explicitly (bait_history/bait_dosage) rather than by excluding treated plots, so both control and treated Ecology data are used. 
There is also additional data sources, such as "GRDC" funded monitoring (via subcontractors). 

This project uses the r-package "targets"; the targets pipeline is "_targets.R" with functions contained within the "r/" folder. It is important to "track" raw files so the pipeline is only re-run (relevant sections of which) when the raw data changes.
When a raw data source is a folder of multiple related files (e.g. an ODK CSV export with main + repeat-group sub-tables), track the whole folder with tar_file(list.files(folder, full.names = TRUE, recursive = TRUE)) and have the downstream read function pick out the files it needs by filename pattern, rather than tracking and passing each file separately.
There is also a "r_not_in_use" folder to store old scripts no longer used, in case they will be useful in the future. 
It is often helpful to review the "r_not_in_use" files when asked to create a new function. 
When a script or function is fully superseded (e.g. replaced by a new approach), move it into "r_not_in_use" rather than deleting it outright. 

The pipeline has two main sections: 
(1) cleaning and combining mouse survey datasets, saving and summarising the resulting dataset through a shiny app for data exploration, as well as a html/pdf simple communication deployed via a website (updated when pushed to github). 
(2) time-series statistical forecast modelling of the cleaned mouse survey dataset, which firstly includes attaching temporally-varying and static covariates to the integrated dataset. 

This workflow is based on function-orientated programming, where each function generally acheives one task. 
The priority is therefore on readability, even if it requires more code. It is important all code is explained using hashed comments. 
Each script should be named the same as the function name, except with a prefix of "a_" or "b_" corresponding to which major pipeline section it is used in (matching the "A)"/"B)" section headers in _targets.R). 


AI assistants should read every .R file.
AI assistants should never commit or push the "docs/" folder to GitHub -- it deploys straight to the public GitHub Pages website, so updates to it are pushed manually/deliberately, not swept up into an ordinary commit.
AI assistants should always explain created code using comments.
AI assistants should put new functions at the top of the r script, although AI assistants should generally not define functions inside of functions unless they are very brief, anonymous functions. 
AI assistants should ensure all created quarto documents are self-contained. 
In "_targets.R", keep comments terse: one short sentence per line, not a wrapped paragraph spanning several "#" lines. Detailed rationale (why a function works the way it does, edge cases, etc.) belongs in that function's own header comment in "r/", not duplicated in "_targets.R" -- "_targets.R" comments should just say what a target does and point to the relevant function for detail.
Within each major section ("A)"/"B)"), subdivide with numbered "1)", "2)", "3)"... subheadings, and within those, lettered "i.", "ii.", "iii."... sub-subheadings where a further split is useful -- matching section A's existing style.
Blank lines around these headings (any level -- "A)", "1)", "i."): one blank line between a heading and its own first sub-heading/target (introducing its children); two blank lines between the end of one heading's content and the next heading of any level (a sibling, or a step back up).
In "_targets.R", a target's call collapses onto one line if it has 3 or fewer arguments (counting whichever call -- the tar_file()/tar_terra_rast() wrapper or the function it wraps -- actually carries the meaningful configuration); with 4+ arguments, give each its own line with the "=" signs vertically aligned. A pipe chain ("|>") always gets one step per line regardless of argument counts. An inline "{ }" block keeps the opening brace on the assignment's own line, indents the body one level, one statement per line, and closes with "}" back at the target's own indentation.

Future AI assistants working on this project should read these help pages via btw tools:
mvgam::mvgam()
targets::tar_plan()

