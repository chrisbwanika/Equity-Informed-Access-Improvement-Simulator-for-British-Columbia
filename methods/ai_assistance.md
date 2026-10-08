# AI assistance statement
This repository was designed and is maintained by Christopher Bwanika. An AI assistant
drafted the code, including helping with the tests and documentation under my direction. I
specified the decision problem, the scope and the methods; reviewed every file, ran the full pipeline and
the full test suite on my own computer, and I am responsible for all modelling choices,
results and any errors. All data are synthetic.

## Verification of AI-drafted material
| File or section | What the AI drafted | How I verified it | Date |
|---|---|---|---|
| R/02_engine.R | Generic cohort engine | Read line by line; known-answer tests pass | |
|Whole pipeline (run_all.R)	Ran on my own laptop (Windows, R 4.6.1) | | all 48 outputs produced in 3.7 minutes| 1 October 2026|
|Test suite (40 tests)	Ran tests/run_tests.R| | 3,794 expectations passed, 0 failed| 1 October 2026|

## Rules followed
- No real, personal or confidential data was ever given to an AI tool.
- No AI output was accepted without running it and testing it.
- No input value was taken from an AI as evidence; all inputs are labelled synthetic.
