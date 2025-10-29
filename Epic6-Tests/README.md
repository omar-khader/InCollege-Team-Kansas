# Epic 6 Test Suite - Quick Reference

## Overview
Comprehensive test suite for Week 6 Job Posting functionality covering 31 test cases across 7 user stories.

## Test Statistics
- **Total Tests:** 31
- **Pass Rate:** 100% ✅
- **Stories Covered:** 7
- **Test Files:** 31 input files, 31 output files

## Directory Structure
```
Epic6-Tests/
├── Story1and3/          # Post Job with Validation (8 tests)
├── Story2/              # Field Capture & Validation (7 tests)
├── Story4/              # Persistence Testing (4 tests)
├── Story5/              # Menu Integration (6 tests)
├── Story6/              # File Input Verification (3 tests)
├── Story7/              # File Output Verification (3 tests)
├── TEST-DOCUMENTATION.md  # Complete test documentation
└── README.md            # This file
```

## Quick Test Execution

### Compile Programs (one-time)
```bash
cd /mnt/c/Users/omark/Desktop/CEN4020-Project
cobc -x -free StoryOneAndThree.cob -o Story1and3
cobc -x -free StoryTwo.cob -o Story2
cobc -x -free StoryFour.cob -o Story4
cobc -x -free StoryFive.cob -o Story5
cobc -x -free StorySix.cob -o Story6
cobc -x -free StorySeven.cob -o Story7
```

### Run Single Test Example
```bash
cp Epic6-Tests/Story1and3/Test1-ValidJobWithSalary-Input.txt InCollege-Input.txt
./Story1and3
cat InCollege-Output.txt
```

## Story Descriptions

### Story 1 & 3: Post Job with Required Fields + Optional Salary
Tests job posting with validation for required fields (title, description, employer, location) and optional salary handling.

**Key Tests:**
- Valid jobs with/without salary
- Missing required fields (4 tests)
- Long descriptions (>200 chars)
- Special characters

### Story 2: Capture Job Fields with Validation
Tests field validation logic before saving to persistence.

**Key Tests:**
- Empty field detection
- Description truncation with warning
- Minimal valid input

### Story 4: Job Persistence Testing
Tests saving and loading jobs from jobs.dat file.

**Key Tests:**
- Save single job
- Load persisted jobs
- Save multiple jobs
- Jobs without salary

### Story 5: Job Search/Internship Menu Integration
Tests menu navigation and job posting workflow.

**Key Tests:**
- Menu navigation (Post/Browse/Back)
- Invalid menu choices
- Multiple jobs in one session
- Error handling in menu context

### Story 6: File-Based Input Verification
Verifies all inputs are read from InCollege-Input.txt file.

**Key Tests:**
- Read all 5 job fields
- Handle "NONE" salary
- Multiline input handling

### Story 7: File-Based Output Verification
Verifies all outputs are written to InCollege-Output.txt file.

**Key Tests:**
- All prompts in output
- Output consistency
- Output counting

## Test Types

### Positive Tests (15)
Tests that should succeed with valid input:
- S1-T1, S1-T2, S2-T1, S2-T7
- S4-T1, S4-T2, S4-T3, S4-T4
- S5-T1, S5-T5
- S6-T1, S6-T2
- S7-T1, S7-T2, S7-T3

### Negative Tests (10)
Tests that should fail gracefully:
- S1-T3, S1-T4, S1-T5, S1-T6 (missing required fields)
- S2-T2, S2-T3, S2-T4, S2-T5 (empty fields)
- S5-T4 (invalid menu)
- S5-T6 (missing field in menu)

### Edge Cases (6)
Tests with boundary conditions:
- S1-T7 (long description)
- S1-T8 (special characters)
- S2-T6 (description >200 chars)
- S2-T7 (minimal input)
- S6-T3 (multiline input)
- S5-T2, S5-T3 (functional tests)

## Deliverables

### Created Files
1. ✅ `Epic6-Storyx-Test-Input.zip` - All 31 test input files
2. ✅ `Epic6-Storyx-Test-Output.zip` - All test outputs and results
3. ✅ `TEST-DOCUMENTATION.md` - Comprehensive test report
4. ✅ `README.md` - Quick reference guide

### Source Programs Tested
- `StoryOneAndThree.cob` (Story 1 & 3)
- `StoryTwo.cob` (Story 2)
- `StoryFour.cob` (Story 4)
- `StoryFive.cob` (Story 5)
- `StorySix.cob` (Story 6)
- `StorySeven.cob` (Story 7)

## Test Results Summary

| Story | Tests | Pass | Fail | Pass Rate |
|-------|-------|------|------|-----------|
| 1 & 3 | 8     | 8    | 0    | 100% ✅   |
| 2     | 7     | 7    | 0    | 100% ✅   |
| 4     | 4     | 4    | 0    | 100% ✅   |
| 5     | 6     | 6    | 0    | 100% ✅   |
| 6     | 3     | 3    | 0    | 100% ✅   |
| 7     | 3     | 3    | 0    | 100% ✅   |
| **Total** | **31** | **31** | **0** | **100% ✅** |

## Key Validation Rules Tested

1. **Required Fields:** Title, Description, Employer, Location must not be empty
2. **Optional Field:** Salary can be empty or "NONE"
3. **Description Length:** Maximum 200 characters (truncated if longer)
4. **Persistence:** Jobs saved to jobs.dat in pipe-delimited format
5. **File I/O:** All input from file, all output to both screen and file

## Data File Format

### jobs.dat Structure
```
username|title|description|employer|location|salary
```

Example:
```
testuser|Software Engineer|Build web apps|Tech Corp|SF, CA|$50,000/year
testuser|Data Analyst|Analyze data|DataCo|Remote|
```

## Important Notes

1. **Description Truncation:** Stories 1&3 truncate silently, Story 2 shows warning
2. **Output Format:** All output lines padded to 80 characters
3. **Persistence:** jobs.dat is created if it doesn't exist, appended if it does
4. **Menu Loop:** Story 5 menu loops until user chooses option 3 (Back)
5. **Salary:** "NONE" and empty string both result in empty salary field

## For Jira Documentation

### Bugs Found: 0
All functionality working as expected.

### Test Execution Status
- Story 1 & 3: ✅ Complete (8/8 passed)
- Story 2: ✅ Complete (7/7 passed)
- Story 4: ✅ Complete (4/4 passed)
- Story 5: ✅ Complete (6/6 passed)
- Story 6: ✅ Complete (3/3 passed)
- Story 7: ✅ Complete (3/3 passed)

### Recommendations
1. Proceed with Week 6 submission
2. Next sprint: Implement "Browse Jobs/Internships" feature
3. Consider adding job editing/deletion features

## Contact
For questions about test cases or results, refer to TEST-DOCUMENTATION.md for detailed information.

---

**Status:** All tests complete ✅  
**Date:** October 21, 2025  
**Ready for submission:** YES

