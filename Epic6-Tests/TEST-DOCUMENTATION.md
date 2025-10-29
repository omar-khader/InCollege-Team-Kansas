# Epic 6 - Week 6 Job Posting Functionality Test Documentation
## InCollege Project Testing Report

**Tester:** Testing Team  
**Sprint:** Week 6  
**Epic:** Epic #6 - Job Board Functionality  
**Date:** October 21, 2025  

---

## Executive Summary

This document provides comprehensive test coverage for Week 6 Job Posting functionality. All 31 test cases across 7 user stories were executed successfully, validating:
- Job posting with required fields (title, description, employer, location)
- Optional salary field handling
- Field validation and error handling
- Data persistence to jobs.dat file
- Menu navigation and integration
- File-based input/output verification

---

## Test Environment

- **COBOL Compiler:** GnuCOBOL (via WSL)
- **Operating System:** Windows 11 with WSL
- **Test Framework:** File-based input/output testing
- **Data Files:** jobs.dat, InCollege-Input.txt, InCollege-Output.txt

---

## User Stories Tested

### Story 1 & 3: Post Job/Internship with Required Fields and Optional Salary
**User Story:** "As a logged-in user, I want to post a new job or internship listing with required fields (title, description, employer, location) and optional salary."

**Acceptance Criteria:**
- ✅ All required fields must be captured
- ✅ Salary field is optional
- ✅ Validation prevents empty required fields
- ✅ Job data is saved to persistent storage
- ✅ User receives confirmation message

### Story 2: Capture Job Fields with Validation
**User Story:** "As a user posting a job, I want field validation to ensure data quality."

**Acceptance Criteria:**
- ✅ Empty required fields display error messages
- ✅ Descriptions exceeding 200 characters are truncated
- ✅ All captured fields are displayed for confirmation
- ✅ Minimal valid input is accepted

### Story 4: Job Persistence Testing
**User Story:** "As a user, I want my posted job listings to be saved persistently."

**Acceptance Criteria:**
- ✅ Jobs are saved to jobs.dat file
- ✅ Jobs persist after program termination
- ✅ Multiple jobs can be saved sequentially
- ✅ Jobs can be loaded and displayed from storage
- ✅ Jobs without salary are saved correctly

### Story 5: Job Search/Internship Menu Integration
**User Story:** "As a user, I want the 'Job search/internship' menu option to allow me to post jobs."

**Acceptance Criteria:**
- ✅ Menu displays three options (Post, Browse, Back)
- ✅ Option 1 allows posting jobs
- ✅ Option 2 shows "under construction" message
- ✅ Option 3 returns to main menu
- ✅ Invalid choices show error messages
- ✅ Menu loops until user exits
- ✅ Multiple jobs can be posted in one session

### Story 6: File-Based Input Verification
**User Story:** "As a tester, I want the program to read all user inputs for job posting from a file."

**Acceptance Criteria:**
- ✅ All 5 job fields are read from InCollege-Input.txt
- ✅ File handles various input formats
- ✅ Input counter tracks number of reads
- ✅ Program displays what was read from file

### Story 7: File-Based Output Verification
**User Story:** "As a tester, I want the program to write all screen output to a file."

**Acceptance Criteria:**
- ✅ All prompts written to InCollege-Output.txt
- ✅ Output file matches screen output exactly
- ✅ Output counter tracks number of writes
- ✅ Success messages included in output

---

## Test Case Summary

### Story 1 & 3: 8 Test Cases

| Test ID | Test Name | Type | Expected Result | Status |
|---------|-----------|------|-----------------|--------|
| S1-T1 | Valid Job With Salary | Positive | Job posted successfully | ✅ PASS |
| S1-T2 | Valid Job Without Salary | Positive | Job posted with empty salary | ✅ PASS |
| S1-T3 | Missing Title | Negative | "Job title is required." error | ✅ PASS |
| S1-T4 | Missing Description | Negative | "Job description is required." error | ✅ PASS |
| S1-T5 | Missing Employer | Negative | "Employer name is required." error | ✅ PASS |
| S1-T6 | Missing Location | Negative | "Location is required." error | ✅ PASS |
| S1-T7 | Long Description (>200) | Edge | Description truncated to 200 chars | ✅ PASS |
| S1-T8 | Special Characters | Edge | Special chars accepted in all fields | ✅ PASS |

### Story 2: 7 Test Cases

| Test ID | Test Name | Type | Expected Result | Status |
|---------|-----------|------|-----------------|--------|
| S2-T1 | All Valid Fields | Positive | All fields captured successfully | ✅ PASS |
| S2-T2 | Empty Title | Negative | "ERROR: Job title is required." | ✅ PASS |
| S2-T3 | Empty Description | Negative | "ERROR: Job description is required." | ✅ PASS |
| S2-T4 | Empty Employer | Negative | "ERROR: Employer name is required." | ✅ PASS |
| S2-T5 | Empty Location | Negative | "ERROR: Location is required." | ✅ PASS |
| S2-T6 | Description >200 Chars | Edge | Truncated with warning | ✅ PASS |
| S2-T7 | Minimal Valid Input | Edge | Single-char fields accepted | ✅ PASS |

### Story 4: 4 Test Cases

| Test ID | Test Name | Type | Expected Result | Status |
|---------|-----------|------|-----------------|--------|
| S4-T1 | Save Job | Positive | Job saved to jobs.dat | ✅ PASS |
| S4-T2 | Load Jobs | Positive | Previously saved job displayed | ✅ PASS |
| S4-T3 | Save Multiple Jobs | Positive | Multiple jobs appended to file | ✅ PASS |
| S4-T4 | Save Job No Salary | Positive | Job with empty salary saved | ✅ PASS |

### Story 5: 6 Test Cases

| Test ID | Test Name | Type | Expected Result | Status |
|---------|-----------|------|-----------------|--------|
| S5-T1 | Post Job From Menu | Positive | Job posted via menu navigation | ✅ PASS |
| S5-T2 | Browse Under Construction | Functional | "under construction" message shown | ✅ PASS |
| S5-T3 | Back to Main Menu | Functional | Returns to main menu | ✅ PASS |
| S5-T4 | Invalid Menu Choice | Negative | Error message for invalid choice | ✅ PASS |
| S5-T5 | Multiple Jobs | Positive | Two jobs posted sequentially | ✅ PASS |
| S5-T6 | Missing Field in Menu | Negative | Validation error, returns to menu | ✅ PASS |

### Story 6: 3 Test Cases

| Test ID | Test Name | Type | Expected Result | Status |
|---------|-----------|------|-----------------|--------|
| S6-T1 | Read All Inputs | Positive | All 5 inputs read from file | ✅ PASS |
| S6-T2 | Read Inputs No Salary | Positive | "NONE" salary read correctly | ✅ PASS |
| S6-T3 | Read Multiline Inputs | Edge | Long description read correctly | ✅ PASS |

### Story 7: 3 Test Cases

| Test ID | Test Name | Type | Expected Result | Status |
|---------|-----------|------|-----------------|--------|
| S7-T1 | Output All Prompts | Positive | All prompts written to output file | ✅ PASS |
| S7-T2 | Output With None Salary | Positive | NONE salary handled in output | ✅ PASS |
| S7-T3 | Output Success Message | Positive | Success message in output file | ✅ PASS |

---

## Test Results Summary

- **Total Test Cases:** 31
- **Passed:** 31 ✅
- **Failed:** 0
- **Pass Rate:** 100%

---

## Detailed Test Case Descriptions

### Story 1 & 3 Test Cases

#### Test 1: Valid Job With Salary
**Input:**
```
Software Engineer Intern
Develop and maintain web applications using modern frameworks
Tech Corp Inc
San Francisco, CA
$50,000/year
```
**Expected:** Job posted successfully with all fields saved to jobs.dat
**Result:** ✅ PASS

#### Test 2: Valid Job Without Salary
**Input:**
```
Data Analyst
Analyze business data and create insightful reports
DataViz Company
Remote
NONE
```
**Expected:** Job posted successfully with empty salary field
**Result:** ✅ PASS

#### Test 3-6: Missing Required Fields
**Scenarios:** Missing title, description, employer, and location respectively
**Expected:** Appropriate error message displayed, job not saved
**Result:** ✅ PASS (all 4 tests)

#### Test 7: Long Description
**Input:** 250-character description
**Expected:** Description truncated to 200 characters, job posted
**Result:** ✅ PASS

#### Test 8: Special Characters
**Input:** Job with C++, &, |, @ symbols
**Expected:** Special characters accepted in all fields
**Result:** ✅ PASS

### Story 2 Test Cases

#### Tests focus on field validation before saving
- Empty field detection for each required field
- Description length validation and truncation
- Display of captured values
- All tests passed with appropriate error messages

### Story 4 Test Cases

#### Test 1-2: Save and Load Persistence
**Process:**
1. Clean jobs.dat
2. Save job with Test 1
3. Load jobs with Test 2
**Expected:** Job persists and can be loaded
**Result:** ✅ PASS - Job data correctly stored and retrieved

#### Test 3-4: Multiple Jobs and No Salary
**Expected:** Multiple jobs appended to file, NONE salary handled
**Result:** ✅ PASS - jobs.dat contains all posted jobs

### Story 5 Test Cases

#### Test 1: Complete Menu Flow
**Input:** Menu choice 1 → job details → menu choice 3
**Expected:** Job posted, menu loops, exits gracefully
**Result:** ✅ PASS

#### Test 2: Browse Under Construction
**Input:** Menu choice 2 → menu choice 3
**Expected:** "Browse Jobs/Internships is under construction."
**Result:** ✅ PASS

#### Test 4: Invalid Menu Choice
**Input:** Menu choice 5 → menu choice 3
**Expected:** "Invalid choice. Please enter 1, 2, or 3."
**Result:** ✅ PASS

#### Test 5: Multiple Jobs in Session
**Input:** Two complete job postings
**Expected:** Both jobs posted and saved
**Result:** ✅ PASS

#### Test 6: Validation in Menu Context
**Input:** Empty title → returns to menu
**Expected:** Error displayed, menu redisplayed
**Result:** ✅ PASS

### Story 6 & 7 Test Cases

#### File I/O Verification
All tests confirm:
- Inputs read from InCollege-Input.txt
- Outputs written to InCollege-Output.txt
- Screen output matches file output exactly
- Input/output counts accurate

---

## Key Findings

### ✅ Strengths
1. **Robust Validation:** All required field validations working correctly
2. **Data Persistence:** Jobs.dat file handling is reliable
3. **Menu Navigation:** Clean menu flow with proper error handling
4. **File I/O:** Input/output file handling is consistent and accurate
5. **Edge Cases:** Long descriptions truncated properly, special characters handled

### 📋 Observations
1. **Description Truncation:** Story 2 truncates with warning, Story 1 truncates silently
   - Not a bug, different story requirements
2. **Output Line Width:** All output lines are 80 characters (padded with spaces)
   - This is by design for consistent file format
3. **Jobs.dat Format:** Pipe-delimited format: `username|title|desc|employer|loc|salary`
   - Clean and parseable format

### 💡 Recommendations
1. **For Future Sprints:**
   - Implement "Browse Jobs/Internships" functionality
   - Add job deletion capability
   - Implement job search/filter features
   - Add job posting editing capability

2. **Test Data:**
   - All test files preserved in Epic6-Tests/ directory
   - Output files saved for verification
   - jobs.dat snapshots saved for persistence tests

---

## Data Persistence Verification

### jobs.dat File Format
Each job is stored as a pipe-delimited line:
```
testuser|Job Title|Description|Employer|Location|Salary
```

### Verified Persistence Scenarios
1. ✅ Single job save and load
2. ✅ Multiple jobs appended correctly
3. ✅ Jobs with empty salary field
4. ✅ Jobs with special characters
5. ✅ File created if doesn't exist
6. ✅ File appended if exists

---

## File Locations

### Test Input Files
- `Epic6-Storyx-Test-Input.zip` - Contains all 31 input test files
- `Epic6-Tests/StoryX/TestY-TestName-Input.txt` - Individual test inputs

### Test Output Files
- `Epic6-Storyx-Test-Output.zip` - Contains all test outputs and results
- `Epic6-Tests/StoryX/outputs/TestY-TestName-Output.txt` - Individual test outputs
- `Epic6-Tests/Story4/outputs/TestX-jobs.dat` - Persistence test data snapshots

### Source Code
- `StoryOneAndThree.cob` - Story 1 & 3 implementation
- `StoryTwo.cob` - Story 2 implementation
- `StoryFour.cob` - Story 4 implementation
- `StoryFive.cob` - Story 5 implementation
- `StorySix.cob` - Story 6 implementation
- `StorySeven.cob` - Story 7 implementation

---

## Running the Tests

### Prerequisites
```bash
# Compile all COBOL programs
cobc -x -free StoryOneAndThree.cob -o Story1and3
cobc -x -free StoryTwo.cob -o Story2
cobc -x -free StoryFour.cob -o Story4
cobc -x -free StoryFive.cob -o Story5
cobc -x -free StorySix.cob -o Story6
cobc -x -free StorySeven.cob -o Story7
```

### Running Individual Tests
```bash
# Example: Run Story 1 Test 1
cp Epic6-Tests/Story1and3/Test1-ValidJobWithSalary-Input.txt InCollege-Input.txt
./Story1and3
# View output
cat InCollege-Output.txt
```

### Running All Tests
```bash
# See full test execution script in this documentation
# Tests are organized by story in Epic6-Tests/ directory
```

---

## Test Coverage Analysis

### Functional Coverage
- ✅ Job posting with all fields
- ✅ Job posting with optional fields omitted
- ✅ Field validation (required fields)
- ✅ Field validation (field length)
- ✅ Data persistence (save)
- ✅ Data persistence (load)
- ✅ Menu navigation
- ✅ Error handling
- ✅ File I/O consistency

### Input Validation Coverage
- ✅ Empty strings
- ✅ Very long strings (>200 chars)
- ✅ Minimal valid strings (1 char)
- ✅ Special characters
- ✅ "NONE" keyword for optional salary
- ✅ Invalid menu choices

### User Experience Coverage
- ✅ Clear prompts
- ✅ Helpful error messages
- ✅ Success confirmation
- ✅ Menu loop functionality
- ✅ Graceful exit options

---

## Conclusion

All 31 test cases passed successfully, demonstrating that Week 6 Job Posting functionality meets all acceptance criteria. The implementation includes:

1. Complete job posting with required and optional fields
2. Comprehensive field validation
3. Reliable data persistence
4. Clean menu integration
5. Consistent file-based I/O for testing

The system is ready for the next phase of development (job browsing/searching functionality).

---

## Sign-off

**Tester:** Testing Team  
**Date:** October 21, 2025  
**Status:** All tests passed ✅  
**Recommendation:** Approve for Week 6 deliverable submission

---

## Appendix: Test Execution Commands

### Complete Test Execution Script
All tests were executed using WSL with the following commands:

```bash
# Create output directories
mkdir -p Epic6-Tests/Story{1and3,2,4,5,6,7}/outputs

# Story 1&3 Tests (8 tests)
for i in {1..8}; do
    cp Epic6-Tests/Story1and3/Test${i}-*-Input.txt InCollege-Input.txt
    ./Story1and3
    cp InCollege-Output.txt Epic6-Tests/Story1and3/outputs/Test${i}-*-Output.txt
done

# Story 2 Tests (7 tests)
for i in {1..7}; do
    cp Epic6-Tests/Story2/Test${i}-*-Input.txt InCollege-Input.txt
    ./Story2
    cp InCollege-Output.txt Epic6-Tests/Story2/outputs/Test${i}-*-Output.txt
done

# Story 4 Tests (4 tests with persistence)
rm -f jobs.dat
for i in {1..4}; do
    cp Epic6-Tests/Story4/Test${i}-*-Input.txt InCollege-Input.txt
    ./Story4
    cp InCollege-Output.txt Epic6-Tests/Story4/outputs/Test${i}-*-Output.txt
    cp jobs.dat Epic6-Tests/Story4/outputs/Test${i}-jobs.dat 2>/dev/null || true
done

# Story 5 Tests (6 tests)
for i in {1..6}; do
    cp Epic6-Tests/Story5/Test${i}-*-Input.txt InCollege-Input.txt
    ./Story5
    cp InCollege-Output.txt Epic6-Tests/Story5/outputs/Test${i}-*-Output.txt
done

# Story 6 Tests (3 tests)
for i in {1..3}; do
    cp Epic6-Tests/Story6/Test${i}-*-Input.txt InCollege-Input.txt
    ./Story6
    cp InCollege-Output.txt Epic6-Tests/Story6/outputs/Test${i}-*-Output.txt
done

# Story 7 Tests (3 tests)
for i in {1..3}; do
    cp Epic6-Tests/Story7/Test${i}-*-Input.txt InCollege-Input.txt
    ./Story7
    cp InCollege-Output.txt Epic6-Tests/Story7/outputs/Test${i}-*-Output.txt
done
```

**Total Execution Time:** ~2 minutes  
**All Tests Passed:** 31/31 ✅

