# Epic 6 Test Cases for Jira

## Test Case Format for Jira Entry

---

## Story 1 & 3: Post Job/Internship with Required Fields and Optional Salary

### TC-S1-01: Valid Job Posting With Salary
**Priority:** High  
**Type:** Positive  
**Preconditions:** User has access to job posting feature  
**Test Steps:**
1. Start job posting flow
2. Enter job title: "Software Engineer Intern"
3. Enter description: "Develop and maintain web applications using modern frameworks"
4. Enter employer: "Tech Corp Inc"
5. Enter location: "San Francisco, CA"
6. Enter salary: "$50,000/year"

**Expected Result:**
- Message displayed: "Job posted successfully!"
- Job saved to jobs.dat with all fields

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S1-02: Valid Job Posting Without Salary
**Priority:** High  
**Type:** Positive  
**Preconditions:** User has access to job posting feature  
**Test Steps:**
1. Start job posting flow
2. Enter job title: "Data Analyst"
3. Enter description: "Analyze business data and create insightful reports"
4. Enter employer: "DataViz Company"
5. Enter location: "Remote"
6. Enter salary: "NONE"

**Expected Result:**
- Message displayed: "Job posted successfully!"
- Job saved with empty salary field

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S1-03: Missing Job Title
**Priority:** High  
**Type:** Negative  
**Preconditions:** User has access to job posting feature  
**Test Steps:**
1. Start job posting flow
2. Leave job title empty (press Enter)
3. Enter description: "Work on exciting AI projects"
4. Enter employer: "AI Research Lab"
5. Enter location: "Boston, MA"
6. Enter salary: "$75,000/year"

**Expected Result:**
- Error message: "Job title is required."
- Job not saved

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S1-04: Missing Job Description
**Priority:** High  
**Type:** Negative  
**Preconditions:** User has access to job posting feature  
**Test Steps:**
1. Start job posting flow
2. Enter job title: "Marketing Manager"
3. Leave description empty (press Enter)
4. Enter employer: "Digital Marketing Inc"
5. Enter location: "New York, NY"
6. Enter salary: "$60,000/year"

**Expected Result:**
- Error message: "Job description is required."
- Job not saved

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S1-05: Missing Employer Name
**Priority:** High  
**Type:** Negative  
**Preconditions:** User has access to job posting feature  
**Test Steps:**
1. Start job posting flow
2. Enter job title: "UX Designer"
3. Enter description: "Create intuitive user interfaces for mobile apps"
4. Leave employer empty (press Enter)
5. Enter location: "Austin, TX"
6. Enter salary: "$55,000/year"

**Expected Result:**
- Error message: "Employer name is required."
- Job not saved

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S1-06: Missing Location
**Priority:** High  
**Type:** Negative  
**Preconditions:** User has access to job posting feature  
**Test Steps:**
1. Start job posting flow
2. Enter job title: "Backend Developer"
3. Enter description: "Build scalable microservices architecture"
4. Enter employer: "Cloud Solutions Ltd"
5. Leave location empty (press Enter)
6. Enter salary: "$70,000/year"

**Expected Result:**
- Error message: "Location is required."
- Job not saved

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S1-07: Description Exceeding 200 Characters
**Priority:** Medium  
**Type:** Edge Case  
**Preconditions:** User has access to job posting feature  
**Test Steps:**
1. Start job posting flow
2. Enter job title: "Project Manager"
3. Enter description: 250+ character text
4. Enter employer: "GlobalTech Corporation"
5. Enter location: "Chicago, IL"
6. Enter salary: "$80,000/year"

**Expected Result:**
- Description truncated to 200 characters
- Job posted successfully

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S1-08: Special Characters in Job Fields
**Priority:** Medium  
**Type:** Edge Case  
**Preconditions:** User has access to job posting feature  
**Test Steps:**
1. Start job posting flow
2. Enter job title: "C++ Developer (Senior)"
3. Enter description: "Work with C++ & Python to build high-performance systems"
4. Enter employer: "Tech@Innovation LLC"
5. Enter location: "Seattle, WA | Hybrid"
6. Enter salary: "$90k-$120k"

**Expected Result:**
- All special characters accepted
- Job posted successfully

**Actual Result:** ✅ PASS  
**Status:** Passed

---

## Story 2: Capture Job Fields with Validation

### TC-S2-01: Capture All Valid Fields
**Priority:** High  
**Type:** Positive  
**Test Steps:**
1. Enter job title: "Full Stack Developer"
2. Enter description: "Build modern web applications with React and Node.js"
3. Enter employer: "StartUp Innovations"
4. Enter location: "Denver, CO"

**Expected Result:**
- All fields captured and displayed
- Success message shown

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S2-02: Empty Title Validation
**Priority:** High  
**Type:** Negative  
**Test Steps:**
1. Leave title empty
2. Enter description: "Design user-friendly interfaces"
3. Enter employer: "Design Studio Pro"
4. Enter location: "Portland, OR"

**Expected Result:**
- Error: "ERROR: Job title is required."

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S2-03: Empty Description Validation
**Priority:** High  
**Type:** Negative  
**Test Steps:**
1. Enter title: "Quality Assurance Engineer"
2. Leave description empty
3. Enter employer: "TestTech Solutions"
4. Enter location: "Miami, FL"

**Expected Result:**
- Error: "ERROR: Job description is required."

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S2-04: Empty Employer Validation
**Priority:** High  
**Type:** Negative  
**Test Steps:**
1. Enter title: "DevOps Engineer"
2. Enter description: "Manage cloud infrastructure and CI/CD pipelines"
3. Leave employer empty
4. Enter location: "Philadelphia, PA"

**Expected Result:**
- Error: "ERROR: Employer name is required."

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S2-05: Empty Location Validation
**Priority:** High  
**Type:** Negative  
**Test Steps:**
1. Enter title: "Mobile App Developer"
2. Enter description: "Create cross-platform mobile applications"
3. Enter employer: "AppBuilder Inc"
4. Leave location empty

**Expected Result:**
- Error: "ERROR: Location is required."

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S2-06: Description Truncation with Warning
**Priority:** Medium  
**Type:** Edge Case  
**Test Steps:**
1. Enter title: "Senior Software Architect"
2. Enter 300+ character description
3. Enter employer: "Enterprise Solutions Group"
4. Enter location: "Washington, DC"

**Expected Result:**
- Warning: "WARNING: Description truncated to 200 chars"
- Description captured (truncated)

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S2-07: Minimal Valid Input
**Priority:** Low  
**Type:** Edge Case  
**Test Steps:**
1. Enter title: "Intern"
2. Enter description: "Learn"
3. Enter employer: "X"
4. Enter location: "Y"

**Expected Result:**
- All single-character fields accepted
- Success message shown

**Actual Result:** ✅ PASS  
**Status:** Passed

---

## Story 4: Job Persistence Testing

### TC-S4-01: Save Job to Persistent Storage
**Priority:** Critical  
**Type:** Positive  
**Test Steps:**
1. Select option 1 (Save Job)
2. Enter complete job details
3. Verify jobs.dat file created
4. Check file contents

**Expected Result:**
- Job saved to jobs.dat
- Confirmation message displayed

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S4-02: Load Jobs from Persistent Storage
**Priority:** Critical  
**Type:** Positive  
**Preconditions:** At least one job saved in jobs.dat  
**Test Steps:**
1. Select option 2 (Load Jobs)
2. View displayed jobs
3. Verify job count

**Expected Result:**
- Previously saved job displayed
- Job count shown correctly

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S4-03: Save Multiple Jobs Sequentially
**Priority:** High  
**Type:** Positive  
**Test Steps:**
1. Save first job
2. Restart program
3. Save second job
4. Load all jobs

**Expected Result:**
- Both jobs present in jobs.dat
- Jobs appended (not overwritten)

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S4-04: Save Job Without Salary
**Priority:** Medium  
**Type:** Positive  
**Test Steps:**
1. Select option 1 (Save Job)
2. Enter job with salary: "NONE"
3. Check jobs.dat contents

**Expected Result:**
- Job saved with empty salary field

**Actual Result:** ✅ PASS  
**Status:** Passed

---

## Story 5: Job Search/Internship Menu Integration

### TC-S5-01: Post Job From Menu
**Priority:** Critical  
**Type:** Positive  
**Test Steps:**
1. Select menu option 1 (Post a Job/Internship)
2. Enter complete job details
3. Job posted successfully
4. Select menu option 3 (Back to Main Menu)

**Expected Result:**
- Job posting flow completed
- Menu redisplayed after posting
- Exit to main menu works

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S5-02: Browse Jobs Under Construction
**Priority:** High  
**Type:** Functional  
**Test Steps:**
1. Select menu option 2 (Browse Jobs/Internships)
2. View message

**Expected Result:**
- Message: "Browse Jobs/Internships is under construction."
- Menu redisplayed

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S5-03: Back to Main Menu
**Priority:** Medium  
**Type:** Functional  
**Test Steps:**
1. Select menu option 3 (Back to Main Menu)

**Expected Result:**
- Message: "Returning to Main Menu..."
- Menu exits

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S5-04: Invalid Menu Choice
**Priority:** Medium  
**Type:** Negative  
**Test Steps:**
1. Enter invalid choice (5)
2. View error message
3. Select valid choice (3)

**Expected Result:**
- Error: "Invalid choice. Please enter 1, 2, or 3."
- Menu redisplayed

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S5-05: Post Multiple Jobs in One Session
**Priority:** High  
**Type:** Positive  
**Test Steps:**
1. Select option 1, post first job
2. Menu redisplays
3. Select option 1, post second job
4. Select option 3 to exit

**Expected Result:**
- Both jobs posted successfully
- Menu loops correctly

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S5-06: Validation Error in Menu Context
**Priority:** Medium  
**Type:** Negative  
**Test Steps:**
1. Select option 1 (Post Job)
2. Leave title empty
3. View error
4. Return to menu

**Expected Result:**
- Validation error displayed
- Menu redisplayed
- Can retry or exit

**Actual Result:** ✅ PASS  
**Status:** Passed

---

## Story 6: File-Based Input Verification

### TC-S6-01: Read All Inputs from File
**Priority:** Critical  
**Type:** Positive  
**Test Steps:**
1. Prepare input file with 5 lines
2. Run program
3. Verify all inputs read

**Expected Result:**
- All 5 inputs read successfully
- Counter shows: "Total inputs read from file: 05"

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S6-02: Read Inputs with NONE Salary
**Priority:** High  
**Type:** Positive  
**Test Steps:**
1. Prepare input file with "NONE" as salary
2. Run program
3. Verify NONE handled correctly

**Expected Result:**
- NONE keyword read correctly
- All 5 inputs processed

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S6-03: Read Multiline Inputs
**Priority:** Medium  
**Type:** Edge Case  
**Test Steps:**
1. Prepare input file with long description
2. Run program
3. Verify description read correctly

**Expected Result:**
- Long description read as single line
- All inputs processed correctly

**Actual Result:** ✅ PASS  
**Status:** Passed

---

## Story 7: File-Based Output Verification

### TC-S7-01: All Prompts Written to Output File
**Priority:** Critical  
**Type:** Positive  
**Test Steps:**
1. Run program with valid input
2. Check InCollege-Output.txt
3. Compare with screen output

**Expected Result:**
- All prompts in output file
- Screen and file output identical

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S7-02: Output with NONE Salary
**Priority:** High  
**Type:** Positive  
**Test Steps:**
1. Post job with salary "NONE"
2. Check output file

**Expected Result:**
- NONE prompt in output
- All 12 output lines written

**Actual Result:** ✅ PASS  
**Status:** Passed

---

### TC-S7-03: Success Message in Output
**Priority:** High  
**Type:** Positive  
**Test Steps:**
1. Post valid job
2. Verify success message in output file

**Expected Result:**
- "Job posted successfully!" in output file
- Output counter shows correct count

**Actual Result:** ✅ PASS  
**Status:** Passed

---

## Test Execution Summary for Jira

**Sprint:** Week 6  
**Epic:** Epic #6 - Job Board Functionality  
**Total Test Cases:** 31  
**Executed:** 31  
**Passed:** 31 ✅  
**Failed:** 0  
**Blocked:** 0  
**Pass Rate:** 100%  

**Test Execution Date:** October 21, 2025  
**Tested By:** Testing Team  
**Environment:** Windows 11 + WSL, GnuCOBOL  

**Bugs Found:** None  
**Blockers:** None  
**Ready for Release:** YES ✅

---

## Jira Labels to Use

- `epic-6`
- `week-6`
- `job-posting`
- `testing`
- `cobol`
- `file-io`
- `validation`
- `persistence`
- `menu-navigation`

---

## Notes for Jira Entry

1. All test cases use file-based input from `InCollege-Input.txt`
2. All test outputs written to `InCollege-Output.txt`
3. Persistence tested using `jobs.dat` file
4. Test files available in `Epic6-Storyx-Test-Input.zip` and `Epic6-Storyx-Test-Output.zip`
5. Full documentation in `Epic6-Tests/TEST-DOCUMENTATION.md`

---

**End of Test Case Document**

