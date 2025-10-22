# Week 6 - Manual Tasks Remaining

## ✅ COMPLETED BY TESTING TEAM

1. ✅ **Roles.txt** - Created with team roles and responsibilities
2. ✅ **Epic6-Storyx-Test-Input.zip** - 6 consolidated test input files
3. ✅ **Epic6-Storyx-Test-Output.zip** - 6 consolidated test output files
4. ✅ **InCollege-Input.txt** - Sample input (exists from test runs)
5. ✅ **InCollege-Output.txt** - Sample output (exists from test runs)
6. ✅ **Test Documentation** - Complete test reports created
7. ✅ **All 31 test cases** - Executed successfully (100% pass)
8. ✅ **Consolidated test files** - One per story (positive, negative, edge cases)

---

## ❌ MANUAL TASKS REMAINING (Cannot be automated)

### 1. **Jira.jpg Screenshot**
**What:** Screenshot of your Jira board showing Epic #6 and all user stories

**How to create:**
1. Go to your team's Jira board
2. Make sure Epic #6 is visible with all user stories:
   - Epic #6: Job Board Functionality
   - Story 1 & 3: Post Job with Required/Optional Fields
   - Story 2: Capture Job Fields with Validation
   - Story 4: Job Persistence Testing
   - Story 5: Job Search Menu Integration
   - Story 6: File-Based Input Verification
   - Story 7: File-Based Output Verification
3. Take a screenshot showing:
   - All stories with their status (In Progress/Done)
   - All tasks under each story
   - Any bug tickets (we have 0 bugs)
4. Save as `Jira.jpg` in project root

**Reference:** Use `Epic6-Tests/JIRA-TEST-CASES.md` for test case details to add to Jira

---

### 2. **Jira Burndown Charts (2 screenshots)**
**What:** Two burndown charts showing sprint progress

**Chart 1 - Created on Monday (Sprint Start):**
1. Go to Jira Reports → Burndown Chart
2. Take screenshot at start of sprint (Monday)
3. Shows initial sprint backlog
4. Save as `Jira-Burndown-Monday.jpg`

**Chart 2 - Created at Sprint End:**
1. Go to Jira Reports → Burndown Chart
2. Take screenshot at end of sprint (today)
3. Shows completed work and remaining work (should be 0)
4. Save as `Jira-Burndown-Complete.jpg`

---

### 3. **GitHub.jpg Screenshot**
**What:** Screenshot of commit history for Week 6

**How to create:**
1. Go to: https://github.com/[your-repo]/CEN4020-Project
2. Click on the "Commits" link (next to green "Code" button)
3. Make sure you're viewing the correct branch (Vivek or main)
4. Screenshot should show:
   - Chronological list of commits
   - Commit messages
   - Author names
   - Timestamps
   - Include at least Week 6 commits showing:
     * COBOL story implementations
     * Test file commits
     * Documentation commits
5. Save as `GitHub.jpg` in project root

---

## 📋 RECOMMENDED JIRA USER STORIES (if not already created)

Based on requirements, create these in Jira:

### Epic #6: Job Board Functionality

**User Stories:**

1. **Story 1 & 3**: "As a logged-in user, I want to post a new job or internship listing so I can recruit for my company/organization."
   - Tasks: Define job data structure, implement validation, add persistence
   - Status: Done ✅
   - Tests: 8 (all passed)

2. **Story 2**: "As a user posting a job, I want to enter a job title, description, employer, and location."
   - Tasks: Create input forms, validate required fields
   - Status: Done ✅
   - Tests: 7 (all passed)

3. **Story 3** (merged with Story 1): "As a user posting a job, I want the option to include a salary for the listing."
   - Tasks: Add optional salary field, handle NONE input
   - Status: Done ✅
   - Covered in Story 1 tests

4. **Story 4**: "As a user, I want my posted job listings to be saved persistently."
   - Tasks: Create jobs.dat, implement save/load functions
   - Status: Done ✅
   - Tests: 4 (all passed)

5. **Story 5**: "As a user, I want the 'Job search/internship' menu option to allow me to post jobs."
   - Tasks: Create menu, integrate posting flow, add under construction message
   - Status: Done ✅
   - Tests: 6 (all passed)

6. **Story 6**: "As a tester, I want the program to read all user inputs for job posting from a file so I can automate testing."
   - Tasks: Implement file-based input reading
   - Status: Done ✅
   - Tests: 3 (all passed)

7. **Story 7**: "As a tester, I want the program to write all screen output related to job posting to a file so I can easily verify results."
   - Tasks: Implement file-based output writing
   - Status: Done ✅
   - Tests: 3 (all passed)

**Total Story Points:** [Fill in based on your team's estimation]
**Sprint Duration:** [Fill in your sprint dates]
**Velocity:** All stories completed on time

---

## 📝 QUICK CHECKLIST

Before submission, ensure you have:

### In Project Root:
- ✅ Roles.txt
- ✅ Epic6-Storyx-Test-Input.zip
- ✅ Epic6-Storyx-Test-Output.zip
- ✅ InCollege-Input.txt (sample)
- ✅ InCollege-Output.txt (sample)
- ❌ Jira.jpg (manual screenshot)
- ❌ Jira-Burndown-Monday.jpg (manual screenshot)
- ❌ Jira-Burndown-Complete.jpg (manual screenshot)
- ❌ GitHub.jpg (manual screenshot)
- ⚠️ InCollege.cob (in Vivek branch - programmers will merge)

### In Epic6-Tests/:
- ✅ TEST-DOCUMENTATION.md (comprehensive test report)
- ✅ JIRA-TEST-CASES.md (Jira-ready test cases)
- ✅ README.md (quick reference)
- ✅ 6 consolidated input files (StoryX-AllTests-Input.txt)
- ✅ 6 consolidated output files (StoryX-AllTests-Output.txt)

### Documentation:
- ✅ EPIC6-TEST-SUMMARY.md (executive summary)
- ✅ All test cases documented with expected vs actual results
- ✅ 100% pass rate documented
- ✅ 0 bugs found documented

---

## 🎯 SUMMARY FOR TESTING TEAM

**Testing work is 95% complete!**

**What the testing team has completed:**
- ✅ Created all 31 test cases (positive, negative, edge)
- ✅ Executed all tests successfully (100% pass rate)
- ✅ Created comprehensive test documentation
- ✅ Generated test input/output zip files
- ✅ Verified data persistence
- ✅ Verified file I/O consistency
- ✅ Created Roles.txt

**What's left (manual only):**
- ❌ Take 4 screenshots (Jira board, 2 burndown charts, GitHub commits)
- ❌ Upload screenshots to project root
- ❌ (Optional) Update Jira with test case details from JIRA-TEST-CASES.md

**Estimated time for remaining tasks:** 10-15 minutes

---

## 💡 TIPS

1. **For Jira screenshots:** Make sure all sprint work is visible and status is updated
2. **For Burndown charts:** The second chart should show all work completed (line at 0)
3. **For GitHub screenshot:** Include enough commits to show the team's work this week
4. **File naming:** Use exact names listed above for consistency

---

**Status:** Ready for final manual tasks and submission! 🎉

