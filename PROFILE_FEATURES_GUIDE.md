# InCollege Profile Features Implementation Guide

## Overview
This guide explains the implemented profile features as per Epic #3 requirements:

1. **View complete saved profile with all optional fields**
2. **Display profile in an easy-to-read format**
3. **Search for other users by their full name**

## Features Implemented

### 1. Enhanced Profile Viewing
The profile viewing feature now displays all fields in a well-organized, easy-to-read format:

- **Personal Information Section**: Name, University, Major, Graduation Year
- **About Me Section**: Optional personal description (max 200 chars)
- **Professional Experience Section**: Up to 3 experiences with title, company, dates, and description
- **Education Section**: Up to 3 education entries with degree, university, and years

The profile is displayed with clear section headers and proper indentation for better readability.

### 2. User Search by Full Name
Users can now search for other users by their full name (first and last name). The search:
- Is case-insensitive
- Displays all matching profiles with their basic information
- Shows username, full name, university, and major for each match

## Testing Instructions

### Test File: InCollege-ProfileDemo-Input.txt
A test input file has been created that demonstrates all features:

1. Creates 3 users (JohnDoe, JaneSmith, BobJohnson)
2. Logs in as JohnDoe
3. Creates a complete profile with:
   - All personal information
   - About Me section
   - 2 experience entries
   - 2 education entries (planning for Bachelor's and Master's)
4. Views the profile (option 2)
5. Searches for user "Jane Smith" (option 3)

### How to Run the Test

```bash
# Copy the test input file
cp InCollege-ProfileDemo-Input.txt InCollege-Input.txt

# Compile and run
cobc -x -free InCollege.cob -o InCollege
./InCollege
```

### Expected Output

1. **Profile Display** - You should see a well-formatted profile with:
   ```
   ================================
            YOUR PROFILE           
   ================================
   --- Personal Information ---
   Name: John Doe
   University: State University
   Major: Computer Science
   Graduation Year: 2025
   
   --- About Me ---
   I am passionate about software development...
   
   --- Professional Experience ---
   
   Experience #1:
     Title: Software Engineering Intern
     Company: Tech Corp
     Dates: Summer 2024
     Description: Developed backend services...
   
   Experience #2:
     Title: Research Assistant
     Company: University Lab
     Dates: Fall 2023
     Description: Worked on data analysis...
   
   --- Education ---
   
   Education #1:
     Degree: Bachelor of Science
     University: State University
     Years: 2021-2025
   
   Education #2:
     Degree: Master of Science
     University: Tech University
     Years: 2025-2027
   ================================
   ```

2. **Search Results** - When searching for "Jane Smith":
   ```
   --- Search for User ---
   Enter the first name of the person you're looking for:
   Enter the last name of the person you're looking for:
   
   Searching...
   
   ================================
   Username: JaneSmith
   Name: Jane Smith
   University: [their university]
   Major: [their major]
   ================================
   
   1 user(s) found.
   ```

## Code Changes Summary

1. **Enhanced view-profile procedure**: Added section headers, proper formatting, and displays all optional fields including empty sections
2. **Implemented search-for-user procedure**: Complete implementation with case-insensitive search by full name
3. **Added search helper procedures**: parse-search-profile and display-search-result for clean code organization
4. **Fixed parse-profile-line-complete**: Now properly handles multiple experience and education entries

## Notes

- The profile file (`profiles.dat`) stores all profile data in a pipe-delimited format
- Search is performed on the profiles file, not the users file
- All optional fields are displayed even if empty (showing "No experience entries added" etc.)
- The search is exact match on both first AND last name (case-insensitive)
