# Epic #3 - Individual Story Implementation Files

This directory contains separate COBOL files for each Epic #3 story, allowing for independent testing and development of each feature.

## Story Files

### 1. Story1-ViewCompleteProfile.cob
**User Story**: "As a logged-in user, I want to view my complete saved profile, including all optional fields."

**Features**:
- Enhanced profile display with formatted sections
- Shows all optional fields (About Me, Experience, Education)
- Displays empty sections with appropriate messages
- Easy-to-read format with clear section headers

**Test Input**: `Story1-Test-Input.txt`
**Compile**: `cobc -x -free Story1-ViewCompleteProfile.cob -o Story1`
**Run**: `./Story1`

### 2. Story2-EasyToReadFormat.cob
**User Story**: "As a logged-in user, I want my profile to be displayed in an easy-to-read format."

**Features**:
- Clear section headers with visual separators
- Proper indentation for sub-fields
- Organized sections: Personal Info, About Me, Experience, Education
- Clean spacing between sections for better readability
- Consistent formatting throughout

**Test Input**: `Story1-Test-Input.txt` (same as Story 1)
**Compile**: `cobc -x -free Story2-EasyToReadFormat.cob -o Story2`
**Run**: `./Story2`

### 3. Story3-SearchByFullName.cob
**User Story**: "As a logged-in user, I want to search for other users by their full name."

**Features**:
- Case-insensitive search by first and last name
- Displays matching users with basic information
- Shows count of users found
- Clear search result formatting
- Handles no results gracefully

**Test Input**: `Story3-Test-Input.txt`
**Compile**: `cobc -x -free Story3-SearchByFullName.cob -o Story3`
**Run**: `./Story3`

## Testing Instructions

### Prerequisites
1. Ensure `profiles.dat` exists with test data
2. Ensure `InCollege-Input.txt` exists (or copy from test input files)

### Running Individual Stories

1. **Test Story 1 (Complete Profile View)**:
   ```bash
   cp Story1-Test-Input.txt InCollege-Input.txt
   cobc -x -free Story1-ViewCompleteProfile.cob -o Story1
   ./Story1
   cat InCollege-Output.txt
   ```

2. **Test Story 2 (Easy-to-Read Format)**:
   ```bash
   cp Story1-Test-Input.txt InCollege-Input.txt
   cobc -x -free Story2-EasyToReadFormat.cob -o Story2
   ./Story2
   cat InCollege-Output.txt
   ```

3. **Test Story 3 (Search by Full Name)**:
   ```bash
   cp Story3-Test-Input.txt InCollege-Input.txt
   cobc -x -free Story3-SearchByFullName.cob -o Story3
   ./Story3
   cat InCollege-Output.txt
   ```

### Expected Output

- **Story 1 & 2**: Should display a formatted profile with all sections clearly marked
- **Story 3**: Should show search results for "Jane Smith" if that user exists in profiles.dat

## Integration

All three stories are integrated into the main `InCollege.cob` file:
- Story 1 & 2: Implemented in the `view-profile` procedure
- Story 3: Implemented in the `search-for-user` procedure

## File Structure

```
InCollege-Team-Kansas/
├── InCollege.cob                    # Main integrated program
├── Story1-ViewCompleteProfile.cob   # Story 1 implementation
├── Story2-EasyToReadFormat.cob     # Story 2 implementation  
├── Story3-SearchByFullName.cob     # Story 3 implementation
├── Story1-Test-Input.txt           # Test input for Stories 1 & 2
├── Story3-Test-Input.txt           # Test input for Story 3
├── profiles.dat                     # Profile data file
└── InCollege-Output.txt            # Output file (generated)
```

## Notes

- Each story file is self-contained and can be compiled/run independently
- All output is written to `InCollege-Output.txt` for easy verification
- The main `InCollege.cob` contains all features integrated together
- Test input files are provided for each story's specific requirements
