# Story 4 - Connection Request Testing Instructions

## Test Story 4 (Connection Requests) - Using Main Program:
cp Story4-Test-Input.txt InCollege-Input.txt
cobc -x -free InCollege.cob -o InCollege
./InCollege
cat InCollege-Output.txt
diff Story4-Test-Output.txt InCollege-Output.txt

## Expected Results:
- Should show successful account creation for alice, bob, charlie
- Should show profile creation for all users
- Should show alice successfully sending connection requests to bob and charlie
- Should show alice viewing pending requests (none)
- Output should match Story4-Test-Output.txt
