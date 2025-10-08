# Week 5 Test Execution Guide

## 📋 Overview
This guide provides step-by-step instructions for executing all Week 5 connection management test cases.

---

## 🛠️ Prerequisites

### Required Files:
- `InCollege.cob` or appropriate Story programs
- COBOL compiler (`cobc`)
- Test input files (in respective test directories)
- Test setup files (`connections.dat`, `users.dat`)

### Environment Setup:
```bash
cd /path/to/CEN4020-Project
```

---

## 🧪 Test Execution Steps

### General Test Procedure

For EACH test case:

1. **Setup Phase**:
   ```bash
   # Clean previous test data
   rm -f InCollege-Output.txt
   rm -f connections.dat users.dat profiles.dat
   
   # Copy setup files
   cp tests/week5/[category]/test-X.X-setup-connections.dat connections.dat
   cp tests/week5/[category]/test-X.X-setup-users.dat users.dat
   
   # Copy test input
   cp tests/week5/[category]/test-X.X-input.txt InCollege-Input.txt
   ```

2. **Compile** (if needed):
   ```bash
   cobc -x -free InCollege.cob -o InCollege
   # OR for specific story:
   cobc -x -free StoryEight.cob -o InCollege
   ```

3. **Execute Test**:
   ```bash
   ./InCollege
   ```

4. **Verification Phase**:
   ```bash
   # Save output
   cp InCollege-Output.txt tests/week5/[category]/test-X.X-actual-output.txt
   
   # Compare with expected (if available)
   diff tests/week5/[category]/test-X.X-expected-output.txt \
        tests/week5/[category]/test-X.X-actual-output.txt
   
   # Verify connections.dat
   cat connections.dat
   ```

5. **Document Results**:
   - Update WEEK5-TESTING-PLAN.md test matrix
   - Create bug report if test fails
   - Save all artifacts

---

## 📂 Test Category Execution

### 1. Accepting Requests Tests

#### Test 1.1: Accept Single Request
```bash
# Setup
rm -f InCollege-Output.txt connections.dat users.dat profiles.dat
cp tests/week5/accepting/test-1.1-setup-connections.dat connections.dat

# Create users (alice and bob must exist)
# Option A: Use existing users.dat
# Option B: Create accounts first

# Execute
cp tests/week5/accepting/test-1.1-accept-single-input.txt InCollege-Input.txt
./InCollege

# Verify
cat connections.dat
# Expected: bob|alice|connected (or removed if rejected requests are deleted)
cat InCollege-Output.txt | grep "accepted"
# Expected: "Connection request from bob accepted!"
```

#### Test 1.2: Accept Multiple Requests
```bash
# Setup
rm -f InCollege-Output.txt connections.dat
cp tests/week5/accepting/test-1.2-setup-connections.dat connections.dat

# Execute
cp tests/week5/accepting/test-1.2-accept-multiple-input.txt InCollege-Input.txt
./InCollege

# Verify
cat connections.dat | grep "connected" | wc -l
# Expected: 3 connected entries
```

---

### 2. Rejecting Requests Tests

#### Test 2.1: Reject Single Request
```bash
# Setup
rm -f InCollege-Output.txt connections.dat
cp tests/week5/rejecting/test-2.1-setup-connections.dat connections.dat

# Execute
cp tests/week5/rejecting/test-2.1-reject-single-input.txt InCollege-Input.txt
./InCollege

# Verify
cat connections.dat | grep "bob"
# Expected: Either no entry OR status=rejected
cat InCollege-Output.txt | grep "rejected"
# Expected: "Connection request from bob rejected!"
```

#### Test 2.2: Reject Multiple Requests
```bash
# Setup
rm -f InCollege-Output.txt connections.dat
cp tests/week5/rejecting/test-2.2-setup-connections.dat connections.dat

# Execute
cp tests/week5/rejecting/test-2.2-reject-multiple-input.txt InCollege-Input.txt
./InCollege

# Verify
cat connections.dat | grep "pending"
# Expected: No pending entries for alice
```

---

### 3. Mixed Scenarios Tests

#### Test 3.1: Accept Some, Reject Others
```bash
# Setup
rm -f InCollege-Output.txt connections.dat
cp tests/week5/mixed/test-3.1-setup-connections.dat connections.dat

# Execute
cp tests/week5/mixed/test-3.1-mixed-accept-reject-input.txt InCollege-Input.txt
./InCollege

# Verify
cat connections.dat | grep "connected" | wc -l
# Expected: 3 connected entries (bob, david, frank)

cat InCollege-Output.txt | grep "accepted" | wc -l
# Expected: 3 acceptance messages

cat InCollege-Output.txt | grep "rejected" | wc -l
# Expected: 2 rejection messages
```

---

### 4. Network Display Tests

#### Test 4.1: No Connections
```bash
# Setup
rm -f InCollege-Output.txt connections.dat
touch connections.dat  # Empty file

# Execute
cp tests/week5/network/test-4.1-no-connections-input.txt InCollege-Input.txt
./InCollege

# Verify
cat InCollege-Output.txt | grep -i "no.*connection"
# Expected: Message indicating no connections
```

#### Test 4.2: One Connection
```bash
# Setup
rm -f InCollege-Output.txt connections.dat
cp tests/week5/network/test-4.2-setup-connections.dat connections.dat

# Execute
cp tests/week5/network/test-4.2-one-connection-input.txt InCollege-Input.txt
./InCollege

# Verify
cat InCollege-Output.txt | grep -i "total connections"
# Expected: "Total connections: 1"
```

#### Test 4.3: Multiple Connections
```bash
# Setup
rm -f InCollege-Output.txt connections.dat
cp tests/week5/network/test-4.3-setup-connections.dat connections.dat

# Execute
cp tests/week5/network/test-4.3-multiple-connections-input.txt InCollege-Input.txt
./InCollege

# Verify
cat InCollege-Output.txt | grep -i "total connections"
# Expected: "Total connections: 5"

cat InCollege-Output.txt | grep "1\." 
# Expected: List of 5 users numbered 1-5
```

---

### 5. Persistence Tests

#### Test 5.1: Connections Persist After Restart
```bash
# Phase 1: Accept connections
rm -f connections.dat InCollege-Output.txt
echo "bob|alice|pending" > connections.dat
echo "charlie|alice|pending" >> connections.dat

# Create input that accepts both
cat > InCollege-Input.txt << EOF
1
alice
AlicePass1!
4
1
1
0
EOF

./InCollege
cp connections.dat connections-after-accept.dat

# Phase 2: Restart and verify
cat > InCollege-Input.txt << EOF
1
alice
AlicePass1!
5
EOF

rm -f InCollege-Output.txt
./InCollege

# Verify
cat InCollege-Output.txt | grep -i "total connections"
# Expected: "Total connections: 2"
diff connections-after-accept.dat connections.dat
# Expected: Files should be identical
```

---

## 🐛 Bug Reporting Workflow

When a test fails:

1. **Capture All Artifacts**:
   ```bash
   # Create bug report directory
   mkdir -p bug-reports/bug-XXX
   
   # Save all relevant files
   cp InCollege-Input.txt bug-reports/bug-XXX/
   cp InCollege-Output.txt bug-reports/bug-XXX/
   cp connections.dat bug-reports/bug-XXX/connections-after.dat
   cp users.dat bug-reports/bug-XXX/
   cp profiles.dat bug-reports/bug-XXX/
   ```

2. **Create Jira Ticket**:
   - Use bug template from WEEK5-TESTING-PLAN.md
   - Attach all artifact files
   - Assign appropriate priority

3. **Document in Test Matrix**:
   - Update status to "FAILED"
   - Link to Jira ticket
   - Add notes about failure

---

## ✅ Output Verification Checklist

For each test execution:

- [ ] Console output saved or captured
- [ ] InCollege-Output.txt generated
- [ ] Console and file outputs compared
- [ ] Line-by-line match verified
- [ ] All prompts present in output file
- [ ] All confirmation messages captured
- [ ] connections.dat in expected state
- [ ] No unexpected side effects
- [ ] Test result documented

---

## 🔧 Troubleshooting

### Common Issues:

**Issue**: "ERROR opening InCollege-Input.txt"
- **Solution**: Ensure input file exists and has correct name

**Issue**: connections.dat not updating
- **Solution**: Check file permissions, verify write access

**Issue**: Output file empty
- **Solution**: Verify `say` procedure writes to OutFile

**Issue**: Users don't exist
- **Solution**: Create users first or use pre-populated users.dat

---

## 📊 Test Results Documentation

After completing all tests:

1. **Update Test Matrix** in WEEK5-TESTING-PLAN.md:
   ```markdown
   | Test ID | Status | Tester | Date | Bugs Found |
   |---------|--------|--------|------|------------|
   | 1.1     | PASS   | Omar   | 10/8 | None       |
   ```

2. **Generate Summary Report**:
   ```bash
   # Count test results
   echo "Total Tests: XX"
   echo "Passed: XX"
   echo "Failed: XX"
   echo "Bugs Found: XX"
   ```

3. **Archive Test Artifacts**:
   ```bash
   tar -czf week5-test-results-$(date +%Y%m%d).tar.gz \
     tests/week5/ \
     bug-reports/ \
     WEEK5-TESTING-PLAN.md
   ```

---

## 🎯 Success Criteria

Testing is complete when:
- ✅ All test cases executed at least once
- ✅ All HIGH priority tests documented
- ✅ All bugs reported in Jira
- ✅ Test matrix 100% filled out
- ✅ Artifacts saved and archived
- ✅ Final report generated

---

## 📞 Support

For questions or issues:
- Check WEEK5-TESTING-PLAN.md for test case details
- Review bug reports for similar issues
- Consult with development team (Adesh/Nicholas)

---

**Document Version**: 1.0  
**Last Updated**: October 8, 2025  
**Prepared By**: Testing Team

