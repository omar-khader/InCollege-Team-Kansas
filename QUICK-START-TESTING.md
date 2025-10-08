# Quick Start: Week 5 Testing

## 🚀 Get Started in 5 Minutes

### Step 1: Review the Documentation
```bash
# Read the comprehensive test plan
cat WEEK5-TESTING-PLAN.md

# Read the execution guide
cat tests/week5/TEST-EXECUTION-GUIDE.md

# Review the summary
cat WEEK5-TESTING-SUMMARY.md
```

### Step 2: Choose Which Branch to Test

**Option A: Nicholas's Implementation (Standalone)**
```bash
git checkout -b testing-nick origin/nick-code
cobc -x -free StoryEight.cob -o InCollege
```

**Option B: Adesh's Implementation (Modular)**
```bash
git checkout -b testing-adesh origin/Adesh-Dev
# Compile individual story programs as needed
cobc -x -free Story93_Remove_Requests.cob -o Story93
cobc -x -free Story94_View_Connected_Users.cob -o Story94
```

### Step 3: Run Your First Test

**Test 1.1: Accept Single Request**
```bash
# Setup
cd /path/to/CEN4020-Project
rm -f InCollege-Output.txt connections.dat

# Prepare test data
echo "alice,AlicePass1!" > users.dat
echo "bob,BobPass1!" >> users.dat
cp tests/week5/accepting/test-1.1-setup-connections.dat connections.dat

# Execute test
cp tests/week5/accepting/test-1.1-accept-single-input.txt InCollege-Input.txt
./InCollege

# Verify
cat InCollege-Output.txt | grep "accepted"
cat connections.dat
```

### Step 4: Document Results
```bash
# Update the test matrix in WEEK5-TESTING-PLAN.md
# Mark test as PASS or FAIL
# If FAIL, create bug report using template
```

---

## 📋 Test Checklist

### HIGH Priority (Do These First)
- [ ] Test 1.1: Accept Single Request
- [ ] Test 1.2: Accept Multiple Requests
- [ ] Test 2.1: Reject Single Request
- [ ] Test 2.2: Reject Multiple Requests
- [ ] Test 3.1: Mixed Accept/Reject
- [ ] Test 4.3: Multiple Connections Display
- [ ] Test 5.1: Persistence After Restart

### MEDIUM Priority (Do These Next)
- [ ] Test 1.3: Bidirectional Visibility
- [ ] Test 4.1: No Connections Display
- [ ] Test 4.2: One Connection Display
- [ ] Test 4.4: No Rejected in Network
- [ ] Test 4.5: No Pending in Network

### LOW Priority (Do If Time)
- [ ] Test 2.3: Reject Resend
- [ ] Test 3.2: Multi-Session
- [ ] Test 5.2: Removal Persistence
- [ ] Test 5.3: Multi-User Persistence

---

## 🎯 Key Files

| File | Purpose |
|------|---------|
| `WEEK5-TESTING-PLAN.md` | Comprehensive test cases & acceptance criteria |
| `WEEK5-TESTING-SUMMARY.md` | Code review findings & deliverables |
| `tests/week5/TEST-EXECUTION-GUIDE.md` | Step-by-step execution procedures |
| `tests/week5/accepting/` | Accept request test files |
| `tests/week5/rejecting/` | Reject request test files |
| `tests/week5/mixed/` | Mixed scenario test files |
| `tests/week5/network/` | Network display test files |

---

## 🐛 Found a Bug?

1. **Capture artifacts**:
   ```bash
   mkdir -p bug-reports/bug-$(date +%Y%m%d-%H%M%S)
   cp InCollege-Input.txt bug-reports/bug-*/
   cp InCollege-Output.txt bug-reports/bug-*/
   cp connections.dat bug-reports/bug-*/
   ```

2. **Create Jira ticket** using template in WEEK5-TESTING-PLAN.md

3. **Update test matrix** with bug link

---

## ✅ When Are You Done?

You're done when:
- ✅ All HIGH priority tests executed
- ✅ Test matrix fully updated
- ✅ All bugs reported in Jira
- ✅ Console/file output verified as identical
- ✅ All artifacts saved

---

## 📞 Need Help?

- **Detailed Test Cases**: See `WEEK5-TESTING-PLAN.md`
- **Execution Steps**: See `tests/week5/TEST-EXECUTION-GUIDE.md`
- **Code Review Info**: See `WEEK5-TESTING-SUMMARY.md`
- **Bug Template**: In `WEEK5-TESTING-PLAN.md` under "Bug Reporting Template"

---

**Ready? Start with Test 1.1!** 🚀

