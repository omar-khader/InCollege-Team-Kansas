1) Create userA and login (seeds users.dat #1)
1b) Create userB and login (seed #2)
1c) Create userC and login (seed #3)
1d) Create userD and login (seed #4)
1e) Create userE and login (seed #5)
2) Invalid password — too short
3) Invalid password — no uppercase
4) Invalid password — no digit
5) Invalid password — no special
6) Login wrong once, then right (assumes userA exists)
7) Login wrong, then EOF (assumes userA exists; should exit, not hang)
8) Account limit reached (run after users A–E exist)
9) Invalid initial choice, then exit
10) Case-insensitive command (uppercase LOGIN)
11) Leading whitespace tolerated