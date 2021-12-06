@echo on
cls
scc --by-file --large-line-count=2000 --no-large -w --include-ext fs  src\renderer\common src\renderer\drawblock src\renderer\ui src\renderer\interface src\renderer\simulator

 