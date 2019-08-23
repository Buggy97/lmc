# LMC
  Little man computer emulator, written in both Prolog and Common Lisp.
  
## Use 
### Prolog
  Get memory representation of assembly source
  ```prolog
  lmc_load(AssemblySource, MemoryRepresentation)
  ```
  Run the assembly file
  ```prolog
  lmc_run(File, InputQueueList, OutputQueueList)
  ``` 
  Get next state of the machine given current state
  ```prolog
  one_instruction(state(Acc, Pc, Mem, In, Out, Flag), Out)
  ```
  Like one_instruction but it loops until the machine halts
  ```
  execution_loop(
	state(Acc, Pc, Mem, In, Out, Flag), Outt)
  ```
  
### Common Lisp
  Get the memory representation of assembly source
  ```lisp
  (lmc-load file)
  ```
  Run the assembly file
  ```lisp
  (lmc-run file inputQueue) 
  ``` 
  Get next state of the machine given current state
  ```prolog
  (process-state (list   'state
                :acc (getf (rest st) :acc)
                :pc (getf (rest st) :pc)
                :mem (getf (rest st) :mem)
                :in (getf (rest st) :in)
                :out (getf (rest st) :out)
                :flag (getf (rest st) :flag)))
  ```
  Like one-instruction but it loops until the machine halts
  ```
  (execution-loop state)
  ```
  
### Assembly file
  The emulator supports SUB, STA, LDA, BRA, BRZ, BRP, DAT, INP, OUT, HLT
  ```assembly
  add 3
  out
  hlt
  dat 42
  ```
  
## Limitations
  Maximum 100 lines for the assembly file, including data declarations.
