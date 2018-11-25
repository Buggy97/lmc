/*state(Acc, Pc, Mem, In, Out, Flag) TODO Controllo cut*/
/*halted_state(Acc, Pc, Mem, In, Out, Flag)*/
/*one_instruction(State, State)*/

/*EXECUTION LOOP*/
execution_loop(State, Out) :- functor(State, halted_state, _),
							  arg(5, State, Out).				 
							  
execution_loop(state(Acc, Pc, Mem, In, Out, Flag), F) :-						 
				one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NextState),	
				execution_loop(NextState, F).						  
							  
/**********************************DEBUG******************************************
dbexecution_loop(State, State) :- functor(State, halted_state, 6).				 *
																				 *
dbexecution_loop(state(Acc, Pc, Mem, In, Out, Flag), F) :-						 *
				one_instruction(state(Acc, Pc, Mem, In, Out, Flag), NextState),	 *
				execution_loop(NextState, F).								     *
****************************************************************************^****/
/*OVERHEAD*/

one_instruction(halted_state(Acc, Pc, Mem, In, Out, Flag), 
				halted_state(Acc, Pc, Mem, In, Out, Flag)) :- !.
				
/*ADD*/
one_instruction(state(Acc, Pc, Mem, In, Out, _), State) :- 
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '1',
				Acc_ is Acc + ARG,
				Pc_ is (Pc+1) mod  100,
				normalize(Acc_, Acc__, Flag_),
				copy_term(state(Acc__, Pc_, Mem, In, Out, Flag_), State),
				Pc_ \= 0,
				!.
				
/*SUB*/			
one_instruction(state(Acc, Pc, Mem, In, Out, _), State) :- 
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '2',
				Acc_ is Acc - ARG,
				Pc_ is (Pc + 1) mod  100,
				normalize(Acc_, Acc__, Flag_),
				copy_term(state(Acc__, Pc_, Mem, In, Out, Flag_), State),
				Pc_ \= 0,
				!.
				
/*STORE*/
one_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :- 
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '3',
				Pc_ is (Pc + 1) mod  100,
				replace(ARG, Acc, Mem_),
				copy_term(state(Acc, Pc_, Mem_, In, Out, Flag), State),
				Pc_ \= 0,
				!.
				
/*LOAD*/
one_instruction(state(_, Pc, Mem, In, Out, Flag), State) :-  			
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '5',
				Pc_ is (Pc + 1) mod  100,
				nth0(ARG, Mem, Acc_),
				copy_term(state(Acc_, Pc_, Mem, In, Out, Flag), State),
				Pc_ \= 0,
				!.
				
/*BRANCH*/
one_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :- 
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '6',
				Pc_ is (ARG) mod  100,
				copy_term(state(Acc, Pc_, Mem, In, Out, Flag), State),
				!.

/*BRANCHIZ*/
/*TRUE*/
one_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :- 
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '7',
				Acc = 0,
				Flag = noflag,
				Pc_ is (ARG) mod 100,
				copy_term(state(Acc, Pc_, Mem, In, Out, Flag), State),
				!.
/*FALSE*/		
one_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :- 
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, _),
				OP = '7',
				Pc_ is (Pc + 1) mod  100,
				copy_term(state(Acc, Pc_, Mem, In, Out, Flag), State),
				!.

/*BRANCHIP*/

/*TRUE*/
one_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :- 
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '8',
				Flag = noflag,
				Pc_ is (ARG) mod  100,
				copy_term(state(Acc, Pc_, Mem, In, Out, Flag), State),
				!.
/*FALSE*/	
one_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :- 
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, _),
				OP = '8',
				Flag \= noflag,
				Pc_ is (Pc + 1) mod  100,
				copy_term(state(Acc, Pc_, Mem, In, Out, Flag), State),
				!.
				
/*INPUT*/		
one_instruction(state(_, Pc, Mem, In, Out, Flag), State) :- 
				nth0(Pc, Mem, INST),
				string_chars(INST, [X, Y, Z|_]),
				X = '9', Y = '0', Z = '1',
				In \= [],
				remove_last(In, In_, T),
				Acc_ is (T) mod  1000,
				Pc_ is (Pc + 1) mod  100,
				copy_term(state(Acc_, Pc_, Mem, In_, Out, Flag), State),
				Pc_ \= 0,
				!.

/*OUTPUT*/				
one_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :- 
				nth0(Pc, Mem, INST),
				string_chars(INST, [X, Y, Z|_]),
				X = '9', Y = '0', Z = '2',
				Pc_ is (Pc + 1) mod  100,
				copy_term(state(Acc, Pc_, Mem, In, [Acc|Out], Flag), State),
				Pc_ \= 0,
				!.

/*HALT*/
one_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :- 
				nth0(Pc, Mem, INST),
				atom_chars(INST, [X]), X = '0',
				copy_term(halted_state(Acc, Pc, Mem, In, Out, Flag), State),
				!.				
				
/*Utils*/
/*Rimuove l'ultimo elemento da una lista*/
remove_last([], [], "") :- !.
remove_last([X], [], X) :- !.
remove_last([H|T], [H|S], F) :- remove_last(T, S, F),
								!.
/*Data una istruzione resituisce OPCODE e argomento*/
split_instr(Inst, X, Arg) :- string_chars(Inst, [X|T]),
							 string_chars(Arg_, T),
							 atom_number(Arg_, Arg).
				
/*Verifica che il valore è positivo e modulo 1000, se il valore non
 *Rispetta le condizioni allora restituisce flag altrimenti noflag
 */
normalize(Val, NewVal, Flag) :- ((Val > 999; Val < 0),
								NewVal is Val mod 1000, 
								atom_string(Flag, "flag")), !;
								NewVal is Val, atom_string(Flag, "noflag"), !.
								
/*Sostituisce un valore in una posizione*/
replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).
								
								