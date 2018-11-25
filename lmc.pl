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
				replace_pos(Mem, ARG, Acc, Mem_),
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

/*Operazioni supportate*/
oP("ADD", 1).
oP("SUB", 2).
oP("STA", 3).
oP("LDA", 5).
oP("BRA", 6).
oP("BRZ", 7).
oP("BRP", 8).
oP("INP", 901).
oP("OUT", 902).
oP("HLT", 0).
/*TODO: aggiungi oP("DAT", 42).*/

/*parser*/
parseLine([X|T], L) :- opp(X).

/*Per OP VAL supponendo che le LBL siano tradotte*/
opp([X|T], OPCode, Arg) :- validOp(X, OPCode),
				                   validVal(T, Arg).

validOp(X, OPCode) :- string_upper(X, Ux),
				              oP(Ux, OPCode).

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

/*Elimina un commento da una riga*/
purge_comm(X, New) :- sub_string(X, Z, _, _, " //"),
                      sub_string(X, 0, Z, _, New), !.
purge_comm(X, New) :- sub_string(X, Z, _, _, "//"),
                      sub_string(X, 0, Z, _, New), !.

/*Elimina tutti i commenti data una lista di righe*/
purge_comms([], []) :- !.
purge_comms([X|S], [H|T]) :- purge_comm(X, H),
                             H \= "",
                             purge_comms(S, T),
                             !.

purge_comms([X|S], T) :- purge_comm(X, H),
                         H = "",
                         purge_comms(S, T),
                         !.

/*Sostituisce le occorrenze in lista*/
replace([], _, _, []) :- !.
replace([H|T], I, F, [H|S]) :- H \= I,
                               replace(T, I, F, S),
                               !.
replace([H|T], I, F, [F|S]) :- H = I,
                               replace(T, I, F, S),
                               !.

/*Sotituisce l'elemento in una determinata posizione*/
replace_pos([_|T], 0, X, [X|T]).
replace_pos([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace_pos(T, NI, X, R), !.
replace_pos(L, _, _, L).


/*Traduce le etichette in indirizzi*/


/*Trova la posizione dell'etichetta nel codice*/
find_pos(OG, [H|T], LBL, ADDR) :- split_string(H, " ", " ", [X|S]),
                                  X = LBL,
                                  nth0(ADDR, OG, H),
                                  !.

find_pos(OG, [H|T], LBL, ADDR) :- split_string(H, " ", " ", [X|S]),
                                  X \= LBL,
                                  find_pos(OG, T, LBL, ADDR),
                                  !.

/*Legge le linee da un file*/
readLines(File, L) :- open(File, read, Stream),
				              read_stream_to_codes(Stream, Codes),
				              close(Stream),
				              atom_codes(String, Codes),
				              string_upper(String, Res),
				              split_string(Res, "\n", "\n", L).
