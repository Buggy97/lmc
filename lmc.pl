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
oP("DAT", '').

/*Utils*/
/*Verifica se stringa e' una istruzione*/
is_op(X, 1) :- oP(X, _), !.
is_op(_, 0) :- !.

validMem([X|S]) :- validArr([X|S]), length([X|S], 100), !.
validArr([]) :- !.
validArr([F|S]) :- atom(F), atom_number(F, X), between(0, 999, X), validArr(S), !.
validArr([X|S]) :- number(X), between(0, 999, X), validArr(S), !.

/*Rimuove l'ultimo elemento da una lista*/
remove_last([], [], "") :- !.
remove_last([X], [], X) :- !.
remove_last([H|T], [H|S], F) :- remove_last(T, S, F),
								!.

/*Resitituisce la prima cifra dato un numero*/
numb('', '') :- !.
numb(F, T) :- number_string(F, S), string_chars(S, [C|_]), 
			  number_chars(T, [C]), !.


/*Data una istruzione numerica resituisce OPCODE e argomento*/
split_instr(Inst, X, Arg) :- string_chars(Inst, [X|T]),
							 string_chars(Arg_, T),
							 atom_number(Arg_, Arg).

/*Data una riga restituisce l'istruzione in memoria*/
/*Gestice il caso ISTRUZIONE LBL/VAL*/							
parseLine(Insts, Line, Inst) :-	split_string(Line, " ", " ", [Y, T]),
								get_value(Insts, T, Val),
								oP(Y, Z),
								atomic_list_concat([Z, Val], Inst),
								%atom_number(Inst2, Inst),
								!.
/*Gestice il caso LBL ISTRUZIONE LBL/VAL*/
parseLine(Insts, Line, Inst) :- split_string(Line, " ", " ", [_, Y, T]),
								get_value(Insts, T, Val),
								oP(Y, Op),
								atomic_list_concat([Op, Val], Inst),
								%atom_number(Inst2, Inst),
								!.


parseLine(_, Line, '0') :- split_string(Line, " ", " ", [X]),
						 X = "DAT",
						 !.

parseLine(_, Line, '0') :- split_string(Line, " ", " ", [_, X]),
						 X = "DAT",
						 !.
	 
parseLine(_, Line, F) :- split_string(Line, " ", " ", [X]),
						 oP(X, F2),
						 atom_number(F, F2),
						 !.
	 
parseLine(_, Line, F) :- split_string(Line, " ", " ", [_, X]),
						 oP(X, F2),
						 atom_number(F, F2),
						 !.							

/*Data una lista di istruzioni restituisce la lista convertita in istruzioni*/
parseLines([], _, []) :- !.
parseLines([H | T], RAW, [X | S]) :- parseLine(RAW, H, X),
									 parseLines(T, RAW, S),
									 !.

/*Dato un argomento di un'istruzione restituisce un valore*/
get_value(_, Term, Final) :- atom_string(Final, Term), atom_number(Final, _),!.
get_value(Insts, Term, Val) :- find_pos(Insts, Insts, Term, Val), !.
						  

/*Verifica che il valore è positivo e modulo 1000, se il valore non
 *Rispetta le condizioni allora restituisce flag altrimenti noflag
 */

out_of_bounds(Val) :- Val < 0, !.
out_of_bounds(Val) :- Val > 999, !.

/*normalize(Val, NewVal, Flag) :- ((Val > 999; Val < 0),
								NewVal is Val mod 1000,
								atom_string(Flag, "flag")), !;
								NewVal is Val, atom_string(Flag, "noflag"), !.*/

normalize(Val, NewVal, Flag) :- out_of_bounds(Val),
								NewVal is Val mod 1000,
								atom_string(Flag, "flag"), !.

normalize(Val, NewVal, Flag) :- NewVal is Val, atom_string(Flag, "noflag"), !.

/*Elimina un commento da una riga*/
purge_comm(X, New2) :- sub_string(X, Z, _, _, "//"),
					   sub_string(X, 0, Z, _, New), 
					   normalize_space(string(New2), New),
					   !.
purge_comm(X, X2) :- normalize_space(string(X2), X), !.


/*Elimina tutti i commenti data una lista di righe*/
purge_comms([], []) :- !.
purge_comms([X|S], [H|T]) :- purge_comm(X, H),
                             H \= "",
                             purge_comms(S, T), !.
                             
purge_comms([X|S], T) :- purge_comm(X, H),
                         H = "",
                         purge_comms(S, T), !.

/*Sostituisce le occorrenze in lista*/
replace([], _, _, []) :- !.
replace([H|T], I, F, [H|S]) :- H \= I,
                               replace(T, I, F, S),
                               !.
replace([H|T], I, F, [F|S]) :- H = I,
                               replace(T, I, F, S),
                               !.

/*Sotituisce l'elemento in una determinata posizione*/
replace_pos([_|T], 0, X, [X|T]) :- !.
replace_pos([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace_pos(T, NI, X, R), !.
replace_pos(L, _, _, L) :- !.


/*Traduce le etichette in indirizzi*/
/*Trova la posizione dell'etichetta nel codice*/
find_pos(OG, [H|_], LBL, ADDR) :- split_string(H, " ", " ", [X|_]),
                                  X = LBL,
								  nth0(ADDR2, OG, H),
								  string_to_atom(ADDR2, ADDR),
                                  !.

find_pos(OG, [H|T], LBL, ADDR) :- split_string(H, " ", " ", [X|_]),
                                  X \= LBL,
                                  find_pos(OG, T, LBL, ADDR),
                                  !.

/*Legge le linee da un file*/
readLines(File, L)	:- open(File, read, Stream),
				       read_stream_to_codes(Stream, Codes),
				       close(Stream),
				       atom_codes(String, Codes),
				       string_upper(String, Res),
				       split_string(Res, "\n", "\n", L).

/*Verifica se il valore e' una etichetta valida*/
validLabel(F) :- string_chars(F, [C|_]), char_type(C, alpha), is_op(F, M), 
				 M = 0, !.
/*Verifica se l'argomento e' valido*/
validArg(F) :- number_string(_, F), !.
validArg(F) :- validLabel(F), !.

/*Verifica che una di input sia corretto*/
/*Caso INST*/
validLine(Line) :- split_string(Line, " ", " ", [X]), X = "DAT", !.
validLine(Line) :- split_string(Line, " ", " ", [X]), X = "INP", !.
validLine(Line) :- split_string(Line, " ", " ", [X]), X = "OUT", !.
validLine(Line) :- split_string(Line, " ", " ", [X]), X = "HLT", !.
/*CASO LBL INST o INST LBL/VAL*/
validLine(Line) :- split_string(Line, " ", " ", [X, Y]), X = "DAT", 
				   number_string(_, Y), !.
validLine(Line) :- split_string(Line, " ", " ", [X, Y]), oP(X, CODE),
				   validArg(Y), number(CODE), between(1, 8, CODE), !.
validLine(Line) :- split_string(Line, " ", " ", [X, Y]), Y = "INP",
				   validLabel(X), !.
validLine(Line) :- split_string(Line, " ", " ", [X, Y]), Y = "DAT",
				   validLabel(X), !.
validLine(Line) :- split_string(Line, " ", " ", [X, Y]), Y = "OUT",
				   validLabel(X), !.
validLine(Line) :- split_string(Line, " ", " ", [X, Y]), Y = "HLT",
				   validLabel(X), !.
/*CASO LBL INST LBL/VAL*/
validLine(Line) :- split_string(Line, " ", " ", [X, Y, Z]), 
				   validLabel(X), oP(Y, CODE), validArg(Z), numb(CODE, N),
				   number(N), between(1, 8, N), !.
validLine(Line) :- split_string(Line, " ", " ", [X, Y, Z]), 
				   validLabel(X), oP(Y, CODE), number_string(_, Z), CODE = '', !.

/*Verifica che un file sia corretto*/
validLines([Line|Lines]) :- validLine(Line), validLines(Lines), !.
validLines([]) :- !.

/*EXECUTION LOOP*/
process_loop(State, Out) :- functor(State, halted_state, _),
							  arg(5, State, Out), !.

process_loop(state(Acc, Pc, Mem, In, Out, Flag), F) :-
				process_instruction(state(Acc, Pc, Mem, In, Out, Flag), NextState),
				process_loop(NextState, F).

/**********************************DEBUG*****************************************************
dbprocess_loop(State, State) :- functor(State, halted_state, 6).				 			*
																				 			*
dbprocess_loop(state(Acc, Pc, Mem, In, Out, Flag), F) :-						 			*
				process_instruction(state(Acc, Pc, Mem, In, Out, Flag), NextState),	 			*
				process_loop(NextState, F).								     			*
****************************************************************************^****************/
db_parse_file(File, MEM) :- readLines(File, L), purge_comms(L, L2), parseLines(L2, L2, MEM).

/*Without aps*/
/*FILL*/
fill(N, E, Xs) :-
	length(Xs, N),       % Xs is a list of length N
	maplist(=(E), Xs).   % all elements in Xs are equal to E

/*Fill it*/
fill_it(Mem, NewMem) :- length(Mem, L), 
						LE is 100-L,
						fill(LE, '0', B),
						append(Mem, B, NewMem).

/*To regular out*/
converto_to_reg([], []) :- !.
converto_to_reg([X], [T]) :- atom_number(X, T), !.
converto_to_reg([X|T], [X1|T1]) :- atom_number(X, X1), converto_to_reg(T, T1), !.

converto_to_reg([X|T], [X1|T1]) :-  number(X1), atom_number(X, X1), atom(X), converto_to_reg(T, T1), !.

lmc_load(File, Mem__) :-	readLines(File, L), 
						purge_comms(L, L2),
						validLines(L2), 
						parseLines(L2, L2, Mem), 
						fill_it(Mem, Mem_),
						converto_to_reg(Mem_, Mem__),
						validMem(Mem__),
						!.


lmc_run(File, Input, Out) :- lmc_load(File, Mem),
						converto_to_reg(Mem_, Mem),
						process_loop(state(0, 0, Mem_, Input, [], noflag), Out),
						!.

one_instruction(
	state(Acc, Pc, Mem, In, Out, Flag), Outt) :- converto_to_reg(Mem_, Mem),
			process_instruction(state(Acc, Pc, Mem_, In, Out, Flag), Outt), !.

execution_loop(
	state(Acc, Pc, Mem, In, Out, Flag), Outt) :- converto_to_reg(Mem_, Mem),
					process_loop(state(Acc, Pc, Mem_, In, Out, Flag), Outt), !.


/*ADD*/
process_instruction(state(Acc, Pc, Mem, In, Out, _), State) :-
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '1',
				nth0(ARG, Mem, Acc_),
				atom_number(Acc_, Acc___),
				Pc_ is (Pc+1) mod  100,
				normalize(Acc + Acc___, Acc__, Flag_),
				copy_term(state(Acc__, Pc_, Mem, In, Out, Flag_), State),
				!.

/*SUB*/
process_instruction(state(Acc, Pc, Mem, In, Out, _), State) :-
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '2',
				nth0(ARG, Mem, Acc_),
				atom_number(Acc_, Acc___),
				Pc_ is (Pc + 1) mod  100,
				normalize(Acc - Acc___, Acc__, Flag_),
				copy_term(state(Acc__, Pc_, Mem, In, Out, Flag_), State),
				%Pc_ \= 0,
				!.

/*STORE*/
process_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :-
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				atom_number(Acc_, Acc),
				OP = '3',
				Pc_ is (Pc + 1) mod  100,
				replace_pos(Mem, ARG, Acc_, Mem_),
				copy_term(state(Acc, Pc_, Mem_, In, Out, Flag), State),
				%Pc_ \= 0,
				!.

/*LOAD*/
process_instruction(state(_, Pc, Mem, In, Out, Flag), State) :-
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '5',
				Pc_ is (Pc + 1) mod  100,
				nth0(ARG, Mem, Acc_),
				atom_number(Acc_, Acc__),
				copy_term(state(Acc__, Pc_, Mem, In, Out, Flag), State),
				%Pc_ \= 0,
				!.

/*BRANCH*/
process_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :-
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '6',
				Pc_ is (ARG) mod  100,
				copy_term(state(Acc, Pc_, Mem, In, Out, Flag), State),
				!.

/*BRANCHIZ*/
/*TRUE*/
process_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :-
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '7',
				Acc = 0,
				Flag = noflag,
				Pc_ is (ARG) mod 100,
				copy_term(state(Acc, Pc_, Mem, In, Out, Flag), State),
				!.
/*FALSE*/
process_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :-
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, _),
				OP = '7',
				Pc_ is (Pc + 1) mod  100,
				copy_term(state(Acc, Pc_, Mem, In, Out, Flag), State),
				!.

/*BRANCHIP*/

/*TRUE*/
process_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :-
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, ARG),
				OP = '8',
				Flag = noflag,
				Pc_ is (ARG) mod  100,
				copy_term(state(Acc, Pc_, Mem, In, Out, Flag), State),
				!.
/*FALSE*/
process_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :-
				nth0(Pc, Mem, INST),
				split_instr(INST, OP, _),
				OP = '8',
				Flag \= noflag,
				Pc_ is (Pc + 1) mod  100,
				copy_term(state(Acc, Pc_, Mem, In, Out, Flag), State),
				!.

/*INPUT*/
process_instruction(state(_, Pc, Mem, [ToIn|In], Out, Flag), State) :-
				nth0(Pc, Mem, INST),
				string_chars(INST, [X, Y, Z|_]),
				X = '9', Y = '0', Z = '1',
				[ToIn|In] \= [],
				number(ToIn),
				Acc_ is (ToIn),
				Pc_ is (Pc + 1) mod  100,
				copy_term(state(Acc_, Pc_, Mem, In, Out, Flag), State),
				%Pc_ \= 0,
				!.

/*OUTPUT*/
process_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :-
				nth0(Pc, Mem, INST),
				string_chars(INST, [X, Y, Z|_]),
				X = '9', Y = '0', Z = '2',
				Pc_ is (Pc + 1) mod  100,
				append(Out, [Acc], Out_),
				copy_term(state(Acc, Pc_, Mem, In, Out_, Flag), State),
				%Pc_ \= 0,
				!.

/*HALT*/
process_instruction(state(Acc, Pc, Mem, In, Out, Flag), State) :-
				nth0(Pc, Mem, INST),
				atom_chars(INST, [X]), X = '0',
				copy_term(halted_state(Acc, Pc, Mem, In, Out, Flag), State),
				!.
