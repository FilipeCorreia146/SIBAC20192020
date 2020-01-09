formata_msg(Conc, Evid, Res):-
	conc('Chosen game: ', Conc, Conc1),
	conc(Conc1,'~M~J~M~JEvidences:~M~J',Conc2),
	junta(Evid, Evid1),
	conc(Conc2,Evid1,Res).

junta([H],H):-!.
junta([H|T],Atom):-
	junta(T, Atom1),
	conc(Atom1,'~M~J',Atom2),
	conc(Atom2,H,Atom).

conc(A1,A2,A3):-
	name(A1,L1),
	name(A2,L2),
	append(L1,L2,L3),
	name(A3,L3).

lista_evidencias(L):-
	findall(Evid, (isa_slot(Atrib, global, Val), isa_question(Atrib,[Q],_,_), conc(Q,' ',Evid1), conc(Evid1,Val,Evid) ), L).


