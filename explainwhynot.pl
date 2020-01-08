:-dynamic conclusion/4, message/1.
:-new_question(q_conclusion,['Seleccione a conclusão que esperava obter:'],single(g_conclusions),none).

getConclusions(LConc):-
	setof(Str,(R,LHS,RHS,Exp,S,C)^(isa_rule(R,LHS,RHS,Exp,S),getC(R,RHS,C,Str)),LConc).

getC(R,new_value(Conc,Value),(Conc,Value),Str):-not isa_slot(Conc,global,Value),(write(Conc),write(': '),write(Value))~>Str,assert(conclusion(R,Conc,Value,Str)).
getC(R,(new_value(Conc,Value),_),(Conc,Value),Str):-not isa_slot(Conc,global,Value),(write(Conc),write(': '),write(Value))~>Str,assert(conclusion(R,Conc,Value,Str)).
getC(R,(_,RC),C,Str):-
	getC(R,RC,C,Str).

explainWhyNot:-
	retractall(conclusion(_,_,_,_)),retractall(message(_)),assert(message(``)),
	getConclusions(LStr),
	new_group(g_conclusions,LStr),
	ask(q_conclusion,ExpectedConc),
	write(ExpectedConc)~>ExpectedConcStr,findall(R,conclusion(R,C,V,ExpectedConcStr),LR),
	conclusion(_,C,V,ExpectedConcStr),
	explainWhyNot(LR,new_value(C,V),0),
	message(M),msgbox('Explicação',M,0,_).

explainWhyNot([],_,_):-!.
explainWhyNot(_,new_value(C,V),Ident):-
	isa_slot(C,global,V),
	(tab(Ident),write('A conclusão é verdadeira!'),nl)~>Msg,appendMessage(Msg).
explainWhyNot([R|LR],new_value(C,V),Ident):-
	isa_rule(R,LHS,_,_,_),
	(tab(Ident),write('A regra '),write(R),write(', que permitiria concluir "'),write(C),write(': '),write(V),write('", não disparou porque:'),nl)~>Msg,appendMessage(Msg),
	Ident1 is Ident+2,
	showFalseConditions(LHS,Ident1),
	explainWhyNot(LR,new_value(C,V),Ident).
	
showFalseConditions(C,Ident):-functor(C,equality,2),
	checkCondition(C,Ident).
showFalseConditions((C1,C2),Ident):-	
	checkCondition(C1,Ident),showFalseConditions(C2,Ident).
showFalseConditions((C1;C2),Ident):-
	checkCondition(C1,Ident),showFalseConditions(C2,Ident).
showFalseConditions(_,_).

checkCondition(equality(C,V),Ident):-
	isa_question(C,_,_,_),
	not isa_slot(C,global,V),!,
	(tab(Ident),write('A evidência "'),write(C),write(': '),write(V),write('" não foi observada'),nl)~>Msg,appendMessage(Msg).
checkCondition(equality(C,V),Ident):-
	not isa_slot(C,global,V),!,
	(tab(Ident),write('A conclusão "'),write(C),write(': '),write(V),write('" não é verdadeira'),nl)~>Msg,appendMessage(Msg),
	findall(R,conclusion(R,C,V,_),LR),
	Ident1 is Ident+2,
	explainWhyNot(LR,new_value(C,V),Ident1).
checkCondition(_,_).

appendMessage(Msg2):-
	retract(message(Msg1)), (write(Msg1),write(Msg2))~>Msg3, assert(message(Msg3)).

