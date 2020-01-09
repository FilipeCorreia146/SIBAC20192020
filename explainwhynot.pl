:-dynamic conclusion/4, message/1.
:-new_question(q_conclusion,['Select the desired conclusion(game):'],single(g_conclusions),none).

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
	message(M),msgbox('Explanation',M,0,_).

explainWhyNot([],_,_):-!.
explainWhyNot(_,new_value(C,V),Ident):-
	isa_slot(C,global,V),
	(tab(Ident),write('The conclusion is true'),nl)~>Msg,appendMessage(Msg).
explainWhyNot([R|LR],new_value(C,V),Ident):-
	isa_rule(R,LHS,_,_,_),
	(tab(Ident),write('The rule '),write(R),write(', that would make it possible to conclude "'),write(C),write(': '),write(V),write('", did not trigger because:'),nl)~>Msg,appendMessage(Msg),
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
	(tab(Ident),write('The evidence "'),write(C),write(': '),write(V),write('" was not observed'),nl)~>Msg,appendMessage(Msg).
checkCondition(equality(C,V),Ident):-
	not isa_slot(C,global,V),!,
	(tab(Ident),write('The conclusion "'),write(C),write(': '),write(V),write('" is not true'),nl)~>Msg,appendMessage(Msg),
	findall(R,conclusion(R,C,V,_),LR),
	Ident1 is Ident+2,
	explainWhyNot(LR,new_value(C,V),Ident1).
checkCondition(_,_).

appendMessage(Msg2):-
	retract(message(Msg1)), (write(Msg1),write(Msg2))~>Msg3, assert(message(Msg3)).

