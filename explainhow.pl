:-dynamic message/1.

explainHow(RuleList):-
	retractall(message(_)),assert(message(``)),
	explainHow1(RuleList),
	message(Exp),
	msgbox('Explicação',Exp,0,_).

explainHow1([]).
explainHow1([R|RuleList]):-
	explainHow1(RuleList),
	explainRule(R).

explainRule(R):-
	isa_rule(R,LHS,RHS,_,_),
	(write('A regra '), write(R), write(' disparou devido aos seguintes factos:'), nl) ~> Str1, appendMessage(Str1),
	checkLHS(LHS),
	(write('Permitindo obter a conclusão:'), nl) ~> Str2, appendMessage(Str2),
	showRHS(RHS).

checkLHS(C):-
	checkCondition(C).
checkLHS((C,RC)):-
	checkCondition(C),
	checkLHS(RC).
checkLHS((C;RC)):-
	(checkCondition(C),!);checkLHS(RC).

checkCondition(equality(Atrib,Value)):-
	isa_question(Atrib,_,_,_),!,
	isa_slot(Atrib,global,Value),
	(tab(2), write('Observação da evidência '), write(Atrib), write(': '), write(Value), nl)~> Str, appendMessage(Str).
checkCondition(equality(Atrib,Value)):-
	isa_slot(Atrib,global,Value),
	(tab(2), write('Conclusão intermédia '), write(Atrib), write(': '), write(Value), nl)~> Str, appendMessage(Str).


showRHS(new_value(C,V)):-!,
	(write(C), write(': '), write(V), nl) ~> Str, appendMessage(Str).
showRHS((new_value(C,V),RC)):-!,
	(write(C), write(': '), write(V), nl) ~> Str, appendMessage(Str),
	showRHS(RC).
showRHS(_).

appendMessage( Msg2 ) :-
   retract( message(Msg1) ),
   (  write( Msg1 ),
      write( Msg2 )
   )  ~> Msg3,
   assert( message(Msg3) ).


