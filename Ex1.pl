:- use_module(library(lists)).

%participant(Id,Age,Performance)
participant(1234, 17, 'Pé coxinho').
participant(3423, 21, 'Programar com os pés').
participant(3788, 20, 'Sing a Bit').
participant(4865, 22, 'Pontes de esparguete').
participant(8937, 19, 'Pontes de pen-drives').
participant(2564, 20, 'Moodle hack').

%performance(Id,Times)
performance(1234,[120,120,120,120]).
performance(3423,[32,120,45,120]).
performance(3788,[110,2,6,43]).
performance(4865,[120,120,110,120]).
performance(8937,[97,101,105,110]).


%madeItThrough(+Participant)
madeItThrough(X):-
	performance(X,Times),
	\+ checkTimes(Times).

%checkTimes(+Time)
checkTimes([]).
checkTimes([X|T]):-
	X =\=120,
	checkTimes(T).
	
%juriTimes(+Participants, +JuriMember, -Times, -Total)
juriTimes(Participants,JuriMember,Times,Total):-
	iteratePartcipant(Participants,JuriMember,Times,[]),
	sumTimes(Times,Total).
	
%iteratePartcipant(-Participants,-JuriMember,+Times,+TimesTemp)		
iteratePartcipant([],_,Times,TimesTemp):-
	append([],TimesTemp,Times).
iteratePartcipant([H|T],JuriMember,Times,TimesTemp):-
	performance(H,ParticipantTimes),
	nth1(JuriMember,ParticipantTimes,Time),
	append(TimesTemp,[Time],TimesTemp2),
	iteratePartcipant(T,JuriMember,Times,TimesTemp2).

%sumTimes(+Times,+Total)
sumTimes([H],Total):-
	Total is H.
sumTimes([H1,H2|T],Total):-
	H3 is H1+H2,
	sumTimes([H3|T],Total).
	
%patientJuri(+JuriMember)
patientJuri(JuriMember):-
	performance(P1,Times1),
	performance(P2,Times2),
	P1 \= P2,
	write(Times1),nl,
	write(Times2),
	nth1(JuriMember,Times1,120),
	nth1(JuriMember,Times2,120).
	
%bestParticipant(+P1, +P2, -P)
bestParticipant(P1,P2,P):-
	performance(P1,Times1),
	performance(P2,Times2),
	sumTimes(Times1,Sum1),
	sumTimes(Times2,Sum2),
	Sum1 > Sum2,
	P is P1.
bestParticipant(P1,P2,P):-
	performance(P1,Times1),
	performance(P2,Times2),
	sumTimes(Times1,Sum1),
	sumTimes(Times2,Sum2),
	Sum1 < Sum2,
	P is P2.

%allPrefs
allPerfs :-
	participant(Id,_,Performance),
	performance(Id,Times),
	format("~w:~w:~w~N",[Id,Performance,Times]),
	fail.
allPerfs.

%nSuccessfulParticipants(-T)
successfulParticipant(Participant) :-
	performance(Participant,Times),
	\+ (member(Time,Times),
	Time \= 120).

nSuccessfulParticipants(T) :-
	findall(Participant,successfulParticipant(Participant),List),
	length(List,T).

juriFans(JuriFansList):-
	findall(Participant-Fans,(performance(Participant,Times),findall(J,nth1(J,Times,120),Fans)),JuriFansList).

%nextPhase(N, Participants)
nextPhase(N, Participants) :-
	setof(TT-Id-Perf,eligibleOutcome(Id,Perf,TT),Results),
	reverse(Results,Ordered),
	sublist(Ordered,Participants,0,N,_).

predX(Q,[R|Rs],[P|Ps]) :-
	participant(R,I,P), I=<Q, !,
	predX(Q,Rs,Ps).
predX(Q,[R|Rs],Ps) :-
	participant(R,I,_), I>Q,
	predX(Q,Rs,Ps).
predX(_,[],[]).