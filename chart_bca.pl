:- dynamic arc/5.

% Initial state
inits(ph).

% Rule definitions
r(ph, [s, conj, s]).
r(ph, [s]).

r(s, [np, vp]).
r(s, [adv, vp]).

r(np, [np2, conj, np2]).
r(np, [np2]).

r(np2, [prep, ap]).
r(np2, [art, ap]).
r(np2, [n, atp]).
r(np2, [n]).
r(np2, [prn]).

r(ap, [adj, np2]).
r(ap, [np2]).

r(atp, [atp2, conj, atp2]).
r(atp, [atp2]).

r(atp2, [art, n]).
r(atp2, [prep, atp3]).
r(atp2, [atp3]).

r(atp3, [n, prep, adj]).
r(atp3, [n, adj]).
r(atp3, [adj, n]).
r(atp3, [n]).
r(atp3, [adj]).

r(vp, [vcpn, conj, s]).
r(vp, [v, np2]).
r(vp, [v, pp]).
r(vp, [vcpn, pastverb]).
r(vp, [vcpn, np]).
r(vp, [prefl, vp]).
r(vp, [v, pconj]).
r(vp, [v]).

r(pconj, [adv, prefl, vp]).
r(pastverb, [prep, vpast]).

r(pp, [cd, ct]).
r(cd, [np2]).
r(ct, [adv, advp]).

r(advp, [advp2, conj, advp2]).
r(advp2, [prep, adv]).
r(advp2, [adv]).

r(vcpn, [vcp, adj]).
r(vcpn, [vcp, adv]).
r(vcpn, [vcp, v]).

w(prn, mie).
w(prn, mine).
w(prn, ea).

% Verbs
w(v, observa).
w(v, combina).
w(v, consta).
w(v, place).
w(v, incerc).
w(v, bucur).

% Verb complements
w(vcp, este).
w(vcp, poate).
w(vcp, sunt).

% Past verbs
w(vpast, observat).

% Nouns
w(n, oamenii).
w(n, culorile).
w(n, zile).
w(n, nuante).
w(n, multitudine).
w(n, tonuri).
w(n, ora).
w(n, mii).
w(n, culori).
w(n, galbenuri).
w(n, albastru).
w(n, intunecimi).
w(n, cerul).
w(n, culoare).
w(n, spectrul).

% Prefixes
w(prefl, imi).
w(prefl, ma).

% Prepositions
w(prep, la).
w(prep, pentru).
w(prep, de).
w(prep, in).
w(prep, din).

% Articles
w(art, o).
w(art, unei).

% Conjunctions
w(conj, si).
w(conj, ',').
w(conj, ca).
w(conj, dar).

% Adjectives
w(adj, singura).
w(adj, diferite).
w(adj, ceruite).
w(adj, importante).
w(adj, ciocolatiu).
w(adj, fiecare).
w(adj, intreg).
w(adj, nepatruns).

% Adverbs
w(adv, numai).
w(adv, sfarsit).
w(adv, inceput).
w(adv, clar).
w(adv, totusi).
w(adv, sa).

test(S):-V0 is 1,inits(Sym),parse(V0,Vn,S),
	dofor(arc(V0,Vn,Sym,[],As),mwrite(As)).
parse(V0,Vn,S):-init_chart(V0,Vn,S),inits(Sym),active(V0,Sym).
init_chart(V,V,[]).
init_chart(V0,Vn,[W|Rw]):-V1 is V0+1,dofor(w(N,W),add_arc(V0,V1,N,[],[W,N])),
			init_chart(V1,Vn,Rw).
active(V,LN):-dofor(r(LN,RN),add_arc(V,V,LN,RN,[LN])).
add_arc(V1, V2, Ln, RRn, Ap):- arc(V1, V2, Ln, RRn, Ap),!.  
add_arc(V1, V2, Ln, [], Aln):-
	asserta(arc(V1, V2, Ln, [], Aln)),
	dofor(arc(V0, V1, NLn, [Ln|RRn], ANLn),
		add_arc(V0, V2, NLn, RRn, [Aln|ANLn])).
add_arc(V1, V2, Ln, [FRn|RRn], Aln):-
	asserta(arc(V1, V2, Ln, [FRn|RRn], Aln)),
	dofor(arc(V2, V3, FRn, [], AFRn),
		add_arc(V1, V3, Ln, RRn, [AFRn|Aln])),
	active(V2, FRn).

dofor(X, Y):- X, do(Y), fail.
dofor(X, Y).
do(Y):- Y, !.

mwrite(Arb):- invers(Arb, Ia), write(Ia), nl.

invers([], []):- !.
invers(A, A):- atomic(A).
invers([H|T], Ia):- invers(H, Ih), invers(T, It), append(It, [Ih], Ia).

append([], L, L).
append([H|T], L, [H|S]):- append(T, L, S).

retract2:-retract(arc(A,B,C,D,E)), %X = arc(A,B,C,D,E), write(X),nl,
		retract2.
retract2.

test(S,As):-S=[oamenii, observa, culorile, unei, zile, numai, la, sfarsit, si, la, inceput, dar, pentru, mine, este, clar, ca, ea, combina, o, multitudine, de, nuante, si, tonuri],
		V0 is 1,inits(Sym),parse(V0,Vn,S),arc(V0,Vn,Sym,[],As),nl,retract2.

test1(S,As):-S=[o, singura, ora, poate, consta, in, mii, de, culori, diferite],
		V0 is 1,inits(Sym),parse(V0,Vn,S),arc(V0,Vn,Sym,[],As),nl,retract2.

test2(S,As):-S=[galbenuri, ceruite, ',', nuante, de, albastru, si, intunecimi, de, nepatruns, sunt, importante, de, observat],
		V0 is 1,inits(Sym),parse(V0,Vn,S),arc(V0,Vn,Sym,[],As),nl,retract2.

test3(S,As):-S=[mie, imi, place, cerul, ciocolatiu],
		V0 is 1,inits(Sym),parse(V0,Vn,S),arc(V0,Vn,Sym,[],As),nl,retract2.

test4(S,As):-S=[totusi, incerc, sa, ma, bucur, de, fiecare, culoare, din, intreg, spectrul],
		V0 is 1,inits(Sym),parse(V0,Vn,S),arc(V0,Vn,Sym,[],As),nl,retract2.

	
test_all:-tell('chart-td-time-.doc'),
            test(E,A), nl,write([E,A]),

            test1(E1,A1), nl,write([E1,A1]),

			test2(E2,A2), nl,write([E2,A2]),

            test3(E3,A3), nl,write([E3,A3]),

			test4(E4,A4), nl,write([E4,A4]).   

write_l([]):-!.
write_l([H|T]):-write(H),nl, write_l(T).

test_time:-
        statistics(walltime, [Start,_]),
        test_all,
        statistics(walltime, [End,_]),
        Time is End - Start,
        nl,retract2,nl,
        format('all solutions in ~3d seconds.~n', [Time])
                              ,told.