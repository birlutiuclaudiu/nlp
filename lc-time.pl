% Leaf Lexicon
leaf(n) --> [oamenii].
leaf(n) --> [culorile].
leaf(n) --> [zile].
leaf(n) --> [nuante].
leaf(n) --> [multitudine].
leaf(n) --> [tonuri].
leaf(n) --> [ora].
leaf(n) --> [mii].
leaf(n) --> [culori].
leaf(n) --> [galbenuri].
leaf(n) --> [albastru].
leaf(n) --> [intunecimi].
leaf(n) --> [cerul].
leaf(n) --> [culoare].
leaf(n) --> [spectrul].

leaf(prn) --> [mie].
leaf(prn) --> [mine].
leaf(prn) --> [ea].

leaf(prefl) --> [imi].
leaf(prefl) --> [ma].

leaf(v) --> [observa].
leaf(v) --> [combina].
leaf(v) --> [consta].
leaf(v) --> [place].
leaf(v) --> [incerc].
leaf(v) --> [bucur].

leaf(vcp) --> [este].
leaf(vcp) --> [poate].
leaf(vcp) --> [sunt].

leaf(vpast) --> [observat].

leaf(art) --> [unei].
leaf(art) --> [o].

leaf(adv) --> [numai].
leaf(adv) --> [sfarsit].
leaf(adv) --> [inceput].
leaf(adv) --> [clar].
leaf(adv) --> [totusi].
leaf(adv) --> [sa].

leaf(adj) --> [singura].
leaf(adj) --> [diferite].
leaf(adj) --> [ceruite].
leaf(adj) --> [nepatruns].
leaf(adj) --> [importante].
leaf(adj) --> [ciocolatiu].
leaf(adj) --> [fiecare].
leaf(adj) --> [intreg].

leaf(prep) --> [la].
leaf(prep) --> [pentru].
leaf(prep) --> [de].
leaf(prep) --> [in].
leaf(prep) --> [din].

leaf(conj) --> [si].
leaf(conj) --> [dar].
leaf(conj) --> [ca].
leaf(conj) --> [','].

lcd(s, ph) :- !.
lcd(np, s) :- !.
lcd(adv, s) :- !.
lcd(np2, np) :- !.
lcd(art, np2) :- !.
lcd(prep, np2) :- !.
lcd(n, np2) :- !.
lcd(prn, np2) :- !.
lcd(adj, ap) :- !.
lcd(np2, ap) :- !.
lcd(atp2, atp) :- !.
lcd(art, atp2) :- !.
lcd(prep, atp2) :- !.
lcd(atp3, atp2) :- !.
lcd(n, atp3) :- !.
lcd(adj, atp3) :- !.
lcd(vcpn, vp) :- !.
lcd(v, vp) :- !.
lcd(prefl, vp) :- !.
lcd(adv, vp) :- !.
lcd(pconj, vp) :- !.
lcd(prep, pastverb) :- !.
lcd(cd, pp) :- !.
lcd(np2, cd) :- !.
lcd(adv, ct) :- !.
lcd(advp2, advp) :- !.
lcd(prep, advp2) :- !.
lcd(adv, advp2) :- !.
lcd(vcp, vcpn) :- !.
lcd(v, vcpn) :- !.
lcd(adj, vcpn) :- !.

lc(X,X):-!.
lc(X,Y):-lcd(X,Y),!.
lc(X,Y):-lcd(X,Z),lc(Z,Y).

% Parse rules
parse(Nterm,As,[W0|W1],Wn):-
     leaf(Pterm,[W0|W1],W1),
     lc(Pterm,Nterm),
     Ap=..[Pterm,W0], write(Ap), nl,
     P=..[Pterm,Nterm,Ap,As,W1,Wn],
     call(P).

% Phrase parsing rules
ph(ph, A, A) --> [].
s(Nt, S, As) --> {lc(ph, Nt)}, ph(Nt, ph(S), As).
s(Nt, S1, As) --> {lc(ph, Nt)}, parse(conj, C), parse(s, S2), ph(Nt, ph(S1, C, S2), As).

% Sentence (s)
s(s, A, A) --> [].
np(Nt, NP, As) --> {lc(s, Nt)}, parse(vp, VP), s(Nt, s(NP, VP), As).
adv(Nt, ADV, As) --> {lc(s, Nt)}, parse(vp, VP), s(Nt, s(ADV, VP), As).

% Noun phrase (np)
np(np, A, A) --> [].
np2(Nt, N1, As) --> {lc(np, Nt)}, parse(conj, C), parse(np2, N2), np(Nt, np(N1, C, N2), As).
np2(Nt, N, As) --> {lc(np, Nt)}, np(Nt, np(N), As).
np2(np2, A, A) --> [].
prep(Nt, PREP, As) --> {lc(np2, Nt)}, parse(ap, N), np2(Nt, np2(PREP, N), As).
art(Nt, ART, As) --> {lc(np2, Nt)}, parse(ap, N), np2(Nt, np2(ART, N), As).
n(Nt, N, As) --> {lc(np2, Nt)}, parse(atp, ATN), np2(Nt, np2(N, ATN), As).
n(Nt, N, As) --> {lc(np2, Nt)}, np2(Nt, np2(N), As).
prn(Nt, N, As) --> {lc(np2, Nt)}, np2(Nt, np2(N), As).

% Adjective phrase (ap)
ap(ap, A, A) --> [].
adj(Nt, ADJ, As) --> {lc(ap, Nt)}, parse(np2, N), ap(Nt, ap(ADJ, N), As).
np2(Nt, N, As) --> {lc(ap, Nt)}, ap(Nt, ap(N), As).

% Attributive phrase (atp)
atp(atp, A, A) --> [].
atp2(Nt, N1, As) --> {lc(atp, Nt)}, parse(conj, C), parse(atp2, N2), atp(Nt, atp(N1, C, N2), As).
atp2(Nt, N, As) --> {lc(atp, Nt)}, atp(Nt, atp(N), As).
atp2(atp2, A, A) --> [].
art(Nt, ART, As) --> {lc(atp2, Nt)}, parse(n, N), atp2(Nt, atp2(ART, N), As).
prep(Nt, PREP, As) --> {lc(atp2, Nt)}, parse(atp3, N), atp2(Nt, atp2(PREP, N), As).
atp3(Nt, N, As) --> {lc(atp2, Nt)}, atp2(Nt, atp2(N), As).
atp3(atp3, A, A) --> [].
n(Nt, N, As) --> {lc(atp3, Nt)}, parse(prep, PREP), parse(adj, ADJ), atp3(Nt, atp3(N, PREP, ADJ), As).
n(Nt, N, As) --> {lc(atp3, Nt)}, parse(adj, ADJ), atp3(Nt, atp3(N, ADJ), As).
adj(Nt, ADJ, As) --> {lc(atp3, Nt)}, parse(n, N), atp3(Nt, atp3(ADJ, N), As).
n(Nt, N, As) --> {lc(atp3, Nt)}, atp3(Nt, atp3(N), As).
adj(Nt, ADJ, As) --> {lc(atp3, Nt)}, atp3(Nt, atp3(ADJ), As).

% Verb phrase (vp)
vp(vp, A, A) --> [].
vcpn(Nt, V, As) --> {lc(vp, Nt)}, parse(conj, C), parse(s, PSEC), vp(Nt, vp(V, C, PSEC), As).
v(Nt, V, As) --> {lc(vp, Nt)}, parse(np2, C), vp(Nt, vp(V, C), As).
v(Nt, V, As) --> {lc(vp, Nt)}, parse(pp, C), vp(Nt, vp(V, C), As).
vcpn(Nt, V, As) --> {lc(vp, Nt)}, parse(pastverb, N), vp(Nt, vp(V, N), As).
vcpn(Nt, V, As) --> {lc(vp, Nt)}, parse(np, N), vp(Nt, vp(V, N), As).
prefl(Nt, PREFL, As) --> {lc(vp, Nt)}, parse(vp, V), vp(Nt, vp(PREFL, V), As).
v(Nt, V, As) --> {lc(vp, Nt)}, parse(pconj, PCONJ), vp(Nt, vp(V, PCONJ), As).
v(Nt, V, As) --> {lc(vp, Nt)}, vp(Nt, vp(V), As).

% Conjunction phrase (pconj)
pconj(pconj, A, A) --> [].
adv(Nt, ADV, As) --> {lc(pconj, Nt)}, parse(prefl, PREFL), parse(vp, V), pconj(Nt, pconj(ADV, PREFL, V), As).

% Past verb phrase (pastverb)
pastverb(pastverb, A, A) --> [].
prep(Nt, PREP, As) --> {lc(pastverb, Nt)}, parse(vpast, PART), pastverb(Nt, pastverb(PREP, PART), As).

% Prepositional phrase (pp)
pp(pp, A, A) --> [].
cd(Nt, CD, As) --> {lc(pp, Nt)}, parse(ct, CT), pp(Nt, pp(CD, CT), As).

% Clause type (cd and ct)
cd(cd, A, A) --> [].
np2(Nt, N, As) --> {lc(cd, Nt)}, cd(Nt, cd(N), As).

ct(ct, A, A) --> [].
adv(Nt, ADV, As) --> {lc(ct, Nt)}, parse(advp, ADVPP), ct(Nt, ct(ADV, ADVPP), As).

% Adverbial phrase (advp)
advp(advp, A, A) --> [].
advp2(Nt, ADV1, As) --> {lc(advp, Nt)}, parse(conj, C), parse(advp2, ADV2), advp(Nt, advp(ADV1, C, ADV2), As).
advp2(advp2, A, A) --> [].
prep(Nt, PREP, As) --> {lc(advp2, Nt)}, parse(adv, ADV), advp2(Nt, advp2(PREP, ADV), As).
adv(Nt, ADV, As) --> {lc(advp2, Nt)}, advp2(Nt, advp2(ADV), As).

% Verb copula (vcpn)
vcpn(vcpn, A, A) --> [].
vcp(Nt, V, As) --> {lc(vcpn, Nt)}, parse(adj, ADJ), vcpn(Nt, vcpn(V, ADJ), As).
vcp(Nt, V, As) --> {lc(vcpn, Nt)}, parse(adv, ADV), vcpn(Nt, vcpn(V, ADV), As).
vcp(Nt, V1, As) --> {lc(vcpn, Nt)}, parse(v, V2), vcpn(Nt, vcpn(V1, V2), As).

% Leaf parsing stubs
n(n, A, A) --> [].
prn(prn, A, A) --> [].
prefl(prefl, A, A) --> [].
v(v, A, A) --> [].
vcp(vcp, A, A) --> [].
prep(prep, A, A) --> [].
adj(adj, A, A) --> [].
conj(conj, A, A) --> [].
art(art, A, A) --> [].
adv(adv, A, A) --> [].
vpast(vpast, A, A) --> [].

test(_, _, 10) :- !.
test(S, A, I) :-
    S = [oamenii, observa, culorile, unei, zile, numai, la, sfarsit, si, la, inceput, dar, pentru, mine, este, clar, ca, ea, combina, o, multitudine, de, nuante, si, tonuri],
    parse(ph, A, S, []),
    I1 is I + 1,
    test(S, A, I1).

test1(_, _, 10) :- !.
test1(S, A, I) :-
    S = [o, singura, ora, poate, consta, in, mii, de, culori, diferite],
    parse(ph, A, S, []),
    I1 is I + 1,
    test1(S, A, I1).

test2(_, _, 10) :- !.
test2(S, A, I) :-
    S = [galbenuri, ceruite, ',', nuante, de, albastru, si, intunecimi, de, nepatruns, sunt, importante, de, observat],
    parse(ph, A, S, []),
    I1 is I + 1,
    test2(S, A, I1).

test3(_, _, 10) :- !.
test3(S, A, I) :-
    S = [mie, imi, place, cerul, ciocolatiu],
    parse(ph, A, S, []),
    I1 is I + 1,
    test3(S, A, I1).

test4(_, _, 10) :- !.
test4(S, A, I) :-
    S = [totusi, incerc, sa, ma, bucur, de, fiecare, culoare, din, intreg, spectrul],
    parse(ph, A, S, []),
    I1 is I + 1,
    test4(S, A, I1).

% Runs all tests and writes results to 'lc-time-bca.doc'
test_all :-
    tell('lc-time-bca.doc'),
    % Test 0
    nl, write('=== Test 0 ==='), nl,
    findall((S, ' : ', A), test(S, A, 0), Results0),
    write_l(Results0),
    % Test 1
    nl, write('=== Test 1 ==='), nl,
    findall((S1, ' : ', A1), test1(S1, A1, 0), Results1),
    write_l(Results1),
    % Test 2
    nl, write('=== Test 2 ==='), nl,
    findall((S2, ' : ', A2), test2(S2, A2, 0), Results2),
    write_l(Results2),
    % Test 3
    nl, write('=== Test 3 ==='), nl,
    findall((S3, ' : ', A3), test3(S3, A3, 0), Results3),
    write_l(Results3),
    % Test 4
    nl, write('=== Test 4 ==='), nl,
    findall((S4, ' : ', A4), test4(S4, A4, 0), Results4),
    write_l(Results4),
    told.

% Helper predicate to write lists
write_l([]) :- !.
write_l([H | T]) :-
    write(H), nl,
    write_l(T).

% Runs all tests and measures execution time
test_time :-
    statistics(walltime, [Start, _]),
    test_all,
    nl,
    statistics(walltime, [End, _]),
    Time is End - Start,
    format('All solutions completed in ~3d milliseconds.~n', [Time]).