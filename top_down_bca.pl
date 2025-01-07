ph(ph(S)) --> s(S).
ph(ph(S1, C, S2)) --> s(S1), conj(C), s(S2).

s(s(N, V)) --> np(N), vp(V).
s(s(ADV, V)) --> adv(ADV), vp(V).

np(np(N1, CONJ, N2)) --> np2(N1), conj(CONJ), np2(N2).
np(np(N)) --> np2(N).
np2(np2(PREP, N)) --> prep(PREP), ap(N).
np2(np2(ART, N)) --> art(ART), ap(N).
np2(np2(N, ATN)) --> n(N), atp(ATN).
np2(np2(N)) --> n(N).
np2(np2(N)) --> prn(N).

ap(inv_ap(ADJ, N)) --> adj(ADJ), np2(N).
ap(inv_ap(N)) --> np2(N).
atp(atp(N1, CONJ, N2)) --> atp2(N1), conj(CONJ), atp2(N2).
atp(atp(N)) --> atp2(N).
atp2(atp2(ART, N)) --> art(ART), n(N).
atp2(atp2(PREP, N)) --> prep(PREP), atp3(N).
atp2(atp2(N)) --> atp3(N).
atp3(atp3(N, PREP, ADJ)) --> n(N), prep(PREP), adj(ADJ).
atp3(atp3(N, ADJ)) --> n(N), adj(ADJ).
atp3(atp3(ADJ, N)) --> adj(ADJ),n(N).
atp3(atp3(N)) --> n(N).
atp3(atp3(N)) --> adj(N).

% Verb phrase rules
vp(vp(V, CONJ, PSEC)) --> vcpn(V), conj(CONJ), s(PSEC).
vp(vp(V, C)) --> v(V), np2(C).
vp(vp(V, C)) --> v(V), pp(C).
vp(vp(VCP, N)) --> vcpn(VCP), pastverb(N).
vp(vp(VCP, N)) --> vcpn(VCP), np(N).
vp(vp(PREFL, V)) --> prefl(PREFL), vp(V).
vp(vp(V1, PCONJ)) --> v(V1), pconj(PCONJ).
vp(vp(V)) --> v(V).

pconj(pconj(ADV, PREFL, V)) --> adv(ADV), prefl(PREFL), vp(V).
pastverb(pastverb(PREP, PART)) --> prep(PREP), vpast(PART).

% Prepositional and adverbial phrases
pp(pp(CD, CT)) --> cd(CD), ct(CT).
cd(cd(CD)) --> np2(CD).

ct(ct(FOCUS_ADV, ADVPP)) --> adv(FOCUS_ADV), advp(ADVPP).

advp(advp(ADV1, CONJ, ADV2)) --> advp2(ADV1), conj(CONJ), advp2(ADV2).
advp2(advp2(PREP, ADV)) --> prep(PREP), adv(ADV).
advp2(advp2(ADV)) --> adv(ADV).

vcpn(vcpn(V, ADJ)) --> vcp(V), adj(ADJ).
vcpn(vcpn(V, ADV)) --> vcp(V), adv(ADV).
vcpn(vcpn(V1, V2)) --> vcp(V1), v(V2).

% Lexicon
n(n(oamenii)) --> [oamenii].
n(n(culorile)) --> [culorile].
n(n(zile)) --> [zile].
n(n(nuante)) --> [nuante].
n(n(multitudine)) --> [multitudine].
n(n(tonuri)) --> [tonuri].
n(n(ora)) --> [ora].
n(n(mii)) --> [mii].
n(n(culori)) --> [culori].
n(n(galbenuri)) --> [galbenuri].
n(n(albastru)) --> [albastru].
n(n(intunecimi)) --> [intunecimi].
n(n(cerul)) --> [cerul].
n(n(culoare)) --> [culoare].
n(n(spectrul)) --> [spectrul].

prn(prn(mie)) --> [mie].
prn(prn(mine)) --> [mine].
prn(prn(ea)) --> [ea].

prefl(prefl(imi)) --> [imi].
prefl(prefl(ma)) --> [ma].

v(v(observa)) --> [observa].
v(v(combina)) --> [combina].
v(v(consta)) --> [consta].
v(v(place)) --> [place].
v(v(incerc)) --> [incerc].
v(v(bucur)) --> [bucur].

vcp(vcp(este)) --> [este].
vcp(vcp(poate)) --> [poate].
vcp(vcp(sunt)) --> [sunt].

vpast(vpast(observat)) --> [observat].

art(art(unei)) --> [unei].
art(art(o)) --> [o].

adv(adv(numai)) --> [numai].
adv(adv(sfarsit)) --> [sfarsit].
adv(adv(inceput)) --> [inceput].
adv(adv(clar)) --> [clar].
adv(adv(totusi)) --> [totusi].
adv(adv(sa)) --> [sa].

adj(adj(singura)) --> [singura].
adj(adj(diferite)) --> [diferite].
adj(adj(ceruite)) --> [ceruite].
adj(adj(nepatruns)) --> [nepatruns].
adj(adj(importante)) --> [importante].
adj(adj(ciocolatiu)) --> [ciocolatiu].
adj(adj(fiecare)) --> [fiecare].
adj(adj(intreg)) --> [intreg].

prep(prep(la)) --> [la].
prep(prep(pentru)) --> [pentru].
prep(prep(de)) --> [de].
prep(prep(in)) --> [in].
prep(prep(din)) --> [din].

conj(conj(si)) --> [si].
conj(conj(dar)) --> [dar].
conj(conj(ca)) --> [ca].
conj(conj(',')) --> [','].

test(_, _, 10) :- !.
test(S, A, I) :-
    ph(A,
    [oamenii, observa, culorile, unei, zile, numai, la, sfarsit, si, la, inceput, dar, pentru, mine, este, clar, ca, ea, combina, o, multitudine, de, nuante, si, tonuri],
    []),
    ph(A, S, []),
    I1 is I + 1,
    test(S1, A1, I1).

test1(_,_,10):-!.
test1(S,A,I):-ph(A,[o, singura, ora, poate, consta, in, mii, de, culori, diferite],[]),ph(A,S,[]),I1 is I+1, test1(S1,A1,I1).

test2(_,_,10):-!.
test2(S,A,I):-ph(A,[galbenuri, ceruite, ',', nuante, de, albastru, si, intunecimi, de, nepatruns, sunt, importante, de, observat],[]),ph(A,S,[]),I1 is I+1, test2(S1,A1,I1).

test3(_,_,10):-!.
test3(S,A,I):-ph(A,[mie, imi, place, cerul, ciocolatiu],[]),ph(A,S,[]),I1 is I+1, test3(S1,A1,I1).

test4(_,_,10):-!.
test4(S,A,I):-ph(A,[totusi, incerc, sa, ma, bucur, de, fiecare, culoare, din, intreg, spectrul],[]),ph(A,S,[]),I1 is I+1, test4(S1,A1,I1).

test_all:-tell('td-time-clau.doc'),
    findall( (E,' : ',A), test(E,A,0), L ), nl,write_l(L), 
    findall( (E1,' : ',A1), test1(E1,A1,0), L1 ), nl,write_l(L1),
    findall( (E2,' : ',A2), test2(E2,A2,0), L2 ), nl,write_l(L2),
    findall( (E3,' : ',A3), test3(E3,A3,0), L3 ), nl,write_l(L3),
    findall( (E4,' : ',A4), test4(E4,A4,0), L4 ), nl,write_l(L4).           

write_l([]):-!.
write_l([H|T]):-write(H),nl, write_l(T).

test_time:-
        statistics(walltime, [Start,_]),
        test_all,nl,
        statistics(walltime, [End,_]),
        Time is End - Start,
        format('all solutions in ~3d seconds.~n', [Time])
                              ,told.