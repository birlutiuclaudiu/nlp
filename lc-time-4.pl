
/*algoritm left_corner fara memorare wf */

leaf(prp)-->[eu].
leaf(prp)-->[le].
leaf(prr)-->[ce].
leaf(prn)-->[altora].
leaf(prpos)-->[mea].
leaf(neg)-->[nu].
leaf(v)-->[strivesc].
leaf(v)-->[ucid].
leaf(v)-->[intalnesc].
leaf(v)-->[sugruma].
leaf(v)-->[sporesc].
leaf(v)-->[micsoreaza].
leaf(v)-->[mareste].
leaf(v)-->[imbogatesc].
leaf(n)-->[corola].
leaf(n)-->[minuni].
leaf(n)-->[lumii].
leaf(n)-->[mintea].
leaf(n)-->[tainele].
leaf(n)-->[flori].
leaf(n)-->[ochi].
leaf(n)-->[buze].
leaf(n)-->[morminte].
leaf(n)-->[lumina].
leaf(n)-->[vraja].
leaf(n)-->[nepatrunsului].
leaf(n)-->[adancimi].
leaf(n)-->[intuneric].
leaf(n)-->[taina].
leaf(n)-->[luna].
leaf(n)-->[noptii].
leaf(n)-->[zare].
leaf(n)-->[fiori].
leaf(n)-->[mister].
leaf(prep)-->[de].
leaf(prep)-->[cu].
leaf(prep)-->[in].
leaf(prep)-->[pe].
leaf(art)-->[a].
leaf(conj)-->[si].
leaf(conj)-->[','].
leaf(conj)-->[ori].
leaf(conj)-->[dar].
leaf(conj)-->[ci].
leaf(adj)-->[ascuns].
leaf(adj)-->[intunecata].
leaf(adj)-->[sfant].
leaf(adv)-->[asa].
leaf(adv)-->[tare].
leaf(adv)-->[mai].
leaf(adv)-->[intocmai].
leaf(adv)-->[cum].


lcd(s, fr).
lcd(prp, s).
lcd(vp, s).
lcd(prr, s).
lcd(np, s).
%lcd(vp, s).
lcd(advp, s).
lcd(neg, vp).
lcd(v, vp).
lcd(np2, vp).
lcd(advp, vp).
lcd(n, np).
%lcd(ap, np).
lcd(prep, np2).
%lcd(npp, np2).
%lcd(npa, np2).
lcd(art, np3).
lcd(adj, ap).
lcd(n, npp).
lcd(n, npa).
lcd(n, np4).
lcd(np2, np4).
lcd(np3, np).
lcd(adv, advp).
%lcd(conj, fr).      %  ???
lc(X, X):-!.
lc(X, Y):-lcd(X, Y),!.
lc(X, Y):-lcd(X, Z), lc(Z, Y).


parse(Nterm,As,[W0|W1],Wn):-
     leaf(Pterm,[W0|W1],W1),
     lc(Pterm,Nterm),
     Ap=..[Pterm,W0],    write(Ap),nl,
     P=..[Pterm,Nterm,Ap,As,W1,Wn],
     call(P).


n(n, A, A)-->[].
prp(prp, A, A)-->[].
prn(prn, A, A)-->[].

%vp(vp, A, A)-->[].
v(v, A, A)-->[].
prep(prep, A, A)-->[].
adj(adj, A, A)-->[].
prr(prr, A, A)-->[].
conj(conj, A, A)-->[].
prpos(prpos, A, A)-->[].
neg(neg, A, A)-->[].
art(art, A, A)-->[].
adv(adv, A, A)-->[].




% fr
fr(fr, A, A)-->[].
s(Nt, S1, As)-->{lc(fr, Nt)}, parse(conj, C), parse(fr, S2), fr(Nt, fr(S1, C, S2), As).
s(Nt, S, As)-->{lc(fr, Nt)},  fr(Nt, fr(S), As).


% s
s(s, A, A)-->[].
prp(Nt, P, As)-->{lc(s, Nt)}, parse(vp, V), s(Nt, s(P, V), As).
vp(Nt, V, As)-->{lc(s, Nt)}, parse(prp, P), parse(ap, A), parse(np2, N), s(Nt, s(V, P, A, N), As).
prr(Nt, P, As)-->{lc(s, Nt)}, parse(s, V), s(Nt, s(P, V), As).
np(Nt, N, As)-->{lc(s, Nt)}, parse(vp, V), s(Nt, s(N, V), As).
advp(Nt, A, As)-->{lc(s, Nt)}, parse(n, N), parse(vp, V), s(Nt, s(A, N, V), As).
vp(Nt, V, As)-->{lc(s, Nt)}, s(Nt, s(V), As).


% vp
vp(vp, A, A)-->[].
neg(Nt, Ne, As)-->{lc(vp, Nt)}, parse(vp, V), vp(Nt, vp(Ne, V), As).
v(Nt, V, As)-->{lc(vp, Nt)}, vp(Nt, vp(V), As).
v(Nt, V, As)-->{lc(vp, Nt)}, parse(np, N), vp(Nt, vp(V, N), As).
v(Nt, V, As)-->{lc(vp, Nt)}, parse(np4, N), vp(Nt, vp(V, N), As).
v(Nt, V, As)-->{lc(vp, Nt)}, parse(np2, N), parse(np, M), vp(Nt, vp(V, N, M), As).
v(Nt, V, As)-->{lc(vp, Nt)}, parse(advp, A), parse(np, N), vp(Nt, vp(V, A, N), As).
np2(Nt, N, As)-->{lc(vp, Nt)}, parse(vp, V), vp(Nt, vp(N, V), As).          
advp(Nt, A, As)-->{lc(vp, Nt)}, parse(vp, V), vp(Nt, vp(A, V), As).        


% np
np(np, A, A)-->[].
np4(np4, A, A)-->[].
np3(np3, A, A)-->[].
np2(np2, A, A)-->[].
n(Nt, N, As)-->{lc(np, Nt)}, np(Nt, np(N), As).
n(Nt, N, As)-->{lc(np, Nt)}, parse(np2, M), np(Nt, np(N, M), As).
n(Nt, N, As)-->{lc(np, Nt)}, parse(ap, A), np(Nt, np(N, A), As).
n(Nt, N, As)-->{lc(np, Nt)}, parse(np, M), np(Nt, np(N, M), As).
                  %  np --->  n

n(Nt, N, As)-->{lc(np, Nt)}, parse(np2, M), parse(np3, M1), np(Nt, np(N, M, M1), As).
prep(Nt, Pre, As)-->{lc(np2, Nt)}, parse(n, N), np2(Nt, np2(Pre, N), As).
prep(Nt, Pre, As)-->{lc(np2, Nt)}, parse(npp, N), np2(Nt, np2(Pre, N), As).
prep(Nt, Pre, As)-->{lc(np2, Nt)}, parse(npa, N), np2(Nt, np2(Pre, N), As).
art(Nt, Ar, As)-->{lc(np3, Nt)}, parse(np, N), np3(Nt, np3(Ar, N), As).
n(Nt, N, As)-->{lc(np, Nt)}, parse(s, S), np(Nt, np(N, S), As).
np2(Nt, N, As)-->{lc(np4, Nt)}, parse(conj, C), parse(np4, M), np4(Nt, np4(N, C, M), As).
n(Nt, N, As)-->{lc(np4, Nt)}, np4(Nt, np4(N), As).
n(Nt, N, As)-->{lc(np, Nt)}, parse(prn, P), np(Nt, np(N, P), As).

np3(Nt, N3, As)-->{lc(np, Nt)}, parse(n, N), np(Nt, np(N3, N), As).             % era lipsa


% ap
ap(ap, A, A)-->[].
adj(Nt, A, As)-->{lc(ap, Nt)}, parse(n, N), ap(Nt, ap(A, N), As).
adj(Nt, A, As)-->{lc(ap, Nt)}, ap(Nt, ap(A), As).
adj(Nt, A, As)-->{lc(ap, Nt)}, parse(np2, N), parse(np2, M), ap(Nt, ap(A, N, M), As).


% npp
npp(npp, A, A)-->[].
npa(npa, A, A)-->[].
n(Nt, N, As)-->{lc(npp, Nt)}, parse(prpos, P), npp(Nt, npp(N, P), As).
n(Nt, N, As)-->{lc(npa, Nt)}, parse(np2, A), npa(Nt, npa(N, A), As).


% advp
advp(advp, A, A)-->[].
adv(Nt, A, As)-->{lc(advp, Nt)}, parse(adv, A2), advp(Nt, advp(A, A2), As).
adv(Nt, A, As)-->{lc(advp, Nt)}, advp(Nt, advp(A), As).




test(_,_,1):-!.
test(S,A,I):- S=[eu,nu,strivesc,corola,de,minuni,a,lumii,si,nu,ucid,cu,mintea,tainele,ce,le,intalnesc,in,flori,',',in,ochi,',',pe,buze,ori,morminte],     parse(fr,A,S,[]),I1 is I+1, test(S1,A1,I1).

test1(_,_,1):-!.
test1(S,A,I):- S=[lumina, altora, sugruma, vraja, nepatrunsului, ascuns, in, adancimi, de, intuneric, dar, eu, cu, lumina, mea, sporesc, a, lumii, taina],     parse(fr,A,S,[]),I1 is I+1, test1(S1,A1,I1).

test2(_,_,1):-!.
test2(S,A,I):- S=[intocmai, cum, luna, nu, micsoreaza, ci, mareste, mai, tare, taina, noptii, ',', asa, imbogatesc, eu, intunecata, zare, cu, fiori, de, mister],     parse(fr,A,S,[]),I1 is I+1, test2(S1,A1,I1).



test_all:-tell('lc-time10.doc'),
        % findall( (E,' : ',A), 
test(E,A,0), nl,write([E,A]),nl,
      %findall( (E1,' : ',A1), 
test1(E1,A1,0), nl,write([E1,A1]),nl,
      %findall( (E2,' : ',A2), 
test2(E2,A2,0), nl,write([E2,A2]),nl.            

write_l([]):-!.
write_l([H|T]):-write(H),nl, write_l(T).

test_time:-
        statistics(walltime, [Start,_]),
        test_all,nl,
        statistics(walltime, [End,_]),
        Time is End - Start,
        format('all solutions in ~3d seconds.~n', [Time])
                              ,told.
		   
