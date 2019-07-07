%---------------------------------
% Aufgabe 7 - Semesteraufgaben 2013
% Le Duyen Sandra, Vu - 768693
%---------------------------------

% Die Anfrage "start." gibt 5 Beispielanfragen und dessen Ausgabe heraus.

start:- 
	write('Anfrage 1: parse([der,stuhl,brennt],A)'), nl, 
	parse([der,stuhl,brennt],A), write(A), nl,nl,
	
	write('Anfrage 2: parse([die,stuehle,brennen],Y)'), nl,
	parse([die,stuehle,brennen],B), write(B), nl,nl,
	
	write('Anfrage 3: parse([das,buch,mit,der,flasche,brennt],C)'), nl, 
	parse([das,buch,mit,der,flasche,brennt],C), write(C), nl,nl,
	
	write('Anfrage 4: parse([john,sieht,das,buch],D)'), nl, 
	parse([john,sieht,das,buch],D), write(D), nl,nl,
	
	write('Anfrage 5: parse([john,sieht,das,buch,auf,dem,tisch],E)'), nl, 
	parse([john,sieht,das,buch,auf,dem,tisch],E), write(E), nl,nl,
	
	write('Anfrage 6: parse([john,sieht,heute,das,buch,auf,dem,tisch],F)'), nl, 
	parse([john,sieht,heute,das,buch,auf,dem,tisch],F), write(F), nl,nl,
	
	write('Anfrage 7: parse([john,sieht,das,buch,heute,auf,dem,tisch],G)'), nl,
	parse([john,sieht,das,buch,heute,auf,dem,tisch],G),write(G),nl.

%---------------------------------
% "parse(Satz,Baumliste)"
%---------------------------------
parse(Satz,Baumliste) :-
	findall(Baum,phrase(satz(Baum),Satz),Baumliste).

%---------------------------------
% Satz -> Vorfeld LK
% S -> NP VP
% Ein Satz kann in NP und VP aufgeteilt werden.
% Genauso gut kann man einen Satz in Vorfeld und Linke Satzklammer aufteilen,
% denn sobald die VP mit dem Verb beginnt, fängt auch die linke Satzklammer an.
% Alles vor der VP ist die NP und alles vor der Linkensatzklammer ist das Vorfeld.
%---------------------------------

satz(s / [NPBaum,VBaum]) -->
	vorfeld(NPBaum,agr(Person,Numerus,nom)),	% NP und VP müssen kongruent sein in Person und Numerus.
	lk1(VBaum,fin(Person,Numerus,_,_)).


%---------------------------------
% Da nicht die komplette VP zur linken Satzklammer gehört,
% muss alles, was nach dem Verb kommt als Mittelfeld bezeichnet werden.
% Satz -> Vorfeld LK Mittelfed
% S --> NP VP
%---------------------------------

satz(s / [NPBaum,VBaum]) -->
	vorfeld(NPBaum,agr(Person,Numerus,nom)),	% Die NP, die im Vorfeld steht, ist hier zwangsweise im Nominativ
	lk_mittelfeld(VBaum,fin(Person,Numerus,_,_)).

	
%---------------------------------
% Linke Satzklammer
% VP --> V
% Ein intransitives Verb gehšrt zur linken Satzklammer.
%---------------------------------

lk1(lk / [VBaum],Merkmale) -->
	v(VBaum,Merkmale,[]).				% intransitives Verb, da leere Liste
	
%---------------------------------
% Linke Satzklammer + Mittelfeld
% VP --> V NP
% Ein Transitives Verb fordert eine NP und diese NP gehört zum Mittelfeld.
%---------------------------------

lk_mittelfeld(lk /[VBaum,NPBaum],Merkmale) -->
	v(VBaum,Merkmale,[regiert(Kasus)]),	% Der Kasus von Verb und der NP, die im Mittelfeld
	mittelfeld(NPBaum,agr(_,_,Kasus)).	% steht, müssen gleich sein.

%---------------------------------
%---------------------------------
% VORFELD
% Im Vorfeld kšnnen viele verschiedene Wortarten stehen.
%---------------------------------
%---------------------------------
% Vorfeld = N
% N
%---------------------------------

vorfeld(vorfeld / [NBaum],agr(3,pl,Kasus)) -->
	n(NBaum,kng(Kasus,pl,_)).			% Nomen ohne Artikel müssen im Plural stehen.

%---------------------------------
% Vorfeld = EN
% EN
%---------------------------------

vorfeld(vorfeld / [ENBaum], agr(3,sg,Kasus)) -->
	en(ENBaum,Kasus).					% Eigennamen stehen nur im Singular.

%---------------------------------
% Vorfeld = NP
% NP -> Art N
%---------------------------------

vorfeld(vorfeld/[ArtBaum,NBaum],agr(3,Numerus,Kasus)) -->
	art(ArtBaum,KNG),					% Artikel und Nomen mŸssen kongruent sein in Kasus und Numerus.
	n(NBaum,KNG),	  
	{ KNG = kng(Kasus,Numerus,_) }.
	
%---------------------------------
% Vorfeld = PP
% PP --> Art N PP
%---------------------------------

vorfeld(vorfeld / [ArtBaum,NBaum,PPBaum],agr(3,Numerus,Kasus)) -->
	art(ArtBaum,KNG),					% Artikel und Nomen müssen kongruent sein in Kasus und Numerus.
	n(NBaum,KNG),
	pp(PPBaum), 
	{ KNG = kng(Kasus,Numerus,_) }.

%---------------------------------
%---------------------------------
% MITTELFELD
% Im Mittelfeld können auch viele verschiedene Wortarten stehen.
%---------------------------------
%---------------------------------
% Mittelfeld = N
% N
%---------------------------------

mittelfeld(mittelfeld / [NBaum],agr(3,pl,Kasus)) -->
	n(NBaum,kng(Kasus,pl,_)).				% Nomen ohne Artikel müssen im Plural stehen.
	
%---------------------------------
% Mittelfeld = EN
% EN
%---------------------------------
mittelfeld(mittelfeld / [ENBaum], agr(3,sg,Kasus)) -->
	en(ENBaum,Kasus).						% Eigennamen stehen nur im Singular.
	
%---------------------------------
% Mittelfeld = NP
% NP -> Art N
%---------------------------------

mittelfeld(mittelfeld/[ArtBaum,NBaum],agr(3,Numerus,Kasus)) -->
	art(ArtBaum,KNG),						% Artikel und Nomen müssen kongruent sein in Kasus und Numerus.
	n(NBaum,KNG),	  
	{ KNG = kng(Kasus,Numerus,_) }.

%---------------------------------
% Mittelfeld = PP
% PP -> PP Art N
%---------------------------------

mittelfeld(mittelfeld / [PPBaum,ArtBaum,NBaum],agr(3,Numerus,Kasus)) -->
	art(ArtBaum,KNG),						% Artikel und Nomen müssen kongruent sein in Kasus und Numerus.
	n(NBaum,KNG),
	pp(PPBaum), 
	{ KNG = kng(Kasus,Numerus,_) }.
	
%---------------------------------
% Mittelfeld = Adv NP
% Adv NP
%---------------------------------

mittelfeld(mittelfeld / [Adv,NPBaum],agr(3,_Numerus,Kasus)) -->
	adv(Adv),
	np(NPBaum,agr(_,_,Kasus)).

%---------------------------------
% Mittelfeld = Adv NP PP
% Mittelfeld = NP Adv PP
% Adv NP PP
%---------------------------------
	
mittelfeld(mittelfeld / [Adv,NPBaum,PPBaum],agr(3,_Numerus,Kasus)) -->
	adv(Adv),
	np(NPBaum,agr(_,_,Kasus)),
	pp(PPBaum).
	
mittelfeld(mittelfeld / [NPBaum,Adv,PPBaum],agr(3,_Numerus,Kasus)) -->
	np(NPBaum,agr(_,_,Kasus)),
	adv(Adv),
	pp(PPBaum).

%---------------------------------
% Definition der Präpositionalphrase im Vorfeld und im Mittelfeld.
% Die PP braucht zur Definition auch die Definition der NP.
%---------------------------------

% PP --> P NP
pp([PBaum,NPBaum]) -->
	p(PBaum,[regiert(Kasus)]),		% Der Kasus der Präposition muss mit dem der NP übereinstimmen.
	np(NPBaum,agr(_,_,Kasus)).

% NP --> Art N
np([ArtBaum,NBaum],agr(3,Numerus,Kasus)) -->
	art(ArtBaum,KNG),				% Artikel und Nomen müssen die selben KNG Merkmale haben.
	n(NBaum,KNG),	  
	{ KNG = kng(Kasus,Numerus,_) }.

%---------------------------------
% Lexikalische Einsetzungsregeln
% Das Lexikon wird in die normale DCG Schreibform umgewandelt.
%---------------------------------

v(v/[-Verb],Merkmale,Valenz) --> [Verb], {verb(Verb,Merkmale,Valenz)}.
art(art/[-Art],KNG) --> [Art], {artikel(Art,KNG)}.
n(n/[-Nomen],KNG) --> [Nomen], {nomen(Nomen,KNG)}.
p(p/[-Prep],Valenz) --> [Prep], {praeposition(Prep,Valenz)}.
en(en/[-Name],Kasus) --> [Name], {eigenname(Name,Kasus)}.


%---------------------------------
% LEXIKON
%---------------------------------

%---------------------------------
% Artikel
%---------------------------------

% bestimmte Artikel: singular
artikel(das,kng(nom,sg,neut)).
artikel(das,kng(akk,sg,neut)).

artikel(der,kng(nom,sg,mask)).
artikel(der,kng(dat,sg,fem)).
artikel(der,kng(gen,sg,fem)).

artikel(die,kng(nom,sg,fem)).
artikel(die,kng(akk,sg,fem)).

artikel(des,kng(gen,sg,mask)).
artikel(des,kng(gen,sg,neut)).

artikel(den,kng(akk,sg,mask)).


artikel(dem,kng(dat,sg,mask)).
artikel(dem,kng(dat,sg,neut)).

% bestimmte Artikel: plural
artikel(die,kng(nom,pl,_)).
artikel(die,kng(akk,pl,_)).
artikel(der,kng(gen,pl,_)).
artikel(den,kng(dat,pl,_)).


% unbestimmte Artikel
artikel(ein,kng(nom,sg,neut)).
artikel(ein,kng(akk,sg,neut)).
artikel(ein,kng(nom,sg,mask)).

artikel(einer,kng(dat,sg,fem)).
artikel(einer,kng(gen,sg,fem)).

artikel(einem,kng(dat,sg,mask)).
artikel(einem,kng(dat,sg,neut)).

artikel(eine,kng(nom,sg,fem)).
artikel(eine,kng(akk,sg,fem)).

artikel(eines,kng(gen,sg,mask)).
artikel(eines,kng(gen,sg,neut)).

artikel(einen,kng(akk,sg,mask)).

%----------------------------------
% Nomen
%----------------------------------

% GegenstŠnde
nomen(stuhl,kng(Kasus,sg,mask)) :- member(Kasus,[nom,akk,dat]).
nomen(stuhl,kng(gen,sg,mask)).
nomen(stuehle,kng(Kasus,pl,mask)) :- member(Kasus,[nom,akk,gen]).
nomen(stuehlen,kng(dat,pl,mask)).

nomen(tisch,kng(Kasus,sg,mask)) :- member(Kasus,[nom,akk,dat]).
nomen(tischs,kng(gen,sg,mask)).
nomen(tische,kng(Kasus,pl,mask)) :- member(Kasus,[nom,akk,gen]).
nomen(tischen,kng(dat,pl,mask)).

nomen(buch,kng(Kasus,sg,neut)) :- member(Kasus,[nom,akk,dat]).
nomen(buches,kng(gen,sg,neut)).
nomen(buecher,kng(Kasus,pl,neut)) :- member(Kasus,[nom,akk,gen]).
nomen(buechern,kng(dat,pl,neut)).

nomen(flasche,kng(_,sg,fem)).
nomen(flaschen,kng(_,pl,fem)).

nomen(fenster,kng(Kasus,sg,neut)) :- member(Kasus,[nom,akk,dat]).
nomen(fensters,kng(gen,sg,neut)).
nomen(fenster,kng(Kasus,pl,neut)) :- member(Kasus,[nom,akk,gen]).
nomen(fenstern,kng(dat,pl,neut)).

% Berufe
nomen(lehrer,kng(Kasus,sg,mask)) :- member(Kasus,[nom,akk,dat]).
nomen(lehrers,kng(gen,sg,mask)).
nomen(lehrer,kng(Kasus,pl,mask)) :- member(Kasus,[nom,akk,gen]).
nomen(lehrern,kng(dat,pl,mask)).

nomen(polizist,kng(Kasus,sg,mask)) :- member(Kasus,[nom,akk,dat]).
nomen(polizisten,kng(gen,sg,mask)).
nomen(polizisten,kng(_,pl,mask)).

nomen(programmierer,kng(Kasus,sg,mask)) :- member(Kasus,[nom,akk,dat]).
nomen(programmierers,kng(gen,sg,mask)).
nomen(programmierer,kng(Kasus,pl,mask)) :- member(Kasus,[nom,akk,gen]).
nomen(programmierern,kng(dat,pl,mask)).

nomen(lehrerin,kng(_,sg,fem)).
nomen(lehrerinnen,kng(_,pl,fem)).

nomen(polizistin,kng(_,sg,fem)).
nomen(polizistinnen,kng(_,pl,fem)).

nomen(programmiererin,kng(_,sg,fem)).
nomen(programmiererinnen,kng(_,pl,fem)).

% Gebäude
nomen(schule,kng(_,sg,fem)).
nomen(schulen,kng(_,pl,fem)).

nomen(agentur,kng(_,sg,fem)).
nomen(agenturen,kng(_,pl,fem)).

nomen(haus,kng(Kasus,sg,neut)) :- member(Kasus,[nom,akk,dat]).
nomen(hauses,kng(gen,sg,neut)).
nomen(haeuser,kng(Kasus,pl,neut)) :- member(Kasus,[nom,akk,gen]).
nomen(haeusern,kng(dat,pl,neut)).

nomen(einkaufszentrum,kng(Kasus,sg,neut)) :- member(Kasus,[nom,akk,dat]).
nomen(einkaufszenrums,kng(gen,sg,neut)).
nomen(einkaufszentren,kng(_,pl,neut)).

% Tiere
nomen(katze,kng(_,sg,fem)).
nomen(katzen,kng(_,pl,fem)).

nomen(hund,kng(Kasus,sg,mask)) :- member(Kasus,[nom,akk,dat]).
nomen(hundes,kng(gen,sg,mask)).
nomen(hunde,kng(Kasus,pl,mask)) :- member(Kasus,[nom,akk,gen]).
nomen(hunden,kng(dat,pl,mask)).

nomen(maus,kng(_,sg,fem)).
nomen(maeuse,kng(_,pl,fem)).

nomen(tiger,kng(Kasus,sg,mask)) :- member(Kasus,[nom,akk,dat]).
nomen(tigers,kng(gen,sg,mask)).
nomen(tiger,kng(Kasus,pl,mask)) :- member(Kasus,[nom,akk,gen]).
nomen(tigern,kng(dat,pl,mask)).

% Pflanzen
nomen(baum,kng(Kasus,sg,mask)) :- member(Kasus,[nom,akk,dat]).
nomen(baums,kng(gen,sg,mask)).
nomen(baeume,kng(Kasus,pl,mask)) :- member(Kasus,[nom,akk,gen]).
nomen(baeumen,kng(dat,pl,mask)).

nomen(blume,kng(_,sg,fem)).
nomen(blume,kng(_,pl,fem)).

%----------------------------------
% finite Verben
%----------------------------------

verb(hat,fin(3,sg,praes,ind),[regiert(akk)]).
verb(haben,fin(1,pl,praes,ind),[regiert(akk)]).
verb(haben,fin(3,pl,praes,ind),[regiert(akk)]).

verb(wird,fin(3,sg,praes,ind),[regiert(akk)]).
verb(werden,fin(1,pl,praes,ind),[regiert(akk)]).
verb(werden,fin(3,pl,praes,ind),[regiert(akk)]).

verb(waechst,fin(3,sg,praes,ind),[regiert(akk)]).
verb(wachsen,fin(1,pl,praes,ind),[regiert(akk)]).
verb(wachsen,fin(3,pl,praes,ind),[regiert(akk)]).

verb(oeffnet,fin(3,sg,praes,ind),[regiert(akk)]).
verb(oeffnen,fin(1,pl,praes,ind),[regiert(akk)]).
verb(oeffnen,fin(3,pl,praes,ind),[regiert(akk)]).

verb(geht,fin(3,sg,praes,ind),[regiert(akk)]).
verb(gehen,fin(1,pl,praes,ind),[regiert(akk)]).
verb(gehen,fin(3,pl,praes,ind),[regiert(akk)]).

verb(mag,fin(3,sg,praes,ind),[regiert(akk)]).
verb(moegen,fin(1,pl,praes,ind),[regiert(akk)]).
verb(moegen,fin(3,pl,praes,ind),[regiert(akk)]).

verb(sieht,fin(3,sg,praes,ind),[regiert(akk)]).
verb(sehen,fin(1,pl,praes,ind),[regiert(akk)]).
verb(sehen,fin(3,pl,praes,ind),[regiert(akk)]).

verb(gibt,fin(3,sg,praes,ind),[regiert(dat),regiert(akk)]).
verb(geben,fin(1,pl,praes,ind),[regiert(dat),regiert(akk)]).
verb(geben,fin(3,pl,praes,ind),[regiert(dat),regiert(akk)]).

verb(steht,fin(3,sg,praes,ind),[regiert(dat),regiert(akk)]).
verb(stehen,fin(1,pl,praes,ind),[regiert(dat),regiert(akk)]).
verb(sehen,fin(3,pl,praes,ind),[regiert(dat),regiert(akk)]).

verb(trifft,fin(3,sg,praes,ind),[regiert(akk)]).
verb(treffen,fin(1,pl,praes,ind),[regiert(akk)]).
verb(treffen,fin(3,pl,praes,ind),[regiert(akk)]).

% Intransitiv
verb(brennt,fin(3,sg,praes,ind),[]).
verb(brennt,fin(2,pl,praes,ind),[]).
verb(brennen,fin(1,pl,praes,ind),[]).
verb(brennen,fin(3,pl,praes,ind),[]).

%----------------------------------
% Adjektive
%----------------------------------
 	
adjektiv(kleinen,kng(Kasus,sg,mask)) :- member(Kasus,[akk,gen]).	
adjektiv(kleiner,kng(nom,sg,mask)).
adjektiv(kleinem,kng(dat,_,mask)).
adjektiv(kleine,kng(Kasus,pl,_)) :- member(Kasus,[nom,akk]).
adjektiv(kleiner,kng(gen,pl,_)). 
adjektiv(kleinen,kng(dat,pl,_)). 

adjektiv(grossen,kng(Kasus,sg,mask)) :- member(Kasus,[akk,gen]).	
adjektiv(grosser,kng(nom,sg,mask)).
adjektiv(grossem,kng(dat,_,mask)).
adjektiv(grosse,kng(Kasus,pl,_)) :- member(Kasus,[nom,akk]).
adjektiv(grosser,kng(gen,pl,_)). 
adjektiv(grossen,kng(dat,pl,_)). 

adjektiv(komischen,kng(Kasus,sg,mask)) :- member(Kasus,[akk,gen]).	
adjektiv(komischer,kng(nom,sg,mask)).
adjektiv(komischem,kng(dat,_,mask)).
adjektiv(komische,kng(Kasus,pl,_)) :- member(Kasus,[nom,akk]).
adjektiv(komischer,kng(gen,pl,_)). 
adjektiv(komischen,kng(dat,pl,_)). 
	
adjektiv(netten,kng(Kasus,sg,mask)) :- member(Kasus,[akk,gen]).	
adjektiv(netter,kng(nom,sg,mask)).
adjektiv(nettem,kng(dat,_,mask)).
adjektiv(nette,kng(Kasus,pl,_)) :- member(Kasus,[nom,akk]).
adjektiv(netter,kng(gen,pl,_)). 
adjektiv(netten,kng(dat,pl,_)). 

%----------------------------------
% Adverbien
%----------------------------------

% temporal
adv(adv/[-gestern])	--> [gestern].
adv(adv/[-heute])	--> [heute].
adv(adv/[-frueher])	--> [frueher].

% lokal
adv(adv/[-hier])	--> [hier].
adv(adv/[-draussen])--> [draussen].
adv(adv/[-rechts])	--> [rechts].
adv(adv/[-links])	--> [links].

% modal
adv(adv/[-vielleicht]) 	--> [vielleicht].
adv(adv/[-gerne]) 	--> [gerne].
adv(adv/[-leider]) 	--> [leider].

% interroagtiv
adv(adv/[-wo]) 		--> [wo].
adv(adv/[-wann]) 	--> [wann].
adv(adv/[-wie]) 	--> [wie].
adv(adv/[-warum]) 	--> [warum].
adv(adv/[-wozu]) 	--> [wozu].
adv(adv/[-womit]) 	--> [womit].

% kausal
adv(adv/[-deshalb])  --> [deshalb].
adv(adv/[-dennoch])  --> [dennoch].
adv(adv/[-naemlich]) --> [naemlich].

%----------------------------------
% PrŠpositionen
%----------------------------------

praeposition(mit,[regiert(dat)]).
praeposition(an,[regiert(akk)]).
praeposition(an,[regiert(dat)]).
praeposition(in,[regiert(akk)]).
praeposition(in,[regiert(dat)]).
praeposition(auf,[regiert(akk)]).
praeposition(auf,[regiert(dat)]).
praeposition(unter,[regiert(akk)]).
praeposition(unter,[regiert(dat)]).

%----------------------------------
% Eigennamen
%----------------------------------

% Namen
eigenname(john,Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname(johns,gen).

eigenname(lilly,Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname(lillys,gen).

eigenname(maria,Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname(marias,gen).

eigenname(leo,Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname(leos,gen).

% StŠdte

eigenname(berlin,Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname(berlins,gen).

eigenname(hamburg,Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname(hamburgs,gen).

eigenname(paris,Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname(parises,gen).

% LŠnder
eigenname(deutschland,Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname(deutschlands,gen).

eigenname(frankreich,Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname(freankreichs,gen).

eigenname(vietnam,Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname(vietnams,gen).

% Peršnlichkeiten

eigenname('angela merkel',Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname('angela merkels',gen).

eigenname('friedrich schiller',Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname('friedrich schillers',gen).

eigenname('franz liszt',Kasus) :- member(Kasus,[nom,akk,dat]).
eigenname('franz liszts',gen).

%----------------------------------
% Konjunktionen
%----------------------------------

konj(konj/[-und])		--> [und].
konj(konj/[-weil])		--> [weil].
konj(konj/[-dass])		--> [dass].
konj(konj/[-obwohl])	--> [obwohl].
konj(konj/[-ob])		--> [ob].
konj(konj/[-als])	  	--> [als].
konj(konj/[-weshalb]) 	--> [weshalb].

%----------------------------------
%----------------------------------
%----------------------------------


% Die rechte Satzklammer entsteht durch Verben, die von einander abhŠngig sind, bzw. zusammengesetzten
% Verben. Die Beziehungen wŠren so Šhnlich wie bei Det und N, egal was dazwischen steht, das kann nur in
% Verbindung mit einem Neutrum stehen.

% ? parse([john,sieht,das,heute,buch],D),write(D),nl,fail.
% [s/[vorfeld/[en/[-john]],lk/[v/[-sieht],mittelfeld/[art/[-das],adv/[-heute],n/[-buch]]]]]
% false.

%----------------------------------
% leider kam ich nicht mehr dazu die Regel komplett umzuformen.
% Zudem fehlen noch die Adjektive, die rekusiv definiert werden mŸssen wie: "die wunder wunder schšne Blume".
% Auch fehlt das Nachfeld, welches durch die Konjunktionen beginnt, wobei der folgende Nebensatz auch
% auf seine Topologischen Felder analysiert werden sollte.
%----------------------------------



%mittelfeld(mittelfeld / [ArtBaum,Adv,NBaum],agr(3,_Numerus,Kasus)) -->
%	art(ArtBaum,KNG),
%	adv(Adv),
%	n(NBaum,KNG),	  
%	{ KNG = kng(Kasus,Numerus,_) }.



% lk_mittelfeld2(lk /[VBaum,NPBaum,V2Baum],Merkmale) -->
%	v1(VBaum,Merkmale,[regiert(Kasus)],[teil(A)]),	
%	mittelfeld(NPBaum,agr(_,_,Kasus)),
%	v2(V2Baum,Merkmale,[regiert(Kasus)],teil(A)).	



%----------------------------------
% Umformungsregel in die DCG
%----------------------------------

% v1(v1/[-Verb],Merkmale,Valenz,Valenz2) --> [Verb], {verb1(Verb,Merkmale,Valenz,Valenz2)}.
% v2(v2/[-Verb],Merkmale,Valenz,Valenz2) --> [Verb], {verb2(Verb,Merkmale,Valenz,Valenz2)}.

%----------------------------------
% LEXIKON Verkomplex
%----------------------------------

% verb1(prueft,fin(3,sg,praes,ind),[regiert(akk)],[teil(ueber)]).
% verb1(pruefen,fin(1,pl,praes,ind),[regiert(akk)],[teil(ueber)]).
% verb1(pruefen,fin(3,pl,praes,ind),[regiert(akk)],[teil(ueber)]).


% verb1(sieht,fin(3,sg,praes,ind),[regiert(akk)],[teil(an)]).
% verb1(sehen,fin(1,pl,praes,ind),[regiert(akk)],[teil(an)]).
% verb1(sehen,fin(3,pl,praes,ind),[regiert(akk)],[teil(an)]).

% verb1(trifft,fin(3,sg,praes,ind),[regiert(akk)],[teil(an)]).
% verb1(treffen,fin(1,pl,praes,ind),[regiert(akk)],[teil(an)]).
% verb1(treffen,fin(3,pl,praes,ind),[regiert(akk)],[teil(an)]).

% verb2(an,fin(3,sg,praes,ind),[regiert(akk)],[teil(an)]).
% verb2(an,fin(1,pl,praes,ind),[regiert(akk)],[teil(an)]).
% verb2(an,fin(3,pl,praes,ind),[regiert(akk)],[teil(an)]).

% verb1(ueber,fin(3,sg,praes,ind),[regiert(akk)],[teil(ueber)]).
% verb1(ueber,fin(1,pl,praes,ind),[regiert(akk)],[teil(ueber)]).
% verb1(ueber,fin(3,pl,praes,ind),[regiert(akk)],[teil(ueber)]).
