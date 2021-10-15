:- ensure_loaded('checker.pl').

test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un string, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de stringuri reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un string)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, ((R, C), x/lit/List), +Vocab), -Lista_intrebari)

% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.

is_char(X):- string(X).

is_l([L]):- is_list(L).


intrebari(integ(_, _, [], _), []).
intrebari(integ(H, W, [((R, C), [(Text, Dir, ID)]) | T], Vocab), [((R, C), Text, Dir, ID) | Res]):-intrebari(integ(H, W, T, Vocab), Res).
intrebari(integ(H, W, [((R, C), [(Text1, Dir1, ID1), (Text2, Dir2, ID2)]) | T], Vocab), [((R, C), Text1, Dir1, ID1), ((R, C), Text2, Dir2, ID2) | Res]):-intrebari(integ(H, W, T, Vocab), Res).
intrebari(integ(H, W, [((_, _), x) | T], Vocab), Res):-intrebari(integ(H, W, T, Vocab), Res).
intrebari(integ(H, W, [((_, _), X) | T], Vocab), Res):-is_char(X) -> intrebari(integ(H, W, T, Vocab), Res).



% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.

id_intrebare(integ(_, _, [], _), _, _) :- false.
id_intrebare(integ(_, _, [((_, _), [])], _), _, _) :- false.
id_intrebare(integ(_, _, [((_, _), [(Text, _, ID)]) | _], _), Text, ID).
id_intrebare(integ(H, W, [((_, _), [(_, _, _)]) | T], Vocab), TextTarget, IDTarget):-id_intrebare(integ(H, W, T, Vocab), TextTarget, IDTarget).
id_intrebare(integ(_, _, [((_, _), [(Text1, _, ID1), (_, _, _)]) | _], _), Text1, ID1).
id_intrebare(integ(_, _, [((_, _), [(_, _, _), (Text2, _, ID2)]) | _], _), Text2, ID2).
id_intrebare(integ(H, W, [((_, _), [(_, _, _), (_, _, _)]) | T], Vocab), TextTarget, IDTarget):-id_intrebare(integ(H, W, T, Vocab), TextTarget, IDTarget).
id_intrebare(integ(H, W, [((_, _), x) | T], Vocab), TextTarget, IDTarget):-id_intrebare(integ(H, W, T, Vocab), TextTarget, IDTarget), !.
id_intrebare(integ(H, W, [((_, _), _) | T], Vocab), TextTarget, IDTarget):-id_intrebare(integ(H, W, T, Vocab), TextTarget, IDTarget).

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvânt
% de completat; ambele sunt stringuri.
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).


% caut in lista pe care mi-o returneaza functia intrebari
% caut intrebarea din solutie
% cand am gasit-o ii iau raspunsul si-l sparg in caractere si directia si (R, C)
% pe baza pozitiei (R, C) returnate, de cautarea anterioara, adaug
% o noua intrare in integrama

% Extrage pe baza raspunsului (R, C), Dir
extract_answer(X, Text, ((R, C), Dir)):- intrebari(X, Res), member(((R, C), Text, Dir, _), Res).

% Formeaza, pe baza listei Sol o lista de forma [((R, C), Dir)]
extract_all_answers(integ(_, _, _, _), [], []).
extract_all_answers(integ(H, W, List, Vocab), [(Text, _) | T], [((R, C), Dir) | Res]):- extract_answer(integ(H, W, List, Vocab), Text, ((R, C), Dir)),
                                                                                          extract_all_answers(integ(H, W, List, Vocab), T, Res).

% Primeste o lista de caractere
% Completeaza pe baza unui (R, C) si Dir dat o lista de ((R, C), litera)
% Intoarce o lista cu acestea
complete_answer([], ((_, _), d), []).
complete_answer([Lit | T], ((R, C), d), [((R, C1), Lit) | LCaract]):- C1 is (C + 1), complete_answer(T, ((R, C1), d), LCaract).

complete_answer([], ((_, _), j), []).
complete_answer([Lit | T], ((R, C), j), [((R1, C), Lit) | LCaract]):- R1 is (R + 1), complete_answer(T, ((R1, C), j), LCaract).

% Completeaza o lista ((R, C), Litera) pe baza unui answer primit ca si parametru
add_one_ans_list(((R, C), Dir), Answer, LCaract):- atom_chars(Answer, Lit), complete_answer(Lit, ((R, C), Dir), LCaract).


% Formeaza pe baza listei Sol o lista cu toate raspunsurile
answers([], []).
answers([(_, Ans) | T], [Ans | AnsL]):- answers(T, AnsL).

% Apeleaza pe head-ul listei de answers si pe head-ul listei de ((R, C), Dir)
% primita in extract_all_answers, functia add_one_ans_list si intoarce o lista cu 
% ((R, C), Litera)) avand toate literele ce trebuie adaugate pentru toate
% raspunsurile din lista Sol
% Am nevoie de:
% lista cu raspunsuri din answers
% lista intoarsa de extract_all_answers
add_all_ans_list([], [], []).                                                                                        
add_all_ans_list([H | Res], [A1 | AnsL], [LCaract | AllC]):- add_one_ans_list(H, A1, LCaract), add_all_ans_list(Res, AnsL, AllC).

% Scoate elementele duplicate
% credits: https://stackoverflow.com/questions/39435709/how-to-remove-duplicates-from-a-list-in-swi-prolog
remove_duplicates([],[]).
remove_duplicates([H | T], List) :- member(H, T), remove_duplicates(T, List).
remove_duplicates([H | T], [H | T1]) :- \+member(H, T), remove_duplicates(T, T1).



% Construieste o lista cu elemente simple, dintr-o lista de liste
% [[a, b], [c], [d]] -> [a, b, c, d]
myAppend([], []).
myAppend([H | LL], NewRes):- myAppend(LL, Res), append(H, Res, NewRes).

% AnsL = lista de raspunsuri din solutie creata cu functia answers
% Res = lista de forma [((R, C), Dir)] creata cu extract_all_answers
% AllC = o lista cu toate literele ce trebuie adaugate pe baza
% listei de answers cu add_all_ans_list, de forma [((R, C), Lit)]
% concatenez la lista rezultata din add_all_ans_list
completare(integ(H, W, List, Vocab), Sol, integ(H, W, NewList, Vocab)):- answers(Sol, AnsL), extract_all_answers(integ(H, W, List, Vocab), Sol, Res), 
                                                                         add_all_ans_list(Res, AnsL, AllC),
                                                                         myAppend(AllC, LAllC),
                                                                         remove_duplicates(LAllC, NewAllC),
                                                                         append(NewAllC, List, NewList).

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).



% extrag pe baza textului intrebarii ((R, C), Dir)
extract_dim_question(integ(H, W, List, Vocab), Intrebare, ((R, C), Dir)):- extract_answer(integ(H, W, List, Vocab), Intrebare, ((R, C), Dir)). 

% caut in lista din integrama in functie de directie, daca pe pozitie exista un element, cresc
% dim col/liniei in functie de directie
% se opreste cand il gaseste in lista sau cand iese din dimensiunea tablei
find_occupied_space(H, W, ((R, C), d), List, (R, C2)):- C1 is (C + 1), \+member(((R, C1), x), List), \+member(((R, C1), [_ | _]), List), C1 < W, 
                                                        find_occupied_space(H, W, ((R, C1), d), List, (R, C2)), !.
find_occupied_space(_, _, ((R, C), d), _, (R, C)).


find_occupied_space(H, W, ((R, C), j), List, (R2, C)):- R1 is (R + 1), \+member(((R1, C), x), List), \+member(((R1, C), [_ | _]), List), R1 < H, 
                                                        find_occupied_space(H, W, ((R1, C), j), List, (R2, C)), !.
find_occupied_space(_, _, ((R, C), j), _, (R, C)).


% calculeaza lungimea raspunsului in functie de directie
calculate_length(((R, C), d), (R, C1), Dim):- Dim is (C1 - C).
calculate_length(((R, C), j), (R1, C), Dim):- Dim is (R1 - R).


lungime_spatiu(integ(H, W, List, Vocab), Intrebare, Lungime):- extract_dim_question(integ(H, W, List, Vocab), Intrebare, ((R, C), Dir)),
                                                               find_occupied_space(H, W, ((R, C), Dir), List, (R1, C1)),
                                                               calculate_length(((R, C), Dir), (R1, C1), Lungime).

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).

extract_answer_id(X, Text, ((R, C), Dir, ID)):- intrebari(X, Res), member(((R, C), Text, Dir, ID), Res).

% extrage ((R, C), Dir) pe baza celor doua intrebari
extract_pos(integ(H, W, List, Vocab), Intrebare1, Intrebare2, ((R1, C1), Dir1, ID1), ((R2, C2), Dir2, ID2)) :- extract_answer_id(integ(H, W, List, Vocab), Intrebare1, ((R1, C1), Dir1, ID1)),
                                                                                                               extract_answer_id(integ(H, W, List, Vocab), Intrebare2, ((R2, C2), Dir2, ID2)).

cond_intersection(((R1, C1), d), Len1, ((R2, C2), d), Len2, Ind1, Ind2):- false, !.
cond_intersection(((R1, C1), j), Len1, ((R2, C2), j), Len2, Ind1, Ind2):- false, !.
cond_intersection(((R1, C1), d), Len1, ((R2, C2), j), Len2, Ind1, Ind2):- Fin1 is (Len1 + C1), Fin2 is (Len2 + R2), C2 > C1, C2 =< Fin1,
                                                                          R2 < R1, Fin2 >= R1,
                                                                          Ind1 is (C2 - C1 - 1), Ind2 is (R1 - R2 - 1).
cond_intersection(((R1, C1), j), Len1, ((R2, C2), d), Len2, Ind1, Ind2):-  Fin1 is (Len1 + R1), Fin2 is (Len2 + C2), R1 < R2, Fin1 >= R2,
                                                                          C1 > C2, C1 =< Fin2,
                                                                           Ind1 is (R2 - R1 - 1), Ind2 is (C1 - C2 - 1).



intersectie(integ(H, W, List, Vocab), I1, Poz1, I2, Poz2) :- extract_pos(integ(H, W, List, Vocab), I1, I2, ((R1, C1), Dir1, ID1), ((R2, C2), Dir2, ID2)),
                                                             lungime_spatiu(integ(H, W, List, Vocab), I1, Len1), 
                                                             lungime_spatiu(integ(H, W, List, Vocab), I2, Len2),
                                                             cond_intersection(((R1, C1), Dir1), Len1, ((R2, C2), Dir2), Len2, Poz1, Poz2).

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de stringuri, fiecare string
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])


% Construieste o lista cu lungimilor tuturor raspunsurilor la intrebari
find_length(_, [], []).
find_length(integ(H, W, List, Vocab), [Ques | Intrebari], [Ans | Lungimi]):- lungime_spatiu(integ(H, W, List, Vocab), Ques, Ans),
                                                                             find_length(integ(H, W, List, Vocab), Intrebari, Lungimi).

% Creeaza din Vocabularul ['DA', 'NU'], Vocabularul [['D', 'A'], ['N','U']].
create_atom_ans([], []).
create_atom_ans([Ans | Vocab], [NewAns | NewVocab]):- atom_chars(Ans, NewAns),
                                                      create_atom_ans(Vocab, NewVocab).

% Calculeaza lungimea unei liste
list_length([], 0).
list_length([_ | T] , L):- list_length(T, N), L is N + 1 .
                                                
% Creez lista de intrebari, pe baza listei returnate de functia intrebari
create_questions_length(_, [], []).
create_questions_length(integ(H, W, List, Vocab), [((R, C), Text, Dir, ID) | Res], [Text | Intrebari]):- create_questions_length(integ(H, W, List, Vocab), Res, Intrebari).
                                                                                     

% Filtreaza noul Vocabular dupa lungime
% Adauga intr-o lista doar acele raspunsuri cu o dimensiune Len data
% Primeste un vocabular format din lista de liste si formeaza pe baza
% unei lungimi date, ca si parametru o lista cu raspunsuri de acea lungime
filter_vocabulary([], _, []).
filter_vocabulary([Ans | NewVocab], Len, [Ans | NewList]):- list_length(Ans, Len1), Len == Len1,
                                                            filter_vocabulary(NewVocab, Len, NewList).
filter_vocabulary([Ans | NewVocab], Len, NewList):- list_length(Ans, Len1), Len \= Len1,
                                                    filter_vocabulary(NewVocab, Len, NewList).


% Pe baza listei de lungimi si de intrebari fac o lista de forma [(intrebare, [raspunsuri])]
iterate_length([], [], _, []).
iterate_length([I | Intrebari], [L | Lungimi], NewVocab, [(I, Ans) | PairList]):- filter_vocabulary(NewVocab, L, Ans),
                                                                                  iterate_length(Intrebari, Lungimi, NewVocab, PairList).
                                                                     

% Creez listele de intrebari cu functia create_questions_length
% pe baza listei primite de la functia intrebari
% Apelez find_length imi intoarce o lista cu toate lungimile raspunsurilor intrebarilor
% Convertesc vocabularul in lista de liste
% Parcurg simultan lista de lungimi si de intrebari si cu functia filter_vocabulary, care
% imi returneaza un raspuns din vocabular pe baza unei lungimi date construiesc perechi de
% forma [(intrebare, [raspunsuri]) | ]
solutii_posibile(integ(H, W, List, Vocab), Solutii) :- intrebari(integ(H, W, List, Vocab), Res),
                                                       create_questions_length(integ(H, W, List, Vocab), Res, Intrebari),
                                                       find_length(integ(H, W, List, Vocab), Intrebari, Lungimi),
                                                       create_atom_ans(Vocab, NewVocab),
                                                       iterate_length(Intrebari, Lungimi, NewVocab, Solutii).


% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de stringuri, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca string) care este
% răspunsul la întrebare.

% generam toate solutiile cu solutii_posibile
% luam o pereche(intrebare, [raspunsuri])

rezolvare(_, _) :- false.
