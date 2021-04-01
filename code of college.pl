% Doctor amer salam

department(amer_salam,'Network and Distributing Organize Engineering').
speciaization(amer_salam,'Network Engineering').
scholarliness_degree(amer_salam,doctoral).
college(amer_salam,'Engineer College').
phone(amer_salam,739937721).
e_mail(amer_salam,'amer.salam@gmail.com').
age(amer_salam,45).

% Doctor  abd_altwab_saef

department_abd(abd_altwab_saef,'Communication Engineering').
speciaization_abd(abd_altwab_saef,'Communication Engineering').
scholarliness_degree_abd(abd_altwab_saef,assistant_professor).
college_abd(abd_altwab_saef,'Engineer College').
phone_abd(abd_altwab_saef,770416863).
e_mail_abd(abd_altwab_saef,'abd_altwab_saef@gmail.com').
age_abd(abd_altwab_saef,65).


%Doctor Mojeeb Mosleh


department_mo(mojeeb_mosleh,'software engineering').
speciaization_mo(mojeeb_mosleh,'software engineering').
scholarliness_degree_mo(mojeeb_mosleh,co_professor).
college_mo(mojeeb_mosleh,'Engineer College').
phone_mo(mojeeb_mosleh,778448888).
phone_mo(mojeeb_mosleh,737322252).
e_mail_mo(mojeeb_mosleh,'mojeebmosleh@taiz.edu.ye').
age_mo(mojeeb_mosleh,47).

%Doctor Ameen Seif
department_am(ameen_seif,'mechatronics').
speciaization_am(ameen_seif,'power and control systems engineering').
scholarliness_degree_am(ameen_seif,assistant_professor).
college_am(ameen_seif,'Engineer College').
phone_am(ameen_seif,771173232).
e_mail_am(ameen_seif,'amyeod8@gmail.com').
age_am(ameen_seif,60).
%Doctor  Mojeeb Al-hakimi
department_ha(mojeeb_al-hakimi,'Network and Distributing Organize Engineering').
speciaization_ha(mojeeb_al-hakimi,'Network and Distributing Organize Engineering').
scholarliness_degree_ha(mojeeb_al-hakimi,doctor).
college_ha(mojeeb_al-hakimi,'Engineer College').
phone_ha(mojeeb_al-hakimi,774577077).
e_mail_ha(mojeeb_al-hakimi,'mojeeb_al-hakimi@gmail.com').
age_ha(mojeeb_al-hakimi,50).

%Doctor  Abdu ALghani Aheme
department_abdu( abdu_alghani_ahemd,'Industrial engineering and distribution systems').
speciaization_abdu(abdu_alghani_ahemd,'Nano-materuls/nanotechnology').
scholarliness_degree_abdu(abdu_aLghani_ahemd,assistant_professor).
college_abdu(abdu_alghani_ahemd,'Engineer College').
phone_abdu(abdu_aLghani_ahemd,777235553).
e_mail_abdu(abdu_alghani_ahemd,'abdu_alghani@gmail.com').
age_abdu(abdu_alghani_ahemd,50).



run :-  write('Our project ask you some questions for specific doctor
    according to your answer the program will gives you aspecific doctor'),
	nl,
	hypothesize(Doctor),
      write('I guess that the Doctor is: '),
      write(Doctor),
	nl,

	   ((Doctor == amer_salam)
    ->
     write('if you want more information about amer_salam'),
		nl,
	 write(' dep_amer(Doctor,Depart).
                 spe_amer(Doctor,Spe).
		 schol_amer(Doctor,Schol).
                 coll_amer(Doctor,Coll) .
                 phone_amer(Doctor,Phone).
                 e_mail_amer(Doctor,E_mail).
                 age_amer(Doctor,A_age).')   ;

    (Doctor == abd_altwab_saef)
     ->
      write('if you want more information about	amer_salam'),
		nl,
      write('    dep_abdd_altwab(Doctor,Depart).
                 spe_abdd_altwab(Doctor,Spe).
		 schol_abdd_altwab(Doctor,Schol).
                 coll_abdd_altwab(Doctor,Coll) .
                 phone_abdd_altwab(Doctor,Phone).
                 e_mail_abdd_altwab(Doctor,E_mail).
                  age_abdd_altwab(Doctor,A_age).');

	    (Doctor == mojeeb_mosleh)
     ->
      write('if you want more information about	amer_salam'),
		nl,
      write('    dep_mojeeb(Doctor,Depart).
                 spe_mojeeb(Doctor,Spe).
		 schol_mojeeb(Doctor,Schol).
                 coll_mojeeb(Doctor,Coll) .
                 phone_mojeeb(Doctor,Phone).
                 e_mail_mojeeb(Doctor,E_mail).
  age_mojeeb(Doctor,A_age).');


	    (Doctor == ameen_seif)
     ->
      write('if you want more information about	amer_salam'),
		nl,
      write('    dep_ameen(Doctor,Depart).
                 spe_ameen(Doctor,Spe).
		 schol_ameen(Doctor,Schol).
                 coll_ameen(Doctor,Coll) .
                 phone_ameen(Doctor,Phone).
                 e_mail_ameen(Doctor,E_mail).
 age_ameen(Doctor,A_age).');


          (Doctor ==mojeeb_al_hakimi )
     ->
      write('if you want more information about	amer_salam'),
		nl,
      write('    dep_mogee_hakimi(Doctor,Depart).
                 spe_mogee_hakimi(Doctor,Spe).
		 scho_mogee_hakimi(Doctor,Schol).
                 coll_mogee_hakimi(Doctor,Coll) .
                 phone_mogee_hakimi(Doctor,Phone).
                 e_mail_mogee_hakimi(Doctor,E_mail).
                 age_mogee_hakimi(Doctor,A_age).');


	    (Doctor == abdu_alghani_ahemd)
     ->
      write('if you want more information about	amer_salam'),
		nl,
      write('    dep_abdualghani(Doctor,Depart).
                 spe_abdualghani(Doctor,Spe).
		 scho_abdualghani(Doctor,Schol).
                 coll_abdualghani(Doctor,Coll) .
                 phone_abdualghani(Doctor,Phone).
                 e_mail_abdualghani(Doctor,E_mail).
                 age_abdualghani(Doctor,A_age).')


	   ),


      nl,
      undo.



hypothesize(amer_salam)   :- amer_salam, !.
hypothesize(abd_altwab_saef)   :- abd_altwab_saef, !.
hypothesize(mojeeb_mosleh)  :- mojeeb_mosleh, !.
hypothesize(ameen_seif)   :-ameen_seif, !.
hypothesize(mojeeb_al_hakimi)   :-mojeeb_al_hakimi, !.

hypothesize(abdu_alghani_ahemd)   :-abdu_alghani_ahemd, !.
hypothesize(unknown).

/* no diagnosis */


amer_salam       :- college ,language ,
                    verify(germany_graduate).

abd_altwab_saef :- college ,language ,
                    verify(russia_graduate).

mojeeb_mosleh:-college ,language ,
                    verify(malaysia_graduate).

ameen_seif:- college ,language ,
                    verify(italia_graduate).
mojeeb_al_hakimi:- college ,language ,
                    verify(india_graduate).
abdu_alghani_ahemd:- college ,language ,
                    verify(china_graduate).


dep_amer(Doctor,Depart) :-department(Doctor,Depart).
spe_amer(Doctor,Spe)    :-speciaization(Doctor,Spe).
schol_amer(Doctor,Schol):-scholarliness_degree(Doctor,Schol).
coll_amer(Doctor,Coll)  :-college(Doctor,Coll).
phone_amer(Doctor,Phone):-phone(Doctor,Phone).
e_mail_amer(Doctor,E_mail):-e_mail(Doctor,E_mail).
age_amer(Doctor,A_age):-age(Doctor,A_age).


dep_abdd_altwab(Doctor,Depart) :-department_abd(Doctor,Depart).
spe_abdd_altwab(Doctor,Spe)    :-speciaization_abd(Doctor,Spe).
schol_abdd_altwab(Doctor,Schol):-scholarliness_degree_abd(Doctor,Schol).
coll_abdd_altwab(Doctor,Coll)  :-college_abd(Doctor,Coll).
phone_abdd_altwab(Doctor,Phone):-phone_abd(Doctor,Phone).
e_mail_abdd_altwab(Doctor,E_mail):-e_mail_abd(Doctor,E_mail).
age_abdd_altwab(Doctor,A_age):-age_abd(Doctor,A_age).

dep_mojeeb(Doctor,Depart) :-department_mo(Doctor,Depart).
spe_mojeeb(Doctor,Spe)    :-speciaization_mo(Doctor,Spe).
schol_mojeeb(Doctor,Schol):-scholarliness_degree_mo(Doctor,Schol).
coll_mojeeb(Doctor,Coll)  :-college_mo(Doctor,Coll).
phone_mojeeb(Doctor,Phone):-phone_mo(Doctor,Phone).
e_mail_mojeeb(Doctor,E_mail):-e_mail_mo(Doctor,E_mail).
age_mojeeb(Doctor,A_age):-age_mo(Doctor,A_age).

dep_ameen(Doctor,Depart) :-department_am(Doctor,Depart).
spe_ameen(Doctor,Spe)    :-speciaization_am(Doctor,Spe).
schol_ameen(Doctor,Schol):-scholarliness_degree_am(Doctor,Schol).
coll_ameen(Doctor,Coll)  :-college_am(Doctor,Coll).
phone_ameen(Doctor,Phone):-phone_am(Doctor,Phone).
e_mail_ameen(Doctor,E_mail):-e_mail_am(Doctor,E_mail).
age_ameen(Doctor,A_age):-age_am(Doctor,A_age).

dep_mogee_hakimi(Doctor,Depart) :-department_ha(Doctor,Depart).
spe_mogee_hakimi(Doctor,Spe)    :-speciaization_ha(Doctor,Spe).
schol_mogee_hakimi(Doctor,Schol):-scholarliness_degree_ha(Doctor,Schol).
coll_mogee_hakimi(Doctor,Coll)  :-college_ha(Doctor,Coll).
phone_mogee_hakimi(Doctor,Phone):-phone_ha(Doctor,Phone).
e_mail_mogee_hakimi(Doctor,E_mail):-e_mail_ha(Doctor,E_mail).
age_mogee_hakimi(Doctor,A_age):-age_ha(Doctor,A_age).

dep_abdualghani(Doctor,Depart) :-department_abdu(Doctor,Depart).
spe_abdualghani(Doctor,Spe)    :-speciaization_abdu(Doctor,Spe).
schol_abdualghani(Doctor,Schol):-scholarliness_degree_abdu(Doctor,Schol).
coll_abdualghani(Doctor,Coll)  :-college_abdu(Doctor,Coll).
phone_abdualghani(Doctor,Phone):-phone_abdu(Doctor,Phone).
e_mail_abdualghani(Doctor,E_mail):-e_mail_abdu(Doctor,E_mail).
age_abdualghani(Doctor,A_age):-age_abdu(Doctor,A_age).


/* classification rules */

gender    :- verify(male), !.
gender    :- verify(famle).

qualification       :- verify(professor), !.
qualification       :- verify(doctoral),!.
qualification       :- verify(assistant_professor),!.
qualification       :- verify(co_professor).

language :- verify(english), !.
language :- verify(arabic).


college :-  gender,qualification,
	    verify(president_department), !.
college  :- gender,qualification,
            verify(doctor).

/* how to ask questions */
ask(Question) :-
    write('Does the Doctor have the following attribute: '),
    write(Question),
    write('? '),
    read(Response),
    nl,
    ( (Response == yes ; Response == y)
      ->
       assert(yes(Question)) ;
       assert(no(Question)), fail).

:- dynamic yes/1,no/1.

/* How to verify something */
verify(S) :-
   (yes(S)
    ->
    true ;
    (no(S)
     ->
     fail ;
     ask(S))).

/* undo all yes/no assertions */
undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.
