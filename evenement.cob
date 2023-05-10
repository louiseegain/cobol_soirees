      ******************************************************************
      * Author: Louise EGAIN - Mathias LORET - Thomas MERLET - Camille LEAU
      * Date:Mai 2023
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. Evenements.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           select futilisateur assign to "utilisateur.dat"
           organization indexed
           access mode is dynamic
           record key is futil_login
           alternate record key IS futil_mail
           alternate record key IS futil_tel
      *    alternate record key is futil_genre WITH DUPLICATES
           ALTERNATE RECORD KEY IS futil_type WITH DUPLICATES
           ALTERNATE RECORD KEY IS futil_formation WITH DUPLICATES
           file status is cr_futil.


           select fevenement assign to "evenement.dat"
           organization indexed
           access mode is dynamic
           record key is fevent_nom
           alternate record key is fevent_type WITH DUPLICATES
           ALTERNATE RECORD KEY IS fevent_date WITH DUPLICATES
           alternate record key is fevent_loginOrga WITH DUPLICATES
           ALTERNATE RECORD KEY IS fevent_etat WITH DUPLICATES
           file status is cr_fevent.


           SELECT fparticipant ASSIGN TO "participant.dat"
           ORGANIZATION INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY fpart_cle
           ALTERNATE RECORD KEY IS fpart_login
           ALTERNATE RECORD KEY IS fpart_nomEvent WITH DUPLICATES
           ALTERNATE RECORD KEY IS fpart_etat WITH DUPLICATES
           FILE STATUS IS cr_fpart.

           SELECT fhistorique ASSIGN TO "historique.dat"
           ORGANIZATION INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY fhisto_nom
           ALTERNATE RECORD KEY fhisto_type WITH DUPLICATES
           ALTERNATE RECORD KEY fhisto_date WITH DUPLICATES
           ALTERNATE RECORD KEY fhisto_etat WITH DUPLICATES
           ALTERNATE RECORD KEY fhisto_participants WITH DUPLICATES
           FILE STATUS IS cr_fhisto.

      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD futilisateur.
       01 tamp_futi.
           02 futil_nom PIC A(30).
           02 futil_prenom PIC A(30).
           02 futil_login PIC X(30).
           02 futil_mdp PIC X(12).
           02 futil_mail PIC X(30).
           02 futil_tel PIC X(14).
      *     02 futil_genre PIC A(8).
           02 futil_type PIC A(10).
           02 futil_formation PIC A(20).
           02 futil_naissance PIC X(10).

       FD fevenement.
       01 tamp_fevent.
           02 fevent_nom PIC A(20).
           02 fevent_type PIC A(20).
           02 fevent_date PIC X(10).
           02 fevent_loginOrga PIC X(30).
           02 fevent_description PIC X(50).
           02 fevent_adresse PIC X(30).
           02 fevent_etat PIC A(8).
           02 fevent_seuil PIC 9(3).
           02 fevent_heure PIC X(5).

       FD fparticipant.
       01 tamp_fpart.
           02 fpart_cle.
               03 fpart_login PIC X(30).
               03 fpart_nomEvent PIC A(20).
           02 fpart_etat PIC A(10).

       FD fhistorique.
       01 tamp_fhisto.
           02 fhisto_nom PIC A(30).
           02 fhisto_type PIC A(20).
           02 fhisto_date PIC X(10).
           02 fhisto_loginOrga PIC X(30).
           02 fhisto_description PIC X(50).
           02 fhisto_adresse PIC X(50).
           02 fhisto_etat PIC A(8).
           02 fhisto_participants PIC 9(3).
      *-----------------------
       WORKING-STORAGE SECTION.
       77 cr_futil PIC 9(2).
       77 cr_fevent PIC 9(2).
       77 cr_fpart PIC 9(2).
       77 cr_fhisto PIC 9(2).
       77 choix PIC S9(1).

       77 verif_login pic X(30).
       77 chaine PIC X(30).
       77 lettre PIC A(1).
       77 I PIC 9(2).
       77 verif_arobase PIC 9(1).
       77 verif_mail_ok PIC 9(1).
       77 verif_tel_ok PIC 9(1).
       77 verif_login_ok PIC 9(1).
       77 verif PIC 9(1).

       77 login PIC X(30).
       77 mdp PIC X(30).

      **fonction supprimer futilisateur
       77 fdf pic 9(1).
       77 suppression_ok pic 9(1).
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **


      *-----------------------------------------------------------------
      *                  CREATION DES FICHIERS
      *-----------------------------------------------------------------
           OPEN I-O futilisateur
           IF cr_futil =35 THEN
               OPEN OUTPUT futilisateur
           END-IF
           CLOSE futilisateur

           OPEN I-O fevenement
           IF cr_fevent =35 THEN
               OPEN OUTPUT fevenement
           END-IF
           CLOSE fevenement

           OPEN I-O fparticipant
           IF cr_fpart =35 THEN
               OPEN OUTPUT fparticipant
           END-IF
           CLOSE fparticipant

           OPEN I-O fhistorique
           IF cr_fhisto =35 THEN
               OPEN OUTPUT fhistorique
           END-IF
           CLOSE fhistorique


      *-----------------------------------------------------------------
      *                  PROGRAMME PRINCIPAL
      *-----------------------------------------------------------------

           PERFORM accueil
            STOP RUN.
      *-----------------------------------------------------------------
      *                  FONCTIONS ET PROCEDURES
      *-----------------------------------------------------------------



      *accueil
       accueil.

           PERFORM WITH TEST AFTER UNTIL choix = 0

               DISPLAY "-----------------------------------------------"
               DISPLAY "|         BIENVENUE SUR L'APPLICATION         |"
               DISPLAY "-----------------------------------------------"
               DISPLAY "|  1 - Me connecter � mon compte              |"
               DISPLAY "|  2 - Cr�er mon compte                       |"
               DISPLAY "|  0 - quitter                                |"
               DISPLAY "-----------------------------------------------"
               DISPLAY "Taper votre choix :"
               ACCEPT choix


               IF choix = 1 THEN
                   DISPLAY "connexion � un compte"
                   PERFORM connexion
               ELSE IF choix = 2 THEN
                   DISPLAY "cr�ation d'un compte"
                   PERFORM creation_compte
               ELSE  IF choix = 0 THEN
                   DISPLAY "au revoir !"
               END-IF
           END-PERFORM.

      *creation_comptes
       creation_compte.

      ** ouverture du fichier
           OPEN I-O futilisateur

      **enregistrement des informations dans ke tampon
           DISPLAY "Entrer votre nom :"
           ACCEPT futil_nom
           DISPLAY "Entrer votre pr�nom :"
           ACCEPT futil_prenom

      **verification du format xxxxx@xxxx.fr ou xxxxx@xxxx.com
           PERFORM WITH TEST AFTER UNTIL verif_mail_ok EQUAL 1
               DISPLAY "Entrer votre adresse mail:"
               ACCEPT futil_mail
               PERFORM verif_mail
           END-PERFORM

      ** verification du format d'un nombre de 10 chiffres pour le tel
           PERFORM WITH TEST AFTER UNTIL verif_tel_ok EQUAL 1
               DISPLAY "Entrer votre numeros de telephone:"
               ACCEPT futil_tel
               PERFORM verif_tel
           END-PERFORM

           DISPLAY "Entrer le nom de votre formation:"
           ACCEPT futil_formation
           DISPLAY "Entrer votre date de naissance :"
           ACCEPT futil_naissance

      **verification que le login n'existe pas d�j�
           MOVE 0 TO verif_login_ok
           PERFORM UNTIL verif_login_ok EQUAL 1
               DISPLAY "Entrer votre login :"
               ACCEPT futil_login
               IF futil_login NOT EQUAL SPACE THEN
                   READ futilisateur
                       INVALID KEY
                           MOVE 1 TO verif_login_ok
                       NOT INVALID KEY
                           DISPLAY "ce login existe d�j�!"
                   ENd-READ
               ELSE
                   DISPLAY "le login ne peut pas etre vide"
               END-IF
           END-PERFORM



           MOVE 0 TO verif
           PERFORM UNTIL verif EQUAL 1
               DISPLAY "Entrer votre mot de passe :"
               ACCEPT futil_mdp
               IF futil_mdp NOT EQUAL SPACE THEN
                   MOVE 1 TO verif
               ELSE
                   DISPLAY "le mot de passe ne peut pas etre vide"
               END-IF
           END-PERFORM

      **on insere les informations dans le fichier
           WRITE tamp_futi
               INVALID KEY
                   DISPLAY "compte non cr�� : un probl�me est survenu"
               NOT INVALID KEY
                   DISPLAY "compte cr��"
           END-WRITE.
               DISPLAY cr_futil
           CLOSE futilisateur.

       verif_mail.
           MOVE 0 TO verif_arobase
           MOVE 1 TO verif_mail_ok
           MOVE 1 TO I
           MOVE futil_mail TO chaine

           STRING chaine ' ' INTO chaine

           PERFORM UNTIL chaine(I:1) EQUAL SPACE OR EQUAL '@'
               ADD 1 TO I
           END-PERFORM

           IF chaine(I:1) EQUAL '@' THEN
               ADD 1 TO I
               PERFORM UNTIL chaine(I:1) EQUAL SPACE
                   OR EQUAL '.'
                   OR EQUAL '@'
                       ADD 1 TO I
               END-PERFORM

               IF chaine(I:1) EQUAL '@' OR chaine(I:1) EQUAL SPACE THEN
                   MOVE 0 TO verif_mail_ok
               END-IF

               ADD 1 TO I

               IF chaine(I:3) NOT EQUAL 'fr'
                   AND chaine(I:3) NOT EQUAL "com" THEN
                       MOVE 0 TO verif_mail_ok
               ELSE
                   IF chaine(I:3) EQUAL "fr" THEN
                       ADD 2 TO I
                   ELSE IF chaine(I:3) EQUAL "com" THEN
                       ADD 3 TO I
                   END-IF
                   IF chaine(I:1) NOT EQUAL SPACE THEN
                       MOVE 0 TO verif_mail_ok
                   END-IF
               END-IF
           END-IF.


       verif_tel.
           MOVE 1 TO I
           MOVE 1 TO verif_tel_ok
           DISPLAY "futil_tel test"
           PERFORM UNTIL futil_tel(I:1) EQUAL SPACE OR I EQUAL 11
                   DISPLAY "letttre =" futil_tel(I:1)
                   IF futil_tel(I:1)
                   NOT EQUAL 0
                   AND NOT EQUAL 1
                   AND NOT EQUAL 2
                   AND NOT EQUAL 3
                   AND NOT EQUAL 4
                   AND NOT EQUAL 5
                   AND NOT EQUAL 6
                   AND NOT EQUAL 7
                   AND NOT EQUAL 8
                   AND NOT EQUAL 9 THEN
                   MOVE 0 TO verif_tel_ok
                END-IF
                ADD 1 TO I
           END-PERFORM
           ADD 1 TO I
           DISPLAY 'letttre suivante =' futil_tel(I:1)
           IF futil_tel(I:1) NOT EQUAL SPACE THEN
               MOVE 0 TO verif_tel_ok
           END-IF.

       connexion.
           MOVE SPACE TO futil_mdp
           MOVE SPACE TO futil_login
           MOVE 0 TO verif
           PERFORM UNTIL verif EQUAL 1
               DISPLAY "entrer votre login:"
               ACCEPT login
               DISPLAY "entrer votre mot de passe:"
               ACCEPT mdp

               IF mdp NOT EQUAL SPACE AND login NOT EQUAL SPACE THEN
                   MOVE login TO futil_login
                   MOVE mdp TO futil_mdp
                   OPEN INPUT futilisateur
                   READ futilisateur
                       INVALID KEY
                           DISPLAY "erreur dans la saisie du login"
                       NOT INVALID KEY
                           DISPLAY tamp_futi
                           IF futil_mdp EQUAL mdp THEN
                               MOVE 1 TO verif
                           ELSE
                               DISPLAY "erreur de mot de passe"
                           END-IF
                   ENd-READ
                   CLOSE futilisateur

               ELSE
                   DISPLAY "mot de passe et login ne peuvent etre vide"
               END-IF

           END-PERFORM.

       suppression_utilisateur.
      **pour cette fonction nous avons besoin de verifier que
      ** l'utilisateur n'est inscrit � aucun evenement et n'en organise
      ** aucun.
      ** le login de l'utilisateur � supprimer doit etre dans futil_login
           open input fevenement
           move 1 to fdf
           move 0 to suppression_ok

      ** on verifie s'il organise un evenement
           move futil_login to fevent_loginOrga
           start fevenement , key is = fevent_loginOrga
               not invalid key
                   move 1 to suppression_ok
                   display "impossible de supprimer cet utilisateur"
                   display "il organise un evenement"
           END-START
           close fevenement

      ** s'il n'organise aucun evenement, alors on va verifier qu'il
      ** n'est pas inscrit � un evenement
           if suppression_ok is equal 0 then
               move futil_login to fpart_login
               open input fparticipant
               start fparticipant , key is = fpart_login
                  not invalid key
                   move 1 to suppression_ok
                   display "impossible de supprimer cet utilisateur"
                   display "il est inscrit a un evenement"
               END-START
               close fparticipant
           end-if

           if suppression_ok is equal 0 then
              open i-o futilisateur
      ** faire la suppression de l'utilisateur
               read futilisateur
               INVALID key
                   display"Impossible de supprim� ce compte"
                   display "sdfsfdgs"
               not invalid KEY
                   delete futilisateur record
                   display 'utilisateur supprim�'
               END-READ
              close futilisateur
           end-if.

      ** fonction d'affichage de tout les organisateurs d'�venement
           affichage_organisateur.
               open input futilisateur
               open input fevenement

               move 1 to fdf

               perform with test after until fdf=0
                   read futilisateur
                   at end move 0 to fdf
                   not at end
                       move futil_login to fevent_loginOrga
                       start fevenement , key is = fevent_loginOrga
                           not invalid key
                               display futil_login
                       END-START
                   END-READ
               end-perform

               close futilisateur
               close fevenement.
      ** add other procedures here
       END PROGRAM Evenements.
