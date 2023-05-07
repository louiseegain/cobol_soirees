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
       77 Fin PIC 9(1).
       77 verif_event PIC 9(1).
       77 fin_boucle PIC 9(1).
       77 choix_gestion_demande PIC 9(1).
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
      *                  CREATION JDD bis
      *-----------------------------------------------------------------
      * Creation des utilisateurs
      * Compte admin temp (KIWIZ)
       MOVE "tmerlet" to futil_login
       MOVE "mdp" to futil_mdp
       MOVE "MERLET" to futil_nom
       MOVE "Thomas" to futil_prenom
       MOVE "thomas.merlet@gmail.tom" TO futil_mail
       MOVE "admin" TO futil_type
       MOVE "sociologie chomage" TO futil_formation
       MOVE "31/02/1999" TO futil_naissance

       OPEN I-O futilisateur
       WRITE tamp_futi
       END-WRITE
       IF cr_futil = 35 THEN
           DISPLAY "Echec d'insertion"
       ELSE
           DISPLAY "Insertion reussie"
           DISPLAY cr_futil
       END-IF
       CLOSE futilisateur

      * Utilisateur lambda 1
       MOVE "lambda1" to futil_login
       MOVE "mdp" to futil_mdp
       MOVE "LAMBDA" to futil_nom
       MOVE "Pierre" to futil_prenom
       MOVE "lambda.lambda@gmail.gom" TO futil_mail
       MOVE "membre" TO futil_type
       MOVE "Commerce" TO futil_formation
       MOVE "13/05/1973" TO futil_naissance

       OPEN OUTPUT futilisateur
       WRITE tamp_futi
       END-WRITE
       IF cr_futil = 35 THEN
           DISPLAY "Echec d'insertion"
       ELSE
           DISPLAY "Insertion reussie"
           DISPLAY cr_futil
       END-IF
       CLOSE futilisateur

      * Utilisateur lambda 2
       MOVE "lambda2" to futil_login
       MOVE "mdp" to futil_mdp
       MOVE "LAMBDA" to futil_nom
       MOVE "Steven" to futil_prenom
       MOVE "lambda2.lambda@gmail.mom" TO futil_mail
       MOVE "membre" TO futil_type
       MOVE "Geographie" TO futil_formation
       MOVE "26/11/1992" TO futil_naissance

       OPEN I-O futilisateur
       WRITE tamp_futi
       END-WRITE
       IF cr_futil = 35 THEN
           DISPLAY "Echec d'insertion"
       ELSE
           DISPLAY "Insertion reussie"
           DISPLAY cr_futil
       END-IF
       CLOSE futilisateur

      * Creation des events

       MOVE "Soiree kapplas" TO fevent_nom
       MOVE "Soiree" TO fevent_type
       MOVE "31/02/2024" TO fevent_date
       MOVE "tmerlet" TO fevent_loginOrga
       MOVE "Soiree gros fun" TO fevent_description
       MOVE "2 rue de l'atlantide, Rennes" TO fevent_adresse
       MOVE "a venir" TO fevent_etat
       MOVE 15 TO fevent_seuil
       MOVE "20h00" TO fevent_heure

       OPEN I-O fevenement
           WRITE tamp_fevent
           END-WRITE
           IF cr_fevent = 35 THEN
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion reussie"
               DISPLAY cr_fevent
           END-IF
           CLOSE fevenement

       MOVE "Triple marathon" TO fevent_nom
       MOVE "Course" TO fevent_type
       MOVE "02/07/2023" TO fevent_date
       MOVE "tmerlet" TO fevent_loginOrga
       MOVE "Petite course du matin" TO fevent_description
       MOVE "9 rue soldat inconnu, Nantes" TO fevent_adresse
       MOVE "a venir" TO fevent_etat
       MOVE 150 TO fevent_seuil
       MOVE "09h00" TO fevent_heure

       OPEN I-O fevenement
           WRITE tamp_fevent
           END-WRITE
           IF cr_fevent = 35 THEN
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion reussie"
               DISPLAY cr_fevent
           END-IF
       CLOSE fevenement.
      * Creation participations
      * participation 1
       MOVE "lambda1" TO fpart_login
       MOVE "Soiree kapplas" TO fpart_nomEvent
       MOVE "attente" TO fpart_etat

       OPEN I-O fparticipant
           WRITE tamp_fpart
           END-WRITE
           IF cr_fpart = 35 THEN
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion reussie"
               DISPLAY cr_fpart
           END-IF
       CLOSE fparticipant

      * participation 2
       MOVE "lambda2" TO fpart_login
       MOVE "Soiree kapplas" TO fpart_nomEvent
       MOVE "acceptee" TO fpart_etat
       OPEN I-O fparticipant
           WRITE tamp_fpart
           END-WRITE
           IF cr_fpart = 35 THEN
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion reussie"
               DISPLAY cr_fpart
           END-IF
       CLOSE fparticipant



      *-----------------------------------------------------------------
      *                  PROGRAMME PRINCIPAL
      *-----------------------------------------------------------------

      *    PERFORM accueil
           DISPLAY "KIWIZ"
      *     PERFORM gestion_demandes
           PERFORM supprimer_evenement
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
               DISPLAY "|  1 - Me connecter à mon compte              |"
               DISPLAY "|  2 - Créer mon compte                       |"
               DISPLAY "|  0 - quitter                                |"
               DISPLAY "-----------------------------------------------"
               DISPLAY "Taper votre choix :"
               ACCEPT choix


               IF choix = 1 THEN
                   DISPLAY "connexion à un compte"
                   PERFORM connexion
               ELSE IF choix = 2 THEN
                   DISPLAY "création d'un compte"
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
           DISPLAY "Entrer votre prénom :"
           ACCEPT futil_prenom

      **verification du format xxxxx@xxxx.fr ou xxxxx@xxxx.com
           PERFORM WITH TEST AFTER UNTIL verif_mail_ok EQUAL 1
               DISPLAY "Entrer votre adresse mail:"
               ACCEPT futil_mail
               PERFORM verif_mail
           END-PERFORM

      ** verification du format d'un nombre de 10 chiffres pour le tel
           PERFORM WITH TEST AFTER UNTIL verif_tel_ok EQUAL 1
               DISPLAY "Entrer votre numero de telephone:"
               ACCEPT futil_tel
               PERFORM verif_tel
           END-PERFORM

           DISPLAY "Entrer le nom de votre formation:"
           ACCEPT futil_formation
           DISPLAY "Entrer votre date de naissance :"
           ACCEPT futil_naissance

      **verification que le login n'existe pas déjà
           MOVE 0 TO verif_login_ok
           PERFORM UNTIL verif_login_ok EQUAL 1
               DISPLAY "Entrer votre login :"
               ACCEPT futil_login
               IF futil_login NOT EQUAL SPACE THEN
                   READ futilisateur
                       INVALID KEY
                           MOVE 1 TO verif_login_ok
                       NOT INVALID KEY
                           DISPLAY 'ce login existe déjà!'
                   ENd-READ
               ELSE
                   DISPLAY 'le login ne peut pas etre vide'
               END-IF
           END-PERFORM



           MOVE 0 TO verif
           PERFORM UNTIL verif EQUAL 1
               DISPLAY "Entrer votre mot de passe :"
               ACCEPT futil_mdp
               IF futil_mdp NOT EQUAL SPACE THEN
                   MOVE 1 TO verif
               ELSE
                   DISPLAY 'le mot de passe ne peut pas etre vide'
               END-IF
           END-PERFORM

      **on insere les informations dans le fichier
           WRITE tamp_futi
               INVALID KEY
                   DISPLAY 'compte non créé : un problème est survenu'
               NOT INVALID KEY
                   DISPLAY 'compte créé'
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
                   AND chaine(I:3) NOT EQUAL 'com' THEN
                       MOVE 0 TO verif_mail_ok
               ELSE
                   IF chaine(I:3) EQUAL 'fr' THEN
                       ADD 2 TO I
                   ELSE IF chaine(I:3) EQUAL 'com' THEN
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
           DISPLAY 'futil_tel test'
           PERFORM UNTIL futil_tel(I:1) EQUAL SPACE OR I EQUAL 11
                   DISPLAY 'letttre =' futil_tel(I:1)
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
               DISPLAY 'entrer votre login:'
               ACCEPT login
               DISPLAY 'entrer votre mot de passe:'
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
                               DISPLAY 'erreur de mot de passe'
                           END-IF
                   ENd-READ
                   CLOSE futilisateur

               ELSE
                   DISPLAY 'mot de passe et login ne peuvent etre vide '
               END-IF

           END-PERFORM.
      *-----------------------------------------------------------------
      *          Procedure prise sur br Louise (temporaire)
      *-----------------------------------------------------------------
           afficheEvent.
           DISPLAY"--------------------------------------------"
           DISPLAY"|          AFFICHAGE EVENEMENT             |"
           DISPLAY"--------------------------------------------"
           OPEN INPUT fevenement
               MOVE 0 TO Fin
               PERFORM WITH TEST AFTER UNTIL Fin = 1
               READ fevenement NEXT
               AT END
                   MOVE 1 TO Fin
               NOT AT END
      **           IF fevent_date>=WS-CURRENT-DATE
                       DISPLAY "Nom : "fevent_nom
                       DISPLAY "Type : "fevent_type
                       DISPLAY"----------"
      **             END-IF
               END-READ
               END-PERFORM
               CLOSE fevenement
           .

      **----------------------------------------------------------------
      *            Procedure prise sur br Louise (modifiee/temporaire)
      **----------------------------------------------------------------
           gestionEvenement.

           DISPLAY"--------------------------------------------"
           DISPLAY"|           GESTION D'EVENEMENT            |"
           DISPLAY"--------------------------------------------"
           MOVE 0 TO choix
           PERFORM WITH TEST AFTER UNTIL choix =0
               DISPLAY "1 - Creer un evenement"
               DISPLAY "2 - Modifier un evenement"
               DISPLAY "3 - Supprimer un evenement"
               DISPLAY "4 - Afficher evenement"
               DISPLAY "5 - Gerer demandes participation"
               DISPLAY "0 - Revenir au menu precedent"
               ACCEPT choix

            EVALUATE choix
      *         WHEN 1 PERFORM creerEvent
      *          WHEN 2 PERFORM modifierEvent
      *          WHEN 3 PERFORM supprimerEvent
      *         WHEN 4 PERFORM afficheEvent
      *         WHEN 0 PERFORM menuUtilisateur
                WHEN 5 PERFORM gestion_demandes
           END-EVALUATE

           END-PERFORM
           .

       gestion_demandes.
      *> Permet d'accepter ou refuser des demandes de participation.
      *> KIWIZ delete next line when connection fixed
           MOVE "tmerlet" TO login
           PERFORM afficheEvent
           MOVE 0 TO verif_event
           PERFORM UNTIL verif_event EQUAL 1
               DISPLAY "Saisissez le nom de l'evenement dont vous"
               DISPLAY "voulez voir les demandes :"
               ACCEPT fevent_nom
               OPEN INPUT fevenement
               READ fevenement
                   INVALID KEY
                       DISPLAY "Saisie invalide"
                   NOT INVALID KEY
                       IF fevent_loginOrga = login THEN
                           MOVE 1 TO verif_event
                       ELSE
                           OPEN INPUT futilisateur
                           MOVE login TO futil_login
                           READ futilisateur
                               INVALID KEY
                                   DISPLAY "Erreur lecture futilisateur"
                               NOT INVALID KEY
      ** utilisateur admin ou organisateur
                                   IF futil_type = 'admin' THEN
                                       MOVE 1 TO verif_event
                                   END-IF
                           END-READ
                           CLOSE futilisateur
                       END-IF

               CLOSE fevenement
           END-PERFORM

           OPEN INPUT fparticipant
           MOVE 0 TO fin_boucle
           MOVE fevent_nom TO fpart_nomEvent
           START fparticipant, KEY IS = fpart_nomEvent
           INVALID KEY
               DISPLAY "Pas de participants"
           NOT INVALID KEY
               PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
                   READ fparticipant NEXT
                   AT END
                       MOVE 1 TO fin_boucle
                   NOT AT END
                       IF fpart_etat = 'attente' THEN
                           DISPLAY "---"
                           DISPLAY "demande de : " fpart_login
                           DISPLAY "---"
                       END-IF
               END-PERFORM
           END-START
           CLOSE fparticipant

           OPEN I-O fparticipant
           MOVE fevent_nom TO fpart_nomEvent
      ** selection des demandes
           MOVE 0 TO fin_boucle
           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               DISPLAY "Saisissez le login de la personne dont vous"
               DISPLAY "souhaitez traiter la demande :"
               ACCEPT fpart_login

               READ fparticipant
                   INVALID KEY
                       DISPLAY "login incorrect"
                   NOT INVALID KEY
                       IF fpart_etat <> "attente" THEN
                           DISPLAY "Attention vous agissez"
                           DISPLAY "sur une demande qui n'est"
                           DISPLAY "pas en attente."

                       DISPLAY "Que souhaitez vous faire ?"
                       DISPLAY "0 - refuser la demande"
                       DISPLAY "1 - accepter la demande"
                       ACCEPT choix_gestion_demande
                       IF choix_gestion_demande = 0 THEN
                           IF fpart_etat <> "attente"
                           MOVE "refusee" TO fpart_etat
                       ELSE
                           MOVE "acceptee" TO fpart_etat
                       END-IF
                       REWRITE tamp_fpart
                       DISPLAY "Demande traitee"
               END-READ
               DISPLAY "Quitter ? (0 pour non, 1 pour oui)"
               ACCEPT fin_boucle
               CLOSE fparticipant
      ** KIWIZ renvoyer ici au menu quand il sera operationnel
           END-PERFORM
           .
       supprimer_evenement.
           MOVE 0 TO verif_event

           PERFORM afficheEvent
      * Selection de l'evenement a supprimer

      * KIWIZ la verif des droits peut être synthetisée (presente aussi dans gestion gestion_demandes
           PERFORM WITH TEST AFTER UNTIL verif_event = 1
               DISPLAY "Saisissez le nom de l'evenement a supprimer"
               ACCEPT fevent_nom
               OPEN I-O fevenement
               READ fevenement
                   INVALID KEY
                       DISPLAY "Saisie invalide"
                   NOT INVALID KEY
      * KIWIZ supprimmer ligne suivante apres implementation connexion valide
                       MOVE "tmerlet" TO login
                       IF fevent_loginOrga = login THEN
                           MOVE 1 TO verif_event
                       ELSE
                           OPEN INPUT futilisateur
                           MOVE login TO futil_login
                           READ futilisateur
                               INVALID KEY
                                   DISPLAY "Erreur lecture futilisateur"
                               NOT INVALID KEY
      ** utilisateur admin ou organisateur
                                   IF futil_type = 'admin' THEN
                                       MOVE 1 TO verif_event
                                   END-IF
                           END-READ
                           CLOSE futilisateur
                       END-IF
           END-PERFORM
      * l'évènement existe, on peut le supprimer mais on supprime ses participations avant
      * KIWIZ verif de suppression des participants

           MOVE fevent_nom TO fpart_nomEvent
           OPEN INPUT fparticipant
           START fparticipant, KEY IS = fpart_nomEvent
           INVALID KEY
               DISPLAY "Pas de participants"
           NOT INVALID KEY
               PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
                   READ fparticipant NEXT
                   AT END
                       DISPLAY "KIWIZ 5"
                       MOVE 1 TO fin_boucle
                   NOT AT END
                           DISPLAY "---"
                           DISPLAY "demande de : " fpart_login
                           DISPLAY "---"
               END-PERFORM
           END-START
           CLOSE fparticipant
           DISPLAY "Suppression des participations liees a l'evenement"
           OPEN INPUT fparticipant
           MOVE 0 TO fin_boucle
           MOVE fevent_nom TO fpart_nomEvent
           START fparticipant, KEY IS = fpart_nomEvent
           INVALID KEY
               DISPLAY "Erreur suppression participants"
           NOT INVALID KEY
               PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
                   READ fparticipant NEXT
                   AT END
                       MOVE 1 TO fin_boucle
                   NOT AT END
                       DELETE fparticipant RECORD
               END-PERFORM
           END-START
           CLOSE fparticipant

           DELETE fevenement RECORD
           DISPLAY "Suppression effectuee"
           PERFORM afficheEvent
           CLOSE fevenement
           OPEN INPUT fparticipant
           MOVE 0 TO fin_boucle
           MOVE fevent_nom TO fpart_nomEvent

      * KIWIZ verif de suppression des participants
           OPEN INPUT fparticipant
           START fparticipant, KEY IS = fpart_nomEvent
           INVALID KEY
               DISPLAY "Pas de participants"
           NOT INVALID KEY
               PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
                   READ fparticipant NEXT
                   AT END
                       DISPLAY "KIWIZ 6"
                       MOVE 1 TO fin_boucle
                   NOT AT END
                       DISPLAY "---"
                       DISPLAY "demande de : " fpart_login
                       DISPLAY "---"
               END-PERFORM
           END-START
           CLOSE fparticipant
           .

      ** add other procedures here
       END PROGRAM Evenements.
