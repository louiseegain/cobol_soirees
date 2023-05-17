      ******************************************************************
      * Author: Louise EGAIN - Mathias LORET - Thomas MERLET - Camille LEAU
      * Date : Mai 2023
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
           select futilisateur assign TO "utilisateur.dat"
           organization indexed
           access mode is dynamic
           record KEY IS futil_login
           ALTERNATE RECORD KEY IS futil_mail
           ALTERNATE RECORD KEY IS futil_tel
           ALTERNATE RECORD KEY IS futil_formation WITH DUPLICATES
           ALTERNATE RECORD KEY IS futil_nom WITH DUPLICATES
           file status is cr_futil.


           select fevenement assign TO "evenement.dat"
           organization indexed
           access mode is dynamic
           record KEY IS fevent_nom
           ALTERNATE RECORD KEY IS fevent_type WITH DUPLICATES
           ALTERNATE RECORD KEY IS fevent_dateMois WITH DUPLICATES
           ALTERNATE RECORD KEY IS fevent_loginOrga WITH DUPLICATES
           file status is cr_fevent.


           SELECT fparticipant ASSIGN TO "participant.dat"
           ORGANIZATION INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY fpart_cle
           ALTERNATE RECORD KEY IS fpart_login with DUPLICATES
           ALTERNATE RECORD KEY IS fpart_nomEvent WITH DUPLICATES
           FILE STATUS IS cr_fpart.

           SELECT fhistorique ASSIGN TO "historique.dat"
           ORGANIZATION INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY fhisto_nom
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
           02 futil_type PIC 9(1).
           02 futil_formation PIC A(20).
           02 futil_naissanceJour PIC 9(2).
           02 futil_naissanceMois PIC 9(2).
           02 futil_naissanceAnnee PIC 9(2).

       FD fevenement.
       01 tamp_fevent.
           02 fevent_nom PIC X(50).
           02 fevent_type PIC A(20).
           02 fevent_dateJour PIC 9(2).
           02 fevent_dateMois PIC 9(2).
           02 fevent_dateAnnee PIC 9(2).
           02 fevent_loginOrga PIC X(30).
           02 fevent_description PIC X(50).
           02 fevent_adresse PIC X(100).
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
           02 fhisto_dateJour PIC 9(2).
           02 fhisto_dateMois PIC 9(2).
           02 fhisto_dateAnnee PIC 9(2).
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
       77 choix PIC 9(1).
       77 nom PIC A(30).
       77 vreTOur PIC 9(1).
       77 estValideEvenementResultat PIC 9(1).
       77 estValideEvenementResultatHisto PIC 9(1).
       77 loginSaved PIC X(30).
       77 adresseEvent PIC X(30).
       77 descriptionEvent PIC X(50).
       77 loginOrga PIC X(30).
       77 dateEvent PIC X(10).
       77 typeEvent PIC A(20).
       77 nomEvent PIC A(30).
       77 etatEvent PIC A(8).
       77 seuilEvent PIC 9(3).
       77 heureEvent PIC X(5).
       77 Fin PIC 9(1).
       01 WS-CURRENT-DATE-DATA.
          05  WS-CURRENT-DATE.
              10  WS-CURRENT-YEAR         PIC 9(2).
              10  WS-CURRENT-MONTH        PIC 9(2).
              10  WS-CURRENT-DAY          PIC 9(2).
          05  WS-CURRENT-TIME.
              10  WS-CURRENT-HOURS        PIC 9(2).
              10  WS-CURRENT-MINUTE       PIC 9(2).
              10  WS-CURRENT-SECOND       PIC 9(2).
              10  WS-CURRENT-MILLISECONDS PIC 9(2).
       77 cpt PIC 9(3).
       77 login PIC X(30).
       77 fermeAppli PIC 9(1).
       77 verif_login pic X(30).
       77 chaine PIC X(30).
       77 lettre PIC A(1).
       77 I PIC 9(2).
       77 verif_arobase PIC 9(1).
       77 verif_mail_ok PIC 9(1).
       77 verif_tel_ok PIC 9(1).
       77 verif_login_ok PIC 9(1).
       77 verif PIC 9(1).
       77 termine PIC 9(1).
       77 mdp PIC X(30).
       77 nomSaved PIC A(30).
       77 prenom PIC A(30).
       77 fdf PIC 9(1).
       77 suppression_ok PIC 9(1).
       77 inscription PIC 9(1).
       77 erreurProfil PIC 9(1).
       77 retour PIC 9(1).
       77 verif_event PIC 9(1).
       77 fin_boucle PIC 9(1).
       77 fin_boucle2 PIC 9(1).
       77 choixProfil PIC 9(1).
       77 choixEvent PIC 9(1).
       77 choixUtil PIC 9(1).
       77 choixStat PIC 9(1).
       77 reponse PIC 9(1).
       77 dateJour PIC 9(2).
       77 dateMois PIC 9(2).
       77 dateAnnee PIC 9(2).
       77 valideInscription PIC 9(1).
       77 choixGestionDemande PIC 9(1).
       77 autoSupprEvent PIC 9(1).
       77 nbEvents PIC 9(3).
       77 nbEventArchivables PIC 9(3).
       77 nbEventArchives PIC 9(3).
       77 nbUtils PIC 9(4).
       77 dateComparee PIC 9(1).
       77 choixModifEvent PIC 9(1).
       77 nbParticipants PIC 9(3).
       77 estValideHeure PIC 9(1).
       77 typeStat PIC A(20).
       77 formaStat PIC A(20).
       77 moisStat PIC 9(2).
       77 nbPartStat PIC 9(3).
       77 longHeure PIC 9(1).
       77 erreurCompte PIC 9(1).
       77 verif_mdp_ok PIC 9(1).
       77 annee PIC 9(4).
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.

      *-----------------------------------------------------------------
      *                  CREATION DES FICHIERS
      *-----------------------------------------------------------------
           OPEN I-O futilisateur
           IF cr_futil =35 THEN
               OPEN OUTPUT futilisateur
           END-IF
           CLOSE futilisateur
           DISPLAY "futil : ",cr_futil

           OPEN I-O fevenement
           IF cr_fevent =35 THEN
               OPEN OUTPUT fevenement
           END-IF
           CLOSE fevenement
           DISPLAY "fevent : ",cr_fevent

           OPEN I-O fparticipant
           IF cr_fpart =35 THEN
               OPEN OUTPUT fparticipant
           END-IF
           CLOSE fparticipant
           DISPLAY "fpart : ", cr_fpart

           OPEN I-O fhistorique
           IF cr_fhisto =35 THEN
               OPEN OUTPUT fhistorique
           END-IF
           CLOSE fhistorique.
           DISPLAY "fhisto : ",cr_fhisto
      *-----------------------------------------------------------------
      *                  CREATION JDD
      *-----------------------------------------------------------------
      * Utilisateur 1 : administrateur
           MOVE "mloret" TO futil_login
           MOVE "mathias" TO futil_mdp
           MOVE "LORET" TO futil_nom
           MOVE "Mathias" TO futil_prenom
           MOVE "mathias.loret@gmail.com" TO futil_mail
           MOVE "0635451225" TO futil_tel
           MOVE 1 TO futil_type
           MOVE "MIAGE" TO futil_formation
           MOVE 14 TO futil_naissanceJour
           MOVE 06 TO futil_naissanceMois
           MOVE 02 TO futil_naissanceAnnee

           OPEN I-O futilisateur
           WRITE tamp_futi
           END-WRITE
           IF cr_futil = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion reussie"
               DISPLAY "U1 : ",cr_futil
           END-IF
           CLOSE futilisateur

      * Utilisateur 2 : administrateur
            MOVE "legain" TO futil_login
            MOVE "louise" TO futil_mdp
            MOVE "EGAIN" TO futil_nom
            MOVE "Louise" TO futil_prenom
            MOVE "louise.egain@gmail.com" TO futil_mail
            MOVE "0770029252" TO futil_tel
            MOVE 1 TO futil_type
            MOVE "IFSI" TO futil_formation
            MOVE 26 TO futil_naissanceJour
            MOVE 09 TO futil_naissanceMois
            MOVE 01 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 35
                DISPLAY "Echec d'insertion"
            ELSE
                DISPLAY "Insertion reussie"
                DISPLAY "U2 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 3 : administrateur
            MOVE "tmerlet" TO futil_login
            MOVE "thomas" TO futil_mdp
            MOVE "MERLET" TO futil_nom
            MOVE "Thomas" TO futil_prenom
            MOVE "thomas.merlet@gmail.com" TO futil_mail
            MOVE "0789654111" TO futil_tel
            MOVE 1 TO futil_type
            MOVE "Commerce" TO futil_formation
            MOVE 25 TO futil_naissanceJour
            MOVE 12 TO futil_naissanceMois
            MOVE 99 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 35
                DISPLAY "Echec d'insertion"
            ELSE
                DISPLAY "Insertion reussie"
                DISPLAY "U3 : ",cr_futil
            END-IF
            CLOSE futilisateur


      * Utilisateur 4 : administrateur
            MOVE "cleau" TO futil_login
            MOVE "camille" TO futil_mdp
            MOVE "LEAU" TO futil_nom
            MOVE "Camille" TO futil_prenom
            MOVE "camille.leau@gmail.com" TO futil_mail
            MOVE "0632154569" TO futil_tel
            MOVE 1 TO futil_type
            MOVE "art" TO futil_formation
            MOVE 02 TO futil_naissanceJour
            MOVE 10 TO futil_naissanceMois
            MOVE 00 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 35
                DISPLAY "Echec d'insertion"
            ELSE
                DISPLAY "Insertion reussie"
                DISPLAY "U4 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 5 : membre
            MOVE "sledourner" TO futil_login
            MOVE "swann" TO futil_mdp
            MOVE "LE DOURNER" TO futil_nom
            MOVE "Swann" TO futil_prenom
            MOVE "swann.ledourner@gmail.com" TO futil_mail
            MOVE "0745197635" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "MIAGE" TO futil_formation
            MOVE 21 TO futil_naissanceJour
            MOVE 12 TO futil_naissanceMois
            MOVE 96 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 35
                DISPLAY "Echec d'insertion"
            ELSE
                DISPLAY "Insertion reussie"
                DISPLAY "U5 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 6 : membre
            MOVE "kcosquer" TO futil_login
            MOVE "kevin" TO futil_mdp
            MOVE "COSQUER" TO futil_nom
            MOVE "Kevin" TO futil_prenom
            MOVE "kevin.cosquer@gmail.com" TO futil_mail
            MOVE "0645879311" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "IFSI" TO futil_formation
            MOVE 23 TO futil_naissanceJour
            MOVE 04 TO futil_naissanceMois
            MOVE 01 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 35
                DISPLAY "Echec d'insertion"
            ELSE
                DISPLAY "Insertion reussie"
                DISPLAY "U6 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 7 : membre
            MOVE "gkoc" TO futil_login
            MOVE "gamze" TO futil_mdp
            MOVE "KOC" TO futil_nom
            MOVE "Gamze" TO futil_prenom
            MOVE "gamze.koc@gmail.com" TO futil_mail
            MOVE "0785460116" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "Commerce" TO futil_formation
            MOVE 28 TO futil_naissanceJour
            MOVE 03 TO futil_naissanceMois
            MOVE 02 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 35
                DISPLAY "Echec d'insertion"
            ELSE
                DISPLAY "Insertion reussie"
                DISPLAY "U7 : ",cr_futil
            END-IF
            CLOSE futilisateur


      * Utilisateur 8 : membre
            MOVE "tleberre" TO futil_login
            MOVE "thibault" TO futil_mdp
            MOVE "LE BERRE" TO futil_nom
            MOVE "Thibault" TO futil_prenom
            MOVE "thibault.leberre@gmail.com" TO futil_mail
            MOVE "0725242923" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "art" TO futil_formation
            MOVE 08 TO futil_naissanceJour
            MOVE 08 TO futil_naissanceMois
            MOVE 01 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 35
                DISPLAY "Echec d'insertion"
            ELSE
                DISPLAY "Insertion reussie"
                DISPLAY "U8 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Evenement 1
           MOVE "Gala 2023" TO fevent_nom
           MOVE "Soiree" TO fevent_type
           MOVE 21 TO fevent_dateJour
           MOVE 06 TO fevent_dateMois
           MOVE 23 TO fevent_dateAnnee
           MOVE "mloret" TO fevent_loginOrga
           MOVE "Soiree de fin d'annee" TO fevent_description
           MOVE "2 rue de la liberte, 35000 Rennes" TO fevent_adresse
           MOVE 150 TO fevent_seuil
           MOVE "20h00" TO fevent_heure

           OPEN I-O fevenement
              WRITE tamp_fevent
              END-WRITE
              IF cr_fevent = 35
                  DISPLAY "Echec d'insertion"
              ELSE
                  DISPLAY "Insertion reussie"
                  DISPLAY "E1 : ",cr_fevent
              END-IF
              CLOSE fevenement

      * Evenement 2
           MOVE "Escape game de l'horreur" TO fevent_nom
           MOVE "Escape Game" TO fevent_type
           MOVE 15 TO fevent_dateJour
           MOVE 07 TO fevent_dateMois
           MOVE 23 TO fevent_dateAnnee
           MOVE "legain" TO fevent_loginOrga
           MOVE "Pour se terrifier en cette belle periode" TO fevent_des
      -    cription
           MOVE "Les loges, 44140 Montbert" TO fevent_adresse
           MOVE 6 TO fevent_seuil
           MOVE "21h00" TO fevent_heure

           OPEN I-O fevenement
              WRITE tamp_fevent
              END-WRITE
              IF cr_fevent = 35
                  DISPLAY "Echec d'insertion"
              ELSE
                  DISPLAY "Insertion reussie"
                  DISPLAY "E2 : ",cr_fevent
              END-IF
              CLOSE fevenement

      * Evenement 3
           MOVE "Afterwork" TO fevent_nom
           MOVE "Soiree" TO fevent_type
           MOVE 02 TO fevent_dateJour
           MOVE 06 TO fevent_dateMois
           MOVE 23 TO fevent_dateAnnee
           MOVE "tmerlet" TO fevent_loginOrga
           MOVE "Afterwork de fin d'annee" TO fevent_description
           MOVE "9 Rue Bon Secours, 44000 Nantes" TO fevent_adresse
           MOVE 150 TO fevent_seuil
           MOVE "18h15" TO fevent_heure

           OPEN I-O fevenement
              WRITE tamp_fevent
              END-WRITE
              IF cr_fevent = 35
                  DISPLAY "Echec d'insertion"
              ELSE
                  DISPLAY "Insertion reussie"
                  DISPLAY "E3 : ",cr_fevent
              END-IF
              CLOSE fevenement

      * Evenement 4
           MOVE "Karting exterieur" TO fevent_nom
           MOVE "Karting" TO fevent_type
           MOVE 21 TO fevent_dateJour
           MOVE 09 TO fevent_dateMois
           MOVE 23 TO fevent_dateAnnee
           MOVE "cleau" TO fevent_loginOrga
           MOVE "Karting de rentree" TO fevent_description
           MOVE "19 Rte des Naudieres, 44880 Sautron" TO fevent_adresse
           MOVE 150 TO fevent_seuil
           MOVE "20h00" TO fevent_heure

           OPEN I-O fevenement
              WRITE tamp_fevent
              END-WRITE
              IF cr_fevent = 35
                  DISPLAY "Echec d'insertion"
              ELSE
                  DISPLAY "Insertion reussie"
                  DISPLAY "E4 : ",cr_fevent
              END-IF
           CLOSE fevenement

      *-----------------------------------------------------------------
      *                  PROGRAMME PRINCIPAL
      *-----------------------------------------------------------------
      *     ACCEPT WS-CURRENT-DATE-DATA FROM DATE
      *     DISPLAY WS-CURRENT-DATE
           *> KIWIZ MP

           DISPLAY "-----------------------------------------------"
           DISPLAY "|         BIENVENUE SUR L'APPLICATION         |"
           DISPLAY "-----------------------------------------------"
           PERFORM accueil
           STOP RUN.
      *-----------------------------------------------------------------
      *                  FONCTIONS ET PROCEDURES
      *-----------------------------------------------------------------

           ACCEPT WS-CURRENT-DATE-DATA FROM DATE
           DISPLAY WS-CURRENT-DATE
           .

      *-----------------------------------------------------------------
      *          Procedure de connexion a l'application
      *-----------------------------------------------------------------

       accueil.
      *     MOVE 9 TO choix
           PERFORM WITH TEST AFTER UNTIL choix = 0
      *         DISPLAY "Annee : "WS-CURRENT-YEAR
      *         DISPLAY "Mois : "WS-CURRENT-MONTH
      *         DISPLAY "Jour : "WS-CURRENT-DAY
               DISPLAY "-----------------------------------------------"
               DISPLAY "|  1 - Me connecter a mon compte              |"
               DISPLAY "|  2 - Creer mon compte                       |"
               DISPLAY "|  0 - Quitter                                |"
               DISPLAY "-----------------------------------------------"
      *         DISPLAY "choix :" choix
               DISPLAY "Taper votre choix :"
               ACCEPT choix
      *         DISPLAY "choix :" choix

               EVALUATE choix
               WHEN 1
                   PERFORM connexion
                   IF erreurCompte = 0 THEN
      * KIWIZ ici bug potentiel si probleme connexion
                       PERFORM menuUtilisateur
                   END-IF
               WHEN 2
                   DISPLAY "-------------------------------------------"
                   DISPLAY "|         Creation d'un compte            |"
                   DISPLAY "-------------------------------------------"
                   PERFORM creation_compte
               WHEN 0
                   DISPLAY "Merci et a bientot"
               END-EVALUATE
           END-PERFORM
           .


      *-----------------------------------------------------------------
      *          Procedure permettant de creer son compte utilisateur
      *-----------------------------------------------------------------
       creation_compte.

      ** ouverture du fichier
           OPEN I-O futilisateur

      **enregistrement des informations dans le tampon
           DISPLAY "Entrer votre nom :"
           ACCEPT futil_nom
           DISPLAY "Entrer votre prenom :"
           ACCEPT futil_prenom

      **verification du format xxxxx@xxxx.fr ou xxxxx@xxxx.com
           PERFORM WITH TEST AFTER UNTIL verif_mail_ok EQUAL 0
               DISPLAY "Entrer votre adresse mail :"
               ACCEPT futil_mail
               PERFORM verif_mail
           END-PERFORM

      ** verification du format d'un nombre de 10 chiffres pour le tel
           PERFORM WITH TEST AFTER UNTIL verif_tel_ok EQUAL 1
               DISPLAY "Entrer votre numero de telephone :"
               ACCEPT futil_tel
               PERFORM verif_tel
           END-PERFORM

           DISPLAY "Entrer le nom de votre formation :"
           ACCEPT futil_formation
           DISPLAY "Entrer votre date de naissance :"
           PERFORM WITH TEST AFTER UNTIL futil_naissanceJour>0 AND
               futil_naissanceJour<=31
           DISPLAY "JOUR (entre 1 et 31): "
               ACCEPT futil_naissanceJour
           END-PERFORM

           PERFORM WITH TEST AFTER UNTIL futil_naissanceMois>0 AND
               futil_naissanceMois<=12
               DISPLAY "MOIS (entre 1 et 12): "
               ACCEPT futil_naissanceMois
           END-PERFORM

            PERFORM WITH TEST AFTER UNTIL
                futil_naissanceAnnee<=2023
                DISPLAY "ANNEE :"
               ACCEPT futil_naissanceAnnee
            END-PERFORM
           MOVE 0 TO futil_type

      **verification que le login n'existe pas deja
           MOVE 0 TO verif_login_ok
           PERFORM UNTIL verif_login_ok EQUAL 1
               DISPLAY "Entrer votre login :"
               ACCEPT futil_login
               IF futil_login NOT EQUAL SPACE THEN
                   READ futilisateur
                       INVALID KEY
                           MOVE 1 TO verif_login_ok
                       NOT INVALID KEY
                           DISPLAY "ce login existe deja!"
                   ENd-READ
               ELSE
                   DISPLAY "le login ne peut pas etre vide"
               END-IF
           END-PERFORM

           MOVE 0 TO verif_mdp_ok
           PERFORM UNTIL verif_mdp_ok EQUAL 1
               DISPLAY "Entrer votre mot de passe :"
               ACCEPT futil_mdp
               IF futil_mdp NOT EQUAL SPACE THEN
                   MOVE 1 TO verif_mdp_ok
               ELSE
                   DISPLAY "le mot de passe ne peut pas etre vide"
               END-IF
           END-PERFORM

      * on insere les informations dans le fichier
           WRITE tamp_futi
               INVALID KEY
                   DISPLAY "compte non cree : un probleme est survenu"
                   MOVE 1 to erreurCompte
               NOT INVALID KEY
                   DISPLAY "compte cree"
           END-WRITE.
               DISPLAY cr_futil
           CLOSE futilisateur.

      *-----------------------------------------------------------------
      *         Procedure permettant de verifier le format d'une
      *         adresse mail
      *-----------------------------------------------------------------
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


      ******************************************************************
      * Fonction annexe :
      *          Procedure permettant de verifier le numero de telephone
      *          saisi par l'utilisateur
      ******************************************************************
       verif_tel.
           MOVE 1 TO I
           MOVE 1 TO verif_tel_ok
      *     DISPLAY 'futil_tel test'
           PERFORM UNTIL futil_tel(I:1) EQUAL SPACE OR I EQUAL 11
      *             DISPLAY 'lettre =' futil_tel(I:1)
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
      *     DISPLAY "letttre suivante =" futil_tel(I:1)
           IF futil_tel(I:1) NOT EQUAL SPACE THEN
               MOVE 0 TO verif_tel_ok
           END-IF.



      *-----------------------------------------------------------------
      *          Procedure permettant de se connecter a son compte
      *-----------------------------------------------------------------
       connexion.
           MOVE SPACE TO futil_mdp
           MOVE SPACE TO futil_login
           MOVE 0 TO verif

           PERFORM UNTIL verif EQUAL 1
               DISPLAY "-------------------------------------------"
               DISPLAY "|        Connexion a votre compte         |"
               DISPLAY "-------------------------------------------"
               DISPLAY "Entrer votre login :"
               ACCEPT login
               MOVE login TO loginSaved
               DISPLAY "Entrer votre mot de passe :"
               ACCEPT mdp

      ** Lors de la connexion on vérifie que le login et le mot de passe
      ** ne soient pas vide
               IF mdp NOT EQUAL SPACE AND login NOT EQUAL SPACE THEN
                   MOVE login TO futil_login
                   MOVE mdp TO futil_mdp
                   OPEN INPUT futilisateur
                   READ futilisateur
                       INVALID KEY
                           DISPLAY "---------------------"
                           DISPLAY "| /!\ ATTENTION /!\ |"
                           DISPLAY "| Compte inexistant |"
                           DISPLAY "---------------------"
                           DISPLAY "Veuillez creer votre compte : "
                           PERFORM creation_compte
                       NOT INVALID KEY
                           IF futil_mdp EQUAL mdp THEN
                               MOVE 1 TO verif
                           ELSE
                               DISPLAY "Erreur de mot de passe"
                           END-IF
                   END-READ
                   CLOSE futilisateur

               ELSE
                   DISPLAY "Mot de passe et login ne peuvent etre vide"
               END-IF

           END-PERFORM

           .


      *-----------------------------------------------------------------
      *      Procedure gerant le menu d'un utilisateur pouvant devenir
      *      organisateur d'un ou plusieurs evenement s'il en cre
      *-----------------------------------------------------------------
           menuUtilisateur.
           MOVE 9 TO fermeAppli
      ** Affichage du menu principal de l'utilisateur
           PERFORM WITH TEST AFTER UNTIL fermeAppli =0
           DISPLAY"--------------------------------------------"
           DISPLAY"|             MENU PRINCIPAL                |"
           DISPLAY"--------------------------------------------"
               DISPLAY "Que souhaitez-vous faire ?"
               DISPLAY "1 - Verifier etat des inscriptions"
               DISPLAY "2 - Gerer votre profil"
               DISPLAY "3 - Rechercher un evenement ou un utilisateur"
               DISPLAY "4 - Gestion d'evenement (creation, modif, etc)"
               IF futil_type=1 THEN
                   DISPLAY "5 - Afficher les statistiques"
                   DISPLAY "6 - Modifier le type d'utilisateur"
                   DISPLAY "7 - Archiver evenement passe"
               END-IF
               DISPLAY "0 - Deconnexion"
               ACCEPT fermeAppli

      ** Evaluation du choix en fonction de sa reponse il va être amener
      ** sur un sous-menu
               EVALUATE fermeAppli
                   WHEN 1 PERFORM etatInscription
                   WHEN 2 PERFORM gererProfil
                   WHEN 3 PERFORM rechercherEvent
                   WHEN 4 PERFORM gestionEvenement
                   WHEN 5
                       IF futil_type=1
                       THEN PERFORM afficheStatistique
                       ELSE DISPLAY "Non autorise"
                           PERFORM menuUtilisateur
                       END-IF
                   WHEN 6
                      IF futil_type = 1
                           THEN PERFORM modifierProfilAdmin
                      ELSE DISPLAY "Non autorise"
                           PERFORM menuUtilisateur
                       END-IF
                   WHEN 7
                       IF futil_type = 1
                           THEN PERFORM archivageEvent
                       ELSE DISPLAY "Non autorise"
                           PERFORM menuUtilisateur
                      END-IF
               END-EVALUATE
           END-PERFORM
           .


      *-----------------------------------------------------------------
      *      Procedure gerant le menu de gestion de son profil
      *      Afficher / Modifier / Supprimer
      *-----------------------------------------------------------------v
           gererProfil.
           MOVE 9 TO choixProfil
           PERFORM WITH TEST AFTER UNTIL choixProfil =0
           DISPLAY"--------------------------------------------"
           DISPLAY"|           GESTION DU PROFIL              |"
           DISPLAY"--------------------------------------------"
               DISPLAY "Que souhaitez-vous faire dans votre"
               DISPLAY" profil ?"
               DISPLAY "1 - Modifier votre profil "
               DISPLAY "2 - Supprimer votre profil "
               DISPLAY "3 - Consulter votre profil"
               DISPLAY "0 - Revenir au menu precedent"
               DISPLAY "choix : "choixProfil
               ACCEPT choixProfil
               DISPLAY "choix : "choixProfil

           EVALUATE choixProfil
               WHEN 1 PERFORM modifierUtilisateur
               WHEN 2 PERFORM suppression_utilisateur
               WHEN 3 PERFORM consulterProfil
               WHEN 0 PERFORM menuUtilisateur
           END-EVALUATE
           END-PERFORM
           .


      *-----------------------------------------------------------------
      *      Procedure gerant le menu de recherche d'un evenement ou
      *      d'un utilisateur
      *-----------------------------------------------------------------
           rechercherEvent.
           PERFORM WITH TEST AFTER UNTIL choixEvent =0
           DISPLAY"--------------------------------------------"
           DISPLAY"| RECHERCHER UN EVENEMENT OU UN UTILISATEUR |"
           DISPLAY"--------------------------------------------"
               DISPLAY "Rechercher un evenement : "
               DISPLAY "1 - Par son nom"
               DISPLAY "2 - Par son organisateur"
               DISPLAY "Recherche un utilisateur :"
               DISPLAY "3 - Par son nom"
               DISPLAY "4 - Par son login"
               DISPLAY "5 - Afficher tous les organisateurs"
               DISPLAY "6 - Afficher tous les utilisateurs"
               DISPLAY "0 - Revenir au menu precedent"
               ACCEPT choixEvent

           EVALUATE choixEvent
              WHEN 1 PERFORM rechercherNom
      *        WHEN 2 PERFORM rechercherType
              WHEN 3 PERFORM rechercherUtilisateurNom
              WHEN 4 PERFORM rechercherUtilisateurLogin
              WHEN 5 PERFORM affichage_organisateur
              WHEN 6 PERFORM consulterUtilisateurs
              WHEN 0 PERFORM menuUtilisateur
           END-EVALUATE

           END-PERFORM
           .


      *-----------------------------------------------------------------
      *      Procedure gerant le menu des evenements
      *      creation / Modification / suppression / consultation
      *-----------------------------------------------------------------
           gestionEvenement.
           PERFORM WITH TEST AFTER UNTIL choix =0
           DISPLAY"--------------------------------------------"
           DISPLAY"|           GESTION D'EVENEMENT            |"
           DISPLAY"--------------------------------------------"
               DISPLAY "1 - Creer un evenement"
               DISPLAY "2 - Modifier un evenement"
               DISPLAY "3 - Supprimer un evenement"
               DISPLAY "4 - Afficher evenements"
               DISPLAY "0 - Revenir au menu precedent"
               ACCEPT choix

            EVALUATE choix
                WHEN 1 PERFORM creerEvent
                WHEN 2 PERFORM modifierEvent
                WHEN 3 PERFORM supprimerEvent
                WHEN 4 PERFORM afficheEvent
                WHEN 0 PERFORM menuUtilisateur
           END-EVALUATE

           END-PERFORM
           .


      *-----------------------------------------------------------------
      *      Procedure permettant d'afficher les statistiques de
      *      l'application
      *-----------------------------------------------------------------
           afficheStatistique.
           PERFORM WITH TEST AFTER UNTIL choixStat =0
           DISPLAY"--------------------------------------------"
           DISPLAY"|       AFFICHAGE DES STATISTIQUES         |"
           DISPLAY"--------------------------------------------"
               DISPLAY "1 - Afficher statistiques generales"
               DISPLAY "2 - Statistique selon formation et mois"
               DISPLAY "0 - Revenir au menu precedent"
               ACCEPT choixStat

               EVALUATE choixStat
               WHEN 1 PERFORM afficherStats
      *         WHEN 2 PERFORM statFormaMois
                WHEN 0 PERFORM menuUtilisateur
               END-EVALUATE
           END-PERFORM
           .

      *-----------------------------------------------------------------
      * Procedure permettant de modifier son profil utilisateur
      *-----------------------------------------------------------------
           modifierUtilisateur.
           OPEN I-O futilisateur
           MOVE 9 TO choixUtil
           PERFORM WITH TEST AFTER UNTIL choixUtil =0
           DISPLAY"--------------------------------------------"
           DISPLAY"|    MODIFIER MON PROFIL UTILISATEUR       |"
           DISPLAY"--------------------------------------------"
               DISPLAY "Que voulez-vous modifier ?"
               DISPLAY "1 - Nom "
               DISPLAY "2 - Prenom "
               DISPLAY "3 - Mail "
               DISPLAY "4 - Telephone "
               DISPLAY "5 - Formation"
               IF futil_type=1
                   DISPLAY "6 - Type d'utilisateur : "futil_type
               END-IF
               DISPLAY "0 - Revenir au menu precedent"
               DISPLAY "choix util "choixUtil
               ACCEPT choixUtil
               DISPLAY "choix util " choixUtil
               EVALUATE choixUtil
               WHEN 1
                   DISPLAY "Votre ancien nom : "futil_nom
                   DISPLAY "Entrer votre nouveau nom :"
                   ACCEPT futil_nom
                   PERFORM modifUtil
               WHEN 2
                   DISPLAY "Votre ancien prenom : "futil_prenom
                   DISPLAY "Entrer votre nouveau prenom :"
                   ACCEPT futil_prenom
                   PERFORM modifUtil
               WHEN 3
                   PERFORM WITH TEST AFTER UNTIL verif_mail_ok EQUAL 1
                       DISPLAY "Votre ancienne adresse mail: "futil_mail
                       DISPLAY "Entrer votre nouvelle adresse mail:"
                       ACCEPT futil_mail
                       PERFORM verif_mail
                       PERFORM modifUtil
                   END-PERFORM
               WHEN 4
                   PERFORM WITH TEST AFTER UNTIL verif_tel_ok EQUAL 1
                   DISPLAY "Votre ancien numero de telephone :"futil_tel
                       DISPLAY "Entrer votre numero de telephone:"
                       ACCEPT futil_tel
                       PERFORM verif_tel
                       PERFORM modifUtil
                   END-PERFORM
               WHEN 5
                   DISPLAY "Votre ancienne formation : "futil_formation
                   DISPLAY "Entrer votre nouvelle formation :"
                   ACCEPT futil_formation
                   PERFORM modifUtil
               WHEN 6
                   IF futil_type=1 THEN
                       DISPLAY "Veuillez entrer le nouveau type"
                       DISPLAY "d'utilisateur"
                       DISPLAY "1 = admin, 0 = membre simple"
                       ACCEPT futil_type
                       PERFORM modifUtil
                   END-IF
               WHEN 0 PERFORM gererProfil
               END-PERFORM
           CLOSE futilisateur
           .

      *-----------------------------------------------------------------
      *      Procedure permettant ï¿½ l'administrateur de rechercher un
      *      utilisateur membre afin de lui donner ou enlever les droits admin
      *-----------------------------------------------------------------
       modifierProfilAdmin.
           PERFORM consulterUtilisateurs
           DISPLAY "----------------------------"
           DISPLAY "|MODIFIER TYPE D'UTILISATEUR|"
           DISPLAY "----------------------------"
           DISPLAY "Quel utilisateur souhaitez-vous modifier ?"
           DISPLAY "Veuillez saisir son login"
           ACCEPT futil_login
           READ futilisateur
           INVALID KEY
               DISPLAY "Erreur dans la saisie du login"
               DISPLAY "Veuillez reessayer"
           NOT INVALID KEY
               DISPLAY "La seule modification possible sur le"
               DISPLAY "profil est le type d'utilisateur"
               DISPLAY "Etes-vous sur de vouloir le modifier ?"
               DISPLAY "1 = Oui, 0 = Non"
               ACCEPT reponse
               IF reponse = 1 THEN
                   DISPLAY "Veuillez saisir le nouveau type"
                   DISPLAY "d'utilisateur"
                   DISPLAY "1 = Admin, 0 = Membre"
                   ACCEPT futil_type
                   PERFORM modifUtil
               ELSE
                   PERFORM menuUtilisateur
               END-IF
           .
      ******************************************************************
      *    Fonction parallele :
      *    Fonction qui permet d'eviter du doublon de code sur la modification
      *    d'un element dans le fichier futilisateurs
      ******************************************************************
       modifUtil.
       REWRITE tamp_futi
               IF cr_futil = 00 THEN
                   DISPLAY "Modification reussie"
               ELSE
                   DISPLAY "Echec de modification"
                   DISPLAY "Reessayer ulterieurement"
               END-IF.
      *-----------------------------------------------------------------
      *      Procedure permettant de consulter son profil utilisateur
      *      Par mesure de securite le mot de passe n'est pas affiche
      *-----------------------------------------------------------------
           consulterProfil.
           DISPLAY"--------------------------------------------"
           DISPLAY"|   CONSULTER MON PROFIL UTILISATEUR       |"
           DISPLAY"--------------------------------------------"
           OPEN INPUT futilisateur
           MOVE loginSaved TO futil_login
           READ futilisateur
               INVALID KEY
                   DISPLAY "Compte inexistant"
               NOT INVALID KEY
                   DISPLAY "Nom :"
                   DISPLAY futil_nom
                   DISPLAY "Prenom :"
                   DISPLAY futil_prenom
                   DISPLAY "Mail : "
                   DISPLAY futil_mail
                   DISPLAY "Telephone : "
                   DISPLAY futil_tel
                   DISPLAY "Formation : "
                   DISPLAY futil_formation
                   DISPLAY "Login : "
                   DISPLAY futil_login
                   DISPLAY "Type d'utilisateur :"
                   IF futil_type=1
                       DISPLAY "Administrateur"
                   ELSE
                       DISPLAY "Membre"
                   END-IF
                   DISPLAY "Date de naissance : "
                   DISPLAY futil_naissanceJour"/"futil_naissanceMois"/"
      -              futil_naissanceAnnee
                   DISPLAY "---------"
                   DISPLAY "Une erreur sur votre profil ?"
                   DISPLAY "Souhaitez-vous la modifier ?"
                   DISPLAY "1 = Oui, 0 = Non"
                   ACCEPT erreurProfil
                   IF erreurProfil = 1
                       PERFORM modifierUtilisateur
                   ELSE
                       PERFORM gererProfil
                   END-IF
           END-READ
           CLOSE futilisateur.


      *-----------------------------------------------------------------
      *      Procedure permettant d'afficher tous les utilisateurs
      *      presents dans l'application. Qu'ils soient membres ou admin
      *-----------------------------------------------------------------
           consulterUtilisateurs.
           DISPLAY"--------------------------------------------"
           DISPLAY"|   CONSULTER TOUS LES UTILISATEURS        |"
           DISPLAY"--------------------------------------------"
           OPEN INPUT futilisateur
           MOVE 0 TO Fin
           PERFORM WITH TEST AFTER UNTIL Fin = 1
               READ futilisateur
                   AT END
                       MOVE 1 TO Fin
                   NOT AT END
                       DISPLAY "Nom :" futil_nom
                       DISPLAY "Prenom :" futil_prenom
                       DISPLAY "Mail : " futil_mail
                       DISPLAY "Formation : "futil_formation
                       DISPLAY "Login : "futil_login
                       DISPLAY "Type d'utilisateur : "
                       IF futil_type=1
                           DISPLAY "Administrateur"
                       ELSE
                           DISPLAY "Membre"
                       END-IF
                       DISPLAY "---------"
               END-READ
           END-PERFORM
           CLOSE futilisateur.

      *-----------------------------------------------------------------
      *      Procedure permettant de supprimer les evenements passes
      *      et de les archiver dans le fichier fhistorique
      *-----------------------------------------------------------------
           supprimerEventPasse.
           DISPLAY"--------------------------------------------"
           DISPLAY"|       SUPPRIMER EVENEMENT PASSE          |"
           DISPLAY"--------------------------------------------"
           .


      *-----------------------------------------------------------------
      * Procedure permettant de rechercher un evenement en fonction de
      *                        son nom
      *----------------------------------------------------------------
           rechercherNom.
           PERFORM afficheEvent
           OPEN INPUT fevenement
               DISPLAY "Quel evenement voulez-vous rechercher ?"
               DISPLAY "(saisir le nom)"
               ACCEPT fevent_nom
               MOVE fevent_nom TO nomSaved
               READ fevenement
                   INVALID KEY
                       DISPLAY "Evenement non trouve"
                       DISPLAY "Veuillez reessayer votre recherche."
                   NOT INVALID KEY
                       DISPLAY "Voici les informations de l'evenement :"
                       DISPLAY "Nom : " fevent_nom
                       DISPLAY "Type : "fevent_type
                       DISPLAY "Date : "fevent_dateJour"/"
                       DISPLAY fevent_dateMois"/"fevent_dateAnnee
                       MOVE fevent_dateJour TO dateJour
                       MOVE fevent_dateMois TO dateMois
                       MOVE fevent_dateAnnee TO dateAnnee
                       DISPLAY "Heure de debut : " fevent_heure
                       DISPLAY "Description : " fevent_description
                       DISPLAY "Adresse : " fevent_adresse
                       DISPLAY "Seuil : "fevent_seuil
                       DISPLAY "Login organisateur : " fevent_loginOrga
                       DISPLAY "--------------------------------------"
                       DISPLAY"|            INSCRIPTION EVENT         |"
                       DISPLAY "--------------------------------------"
                       MOVE 0 TO inscription
                       DISPLAY "Souhaitez-vous vous inscrire ?"
                       DISPLAY "1 = Oui, 0 = Non"
                       ACCEPT inscription
                       IF inscription = 1 THEN
                          PERFORM inscriptionEvent
                       ELSE
                           DISPLAY "Merci pour votre consultation."
                           PERFORM rechercherEvent
                       END-IF
               END-READ
           CLOSE fevenement.



      *-----------------------------------------------------------------
      *          Procedure permettant de creer un evenement
      *-----------------------------------------------------------------

      ******************************************************************
      *    Fonction parallele :
      *    Fonction qui verifie que le nom de l'evenement n'est pas deja
      *    present dans fevenement
      ******************************************************************
           existeEvent.
           OPEN INPUT fevenement
           READ fevenement
           INVALID KEY
               MOVE 0 TO estValideEvenementResultat
           NOT INVALID KEY
               IF nomEvent EQUALS fevent_nom THEN
                   MOVE 1 TO estValideEvenementResultat
                   DISPLAY fevent_nom
               END-IF
           END-READ

      *     IF cr_fevent = 00
      *     THEN DISPLAY "Evenement trouve"
      *     ELSE DISPLAY "Evenement non trouve"
      *     END-IF
      *     CLOSE fevenement
           .

      ******************************************************************
      *    Fonction parallele :
      *    Fonction qui verifie que le nom de l'evenement n'est pas deja
      *    present dans fhistorique
      ******************************************************************
           existeEventHisto.
           OPEN INPUT fhistorique
           READ fhistorique
           INVALID KEY
               MOVE 0 TO estValideEvenementResultatHisto
           NOT INVALID KEY
               IF nomEvent EQUALS fhisto_nom THEN
                   MOVE 1 TO estValideEvenementResultatHisto
                   DISPLAY fevent_nom
               END-IF
           END-READ

      *     IF cr_fevent = 00
      *     THEN DISPLAY "Evenement trouve"
      *     ELSE DISPLAY "Evenement non trouve"
      *     END-IF
      *     CLOSE fevenement
           .

      *----------------------------------------------------------------
      *    Fonction qui permet de creer un evenement
      *----------------------------------------------------------------
           creerEvent.
           DISPLAY "-------------------------------------------"
           DISPLAY "|          CREATION EVENEMENT             |"
           DISPLAY "-------------------------------------------"
           PERFORM WITH TEST AFTER UNTIL estValideEvenementResultat = 0
               DISPLAY "Saisir le nom de l'evenement"
               DISPLAY"(maximum 30 caracteres)"
               ACCEPT nomEvent
               PERFORM existeEvent
           END-PERFORM
           DISPLAY "Saisir le type d'evenement"
           ACCEPT typeEvent
           DISPLAY "Saisir la date de l'evenement"
           DISPLAY "JOUR : "
           PERFORM WITH TEST AFTER UNTIL
               fevent_dateJour>0 AND fevent_dateJour<=31
               ACCEPT fevent_dateJour
           END-PERFORM
           DISPLAY "MOIS : "
           PERFORM WITH TEST AFTER UNTIL
               fevent_dateMois>0 AND fevent_dateMois<=12
               ACCEPT fevent_dateMois
           END-PERFORM
           DISPLAY "ANNEE : "
           PERFORM WITH TEST AFTER UNTIL
               fevent_dateAnnee>=WS-CURRENT-YEAR
               ACCEPT fevent_dateAnnee
           END-PERFORM
           DISPLAY "Veuillez decrire votre evenement"
           DISPLAY "Format : maximum 50 caracteres"
           ACCEPT descriptionEvent
           DISPLAY "Veuillez saisir l'adresse de l'evenement"
           ACCEPT adresseEvent
           PERFORM UNTIL seuilEvent > 0
               DISPLAY "Veuillez saisir le nombre maximal de personne"
               ACCEPT seuilEvent
           END-PERFORM
           DISPLAY "Veuillez saisir l'heure de debut de l'evenement"
           DISPLAY " Format : xxhxx, avec x un chiffre"
           ACCEPT heureEvent

           MOVE nomEvent TO fevent_nom
           MOVE typeEvent TO fevent_type
           MOVE loginSaved TO fevent_loginOrga
           MOVE descriptionEvent TO fevent_description
           MOVE adresseEvent TO fevent_adresse
           MOVE seuilEvent TO fevent_seuil
           MOVE heureEvent TO fevent_heure

           OPEN I-O fevenement
           WRITE tamp_fevent
           END-WRITE
           IF cr_fevent=00
               DISPLAY "Insertion reussie"
           ELSE
               DISPLAY "Echec d'insertion, veuillez reessayer"
               DISPLAY cr_fevent
           END-IF

           CLOSE fevenement
           .


      *-----------------------------------------------------------------
      *          Procedure permettant d'afficher les evenements
      *-----------------------------------------------------------------
           afficheEvent.
           DISPLAY"--------------------------------------------"
           DISPLAY"|          AFFICHAGE EVENEMENT             |"
           DISPLAY"--------------------------------------------"
           OPEN INPUT fevenement
               MOVE 0 TO Fin
      *         PERFORM WITH TEST AFTER UNTIL Fin = 1
               PERFORM UNTIL Fin = 1
                   READ fevenement NEXT
      *             MOVE 1 TO Fin
                   AT END
      *                 PERFORM gestionEvenement KIWIZ pas utile
                       MOVE 1 TO Fin
                   NOT AT END
                       IF fevent_dateJour>=WS-CURRENT-DAY
                           AND fevent_dateMois>= WS-CURRENT-MONTH
                           AND fevent_dateAnnee >= WS-CURRENT-YEAR
                               DISPLAY "Nom : "fevent_nom
                               DISPLAY "Type : "fevent_type
                               DISPLAY"----------"
                       END-IF
               END-READ
               END-PERFORM
               CLOSE fevenement.
           .

      *-----------------------------------------------------------------
      *          Procedure permettant de s'inscrire a un evenement
      *-----------------------------------------------------------------
        inscriptionEvent.
         OPEN I-O fparticipant
      * On verifie dans un premier temps qu'il n'est pas deja inscrit a un evenement ou fait une demande
           MOVE loginSaved TO fpart_login
           PERFORM WITH TEST AFTER UNTIL Fin = 1
           START fparticipant, KEY IS = fpart_login
               INVALID KEY
                   MOVE 1 TO Fin
               NOT INVALID KEY
                   READ fparticipant NEXT
                       AT END
                           MOVE 1 TO Fin
                       NOT AT END
                       IF fpart_etat ="attente"
                           OR fpart_etat = "acceptee" THEN
                               IF fevent_dateJour=dateJour
                                  AND fevent_dateMois = dateMois
                                 AND fevent_dateAnnee = dateAnnee THEN
                                 MOVE 0 TO valideInscription
                                ELSE
                                    MOVE 1 TO valideInscription
                                END-IF
                       ELSE
                           MOVE 1 TO valideInscription
                       END-IF
                   END-READ
           END-START
           END-PERFORM

           IF valideInscription = 1 THEN
      * On verifie dans un premier temps qu'il reste de la place dans l'evenment
           PERFORM compte_nb_part
           IF fevent_seuil - nbParticipants <= 0 THEN
               DISPLAY "Evenement complet"
           ELSE
      * S'il reste de la place on saisie les valeurs pour inscrire l'utilisateur ï¿½ l'ï¿½vï¿½nement
             MOVE "attente" TO fpart_etat
             MOVE loginSaved TO fpart_login
             MOVE nomSaved TO fpart_nomEvent
             WRITE tamp_fpart
             END-WRITE
             IF cr_fpart =00 THEN
                 DISPLAY "Demande d'inscription validee"
                 DISPLAY "L'organisateur va verifier votre demande"
             ELSE
                 DISPLAY "Echec de demande d'inscription"
                 DISPLAY "Veuillez reessayer ulterieurement"
             END-IF
          ELSE DISPLAY "Vous avez deja un evenement de prevu"
          END-IF
         CLOSE fparticipant.

      *-----------------------------------------------------------------
      *          Procedure permettant de verifier l'etat des inscriptions
      *-----------------------------------------------------------------
       etatInscription.
           DISPLAY "--------------------------------------------------"
           DISPLAY "|                ETAT DES INSCRIPTIONS           |"
           DISPLAY "--------------------------------------------------"
           OPEN INPUT fparticipant
           MOVE loginSaved TO fpart_login
           START fparticipant, KEY IS = fpart_login
           INVALID KEY
               DISPLAY "Aucune inscription a un evenement"
           NOT INVALID KEY
               PERFORM WITH TEST AFTER UNTIL Fin = 1
                   READ fparticipant NEXT
                   AT END
                       MOVE 1 To Fin
                   NOT AT END
                       IF fpart_etat = "attente" THEN
                           DISPLAY "----------------------------------"
                           DISPLAY "|      EVENEMENTS EN ATTENTE      |"
                           DISPLAY "----------------------------------"
                           DISPLAY "Nom evenement : "fpart_nomEvent
                           DISPLAY "-------"
                       ELSE IF fpart_etat = "acceptee" THEN
                           DISPLAY "----------------------------------"
                           DISPLAY "|      EVENEMENTS ACCEPTEES      |"
                           DISPLAY "----------------------------------"
                           DISPLAY "Nom evenement : "fpart_nomEvent
                           DISPLAY "-------"
                       ELSE IF fpart_etat = "refusee" THEN
                           DISPLAY "----------------------------------"
                           DISPLAY "|       EVENEMENTS REFUSEES      |"
                           DISPLAY "----------------------------------"
                           DISPLAY "Nom evenement : "fpart_nomEvent
                           DISPLAY "-------"
                       END-IF
                       END-IF
                       END-IF
                   END-READ
               END-PERFORM
           END-START
           CLOSE fparticipant.
      *-----------------------------------------------------------------
      *          Procedure permettant de rechercher un utilisateur par
      *          son nom
      *-----------------------------------------------------------------
           rechercherUtilisateurNom.
           DISPLAY "--------------------------------------------------"
           DISPLAY "|       RECHERCHER UTILISATEUR AVEC SON NOM      |"
           DISPLAY "--------------------------------------------------"
           DISPLAY "Veuillez saisir le nom de la personne souhaitee :"
           ACCEPT nom
           OPEN INPUT futilisateur
           MOVE nom TO futil_nom
           START futilisateur, KEY IS = futil_nom
              INVALID KEY
                   DISPLAY "Utilisateur inexistant"
              NOT INVALID KEY
                  PERFORM WITH TEST AFTER UNTIL Fin = 1
                      READ    futilisateur    NEXT
                      AT    END
                           MOVE 1 TO Fin
                      NOT AT END
                           DISPLAY "Nom trouve, a present"
                           DISPLAY "Veuillez saisir son prenom"
                           ACCEPT prenom
                           IF prenom = futil_prenom THEN
                               DISPLAY "------------------------"
                               DISPLAY "|INFORMATION UTILISATEUR|"
                               DISPLAY "------------------------"
                               DISPLAY "Nom : "
                               DISPLAY futil_nom
                               DISPLAY "Prenom : "
                               DISPLAY futil_prenom
                               DISPLAY "Mail : "
                               DISPLAY futil_mail
                               DISPLAY "Telephone : "
                               DISPLAY futil_tel
                               DISPLAY "Login : "
                               DISPLAY futil_login
                               DISPLAY "Type d'utilisateur : "
                               IF futil_type = 1 THEN
                                   DISPLAY "Administrateur"
                               ELSE
                                   DISPLAY "Membre"
                               END-IF
                           END-IF
                      END-READ
                  END-PERFORM
           END-START
           CLOSE futilisateur
           .

      *-----------------------------------------------------------------
      *          Procedure permettant de rechercher un utilisateur par
      *          son login
      *-----------------------------------------------------------------
           rechercherUtilisateurLogin.
           DISPLAY "--------------------------------------------------"
           DISPLAY "|       RECHERCHER UTILISATEUR AVEC SON LOGIN    |"
           DISPLAY "--------------------------------------------------"
           DISPLAY "Veuillez saisir le login de la personne souhaitee :"
           ACCEPT login
           OPEN INPUT futilisateur
           READ futilisateur
               INVALID KEY
                   DISPLAY "Utilisateur non trouve"
               NOT INVALID KEY
                   IF login = futil_login THEN
                       DISPLAY "------------------------"
                       DISPLAY "|INFORMATION UTILISATEUR|"
                       DISPLAY "------------------------"
                       DISPLAY "Nom : "
                       DISPLAY futil_nom
                       DISPLAY "Prenom :"
                       DISPLAY futil_prenom
                       DISPLAY "Mail : "
                       DISPLAY futil_mail
                       DISPLAY "Telephone : "
                       DISPLAY futil_tel
                       DISPLAY "Login : "
                       DISPLAY futil_login
                       DISPLAY "Type utilisateur : "
                       IF futil_type = 1 THEN
                            DISPLAY "Administrateur"
                       ELSE
                           DISPLAY "Membre"
                       END-IF
                   END-IF
           CLOSE futilisateur
           .

      *-----------------------------------------------------------------
      *          Procedure permettant de supprimer un utilisateur
      *-----------------------------------------------------------------
       suppression_utilisateur.
      **pour cette fonction nous avons besoin de verifier que
      ** l'utilisateur n'est inscrit a aucun evenement et n'en organise
      ** aucun.
      ** le login de l'utilisateur a supprimer doit etre dans futil_login

           DISPLAY "--------------------------------------------------"
           DISPLAY "|        SUPPRIMER MON COMPTE UTILISATEUR        |"
           DISPLAY "--------------------------------------------------"
           OPEN INPUT fevenement
           MOVE 1 TO fdf
           MOVE 0 TO suppression_ok

      ** on verifie s'il organise un evenement
           MOVE futil_login TO fevent_loginOrga
           START fevenement, KEY IS = fevent_loginOrga
               NOT INVALID KEY
                   MOVE 1 TO suppression_ok
                   DISPLAY "Impossible de supprimer cet utilisateur"
                   DISPLAY "Il organise un evenement"
           END-START
           CLOSE fevenement

      ** s'il n'organise aucun evenement, alors on va verifier qu'il
      ** n'est pas inscrit a un evenement
           IF suppression_ok IS equal 0 THEN
               MOVE futil_login TO fpart_login
               OPEN INPUT fparticipant
               START fparticipant , KEY IS = fpart_login
                  NOT INVALID KEY
                   MOVE 1 TO suppression_ok
                   DISPLAY "Impossible de supprimer cet utilisateur"
                   DISPLAY "Il est inscrit a un evenement"
               END-START
               CLOSE fparticipant
           END-IF

           IF suppression_ok IS equal 0 THEN
              OPEN I-O futilisateur
      ** faire la suppression de l'utilisateur
      ** et revenir ï¿½ la page de connexion
               READ futilisateur
               INVALID key
                   DISPLAY "Impossible de supprime ce compte"
               NOT INVALID KEY
                   DELETE futilisateur RECORD
                   DISPLAY "-------------------------"
                   DISPLAY "|  Utilisateur supprime  |"
                   DISPLAY "-------------------------"
                   DISPLAY "-------------------------------------------"
                   DISPLAY "|         VOUS AVEZ ETE DECONNECTE        |"
                   DISPLAY "-------------------------------------------"
                   PERFORM accueil
               END-READ
              CLOSE futilisateur
           END-IF.

      *-----------------------------------------------------------------
      *          Procedure permettant d'afficher tous les organisateurs
      *           d'evenement
      *-----------------------------------------------------------------
       affichage_organisateur.

           DISPLAY "--------------------------------------------------"
           DISPLAY "|           AFFICHER LES ORGANISATEURS           |"
           DISPLAY "--------------------------------------------------"
               OPEN INPUT futilisateur
               OPEN INPUT fevenement

               MOVE 1 TO fdf

               PERFORM WITH TEST AFTER UNTIL fdf=0
                   READ futilisateur
                   AT END MOVE 0 TO fdf
                   not AT END
                       MOVE futil_login TO fevent_loginOrga
                       START fevenement , KEY IS = fevent_loginOrga
                           NOT INVALID KEY
                               DISPLAY futil_login
                       END-START
                   END-READ
               END-PERFORM

               CLOSE futilisateur
               CLOSE fevenement.

      *-----------------------------------------------------------------
      *          Procedure permettant d'accepter ou refuser des demandes
      *          de participation a un evenement
      *-----------------------------------------------------------------
       gestion_demandes.
           DISPLAY "--------------------------------------------------"
           DISPLAY "|                GESTION DEMANDES                |"
           DISPLAY "--------------------------------------------------"

           PERFORM afficheEvent
           MOVE 0 TO verif_event
           PERFORM UNTIL verif_event = 1 OR retour = 1
               PERFORM verif_permission
           END-PERFORM
           IF retour = 1 THEN
               PERFORM menuUtilisateur
           ELSE
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
                           DISPLAY "Cette demande n'est"
                           DISPLAY "pas en attente !"
                       ELSE
                           DISPLAY "Que souhaitez vous faire ?"
                           DISPLAY "0 - refuser la demande"
                           DISPLAY "1 - accepter la demande"
                           ACCEPT choixGestionDemande
                               IF choixGestionDemande = 0 THEN
                               MOVE "refusee" TO fpart_etat
                           ELSE
                               MOVE "acceptee" TO fpart_etat
                           END-IF
                           REWRITE tamp_fpart
                           DISPLAY "Demande traitee"
                       END-IF
               END-READ
               DISPLAY "Quitter ? (0 pour non, 1 pour oui)"
               ACCEPT fin_boucle
               CLOSE fparticipant
           END-PERFORM
           END-IF.

      *-----------------------------------------------------------------
      *          Procedure permettant de supprimer des evenements
      *          automatiquement ou non en fonction de autoSupprEvent
      *-----------------------------------------------------------------
       supprimerEvent.
           MOVE 0 TO verif_event
           MOVE 0 TO retour
           IF autoSupprEvent <> 1 THEN
               DISPLAY"--------------------------------------------"
               DISPLAY"|        SUPPRESSION EVENEMENT             |"
               DISPLAY"--------------------------------------------"
               PERFORM afficheEvent
      * Selection de l'evenement a supprimer
           PERFORM WITH TEST AFTER UNTIL verif_event = 1 OR retour = 1
                PERFORM verif_permission
           END-PERFORM
           END-IF

           IF retour = 1 THEN
               DISPLAY "Retour au menu"
           ELSE
      *    l'evenement existe, on peut le supprimer mais on supprime ses
      *    participants avant :

               DISPLAY "Suppression des participations liees a"
      -     "l'evenement"
               OPEN I-O fparticipant
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
               OPEN I-O fevenement
                   READ fevenement
                       INVALID KEY
                           DISPLAY "erreur dans lecture fevenement"
                       NOT INVALID KEY
                          DELETE fevenement RECORD
                   END-READ
               CLOSE fevenement
               DISPLAY "Suppression effectuee"
               IF autoSupprEvent <> 1 THEN
                   PERFORM afficheEvent
               END-IF
           END-IF
           .
      *-----------------------------------------------------------------
      *          Affiche tous les evenements d'un organisateur
      *-----------------------------------------------------------------
       afficher_events_organisateur.
           DISPLAY"--------------------------------------------"
           DISPLAY"|      AFFICHAGE DE VOS EVENEMENTS          |"
           DISPLAY"--------------------------------------------"

           OPEN INPUT fevenement
           MOVE 0 TO fin_boucle
           MOVE loginSaved TO fevent_loginOrga
           START fevenement, KEY IS = fevent_loginOrga
           INVALID KEY
               DISPLAY "Erreur lecture fevenement"
           NOT INVALID KEY
               PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
                   READ fevenement NEXT
                   AT END
                       MOVE 1 TO fin_boucle
                   NOT AT END
                       DISPLAY "---------------------------------------"
                       DISPLAY "Nom : ",fevent_nom
                       DISPLAY "Places : ", fevent_seuil
                       DISPLAY "Date : "fevent_dateJour"/"
      -                fevent_dateMois"/"fevent_dateAnnee
                       DISPLAY "Type : ", fevent_type
                       DISPLAY "Description : ", fevent_description
                       DISPLAY "Adresse : ", fevent_adresse
                       DISPLAY "---------------------------------------"
               END-PERFORM
           END-START
           CLOSE fevenement
           .

       afficherStats.
      * Affiche le nombre d'ï¿½vï¿½nements prï¿½sents sur toute la plateforme
           MOVE 0 TO nbEvents
           MOVE 0 TO nbEventArchivables
           MOVE 0 TO nbUtils
           MOVE 0 TO fin_boucle

           OPEN INPUT fevenement
           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               READ fevenement NEXT
                   AT END
                       MOVE 1 TO fin_boucle
                   NOT AT END
                       ADD 1 TO nbEvents
                       PERFORM comparer_date
                       IF dateComparee = 1 THEN
                           ADD 1 TO nbEventArchivables
                       END-IF
               END-READ
           END-PERFORM
           CLOSE fevenement
           MOVE 0 TO fin_boucle
           OPEN INPUT futilisateur
           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               READ futilisateur NEXT
                   AT END
                       MOVE 1 TO fin_boucle
                   NOT AT END
                       ADD 1 TO nbUtils
               END-READ
           END-PERFORM
           CLOSE futilisateur
           MOVE 1 TO nbEventArchives
           MOVE 0 TO fin_boucle
           OPEN INPUT fhistorique
           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               READ fhistorique NEXT
                   AT END
                       MOVE 1 TO fin_boucle
                   NOT AT END
                       ADD 1 TO nbEventArchives
               END-READ
           END-PERFORM
           DISPLAY"--------------------------------------------"
           DISPLAY"|          STATISTIQUES GLOBALES           |"
           DISPLAY"--------------------------------------------"
           DISPLAY "---------------------------------------------------"
           DISPLAY "Nombre d'evenements : ", nbEvents
           DISPLAY "Archivables : ", nbEventArchivables
           DISPLAY "Nombre d'utilisateurs : ", nbUtils
           DISPLAY "Nombre d'evenements archives : ", nbEventArchives.


      *-----------------------------------------------------------------
      *          Compare la date d'un evenement avec la date actuelle
      *           0 - meme date
      *           1 - evenement passe
      *           2 - evenement a venir
      *-----------------------------------------------------------------
       comparer_date.
           MOVE 0 TO dateComparee
           IF WS-CURRENT-YEAR > fevent_dateAnnee THEN
               MOVE 1 TO dateComparee
           ELSE
               IF WS-CURRENT-YEAR < fevent_dateAnnee THEN
                   MOVE 2 TO dateComparee
               ELSE
                   IF WS-CURRENT-MONTH > fevent_dateMois THEN
                       MOVE 1 TO dateComparee
                   ELSE
                       IF WS-CURRENT-MONTH < fevent_dateMois THEN
                           MOVE 2 TO dateComparee
                       ELSE
                           IF WS-CURRENT-DAY < fevent_dateJour THEN
                               MOVE 1 TO dateComparee
                           ELSE
                               IF WS-CURRENT-DAY > fevent_dateJour THEN
                                   MOVE 2 TO dateComparee
                               ELSE
                                   MOVE 0 TO dateComparee
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       modifierEvent.
      * Permet la modification d'un evenement
           PERFORM afficheEvent
           MOVE 0 TO fin_boucle
           MOVE 0 TO verif_event
           MOVE 0 TO retour

           PERFORM WITH TEST AFTER UNTIL verif_event = 1 OR retour = 1
               PERFORM verif_permission
           END-PERFORM

           OPEN I-O fevenement
           MOVE 9 TO choixModifEvent
           PERFORM WITH TEST AFTER UNTIL choixModifEvent =0
           DISPLAY"--------------------------------------------"
           DISPLAY"|          MODIFIER EVENEMENT              |"
           DISPLAY"--------------------------------------------"
           DISPLAY "Que voulez-vous modifier ?"
           DISPLAY "1 - Type"
           DISPLAY "2 - date "
           DISPLAY "3 - description "
           DISPLAY "4 - adresse "
           DISPLAY "5 - seuil"
           DISPLAY "6 - heure"
           DISPLAY "0 - Revenir au menu precedent"

           ACCEPT choixModifEvent
               EVALUATE choixModifEvent
               WHEN 1
                   DISPLAY "Ancien type : "fevent_type
                   DISPLAY "Entrez le nouveau type :"
                   ACCEPT fevent_type
                   REWRITE tamp_fevent
                   IF cr_futil = 00
                       THEN
                           DISPLAY "Modification reussie"
                       ELSE
                           DISPLAY "Modification en echec"
                           DISPLAY cr_fevent
                   END-IF
               WHEN 2
                   DISPLAY "Date precedente : "
                   DISPLAY fevent_dateJour"/"fevent_dateMois"/"
      -            fevent_dateAnnee
                   PERFORM WITH TEST AFTER UNTIL
           fevent_dateJour <= 1 AND fevent_dateJour <= 31
                   DISPLAY "Entrer le nouveau jour :"
                   ACCEPT fevent_dateJour
                   END-PERFORM
                   REWRITE tamp_fevent
                   IF cr_futil = 00
                       THEN
                           DISPLAY "Modification reussie"
                       ELSE
                           DISPLAY "Modification en echec"
                           DISPLAY cr_fevent
                   END-IF
               WHEN 3
                   DISPLAY "Ancienne description : "fevent_description
                   DISPLAY "Entrez la nouvelle description : "
                   ACCEPT fevent_description
                   REWRITE tamp_fevent
               IF cr_futil = 00 THEN
                   DISPLAY "Modification reussie"
               ELSE
                   DISPLAY "Modification en echec"
                   DISPLAY cr_fevent
               END-IF
               WHEN 4
               DISPLAY "Ancienne adresse :"fevent_adresse
               DISPLAY "Entrez la nouvelle adresse :"
               ACCEPT fevent_adresse
               REWRITE tamp_fevent
               IF cr_futil = 00
                   THEN
                       DISPLAY "Modification reussie"
                   ELSE
                       DISPLAY "Modification en echec"
                       DISPLAY cr_fevent
               END-IF
               WHEN 5
                   DISPLAY "Ancien seuil : "fevent_seuil
                   MOVE 0 TO fin_boucle
                   PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
                       DISPLAY "Entrez le nouveau seuil :"
                       ACCEPT fevent_seuil
                       PERFORM compte_nb_part
                       IF fevent_seuil <= 0 THEN
                           DISPLAY "Entrez une valeur valide !"
                       ELSE
                           IF fevent_seuil < nbParticipants THEN
                               DISPLAY "Le seuil est inferieur"
                               DISPLAY "au nombre de participants"
                               DISPLAY "deja inscrits"
                           ELSE
                               MOVE 1 TO fin_boucle
                           END-IF
                       END-IF
                   END-PERFORM
                   REWRITE tamp_fevent
                   IF cr_fevent = 00
                       THEN
                           DISPLAY "Modification reussie"
                       ELSE
                           DISPLAY "Modification en echec"
                           DISPLAY cr_fevent
                   END-IF
               WHEN 6
                   PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
                       DISPLAY "Entrez la nouvelle heure :"
                       DISPLAY "(format : xxHxx)"
                       ACCEPT fevent_heure
                   END-PERFORM
                   REWRITE tamp_fevent
                   IF cr_fevent = 00
                       THEN
                           DISPLAY "Modification reussie"
                       ELSE
                           DISPLAY "Modification en echec"
                           DISPLAY cr_fevent
                   END-IF
               WHEN 0 PERFORM gestionEvenement
               END-PERFORM
           CLOSE fevenement.

      *-----------------------------------------------------------------
      *          Procedure verifiant que l'utilisateur est
      *          Un administrateur ou l'organisateur de
      *                       L'evenement
      *-----------------------------------------------------------------
       verif_permission.
      * verifie que l'utilisateur a les permissions pour l'action
           MOVE 0 TO verif_event
           MOVE 0 TO retour

           PERFORM WITH TEST AFTER UNTIL verif_event = 1 OR retour = 1
               DISPLAY "Saisissez le nom de l'evenement a traiter : "
               ACCEPT fevent_nom
               OPEN INPUT fevenement
               READ fevenement
                   INVALID KEY
                       DISPLAY "invalide" fevent_nom
                       DISPLAY fevent_type
                       DISPLAY "Saisie invalide"
                   NOT INVALID KEY
                       DISPLAY fevent_nom
                       DISPLAY fevent_type
                       IF fevent_loginOrga = loginSaved THEN
                           MOVE 1 TO verif_event
                       ELSE
                           OPEN INPUT futilisateur
                           MOVE loginSaved TO futil_login
                           READ futilisateur
                               INVALID KEY
                                   DISPLAY "Erreur lecture futilisateur"
                               NOT INVALID KEY
      ** utilisateur admin ou organisateur
                                   IF futil_type = 1 THEN
                                       MOVE 1 TO verif_event
                                   END-IF
                           END-READ
                           CLOSE futilisateur
                       END-IF
               CLOSE fevenement
               IF verif_event = 0 THEN
                   DISPLAY "Voulez-vous revenir en arriere ?"
                   DISPLAY "0 - Non"
                   DISPLAY "1 - Oui"
                   ACCEPT retour
               END-IF
           END-PERFORM
           .

      *-----------------------------------------------------------------
      *          Procedure realisant l'archivage d'un evenement
      *-----------------------------------------------------------------
       archiver_event.
      * Realise l'archivage d'un evenement
           OPEN I-O fhistorique
               MOVE fevent_nom TO fhisto_nom
               MOVE fevent_type TO fhisto_type
               MOVE fevent_dateJour TO fhisto_dateJour
               MOVE fevent_dateMois TO fhisto_dateMois
               MOVE fevent_dateAnnee TO fhisto_dateAnnee
               MOVE fevent_loginOrga TO fhisto_loginOrga
               MOVE fevent_description TO fhisto_description
               MOVE fevent_adresse TO fhisto_adresse
               MOVE "termine" TO fhisto_etat
               PERFORM compte_nb_part
               MOVE nbParticipants TO fhisto_participants
      * Ecriture du nouvel element dans fhistorique :
           WRITE tamp_fevent
               INVALID KEY
                   DISPLAY "Erreur lecture fhistorique"
               NOT INVALID KEY
                   DISPLAY "Archivage reussi"
                   MOVE 1 TO autoSupprEvent
                   PERFORM supprimerEvent
                   MOVE 0 TO autoSupprEvent
           END-WRITE
           CLOSE fhistorique
           .

      *-----------------------------------------------------------------
      *          Procedure permettant a l'utilisateur d'archiver
      *          des evenements passes
      *-----------------------------------------------------------------
       archivageEvent.
           DISPLAY"--------------------------------------------"
           DISPLAY"|               ARCHIVAGE                   |"
           DISPLAY"--------------------------------------------"

           OPEN I-O fevenement
           MOVE 0 TO fin_boucle

           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               READ fevenement
                   AT END
                       MOVE 1 TO fin_boucle
                   NOT AT END
                       PERFORM comparer_date
                       IF dateComparee = 1 THEN
                           DISPLAY "-----------------------------------"
                           DISPLAY "Nom : "fevent_nom
                           DISPLAY "Date : "fevent_dateJour"/"
      -                     fevent_dateMois"/"fevent_dateAnnee
                           DISPLAY "-----------------------------------"
                       END-IF
               END-READ
           END-PERFORM

           MOVE 0 TO fin_boucle
           MOVE 0 TO retour
           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1 OR retour = 1
               DISPLAY "Saissisez le nom de l'event a archiver"
               ACCEPT fevent_nom
               READ fevenement
                   AT END
                       MOVE 1 TO fin_boucle
                   NOT AT END
                       PERFORM comparer_date
                       IF dateComparee = 1 THEN
                           PERFORM archiver_event
                       ELSE
                           DISPLAY "L'evenement n'est pas passe !"
                       END-IF
                       DISPLAY "Retourner au menu precedent ?"
                       DISPLAY "0 - Non 1 - Oui"
                       ACCEPT retour
               END-READ
           END-PERFORM
           IF retour = 1 THEN
               PERFORM menuUtilisateur
           END-IF.

           tout_archiver.
           OPEN INPUT fevenement
           MOVE 0 TO fin_boucle

           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               READ fevenement
               AT END
                   MOVE 1 TO fin_boucle
               NOT AT END
                   PERFORM archiver_event
               END-READ
           END-PERFORM
           CLOSE fevenement
           .

      ******************************************************************
      *          Procedure comptant les participants a un evenement
      *          fpart_nomEvent doit avoir sa valeur avant l'appel
      ******************************************************************
       compte_nb_part.
           MOVE 0 TO nbParticipants
           MOVE 0 TO fin_boucle
           OPEN INPUT fparticipant
           START fparticipant, KEY IS = fpart_nomEvent
               INVALID KEY
                   DISPLAY "Erreur comptage des participants"
               NOT INVALID KEY
                   PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
                       READ fparticipant NEXT
                       AT END
                           MOVE 1 TO fin_boucle
                       NOT AT END
                           IF fpart_etat = "acceptee" THEN
                               ADD 1 TO nbParticipants
                           END-IF
                        END-READ
                   END-PERFORM
                   CLOSE fparticipant
           END-START
           .

      ******************************************************************
      *          Procedure verifiant le format horaire
      *          La variable verifiee est heureEvent
      ******************************************************************
       *> verifHeure.
           *> MOVE 1 TO estValideHeure
           *> COMPUTE longHeure = FUNCTION LENGTH (heureEvent)
           *> IF longHeure <> 5 THEN
               *> MOVE 0 TO estValideHeure
           *> ELSE
               *> IF NOT NUMERIC(heureEvent(1:2)) THEN
                   *> MOVE 0 TO estValideHeure
               *> ELSE
                   *> IF heureEvent(3:1) <> 'h' THEN
                       *> MOVE 0 TO estValideHeure
                   *> ELSE
                       *> IF NOT NUMERIC(heureEvent(4:2))
                           *> MOVE 0 TO estValideHeure
                       *> END-IF
                   *> END-IF
               *> END-IF
           *> END-IF.

      *-----------------------------------------------------------------
      *          Procedure calculant le nombre de participations
      *           d'etudiants d'une formation F ï¿½  un evenement
      *                            de type T
      *-----------------------------------------------------------------
       statFormaMois.
           DISPLAY"--------------------------------------------"
           DISPLAY"|          STATISTIQUES SELON :            |"
           DISPLAY"|         LE MOIS, LA FORMATION            |"
           DISPLAY"|         ET LE TYPE D'EVENEMENT           |"
           DISPLAY"--------------------------------------------"

           DISPLAY"--------------------------------------------"
           DISPLAY"|        AFFICHAGE DES FORMATIONS          |"
           DISPLAY"--------------------------------------------"

           OPEN INPUT futilisateur
           MOVE 0 TO fin_boucle

           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               READ futilisateur
               AT END MOVE 1 TO fin_boucle
               NOT AT END
                   DISPLAY "---"
                   DISPLAY futil_formation
                   DISPLAY "---"
               END-READ
           END-PERFORM


           DISPLAY"--------------------------------------------"
           DISPLAY"|     AFFICHAGE DES TYPES D'EVENEMENTS     |"
           DISPLAY"--------------------------------------------"

           OPEN INPUT fevenement
           MOVE 0 TO fin_boucle

           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               READ fevenement
               AT END MOVE 1 TO fin_boucle
               NOT AT END
                   DISPLAY "---"
                   DISPLAY fevent_type
                   DISPLAY "---"
               END-READ
           END-PERFORM


           DISPLAY "Saisissez le type d'evenement :"
           ACCEPT typeStat

           DISPLAY "Saisissez la formation :"
           ACCEPT formaStat

           MOVE 0 TO fin_boucle
           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               DISPLAY "Saisissez le numero du mois :"
               ACCEPT moisStat
               IF moisStat > 0 AND moisStat < 13 THEN
                   MOVE 1 TO fin_boucle
           END-PERFORM

      * Debut du compte
           MOVE moisStat TO fevent_dateMois
           MOVE 0 TO fin_boucle
           START fevenement, KEY IS =fevent_dateMois
           INVALID KEY
               DISPLAY "Erreur dans fevenement"
           NOT INVALID KEY
               PERFORM WITH TEST AFTER UNTIL fin_boucle =1
                   READ fevenement NEXT
                   AT END MOVE 1 TO fin_boucle
                   NOT AT END
                       IF fevent_type = typeStat THEN
                           OPEN INPUT fparticipant
                           MOVE fevent_nom TO fpart_nomEvent
                           MOVE 0 TO fin_boucle2
                           START fparticipant, KEY IS =fpart_nomEvent
                           INVALID KEY
                               DISPLAY "Erreur fparticipant"
                           NOT INVALID KEY
                           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
                               READ fparticipant NEXT
                               AT END MOVE 1 TO fin_boucle2
                               NOT AT END
                                   MOVE fpart_login TO futil_login
                                   READ futilisateur
                                   INVALID KEY DISPLAY "futilisateur KO"
                                   NOT INVALID KEY
                                       ADD 1 TO nbPartStat
                                   END-READ
                               END-READ
                           END-PERFORM
                       END-IF
                   END-READ
               END-PERFORM
           END-START
           CLOSE fevenement
           CLOSE fparticipant
           CLOSE futilisateur
           DISPLAY "Nombre de participations repondant aux criteres :"
           DISPLAY nbPartStat
           .

      ** add other procedures here
       END PROGRAM Evenements.
