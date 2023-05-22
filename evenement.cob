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
           SELECT futilisateur ASSIGN TO "utilisateur.dat"
           ORGANIZATION INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS futil_login
           ALTERNATE RECORD KEY IS futil_mail
           ALTERNATE RECORD KEY IS futil_tel
           ALTERNATE RECORD KEY IS futil_formation WITH DUPLICATES
           ALTERNATE RECORD KEY IS futil_nom WITH DUPLICATES
           FILE STATUS IS cr_futil.


           SELECT fevenement ASSIGN TO "evenement.dat"
           ORGANIZATION INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS fevent_nom
           ALTERNATE RECORD KEY IS fevent_type WITH DUPLICATES
           ALTERNATE RECORD KEY IS fevent_dateMois WITH DUPLICATES
           ALTERNATE RECORD KEY IS fevent_loginOrga WITH DUPLICATES
           FILE STATUS IS cr_fevent.


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
           02 futil_nom PIC A(40).
           02 futil_prenom PIC A(40).
           02 futil_login PIC X(30).
           02 futil_mdp PIC X(40).
           02 futil_mail PIC X(40).
           02 futil_tel PIC X(10).
           02 futil_type PIC 9(1).
           02 futil_formation PIC A(40).
           02 futil_naissanceJour PIC 9(2).
           02 futil_naissanceMois PIC 9(2).
           02 futil_naissanceAnnee PIC 9(4).

       FD fevenement.
       01 tamp_fevent.
           02 fevent_nom PIC X(50).
           02 fevent_type PIC A(30).
           02 fevent_dateJour PIC 9(2).
           02 fevent_dateMois PIC 9(2).
           02 fevent_dateAnnee PIC 9(4).
           02 fevent_loginOrga PIC X(30).
           02 fevent_description PIC X(250).
           02 fevent_adresse PIC X(100).
           02 fevent_seuil PIC 9(3).
           02 fevent_heure PIC X(5).

       FD fparticipant.
       01 tamp_fpart.
           02 fpart_cle.
               03 fpart_login PIC X(30).
               03 fpart_nomEvent PIC A(50).
           02 fpart_etat PIC A(20).

       FD fhistorique.
       01 tamp_fhisto.
           02 fhisto_nom PIC A(30).
           02 fhisto_type PIC A(30).
           02 fhisto_dateJour PIC 9(2).
           02 fhisto_dateMois PIC 9(2).
           02 fhisto_dateAnnee PIC 9(4).
           02 fhisto_loginOrga PIC X(30).
           02 fhisto_description PIC X(250).
           02 fhisto_adresse PIC X(100).
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
       77 vretour PIC 9(1).
       77 estValideEvenementResultat PIC 9(1).
       77 estValideEvenementResultatHisto PIC 9(1).
       77 loginSaved PIC X(30).
       77 typeSaved PIC 9(1).
       77 adresseEvent PIC X(100).
       77 descriptionEvent PIC X(250).
       77 loginOrga PIC X(30).
       77 typeEvent PIC A(30).
       77 nomEvent PIC A(30).
       77 etatEvent PIC A(20).
       77 seuilEvent PIC 9(3).
       77 heureEvent PIC X(5).
       77 Fin PIC 9(1).
       01 WS-CURRENT-DATE-DATA.
          05  WS-CURRENT-DATE.
              10  WS-CURRENT-YEAR         PIC 9(4).
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
      * 77 lettre PIC A(1).
       77 I PIC 9(2).
       77 verif_arobase PIC 9(1).
       77 verif_mail_ok PIC 9(1).
       77 verif_tel_ok PIC 9(1).
       77 verif_login_ok PIC 9(1).
       77 verif PIC 9(1).
       77 termine PIC 9(1).
       77 mdp PIC X(40).
       77 nomSaved PIC A(40).
       77 prenom PIC A(40).
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
       77 dateAnnee PIC 9(4).
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
       77 verif_dot PIC 9(1).
       77 verif_domain PIC 9(1).
       77 valide_alpha PIC 9(1).
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
           MOVE "admin" TO futil_login
           MOVE "admin" TO futil_mdp
           MOVE "LORET" TO futil_nom
           MOVE "Mathias" TO futil_prenom
           MOVE "mathias.loret@gmail.com" TO futil_mail
           MOVE "0635451225" TO futil_tel
           MOVE 1 TO futil_type
           MOVE "MIAGE" TO futil_formation
           MOVE 14 TO futil_naissanceJour
           MOVE 06 TO futil_naissanceMois
           MOVE 2002 TO futil_naissanceAnnee

           OPEN I-O futilisateur
           WRITE tamp_futi
           END-WRITE
           IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U1 : ",cr_futil
            END-IF
           CLOSE futilisateur

      * Utilisateur 2 :
            MOVE "legain" TO futil_login
            MOVE "louise" TO futil_mdp
            MOVE "EGAIN" TO futil_nom
            MOVE "Louise" TO futil_prenom
            MOVE "louise.egain@gmail.com" TO futil_mail
            MOVE "0670029252" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "IFSI" TO futil_formation
            MOVE 26 TO futil_naissanceJour
            MOVE 09 TO futil_naissanceMois
            MOVE 2001 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U2 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 3 :
            MOVE "tmerlet" TO futil_login
            MOVE "tmerlet" TO futil_mdp
            MOVE "MERLET" TO futil_nom
            MOVE "Thomas" TO futil_prenom
            MOVE "thomas.merlet@gmail.com" TO futil_mail
            MOVE "0789654111" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "Commerce" TO futil_formation
            MOVE 25 TO futil_naissanceJour
            MOVE 12 TO futil_naissanceMois
            MOVE 1999 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U3 : ",cr_futil
            END-IF
            CLOSE futilisateur


      * Utilisateur 4 :
            MOVE "cleau" TO futil_login
            MOVE "camille" TO futil_mdp
            MOVE "LEAU" TO futil_nom
            MOVE "Camille" TO futil_prenom
            MOVE "camille.leau@gmail.com" TO futil_mail
            MOVE "0632154569" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "art" TO futil_formation
            MOVE 02 TO futil_naissanceJour
            MOVE 10 TO futil_naissanceMois
            MOVE 2000 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U4 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 5 :
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
            MOVE 1996 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U5 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 6 :
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
            MOVE 2001 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U6 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 7 :
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
            MOVE 2002 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U7 : ",cr_futil
            END-IF
            CLOSE futilisateur


      * Utilisateur 8 :
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
            MOVE 2001 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U8 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 9 :
            MOVE "jmeunier" TO futil_login
            MOVE "jeanne" TO futil_mdp
            MOVE "MEUNIER" TO futil_nom
            MOVE "Jeanne" TO futil_prenom
            MOVE "jeannemeunier@gmail.com" TO futil_mail
            MOVE "0732405887" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "IFSI" TO futil_formation
            MOVE 30 TO futil_naissanceJour
            MOVE 06 TO futil_naissanceMois
            MOVE 1998 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U9 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 10 :
            MOVE "mramond" TO futil_login
            MOVE "maxime" TO futil_mdp
            MOVE "RAMOND" TO futil_nom
            MOVE "Maxime" TO futil_prenom
            MOVE "maximeramond@gmail.com" TO futil_mail
            MOVE "0721723429" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "Commerce" TO futil_formation
            MOVE 10 TO futil_naissanceJour
            MOVE 10 TO futil_naissanceMois
            MOVE 1995 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U10 : ",cr_futil
            END-IF
            CLOSE futilisateur

      * Utilisateur 11 :
            MOVE "alepocreau" TO futil_login
            MOVE "anne" TO futil_mdp
            MOVE "LE POCREAU" TO futil_nom
            MOVE "Anne" TO futil_prenom
            MOVE "alp@gmail.com" TO futil_mail
            MOVE "0724433573" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "Commerce" TO futil_formation
            MOVE 11 TO futil_naissanceJour
            MOVE 02 TO futil_naissanceMois
            MOVE 1970 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U11 : ",cr_futil
            END-IF
            CLOSE futilisateur


      * Utilisateur 12 :
            MOVE "dnicol" TO futil_login
            MOVE "dominique" TO futil_mdp
            MOVE "NICOL" TO futil_nom
            MOVE "Dominique" TO futil_prenom
            MOVE "domnicol@gmail.com" TO futil_mail
            MOVE "0711976890" TO futil_tel
            MOVE 0 TO futil_type
            MOVE "art" TO futil_formation
            MOVE 19 TO futil_naissanceJour
            MOVE 11 TO futil_naissanceMois
            MOVE 1965 TO futil_naissanceAnnee

            OPEN I-O futilisateur
            WRITE tamp_futi
            END-WRITE
            IF cr_futil = 00
                DISPLAY "Insertion reussie"
            ELSE
                DISPLAY "Echec d'insertion"
                DISPLAY "U12 : ",cr_futil
            END-IF
            CLOSE futilisateur


      * Evenement 1
           MOVE "Gala 2023" TO fevent_nom
           MOVE "Soiree" TO fevent_type
           MOVE 21 TO fevent_dateJour
           MOVE 06 TO fevent_dateMois
           MOVE 2023 TO fevent_dateAnnee
           MOVE "admin" TO fevent_loginOrga
           MOVE "Soiree de fin d'annee" TO fevent_description
           MOVE "2 rue de la liberte, 35000 Rennes" TO fevent_adresse
           MOVE 150 TO fevent_seuil
           MOVE "20h00" TO fevent_heure

           OPEN I-O fevenement
              WRITE tamp_fevent
              END-WRITE
              IF cr_fevent = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "E1 : ",cr_fevent
              END-IF
              CLOSE fevenement

      * Evenement 2
           MOVE "Escape game de l'horreur" TO fevent_nom
           MOVE "Escape Game" TO fevent_type
           MOVE 15 TO fevent_dateJour
           MOVE 07 TO fevent_dateMois
           MOVE 2023 TO fevent_dateAnnee
           MOVE "legain" TO fevent_loginOrga
           MOVE "Pour se terrifier en cette belle periode" TO
           fevent_description
           MOVE "Les loges, 44140 Montbert" TO fevent_adresse
           MOVE 6 TO fevent_seuil
           MOVE "21h00" TO fevent_heure

           OPEN I-O fevenement
              WRITE tamp_fevent
              END-WRITE
              IF cr_fevent = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "E2 : ",cr_fevent
              END-IF
              CLOSE fevenement

      * Evenement 3
           MOVE "Afterwork" TO fevent_nom
           MOVE "Soiree" TO fevent_type
           MOVE 02 TO fevent_dateJour
           MOVE 06 TO fevent_dateMois
           MOVE 2023 TO fevent_dateAnnee
           MOVE "tmerlet" TO fevent_loginOrga
           MOVE "Afterwork de fin d'annee" TO fevent_description
           MOVE "9 Rue Bon Secours, 44000 Nantes" TO fevent_adresse
           MOVE 150 TO fevent_seuil
           MOVE "18h15" TO fevent_heure

           OPEN I-O fevenement
              WRITE tamp_fevent
              END-WRITE
              IF cr_fevent = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "E3 : ",cr_fevent
              END-IF
              CLOSE fevenement

      * Evenement 4
           MOVE "Karting exterieur" TO fevent_nom
           MOVE "Karting" TO fevent_type
           MOVE 21 TO fevent_dateJour
           MOVE 09 TO fevent_dateMois
           MOVE 2023 TO fevent_dateAnnee
           MOVE "cleau" TO fevent_loginOrga
           MOVE "Karting de rentree" TO fevent_description
           MOVE "19 Rte des Naudieres, 44880 Sautron" TO fevent_adresse
           MOVE 150 TO fevent_seuil
           MOVE "20h00" TO fevent_heure

           OPEN I-O fevenement
              WRITE tamp_fevent
              END-WRITE
              IF cr_fevent = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "E4 : ",cr_fevent
              END-IF
           CLOSE fevenement

      * Evenement 5
           MOVE "Paintball" TO fevent_nom
           MOVE "Paintball" TO fevent_type
           MOVE 01 TO fevent_dateJour
           MOVE 02 TO fevent_dateMois
           MOVE 2020 TO fevent_dateAnnee
           MOVE "alepocreau" TO fevent_loginOrga
           MOVE "Karting de rentree" TO fevent_description
           MOVE "19 Rte des Naudieres, 44880 Sautron" TO fevent_adresse
           MOVE 150 TO fevent_seuil
           MOVE "20h00" TO fevent_heure

           OPEN I-O fevenement
              WRITE tamp_fevent
              END-WRITE
              IF cr_fevent = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "E5 : ",cr_fevent
              END-IF
           CLOSE fevenement

      * Evenement 6
           MOVE "Journee Nationale MIAGE" TO fevent_nom
           MOVE "associatif" TO fevent_type
           MOVE 25 TO fevent_dateJour
           MOVE 06 TO fevent_dateMois
           MOVE 2023 TO fevent_dateAnnee
           MOVE "kcosquer" TO fevent_loginOrga
           MOVE "Journee de rencontre" TO fevent_description
           MOVE "123 rue des nuages, 75000 Paris" TO fevent_adresse
           MOVE 500 TO fevent_seuil
           MOVE "12h00" TO fevent_heure

           OPEN I-O fevenement
              WRITE tamp_fevent
              END-WRITE
              IF cr_fevent = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "E6 : ",cr_fevent
              END-IF
           CLOSE fevenement

      * Participant 1 :
           MOVE "Journee Nationale MIAGE" TO fpart_nomEvent
           MOVE "sledourner" TO fpart_login
           MOVE "acceptee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P1 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 2 :
           MOVE "Journee Nationale MIAGE" TO fpart_nomEvent
           MOVE "legain" TO fpart_login
           MOVE "acceptee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P2 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 3 :
           MOVE "Journee Nationale MIAGE" TO fpart_nomEvent
           MOVE "tmerlet" TO fpart_login
           MOVE "acceptee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P3 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 4 :
           MOVE "Journee Nationale MIAGE" TO fpart_nomEvent
           MOVE "tleberre" TO fpart_login
           MOVE "refusee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P4 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 5 :
           MOVE "Journee Nationale MIAGE" TO fpart_nomEvent
           MOVE "dnicol" TO fpart_login
           MOVE "refusee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P5 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 6 :
           MOVE "Journee Nationale MIAGE" TO fpart_nomEvent
           MOVE "jmeunier" TO fpart_login
           MOVE "attente" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P6 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 7 :
           MOVE "Journee Nationale MIAGE" TO fpart_nomEvent
           MOVE "kcosquer" TO fpart_login
           MOVE "attente" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P7 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 8 :
           MOVE "Journee Nationale MIAGE" TO fpart_nomEvent
           MOVE "mramond" TO fpart_login
           MOVE "refusee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P8 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 9 :
           MOVE "Journee Nationale MIAGE" TO fpart_nomEvent
           MOVE "cleau" TO fpart_login
           MOVE "accepte" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P9 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 10 :
           MOVE "Afterwork" TO fpart_nomEvent
           MOVE "kcosquer" TO fpart_login
           MOVE "acceptee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P10 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 11 :
           MOVE "Afterwork" TO fpart_nomEvent
           MOVE "legain" TO fpart_login
           MOVE "refusee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P11 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 12 :
           MOVE "Afterwork" TO fpart_nomEvent
           MOVE "tmerlet" TO fpart_login
           MOVE "acceptee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P12 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 13 :
           MOVE "Afterwork" TO fpart_nomEvent
           MOVE "tleberre" TO fpart_login
           MOVE "attente" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P13 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 14 :
           MOVE "Journee Nationale MIAGE" TO fpart_nomEvent
           MOVE "alepocreau" TO fpart_login
           MOVE "refusee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P14 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 15 :
           MOVE "Afterwork" TO fpart_nomEvent
           MOVE "jmeunier" TO fpart_login
           MOVE "attente" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P15 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 16 :
           MOVE "Afterwork" TO fpart_nomEvent
           MOVE "dnicol" TO fpart_login
           MOVE "attente" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P16 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 17 :
           MOVE "Afterwork" TO fpart_nomEvent
           MOVE "admin" TO fpart_login
           MOVE "acceptee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P17 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 18 :
           MOVE "Afterwork" TO fpart_nomEvent
           MOVE "gkoc" TO fpart_login
           MOVE "acceptee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P18 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 19 :
           MOVE "Paintball" TO fpart_nomEvent
           MOVE "dnicol" TO fpart_login
           MOVE "acceptee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P19 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 20 :
           MOVE "Paintball" TO fpart_nomEvent
           MOVE "alepocreau" TO fpart_login
           MOVE "acceptee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P20 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 21 :
           MOVE "Paintball" TO fpart_nomEvent
           MOVE "jmeunier" TO fpart_login
           MOVE "acceptee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P21 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 22 :
           MOVE "Paintball" TO fpart_nomEvent
           MOVE "gkoc" TO fpart_login
           MOVE "attente" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P22 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 23 :
           MOVE "Paintball" TO fpart_nomEvent
           MOVE "legain" TO fpart_login
           MOVE "refusee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P23 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 24 :
           MOVE "Paintball" TO fpart_nomEvent
           MOVE "cleau" TO fpart_login
           MOVE "attente" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P24 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 25 :
           MOVE "Paintball" TO fpart_nomEvent
           MOVE "kcosquer" TO fpart_login
           MOVE "attente" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P25 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 26 :
           MOVE "Paintball" TO fpart_nomEvent
           MOVE "sledourner" TO fpart_login
           MOVE "refusee" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P26 : ",cr_fpart
              END-IF
           CLOSE fparticipant

      * Participant 27 :
           MOVE "Paintball" TO fpart_nomEvent
           MOVE "mramond" TO fpart_login
           MOVE "accepte" TO fpart_etat

           OPEN I-O fparticipant
              WRITE tamp_fpart
              END-WRITE
              IF cr_fpart = 00
                  DISPLAY "Insertion reussie"
              ELSE
                  DISPLAY "Echec d'insertion"
                  DISPLAY "P27 : ",cr_fpart
              END-IF
           CLOSE fparticipant
      *-----------------------------------------------------------------
      *                  PROGRAMME PRINCIPAL
      *-----------------------------------------------------------------
           DISPLAY " _____________________________________"
           DISPLAY "|                                     |"
           DISPLAY "|    BIENVENUE SUR L'APPLICATION      |"
           DISPLAY "|_____________________________________|"
           MOVE FUNCTION CURRENT-DATE to WS-CURRENT-DATE-DATA
           PERFORM accueil
           STOP RUN.
      *-----------------------------------------------------------------
      *                  FONCTIONS ET PROCEDURES
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      *          Procedure de connexion a l'application
      *-----------------------------------------------------------------

       accueil.
           PERFORM WITH TEST AFTER UNTIL choix = 0
               DISPLAY " _____________________________________"
               DISPLAY "|                                     |"
               DISPLAY "|  1 - Me connecter a mon compte      |"
               DISPLAY "|  2 - Creer mon compte               |"
               DISPLAY "|  0 - Quitter                        |"
               DISPLAY "|_____________________________________|"
               DISPLAY " "
               DISPLAY "Votre choix :"
               ACCEPT choix

               EVALUATE choix
               WHEN 1
                   PERFORM connexion
                   IF erreurCompte = 0 THEN
                       PERFORM menuUtilisateur
                   END-IF
               WHEN 2
                   PERFORM creation_compte
               WHEN 0
                   DISPLAY " _____________________________________"
                   DISPLAY "|                                     |"
                   DISPLAY "|         Merci et a bientot          |"
                   DISPLAY "|_____________________________________|"
               END-EVALUATE
           END-PERFORM
           .


      *-----------------------------------------------------------------
      *          Procedure permettant de creer son compte utilisateur
      *-----------------------------------------------------------------
       creation_compte.
           DISPLAY " _____________________________________ "
           DISPLAY "|                                     |"
           DISPLAY "|         CREATION DE COMPTE          |"
           DISPLAY "|_____________________________________|"

      ** ouverture du fichier
           OPEN I-O futilisateur

      **enregistrement des informations dans le tampon
           DISPLAY "Entrer votre nom :"
           ACCEPT futil_nom
           DISPLAY "Entrer votre prenom :"
           ACCEPT futil_prenom

      **verification du format xxxxx@xxxx.fr ou xxxxx@xxxx.com
           MOVE 0 TO verif_mail_ok
           PERFORM WITH TEST AFTER UNTIL verif_mail_ok EQUAL 1
               DISPLAY "Entrer votre adresse mail :"
               ACCEPT futil_mail
               PERFORM verif_mail2
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

      ** on verifie que le jour de naissance est bien compris entre 1 et 31
           PERFORM WITH TEST AFTER UNTIL futil_naissanceJour>0 AND
               futil_naissanceJour<=31
           DISPLAY "JOUR (entre 1 et 31): "
               ACCEPT futil_naissanceJour
           END-PERFORM

      ** on verifie que le mois de naissance est bien compris entre 1 et 12
           PERFORM WITH TEST AFTER UNTIL futil_naissanceMois>0 AND
               futil_naissanceMois<=12
               DISPLAY "MOIS (entre 1 et 12): "
               ACCEPT futil_naissanceMois
           END-PERFORM

      **on verifie que l'annee de naissance est inferieur a l'annee en cours
            PERFORM WITH TEST AFTER UNTIL
                futil_naissanceAnnee<=WS-CURRENT-YEAR AND
                futil_naissanceAnnee<2006
                DISPLAY "ANNEE :"
               ACCEPT futil_naissanceAnnee

      **on verifie que l'utilisateur est majeur
               IF futil_naissanceAnnee>2005 AND
                   futil_naissanceAnnee<= WS-CURRENT-YEAR THEN
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|     Vous devez etre dans      |"
                   DISPLAY "|  l'annee de votre majorite    |"
                   DISPLAY "|     pour vous inscrire        |"
                   DISPLAY "|_______________________________|"

      ** on verifie qu'il ne met pas une annee superieure a l'anne courante
                ELSE IF futil_naissanceAnnee > WS-CURRENT-YEAR THEN
                    DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|   Veuillez saisir une annee   |"
                   DISPLAY "| inferieure a l'annee courante |"
                   DISPLAY "|     pour vous inscrire        |"
                   DISPLAY "|_______________________________|"
               END-IF
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
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|     Le login ne peut pas      |"
                   DISPLAY "|           etre vide !         |"
                   DISPLAY "|_______________________________|"
               END-IF
           END-PERFORM

           MOVE 0 TO verif_mdp_ok
           PERFORM UNTIL verif_mdp_ok EQUAL 1
               DISPLAY "Entrer votre mot de passe :"
               ACCEPT futil_mdp
               IF futil_mdp NOT EQUAL SPACE THEN
                   MOVE 1 TO verif_mdp_ok
               ELSE
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|  Le mot de passe ne peut pas  |"
                   DISPLAY "|           etre vide !         |"
                   DISPLAY "|_______________________________|"
               END-IF
           END-PERFORM

      * on insere les informations dans le fichier
           WRITE tamp_futi
               INVALID KEY
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|   Un probleme est survenu     |"
                   DISPLAY "|                               |"
                   DISPLAY "|     Le compte n'a pas pu      |"
                   DISPLAY "|           etre cree !         |"
                   DISPLAY "|_______________________________|"
                   MOVE 1 to erreurCompte
               NOT INVALID KEY
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|          INFORMATION          |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|          compte cree !        |"
                   DISPLAY "|_______________________________|"
           END-WRITE.
      *     DISPLAY cr_futil
           CLOSE futilisateur.

      ******************************************************************
      * Fonction annexe :
      *    Procedure permettant de verifier l'adresse mail
      *    saisi par l'utilisateur
      ******************************************************************
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

       verif_mail2.
           MOVE 0 TO verif_arobase
           MOVE 0 TO verif_dot
           MOVE 0 TO verif_domain
           MOVE 0 TO verif_mail_ok
           MOVE 1 TO I
           MOVE futil_mail TO chaine

      *    Le premier caractere est une lettre
           IF chaine(1:1) IS ALPHABETIC THEN
               MOVE 0 TO fin_boucle
      *    La chaine contient un arobase :
               PERFORM UNTIL verif_arobase = 1 or fin_boucle = 1
                   IF chaine(I:1) EQUAL '@' THEN
                       MOVE 1 TO verif_arobase
                   END-IF
                   IF chaine(I:1) EQUAL SPACE THEN
                       MOVE 1 TO fin_boucle
                   END-IF
                   ADD 1 TO I
               END-PERFORM
      *    Une lettre est presente apres l'arobase
      *    Un bug peut venir du I auquel il faut ajouter 1
               IF chaine(I:1) IS ALPHABETIC THEN
                   IF verif_arobase = 1 THEN
                       MOVE 0 TO fin_boucle
      *    On verifie la presence d'un point
                       PERFORM UNTIL verif_dot = 1 OR fin_boucle = 1
                           IF chaine(I:1) EQUAL "." THEN
                               MOVE 1 TO verif_dot
                           END-IF
                           IF chaine(I:1) EQUAL SPACE THEN
                               MOVE 1 TO fin_boucle
                           END-IF
                           ADD 1 TO I
                       END-PERFORM
                       IF verif_dot = 1 THEN
                           ADD 1 TO I
      *    On verifie la presence d'une lettre apres le point
                           IF chaine(I:1) IS ALPHABETIC THEN
                               MOVE 1 TO verif_mail_ok
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .



      ******************************************************************
      * Fonction annexe :
      *    Procedure permettant de verifier le numero de telephone
      *    saisi par l'utilisateur
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
           END-IF
           .



      *-----------------------------------------------------------------
      *          Procedure permettant de se connecter a son compte
      *-----------------------------------------------------------------
       connexion.
           MOVE SPACE TO futil_mdp
           MOVE SPACE TO futil_login
           MOVE 0 TO verif

           PERFORM UNTIL verif EQUAL 1
               DISPLAY " _____________________________________ "
               DISPLAY "|                                     |"
               DISPLAY "|             CONNEXION               |"
               DISPLAY "|_____________________________________|"
               DISPLAY "|  Entrer votre login :               |"
               ACCEPT login
               DISPLAY "|  Entrer votre mot de passe :        |"
               ACCEPT mdp
               DISPLAY "|_____________________________________|"

      ** Lors de la connexion on verifie que le login et le mot de passe
      ** ne soient pas vides
               IF mdp NOT EQUAL SPACE AND login NOT EQUAL SPACE THEN
                   MOVE login TO futil_login
                   OPEN INPUT futilisateur
                   READ futilisateur
                       INVALID KEY
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|        Compte inexistant      |"
                           DISPLAY "|                               |"
                           DISPLAY "|      Veuillez en creer un     |"
                           DISPLAY "|_______________________________|"
                           PERFORM creation_compte
                       NOT INVALID KEY
                           IF futil_mdp EQUAL mdp THEN
                               MOVE login TO loginSaved
                               MOVE futil_type TO typeSaved
                               MOVE 1 TO verif
                           ELSE
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|     Mot de passe incorrect    |"
                           DISPLAY "|_______________________________|"
                           END-IF
                   END-READ
                   CLOSE futilisateur

               ELSE
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|  Le login ou le mot de passe  |"
                   DISPLAY "|     ne peuvent etre vides     |"
                   DISPLAY "|_______________________________|"
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
           DISPLAY " _____________________________________ "
           DISPLAY "|                                     |"
           DISPLAY "|            MENU PRINCIPAL           |"
           DISPLAY "|_____________________________________|"
           DISPLAY "|                                     |"
           DISPLAY "|     Que souhaitez-vous faire ?      |"
           DISPLAY "|                                     |"
           DISPLAY "|  1 - Gerer votre profil             |"
           DISPLAY "|  2 - Gerer les evenements           |"
           DISPLAY "|  3 - Rechercher un evenement        |"
           DISPLAY "|  4 - Recherche un utilisateur       |"
           DISPLAY "|  5 - Afficher etat des inscriptions |"
           IF typeSaved = 1 THEN
               DISPLAY "|                                     |"
               DISPLAY "|-------------------------------------|"
               DISPLAY "|                                     |"
               DISPLAY "|  6 - Afficher les statistiques      |"
               DISPLAY "|  7 - Gerer les utilisateurs         |"
               DISPLAY "|  8 - Archiver un evenement passe    |"
           END-IF

           DISPLAY "|                                     |"
           DISPLAY "|-------------------------------------|"
           DISPLAY "|                                     |"
           DISPLAY "|  0 - Deconnexion                    |"
           DISPLAY "|_____________________________________|"
           DISPLAY " "
           DISPLAY "Votre choix :"

           ACCEPT fermeAppli

      **en fonction du choix saisi par l'utilisateur, il sera dirige vers
      **le menu souhaite
           EVALUATE fermeAppli
               WHEN 1 PERFORM gererProfil
               WHEN 2 PERFORM gestionEvenement
               WHEN 3 PERFORM rechercherEvent
               WHEN 4 PERFORM rechercherUtil
               WHEN 5 PERFORM etatInscription
               WHEN 6
                       IF typeSaved=1
                       THEN PERFORM afficheStatistique
                       ELSE
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\   NON AUTORISE   /!\    |"
                           DISPLAY "|                               |"
                           DISPLAY "|-------------------------------|"
                           DISPLAY "|                               |"
                           DISPLAY "|       Vous n'etes pas         |"
                           DISPLAY "|      autorise a acceder       |"
                           DISPLAY "|      a cette ressource        |"
                           DISPLAY "|                               |"
                           DISPLAY "|_______________________________|"
                           PERFORM menuUtilisateur
                       END-IF
               WHEN 7
                      IF typeSaved = 1
                           THEN PERFORM modifierProfilAdmin
                      ELSE
                           DISPLAY " _______________________________"
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\   NON AUTORISE   /!\    |"
                           DISPLAY "|                               |"
                           DISPLAY "|-------------------------------|"
                           DISPLAY "|                               |"
                           DISPLAY "|       Vous n'etes pas         |"
                           DISPLAY "|     autorise a acceder        |"
                           DISPLAY "|      a cette ressource        |"
                           DISPLAY "|                               |"
                           DISPLAY "|_______________________________|"
                           PERFORM menuUtilisateur
                       END-IF
               WHEN 8
                       IF typeSaved = 1
                           THEN PERFORM archivageEvent
                       ELSE
                           DISPLAY " _______________________________"
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\   NON AUTORISE   /!\    |"
                           DISPLAY "|                               |"
                           DISPLAY "|-------------------------------|"
                           DISPLAY "|                               |"
                           DISPLAY "|       Vous n'etes pas         |"
                           DISPLAY "|      autorise a acceder       |"
                           DISPLAY "|      a cette ressource        |"
                           DISPLAY "|                               |"
                           DISPLAY "|_______________________________|"
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
           DISPLAY " _____________________________________"
           DISPLAY "|                                     |"
           DISPLAY "|         GESTION DU PROFIL           |"
           DISPLAY "|-------------------------------------|"
           DISPLAY "|                                     |"
           DISPLAY "|  1 - Modifier votre profil          |"
           DISPLAY "|  2 - Supprimer votre profil         |"
           DISPLAY "|  3 - Consulter votre profil         |"
           DISPLAY "|                                     |"
           DISPLAY "|-------------------------------------|"
           DISPLAY "|                                     |"
           DISPLAY "|  0 - Revenir au menu precedent      |"
           DISPLAY "|_____________________________________|"
           DISPLAY " "
           DISPLAY "Votre choix :"
      **         DISPLAY "choix : "choixProfil
                 ACCEPT choixProfil
      **         DISPLAY "choix : "choixProfil

           EVALUATE choixProfil
               WHEN 1 PERFORM modifierUtilisateur
               WHEN 2 PERFORM suppression_utilisateur
               WHEN 3 PERFORM consulterProfil
               WHEN 0 PERFORM menuUtilisateur
           END-EVALUATE
           END-PERFORM
           .


      *-----------------------------------------------------------------
      *      Procedure gerant le menu de recherche d'un evenement
      *-----------------------------------------------------------------
       rechercherEvent.
           PERFORM WITH TEST AFTER UNTIL choixEvent =0
           DISPLAY " _____________________________________"
           DISPLAY "|                                     |"
           DISPLAY "|             RECHERCHER              |"
           DISPLAY "|            UN EVENEMENT             |"
           DISPLAY "|-------------------------------------|"
           DISPLAY "|                                     |"
           DISPLAY "|        Rechercher un evenement      |"
           DISPLAY "|                                     |"
           DISPLAY "|  1 - Par son nom                    |"
           DISPLAY "|  2 - Par son type                   |"
           DISPLAY "|                                     |"
           DISPLAY "|-------------------------------------|"
           DISPLAY "|                                     |"
           DISPLAY "|  0 - Revenir au menu precedent      |"
           DISPLAY "|_____________________________________|"
           DISPLAY " "
           DISPLAY "Votre choix :"
           ACCEPT choixEvent

           EVALUATE choixEvent
              WHEN 1 PERFORM rechercherNom
              WHEN 2 PERFORM rechercherType
              WHEN 0 PERFORM menuUtilisateur
           END-EVALUATE

           END-PERFORM
           .
      *-----------------------------------------------------------------
      *      Procedure gerant le menu de recherche d'un utilisateur
      *-----------------------------------------------------------------
       rechercherUtil.
           PERFORM WITH TEST AFTER UNTIL choixEvent =0
           DISPLAY " _____________________________________"
           DISPLAY "|                                     |"
           DISPLAY "|             RECHERCHER              |"
           DISPLAY "|           UN UTILISATEUR            |"
           DISPLAY "|-------------------------------------|"
           DISPLAY "|                                     |"
           DISPLAY "|  1 - Par son nom                    |"
           DISPLAY "|  2 - Par son login                  |"
           DISPLAY "|  3 - Afficher tous les organisateurs|"
           DISPLAY "|  4 - Afficher tous les utilisateurs |"
           DISPLAY "|                                     |"
           DISPLAY "|-------------------------------------|"
           DISPLAY "|                                     |"
           DISPLAY "|  0 - Revenir au menu precedent      |"
           DISPLAY "|_____________________________________|"
           DISPLAY " "
           DISPLAY "Votre choix :"
           ACCEPT choixEvent

           EVALUATE choixEvent
              WHEN 1 PERFORM rechercherUtilisateurNom
              WHEN 2 PERFORM rechercherUtilisateurLogin
              WHEN 3 PERFORM affichage_organisateur
              WHEN 4 PERFORM consulterUtilisateurs
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
               DISPLAY " _____________________________________"
               DISPLAY "|                                     |"
               DISPLAY "|         GESTION D'EVENEMENT         |"
               DISPLAY "|-------------------------------------|"
               DISPLAY "|                                     |"
               DISPLAY "|  1 - Creer un evenement             |"
               DISPLAY "|  2 - Modifier un evenement          |"
               DISPLAY "|  3 - Supprimer un evenement         |"
               DISPLAY "|  4 - Afficher evenements            |"
               DISPLAY "|  5 - Gestion des demandes           |"
               DISPLAY "|                                     |"
               DISPLAY "|-------------------------------------|"
               DISPLAY "|                                     |"
               DISPLAY "|-------------------------------------|"
               DISPLAY "|                                     |"
               DISPLAY "|  0 - Revenir au menu precedent      |"
               DISPLAY "|_____________________________________|"
               DISPLAY " "
               DISPLAY "Votre choix :"
               ACCEPT choix

               EVALUATE choix
                WHEN 1 PERFORM creerEvent
                WHEN 2 PERFORM modifierEvent
                WHEN 3 PERFORM supprimerEvent
                WHEN 4 PERFORM afficheEvent
                WHEN 5 PERFORM gestion_demandes
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
               DISPLAY " _____________________________________"
               DISPLAY "|                                     |"
               DISPLAY "|       AFFICHAGE DES STATISTIQUES    |"
               DISPLAY "|-------------------------------------|"
               DISPLAY "|                                     |"
               DISPLAY "|  1 - Statistiques generales         |"
               DISPLAY "|  2 - Statistique selon formation    |"
               DISPLAY "|      mois et type                   |"
               DISPLAY "|                                     |"
               DISPLAY "|-------------------------------------|"
               DISPLAY "|                                     |"
               DISPLAY "|  0 - Revenir au menu precedent      |"
               DISPLAY "|_____________________________________|"
               DISPLAY " "
               DISPLAY "Votre choix :"
               ACCEPT choixStat

               EVALUATE choixStat
               WHEN 1 PERFORM afficherStats
               WHEN 2 PERFORM statFormaMois
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
               DISPLAY " _____________________________________ "
               DISPLAY "|                                     |"
               DISPLAY "|   MODIFIER MON PROFIL UTILISATEUR   |"
               DISPLAY "|-------------------------------------|"
               DISPLAY "|                                     |"
               DISPLAY "| Que voulez-vous modifier ?          |"
               DISPLAY "|                                     |"
               DISPLAY "|  1 - Nom                            |"
               DISPLAY "|  2 - Prenom                         |"
               DISPLAY "|  3 - Mail                           |"
               DISPLAY "|  4 - Telephone                      |"
               DISPLAY "|  5 - Formation                      |"
               IF futil_type=1
                   DISPLAY "|  6 - Type utilisateur               |"
               END-IF
               DISPLAY "|                                     |"
               DISPLAY "|-------------------------------------|"
               DISPLAY "|                                     |"
               DISPLAY "|  0 - Revenir au menu precedent      |"
               DISPLAY "|_____________________________________|"
               DISPLAY " "
               DISPLAY "Votre choix :"

               ACCEPT choixUtil
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
                       PERFORM verif_mail2
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
      *    Procedure permettant a l'administrateur de rechercher un
      *    utilisateur membre afin de lui donner ou enlever les droits admin
      *-----------------------------------------------------------------
       modifierProfilAdmin.
           PERFORM consulterUtilisateurs
           DISPLAY " _____________________________________"
           DISPLAY "|                                     |"
           DISPLAY "|    MODIFIER TYPE D'UTILISATEUR      |"
           DISPLAY "|-------------------------------------|"
           DISPLAY "|                                     |"
           DISPLAY "|           Quel utilisateur          |"
           DISPLAY "|       souhaitez-vous modifier ?     |"
           DISPLAY "|_____________________________________|"
           DISPLAY" "
           DISPLAY "Veuillez saisir son login :"
           ACCEPT futil_login
           OPEN I-O futilisateur
           READ futilisateur
               INVALID KEY
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|  Saisie du login incorrecte   |"
                   DISPLAY "|_______________________________|"
                   DISPLAY" "
                   DISPLAY "Veuillez reessayer :"
               NOT INVALID KEY
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|          INFORMATION          |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|     La seule modification     |"
                   DISPLAY "|      possible est le type     |"
                   DISPLAY "|         d'utilisateur         |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|   Etes-vous sur de vouloir    |"
                   DISPLAY "|         le modifier ?         |"
                   DISPLAY "|                               |"
                   DISPLAY "|  1 - Oui                      |"
                   DISPLAY "|  0 - Non                      |"
                   DISPLAY "|_______________________________|"
                   DISPLAY " "
                  DISPLAY "Votre choix :"
                   ACCEPT reponse
                   IF reponse = 1 THEN
                       DISPLAY "Veuillez saisir le nouveau type"
                       DISPLAY "d'utilisateur (1 = Admin, 0 = Membre) :"
                       ACCEPT futil_type
                       PERFORM modifUtil
                       IF futil_login = loginSaved THEN
                           MOVE futil_type TO typeSaved
                       END-IF
                   END-IF
                   CLOSE futilisateur
           END-READ
           .

      ******************************************************************
      *    Fonction parallele :
      *    Fonction qui permet d'eviter du doublon de code sur la modification
      *    d'un element dans le fichier futilisateurs
      ******************************************************************
       modifUtil.
           REWRITE tamp_futi
               IF cr_futil = 00 THEN
               DISPLAY " _______________________________ "
               DISPLAY "|                               |"
               DISPLAY "|          INFORMATION          |"
               DISPLAY "|_______________________________|"
               DISPLAY "|                               |"
               DISPLAY "|    Modification reussie !     |"
               DISPLAY "|_______________________________|"
               ELSE
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|     Echec de modification     |"
                   DISPLAY "|                               |"
                   DISPLAY "|   Reesayer ulterieurement     |"
                   DISPLAY "|_______________________________|"
               END-IF.
      *-----------------------------------------------------------------
      *      Procedure permettant de consulter son profil utilisateur
      *      Par mesure de securite le mot de passe n'est pas affiche
      *-----------------------------------------------------------------
       consulterProfil.
           DISPLAY " _____________________________________ "
           DISPLAY "|                                     |"
           DISPLAY "|  CONSULTER MON PROFIL UTILISATEUR   |"
           DISPLAY "|-------------------------------------|"
           DISPLAY "|                                     |"
           OPEN INPUT futilisateur
           MOVE loginSaved TO futil_login
           READ futilisateur
               INVALID KEY
                   DISPLAY "|  /!\    Compte inexistant     /!\   |"
                   DISPLAY "|_____________________________________|"

               NOT INVALID KEY
                   DISPLAY "| Nom :                               |"
                   DISPLAY "|   "futil_nom
                   DISPLAY "| Prenom :                            |"
                   DISPLAY "|   "futil_prenom
                   DISPLAY "| Mail :                              |"
                   DISPLAY "|   "futil_mail
                   DISPLAY "| Telephone :                         |"
                   DISPLAY "|   "futil_tel
                   DISPLAY "| Formation :                         |"
                   DISPLAY "|   "futil_formation
                   DISPLAY "| Login :                             |"
                   DISPLAY "|   "futil_login
                   DISPLAY "| Type d'utilisateur :                |"
                   IF futil_type=1
                       DISPLAY "|   Administrateur"
                   ELSE
                       IF futil_type=2 THEN
                               DISPLAY "|  Organisateur"
                           ELSE
                               DISPLAY "|  Membre"
                       END-IF
                   END-IF
                   DISPLAY "| Date de naissance :                 |"
                   DISPLAY "|   " futil_naissanceJour"/"
                       futil_naissanceMois"/"
      -              futil_naissanceAnnee
                       DISPLAY "|-------------------------------------|"
                       DISPLAY "|                                     |"
                       DISPLAY "|     Une erreur sur votre profil ?   |"
                       DISPLAY "|                                     |"
                       DISPLAY "|-------------------------------------|"
                       DISPLAY "|                                     |"
                       DISPLAY "|    Souhaitez-vous la modifier ?     |"
                       DISPLAY "|                                     |"
                       DISPLAY "|  1 - Oui                            |"
                       DISPLAY "|  0 - Non                            |"
                       DISPLAY "|_____________________________________|"
                       DISPLAY " "
                      DISPLAY "Votre choix :"
                       ACCEPT erreurProfil
                   IF erreurProfil = 1
                       PERFORM modifierUtilisateur
                   ELSE
                       PERFORM gererProfil
                   END-IF
           END-READ
           CLOSE futilisateur.


      *-----------------------------------------------------------------
      *    Procedure permettant d'afficher tous les utilisateurs
      *    presents dans l'application. Qu'ils soient membres ou admin
      *-----------------------------------------------------------------
       consulterUtilisateurs.
           DISPLAY " _____________________________________"
           DISPLAY "|                                     |"
           DISPLAY "|   CONSULTER TOUS LES UTILISATEURS   |"
           DISPLAY "|-------------------------------------|"
           OPEN INPUT futilisateur
           MOVE 0 TO Fin
           PERFORM WITH TEST AFTER UNTIL Fin = 1
               READ futilisateur
                   AT END
                       MOVE 1 TO Fin
                   NOT AT END
                       DISPLAY "| Nom :                               |"
                       DISPLAY "|  "futil_nom
                       DISPLAY "| Prenom :                            |"
                       DISPLAY "|  " futil_prenom
                       DISPLAY "| Mail :                              |"
                       DISPLAY "|  "futil_mail
                       DISPLAY "| Formation :                         |"
                       DISPLAY "|  "futil_formation
                       DISPLAY "| Login :                             |"
                       DISPLAY "|  "futil_login
                       DISPLAY "| Type d'utilisateur :                |"
                       IF futil_type=1
                           DISPLAY "|  Administrateur"
                       ELSE
                           IF futil_type=2 THEN
                               DISPLAY "|  Organisateur"
                           ELSE
                               DISPLAY "|  Membre"
                       END-IF
                       DISPLAY "|____________________________________|"
               END-READ
           END-PERFORM
           CLOSE futilisateur.

      *-----------------------------------------------------------------
      * Procedure permettant de rechercher un evenement en fonction de
      *                        son nom
      *----------------------------------------------------------------
       rechercherNom.
           PERFORM afficheEvent
           OPEN INPUT fevenement
               DISPLAY " ____________________________________ "
               DISPLAY "|                                    |"
               DISPLAY "|  RECHERCHER EVENEMENT PAR LE NOM   |"
               DISPLAY "|____________________________________|"
               DISPLAY " "
               DISPLAY "Quel evenement voulez-vous rechercher ?"
               DISPLAY "(saisir le nom)"
               ACCEPT fevent_nom
               MOVE fevent_nom TO nomSaved
               READ fevenement
                   INVALID KEY
                       DISPLAY " _______________________________ "
                       DISPLAY "|                               |"
                       DISPLAY "|   /!\       ERREUR       /!\  |"
                       DISPLAY "|_______________________________|"
                       DISPLAY "|                               |"
                       DISPLAY "|     Evenement non trouve      |"
                       DISPLAY "|                               |"
                       DISPLAY "|    Changer votre recherche    |"
                       DISPLAY "|_______________________________|"
                   NOT INVALID KEY
                       DISPLAY " ______________________________"
                       DISPLAY "|    Voici les informations de |"
                       DISPLAY "|         l'evenement :        |"
                       DISPLAY "| Nom :                        |" 
                       DISPLAY "|   "fevent_nom
                       DISPLAY "| Type :                       |" 
                       DISPLAY "|   "fevent_type
                       DISPLAY "| Date :                       |" 
                       DISPLAY "|   "fevent_dateJour"/"
      -    fevent_dateMois"/"fevent_dateAnnee
                       MOVE fevent_dateJour TO dateJour
                       MOVE fevent_dateMois TO dateMois
                       MOVE fevent_dateAnnee TO dateAnnee
                       DISPLAY "| Heure de debut :             |"
                       DISPLAY "|   "fevent_heure
                       DISPLAY "| Description :                |"
                       DISPLAY "|   "fevent_description
                       DISPLAY "| Adresse :                    |"
                       DISPLAY "|   "fevent_adresse
                       DISPLAY "| Seuil :                      |" 
                       DISPLAY "|   "fevent_seuil
                       DISPLAY "| Login organisateur :         |"
                       DISPLAY "|   "fevent_loginOrga
                       DISPLAY "|______________________________|"
                       DISPLAY " ____________________________________"
                       DISPLAY "|                                    |"
                       DISPLAY "|          INSCRIPTION EVENT         |"
                       DISPLAY "|------------------------------------|"
                       DISPLAY "|                                    |"
                       DISPLAY "|     Voulez-vous vous inscrire ?    |"
                       DISPLAY "|                                    |"
                       DISPLAY "|  1 - Oui                           |"
                       DISPLAY "|  2 - Non                           |"
                       DISPLAY "|                                    |"
                       DISPLAY "|____________________________________|"
                       DISPLAY " "
                      DISPLAY "Votre choix :"
                       MOVE 0 TO inscription

                       ACCEPT inscription
                       IF inscription = 1 THEN
                          PERFORM inscriptionEvent
                       ELSE
                        DISPLAY " ____________________________________"
                        DISPLAY "|                                    |"
                        DISPLAY "|   Merci pour votre consultation !  |"
                        DISPLAY "|____________________________________|"
                           PERFORM rechercherEvent
                       END-IF
               END-READ
           CLOSE fevenement.

      *-----------------------------------------------------------------
      * Procedure permettant de rechercher un evenement en fonction de
      *                        son type
      *----------------------------------------------------------------
       rechercherType.
           PERFORM afficheEvent
             DISPLAY " ____________________________________ "
             DISPLAY "|                                    |"
             DISPLAY "|   RECHERCHER EVENEMENT PAR TYPE    |"
             DISPLAY "|____________________________________|"
             DISPLAY " "
             DISPLAY "Quel type d'evenement voulez-vous rechercher ?"
             DISPLAY "(saisir le type)"
             ACCEPT typeEvent
             OPEN INPUT fevenement
             MOVE 0 to Fin
             MOVE typeEvent TO fevent_type
             START fevenement, KEY IS = fevent_type
               INVALID KEY
                 DISPLAY " _______________________________ "
                 DISPLAY "|                               |"
                 DISPLAY "|   /!\       ERREUR       /!\  |"
                 DISPLAY "|_______________________________|"
                 DISPLAY "|                               |"
                 DISPLAY "|        Type non trouve        |"
                 DISPLAY "|                               |"
                 DISPLAY "|    Changer votre recherche    |"
                 DISPLAY "|_______________________________|"
               NOT INVALID KEY
                 DISPLAY "Voici les informations de l'evenement :"
                 PERFORM WITH TEST AFTER UNTIL Fin = 1
                   READ    fevenement    NEXT
                   AT END
                     MOVE 1 TO Fin
                   NOT AT END
                     IF fevent_type = typeEvent THEN
                       DISPLAY "Nom : " fevent_nom
                       DISPLAY "Type : "fevent_type
                       DISPLAY "Date : "fevent_dateJour"/"
      -                fevent_dateMois"/"fevent_dateAnnee
                       MOVE fevent_dateJour TO dateJour
                       MOVE fevent_dateMois TO dateMois
                       MOVE fevent_dateAnnee TO dateAnnee
                       DISPLAY "Heure de debut : " fevent_heure
                       DISPLAY "Description : " fevent_description
                       DISPLAY "Adresse : " fevent_adresse
                       DISPLAY "Seuil : "fevent_seuil
                       DISPLAY "Login organisateur : " fevent_loginOrga
                       DISPLAY "------------------------------------"
                     END-IF
                   END-READ
                 END-PERFORM
             END-START
             DISPLAY " ____________________________________"
             DISPLAY "|                                    |"
             DISPLAY "|          INSCRIPTION EVENT         |"
             DISPLAY "|------------------------------------|"
             DISPLAY "|                                    |"
             DISPLAY "|     Voulez-vous vous inscrire a    |"
             DISPLAY "|         un des evenement ?         |"
             DISPLAY "|                                    |"
             DISPLAY "|  1 - Oui                           |"
             DISPLAY "|  2 - Non                           |"
             DISPLAY "|                                    |"
             DISPLAY "|____________________________________|"
             DISPLAY " "
             DISPLAY "Votre choix :"
             MOVE 0 TO inscription
             ACCEPT inscription
               IF inscription = 1 THEN
                 DISPLAY "Veuillez saisir son nom :"
                 ACCEPT fevent_nom
                 MOVE fevent_nom TO nomSaved
                 READ fevenement
                   INVALID KEY

                     DISPLAY " _______________________________ "
                     DISPLAY "|                               |"
                     DISPLAY "|   /!\       ERREUR       /!\  |"
                     DISPLAY "|_______________________________|"
                     DISPLAY "|                               |"
                     DISPLAY "|     Evenement non trouve      |"
                     DISPLAY "|_______________________________|"
                   NOT INVALID KEY
                     PERFORM inscriptionEvent
                 END-READ
               ELSE
                 DISPLAY " ____________________________________"
                 DISPLAY "|                                    |"
                 DISPLAY "|   Merci pour votre consultation !  |"
                 DISPLAY "|____________________________________|"
                 PERFORM rechercherEvent
               END-IF
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
           MOVE nomEvent to fevent_nom
           READ fevenement
           INVALID KEY
               MOVE 0 TO estValideEvenementResultat
           NOT INVALID KEY
                   MOVE 1 TO estValideEvenementResultat
           END-READ

           IF cr_fevent = 00
           THEN DISPLAY "Evenement existant en cours"
      *     ELSE DISPLAY "Evenement non trouve"
           END-IF
           CLOSE fevenement
           .

      ******************************************************************
      *    Fonction parallele :
      *    Fonction qui verifie que le nom de l'evenement n'est pas deja
      *    present dans fhistorique
      ******************************************************************
       existeEventHisto.
           OPEN INPUT fhistorique
            MOVE nomEvent TO fhisto_nom
           READ fhistorique
           INVALID KEY
               MOVE 0 TO estValideEvenementResultatHisto
           NOT INVALID KEY
                   MOVE 1 TO estValideEvenementResultatHisto
           END-READ

           IF cr_fhisto = 00
           THEN DISPLAY "Evenement existant dans l'historique"
      *     ELSE DISPLAY "Evenement non trouve"
           END-IF
           CLOSE fhistorique
           .

      *----------------------------------------------------------------
      *    Fonction qui permet de creer un evenement
      *----------------------------------------------------------------
       creerEvent.
           DISPLAY " ____________________________________"
           DISPLAY "|                                    |"
           DISPLAY "|         CREATION EVENEMENT         |"
           DISPLAY "|------------------------------------|"
      **on verifie que le nom de l'evenement est bon
           PERFORM WITH TEST AFTER UNTIL estValideEvenementResultat = 0
               AND estValideEvenementResultatHisto = 0
               DISPLAY "|Saisir le nom de l'evenement        |"
               DISPLAY "|(maximum 40 caracteres)             |"
               ACCEPT nomEvent
               PERFORM existeEvent
               PERFORM existeEventHisto
           END-PERFORM
      **aucune contrainte sur ce champ
           DISPLAY "|Saisir le type d'evenement          |"
           ACCEPT fevent_type
           DISPLAY "|Saisir la date de l'evenement       |"

           PERFORM WITH TEST AFTER UNTIL dateComparee = 2
      **on verifie que le jour est bien compris entre 1 et 31
               PERFORM WITH TEST AFTER UNTIL
                   fevent_dateJour>0 AND fevent_dateJour<=31
                   DISPLAY "|JOUR :                              |"
                   ACCEPT fevent_dateJour
               END-PERFORM
      **on verifie que le mois est bien compris entre 1 et 12
               PERFORM WITH TEST AFTER UNTIL
                   fevent_dateMois>0 AND fevent_dateMois<=12
                   DISPLAY "|MOIS :                              |"
                   ACCEPT fevent_dateMois
               END-PERFORM
      **on verifie que l'annee est bien superieure ou egale a l'annee courante
               PERFORM WITH TEST AFTER UNTIL
                   fevent_dateAnnee>=WS-CURRENT-YEAR
                   DISPLAY "|ANNEE :                             |"
                   ACCEPT fevent_dateAnnee
               END-PERFORM
               PERFORM comparer_date
           END-PERFORM
           DISPLAY "|Veuillez decrire votre evenement    |"
           DISPLAY "|Format : maximum 250 caracteres     |"
           ACCEPT descriptionEvent
           DISPLAY "|Veuillez saisir l'adresse de        |"
           DISPLAY "|l'evenement                         |"
           ACCEPT adresseEvent
      **on verifie que le seuil n'est pas de 0 et donc aucune inscription possible
           PERFORM UNTIL seuilEvent > 0
               DISPLAY "|Veuillez saisir le nombre maximal   |"
               DISPLAY "|de personne                         |"
               ACCEPT seuilEvent
           END-PERFORM
      **/!\ nous n'avons pas reussie a faire une verification sur le format de l'heure/!\
           PERFORM WITH TEST AFTER UNTIL estValideHeure = 1
               DISPLAY "|Veuillez saisir l'heure de debut    |"
               DISPLAY "|de l'evenement                      |"
               DISPLAY "|Format : xxhxx, avec x un chiffre   |"
               ACCEPT heureEvent
               PERFORM verifHeure
               DISPLAY "|____________________________________|"
           END-PERFORM

           OPEN I-O fevenement
           MOVE nomEvent TO fevent_nom
           MOVE typeEvent TO fevent_type
           MOVE loginSaved TO fevent_loginOrga
           MOVE descriptionEvent TO fevent_description
           MOVE adresseEvent TO fevent_adresse
           MOVE seuilEvent TO fevent_seuil
           MOVE heureEvent TO fevent_heure
           MOVE 2 TO futil_type


           WRITE tamp_fevent
           END-WRITE
           IF cr_fevent=00
               DISPLAY " _______________________________ "
               DISPLAY "|                               |"
               DISPLAY "|          INFORMATION          |"
               DISPLAY "|_______________________________|"
               DISPLAY "|                               |"
               DISPLAY "|      Insertion reussie !      |"
               DISPLAY "|_______________________________|"
           ELSE
               DISPLAY " _______________________________ "
               DISPLAY "|                               |"
               DISPLAY "|   /!\       ERREUR       /!\  |"
               DISPLAY "|_______________________________|"
               DISPLAY "|                               |"
               DISPLAY "|   Un probleme est survenu     |"
               DISPLAY "|                               |"
               DISPLAY "|   L'evenement n'a pas pu      |"
               DISPLAY "|         etre cree !           |"
               DISPLAY "|_______________________________|"
               DISPLAY cr_fevent
           END-IF

           CLOSE fevenement.

      *-----------------------------------------------------------------
      *    Procedure permettant d'afficher les evenements
      *-----------------------------------------------------------------
       afficheEvent.
           DISPLAY " ______________________________ "
           DISPLAY "|                              |"
           DISPLAY "|     AFFICHAGE EVENEMENT      |"
           DISPLAY "|------------------------------|"
           OPEN INPUT fevenement
               MOVE 0 TO Fin
      *         PERFORM WITH TEST AFTER UNTIL Fin = 1
               PERFORM UNTIL Fin = 1
                   READ fevenement NEXT
      *             MOVE 1 TO Fin
                   AT END
                       MOVE 1 TO Fin
                   NOT AT END
                       PERFORM comparer_date
                       IF dateComparee = 2 THEN
                           DISPLAY "|Nom :                         |"
                            DISPLAY "|  "fevent_nom
                           DISPLAY "|Type :                        |"
                           DISPLAY "|  "fevent_type
                      DISPLAY "|------------------------------|"
                       END-IF

                   END-READ
               END-PERFORM
               DISPLAY "|______________________________|"
               CLOSE fevenement
           .

      *-----------------------------------------------------------------
      *    Procedure permettant de s'inscrire a un evenement
      *-----------------------------------------------------------------
        inscriptionEvent.
           MOVE 0 TO Fin
           MOVE 1 TO valideInscription
           MOVE 0 TO nbParticipants

           OPEN I-O fparticipant
      * On verifie dans un premier temps qu'il n'est pas deja inscrit a
      * un evenement ou fait une demande
           MOVE loginSaved TO fpart_login
      *    PERFORM WITH TEST AFTER UNTIL Fin = 1
               START fparticipant, KEY IS = fpart_login
                   INVALID KEY
                       MOVE 1 TO Fin
                   NOT INVALID KEY
                       ADD 1 TO nbParticipants
                       READ fparticipant NEXT
                           AT END
                               MOVE 1 TO Fin
                           NOT AT END
                           IF fpart_etat ="acceptee" THEN
                               PERFORM comparer_date
                                   IF dateComparee = 0 THEN
      * On a deja une inscription a la meme date donc on ne peut pas 
      * s'inscrire.   
                                       MOVE 0 TO valideInscription
                                       MOVE 1 TO Fin
                                   END-IF
                            END-IF
                            IF fevent_nom = fpart_nomEvent THEN
                                IF fpart_etat = "acceptee" THEN
                                    DISPLAY " ________________________"
                                    DISPLAY "|                        |"
                                    DISPLAY "| Vous etes deja inscrit |"
                                    DISPLAY "|     a cet evenement    |"
                                    DISPLAY "|________________________|"
                                END-IF
                                
                                IF fpart_etat = "attente" THEN
                                    DISPLAY " ________________________"
                                    DISPLAY "|                        |"
                                    DISPLAY "|   Vous avez deja une   |"
                                    DISPLAY "|   demande pour cet cet |"
                                    DISPLAY "|        evenement       |"
                                    DISPLAY "|________________________|"
                                END-IF
                                IF fpart_etat = "refusee" THEN
                                    DISPLAY " ________________________"
                                    DISPLAY "|                        |"
                                    DISPLAY "|   Votre demande a ete  |"
                                    DISPLAY "|    refusee pour cet    |"
                                    DISPLAY "|        evenement       |"
                                    DISPLAY "|________________________|"
                                END-IF
                                MOVE 0 TO valideInscription
                            END-IF
                       END-READ
               END-START
      *     END-PERFORM


           IF valideInscription = 1 THEN
      * On verifie dans un deuxieme temps qu'il reste de la place dans l'evenment
               IF fevent_seuil - nbParticipants <= 0 THEN
                   DISPLAY "Evenement complet"
                   DISPLAY "________________________"
                   DISPLAY "|                        |"
                   DISPLAY "|   Cet evenement est    |"
                   DISPLAY "|         complet        |"
                   DISPLAY "|________________________|"
               END-IF
           END-IF

           IF valideInscription = 1 THEN
      * S'il reste de la place on saisit les valeurs pour inscrire l'utilisateur
      * a l'evenement
               MOVE "attente" TO fpart_etat
               MOVE loginSaved TO fpart_login
               MOVE nomSaved TO fpart_nomEvent
               WRITE tamp_fpart
               END-WRITE
               IF cr_fpart =00 THEN
               DISPLAY " _______________________________ "
               DISPLAY "|                               |"
               DISPLAY "|          INFORMATION          |"
               DISPLAY "|_______________________________|"
               DISPLAY "|                               |"
               DISPLAY "|      Inscription reussie !    |"
               DISPLAY "|      l'organisateur va        |"
               DISPLAY "|      examiner votre demande   |"
               DISPLAY "|_______________________________|"
               ELSE
               DISPLAY " _______________________________ "
               DISPLAY "|                               |"
               DISPLAY "|          INFORMATION          |"
               DISPLAY "|_______________________________|"
               DISPLAY "|                               |"
               DISPLAY "|      Echec de la demande      |"
               DISPLAY "|         d'inscription         |"
               DISPLAY "|_______________________________|"
               END-IF
           END-IF
           CLOSE fparticipant.

      *-----------------------------------------------------------------
      *          Procedure permettant de verifier l'etat des inscriptions
      *-----------------------------------------------------------------
       etatInscription.
           DISPLAY " ____________________________________ "
           DISPLAY "|                                    |"
           DISPLAY "|        ETAT DES INSCRIPTIONS       |"
           DISPLAY "|------------------------------------|"
           OPEN INPUT fparticipant
           MOVE loginSaved TO fpart_login
           START fparticipant, KEY IS = fpart_login
           INVALID KEY
               DISPLAY "|                                    |"
               DISPLAY "| Aucune inscription a un evenement  |"
               DISPLAY "|____________________________________|"
           NOT INVALID KEY
               PERFORM WITH TEST AFTER UNTIL Fin = 1
                   READ fparticipant NEXT
                   AT END
                       MOVE 1 To Fin
                   NOT AT END
                       IF fpart_etat = "attente" THEN
                        DISPLAY "|                                    |"
                        DISPLAY "|             EN ATTENTE             |"
                        DISPLAY "|____________________________________|"
                        DISPLAY "|                                    |"
                        DISPLAY "| Nom evenement : "fpart_nomEvent
                        DISPLAY "|____________________________________|"
                       ELSE IF fpart_etat = "acceptee" THEN
                        DISPLAY "|                                    |"
                        DISPLAY "|              ACCEPTES              |"
                        DISPLAY "|____________________________________|"
                        DISPLAY "|                                    |"
                        DISPLAY "| Nom evenement : "fpart_nomEvent
                        DISPLAY "|____________________________________|"
                       ELSE IF fpart_etat = "refusee" THEN
                        DISPLAY "|                                    |"
                        DISPLAY "|              REFUSES               |"
                        DISPLAY "|____________________________________|"
                        DISPLAY "|                                    |"
                        DISPLAY "| Nom evenement : "fpart_nomEvent
                        DISPLAY "|____________________________________|"
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
           DISPLAY " ------------------------------------"
           DISPLAY "|                                    |"
           DISPLAY "|       RECHERCHER UTILISATEUR       |"
           DISPLAY "|           AVEC SON NOM             |"
           DISPLAY "|------------------------------------|"
           DISPLAY " "
           DISPLAY "Veuillez saisir le nom de la personne souhaitee :"
           ACCEPT nom
           OPEN INPUT futilisateur
           MOVE nom TO futil_nom
           START futilisateur, KEY IS = futil_nom
              INVALID KEY
                   DISPLAY "|                                    |"
                   DISPLAY "|____________________________________|"
                   DISPLAY " "
                   DISPLAY " "
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|     Utilisateur inexistant    |"
                   DISPLAY "|_______________________________|"
              NOT INVALID KEY
                  PERFORM WITH TEST AFTER UNTIL Fin = 1
                      READ    futilisateur    NEXT
                      AT    END
                           MOVE 1 TO Fin
                      NOT AT END
                        DISPLAY "|------------------------------------|"
                        DISPLAY "|        Nom trouve, a present       |"
                        DISPLAY "|------------------------------------|"
                        DISPLAY " "
                        DISPLAY "Veuillez saisir son prenom :"
                        ACCEPT prenom
                        IF prenom = futil_prenom THEN
                        DISPLAY "|------------------------------------|"
                        DISPLAY "|      INFORMATIONS UTILISATEUR      |"
                        DISPLAY "|------------------------------------|"
                        DISPLAY "| Nom :                              |"
                        DISPLAY "|   "futil_nom
                        DISPLAY "| Prenom :                           |"
                        DISPLAY "|   "futil_prenom
                        DISPLAY "| Mail :                             |"
                        DISPLAY "|   "futil_mail
                        DISPLAY "| Telephone :                        |"
                        DISPLAY "|   "futil_tel
                        DISPLAY "| Login :                            |"
                        DISPLAY "|   "futil_login
                        DISPLAY "| Type d'utilisateur :               |"
                        IF futil_type = 1 THEN
                            DISPLAY "|   Administrateur"
                        ELSE
                            DISPLAY "|   Membre"
                        END-IF
                        DISPLAY "|____________________________________|"
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
           DISPLAY " ------------------------------------"
           DISPLAY "|                                    |"
           DISPLAY "|       RECHERCHER UTILISATEUR       |"
           DISPLAY "|           AVEC SON LOGIN           |"
           DISPLAY "|------------------------------------|"
           DISPLAY "Veuillez saisir le login de la personne souhaitee :"
           ACCEPT login
           MOVE login to futil_login
           OPEN INPUT futilisateur
           READ futilisateur
               INVALID KEY
                   DISPLAY "|                                    |"
                   DISPLAY "|____________________________________|"
                   DISPLAY " "
                   DISPLAY " "
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|     Utilisateur inexistant    |"
                   DISPLAY "|_______________________________|"
               NOT INVALID KEY
                   IF login = futil_login THEN
                        DISPLAY "|------------------------------------|"
                        DISPLAY "|      INFORMATIONS UTILISATEUR      |"
                        DISPLAY "|------------------------------------|"
                        DISPLAY "| Nom :                              |"
                        DISPLAY "|   "futil_nom
                        DISPLAY "| Prenom :                           |"
                        DISPLAY "|   "futil_prenom
                        DISPLAY "| Mail :                             |"
                        DISPLAY "|   "futil_mail
                        DISPLAY "| Telephone :                        |"
                        DISPLAY "|   "futil_tel
                        DISPLAY "| Login :                            |"
                        DISPLAY "|   "futil_login
                        DISPLAY "| Type d'utilisateur :               |"
                        IF futil_type = 1 THEN
                            DISPLAY "|   Administrateur"
                        ELSE
                            DISPLAY "|   Membre"
                        END-IF
                        DISPLAY "|____________________________________|"
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


           DISPLAY " ________________________________"
           DISPLAY "|                               |"
           DISPLAY "|       SUPPRIMER MON COMPTE    |"
           DISPLAY "|          UTILISATEUR          |"
           DISPLAY "|_______________________________|"
           OPEN INPUT fevenement
           MOVE 1 TO fdf
           MOVE 0 TO suppression_ok

      ** on verifie s'il organise un evenement
           MOVE futil_login TO fevent_loginOrga
           START fevenement, KEY IS = fevent_loginOrga
               NOT INVALID KEY
                   MOVE 1 TO suppression_ok
                   DISPLAY "|                               |"
                   DISPLAY "|  Impossible de supprimer cet  |"
                   DISPLAY "|  utilisateur. Il organise un  |"
                   DISPLAY "|    evenement                  |"
                   DISPLAY "|_______________________________|"
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
                   DISPLAY " _______________________________ "
                   DISPLAY "|  Impossible de supprimer cet  |"
                   DISPLAY "| utilisateur. Il est inscrit a |"
                   DISPLAY "|    un evenement               |"
                   DISPLAY "|_______________________________|"
               END-START
               CLOSE fparticipant
           END-IF

           IF suppression_ok IS equal 0 THEN
              OPEN I-O futilisateur
      ** faire la suppression de l'utilisateur
      ** et revenir a la page de connexion
               READ futilisateur
               INVALID key
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|     Impossible de supprimer   |"
                   DISPLAY "|          ce compte            |"
                   DISPLAY "|_______________________________|"
               NOT INVALID KEY
                   DELETE futilisateur RECORD
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|     Utilisateur supprime      |"
                   DISPLAY "|_______________________________|"
                   DISPLAY " "
                   DISPLAY " _______________________________ "
                   DISPLAY "|   VOUS AVEZ ETE DECONNECTE    |"
                   DISPLAY "|_______________________________|"
                   PERFORM accueil
               END-READ
              CLOSE futilisateur
           END-IF.

      *-----------------------------------------------------------------
      *          Procedure permettant d'afficher tous les organisateurs
      *           d'evenement
      *-----------------------------------------------------------------
       affichage_organisateur.

           DISPLAY " ____________________________________"
           DISPLAY "|                                    |"
           DISPLAY "|      AFFICHAGE ORGANISATEURS       |"
           DISPLAY "|------------------------------------|"
               OPEN INPUT futilisateur
               OPEN INPUT fevenement

               MOVE 1 TO fdf

               PERFORM WITH TEST AFTER UNTIL fdf=0
                   READ futilisateur
                   AT END
                       MOVE 0 TO fdf
                   not AT END
                       MOVE futil_login TO fevent_loginOrga
                       START fevenement , KEY IS = fevent_loginOrga
                        NOT INVALID KEY
                        DISPLAY "|                                    |"
                        DISPLAY "| Nom :                              |"
                        DISPLAY "|   "futil_nom
                        DISPLAY "| Prenom :                           |"
                        DISPLAY "|   "futil_prenom
                        DISPLAY "| Login :                            |"
                        DISPLAY "|   "futil_login
                        DISPLAY "|--------                            |"
                       END-START
                   END-READ
               END-PERFORM
               DISPLAY "|____________________________________|"
               CLOSE futilisateur
               CLOSE fevenement.

      *-----------------------------------------------------------------
      *          Procedure permettant d'accepter ou refuser des demandes
      *          de participation a un evenement
      *-----------------------------------------------------------------
       gestion_demandes.
           DISPLAY " ____________________________________"
           DISPLAY "|                                    |"
           DISPLAY "|       GESTION DES DEMANDES         |"
           DISPLAY "|------------------------------------|"

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
                       DISPLAY "|                                    |"
                       DISPLAY "|        Pas de participants         |"
                       DISPLAY "|____________________________________|"
                   NOT INVALID KEY
                       PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
                           READ fparticipant NEXT
                           AT END
                               MOVE 1 TO fin_boucle
                           NOT AT END
                               IF fpart_etat = 'attente' THEN
                                   DISPLAY "| demande de : " fpart_login
                                   DISPLAY "|--------------------------"
                               END-IF
                       END-PERFORM
                   END-START
               CLOSE fparticipant
               DISPLAY "|____________________________|"
               DISPLAY " "
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
                       DISPLAY " _______________________________ "
                       DISPLAY "|                               |"
                       DISPLAY "|   /!\       ERREUR       /!\  |"
                       DISPLAY "|_______________________________|"
                       DISPLAY "|                               |"
                       DISPLAY "|        Login Incorrecte       |"
                       DISPLAY "|_______________________________|"
                   NOT INVALID KEY
                       IF fpart_etat <> "attente" THEN
                           DISPLAY "Cette demande n'est"
                           DISPLAY "pas en attente !"
                       ELSE
                        DISPLAY " ____________________________________ "
                        DISPLAY "|                                    |"
                        DISPLAY "|     Que souhaitez vous faire ?     |"
                        DISPLAY "|                                    |"
                        DISPLAY "|------------------------------------|"
                        DISPLAY "|                                    |"
                        DISPLAY "|   0 - Refuser la demande           |"
                        DISPLAY "|   1 - Accepter la demande          |"
                        DISPLAY "|____________________________________|"
                        DISPLAY " "
                        DISPLAY "Votre choix :"
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
                        DISPLAY " ____________________________________ "
                        DISPLAY "|                                    |"
                        DISPLAY "|        Voulez-vous quitter  ?      |"
                        DISPLAY "|                                    |"
                        DISPLAY "|------------------------------------|"
                        DISPLAY "|                                    |"
                        DISPLAY "|   0 - Non                          |"
                        DISPLAY "|   1 - Oui                          |"
                        DISPLAY "|____________________________________|"
                        DISPLAY " "
                        DISPLAY "Votre choix :"
                        ACCEPT fin_boucle
           END-PERFORM
           END-IF
           CLOSE fparticipant.

      *-----------------------------------------------------------------
      *          Procedure permettant de supprimer des evenements
      *          automatiquement ou non en fonction de autoSupprEvent
      *-----------------------------------------------------------------
       supprimerEvent.
           MOVE 0 TO verif_event
           MOVE 0 TO retour
           IF autoSupprEvent <> 1 THEN
               DISPLAY " ____________________________________"
               DISPLAY "|                                    |"
               DISPLAY "|       SUPPRESSION EVENEMENT        |"
               DISPLAY "|------------------------------------|"
               PERFORM afficheEvent
      * Selection de l'evenement a supprimer
           PERFORM WITH TEST AFTER UNTIL verif_event = 1 OR retour = 1
                PERFORM verif_permission
           END-PERFORM
           END-IF

           IF retour = 1 THEN
               DISPLAY " ____________________________________ "
               DISPLAY "|                                    |"
               DISPLAY "|           Retour au menu           |"
               DISPLAY "|                                    |"
               DISPLAY "|____________________________________|"
           ELSE
      *    l'evenement existe, on peut le supprimer mais on supprime ses
      *    participants avant :

               DISPLAY "Suppression des participations liees a"
      -     "l'evenement"
               DISPLAY "|                                    |"
               DISPLAY "|    Suppresion des participants     |"
               DISPLAY "|         liees a l'evenement        |"
               DISPLAY "|                                    |"
               DISPLAY "|____________________________________|"
               OPEN I-O fparticipant
               MOVE 0 TO fin_boucle
               MOVE fevent_nom TO fpart_nomEvent
               START fparticipant, KEY IS = fpart_nomEvent
               INVALID KEY
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|   Erreur dans la suppression  |"
                           DISPLAY "|   des participants peut etre  |"
                           DISPLAY "|   pas de participants ?       |"
                           DISPLAY "|_______________________________|"
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
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|     Erreur dans la lecture    |"
                           DISPLAY "|          de fevenement        |"
                           DISPLAY "|_______________________________|"
                       NOT INVALID KEY
                          DELETE fevenement RECORD
                   END-READ
               CLOSE fevenement
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|          INFORMATION          |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|     Suppression reussie !     |"
                   DISPLAY "|_______________________________|"
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

           DISPLAY " ____________________________________"
           DISPLAY "|                                    |"
           DISPLAY "|      AFFICHAGE DE VOS EVENEMENTS   |"
           DISPLAY "|------------------------------------|"

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
                       DISPLAY "|------------------------------------|"
                       DISPLAY "| Nom :                              |"
                       DISPLAY "|   "fevent_nom
                       DISPLAY "| Places :                           |"
                       DISPLAY "|   "fevent_seuil
                       DISPLAY "| Date :                             |"
                       DISPLAY "|   "fevent_dateJour"/"
      -                fevent_dateMois"/"fevent_dateAnnee
                       DISPLAY "| Type :                             |"
                       DISPLAY "|   "fevent_type
                       DISPLAY "| Description :                      |"
                       DISPLAY "|   "fevent_description
                       DISPLAY "| Adresse :                          |"
                       DISPLAY "|   "fevent_adresse
                       DISPLAY "|------------------------------------|"
               END-PERFORM
           END-START
           CLOSE fevenement
           .
      *-----------------------------------------------------------------
      *    Affiche toutes les statistiques globales de l'administrateur
      *-----------------------------------------------------------------

       afficherStats.
      **Affiche le nombre d'evenements presents sur toute la plateforme
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
           DISPLAY " ____________________________________"
           DISPLAY "|                                    |"
           DISPLAY "|       STATISTIQUES GLOBALES        |"
           DISPLAY "|------------------------------------|"
           DISPLAY "| Nombre d'evenements :              |"
           DISPLAY "|   "nbEvents "                              |"
           DISPLAY "| Archivables :                      |"
           DISPLAY "|   "nbEventArchivables"
      -    "  |"
           DISPLAY "| Nombre d'utilisateurs :            |"
           DISPLAY "|   "nbUtils"                             |"
           DISPLAY "| Nombre d'evenements archives :     |"
           DISPLAY "|   "nb
      -    EventArchives"                              |"
           DISPLAY "|____________________________________|"
           CLOSE fhistorique.

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

      *-----------------------------------------------------------------
      *    Permet de modifier le type, la date, la description, l'adresse
      *    le seuil ou l'heure de l'evenement
      *-----------------------------------------------------------------
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
           DISPLAY " ____________________________________"
           DISPLAY "|                                    |"
           DISPLAY "|         MODIFIER EVENEMENT         |"
           DISPLAY "|------------------------------------|"
           DISPLAY "|                                    |"
           DISPLAY "|    Que voulez-vous modifier ?      |"
           DISPLAY "|                                    |"
           DISPLAY "| 1 - Type                           |"
           DISPLAY "| 2 - description                    |"
           DISPLAY "| 3 - adresse                        |"
           DISPLAY "| 4 - seuil                          |"
           DISPLAY "|                                    |"
           DISPLAY "|------------------------------------|"
           DISPLAY "|                                    |"
           DISPLAY "| 0 - Revenir au menu precedent      |"
           DISPLAY "|____________________________________|"
           DISPLAY " "
           DISPLAY "Votre choix :"

           ACCEPT choixModifEvent
               EVALUATE choixModifEvent
               WHEN 1
                   DISPLAY "Ancien type : "fevent_type
                   DISPLAY "Entrez le nouveau type :"
                   ACCEPT fevent_type
                   REWRITE tamp_fevent
                   IF cr_futil = 00
                       THEN
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|          INFORMATION          |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|     Modification reussie !    |"
                           DISPLAY "|_______________________________|"
                       ELSE
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|   Echec de la modification    |"
                           DISPLAY "|_______________________________|"
                           DISPLAY cr_fevent
                   END-IF
               WHEN 2
                   DISPLAY "Ancienne description : "fevent_description
                   DISPLAY "Entrez la nouvelle description : "
                   ACCEPT fevent_description
                   REWRITE tamp_fevent
               IF cr_futil = 00 THEN
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|          INFORMATION          |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|     Modification reussie !    |"
                           DISPLAY "|_______________________________|"
               ELSE
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|   Echec de la modification    |"
                           DISPLAY "|_______________________________|"
                   DISPLAY cr_fevent
               END-IF
               WHEN 3
               DISPLAY "Ancienne adresse :"fevent_adresse
               DISPLAY "Entrez la nouvelle adresse :"
               ACCEPT fevent_adresse
               REWRITE tamp_fevent
               IF cr_futil = 00
                   THEN
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|          INFORMATION          |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|     Modification reussie !    |"
                           DISPLAY "|_______________________________|"
                   ELSE
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|   Echec de la modification    |"
                           DISPLAY "|_______________________________|"
                       DISPLAY cr_fevent
               END-IF
               WHEN 4
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

                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|    Le seuil est inferieur     |"
                           DISPLAY "|           au  nombre          |"
                           DISPLAY "|     de participants deja      |"
                           DISPLAY "|           inscrits            |"
                           DISPLAY "|_______________________________|"
                           ELSE
                               MOVE 1 TO fin_boucle
                           END-IF
                       END-IF
                   END-PERFORM
                   REWRITE tamp_fevent
                   IF cr_fevent = 00
                       THEN
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|          INFORMATION          |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|     Modification reussie !    |"
                           DISPLAY "|_______________________________|"
                       ELSE
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|   Echec de la modification    |"
                           DISPLAY "|_______________________________|"
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
                       DISPLAY " _______________________________ "
                       DISPLAY "|                               |"
                       DISPLAY "|   /!\       ERREUR       /!\  |"
                       DISPLAY "|_______________________________|"
                       DISPLAY "|                               |"
                       DISPLAY "|        Saisie invalide        |"
                       DISPLAY "|_______________________________|"
                   NOT INVALID KEY
                       IF fevent_loginOrga = loginSaved THEN
                           MOVE 1 TO verif_event
                       ELSE
                           OPEN INPUT futilisateur
                           MOVE loginSaved TO futil_login
                           READ futilisateur
                               INVALID KEY
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|      Echec de la lecture      |"
                           DISPLAY "|_______________________________|"
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
           WRITE tamp_fhisto
               INVALID KEY
                   DISPLAY " _______________________________ "
                   DISPLAY "|                               |"
                   DISPLAY "|   /!\       ERREUR       /!\  |"
                   DISPLAY "|_______________________________|"
                   DISPLAY "|                               |"
                   DISPLAY "|      Echec de la lecture      |"
                   DISPLAY "|_______________________________|"
               NOT INVALID KEY
                   MOVE 1 TO autoSupprEvent
                   PERFORM supprimerEvent
                   MOVE 0 TO autoSupprEvent
           END-WRITE
           CLOSE fhistorique
           .

      *-----------------------------------------------------------------
      *    Procedure permettant a l'utilisateur d'archiver
      *    des evenements passes
      *-----------------------------------------------------------------
       archivageEvent.
           DISPLAY " ____________________________________"
           DISPLAY "|                                    |"
           DISPLAY "|             ARCHIVAGE              |"

           OPEN I-O fevenement
           MOVE 0 TO fin_boucle
           MOVE 0 TO retour

           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               READ fevenement
                   AT END
                       MOVE 1 TO fin_boucle
                   NOT AT END
                       PERFORM comparer_date
                       IF dateComparee = 1 THEN
                        DISPLAY "|------------------------------------|"
                        DISPLAY "| Nom :                              |"
                        DISPLAY "|   "fevent_nom
                        DISPLAY "| Date :                             |"
                        DISPLAY "|   "fevent_dateJour"/"
      -                     fevent_dateMois"/"fevent_dateAnnee
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY "|                                    |"
           DISPLAY "|____________________________________|"

           MOVE 0 TO fin_boucle

      *     CLOSE fevenement
      *     OPEN I-O fevenement
           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1 OR retour = 1
               DISPLAY "Saissisez le nom de l'evenement a archiver :"
               ACCEPT fevent_nom
               READ fevenement
                   INVALID KEY
                       MOVE 1 TO fin_boucle
                   NOT INVALID KEY
                       PERFORM comparer_date
                       IF dateComparee = 1 THEN
                           PERFORM archiver_event
                       ELSE
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "| L'evenement n'est pas termine |"
                           DISPLAY "|_______________________________|"
                       END-IF
               END-READ
               DISPLAY " ____________________________________ "
               DISPLAY "|                                    |"
               DISPLAY "|        Voulez-vous revenir         |"
               DISPLAY "|        au menu precedent  ?        |"
               DISPLAY "|                                    |"
               DISPLAY "|------------------------------------|"
               DISPLAY "|                                    |"
               DISPLAY "|   0 - Non                          |"
               DISPLAY "|   1 - Oui                          |"
               DISPLAY "|____________________________________|"
               DISPLAY " "
               DISPLAY "Votre choix :"
               ACCEPT retour
           END-PERFORM
           CLOSE fevenement
           .
      *-----------------------------------------------------------------
      *    Procedure permettant a l'utilisateur d'archiver tous
      *    les evenements passes en faisant appel a la fonction archiver_event
      *-----------------------------------------------------------------
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
      *    Fonction annexe :
      *    Fonction comptant les participants a un evenement
      *    fpart_nomEvent doit avoir sa valeur avant l'appel
      ******************************************************************
       compte_nb_part.
           MOVE 0 TO nbParticipants
           MOVE 0 TO fin_boucle
           OPEN INPUT fparticipant
           START fparticipant, KEY IS = fpart_nomEvent
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
      *    Procedure verifiant le format horaire
      *    La variable verifiee est heureEvent
      ******************************************************************
       verifHeure.
           MOVE 1 TO estValideHeure
           IF heureEvent(1:1) < 0 OR heureEvent(1:1) > 2 THEN
               MOVE 0 TO estValideHeure
           END-IF

           IF heureEvent(1:1) = 0 OR heureEvent(1:1) = 1 THEN
               IF heureEvent(1:1) IS NOT NUMERIC THEN
                   MOVE 0 TO estValideHeure
               END-IF
           ELSE
               IF heureEvent(1:1) = 2 THEN
                   IF heureEvent(2:1) > 3 THEN
                       MOVE 0 TO estValideHeure
                   END-IF
               END-IF
           END-IF

           IF heureEvent(3:1) <> 'h' THEN
                IF heureEvent(3:1) <> 'H' THEN
                    MOVE 0 TO estValideHeure
                END-IF
           END-IF
           IF heureEvent(4:1) > 5 THEN
               MOVE 0 TO estValideHeure
           END-IF
           IF heureEvent(5:1) IS NOT NUMERIC THEN
            MOVE 0 TO estValideHeure
           END-IF
           .

      *-----------------------------------------------------------------
      *    Procedure calculant le nombre de participations
      *    d'etudiants d'une formation F a  un evenementde type T
      *-----------------------------------------------------------------
       statFormaMois.
           DISPLAY " ____________________________________"
           DISPLAY "|                                    |"
           DISPLAY "|        STATISTIQUES SELON :        |"
           DISPLAY "|       LE MOIS, LA FORMATION        |"
           DISPLAY "|       ET LE TYPE D'EVENEMENT       |"
           DISPLAY "|____________________________________|"
           DISPLAY "|                                    |"
           DISPLAY "|      AFFICHAGE DES FORMATIONS      |"
           DISPLAY "|------------------------------------|"

           OPEN INPUT futilisateur
           MOVE 0 TO fin_boucle

           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               READ futilisateur
               AT END MOVE 1 TO fin_boucle
               NOT AT END
                   DISPLAY "| - "futil_formation
                   DISPLAY "|     -------------------"
               END-READ
           END-PERFORM


           DISPLAY "|____________________________________|"
           DISPLAY "|                                    |"
           DISPLAY "|  AFFICHAGE DES TYPES D'EVENEMENTS  |"
           DISPLAY "|------------------------------------|"

           OPEN INPUT fevenement
           MOVE 0 TO fin_boucle

           PERFORM WITH TEST AFTER UNTIL fin_boucle = 1
               READ fevenement
               AT END MOVE 1 TO fin_boucle
               NOT AT END

                   DISPLAY "| - " fevent_type
                   DISPLAY "|     -------------------"
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
               DISPLAY " _______________________________ "
               DISPLAY "|                               |"
               DISPLAY "|   /!\       ERREUR       /!\  |"
               DISPLAY "|_______________________________|"
               DISPLAY "|                               |"
               DISPLAY "|    Erreur dans l'evenement    |"
               DISPLAY "|_______________________________|"
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
                           DISPLAY " _______________________________ "
                           DISPLAY "|                               |"
                           DISPLAY "|   /!\       ERREUR       /!\  |"
                           DISPLAY "|_______________________________|"
                           DISPLAY "|                               |"
                           DISPLAY "|    Erreur dans fparticipant   |"
                           DISPLAY "|_______________________________|"
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
      *-----------------------------------------------------------------
      *    Procedure permettant d'afficher les evenements
      *-----------------------------------------------------------------
       afficheFormation.
           DISPLAY " ______________________________ "
           DISPLAY "|                              |"
           DISPLAY "|     AFFICHAGE FORMATION      |"
           DISPLAY "|------------------------------|"
           OPEN INPUT futilisateur
               MOVE 0 TO Fin
      *         PERFORM WITH TEST AFTER UNTIL Fin = 1
               PERFORM UNTIL Fin = 1
                   READ futilisateur NEXT
      *             MOVE 1 TO Fin
                   AT END
                       MOVE 1 TO Fin
                   NOT AT END
                           DISPLAY "|Formation :                   |"
                            DISPLAY "|  "futil_formation
                      DISPLAY "|------------------------------|"
                   END-READ
               END-PERFORM
               DISPLAY "|______________________________|"
               CLOSE futilisateur
           .
      ** add other procedures here
       END PROGRAM Evenements.
