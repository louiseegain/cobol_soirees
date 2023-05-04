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
       77 choix PIC 9(1).
       77 nom PIC A(30).
       77 vretour PIC 9(1).
       77 estValideEvenementResultat PIC 9(1).
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
              10  WS-CURRENT-YEAR         PIC 9(04).
              10  WS-CURRENT-MONTH        PIC 9(02).
              10  WS-CURRENT-DAY          PIC 9(02).
          05  WS-CURRENT-TIME.
              10  WS-CURRENT-HOURS        PIC 9(02).
              10  WS-CURRENT-MINUTE       PIC 9(02).
              10  WS-CURRENT-SECOND       PIC 9(02).
              10  WS-CURRENT-MILLISECONDS PIC 9(02).
       77 cpt PIC 9(3).
       77 nom_saved PIC A(30).
       77 login PIC X(30).
      * 77 choix PIC S9(1).

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
      * 77 login PIC X(30).
       77 mdp PIC X(30).
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
           CLOSE fhistorique.

      *-----------------------------------------------------------------
      *                  CREATION JDD
      *-----------------------------------------------------------------
      * Utilisateur 1 : administrateur
           *> MOVE "m.loret" TO futil_login
           *> MOVE "mathias" TO futil_mdp
           *> MOVE "LORET" TO futil_nom
           *> MOVE "Mathias" TO futil_prenom
           *> MOVE "mathias.loret@gmail.com" TO futil_mail
           *> MOVE "admin" TO futil_type
           *> MOVE "MIAGE" TO futil_formation
           *> MOVE "14/06/2002" TO futil_naissance

           *> OPEN I-O futilisateur
           *> WRITE tamp_futi
           *> END-WRITE
           *> IF cr_futil = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_futil
           *> END-IF
           *> CLOSE futilisateur

      *> * Utilisateur 2 : administrateur
           *> MOVE "l.egain" TO futil_login
           *> MOVE "louise" TO futil_mdp
           *> MOVE "EGAIN" TO futil_nom
           *> MOVE "Louise" TO futil_prenom
           *> MOVE "louise.egain@gmail.com" TO futil_mail
           *> MOVE "admin" TO futil_type
           *> MOVE "IFSI" TO futil_formation
           *> MOVE "26/09/2001" TO futil_naissance

           *> OPEN I-O futilisateur
           *> WRITE tamp_futi
           *> END-WRITE
           *> IF cr_futil = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_futil
           *> END-IF
           *> CLOSE futilisateur

      *> * Utilisateur 3 : administrateur
           *> MOVE "t.merlet" TO futil_login
           *> MOVE "thomas" TO futil_mdp
           *> MOVE "MERLET" TO futil_nom
           *> MOVE "Thomas" TO futil_prenom
           *> MOVE "thomas.merlet@gmail.com" TO futil_mail
           *> MOVE "admin" TO futil_type
           *> MOVE "Commerce" TO futil_formation
           *> MOVE "25/02/1999" TO futil_naissance

           *> OPEN I-O futilisateur
           *> WRITE tamp_futi
           *> END-WRITE
           *> IF cr_futil = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_futil
           *> END-IF
           *> CLOSE futilisateur


      *> * Utilisateur 4 : administrateur
           *> MOVE "c.leau" TO futil_login
           *> MOVE "camille" TO futil_mdp
           *> MOVE "LEAU" TO futil_nom
           *> MOVE "Camille" TO futil_prenom
           *> MOVE "camille.leau@gmail.com" TO futil_mail
           *> MOVE "admin" TO futil_type
           *> MOVE "art" TO futil_formation
           *> MOVE "02/10/2000" TO futil_naissance

           *> OPEN I-O futilisateur
           *> WRITE tamp_futi
           *> END-WRITE
           *> IF cr_futil = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_futil
           *> END-IF
           *> CLOSE futilisateur

      *> * Utilisateur 5 : membre
           *> MOVE "s.ledourner" TO futil_login
           *> MOVE "swann" TO futil_mdp
           *> MOVE "LE DOURNER" TO futil_nom
           *> MOVE "Swann" TO futil_prenom
           *> MOVE "swann.ledourner@gmail.com" TO futil_mail
           *> MOVE "membre" TO futil_type
           *> MOVE "MIAGE" TO futil_formation
           *> MOVE "21/12/1996" TO futil_naissance

           *> OPEN I-O futilisateur
           *> WRITE tamp_futi
           *> END-WRITE
           *> IF cr_futil = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_futil
           *> END-IF
           *> CLOSE futilisateur

      *> * Utilisateur 6 : membre
           *> MOVE "k.cosquer" TO futil_login
           *> MOVE "kevin" TO futil_mdp
           *> MOVE "COSQUER" TO futil_nom
           *> MOVE "Kevin" TO futil_prenom
           *> MOVE "kevin.cosquer@gmail.com" TO futil_mail
           *> MOVE "membre" TO futil_type
           *> MOVE "IFSI" TO futil_formation
           *> MOVE "23/04/2001" TO futil_naissance

           *> OPEN I-O futilisateur
           *> WRITE tamp_futi
           *> END-WRITE
           *> IF cr_futil = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_futil
           *> END-IF
           *> CLOSE futilisateur

      *> * Utilisateur 7 : membre
           *> MOVE "g.koc" TO futil_login
           *> MOVE "gamze" TO futil_mdp
           *> MOVE "KOC" TO futil_nom
           *> MOVE "Gamze" TO futil_prenom
           *> MOVE "gamze.koc@gmail.com" TO futil_mail
           *> MOVE "membre" TO futil_type
           *> MOVE "Commerce" TO futil_formation
           *> MOVE "28/03/2002" TO futil_naissance

           *> OPEN I-O futilisateur
           *> WRITE tamp_futi
           *> END-WRITE
           *> IF cr_futil = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_futil
           *> END-IF
           *> CLOSE futilisateur


      *> * Utilisateur 8 : membre
           *> MOVE "t.leberre" TO futil_login
           *> MOVE "thibault" TO futil_mdp
           *> MOVE "LE BERRE" TO futil_nom
           *> MOVE "Thibault" TO futil_prenom
           *> MOVE "thibault.leberre@gmail.com" TO futil_mail
           *> MOVE "membre" TO futil_type
           *> MOVE "art" TO futil_formation
           *> MOVE "08/08/2001" TO futil_naissance

           *> OPEN I-O futilisateur
           *> WRITE tamp_futi
           *> END-WRITE
           *> IF cr_futil = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_futil
           *> END-IF
           *> CLOSE futilisateur

      *> * Evenement 1
        *> MOVE "Gala 2023" TO fevent_nom
        *> MOVE "Soiree" TO fevent_type
        *> MOVE "21/06/2023" TO fevent_date
        *> MOVE "m.loret" TO fevent_loginOrga
        *> MOVE "Soiree de fin d'annee" TO fevent_description
        *> MOVE "2 rue de la liberte, 35000 Rennes" TO fevent_adresse
        *> MOVE "En cours" TO fevent_etat
        *> MOVE 150 TO fevent_seuil
        *> MOVE "20h00" TO fevent_heure

        *> OPEN I-O fevenement
           *> WRITE tamp_fevent
           *> END-WRITE
           *> IF cr_fevent = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_fevent
           *> END-IF
           *> CLOSE fevenement

      *> * Evenement 2
        *> MOVE "Escape game de l'horreur" TO fevent_nom
        *> MOVE "Escape Game" TO fevent_type
        *> MOVE "15/07/2023" TO fevent_date
        *> MOVE "l.egain" TO fevent_loginOrga
        *> MOVE "Pour se terrifier en cette
        *> belle periode" TO fevent_description
        *> MOVE "Les loges, 44140 Montbert" TO fevent_adresse
        *> MOVE "En cours" TO fevent_etat
        *> MOVE 6 TO fevent_seuil
        *> MOVE "21h00" TO fevent_heure

        *> OPEN I-O fevenement
           *> WRITE tamp_fevent
           *> END-WRITE
           *> IF cr_fevent = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_fevent
           *> END-IF
           *> CLOSE fevenement

      *> * Evenement 3
        *> MOVE "Afterwork" TO fevent_nom
        *> MOVE "Soiree" TO fevent_type
        *> MOVE "02/06/2023" TO fevent_date
        *> MOVE "t.merlet" TO fevent_loginOrga
        *> MOVE "Afterwork de fin d'annee" TO fevent_description
        *> MOVE "9 Rue Bon Secours, 44000 Nantes" TO fevent_adresse
        *> MOVE "En cours" TO fevent_etat
        *> MOVE 150 TO fevent_seuil
        *> MOVE "18h15" TO fevent_heure

        *> OPEN I-O fevenement
           *> WRITE tamp_fevent
           *> END-WRITE
           *> IF cr_fevent = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_fevent
           *> END-IF
           *> CLOSE fevenement

      *> * Evenement 4
        *> MOVE "Karting exterieur" TO fevent_nom
        *> MOVE "Karting" TO fevent_type
        *> MOVE "22/09/22023" TO fevent_date
        *> MOVE "c.leau" TO fevent_loginOrga
        *> MOVE "Karting de rentree" TO fevent_description
        *> MOVE "19 Rte des Naudieres, 44880 Sautron" TO fevent_adresse
        *> MOVE "En cours" TO fevent_etat
        *> MOVE 150 TO fevent_seuil
        *> MOVE "20h00" TO fevent_heure

        *> OPEN I-O fevenement
           *> WRITE tamp_fevent
           *> END-WRITE
           *> IF cr_fevent = 35
               *> DISPLAY "Echec d'insertion"
           *> ELSE
               *> DISPLAY "Insertion reussie"
               *> DISPLAY cr_fevent
           *> END-IF
           *> CLOSE fevenement
      *-----------------------------------------------------------------
      *                  PROGRAMME PRINCIPAL
      *-----------------------------------------------------------------
           PERFORM accueil
            STOP RUN.
      *-----------------------------------------------------------------
      *                  FONCTIONS ET PROCEDURES
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      *      Procedure gerant le menu d'un utilisateur pouvant devenir
      *      organisateur d'un ou plusieurs evenement s'il en cre
      *-----------------------------------------------------------------
           menuUtilisateur.
           DISPLAY"--------------------------------------------"
           DISPLAY"|             MENU PRINCIPAL                |"
           DISPLAY"--------------------------------------------"
           PERFORM WITH TEST AFTER UNTIL choix =0
               DISPLAY "Que souhaitez-vous faire ?"
               DISPLAY "1 - Gerer votre profil"
               DISPLAY "2 - Rechercher un evenement"
               DISPLAY "3 - Gestion d'evenement"
               IF futil_type='admin' THEN
                   DISPLAY "4 - Afficher les statistiques"
                   DISPLAY "5 - Modifier le type d'utilisateur"
                   DISPLAY "6 - Supprimer evenement passe"
               END-IF
               DISPLAY "0 - Deconnexion"
               ACCEPT choix


               EVALUATE choix
                   WHEN 1 PERFORM gererProfil
                   WHEN 2 PERFORM rechercherEvent
                   WHEN 3 PERFORM gestionEvenement
                   WHEN 4
                       IF futil_type='admin'
                       THEN PERFORM afficheStatistique
                       ELSE DISPLAY "Non autorise"
                           PERFORM menuUtilisateur
                       END-IF
                   WHEN 5
                      IF futil_type = 'admin'
                           THEN PERFORM modifierUtilisateur
                      ELSE DISPLAY "Non autorise"
                           PERFORM menuUtilisateur
                       END-IF
                   WHEN 6
                       IF futil_type = 'admin'
                           THEN PERFORM supprimerEventPasse
                       ELSE DISPLAY "Non autorise"
                           PERFORM menuUtilisateur
                      END-IF
               END-EVALUATE
           END-PERFORM
           .

           gererProfil.
           DISPLAY"--------------------------------------------"
           DISPLAY"|           GESTION DU PROFIL              |"
           DISPLAY"--------------------------------------------"
           PERFORM WITH TEST AFTER UNTIL choix =0
               DISPLAY "Que souhaitez-vous faire dans votre
                   profil"
               DISPLAY "1 - Modifier votre profil "
               DISPLAY "2 - Supprimer votre profil "
               DISPLAY "3 - Consulter votre profil"
               DISPLAY "0 - Revenir au menu precedent"
               ACCEPT choix

           EVALUATE choix
      *         WHEN 1 PERFORM modifierProfil
      *         WHEN 2 PERFORM supprimerProfil
      *         WHEN 3 PERFORM consulterProfil
                WHEN 0 PERFORM menuUtilisateur
           END-EVALUATE
           END-PERFORM
           .

           rechercherEvent.
           DISPLAY"--------------------------------------------"
           DISPLAY"|           RECHERCHER UN EVENEMENT        |"
           DISPLAY"--------------------------------------------"
           PERFORM WITH TEST AFTER UNTIL choix =0
               DISPLAY "1 - Rechercher avec le nom de l'evenement"
               DISPLAY "2 - Rechercher sur le type d'evenement"
               DISPLAY "3 - Rechercher par organisateur"
               DISPLAY "0 - Revenir au menu precedent"
               ACCEPT choix

           EVALUATE choix
              WHEN 1 PERFORM rechercherNom
      *        WHEN 2 PERFORM rechercherType
      *        WHEN 3 PERFORM rechercherOrga
               WHEN 0 PERFORM menuUtilisateur
           END-EVALUATE

           END-PERFORM
           .

           gestionEvenement.
           DISPLAY"--------------------------------------------"
           DISPLAY"|           GESTION D'EVENEMENT            |"
           DISPLAY"--------------------------------------------"
           PERFORM WITH TEST AFTER UNTIL choix =0
               DISPLAY "1 - Creer un evenement"
               DISPLAY "2 - Modifier un evenement"
               DISPLAY "3 - Supprimer un evenement"
               DISPLAY "4 - Afficher evenement"
               DISPLAY "0 - Revenir au menu precedent"
               ACCEPT choix

            EVALUATE choix
                WHEN 1 PERFORM creerEvent
      *          WHEN 2 PERFORM modifierEvent
      *          WHEN 3 PERFORM supprimerEvent
                WHEN 4 PERFORM afficheEvent
                WHEN 0 PERFORM menuUtilisateur
           END-EVALUATE

           END-PERFORM
           .

           afficheStatistique.
           DISPLAY"--------------------------------------------"
           DISPLAY"|       AFFICHAGE DES STATISTIQUES         |"
           DISPLAY"--------------------------------------------"
           PERFORM WITH TEST AFTER UNTIL choix =0
               DISPLAY "1 - Afficher statistique"
               DISPLAY "0 - Revenir au menu precedent"
               ACCEPT choix

               EVALUATE choix
      *         WHEN 1 PERFORM afficherStat
                WHEN 0 PERFORM menuUtilisateur
               END-EVALUATE
           END-PERFORM
           .

      *-----------------------------------------------------------------
      * Procedure permettant de modifier un utilisateur
      *-----------------------------------------------------------------
           modifierUtilisateur.
           DISPLAY"--------------------------------------------"
           DISPLAY"|          MODIFIER UTILISATEUR            |"
           DISPLAY"--------------------------------------------"
           PERFORM WITH TEST AFTER UNTIL choix =0
               DISPLAY "Que voulez-vous modifier ?"
               DISPLAY "1 - Nom "
               DISPLAY "2 - Prenom "
               DISPLAY "3 - Mail "
               DISPLAY "4 - Telephone "
               DISPLAY "5 - Formation"
               IF futil_type='admin'
                   DISPLAY "6 - Type d'utilisateur : "futil_type
               END-IF
               ACCEPT choix
               EVALUATE choix
               WHEN 1
                   DISPLAY "Entrer votre nouveau nom :"
                   ACCEPT futil_nom
               WHEN 2
                   DISPLAY "Entrer votre nouveau prenom :"
                   ACCEPT futil_prenom
               WHEN 3
                   PERFORM WITH TEST AFTER UNTIL verif_mail_ok EQUAL 1
                       DISPLAY "Entrer votre nouvelle adresse mail:"
                       ACCEPT futil_mail
                       PERFORM verif_mail
                   END-PERFORM
               WHEN 4
                   PERFORM WITH TEST AFTER UNTIL verif_tel_ok EQUAL 1
                       DISPLAY "Entrer votre numero de telephone:"
                       ACCEPT futil_tel
                       PERFORM verif_tel
                   END-PERFORM
               WHEN 5
                   DISPLAY "Entrer votre nouvelle formation :"
                   ACCEPT futil_formation
               WHEN 6
                   IF futil_type='admin' THEN
                       DISPLAY "Veuillez entrer le nouveau type
                       d'utilisateur"
                       ACCEPT futil_type
                   END-IF
               END-PERFORM
           OPEN I-O futilisateur
               REWRITE tamp_futi
                   IF cr_futil = 00
                       THEN
                           DISPLAY "Modification reussie"
                       ELSE
                           DISPLAY "Modification en echec"
                   END-IF
           CLOSE futilisateur

           .

           supprimerEventPasse.
           DISPLAY"--------------------------------------------"
           DISPLAY"|       SUPPRIMER EVENEMENT PASSE          |"
           DISPLAY"--------------------------------------------"
           .
      *-----------------------------------------------------------------
      * Procedure permettant de rechercher un evenement en fonction de
      *                        son nom
      *-----------------------------------------------------------------


           rechercherNom.
           PERFORM afficheEvent
           OPEN INPUT fevenement
               DISPLAY "Quel evenement voulez-vous rechercher ?"
               DISPLAY "(saisir le nom)"
               ACCEPT fevent_nom
               READ fevenement
                   INVALID KEY
                       DISPLAY "Evenement non trouve"
                       DISPLAY "Veuillez reessayer votre recherche."
                   NOT INVALID KEY
                       DISPLAY "Voici les informations de l'evenement :"
                       DISPLAY "Nom : " fevent_nom
                       DISPLAY "Type : "fevent_type
                       DISPLAY "Date : "fevent_date
                       DISPLAY "Heure de debut : " fevent_heure
                       DISPLAY "Description : " fevent_description
                       DISPLAY "Adresse : " fevent_adresse
                       DISPLAY "Seuil : "fevent_seuil
                       DISPLAY "Login organisateur : " fevent_loginOrga
               END-READ
           CLOSE fevenement.



      *-----------------------------------------------------------------
      *          Procedure permettant de Creer un evenement
      *-----------------------------------------------------------------

      ******************************************************************
      *    Fonction parallele :
      *    Fonction qui verifie que le nom de l'evenement n'est pas deja
      *    present
      ******************************************************************
           existeEvent.
           OPEN INPUT fevenement
           READ fevenement NEXT
           INVALID KEY
               MOVE 0 TO estValideEvenementResultat
           NOT INVALID KEY
               MOVE 1 TO estValideEvenementResultat
           END-READ

      *     IF cr_fevent = 00
      *     THEN DISPLAY "Evenement trouve"
      *     ELSE DISPLAY "Evenement non trouve"
      *     END-IF
           CLOSE fevenement
           .
      ******************************************************************
      *    Fonction principale :
      *    Fonction qui permet de creer l'evenement
      ******************************************************************
           creerEvent.
           DISPLAY"-------------------------------------------"
           DISPLAY"|          CREATION EVENEMENT             |"
           DISPLAY"-------------------------------------------"
           PERFORM WITH TEST AFTER UNTIL estValideEvenementResultat = 1
               DISPLAY "Saisir le nom de l'evenement"
               DISPLAY"(maximum 30 caracteres)"
               ACCEPT nomEvent
               PERFORM existeEvent
           END-PERFORM
           DISPLAY "Saisir le type d'evenement"
           ACCEPT typeEvent
           DISPLAY "Saisir la date de l'evenement"
           DISPLAY "Format : JJ/MM/AAAA"
           ACCEPT dateEvent
      *     PERFORM UNTIL loginOrga EQUALS loginSaved
               DISPLAY "Saisir votre identifiant de connexion"
               DISPLAY "(correspondant a votre login de connexion)"
               ACCEPT loginOrga
      *     END-PERFORM
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
           DISPLAY " Format : xxHxx, avec x un chiffre"
           ACCEPT heureEvent

           MOVE nomEvent TO fevent_nom
           MOVE typeEvent TO fevent_type
           MOVE dateEvent TO fevent_date
           MOVE loginOrga TO fevent_loginOrga
           MOVE descriptionEvent TO fevent_description
           MOVE adresseEvent TO fevent_adresse
           MOVE seuilEvent TO fevent_seuil
           MOVE heureEvent TO fevent_heure
           MOVE "En cours" TO fevent_etat

           OPEN I-O fevenement
           WRITE tamp_fevent
           END-WRITE
           IF cr_fevent=35
               DISPLAY "Echec d'insertion, veuillez reesayer"
           ELSE
               DISPLAY "Insertion reussie"
               DISPLAY cr_fevent
           END-IF

           CLOSE fevenement
           .


      *-----------------------------------------------------------------
      *          Procedure permettant d'afficher les evenement
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
                   PERFORM gestionEvenement
                   MOVE 1 TO Fin
               NOT AT END
                   IF fevent_date>=WS-CURRENT-DATE
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
      *     inscriptionEvent.
      *     PERFORM afficheEvent
      *     DISPLAY "A quel evenement voulez vous vous inscrire?"
      *     ACCEPT nomEvent
      *     MOVE nomEvent TO nom_saved
      *     OPEN I-O fparticipant
      *         MOVE "En attente" TO fpart_etat
      *         MOVE 0 TO Fin
      *         MOVE 1 TO cpt
      *         PERFORM WITH TEST AFTER UNTIL Fin = 1
      *             READ fparticipant NEXT
      *                 AT END
      *                     MOVE 1 TO Fin
      *                 NOT AT END
      *                     MOVE cpt+1 TO cpt
      *             END-READ
      *         END-PERFORM
      *         MOVE login_saved TO fpart_login
      *         MOVE nom_saved TO fpart_nomEvent
      *     CLOSE fparticipant
      *    .

      *-----------------------------------------------------------------
      *          Procedure permettant de rechercher un utilisateur par
      *          son nom
      *-----------------------------------------------------------------
           rechercherUtilisateurNom.
           DISPLAY "Veuillez saisir le nom de la personne souhaitee :"
           ACCEPT nom
           OPEN INPUT futilisateur
           PERFORM WITH TEST AFTER UNTIL Fin = 1
               READ futilisateur
               AT END
                   MOVE 1 TO Fin
               NOT AT END
                   IF nom = futil_nom THEN
                       DISPLAY "Nom : " futil_nom
                       DISPLAY "Prenom : " futil_prenom
                       DISPLAY "Mail : " futil_mail
                       DISPLAY "Telephone : " futil_tel
                       DISPLAY "Login : "futil_login
                       DISPLAY "Type d'utilisateur : "futil_type
                   END-IF
           END-PERFORM
           CLOSE futilisateur
           .

      *-----------------------------------------------------------------
      *          Procedure permettant de rechercher un utilisateur par
      *          son login
      *-----------------------------------------------------------------
           rechercherUtilisateurLogin.
           DISPLAY "Veuillez saisir le login de la personne souhaitee :"
           ACCEPT login
           OPEN INPUT futilisateur
           READ futilisateur
               INVALID KEY
                   DISPLAY "Utilisateur non trouve"
               NOT INVALID KEY
                   IF login = futil_login THEN
                       DISPLAY "Nom : "futil_nom
                       DISPLAY "Prenom :" futil_prenom
                       DISPLAY "Mail :  " futil_mail
                       DISPLAY "Telephone :  " futil_tel
                       DISPLAY "Login :  " futil_login
                       DISPLAY "Type utilisateur :  " futil_type
                   END-IF
           CLOSE futilisateur
           .


      *-----------------------------------------------------------------
      *          Procedure de connexion à l'application
      *-----------------------------------------------------------------

       accueil.

           PERFORM WITH TEST AFTER UNTIL choix = 0

               DISPLAY "-----------------------------------------------"
               DISPLAY "|         BIENVENUE SUR L'APPLICATION         |"
               DISPLAY "-----------------------------------------------"
               DISPLAY "|  1 - Me connecter a mon compte              |"
               DISPLAY "|  2 - Creer mon compte                       |"
               DISPLAY "|  0 - Quitter                                |"
               DISPLAY "-----------------------------------------------"
               DISPLAY "Taper votre choix :"
               ACCEPT choix

               EVALUATE choix
               WHEN 1
                   DISPLAY "Connexion a un compte"
                   PERFORM connexion
                   PERFORM menuUtilisateur
               WHEN 2
                   DISPLAY "Creation d'un compte"
                   PERFORM creation_compte
               WHEN 0
                   DISPLAY "Merci et a bientot"
               END-EVALUATE
           END-PERFORM
           .

      *-----------------------------------------------------------------
      *          Procedure permettant de créer son compte utilisateur
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
           ACCEPT futil_naissance

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
                   DISPLAY "compte non cree : un probleme est survenu"
               NOT INVALID KEY
                   DISPLAY "compte cree"
           END-WRITE.
               DISPLAY cr_futil
           CLOSE futilisateur.

      *-----------------------------------------------------------------
      *          Procedure permettant de verifier le format d'une
      *          adresse mail
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


      *-----------------------------------------------------------------
      *          Procedure permettant de verifier le numero de telephone
      *          saisi par l'utilisateur
      *-----------------------------------------------------------------
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
               DISPLAY "Entrer votre login :"
               ACCEPT login
               DISPLAY "Entrer votre mot de passe :"
               ACCEPT mdp

               IF mdp NOT EQUAL SPACE AND login NOT EQUAL SPACE THEN
                   MOVE login TO futil_login
                   MOVE mdp TO futil_mdp
                   OPEN INPUT futilisateur
                   READ futilisateur
                       INVALID KEY
                           DISPLAY "/!\ ATTENTION /!\"
                           DISPLAY "Erreur dans la saisie du login"
                           DISPLAY "N'oubliez pas de créer votre compte
                           avant de vous connecter"
                           PERFORM accueil
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

      ** add other procedures here
       END PROGRAM Evenements.
