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
           02 fevent_nom PIC A(30).
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
           MOVE "m.loret" TO futil_login
           MOVE "mathias" TO futil_mdp
           MOVE "LORET" TO futil_nom
           MOVE "Mathias" TO futil_prenom
           MOVE "mathias.loret@gmail.com" TO futil_mail
           MOVE "admin" TO futil_type
           MOVE "MIAGE" TO futil_formation
           MOVE "14/06/2002" TO futil_naissance

           OPEN I-O futilisateur
           WRITE tamp_futi
           END-WRITE
           IF cr_futil = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_futil
           END-IF
           CLOSE futilisateur

      * Utilisateur 2 : administrateur
           MOVE "l.egain" TO futil_login
           MOVE "louise" TO futil_mdp
           MOVE "EGAIN" TO futil_nom
           MOVE "Louise" TO futil_prenom
           MOVE "louise.egain@gmail.com" TO futil_mail
           MOVE "admin" TO futil_type
           MOVE "IFSI" TO futil_formation
           MOVE "26/09/2001" TO futil_naissance

           OPEN I-O futilisateur
           WRITE tamp_futi
           END-WRITE
           IF cr_futil = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_futil
           END-IF
           CLOSE futilisateur

      * Utilisateur 3 : administrateur
           MOVE "t.merlet" TO futil_login
           MOVE "thomas" TO futil_mdp
           MOVE "MERLET" TO futil_nom
           MOVE "Thomas" TO futil_prenom
           MOVE "thomas.merlet@gmail.com" TO futil_mail
           MOVE "admin" TO futil_type
           MOVE "Commerce" TO futil_formation
           MOVE "25/02/1999" TO futil_naissance

           OPEN I-O futilisateur
           WRITE tamp_futi
           END-WRITE
           IF cr_futil = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_futil
           END-IF
           CLOSE futilisateur


      * Utilisateur 4 : administrateur
           MOVE "c.leau" TO futil_login
           MOVE "camille" TO futil_mdp
           MOVE "LEAU" TO futil_nom
           MOVE "Camille" TO futil_prenom
           MOVE "camille.leau@gmail.com" TO futil_mail
           MOVE "admin" TO futil_type
           MOVE "art" TO futil_formation
           MOVE "02/10/2000" TO futil_naissance

           OPEN I-O futilisateur
           WRITE tamp_futi
           END-WRITE
           IF cr_futil = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_futil
           END-IF
           CLOSE futilisateur

      * Utilisateur 5 : membre
           MOVE "s.ledourner" TO futil_login
           MOVE "swann" TO futil_mdp
           MOVE "LE DOURNER" TO futil_nom
           MOVE "Swann" TO futil_prenom
           MOVE "swann.ledourner@gmail.com" TO futil_mail
           MOVE "membre" TO futil_type
           MOVE "MIAGE" TO futil_formation
           MOVE "21/12/1996" TO futil_naissance

           OPEN I-O futilisateur
           WRITE tamp_futi
           END-WRITE
           IF cr_futil = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_futil
           END-IF
           CLOSE futilisateur

      * Utilisateur 6 : membre
           MOVE "k.cosquer" TO futil_login
           MOVE "kevin" TO futil_mdp
           MOVE "COSQUER" TO futil_nom
           MOVE "Kevin" TO futil_prenom
           MOVE "kevin.cosquer@gmail.com" TO futil_mail
           MOVE "membre" TO futil_type
           MOVE "IFSI" TO futil_formation
           MOVE "23/04/2001" TO futil_naissance

           OPEN I-O futilisateur
           WRITE tamp_futi
           END-WRITE
           IF cr_futil = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_futil
           END-IF
           CLOSE futilisateur

      * Utilisateur 7 : membre
           MOVE "g.koc" TO futil_login
           MOVE "gamze" TO futil_mdp
           MOVE "KOC" TO futil_nom
           MOVE "Gamze" TO futil_prenom
           MOVE "gamze.koc@gmail.com" TO futil_mail
           MOVE "membre" TO futil_type
           MOVE "Commerce" TO futil_formation
           MOVE "28/03/2002" TO futil_naissance

           OPEN I-O futilisateur
           WRITE tamp_futi
           END-WRITE
           IF cr_futil = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_futil
           END-IF
           CLOSE futilisateur


      * Utilisateur 8 : membre
           MOVE "t.leberre" TO futil_login
           MOVE "thibault" TO futil_mdp
           MOVE "LE BERRE" TO futil_nom
           MOVE "Thibault" TO futil_prenom
           MOVE "thibault.leberre@gmail.com" TO futil_mail
           MOVE "membre" TO futil_type
           MOVE "art" TO futil_formation
           MOVE "08/08/2001" TO futil_naissance

           OPEN I-O futilisateur
           WRITE tamp_futi
           END-WRITE
           IF cr_futil = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_futil
           END-IF
           CLOSE futilisateur

      * Evènement 1
        MOVE "Gala 2023" TO fevent_nom
        MOVE "Soirée" TO fevent_type
        MOVE "21/06/2023" TO fevent_date
        MOVE "m.loret" TO fevent_loginOrga
        MOVE "Soirée de fin d'année" TO fevent_description
        MOVE "2 rue de la liberte, 35000 Rennes" TO fevent_adresse
        MOVE "En cours" TO fevent_etat
        MOVE 150 TO fevent_seuil
        MOVE "20h00" TO fevent_heure

        OPEN I-O fevenement
           WRITE tamp_fevent
           END-WRITE
           IF cr_fevent = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_fevent
           END-IF
           CLOSE fevenement

      * Evènement 2
        MOVE "Escape game de l'horreur" TO fevent_nom
        MOVE "Escape Game" TO fevent_type
        MOVE "15/07/2023" TO fevent_date
        MOVE "l.egain" TO fevent_loginOrga
        MOVE "Pour se térrifier en cette
        belle période" TO fevent_description
        MOVE "Les loges, 44140 Montbert" TO fevent_adresse
        MOVE "En cours" TO fevent_etat
        MOVE 6 TO fevent_seuil
        MOVE "21h00" TO fevent_heure

        OPEN I-O fevenement
           WRITE tamp_fevent
           END-WRITE
           IF cr_fevent = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_fevent
           END-IF
           CLOSE fevenement

      * Evènement 3
        MOVE "Afterwork" TO fevent_nom
        MOVE "Soirée" TO fevent_type
        MOVE "02/06/2023" TO fevent_date
        MOVE "t.merlet" TO fevent_loginOrga
        MOVE "Afterwork de fin d'année" TO fevent_description
        MOVE "9 Rue Bon Secours, 44000 Nantes" TO fevent_adresse
        MOVE "En cours" TO fevent_etat
        MOVE 150 TO fevent_seuil
        MOVE "18h15" TO fevent_heure

        OPEN I-O fevenement
           WRITE tamp_fevent
           END-WRITE
           IF cr_fevent = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_fevent
           END-IF
           CLOSE fevenement

      * Evènement 4
        MOVE "Karting extérieur" TO fevent_nom
        MOVE "Karting" TO fevent_type
        MOVE "22/09/22023" TO fevent_date
        MOVE "c.leau" TO fevent_loginOrga
        MOVE "Karting de rentrée" TO fevent_description
        MOVE "19 Rte des Naudières, 44880 Sautron" TO fevent_adresse
        MOVE "En cours" TO fevent_etat
        MOVE 150 TO fevent_seuil
        MOVE "20h00" TO fevent_heure

        OPEN I-O fevenement
           WRITE tamp_fevent
           END-WRITE
           IF cr_fevent = 35
               DISPLAY "Echec d'insertion"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_fevent
           END-IF
           CLOSE fevenement
      *-----------------------------------------------------------------
      *                  PROGRAMME PRINCIPAL
      *-----------------------------------------------------------------
           DISPLAY "---------------------------------------------------"
           DISPLAY "Bienvenue sur l'application de gestion d'évènements"
           DISPLAY "---------------------------------------------------"

           PERFORM menuUtilisateur

            STOP RUN.
      *-----------------------------------------------------------------
      *                  FONCTIONS ET PROCEDURES
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
      *      Procédure gérant le menu d'un utilisateur pouvant devenir
      *      organisateur d'un ou plusieurs évènements s'il en crée
      *-----------------------------------------------------------------
           menuUtilisateur.
           DISPLAY"--------------------------------------------"
           DISPLAY"|             MENU PRINCIPAL                |"
           DISPLAY"--------------------------------------------"
           PERFORM WITH TEST AFTER UNTIL choix =0
               DISPLAY "Que souhaitez-vous faire ?"
               DISPLAY "1 - Gérer votre profil"
               DISPLAY "2 - Rechercher un évènement"
               DISPLAY "3 - Gestion d'évènement"
               IF futil_type='admin' THEN
                   DISPLAY "4 - Afficher les statistiques"
                   DISPLAY "5 - Modifier le type d'utilisateur"
                   DISPLAY "6 - Supprimer évènement passé"
               END-IF
               DISPLAY "0 - Déconnexion"
               ACCEPT choix


               EVALUATE choix
                   WHEN 1 PERFORM gererProfil
                   WHEN 2 PERFORM rechercherEvent
                   WHEN 3 PERFORM gestionEvenement
                   WHEN 4
                       IF futil_type='admin'
                       THEN PERFORM afficheStatistique
                       ELSE DISPLAY "Non autorisé"
                           PERFORM menuUtilisateur
                       END-IF
                   WHEN 5
                      IF futil_type = 'admin'
                           THEN PERFORM modifierUtilisateur
                      ELSE DISPLAY "Non autorisé"
                           PERFORM menuUtilisateur
                       END-IF
                   WHEN 6
                       IF futil_type = 'admin'
                           THEN PERFORM supprimerEventPasse
                       ELSE DISPLAY "Non autorisé"
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
               DISPLAY "0 - Revenir au menu précédent"
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
               DISPLAY "1 - Rechercher avec le nom de l'évènement"
               DISPLAY "2 - Rechercher sur le type d'évènement"
               DISPLAY "3 - Rechercher par organisateur"
               DISPLAY "0 - Revenir au menu précédent"
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
               DISPLAY "1 - Créer un évènement"
               DISPLAY "2 - Modifier un évènement"
               DISPLAY "3 - Supprimer un évènement"
               DISPLAY "4 - Afficher évènement"
               DISPLAY "0 - Revenir au menu précédent"
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
               DISPLAY "0 - Revenir au menu précédent"
               ACCEPT choix

               EVALUATE choix
      *         WHEN 1 PERFORM afficherStat
                WHEN 0 PERFORM menuUtilisateur
               END-EVALUATE
           END-PERFORM
           .

           modifierUtilisateur.
           DISPLAY"--------------------------------------------"
           DISPLAY"|          MODIFIER UTILISATEUR            |"
           DISPLAY"--------------------------------------------"
           .

           supprimerEventPasse.
           DISPLAY"--------------------------------------------"
           DISPLAY"|       SUPPRIMER EVENEMENT PASSE          |"
           DISPLAY"--------------------------------------------"
           .
      *-----------------------------------------------------------------
      * Procédure permettant de rechercher un évènement en fonction de
      *                        son nom
      *-----------------------------------------------------------------


           rechercherNom.
           PERFORM afficheEvent
           OPEN INPUT fevenement
               DISPLAY "Quel évènement voulez-vous rechercher ?"
               DISPLAY "(saisir le nom)"
               ACCEPT fevent_nom
               READ fevenement
                   INVALID KEY
                       DISPLAY "Evènement non trouvé."
                       DISPLAY "Veuillez réessayer votre recherche."
                   NOT INVALID KEY
                       DISPLAY "Voici les informations de l'évènement :"
                       DISPLAY "Nom : " fevent_nom
                       DISPLAY "Type : "fevent_type
                       DISPLAY "Date : "fevent_date
                       DISPLAY "Heure de début : " fevent_heure
                       DISPLAY "Description : " fevent_description
                       DISPLAY "Adresse : " fevent_adresse
                       DISPLAY "Seuil : "fevent_seuil
                       DISPLAY "Login organisateur : " fevent_loginOrga
               END-READ
           CLOSE fevenement.



      *-----------------------------------------------------------------
      *          Procédures permettant de créer un évènement
      *-----------------------------------------------------------------

      ******************************************************************
      *    Fonction parallèle :
      *    Fonction qui vérifie que le nom de l'évènement n'est pas déjà
      *    présent
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
      *     THEN DISPLAY "Evènement trouve"
      *     ELSE DISPLAY "Evènement non trouve"
      *     END-IF
           CLOSE fevenement
           .
      ******************************************************************
      *    Fonction principale :
      *    Fonction qui permet de créer l'évènement
      ******************************************************************
           creerEvent.
           DISPLAY"-------------------------------------------"
           DISPLAY"|          CREATION EVENEMENT             |"
           DISPLAY"-------------------------------------------"
           PERFORM WITH TEST AFTER UNTIL estValideEvenementResultat = 0
               DISPLAY "Saisir le nom de l'évènement"
               DISPLAY"(maximum 30 caractères)"
               ACCEPT nomEvent
               PERFORM existeEvent
           END-PERFORM
           DISPLAY "Saisir le type d'évènement"
           ACCEPT typeEvent
           DISPLAY "Saisir la date de l'évènement"
           DISPLAY "Format : JJ/MM/AAAA"
           ACCEPT dateEvent
      *     PERFORM UNTIL loginOrga EQUALS loginSaved
               DISPLAY "Saisir votre identifiant de connexion"
               DISPLAY "(correspondant à votre login de connexion)"
               ACCEPT loginOrga
      *     END-PERFORM
           DISPLAY "Veuillez décrire votre évènement"
           DISPLAY "Format : maximum 50 caractères"
           ACCEPT descriptionEvent
           DISPLAY "Veuillez saisir l'adresse de l'évènement"
           ACCEPT adresseEvent
           PERFORM UNTIL seuilEvent > 0
               DISPLAY "Veuillez saisir le nombre maximal de personne"
               ACCEPT seuilEvent
           END-PERFORM
           DISPLAY "Veuillez saisir l'heure de début de l'évènement"
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
               DISPLAY "Echec d'insertion, veuillez réessayer"
           ELSE
               DISPLAY "Insertion réussie"
               DISPLAY cr_fevent
           END-IF

           CLOSE fevenement
           .


      *-----------------------------------------------------------------
      *          Procédures permettant d'afficher les évènements
      *-----------------------------------------------------------------
           afficheEvent.
           DISPLAY"--------------------------------------------"
           DISPLAY"|          AFFICHAGE EVENEMENT             |"
           DISPLAY"--------------------------------------------"
           OPEN INPUT fevenement
               MOVE 0 TO Fin
               PERFORM WITH TEST AFTER UNTIL Fin = 1
               READ fevenement
               AT END MOVE 1 TO Fin
               NOT AT END
                   IF fevent_date>=WS-CURRENT-DATE
                   DISPLAY "Nom : "fevent_nom
                   DISPLAY "Type : "fevent_type
                   DISPLAY"----------"
               END-READ
               END-PERFORM
               CLOSE fevenement.
           .

      *-----------------------------------------------------------------
      *          Procédures permettant de s'inscrire à un évènement
      *-----------------------------------------------------------------
      *     inscriptionEvent.
      *     PERFORM afficheEvent
      *     DISPLAY "A quel évènement voulez vous vous inscrire?"
      *     ACCEPT nomEvent
      *     MOVE nomEvent TO nom_saved
      *     OPEN I-O fparticipant
      *         MOVE "En attente" TO fpart_etat
      *         MOVE 0 TO Fin
      *         MOVE 1 TO cpt
      *         PERFORM WITH TEST AFTER UNTIL Fin = 1
      *             READ fparticipant
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
      *          Procédures permettant de rechercher un utilisateur par
      *          son nom
      *-----------------------------------------------------------------
           rechercherUtilisateurNom.
           DISPLAY "Veuillez saisir le nom de la personne souhaitée : "
           ACCEPT nom
           OPEN INPUT futilisateur
           PERFORM WITH TEST AFTER UNTIL Fin = 1
               READ futilisateur
               AT END
                   MOVE 1 TO Fin
               NOT AT END
                   IF nom = futil_nom THEN
                       DISPLAY "Nom : " futil_nom
                       DISPLAY "Prénom : " futil_prenom
                       DISPLAY "Mail : " futil_mail
                       DISPLAY "Téléphone : " futil_tel
                       DISPLAY "Login : "futil_login
                       DISPLAY "Type d'utilisateur : "futil_type
                   END-IF
           END-PERFORM
           CLOSE futilisateur
           .

      *-----------------------------------------------------------------
      *          Procédures permettant de rechercher un utilisateur par
      *          son login
      *-----------------------------------------------------------------
           rechercherUtilisateurLogin.
           DISPLAY "Veuillez saisir le login de la personne souhaitée :"
           ACCEPT login
           OPEN INPUT futilisateur
           READ futilisateur
               INVALID KEY
                   DISPLAY "Utilisateur non trouvé"
               NOT INVALID KEY
                   IF login = futil_login THEN
                       DISPLAY "Nom : "futil_nom
                       DISPLAY "Prénom :" futil_prenom
                       DISPLAY "Mail :  " futil_mail
                       DISPLAY "Téléphone :  " futil_tel
                       DISPLAY "Login :  " futil_login
                       DISPLAY "Type utilisateur :  " futil_type
                   END-IF
           CLOSE futilisateur
           .
