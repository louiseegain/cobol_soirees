      ******************************************************************
      * Author:
      * Date:
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



      *-----------------------------------------------------------------
      *                  FONCTIONS ET PROCEDURES
      *-----------------------------------------------------------------



            STOP RUN.
      ** add other procedures here
       END PROGRAM Evenements.
