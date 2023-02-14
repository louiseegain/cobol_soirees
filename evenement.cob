      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           select futilisateur assign to "utilisateur.dat"
           organization indexed
           access mode is dynamic
           record key is futi_login
           alternate record key is futi_genre WITH DUPLICATES
           ALTERNATE RECORD KEY IS futi_type WITH DUPLICATES
           file status is cr_futi.


           select fevenement assign to "evenement.dat"
           organization indexed
           access mode is dynamic
           record key is fevent_id
           alternate record key is fevent_type WITH DUPLICATES
           alternate record key is fevent_idOrga WITH DUPLICATES
           file status is cr_fevent.


           SELECT fparticipant ASSIGN TO "participant.dat"
           ORGANIZATION INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY fpart_inscriptionId
           ALTERNATE RECORD KEY fpart_inscriptionEtat WITH DUPLICATES
           ALTERNATE RECORD KEY fpart_login WITH DUPLICATES
           ALTERNATE RECORD KEY fpart_idEvent WITH DUPLICATES
           FILE STATUS IS cr_fpart.

           SELECT fhistorique ASSIGN TO "historique.dat"
           ORGANIZATION SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS cr_fhisto
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD futilisateur.
       01 tamp_futi.
           02 futi_nom PIC A(30).
           02 futi_prenom PIC A(30).
           02 futi_login PIC X(30).
           02 futi_mdp PIC X(12).
           02 futi_genre PIC A(8).
           02 futi_type PIC A(10).

       FD fevenement.
       01 tamp_fevent.
           02 fevent_id PIC 9(8).
           02 fevent_type PIC A(20).
           02 fevent_date PIC X(10).
           02 fevent_idOrga PIC X(30).
           02 fevent_description PIC X(50).
           02 fevent_adresse PIC X(30).

       FD fparticipant.
       01 tamp_fpart.
           02 fpart_inscriptionId PIC 9(8).
           02 fpart_inscriptionEtat PIC 9(10).
           02 fpart_inscriptionDate PIC X(10).
           02 fpart_login PIC X(30).
           02 fpart_idEvent PIC 9(8).

       FD fhistorique.
       01 tamp_fhisto.
           02 fhisto_eventId PIC 9(8).
           02 fhisto_eventType PIC A(20).
           02 fhisto_eventDate PIC X(10).
           02 fhisto_eventIdOrga PIC X(30).
           02 fhisto_description PIC X(50).
           fhisto_nbPart PIC 9(3).
      *-----------------------
       WORKING-STORAGE SECTION.
       77 cr_futi PIC 9(2).
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
            DISPLAY "Hello world"
            STOP RUN.
      ** add other procedures here
       END PROGRAM YOUR-PROGRAM-NAME.
