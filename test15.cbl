      ******************************************************************
      * Author: YANN
      * Date: 23/11/2023
      * Purpose: APPLIQUER DES REMISES AUX CLIENTS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REMCLI.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *--------------------
       FILE-CONTROL.
      *--------------------
       SELECT FIC-R1    ASSIGN TO 'C:/Users/y_cle/ENTREE/R1_VIDE.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS                IS FS-F-CLI.
       SELECT FIC-R2    ASSIGN TO 'C:/Users/y_cle/ENTREE/R2_VIDE.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS                 IS FS-F-COM.
       SELECT CARTE-P1    ASSIGN TO 'C:/Users/y_cle/ENTREE/P1.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS                 IS FS-F-P1.
       SELECT CARTE-P2    ASSIGN TO 'C:/Users/y_cle/ENTREE/P2.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS                 IS FS-F-P2.
       SELECT CARTE-P3       ASSIGN TO 'C:/Users/y_cle/ENTREE/P3.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS                 IS FS-F-P3.
       SELECT FIC-W1   ASSIGN TO 'C:/Users/y_cle/SORTIE/W1.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS                 IS FS-F-W1.
       SELECT FIC-W2   ASSIGN TO 'C:/Users/y_cle/SORTIE/W2.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS                 IS FS-F-W2.
       SELECT FIC-W3     ASSIGN TO 'C:/Users/y_cle/SORTIE/W3.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS                 IS FS-F-W3.
       SELECT FIC-W4    ASSIGN TO 'C:/Users/y_cle/SORTIE/W4.txt'
                   ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS                 IS FS-F-W4.
      *
      *****************************************************************
      *                     DESCRIPTION DES FICHIERS                  *
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  FIC-R1       RECORDING F.
       01  ENRG-CLI            PIC X(79).
       FD  FIC-R2       RECORDING F.
       01  ENRG-COMMAN         PIC X(58).
       FD  CARTE-P1     RECORDING F.
       01  ENRG-DATEJ          PIC X(8).
       FD  CARTE-P2     RECORDING F.
       01  ENRG-REM-TAB.
           05  FS-NUM-REM      PIC X(10).
           05  FILLER          PIC X(26).
       FD  CARTE-P3     RECORDING F.
       01  ENRG-SEXECLI.
           05  FS-COD-SX       PIC 9.
           05  FILLER          PIC X(9).
       FD  FIC-W1       RECORDING F.
       01  ENRG-INFOCPT        PIC X(98).
       FD  FIC-W2       RECORDING F.
       01  ENRG-FICERROR       PIC X(102).
       FD  FIC-W3       RECORDING F.
       01  ENRG-LISTCO         PIC X(102).
       FD  FIC-W4       RECORDING F.
       01  ENRG-REMISES        PIC X(98).
      *
       WORKING-STORAGE SECTION.
      ******************************************************************
      *                     WORKING STORAGE SECTION                    *
      ******************************************************************
      *----------------------------------------------------------------*
      *                        R1 - CLIENTS                            *
      *----------------------------------------------------------------*
       01  WS-ENRG-CLI.
           05  WS-NUM-CLI      PIC 9(10).
           05  WS-NOM-CLI      PIC X(10).
           05  WS-PRE-CLI      PIC X(10).
           05  WS-CIV-CLI      PIC 9.
           05  WS-INS-CLI      PIC 9(8).
           05  WS-ADR-CLI      PIC X(40).
      *----------------------------------------------------------------*
      *                        R2 - COMMANDES                          *
      *----------------------------------------------------------------*
       01  WS-ENRG-COM.
           05  WS-KEY-COM      PIC 9(10).
           05  WS-DAY-COM      PIC 9(8).
           05  WS-LIB-COM      PIC X(20).
           05  WS-ART-COM      PIC 9(3).
           05  WS-PRI-COM      PIC 9(3)V9(2).
           05  WS-QTY-COM      PIC 9(2).
           05  WS-NUM-COM      PIC X(10).
      *----------------------------------------------------------------*
      *                        P1 - DATEJOUR                           *
      *----------------------------------------------------------------*
       01  WS-ENRG-DATEJ.
           05  WS-DATE-J       PIC 9(8).
      *----------------------------------------------------------------*
      *                        P2 - REMISES                            *
      *----------------------------------------------------------------*
       01  WS-ENRG-REM.
           05  WS-TAB-COD      PIC 9(10).
           05  WS-TAB-LIB      PIC X(8).
           05  WS-TAB-REM      PIC 9(2).
           05  WS-TAB-DEB      PIC 9(8).
           05  WS-TAB-FIN      PIC 9(8).
      *----------------------------------------------------------------*
      *                        P3 - SEXE                               *
      *----------------------------------------------------------------*
       01  ENR-P3.
           05  WS-SEX-COD      PIC 9.
           05  WS-SEX-LIB      PIC X(9).
      *----------------------------------------------------------------*
      *                        W1 - INFCOMPT  (SUR 24 MOIS)            *
      *----------------------------------------------------------------*
       01  WS-ENRG-INFCOMPT.
           05  WS-CPT-CLI      PIC 9(10).
           05  WS-CPT-NOM      PIC X(10).
           05  WS-CPT-PRE      PIC X(10).
           05  WS-CPT-COM      PIC 9(2).
           05  WS-CPT-TOT      PIC 9(3)V9(2).
      *----------------------------------------------------------------*
      *                        W2 - FICERRO   (JOURNAL DES ERREURS)    *
      *----------------------------------------------------------------*
       01  WS-FICERROR         PIC X(80).
      *----------------------------------------------------------------*
      *                        W3 - LISTCO   (ETAT LINSTING 24M )      *
      *----------------------------------------------------------------*
      *    Entete
       01  WS-BLANK.
           05 FILLER           PIC X(98)   VALUE SPACES.
       01  WS-LCPTE.
           05  FILLER          PIC X(25)   VALUE
               'Listing des commandes de '.
           05  WS-CIV          PIC X(8).
           05  WS-SPA          PIC X       VALUE SPACE.
           05  WS-NOM          PIC X(15).
           05  WS-N            PIC X(6)    VALUE ' (N°'.
           05  WS-NUMC         PIC X(10).
           05  WS-LAST         PIC X(25)   VALUE
               ') sur 24 mois établie le '.
           05  WS-DTJ          PIC X(10).
      *    Vide
       01  WS-VIDE             PIC X(87)   VALUE ALL SPACES.
      *    Titres tableau
       01  WS-TITLE.
           05  FILLER          PIC X(17)   VALUE
           ' | Date Commande '.
           05  FILLER          PIC X(23)   VALUE
           '|        Article       '.
           05  FILLER          PIC X(17)   VALUE
           '| Numéro article '.
           05  FILLER          PIC X(16)   VALUE
           '| Prix unitaire '.
           05  FILLER          PIC X(11)   VALUE
           '| Quantité '.
           05  FILLER          PIC X(18)   VALUE
           '|   Prix total   |'.
      *    Tableau premiere ligne vide
       01  WS-TABVID.
           05  FILLER          PIC X(17)   VALUE
           ' |               '.
           05  FILLER          PIC X(23)   VALUE
           '|                      '.
           05  FILLER          PIC X(17)   VALUE
           '|                '.
           05  FILLER          PIC X(16)   VALUE
           '|               '.
           05  FILLER          PIC X(11)   VALUE
           '|          '.
           05  FILLER          PIC X(19)   VALUE
           '|                |'.
      *    Tableau ligne achat
       01  WS-ACHAT.
           05  FILLER          PIC X(3)    VALUE ' | '.
           05  WS-DTCOM        PIC X(13).
           05  FILLER          PIC X(3)    VALUE ' | '.
           05  WS-LIBART       PIC X(20).
           05  FILLER          PIC X(3)    VALUE ' | '.
           05  WS-NUMART       PIC X(14).
           05  FILLER          PIC X(8)    VALUE ' |      '.
           05  WS-PUART        PIC Z(3).99.
           05  FILLER          PIC X(5)    VALUE '   | '.
           05  WS-QTYART       PIC X(8).
           05  FILLER          PIC X(5)    VALUE ' |   '.
           05  WS-TOTPRIX      PIC Z(7).99.
           05  FILLER          PIC X(4)    VALUE '   |'.
      *    Total des commandes
       01  WS-TOTLIGNE.
           05  FILLER          PIC X(38)   VALUE ALL SPACES.
           05  FILLER          PIC X(43)   VALUE
           'TOTAL DES COMMANDES DES 24 DERNIERS MOIS : '.
           05  WS-PANIERTOT    PIC Z(7).99.
           05  FILLER          PIC X       VALUE '€'.
      *----------------------------------------------------------------*
      *                        W4 - REMISES   (ETAT remise par client) *
      *----------------------------------------------------------------*
      *    Lignes communes
       01  WS-VIDED            PIC X(72)   VALUE ALL SPACES.
       01  WS-TIRED            PIC X(72)   VALUE ALL '-'.
       01  WS-STARS            PIC X(72)   VALUE ALL '*'.
      *    Ligne 1
       01  WS-LIGNE1.
           05  FILLER          PIC X(62)   VALUE 'Bonjour'.
           05  WS-DATED        PIC X(10).
      *    Ligne 2
       01  WS-LIGNE2.
           05  WS-CIVED        PIC X(9).
           05  FILLER          PIC X       VALUE SPACE.
           05  WS-NOMED        PIC X(10).
      *    Ligne 3
       01  WS-LIGNE3.
           05  FILLER          PIC X(33)   VALUE
           'Vous êtes clients depuis plus de '.
           05  WS-ANNEED       PIC 9.
           05  FILLER          PIC X(24)   VALUE
           ' ans et à l''ocasion de '.
           05  WS-LIBLED       PIC X(12).
           05  FILLER          PIC X(2)    VALUE ' :'.
      *    Ligne 3A
       01  WS-LIGNE3A.
           05  FILLER          PIC X(35)   VALUE
           'Vous êtes clients depuis moins d''un'.
           05  FILLER          PIC X(24)   VALUE
           ' an et à l''ocasion de '.
           05  WS-LIBLEDA      PIC X(12).
           05  FILLER          PIC X(2)    VALUE ' :'.
      *    Ligne 3B
       01  WS-LIGNE3B.
           05  FILLER          PIC X(33)   VALUE
           'Vous êtes clients depuis plus de '.
           05  WS-ANNEEDB      PIC 9.
           05  FILLER          PIC X(24)   VALUE
           ' an et à l''ocasion de '.
           05  WS-LIBLEDB      PIC X(12).
           05  FILLER          PIC X(2)    VALUE ' :'.
      *    Ligne 4
       01  WS-LIGNE4.
           05  FILLER          PIC X(19)   VALUE
           'Vous bénéficiez de '.
           05  WS-REMED        PIC X(2).
           05  FILLER          PIC X(44)   VALUE
           '% sur tous les articles de notre catalogue !'.
      *    Ligne 5
       01  WS-LIGNE5           PIC X(23)   VALUE
           'Merci de votre fidélité'.
      *----------------------------------------------------------------*
      *                        FILE STATUS                             *
      *----------------------------------------------------------------*
      *    R2 - CLIENTS        file status
       01  FS-F-CLI            PIC X(2).
           88  FS-CLI-00                   VALUE '00'.
           88  FS-CLI-10                   VALUE '10'.
           88  FS-CLI-00-10                VALUE '00'
                                                 '10'.
           88  FS-CLI-23-00                VALUE '00'
                                                 '23'.
      *    R1 - COMMANDE       file status
       01  FS-F-COM            PIC X(2).
           88  FS-COM-00                   VALUE '00'.
           88  FS-COM-10                   VALUE '10'.
           88  FS-COM-10-46                VALUE '10'
                                                 '46'.
           88  FS-COM-00-10                VALUE '00'
                                                 '10'.
           88  FS-COM-23-00                VALUE '00'
                                                 '23'.
      *    P1 - DATEJOUR       file status
       01  FS-F-P1             PIC X(2).
           88  FS-P1-00                    VALUE '00'.
           88  FS-P1-10                    VALUE '10'.
      *    P2 - REMISES        file status
       01  FS-F-P2             PIC X(2).
           88  FS-P2-00                    VALUE '00'.
           88  FS-P2-10                    VALUE '10'.
      *    P3 - SEXE           file status
       01  FS-F-P3             PIC X(2).
           88  FS-P3-00                    VALUE '00'.
           88  FS-P3-10                    VALUE '10'.
      *    W1 - INFCOMPT       file status
       01  FS-F-W1             PIC X(2).
           88  FS-W1-00                    VALUE '00'.
      *    W2 - FICERRO        file status
       01  FS-F-W2             PIC X(2).
           88  FS-W2-00                    VALUE '00'.
      *    W3 - LISTCO (etat)  file status
       01  FS-F-W3             PIC X(2).
           88  FS-W3-00                    VALUE '00'.
      *    W4 - REMISES (etat) file status
       01  FS-F-W4             PIC X(2).
           88  FS-W4-00                    VALUE '00'.
      *    EOF status
       01  EOF-1               PIC X(1)    VALUE 'N'.
       01  EOF-2               PIC X(1)    VALUE 'N'.
      *----------------------------------------------------------------*
      *                        VARIABLES DE CALCUL                     *
      *----------------------------------------------------------------*
      *
       01  WS-CLI-TRT          PIC 9(10).
      *    VARIABLE DATE
       01  WS-DATE-EN          PIC X(8).
       01  WS-DATE-US.
           05  YYYY-US.
               10 SS           PIC 99.
               10 AA           PIC 99.
           05  MM-EN           PIC 99.
           05  DD-EN           PIC 99.
       01  WS-DATE-FR.
           05  DD-FR           PIC 99.
           05  FILLER          PIC X       VALUE '/'.
           05  MM-FR           PIC 99.
           05  FILLER          PIC X       VALUE '/'.
           05  YYYY-FR.
               10  SS-FR       PIC 99.
               10  AA-FR       PIC 99.
      *
           01 NBJOURS          PIC 9(8).
      *
       01  WS-CLI-IN           PIC 9(10).
       01  WS-CMD-IN           PIC 9(10).
      *    BOOLEENS
       01  WS-CLI-24           PIC X.
           88 WS-CLI-24-NON                VALUE 'N'.
           88 WS-CLI-24-OUI                VALUE 'Y'.
       01  WS-CMD-24           PIC X.
           88 WS-CMD-24-NON                VALUE 'N'.
           88 WS-CMD-24-OUI                VALUE 'Y'.
       01  WS-MILLE            PIC X.
           88 WS-MILLE-NON                 VALUE 'N'.
           88 WS-MILLE-OUI                 VALUE 'Y'.
       01  WS-ISFIRST          PIC X.
           88 WS-ISFIRST-NON               VALUE 'N'.
           88 WS-ISFIRST-OUI               VALUE 'Y'.
       01  WS-CLIENT-FIN       PIC X.
           88 WS-CLIENT-FIN-OUI            VALUE 'N'.
           88 WS-CLIENT-FIN-NON            VALUE 'Y'.
       01  WS-ANNIV            PIC X       VALUE 'N'.
           88 WS-ANNIV-OUI                 VALUE 'Y'.
       01  WS-FIDEL            PIC X       VALUE 'N'.
           88 WS-FIDEL-OUI                 VALUE 'Y'.
       01  WS-NOEL             PIC X       VALUE 'N'.
           88 WS-NOEL-OUI                  VALUE 'Y'.
      *    COMPTEURS
       01  WS-NB-CMD           PIC S9(4)   COMP
                                           VALUE ZERO.
       01  WS-CLI-NB-CMD       PIC S9(4)   COMP
                                           VALUE ZERO.
      *    TOTAUX
       01  WS-TOT-CMD          PIC S9(7)V99 COMP-3.
       01  WS-TOT-ART          PIC S9(7)V99 COMP-3.
      *    CALCUL DES NBJOURS
       01  NBDAYS-1            PIC 9(08).
       01  NBDAYS-2            PIC 9(08).
       01  NBDAYS-3            PIC 9(08).
       01  DIFFERENCE-DAY      PIC 9(04).
       01  DIFFERENCE-CLI      PIC 9(04).
       01  YEARS               PIC 9.
       01  WS-COD-REM          PIC 9(10).
       01  YEARS-CLI           PIC 99V99.
       01  YEARS-COM           PIC 99V99.
      *
       01  COMPTEUR-CLI        PIC 99     COMP
                                          VALUE ZERO.
       01  COMPTEUR-ERR        PIC 99     COMP
                                          VALUE ZERO.
      *
       01  tableau OCCURS 10 TIMES.
           05  CODIF PIC 9.
           05  LIBEL PIC X(9).
       77  COMPT-MAX           PIC 99     VALUE 10.
       77  COMPT-LU            PIC 99     VALUE ZERO.
       77  COMPT-PAR           PIC 99     VALUE ZERO.
       01  EOF-P3              PIC 9      VALUE ZERO.
       01  WS-GENDER-CODE      PIC 9      VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *
      ******************************0000*******************************
      *****************************************************************
      *---------------------------------------------------------------*
      *DEBUT DU TRAITEMENT PRINCIPAL                                  *
      *---------------------------------------------------------------*
      *
       TRT-PRINCIPAL-DEB.
      *
      *---------------------------------------------------------------*
      *PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
      *
           DISPLAY '***********0000-DEB***********'
           PERFORM OV-CLI-DEB
              THRU OV-CLI-FIN.
           PERFORM READ-F-CLI-DEB
              THRU READ-F-CLI-FIN.
      *    Test si fichier client vide
           IF FS-F-CLI = '10'
              DISPLAY 'Fichier client vide (' FS-F-CLI ')'
              CLOSE FIC-R1
              PERFORM FIN-PROG-DEB
                 THRU FIN-PROG-FIN
           END-IF.
      *
           PERFORM OV-COM-DEB
              THRU OV-COM-FIN.
           PERFORM READ-F-CMD-DEB
              THRU READ-F-CMD-FIN.
      *    Test si fichier commande vide
           IF FS-F-COM = '10'
              DISPLAY 'Fichier commande vide (' FS-F-COM ')'
              CLOSE FIC-R1
              CLOSE FIC-R2
              PERFORM FIN-PROG-DEB
                 THRU FIN-PROG-FIN
           END-IF.
      *
           PERFORM OV-P1-DEB
              THRU OV-P1-FIN.
           PERFORM OV-P2-DEB
              THRU OV-P2-FIN.
           PERFORM READ-P2-DEB
              THRU READ-P2-FIN.
      *    Parametrage de la remise en cours apres lecture P2
           IF          WS-TAB-COD = 0000000001
               SET WS-ANNIV-OUI TO TRUE
           ELSE IF     WS-TAB-COD = 0000000002
               SET WS-FIDEL-OUI TO TRUE
           ELSE IF     WS-TAB-COD = 0000000003
               SET WS-NOEL-OUI  TO TRUE
           END-IF.
      *
           PERFORM OV-P3-DEB
              THRU OV-P3-FIN.
      *    Lecture de la carte P3 et alimentation du tableau
           PERFORM UNTIL EOF-P3 = 1
               READ CARTE-P3 INTO ENR-P3
               AT END
                   MOVE 1       TO EOF-P3
               NOT AT END
                   ADD 1        TO COMPT-LU
                   MOVE ENR-P3  TO tableau (COMPT-LU)
                   MOVE LOW-VALUES TO ENR-P3
               END-READ
               IF COMPT-LU > COMPT-MAX
                   DISPLAY 'ERREUR LECTURE COMPTEUR TABLEAU'
                   GOBACK
               END-IF
           END-PERFORM.
      *     PERFORM VARYING COMPT-PAR FROM 1 BY 1
      *     UNTIL COMPT-PAR > COMPT-LU
      *     DISPLAY tableau ( COMPT-PAR ) NO ADVANCING
      *     END-PERFORM.
      *         DISPLAY ' HOMME : ' LIBEL (1)
      *         DISPLAY ' FEMME : ' LIBEL (2)
           PERFORM OV-W1-DEB
              THRU OV-W1-FIN.
           PERFORM OV-W2-DEB
              THRU OV-W2-FIN.
           PERFORM OV-W3-DEB
              THRU OV-W3-FIN.
           PERFORM OV-W4-DEB
              THRU OV-W4-FIN.
      *    Préparation date du jour
           PERFORM DATEJ-IS-DEB
              THRU DATEJ-IS-FIN.
      *
      *---------------------------------------------------------------*
      *APPEL DU COMPOSANT TRAITEMENT COMPTE (ITERATIVE)
      *---------------------------------------------------------------*
      *
           PERFORM TRT-ASRTMT-DEB
              THRU TRT-ASRTMT-FIN
             UNTIL EOF-1 = 'Y' AND EOF-2 = 'Y'.
      *
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)
      *---------------------------------------------------------------*
           DISPLAY '***********0000-FIN***********'
           PERFORM FE-CLI-DEB
              THRU FE-CLI-FIN.
           PERFORM FE-COM-DEB
              THRU FE-COM-FIN.
           PERFORM FE-P1-DEB
              THRU FE-P1-FIN.
           PERFORM FE-P2-DEB
              THRU FE-P2-FIN.
           PERFORM FE-P3-DEB
              THRU FE-P3-FIN.
           PERFORM FE-W1-DEB
              THRU FE-W1-FIN.
           PERFORM FE-W2-DEB
              THRU FE-W2-FIN.
           PERFORM FE-W3-DEB
              THRU FE-W3-FIN.
           PERFORM FE-W4-DEB
              THRU FE-W4-FIN.
           PERFORM FIN-PROG-DEB
              THRU FIN-PROG-FIN.
      *
      *---------------------------------------------------------------*
      *FIN DU PROGRAMME
      *---------------------------------------------------------------*
      *
       TRT-PRINCIPAL-FIN.
           STOP RUN.
      ******************************1000*******************************
      **********************TRAITEMENT ASSORTIMENT*********************
      *---------------------------------------------------------------*
      *DEBUT DU TRAITEMENT ASSORTIMENT                                *
      *---------------------------------------------------------------*
       TRT-ASRTMT-DEB.
      *---------------------------------------------------------------*
      *PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
           DISPLAY '***********1000-DEB***********'
      *    Préparation des données clients
           PERFORM VAR-ASRTM-DEB
              THRU VAR-ASRTM-FIN.
      *
      *---------------------------------------------------------------*
      *APPEL DU COMPOSANT TRAITEMENT MOUVEMENT (ALTERNATIVE)
      *---------------------------------------------------------------*
           EVALUATE TRUE
               WHEN    WS-NUM-CLI < WS-KEY-COM
                   PERFORM TRT-CLI-NO-CMD-DEB
                      THRU TRT-CLI-NO-CMD-FIN
               WHEN    WS-NUM-CLI = WS-KEY-COM
                   PERFORM TRT-CLI-DEB
                      THRU TRT-CLI-FIN
               WHEN    WS-NUM-CLI > WS-KEY-COM
                   PERFORM TRT-CMD-NO-CLI-DEB
                      THRU TRT-CMD-NO-CLI-FIN
           END-EVALUATE.
      *
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT COMPTE
      *---------------------------------------------------------------*
      *
       TRT-ASRTMT-FIN.
           EXIT.
      ******************************2000*******************************
      ***********************CLIENT SANS COMMANDE**********************
      *---------------------------------------------------------------*
      *DEBUT DU TRAITEMENT ANOMALIE 1                                 *
      *---------------------------------------------------------------*
       TRT-CLI-NO-CMD-DEB.
      *---------------------------------------------------------------*
      *BAS DU TRAITEMENT ANOMALIE 1
      *---------------------------------------------------------------*
           DISPLAY '***********2000-DEB/FIN***********'
      *    lecture CLIENT
           PERFORM READ-F-CLI-DEB
              THRU READ-F-CLI-FIN.
              IF EOF-1 = 'Y'
                  MOVE 999999 TO WS-NUM-CLI
                  DISPLAY '/////////*****FIN FICHIER CLIENT***/////////'
              END-IF.
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT ANOMALIE 1
      *---------------------------------------------------------------*
       TRT-CLI-NO-CMD-FIN.
           EXIT.
      ******************************2010*******************************
      ***********************TRAITEMENT CLIENT***********************
      *---------------------------------------------------------------*
      *DEBUT DU TRAITEMENT CLIENT                                     *
      *---------------------------------------------------------------*
       TRT-CLI-DEB.
      *---------------------------------------------------------------*
      *PREPARATION DU TRAITEMENT (OREILLETTE GAUCHE)
      *---------------------------------------------------------------*
           DISPLAY '***********2010-DEB***********'
      *    Calcul ancienneté du client
           PERFORM DATE-CLI-DEB
              THRU DATE-CLI-FIN.
           IF YEARS-CLI >= 2
               SET WS-CLI-24-OUI       TO TRUE
           END-IF.
      *---------------------------------------------------------------*
      *APPEL DU COMPOSANT OPERATION SUIVANT (ITERATIVE)
      *---------------------------------------------------------------*
           PERFORM TRT-CMD-DEB
              THRU TRT-CMD-FIN
             UNTIL WS-NUM-CLI < WS-KEY-COM
                OR EOF-2 = 'Y'.
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT (OREILLETTE DROITE)
      *----------------------------------------------------------------
           DISPLAY '***********2010-FIN***********'
      *SI
      *    total commande <> '0'
      *    ecrire total client
      *FIN
           IF WS-TOT-CMD <> '0'
               MOVE WS-TOT-CMD         TO WS-PANIERTOT
               PERFORM WRITE-FIN-TAB-DEB
                  THRU WRITE-FIN-TAB-FIN
           DISPLAY '*** ECRITURE FIN TOTAL : ' WS-PANIERTOT '***'
           DISPLAY '*** POUR LE CLIENT : ' WS-NOM-CLI '***'
      *SI
      *    INSCRIPTION CLIENT +24 mois & une COMMANDE -24 mois
      *        REMISE ANNIVERSAIRE
      *        ECRITURE W4 REMISE
      *FIN
           IF WS-CLI-24 = 'Y' AND WS-CMD-24 = 'Y' AND WS-ANNIV = 'Y'
               DISPLAY '***REMISE ANNIERSAIRE POUR ' WS-NOM-CLI '***'
               PERFORM WRITE-W4-DEB
                  THRU WRITE-W4-FIN
      *SI
      *    COMMANDE -24 MOIS & MT TOTAL +1000€
      *        REMISE FIDELITE
      *        ECRITURE W4 REMISE
           ELSE IF WS-CMD-24 = 'Y' AND WS-MILLE = 'Y' AND WS-FIDEL = 'Y'
               DISPLAY '***REMISE FIDELITE POUR ' WS-NOM-CLI '***'
               PERFORM WRITE-W4-DEB
                  THRU WRITE-W4-FIN
           END-IF.
      *SI COMPETEUR COMMANDE DIFFERENT DE 0
           IF WS-CLI-NB-CMD <> 0
               PERFORM INIT-W1-DEB
                  THRU INIT-W1-FIN
               IF COMPTEUR-CLI > 0
                   PERFORM WRITE-W1-SAUT-DEB
                      THRU WRITE-W1-SAUT-FIN
               ELSE
                   PERFORM WRITE-W1-DEB
                      THRU WRITE-W1-FIN
               END-IF
           END-IF.
      *SINON
      *    pas de remise si aucune condition respectée
      *    lecture client suivant
           PERFORM READ-F-CLI-DEB
              THRU READ-F-CLI-FIN.
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT CLIENT
      *---------------------------------------------------------------*
      *
       TRT-CLI-FIN.
           EXIT.
      ******************************2020*******************************
      ******************COMMANDE SANS COMPTE CLIENT********************
      *---------------------------------------------------------------*
      *DEBUT DU TRAITEMENT ANOMALIE 2                                 *
      *---------------------------------------------------------------*
       TRT-CMD-NO-CLI-DEB.
      *---------------------------------------------------------------*
      *BAS DU TRAITEMENT ANOMALIE 2
      *---------------------------------------------------------------*
      *    écriture fichier FICERRO (commande sans compte client)
           DISPLAY '***********2020-FIN***********'
           MOVE WS-ENRG-COM    TO WS-FICERROR.
           DISPLAY 'CMD SANS CLIENT ' WS-ENRG-COM
           PERFORM WRITE-W2-DEB
              THRU WRITE-W2-FIN
      *    lecture du fichier COMMAND
           PERFORM READ-F-CMD-DEB
              THRU READ-F-CMD-FIN
           IF EOF-2 = 'Y'
               MOVE 999999 TO WS-KEY-COM
               DISPLAY '/////////***FIN FICHIER COMMANDE***/////////'
           END-IF.
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT ANOMALIE 2
      *---------------------------------------------------------------*
      *
       TRT-CMD-NO-CLI-FIN.
           EXIT.
      ******************************3000*******************************
      **********************TRAITEMENT CLIENT**************************
      *---------------------------------------------------------------*
      *DEBUT DU TRAITEMENT BOUCLE                                     *
      *---------------------------------------------------------------*
       TRT-CMD-DEB.
      *---------------------------------------------------------------*
      *BAS DU TRAITEMENT BOUCLE
      *---------------------------------------------------------------*
           DISPLAY '***********PASSAGE 3000***********'
      *SI
      *    commande passée date -24 mois et premiere commande
           PERFORM DATE-CMD-DEB
              THRU DATE-CMD-FIN.
           IF YEARS-COM <= 2
               DISPLAY 'COMMANDE RECENTE : OUI '
               SET WS-CMD-24-OUI       TO TRUE
               DISPLAY 'DATE COMMANDE SUR 24M : ' YEARS-COM.
      *SI
      *    commande passée date -24 mois et commande suivante
           IF WS-ISFIRST-NON AND YEARS-COM < 2
               PERFORM SUM-TOTAL-DEB
                  THRU SUM-TOTAL-FIN
               PERFORM WRITE-CMD-RECAP-DEB
                  THRU WRITE-CMD-RECAP-FIN
      *SINON
      *    commande passée date -24 mois et premiere commande
           ELSE IF WS-ISFIRST-OUI AND YEARS-COM < 2
               PERFORM INIT-INFO-CLI-DEB
                  THRU INIT-INFO-CLI-FIN
               PERFORM WRITE-SP-TAB-DEB
                  THRU WRITE-SP-TAB-FIN
               PERFORM SUM-TOTAL-DEB
                  THRU SUM-TOTAL-FIN
               PERFORM WRITE-LINE-CMD-DEB
                  THRU WRITE-LINE-CMD-FIN
                  SET WS-ISFIRST-NON      TO TRUE
           END-IF.
      *    lecture de la prochaine commande
           PERFORM READ-F-CMD-DEB
              THRU READ-F-CMD-FIN.
      *---------------------------------------------------------------*
      *FIN DU TRAITEMENT BOUCLE
      *---------------------------------------------------------------*
       TRT-CMD-FIN.
           EXIT.
      *---------------------------------------------------------------*
      *TRAITEMENT DES MOUVEMENTS TRANSFERTS CALCULS ET IMPRESSIONS
      *---------------------------------------------------------------*
      ******************************6000*******************************
      *****************************************************************
      *---------------------------------------------------------------*
      *            ORDRES DE MANIPULATION DES FICHIERS                *
      *---------------------------------------------------------------*
      *****************************************************************
      *    OUVERTURE DES FICHIERS
      *****************************************************************
       OV-CLI-DEB.
           OPEN INPUT FIC-R1.
           IF NOT FS-F-CLI = '00'
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER CLIENT'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-CLI
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       OV-CLI-FIN.
           EXIT.
      *
       OV-COM-DEB.
           OPEN INPUT FIC-R2.
           IF NOT FS-F-COM = '00'
              DISPLAY 'PROBLEME D''OUVERTURE DU FICHIER COMMANDE'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-COM
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       OV-COM-FIN.
           EXIT.
      *
       OV-P1-DEB.
           OPEN INPUT CARTE-P1.
           IF NOT FS-F-P1 = '00'
              DISPLAY 'PROBLEME D''OUVERTURE CARTE P1'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-P1
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       OV-P1-FIN.
           EXIT.
      *
       OV-P2-DEB.
           OPEN INPUT CARTE-P2.
           IF NOT FS-F-P2 = '00'
              DISPLAY 'PROBLEME D''OUVERTURE CARTE P2'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-P2
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       OV-P2-FIN.
           EXIT.
      *
       OV-P3-DEB.
           OPEN INPUT CARTE-P3.
           IF NOT FS-F-P3 = '00'
              DISPLAY 'PROBLEME D''OUVERTURE CARTE P3'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-P3
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       OV-P3-FIN.
           EXIT.
      *
       OV-W1-DEB.
           OPEN OUTPUT FIC-W1.
           IF NOT FS-F-W1 = '00'
              DISPLAY 'PROBLEME D''OUVERTURE FICHIER W1'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W1
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       OV-W1-FIN.
           EXIT.
      *
       OV-W2-DEB.
           OPEN OUTPUT FIC-W2.
           IF NOT FS-F-W2 = '00'
              DISPLAY 'PROBLEME D''OUVERTURE FICHIER W2'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W2
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       OV-W2-FIN.
           EXIT.
      *
       OV-W3-DEB.
           OPEN OUTPUT FIC-W3.
           IF NOT FS-F-W3 = '00'
              DISPLAY 'PROBLEME D''OUVERTURE FICHIER W3'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W3
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       OV-W3-FIN.
           EXIT.
      *
       OV-W4-DEB.
           OPEN OUTPUT FIC-W4.
           IF NOT FS-F-W4 = '00'
              DISPLAY 'PROBLEME D''OUVERTURE FICHIER W4'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W4
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       OV-W4-FIN.
           EXIT.
      *****************************************************************
      *    LECTURE DES FICHIERS
      *****************************************************************
      ***************************LECTURE P2***************************
       READ-P2-DEB.
           READ CARTE-P2 NEXT
              INTO  WS-ENRG-REM
           END-READ.
           IF NOT (FS-F-P2 = '00' OR '10')
              DISPLAY 'PROBLEME DE LECTURE CARTE P2'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-P2
              PERFORM ERREUR-PROG-DEB
                 THRU ERREUR-PROG-FIN
           END-IF.
       READ-P2-FIN.
           EXIT.
      *************************LECTURE R1******************************
       READ-F-CLI-DEB.
           READ FIC-R1 INTO WS-ENRG-CLI
              AT END move 'Y' to EOF-1.
       READ-F-CLI-FIN.
           EXIT.
      *************************LECTURE R2******************************
       READ-F-CMD-DEB.
           READ FIC-R2 INTO WS-ENRG-COM
               AT END move 'Y' to EOF-2.
       READ-F-CMD-FIN.
           EXIT.
      *
      *****************************************************************
      *    FERMETURE DES FICHIERS
      *****************************************************************
       FE-CLI-DEB.
           CLOSE FIC-R1.
           IF NOT FS-F-CLI = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER CLIENT'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-CLI
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       FE-CLI-FIN.
           EXIT.
      *
       FE-COM-DEB.
           CLOSE FIC-R2.
           IF NOT FS-F-COM = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER COMMANDE'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-COM
              PERFORM ERREUR-PROG-DEB THRU ERREUR-PROG-FIN
           END-IF.
       FE-COM-FIN.
           EXIT.
      *
       FE-P1-DEB.
           CLOSE CARTE-P1.
           IF NOT FS-F-P1 = '00'
              DISPLAY 'PROBLEME DE FERMETURE CARTE P1'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-P1
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       FE-P1-FIN.
           EXIT.
      *
       FE-P2-DEB.
           CLOSE CARTE-P2.
           IF NOT FS-F-P2 = '00'
              DISPLAY 'PROBLEME DE FERMETURE CARTE P2'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-P2
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       FE-P2-FIN.
           EXIT.
      *
       FE-P3-DEB.
           CLOSE CARTE-P3.
           IF NOT FS-F-P3 = '00'
              DISPLAY 'PROBLEME DE FERMETURE CARTE P3'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-P3
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       FE-P3-FIN.
           EXIT.
      *
       FE-W1-DEB.
           CLOSE FIC-W1.
           IF NOT FS-F-W1 = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER W1'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W1
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       FE-W1-FIN.
           EXIT.
      *
       FE-W2-DEB.
           CLOSE FIC-W2.
           IF NOT FS-F-W2 = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER W2'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W2
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       FE-W2-FIN.
           EXIT.
      *
       FE-W3-DEB.
           CLOSE FIC-W3.
           IF NOT FS-F-W3 = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER W3'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W3
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       FE-W3-FIN.
           EXIT.
      *
       FE-W4-DEB.
           CLOSE FIC-W4.
           IF NOT FS-F-W4 = '00'
              DISPLAY 'PROBLEME DE FERMETURE DU FICHIER W4'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W4
              PERFORM ERREUR-PROG-DEB  THRU ERREUR-PROG-FIN
           END-IF.
       FE-W4-FIN.
           EXIT.
      ****************************************************************
      *              ECRITURE FICHIER W1                             *
      ****************************************************************
       WRITE-W1-DEB.
           WRITE ENRG-INFOCPT FROM WS-ENRG-INFCOMPT.
           ADD 1 TO COMPTEUR-CLI.
           IF NOT FS-F-W1 = '00'
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER INFCO'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W1
              PERFORM ERREUR-PROG-DEB
                 THRU ERREUR-PROG-FIN
           END-IF.
       WRITE-W1-FIN.
           EXIT.
      *
       WRITE-W1-SAUT-DEB.
           WRITE ENRG-INFOCPT FROM WS-ENRG-INFCOMPT
           ADD 1 TO COMPTEUR-CLI.
           IF NOT FS-F-W1 = '00'
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER INFCO'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W1
              PERFORM ERREUR-PROG-DEB
                 THRU ERREUR-PROG-FIN
           END-IF.
       WRITE-W1-SAUT-FIN.
           EXIT.
      ****************************************************************
      *                      ECRITURE FICHIER W2                     *
      ****************************************************************
       WRITE-W2-DEB.
           WRITE ENRG-FICERROR FROM WS-FICERROR
           ADD 1 TO COMPTEUR-ERR.
           DISPLAY 'ECRITURE DANS FICHIER ERR : ' ENRG-FICERROR
           IF NOT FS-F-W2 = '00'
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER COMMANDE RECAP'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W2
              PERFORM ERREUR-PROG-DEB
                 THRU ERREUR-PROG-FIN
           END-IF.
       WRITE-W2-FIN.
           EXIT.
      *
       WRITE-W2-SAUT-DEB.
           WRITE ENRG-FICERROR FROM WS-FICERROR
           AFTER ADVANCING 1 LINE.
           ADD 1 TO COMPTEUR-ERR.
           IF NOT FS-F-W2 = '00'
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER COMMANDE RECAP'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W2
              PERFORM ERREUR-PROG-DEB
                 THRU ERREUR-PROG-FIN
           END-IF.
       WRITE-W2-SAUT-FIN.
           EXIT.
      ****************************************************************
      *              ECRITURE FICHIER W3 AFTER PAGE/PAGE             *
      ****************************************************************
       WRITE-W3-AFTER-DEB.
           WRITE ENRG-LISTCO AFTER PAGE
           IF NOT FS-F-W3 ='00'
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER COMMANDE RECAP'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W3
              PERFORM ERREUR-PROG-DEB
                 THRU ERREUR-PROG-FIN
           END-IF.
       WRITE-W3-AFTER-FIN.
           EXIT.
      *
       WRITE-W3-DEB.
           WRITE ENRG-LISTCO
           AFTER ADVANCING 1 LINE.
           IF NOT FS-F-W3 ='00'
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER COMMANDE RECAP'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W3
              PERFORM ERREUR-PROG-DEB
                 THRU ERREUR-PROG-FIN
           END-IF.
       WRITE-W3-FIN.
           EXIT.
      *
      ****************************************************************
      *                   ECRITURE FICHIER W4 PAGE                   *
      ****************************************************************
       WRITE-W4-LINE-DEB.
           WRITE ENRG-REMISES
           IF NOT FS-F-W4 = '00'
              DISPLAY 'PROBLEME D''ECRITURE DU FICHIER REMISE'
              DISPLAY 'VALEUR DU FILE STATUS = ' FS-F-W4
              PERFORM ERREUR-PROG-DEB
                 THRU ERREUR-PROG-FIN
           END-IF.
       WRITE-W4-LINE-FIN.
           EXIT.
      *
      ******************************7000*******************************
      *****************************************************************
      *---------------------------------------------------------------*
      *              TRANSFERTS ET CALCULS COMPLEXES                  *
      *---------------------------------------------------------------*
      *    NOM CONDITION & VARIABLE ASSORTIMENT
      *
      *
       VAR-ASRTM-DEB.
           MOVE WS-NUM-CLI        TO       WS-CLI-IN.
           MOVE WS-KEY-COM        TO       WS-CMD-IN.
           MOVE WS-CIV-CLI        TO       FS-COD-SX.
           SET  WS-MILLE-NON      TO       TRUE.
           SET  WS-CMD-24-NON     TO       TRUE.
           SET  WS-CLI-24-NON     TO       TRUE.
           SET  WS-ISFIRST-OUI    TO       TRUE.
           MOVE ZERO              TO       WS-TOT-CMD.
           MOVE ZERO              TO       WS-TOT-ART.
           MOVE ZERO              TO       WS-NB-CMD.
           MOVE WS-CIV-CLI        TO       WS-GENDER-CODE.
       VAR-ASRTM-FIN.
           EXIT.
      *
       DATE-CLI-DEB.
           COMPUTE  NBDAYS-3 =
           FUNCTION INTEGER-OF-DATE(WS-INS-CLI).
           COMPUTE  DIFFERENCE-CLI = NBDAYS-1 - NBDAYS-3.
           COMPUTE  YEARS-CLI = DIFFERENCE-CLI / 365.
       DATE-CLI-FIN.
           EXIT.
      *
       DATE-CMD-DEB.
           COMPUTE  NBDAYS-2 =
           FUNCTION INTEGER-OF-DATE(WS-DAY-COM).
           COMPUTE  DIFFERENCE-DAY = NBDAYS-1 - NBDAYS-2.
           COMPUTE  YEARS-COM = DIFFERENCE-DAY / 365.
       DATE-CMD-FIN.
           EXIT.
      *
       DATEJ-IS-DEB.
           READ CARTE-P1 INTO WS-DATE-J.
           COMPUTE NBDAYS-1 = FUNCTION INTEGER-OF-DATE(WS-DATE-J).
           ACCEPT WS-DATE-US FROM DATE YYYYMMDD.
           MOVE SS                TO       SS-FR.
           MOVE AA                TO       AA-FR.
           MOVE MM-EN             TO       MM-FR.
           MOVE DD-EN             TO       DD-FR.
           MOVE WS-DATE-FR        TO       WS-DATED.
       DATEJ-IS-FIN.
           EXIT.
      *
       CLIENT-IN-DEB.
           MOVE WS-NUM-CLI        TO       WS-CLI-IN.
       CLIENT-IN-FIN.
           EXIT.
      *
       CMD-IN-DEB.
           MOVE WS-KEY-COM     TO       WS-CMD-IN.
       CMD-IN-FIN.
      *
       SUM-TOTAL-DEB.
           COMPUTE WS-TOT-ART =  WS-PRI-COM * WS-QTY-COM.
           COMPUTE WS-TOT-CMD =  WS-TOT-CMD + WS-TOT-ART.
           ADD 1                  TO       WS-CLI-NB-CMD
           MOVE WS-TOT-ART        TO       WS-TOTPRIX.
           MOVE WS-DAY-COM        TO       WS-DTCOM.
           MOVE WS-LIB-COM        TO       WS-LIBART.
           MOVE WS-ART-COM        TO       WS-NUMART.
           MOVE WS-PRI-COM        TO       WS-PUART.
           MOVE WS-QTY-COM        TO       WS-QTYART.
           IF  WS-TOT-CMD >= 1000
               SET WS-MILLE-OUI   TO       TRUE
           END-IF.
       SUM-TOTAL-FIN.
      *
      *
       INIT-W1-DEB.
           MOVE WS-NUM-CLI        TO       WS-CPT-CLI.
           MOVE WS-NOM-CLI        TO       WS-CPT-NOM.
           MOVE WS-PRE-CLI        TO       WS-CPT-PRE.
           MOVE WS-CLI-NB-CMD     TO       WS-CPT-COM.
           MOVE WS-TOT-CMD        TO       WS-CPT-TOT.
       INIT-W1-FIN.
           EXIT.
      *
       INIT-INFO-CLI-DEB.
           MOVE WS-NOM-CLI        TO       WS-NOM.
           MOVE LIBEL (WS-GENDER-CODE)
                                  TO       WS-CIV.
           MOVE WS-NUM-CLI        TO       WS-NUMC.
           MOVE WS-DATE-FR        TO       WS-DTJ.
       INIT-INFO-CLI-FIN.
           EXIT.
      *
      ******************************8000*******************************
      *****************************************************************
      *---------------------------------------------------------------*
      *                     VARIABLES D'EDITIONS                      *
      *---------------------------------------------------------------*
       WRITE-SP-TAB-DEB.
           MOVE WS-BLANK          TO       ENRG-LISTCO.
           PERFORM WRITE-W3-DEB
              THRU WRITE-W3-FIN.
           MOVE WS-LCPTE          TO       ENRG-LISTCO.
           PERFORM WRITE-W3-AFTER-DEB
              THRU WRITE-W3-AFTER-FIN.
           MOVE WS-TITLE          TO       ENRG-LISTCO.
           PERFORM WRITE-W3-DEB
              THRU WRITE-W3-FIN.
           MOVE WS-TABVID         TO       ENRG-LISTCO.
           PERFORM WRITE-W3-DEB
              THRU WRITE-W3-FIN.
       WRITE-SP-TAB-FIN.
           EXIT.
      *
       WRITE-CONTINUE-DEB.
           MOVE WS-BLANK          TO       ENRG-LISTCO.
           PERFORM WRITE-W3-DEB
              THRU WRITE-W3-FIN.
           MOVE WS-BLANK          TO       ENRG-LISTCO.
           PERFORM WRITE-W3-AFTER-DEB
              THRU WRITE-W3-AFTER-FIN.
           MOVE WS-TITLE          TO       ENRG-LISTCO.
           PERFORM WRITE-W3-DEB
              THRU WRITE-W3-FIN.
           MOVE WS-TABVID         TO       ENRG-LISTCO.
           PERFORM WRITE-W3-DEB
              THRU WRITE-W3-FIN.
       WRITE-CONTINUE-FIN.
           EXIT.
      *
       WRITE-FIN-TAB-DEB.
           MOVE WS-TABVID         TO       ENRG-LISTCO.
           PERFORM WRITE-W3-DEB
              THRU WRITE-W3-FIN.
           MOVE WS-TOTLIGNE       TO       ENRG-LISTCO.
           PERFORM WRITE-W3-DEB
              THRU WRITE-W3-FIN.
       WRITE-FIN-TAB-FIN.
           EXIT.
      *
       WRITE-LINE-CMD-DEB.
           MOVE WS-ACHAT          TO       ENRG-LISTCO.
           PERFORM WRITE-W3-DEB
              THRU WRITE-W3-FIN.
       WRITE-LINE-CMD-FIN.
      *
       WRITE-CMD-RECAP-DEB.
           IF WS-NB-CMD > 60
               PERFORM WRITE-CONTINUE-DEB
                  THRU WRITE-CONTINUE-FIN
           MOVE ZERO   TO WS-NB-CMD
           END-IF.
           PERFORM WRITE-LINE-CMD-DEB  THRU WRITE-LINE-CMD-FIN.
           ADD 1             TO       WS-NB-CMD.
       WRITE-CMD-RECAP-FIN.
           EXIT.
      *
       WRITE-W4-DEB.
           MOVE WS-NOM-CLI       TO        WS-NOMED
               COMPUTE YEARS = DIFFERENCE-DAY / 365
               MOVE YEARS        TO        WS-ANNEED
                                           WS-ANNEEDB
               MOVE WS-TAB-LIB   TO        WS-LIBLED
                                           WS-LIBLEDA
                                           WS-LIBLEDB
               MOVE WS-TAB-REM   TO        WS-REMED
               MOVE LIBEL (WS-GENDER-CODE)
                                 TO        WS-CIVED.
           MOVE WS-STARS         TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           MOVE WS-TIRED         TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           MOVE WS-LIGNE1        TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           MOVE WS-VIDED         TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           MOVE WS-LIGNE2        TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           MOVE WS-VIDED         TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           MOVE WS-TIRED         TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           IF YEARS <= 0
               MOVE WS-LIGNE3A        TO       ENRG-REMISES
               PERFORM WRITE-W4-LINE-DEB
                  THRU WRITE-W4-LINE-FIN
           ELSE IF YEARS = 1
               MOVE WS-LIGNE3B        TO       ENRG-REMISES
               PERFORM WRITE-W4-LINE-DEB
                  THRU WRITE-W4-LINE-FIN
           ELSE
               MOVE WS-LIGNE3         TO       ENRG-REMISES
               PERFORM WRITE-W4-LINE-DEB
                  THRU WRITE-W4-LINE-FIN
           END-IF.
           MOVE WS-LIGNE4        TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           MOVE WS-TIRED         TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           MOVE WS-VIDED         TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           MOVE WS-LIGNE5        TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
           MOVE WS-TIRED         TO        ENRG-REMISES.
           PERFORM WRITE-W4-LINE-DEB
              THRU WRITE-W4-LINE-FIN.
       WRITE-W4-FIN.
           EXIT.
      *
       EXIT.
      *
      *8999-STATISTIQUES-DEB.
      *
      *     DISPLAY '************************************************'
      *     DISPLAY '*     STATISTIQUES DU PROGRAMME XXXXXXXX       *'
      *     DISPLAY '*     ==================================       *'
      *     DISPLAY '************************************************'.
      *
      *****************************************************************
      *****************************************************************
      *****************************************************************
      *---------------------------------------------------------------*
      *                  PROTECTION FIN DE PROGRAMME                  *
      *---------------------------------------------------------------*
      *
       FIN-PROG-DEB.
            DISPLAY '*==============================================*'.
            DISPLAY '*     FIN NORMALE DU PROGRAMME REMCLI          *'.
            DISPLAY '*==============================================*'.
       FIN-PROG-FIN.
            STOP RUN.
            EXIT.
      *
       ERREUR-PROG-DEB.
      *
            DISPLAY '*==============================================*'.
            DISPLAY '*        UNE ANOMALIE A ETE DETECTEE           *'.
            DISPLAY '*     FIN ANORMALE DU PROGRAMME REMCLI         *'.
            DISPLAY '*==============================================*'.
            MOVE 12 TO RETURN-CODE.
      *
       ERREUR-PROG-FIN.
            STOP RUN.
       END PROGRAM REMCLI.
