*&---------------------------------------------------------------------*
*& Report  ZGARAGINO_KUNDEN_OEFFNUNGSZEIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*der Name und die Länge (Format)
REPORT zgaragino_kunden_oeffnungszeit NO STANDARD PAGE HEADING  LINE-SIZE 237.
*LINE-COUNT 200(8)
*Variablen festlegen

TABLES: kna1,
        zpapp_opentimes.
*interne Tabellen erstellen mit den werten von..
* gt_ = Globale Programminterne Tabelle
* wa_
DATA  : gt_kna1            TYPE TABLE OF kna1,
        gt_zpapp_opentimes TYPE TABLE OF zpapp_opentimes,
        wa_zpapp_opentimes TYPE zpapp_opentimes.


TYPES: BEGIN OF _zgaragino_kot1,
         kunnr TYPE char10,
         name1 TYPE name1_gp,
         mo    TYPE char40,
         di    TYPE char40,
         mi    TYPE char40,
         do    TYPE char40,
         fr    TYPE char40,
         sa    TYPE char40,
         so    TYPE char40,
       END OF _zgaragino_kot1.

DATA: wa_opentime TYPE _zgaragino_kot1.
DATA: gt_opentime TYPE TABLE OF _zgaragino_kot1.
*interne tabelle erstellt mit Werten von:
FIELD-SYMBOLS:
  <fs_kna1>            TYPE kna1,
  <fs_zpapp_opentimes> TYPE zpapp_opentimes.
*Mehrfachauswahl von bis
SELECT-OPTIONS: s_kunnr FOR kna1-kunnr.
*SELECT-OPTIONS: s_REGIO FOR kna1-REGIO.
* parameter eigabe
*PARAMETERS: s_name1.
*OBLIGATORY DEFAULT '6'.
PARAMETERS: p_datei AS CHECKBOX DEFAULT 'X'.

*--------------------------------------------------------------------------------------
* Main Programm
*--------------------------------------------------------------------------------------
START-OF-SELECTION.
*cleant gt_kna1 zum sicherstellen das wirklich nichts drin ist.
  CLEAR gt_kna1.
  CLEAR gt_zpapp_opentimes.

  PERFORM daten_beschaffen.
  PERFORM daten_ausgeben_screen.
  IF p_datei = 'X'.
    PERFORM daten_ausgeben_file.
  ENDIF.

*--------------------------------------------------------------------------------------
* REPORT TOP (Header oder Überschriften)
*--------------------------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM top_of_page.


FORM daten_beschaffen.
*    wähle alles von der Tabelle kna1 in die Tabelle gt_kna1 mit er selectionsoption kundennummer von-bis
  SELECT * FROM kna1 INTO TABLE gt_kna1 WHERE kunnr IN s_kunnr.
*     AND zzgaragino_active ='X'.

*     für Kunden Namen eingabe und Zeit ausgabe
* SELECT * FROM kna1 INTO TABLE gt_kna1 WHERE name1 IN s_name1
*     AND zzgaragino_active ='X'.

*  SELECT * FROM kna1 INTO TABLE gt_kna1 WHERE REGIO IN s_REGIO.


*wähle alles von der Tabelle zpapp_opentimes in die Tabelle gt_zpapp_opentimes.
*wo die kundennummer ist gefragt in gt_kna1-kunnr.
  SELECT * FROM zpapp_opentimes INTO TABLE gt_zpapp_opentimes
     FOR ALL ENTRIES IN gt_kna1
   WHERE kunnr = gt_kna1-kunnr.

  CLEAR: gt_opentime[].
  LOOP AT gt_kna1 ASSIGNING <fs_kna1>.
    CLEAR: wa_opentime.

    wa_opentime-kunnr = <fs_kna1>-kunnr.
*   Führende Nullen wegmachen...
    SHIFT wa_opentime-kunnr LEFT DELETING LEADING '0'.

    wa_opentime-name1 = <fs_kna1>-name1.

*   Wochentage abfüllen...
    PERFORM save_wochentag USING 'mo' 'mo'.
    PERFORM save_wochentag USING 'di' 'tu'.
    PERFORM save_wochentag USING 'mi' 'we'.
    PERFORM save_wochentag USING 'do' 'th'.
    PERFORM save_wochentag USING 'fr' 'fr'.
    PERFORM save_wochentag USING 'sa' 'sa'.
    PERFORM save_wochentag USING 'so' 'su'.

*   Daten in Tabelle ablegen...
    APPEND wa_opentime TO gt_opentime.
  ENDLOOP.
ENDFORM.

FORM daten_ausgeben_screen.
  DATA: gv_outlen TYPE i.
  gv_outlen = 26.

  LOOP AT gt_opentime INTO wa_opentime.

    IF ( sy-tabix MOD 2 = 1 ).

      FORMAT COLOR COL_HEADING.
    ELSE.
      FORMAT COLOR COL_NORMAL.
    ENDIF.

    WRITE: / sy-vline NO-GAP, wa_opentime-kunnr NO-GAP,
    sy-vline NO-GAP, wa_opentime-name1 NO-GAP,
    sy-vline NO-GAP, wa_opentime-mo(gv_outlen) NO-GAP,
    sy-vline NO-GAP, wa_opentime-di(gv_outlen) NO-GAP,
    sy-vline NO-GAP, wa_opentime-mi(gv_outlen) NO-GAP,
    sy-vline NO-GAP, wa_opentime-do(gv_outlen) NO-GAP,
    sy-vline NO-GAP, wa_opentime-fr(gv_outlen) NO-GAP,
    sy-vline NO-GAP, wa_opentime-sa(gv_outlen) NO-GAP,
    sy-vline NO-GAP, wa_opentime-so(gv_outlen) NO-GAP,
    sy-vline NO-GAP.

  ENDLOOP.

ENDFORM.
FORM daten_ausgeben_file.
  DATA: lv_filename TYPE string,
        lv_path     TYPE string,
        lv_fullpath TYPE string.
*        lv_sap_codepage type  cpcodepage,
*        lv_ABAP_ENCODING type ABAP_ENCODING.
  DATA: lt_csv TYPE truxs_t_text_data.
  DATA: wa_csv TYPE LINE OF truxs_t_text_data.
  WRITE: / 'Die Tabelle wurde in den Ihren festgelegten Pfad als csv. Datei hinterlegt.'.
*  Jetzt Filename ermitteln...
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
*     window_title              =
      default_extension         = 'csv'
      default_file_name         = 'OpenTime'
      file_filter               = '*.csv'
      initial_directory         = 'c:\temp\'
    CHANGING
      filename                  = lv_filename
      path                      = lv_path
      fullpath                  = lv_fullpath
*     user_action               =
*     file_encoding             =
    EXCEPTIONS
      cntl_error                = 1
      error_no_gui              = 2
      not_supported_by_gui      = 3
      invalid_default_file_name = 4
      OTHERS                    = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
    EXPORTING
      i_field_seperator    = ';'
    TABLES
      i_tab_sap_data       = gt_opentime
    CHANGING
      i_tab_converted_data = lt_csv
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*Weil es oben nicht so einfach geht, muss hier noch eine Headerzeile eingefügt
  CONCATENATE " Kommentar Test
  'KunNr'
  'Name'
  'Montag'
  'Dienstag'
  'Mittwoch'
  'Donnerstag'
  'Freitag'
  'Samstag'
  'Sonntag' INTO wa_csv SEPARATED BY ';'.
  INSERT wa_csv INTO lt_csv INDEX 1.



  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
*     bin_filesize            =
      filename                = lv_fullpath
*     codepage                = lv_ABAP_ENCODING
    CHANGING
      data_tab                = lt_csv
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

FORM save_wochentag USING lv_wtag_de TYPE zzcdel_app_wochentag lv_wtag_en TYPE zzcdel_app_wochentag.
  DATA: lv_output TYPE char40.
  DATA: lv_von TYPE char05.
  DATA: lv_bis TYPE char05.

  CLEAR: lv_output.
  READ TABLE gt_zpapp_opentimes
       INTO wa_zpapp_opentimes
       WITH KEY kunnr = <fs_kna1>-kunnr
                wochentag = lv_wtag_de.
  IF sy-subrc <> 0. "nicht gefunden
    READ TABLE gt_zpapp_opentimes
     INTO wa_zpapp_opentimes
     WITH KEY kunnr = <fs_kna1>-kunnr
              wochentag = lv_wtag_en.
  ENDIF.
*--------------------------------------------------------------------------------------------------------
  IF sy-subrc = 0. "Gefunden
    IF wa_zpapp_opentimes-durchgehend = 'X'.
      CONCATENATE wa_zpapp_opentimes-morgen_von+0(2)
            ':'
            wa_zpapp_opentimes-morgen_von+2(2) INTO lv_von.

      CONCATENATE wa_zpapp_opentimes-mittag_bis+0(2)
                  ':'
                  wa_zpapp_opentimes-mittag_bis+2(2) INTO lv_bis.
      CONCATENATE lv_von 'durgehend bis' lv_bis INTO lv_output SEPARATED BY space.
*------------------------------------------------------------------------------------------------------

    ELSEIF wa_zpapp_opentimes-geschlossen = 'X'.
      lv_output = '(Geschlossen)'.
    ELSE.
*--------------------------------------------------------------------------------------------------------
*      PERFORM show_normal  USING p_pos lw_zpapp_opentimes.
      CONCATENATE wa_zpapp_opentimes-morgen_von+0(2)
                  ':'
                  wa_zpapp_opentimes-morgen_von+2(2)
                  '-'
                  wa_zpapp_opentimes-morgen_bis+0(2)
                  ':'
                  wa_zpapp_opentimes-morgen_bis+2(2)
                  ' - '
                  wa_zpapp_opentimes-mittag_von+0(2)
                  ':'
                  wa_zpapp_opentimes-mittag_von+2(2)
                  '-'
                  wa_zpapp_opentimes-mittag_bis+0(2)
                  ':'
                  wa_zpapp_opentimes-mittag_bis+2(2)

                  INTO lv_output RESPECTING BLANKS.
*----------------------------------------------------------------------------------------------------
    ENDIF.
  ELSE.   "Nicht gefunden
    lv_output = '(keine Angaben)'.
  ENDIF.
*  ------------------------------------------------------------------------------------------------

* richtiges Speichern der Werte...
  CASE lv_wtag_de.
    WHEN 'mo'.
      wa_opentime-mo = lv_output.
    WHEN 'di'.
      wa_opentime-di = lv_output.
    WHEN 'mi'.
      wa_opentime-mi = lv_output.
    WHEN 'do'.
      wa_opentime-do = lv_output.
    WHEN 'fr'.
      wa_opentime-fr = lv_output.
    WHEN 'sa'.
      wa_opentime-sa = lv_output.
    WHEN 'so'.
      wa_opentime-so = lv_output.

    WHEN OTHERS.
      WRITE: / 'Leider nicht vorhanden - ', lv_wtag_de.
  ENDCASE.

ENDFORM.


FORM top_of_page.
  DATA: tmptext(80) TYPE c.

*  CONCATENATE
*       ztitel0 ztitel '/' zrtitel INTO tmptext SEPARATED BY space.
  tmptext = syst-cprog.
  ULINE.
  WRITE: sy-vline NO-GAP,
         AT 2(15) 'ESA Burgdorf'.
*  WRITE: AT 17(48) tmptext CENTERED.
*  write: at 16(48) text-051 centered.

  WRITE:
         AT 83(5)  text-992 RIGHT-JUSTIFIED,
         AT 211(10) syst-datum USING EDIT MASK '__.__.____' ,
         AT 221(9)  text-991 RIGHT-JUSTIFIED,
         AT 233(3)  syst-pagno RIGHT-JUSTIFIED,
         237 sy-vline NO-GAP.
  ULINE.

* Zeile 1
  WRITE: /
         sy-vline NO-GAP, (10) 'K.Nummer' NO-GAP,
         sy-vline NO-GAP, (35) 'Name' NO-GAP,
         sy-vline NO-GAP, (26)  'Montag' NO-GAP,
         sy-vline NO-GAP, (26) 'Dienstag' NO-GAP,
         sy-vline NO-GAP, (26) 'Mittwoch' NO-GAP,
         sy-vline NO-GAP, (26) 'Donnerstag' NO-GAP,
         sy-vline NO-GAP, (26) 'Freitag' NO-GAP,
         sy-vline NO-GAP, (26) 'Samstag' NO-GAP,
         sy-vline NO-GAP, (26) 'Sonntag' NO-GAP,
         sy-vline NO-GAP.

  ULINE.

ENDFORM.
*--------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------------