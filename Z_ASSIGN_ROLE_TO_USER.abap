*&---------------------------------------------------------------------*
*& Report  Z_ASSIGN_ROLE_TO_USER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  Z_ASSIGN_ROLE_TO_USER.
INCLUDE <list>.
TYPE-POOLS: line, rs.
*"=============================================================================
*" Fortschrittsanzeige
*"=============================================================================
DEFINE progress.
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = &1
      text       = &2.
END-OF-DEFINITION.
*"=============================================================================
*" Benutzerstamm-Abgleich
*"=============================================================================
DEFINE rh_auth_update.

  data ret_tab type bapirettab.

*  call function 'PRGN_ACTIVITY_GROUP_USERPROF'
*    exporting
*      activity_group   = &1
*      display_messages = space.
*
  CALL FUNCTION 'PRGN_ACTIVITY_GROUP_USERPROF'
    EXPORTING
      activity_group                      = &1
*     EXPERT_MODE_WITHOUT_UPDATE          = ' '
*     HR_MODE                             = ' '
*     ACTION_INSERT                       = 'X'
*     ACTION_DELETE                       = 'X'
     DISPLAY_MESSAGES                    = ' '
*     DELETE_INVALID_PROFILES             = 'X'
     ONLY_IMPORTANT_MESSAGES             = 'X'
*     ONLY_DISTRIBUTE_USERS               = ' '
*   TABLES
*     DEL_TAB                             = DEL_TAB
*     INS_TAB                             = INS_TAB
   EXCEPTIONS
     NO_AUTHORITY_FOR_USER_COMPARE       = 1
     AT_LEAST_ONE_USER_ENQUEUED          = 2
     AUTHORITY_INCOMPLETE                = 3
     NO_PROFILES_AVAILABLE               = 4
     TOO_MANY_PROFILES_IN_USER           = 5
     CHILD_AGR_ENQUEUED                  = 6
     OTHERS                              = 7
            .
  IF sy-subrc <> 0.
    BREAK-POINT.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


END-OF-DEFINITION.                    "RH_AUTH_UPDATE
*"-----------------------------------------------------------------------------
*" DATA
*"-----------------------------------------------------------------------------
DATA: clin TYPE tab512,
      ctab TYPE STANDARD TABLE OF tab512,
      lv_sfilename TYPE string,
      lt_agrs TYPE TABLE OF str_agrs,
      ls_agrs TYPE str_agrs,
      lv_datab(10) TYPE c,
      lv_datbis(10) TYPE c,
      lv_year(4) TYPE c,
      lv_month(2) TYPE c,
      lv_day(2) TYPE c,
      lt_users TYPE TABLE OF syuname,
      ls_users TYPE syuname,
      lt_temp TYPE TABLE OF str_agrs,
      lv_maxcnt TYPE i,
      lv_faktor TYPE f,
      lv_perc TYPE i,
      lv_statustext TYPE string,
      lt_not_authorized TYPE TABLE OF str_agrs,
      lt_enqueued TYPE TABLE OF str_agrs,
      lt_not_existing TYPE TABLE OF str_agrs,
      lt_it_bapiret2 TYPE TABLE OF bapiret2,
      lv_output TYPE string,
      lt_roles TYPE TABLE OF agr_name,
      ls_role TYPE agr_name
      .
*"-----------------------------------------------------------------------------
*" SELECTION-SCREEN 0001
*"-----------------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF SCREEN 0001 TITLE t_screen.
SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE t_blk3.
"-- Auswahl zuordnen/entziehen --
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_add RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 3(30) p6 FOR FIELD p_add.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rem RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 3(30) p7 FOR FIELD p_rem.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
"-- Automatischer Stammabgleich --
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: pa_updau AS CHECKBOX DEFAULT rs_c_true.
SELECTION-SCREEN COMMENT 3(79) p8 FOR FIELD pa_updau.
SELECTION-SCREEN END OF LINE.
"-- Create Changelog --
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_clog AS CHECKBOX DEFAULT rs_c_true USER-COMMAND flag.
SELECTION-SCREEN COMMENT 3(79) p9 FOR FIELD p_clog.
SELECTION-SCREEN END OF LINE.
"-- Unbekannte Benutzer anlegen --
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_create AS CHECKBOX DEFAULT rs_c_false USER-COMMAND flag.
SELECTION-SCREEN COMMENT 3(79) p4 FOR FIELD p_create.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk3.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE t_blk1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(79) c2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(79) c3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
"-- Dateiname --
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) p1 FOR FIELD p_filenm.
PARAMETERS: p_filenm TYPE bds_uri.
SELECTION-SCREEN END OF LINE.
"-- Kopfzeilen --
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) p2 FOR FIELD p_headln.
PARAMETERS: p_headln TYPE c LENGTH 2 DEFAULT 1.
SELECTION-SCREEN END OF LINE.
"-- Trennzeichen --
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) p3 FOR FIELD p_seprtr.
PARAMETERS: p_seprtr TYPE c LENGTH 1 DEFAULT ';'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE t_blk2.
SELECTION-SCREEN SKIP.
"-- Initialkennwort --
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) p5 FOR FIELD p_initpw MODIF ID bl2.
PARAMETERS: p_initpw(40) TYPE c LOWER CASE MODIF ID bl2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk2.
SELECTION-SCREEN END OF SCREEN 0001.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" ON EXIT-COMMAND
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AT SELECTION-SCREEN ON EXIT-COMMAND.
  EXIT.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" Check obligatory fields
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" -- Filename --
AT SELECTION-SCREEN ON p_filenm.
  IF p_filenm IS INITIAL AND sy-ucomm EQ 'CRET'.
    MESSAGE 'Bitte alle Mussfelder angeben' TYPE 'E'.
  ENDIF.
*" -- Seperator --
AT SELECTION-SCREEN ON p_seprtr.
  IF p_seprtr IS INITIAL AND sy-ucomm EQ 'CRET'.
    MESSAGE 'Bitte alle Mussfelder angeben' TYPE 'E'.
  ENDIF.
*" -- Headline --
AT SELECTION-SCREEN ON p_headln.
  IF p_headln IS INITIAL AND sy-ucomm EQ 'CRET'.
    MESSAGE 'Bitte alle Mussfelder angeben' TYPE 'E'.
  ENDIF.
*" -- Password --
AT SELECTION-SCREEN ON p_initpw.
  IF p_create EQ rs_c_true AND p_initpw IS INITIAL AND sy-ucomm EQ 'CRET'.
    MESSAGE 'Bitte alle Mussfelder angeben' TYPE 'E'.
  ENDIF.
  IF p_create EQ rs_c_true AND p_initpw IS NOT INITIAL AND sy-ucomm EQ 'CRET'.
    DATA: lv_subrc LIKE sy-subrc,
          lv_msgid LIKE sy-msgid,
          lv_msgno LIKE sy-msgno,
          lv_msgv1 LIKE sy-msgv1,
          lv_msgv2 LIKE sy-msgv2,
          lv_msgv3 LIKE sy-msgv3,
          lv_msgv4 LIKE sy-msgv4,
          lv_pwd LIKE bapipwd.

    lv_pwd = p_initpw.
    CALL FUNCTION 'PASSWORD_FORMAL_CHECK'
      EXPORTING
        password                   = lv_pwd
*       DOWNWARDS_COMPATIBLE       = ' '
      IMPORTING
        rc                         = lv_subrc
        msgid                      = lv_msgid
        msgno                      = lv_msgno
        msgv1                      = lv_msgv1
        msgv2                      = lv_msgv2
        msgv3                      = lv_msgv3
        msgv4                      = lv_msgv4
*       MSGTEXT                    =
*     EXCEPTIONS
*       INTERNAL_ERROR             = 1
*       OTHERS                     = 2
              .

    IF lv_subrc NE 0.
      MESSAGE ID lv_msgid TYPE 'E' NUMBER lv_msgno WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4.
    ENDIF.

  ENDIF.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" Hide block
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_create EQ rs_c_false AND screen-group1 = 'BL2'.
      screen-active = '0'.
      CLEAR p_initpw.
    ENDIF.
    IF screen-name = 'P_INITPW'.
      screen-invisible = '1'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" Value-Help for filename
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filenm.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = p_filenm
      mask             = ',*.csv.'
      mode             = 'O'
      title            = 'Upload File'(078)
    IMPORTING
      filename         = p_filenm
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
*"-----------------------------------------------------------------------------
*" INITIALIZATION
*"-----------------------------------------------------------------------------
INITIALIZATION.
  t_screen = 'Benutzer-Rollen-Zuordnung'.
  t_blk1 = 'CSV-Upload'.
  t_blk2 = 'Benutzer-Einstellungen'.
  t_blk3 = 'Aktion'.
*  c1 = 'Dieses Programm ordnet aus einer CSV-Datei Rollen den Benutzern zu.'.
  c2 = 'CSV-Format: [USER];[ROLLE];[GUELTIG_VON];[GUELTIG_BIS].'.
  c3 = 'Datums-Format: TT.MM.YYYY'.
  p1 = 'CSV-Datei'.
  p2 = 'Kopfzeilen'.
  p3 = 'Trennzeichen'.
  p4 = 'Nicht existierende Benutzer anlegen'.
  p5 = 'Initialpasswort'.
  p6 = 'Rollen zuordnen'.
  p7 = 'Rollen entziehen'.
  p8 = 'Automatischer Benutzerstammabgleich'.
  p9 = 'Changlog pflegen'.

  CALL SCREEN 0001.
*"-----------------------------------------------------------------------------
*" START-OF-SELECTION
*"-----------------------------------------------------------------------------
START-OF-SELECTION.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" CSV-FILE UPLOAD
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*  CLEAR p_filenm.

  IF p_filenm IS INITIAL OR p_seprtr IS INITIAL OR p_headln IS INITIAL OR ( p_create EQ rs_c_true AND p_initpw IS INITIAL ).
    EXIT.
*    MESSAGE 'Bitte alle Mussfelder angeben' TYPE 'E'.
*    CALL SCREEN 0001.
  ENDIF.

  CONCATENATE '' p_filenm INTO lv_sfilename.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_sfilename
      no_auth_check           = 'X'
    TABLES
      data_tab                = ctab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" CSV-FILE PARSING
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  LOOP AT ctab INTO clin.
    CHECK sy-tabix > p_headln. " Skip Headlines

    SPLIT clin AT p_seprtr INTO ls_agrs-uname
                                ls_agrs-agr_name
                                lv_datab
                                lv_datbis
                                .

*"  Transform DATVON
    SPLIT lv_datab AT '.' INTO  lv_day lv_month lv_year.
    CONCATENATE lv_year lv_month lv_day INTO ls_agrs-from_dat.
*"  Transform DATBIS
    SPLIT lv_datbis AT '.' INTO  lv_day lv_month lv_year.
    CONCATENATE lv_year lv_month lv_day INTO ls_agrs-to_dat.
*"  Fill tables
    APPEND ls_agrs TO lt_agrs.
    ls_users = ls_agrs-uname.
    TRANSLATE ls_users TO UPPER CASE.
    ls_role = ls_agrs-agr_name.
    TRANSLATE ls_role TO UPPER CASE.
    APPEND ls_role TO lt_roles.
    APPEND ls_users TO lt_users.
  ENDLOOP.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" Delete Duplicates
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SORT lt_agrs BY uname agr_name.
  SORT lt_users.
  SORT lt_roles.
  DELETE ADJACENT DUPLICATES FROM lt_users.
  DELETE ADJACENT DUPLICATES FROM lt_roles.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" Init progress view
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  DESCRIBE TABLE lt_users LINES lv_maxcnt.
  lv_faktor = 100 / lv_maxcnt.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" Start assignment
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  LOOP AT lt_users INTO ls_users.
    lv_perc = lv_faktor * sy-tabix.
    CONCATENATE 'Updating user ' ls_users '...' INTO lv_statustext RESPECTING BLANKS.
    progress lv_perc lv_statustext.
    CLEAR lt_temp.
    CLEAR ls_agrs.
    CLEAR lt_not_authorized.
    CLEAR lt_enqueued.
    CLEAR lt_not_existing.
    CLEAR lt_it_bapiret2.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" Get AUHTS of current user
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    LOOP AT lt_agrs INTO ls_agrs WHERE uname = ls_users.
      APPEND ls_agrs TO lt_temp.
    ENDLOOP.

    DATA: ls_bapipwd TYPE bapipwd,
          ls_bapilogond TYPE bapilogond,
          ls_adress TYPE bapiaddr3
          .
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" Create non-existing users
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    IF p_create EQ rs_c_true.
*" Check first if user already exists
      CALL FUNCTION 'SUSR_USER_CHECK_EXISTENCE'
        EXPORTING
          user_name            = ls_users
        EXCEPTIONS
          user_name_not_exists = 1
          OTHERS               = 2.

      IF sy-subrc EQ 1.
        ls_bapipwd-bapipwd = p_initpw.
        ls_bapilogond-gltgv = '20110626'.
        ls_bapilogond-gltgb = '99991231'.
        ls_bapilogond-ustyp = 'A'.
        ls_adress-lastname = ls_users.

        CALL FUNCTION 'BAPI_USER_CREATE1'
          EXPORTING
            username  = ls_users
            logondata = ls_bapilogond
            password  = ls_bapipwd
            address   = ls_adress.
      ENDIF.
    ENDIF.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" Assign roles to user
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    PERFORM assign_role_to_user
                TABLES
                   lt_temp
                USING
                   ls_users
                   p_clog
                   p_rem
                CHANGING
                   sy-subrc.

*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" ... and show the status
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    IF sy-subrc NE 0.
      CASE sy-subrc.
        WHEN 1. CONCATENATE ls_users ': User gesperrt.' INTO lv_output.
        WHEN 2. CONCATENATE ls_users ': Rollen gesperrt.' INTO lv_output.
        WHEN 3. CONCATENATE ls_users ': Einige Rollen existieren nicht.' INTO lv_output.
        WHEN 4. CONCATENATE ls_users ': Fehler aufgetreten.' INTO lv_output.
      ENDCASE.

      WRITE AT 3 icon_led_red AS ICON. WRITE AT 8 lv_output.
      sy-subrc = 0.
    ELSE.
      CONCATENATE ls_users ': Erfolgreich zugeordnet.' INTO lv_output.
      WRITE AT 3 icon_led_green AS ICON. WRITE AT 8 lv_output.
    ENDIF.
    NEW-LINE.
  ENDLOOP.

*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" Benutzerstammabgleich
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  DESCRIBE TABLE lt_roles LINES lv_maxcnt.
  lv_faktor = 100 / lv_maxcnt.

  IF pa_updau EQ rs_c_true.
    LOOP AT lt_roles INTO ls_role.
      lv_perc = lv_faktor * sy-tabix.
      CONCATENATE 'Benutzerstammabgleich für Rolle ' ls_role '...' INTO lv_statustext RESPECTING BLANKS.
      progress lv_perc lv_statustext.

      rh_auth_update ls_role.
    ENDLOOP.
  ENDIF.
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*" END-OF-SELECTION
*"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  CREATE_CHANGELOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LS_AGRUSERS  text
*----------------------------------------------------------------------*
FORM create_changelog USING ls_agrusers TYPE agr_users
                            lv_logtype TYPE c.
  DATA: lt_xagr_agrs  LIKE TABLE OF vagr_agrs,
        lt_yagr_agrs  LIKE TABLE OF vagr_agrs,
        lt_xagr_buffi  LIKE TABLE OF vagr_buffi,
        lt_yagr_buffi  LIKE TABLE OF vagr_buffi,
        lt_xagr_flags  LIKE TABLE OF vagr_flags,
        lt_yagr_flags  LIKE TABLE OF vagr_flags,
        lt_xagr_hpage  LIKE TABLE OF vagr_hpage,
        lt_yagr_hpage  LIKE TABLE OF vagr_hpage,
        lt_xagr_mapp  LIKE TABLE OF vagr_mapp,
        lt_yagr_mapp  LIKE TABLE OF vagr_mapp,
        lt_xagr_obj  LIKE TABLE OF vagr_obj,
        lt_yagr_obj  LIKE TABLE OF vagr_obj,
        lt_xagr_texts  LIKE TABLE OF vagr_texts,
        lt_yagr_texts  LIKE TABLE OF vagr_texts,
        lt_xagr_users  LIKE TABLE OF vagr_users,
        lt_yagr_users  LIKE TABLE OF vagr_users,
        lt_xcd1016  LIKE TABLE OF vcd1016,
        lt_ycd1016  LIKE TABLE OF vcd1016,
        lt_xcd1251  LIKE TABLE OF vcd1251,
        lt_ycd1251  LIKE TABLE OF vcd1251,
        lt_xcd1252  LIKE TABLE OF vcd1252,
        lt_ycd1252  LIKE TABLE OF vcd1252,
        ls_vagr_users LIKE vagr_users,
        ls_n_agr_define TYPE AGR_DEFINE,
        ls_o_agr_define TYPE AGR_DEFINE,
        lv_obj_id TYPE CDHDR-OBJECTID.

  MOVE: ls_agrusers TO ls_vagr_users.

  ls_vagr_users-change_dat = sy-datum.
  ls_vagr_users-change_tim = sy-uzeit.
  ls_vagr_users-change_tst = 0.
  APPEND ls_vagr_users TO lt_xagr_users.

  lv_obj_id = ls_agrusers-agr_name.

  CALL FUNCTION 'PFCG_WRITE_DOCUMENT'
    EXPORTING
      objectid                      = lv_obj_id
      tcode                         = ''
      utime                         = sy-uzeit
      udate                         = sy-datum
      username                      = sy-uname
*         PLANNED_CHANGE_NUMBER         = ' '
      object_change_indicator       = space
*         PLANNED_OR_REAL_CHANGES       = ' '
*         NO_CHANGE_POINTERS            = ' '
*         UPD_AGR_AGRS                  = ' '
*         UPD_AGR_BUFFI                 = ' '
      n_agr_define                  = ls_n_agr_define
      o_agr_define                  = ls_o_agr_define
*         UPD_AGR_DEFINE                = ' '
*         UPD_AGR_FLAGS                 = ' '
*         UPD_AGR_HPAGE                 = ' '
*         UPD_AGR_MAPP                  = ' '
*         UPD_AGR_OBJ                   = ' '
*         UPD_AGR_TEXTS                 = ' '
          upd_agr_users                 = lv_logtype
*         UPD_CD1016                    = ' '
*         UPD_CD1251                    = ' '
*         UPD_CD1252                    = ' '
    TABLES
      xagr_agrs                     = lt_xagr_agrs
      yagr_agrs                     = lt_yagr_agrs
      xagr_buffi                    = lt_xagr_buffi
      yagr_buffi                    = lt_yagr_buffi
      xagr_flags                    = lt_xagr_flags
      yagr_flags                    = lt_yagr_flags
      xagr_hpage                    = lt_xagr_hpage
      yagr_hpage                    = lt_yagr_hpage
      xagr_mapp                     = lt_xagr_mapp
      yagr_mapp                     = lt_yagr_mapp
      xagr_obj                      = lt_xagr_obj
      yagr_obj                      = lt_yagr_obj
      xagr_texts                    = lt_xagr_texts
      yagr_texts                    = lt_yagr_texts
      xagr_users                    = lt_xagr_users
      yagr_users                    = lt_yagr_users
      xcd1016                       = lt_xcd1016
      ycd1016                       = lt_ycd1016
      xcd1251                       = lt_xcd1251
      ycd1251                       = lt_ycd1251
      xcd1252                       = lt_xcd1252
      ycd1252                       = lt_ycd1252
            .

ENDFORM.                    "CREATE_CHANGELOG

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ROLE_TO_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_AGRS    text
*      -->IV_USER    text
*      -->IV_LOG     text
*      -->IV_DEL     text
*----------------------------------------------------------------------*
FORM assign_role_to_user  TABLES    it_agrs TYPE smum_agr_users
                          USING     iv_user TYPE xubname
                                    iv_log  TYPE rs_bool
                                    iv_del  TYPE rs_bool
                          CHANGING  cv_rc  LIKE sy-subrc.
  .

  DATA: ls_agrusers TYPE agr_users,
        lt_agrusers TYPE TABLE OF agr_users,
        ls_stragrs TYPE str_agrs,
        lt_filter TYPE TABLE OF agr_name,
        lt_return TYPE TABLE OF bapiret2,
        lv_logtype LIKE smensapnew-customized
        .
*"---------------------------------------------------------
*" Rollen-Benutzerzuordnung vorbereiten
*"---------------------------------------------------------
  IF iv_del EQ abap_true.
*" Rollenzuordnungen werden gelöscht
*    LOOP AT it_agrs INTO ls_stragrs.
*      APPEND ls_stragrs-AGR_NAME TO lt_filter.
*    ENDLOOP.

*    SORT lt_filter.
*    DELETE ADJACENT DUPLICATES FROM lt_filter.

    SELECT * FROM agr_users INTO TABLE lt_agrusers FOR ALL ENTRIES IN it_agrs WHERE uname EQ iv_user AND agr_name EQ it_agrs-agr_name.
  ELSE.
*" Rollenzuordnungen werden erzeugt
    LOOP AT it_agrs INTO ls_stragrs.
      CLEAR ls_agrusers.

      MOVE-CORRESPONDING ls_stragrs TO ls_agrusers.
      ls_agrusers-mandt        = sy-mandt.
*    ls_agrusers-AGR_NAME     = ''.
      ls_agrusers-uname        = iv_user.
*    ls_agrusers-FROM_DAT     = ''.
*    ls_agrusers-TO_DAT       = ''.
      ls_agrusers-exclude      = space.
      ls_agrusers-change_dat   = sy-datum.
      ls_agrusers-change_tim   = sy-uzeit.
*    ls_agrusers-CHANGE_TST   = ''.
*    ls_agrusers-ORG_FLAG     = ''.
*    ls_agrusers-COL_FLAG     = ''.

      APPEND ls_agrusers TO lt_agrusers.
    ENDLOOP.

    SORT lt_agrusers BY mandt uname agr_name from_dat to_dat DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_agrusers COMPARING mandt uname agr_name.
  ENDIF.
*"---------------------------------------------------------
*" User sperren
*"---------------------------------------------------------
  CALL FUNCTION 'SUSR_USER_ENQUEUE'
    EXPORTING
      bname          = iv_user
      repeat_enqueue = 'X'
    TABLES
      return         = lt_return.

  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
*" User konnte nicht gesperrt werden -> abbruch
    cv_rc = 1.
    EXIT.
  ENDIF.
*"---------------------------------------------------------
*" Rollen sperren
*"---------------------------------------------------------
  LOOP AT lt_agrusers INTO ls_agrusers.
    CALL FUNCTION 'PRGN_ACTIVITY_GROUP_ENQUEUE'
      EXPORTING
        activity_group          = ls_agrusers-agr_name
      EXCEPTIONS
        foreign_lock            = 1
        transport_check_problem = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
*" Rolle konnte nicht gesperrt werden
      cv_rc = 2.
      DELETE lt_agrusers FROM ls_agrusers.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDLOOP.
*"---------------------------------------------------------
*" Rollen zuordnen/löschen
*"---------------------------------------------------------
  IF iv_del EQ abap_true.
    DELETE agr_users FROM TABLE lt_agrusers.
  ELSE.
    MODIFY agr_users FROM TABLE lt_agrusers.
  ENDIF.

  IF sy-subrc EQ 0 AND iv_log EQ abap_true.
    IF iv_del EQ abap_true.
      lv_logtype = 'D'.
    ELSE.
      lv_logtype = 'I'.
    ENDIF.

    LOOP AT lt_agrusers INTO ls_agrusers.
      perform create_changelog
                  using
                     ls_agrusers
                     lv_logtype.
    ENDLOOP.
  ELSE.
*" Fehler aufgetreten
    cv_rc = 4.
  ENDIF.
*"---------------------------------------------------------
*" Rollen entsperren
*"---------------------------------------------------------
  LOOP AT lt_agrusers INTO ls_agrusers.
    TRY.
    CALL FUNCTION 'PRGN_ACTIVITY_GROUP_DEQUEUE'
      EXPORTING
        activity_group = ls_agrusers-agr_name.
    CATCH cx_root.
    ENDTRY.
  ENDLOOP.
*"---------------------------------------------------------
*" User entsperren
*"---------------------------------------------------------
  CALL FUNCTION 'SUSR_USER_DEQUEUE'
    EXPORTING
     bname           = iv_user
*   ENQ_MODE        = 'E'
*   ENQ_SCOPE       = '1'
    TABLES
*   USERS           =
      return          = lt_return
            .


ENDFORM.                    "ASSIGN_ROLE_TO_USER
