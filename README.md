# SYNTH_-Sales-Order-Approval-and-Email-Notifications-
Sales Order Approval and Email Notifications

Data Definitions 

@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order Approval Check'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_SO_Approval_Check
  as select from zsd_so_wf_log
{
  key vbeln as vbeln,
      zdctp as Doctype,
      zsts1 as Zsts1,
      zsts2 as Zsts2,
      zsts3 as Zsts3
}
//where
//  (
//       zsts1 = 'REJECTED'
//    or zsts2 = 'REJECTED'
//    or zsts3 = 'REJECTED'
//  )
************************************************************************************************************************************

Database Tables

@EndUserText.label : 'Maintain Sales Order Approvals'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zsd_approvals {

  key cilent         : mandt not null;
  key vkorg          : vkorg not null;
  key auart          : zzauart not null;
  key zlevelapproval : zzlevelapproval not null;
  key zusername      : zzusername not null;
  key zemailid       : zzemailid not null;
  zwatermark         : zzwatermark;

}
**************************************************************************************************************************

@EndUserText.label : 'SD_Feed: Approval Matrix'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zsd_sales_appmtr {

  key mandt : mandt not null;
  key werks : werks_d not null;
  key zsdtp : zsdtp not null;
  zusrl     : zusrl;
  zuid1     : zuid1;
  zuid2     : zuid2;
  zuid3     : zuid3;
  zuid4     : zuid4;
  zuid5     : zuid5;

}
***********************************************************************************************************************************

@EndUserText.label : 'Sales Work Flow Log'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zsd_so_wf_log {

  key client : mandt not null;
  key zwfid  : zzwfid not null;
  key vbeln  : vbeln not null;
  key zdctp  : zzdctp not null;
  key vbtyp  : vbtyp not null;
  key ernam  : ernam not null;
  key erdat  : erdat not null;
  zlevel     : zzlevel;
  zuid1      : zzuid1;
  zsts1      : zzsts1;
  zaprtm1    : uzeit;
  zaprdt1    : zzaprdt1;
  zaprre1    : zzaprre1;
  zuid2      : zzuid2;
  zaprtm2    : uzeit;
  zaprdt2    : zzaprdt1;
  zaprre2    : zzaprre1;
  zsts2      : zzsts2;
  zuid3      : zzuid3;
  zaprtm3    : uzeit;
  zaprdt3    : zzaprdt1;
  zaprre3    : zzaprre1;
  zsts3      : zzsts3;

}
**********************************************************************************************************************************

@EndUserText.label : 'Sales Work flow Log'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zsd_wflog {

  key client : mandt not null;
  key zwfid  : zzwfid not null;
  key vbeln  : vbeln_vl not null;
  key zdctp  : zzdctp not null;
  key vbtyp  : vbtyp not null;
  key ernam  : ernam not null;
  key erdat  : erdat not null;
  zlevel     : zzlevel;
  zuid1      : zzuid1;
  zsts1      : zzsts1;
  zaprdt1    : zzaprdt1;
  zaprre1    : zzaprre1;
  zuid2      : zzuid2;
  zsts2      : zzsts1;
  zaprdt2    : zzaprdt1;
  zaprre2    : zzaprre1;
  zuid3      : zzuid3;
  zsts3      : zzsts1;
  zaprdt3    : zzaprdt1;
  zaprre3    : zzaprre1;
  zuid4      : zzuid4;
  zsts4      : zzsts1;
  zaprdt4    : zzaprdt1;
  zaprre4    : zzaprre1;
  zuid5      : zzuid5;
  zsts5      : zzsts1;
  zaprdt5    : zzaprdt1;
  zaprre5    : zzaprre1;

}
********************************************************************************************************************************************************

Strutures

@EndUserText.label : 'Item Details'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
define structure zsd_matcode {

  matnr  : matnr;
  posnr  : posnr_va;
  @Semantics.quantity.unitOfMeasure : 'vbap.vrkme'
  kwmwng : kwmeng;

}
***************************************************************************************************************************************

@EndUserText.label : 'Sales Order Approval Structure'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
define structure zsd_s_so_appr {

  user  : zuid1;
  mail  : ad_smtpadr;
  level : zusrl;
  flag  : char1;

}
*******************************************************************************************************

@EndUserText.label : 'Sales Order Workflow Details'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
define structure zswfdetails {

  salesorder         : vbeln;
  salesorg           : vkorg;
  saledoctype        : auart;
  initiator          : ernam;
  rejectedfrom       : char2;
  @Semantics.amount.currencyCode : 'vbak.waerk'
  newnetvalue        : netwr_ak;
  newportofloading   : zz1_prtld;
  newportofunloading : zz1_prtuld;
  newpaymentterms    : dzterm;
  newincoterms       : inco1;
  oldportofloading   : zz1_prtld;
  oldportofunloading : zz1_prtuld;
  oldpaymentterms    : dzterm;
  oldincoterms       : inco1;
  @Semantics.amount.currencyCode : 'vbak.waerk'
  oldnetvalue        : netwr_ak;
  soldtoparty        : kunnr;
  customercode       : kunnr;
  material           : matnr;
  @Semantics.quantity.unitOfMeasure : 'vbrp.meins'
  quantity           : menge_d;
  quantityunit       : meins;
  @Semantics.amount.currencyCode : 'vbrk.waerk'
  valueofmaterial    : netwr;
  payment            : char250;
  @Semantics.quantity.unitOfMeasure : 'vbap.vrkme'
  new_qty            : kwmeng;
  @Semantics.quantity.unitOfMeasure : 'vbap.vrkme'
  old_qty            : kwmeng;

}
****************************************************************************************************************************

Classes 

class ZCL_EI_SO_CHECK_BEFORESAVE definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_SD_SLS_CHECK_BEFORE_SAVE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_EI_SO_CHECK_BEFORESAVE IMPLEMENTATION.


  METHOD if_sd_sls_check_before_save~check_document.
    CHECK sy-tcode = 'VA02' AND ( salesdocument-salesdocumenttype = 'ZDSF' OR
                                  salesdocument-salesdocumenttype = 'ZESF' OR
                                  salesdocument-salesdocumenttype = 'ZRSF' OR
                                  salesdocument-salesdocumenttype = 'ZMSF' OR
                                  salesdocument-salesdocumenttype = 'ZUSF' OR
                                  salesdocument-salesdocumenttype = 'ZSCR' OR
                                  salesdocument-salesdocumenttype = 'ZSEZ' OR
                                  salesdocument-salesdocumenttype = 'ZZSF' OR
                                  salesdocument-salesdocumenttype = 'ZEXP' OR
                                  salesdocument-salesdocumenttype = 'ZDOM' OR
                                  salesdocument-salesdocumenttype = 'ZEOU' OR
                                  salesdocument-salesdocumenttype = 'ZMER' OR
                                  salesdocument-salesdocumenttype = 'ZRMP' ).




    SELECT SINGLE vbeln,
                  zsts1,
                  zsts2,
                  zsts3 FROM zi_so_approval_check INTO  @DATA(checkapproval)
      WHERE vbeln = @salesdocument-salesdocument AND doctype = @salesdocument-salesdocumenttype.

    IF ( checkapproval-zsts1 = 'PENDING' OR checkapproval-zsts2 = 'PENDING' OR checkapproval-zsts3 = 'PENDING' ).
*    ELSE.
      APPEND VALUE #( messagetext = TEXT-001 && | | && COND #( WHEN checkapproval-zsts3 IS NOT INITIAL
                                                               THEN 'L3'
                                                               WHEN checkapproval-zsts2 IS NOT INITIAL
                                                               THEN 'L2'
                                                               WHEN checkapproval-zsts1 IS NOT INITIAL
                                                               THEN 'L1' ) && | | && TEXT-002
                      messagetype = 'E' ) TO messages.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
**************************************************************************************************************************************

CLASS zcl_global_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_t_email_ids TYPE TABLE OF ad_smtpadr .
    TYPES:
      BEGIN OF ty_mailinfo,
        message_body   TYPE  bcsy_text,
        formoutput     TYPE  fpformoutput,
        toemailaddress TYPE  ad_smtpadr,
        sender_mail    TYPE  ad_smtpadr,
        subject        TYPE  so_obj_des,
        pdfname        TYPE  string,
      END OF ty_mailinfo .
    TYPES:
      BEGIN OF ty_text,
        textid  TYPE  thead-tdid,
        name    TYPE  thead-tdname,
        textobj TYPE  thead-tdobject,
      END OF ty_text .
    TYPES:
      tt_line TYPE  TABLE OF tline WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_tvarvc_values,
        sign   TYPE    ddsign,
        option TYPE    ddoption,
        low    TYPE    rvari_vnam,
        high   TYPE    rvari_vnam,
      END OF ty_tvarvc_values .
    TYPES:
      tt_tvarvc_values TYPE STANDARD TABLE OF ty_tvarvc_values .
    TYPES:
      BEGIN OF ty_tab,
        field1 TYPE    string,
        field2 TYPE    string,
      END OF ty_tab .
    TYPES:
      tt_tab TYPE STANDARD TABLE OF ty_tab .
    TYPES:
      BEGIN OF ty_s_email_ids,
        email_id TYPE ad_smtpadr,
      END  OF ty_s_email_ids .

*  types:
*    ty_t_email_ids TYPE TABLE OF ad_smtpadr .
    CONSTANTS gc_object TYPE tdobjectgr VALUE 'GRAPHICS' ##NO_TEXT.
    CONSTANTS gc_logo TYPE tdobname VALUE 'ZZ_DEVI_LOGO' ##NO_TEXT.
    CONSTANTS gc_id TYPE tdidgr VALUE 'BMAP' ##NO_TEXT.
    CONSTANTS gc_btype TYPE tdbtype VALUE 'BCOL' ##NO_TEXT.
    CLASS-DATA iv_formname TYPE fpname .
    CLASS-DATA iv_device TYPE rspopname VALUE 'LP01' ##NO_TEXT.
    CLASS-DATA iv_skip TYPE char1 .
    CLASS-DATA iv_preview TYPE fppreview VALUE 'X' ##NO_TEXT.
    CLASS-DATA iv_call_obj_type TYPE char1 .
    CLASS-DATA iv_download TYPE char1 .
    CLASS-DATA iv_dgsign TYPE char1 .
    CLASS-DATA et_return TYPE bapiret2_t .
    CLASS-DATA es_formoutput TYPE fpformoutput .
    CONSTANTS gc_mailsent_successfully TYPE string VALUE 'Mail Sent Successfully' ##NO_TEXT.
    CONSTANTS gc_failedtosendmail TYPE string VALUE 'Failed To Send Mail' ##NO_TEXT.

    CLASS-METHODS get_tvarvc
      IMPORTING
        !iv_name        TYPE rvari_vnam
        !iv_type        TYPE rsscr_kind
        !iv_low         TYPE rvari_val_255 OPTIONAL
      EXPORTING
        !et_values      TYPE tt_tvarvc_values
      RETURNING
        VALUE(rv_value) TYPE rvari_val_255 .
    CLASS-METHODS get_logo
      IMPORTING
        !iv_name        TYPE tdobname
      RETURNING
        VALUE(rv_image) TYPE xstring .
    METHODS lock_unlock
      IMPORTING
        !iv_lock        TYPE char1
        !iv_tabname     TYPE rstable-tabname
      RETURNING
        VALUE(rv_subrc) TYPE sy-subrc .
    CLASS-METHODS job_open
      IMPORTING
        !iv_skip         TYPE char1 OPTIONAL
      EXPORTING
        !es_outputparams TYPE sfpoutputparams .
    CLASS-METHODS job_close .
    CLASS-METHODS email_with_attachment
      IMPORTING
        !is_mailinfo     TYPE ty_mailinfo
        !it_email_ids_to TYPE ty_t_email_ids
        !iv_pdfflag      TYPE char1 OPTIONAL
      EXPORTING
        !ev_success_flag TYPE boolean
        !ev_message      TYPE string .
    CLASS-METHODS f4_help
      IMPORTING
        VALUE(iv_retfield)    TYPE dfies-fieldname
        VALUE(iv_dynprofield) TYPE help_info-dynprofld
        VALUE(it_value_tab)   TYPE tt_tab .
    CLASS-METHODS amt_in_words
      IMPORTING
        !iv_amt_in_num   TYPE pc207-betrg
      EXPORTING
        !ev_amt_in_words TYPE c
      EXCEPTIONS
        data_type_mismatch .
    CLASS-METHODS read_text
      IMPORTING
        !iv_textid  TYPE thead-tdid
        !iv_name    TYPE thead-tdname
        !iv_textobj TYPE thead-tdobject
      EXPORTING
        !ev_text    TYPE string .
    CLASS-METHODS read_text_tab
      IMPORTING
        !is_textdetails TYPE ty_text
      RETURNING
        VALUE(rt_text)  TYPE tt_line .
    CLASS-METHODS sendemail
      IMPORTING
        !iv_email_sender  TYPE ad_smtpadr OPTIONAL
        !iv_email_subject TYPE so_obj_des OPTIONAL
        !it_email_ids_to  TYPE ty_t_email_ids OPTIONAL
        !it_email_body    TYPE bcsy_text OPTIONAL
        !it_email_ids_cc  TYPE ty_t_email_ids OPTIONAL
      EXPORTING
        !ev_sucess_flag   TYPE boolean
        !ev_message       TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GLOBAL_UTILITIES IMPLEMENTATION.


  METHOD amt_in_words.
**==> Call function for converting amount in words
    CALL FUNCTION 'HR_IN_CHG_INR_WRDS'
      EXPORTING
        amt_in_num         = iv_amt_in_num
      IMPORTING
        amt_in_words       = ev_amt_in_words
      EXCEPTIONS
        data_type_mismatch = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
    ELSE.
**==>If any error/exception occurs during the function module
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD email_with_attachment.
*==>Data Declaration for Mail
    DATA : lo_send_request  TYPE REF TO cl_bcs,
           lo_document      TYPE REF TO cl_document_bcs,
           lo_recipient     TYPE REF TO if_recipient_bcs,
           lo_document_bcs  TYPE REF TO cx_document_bcs,
           lo_bcs_exception TYPE REF TO cx_bcs,
           lv_sent_to_all   TYPE os_boolean,
           lv_pdf_size      TYPE so_obj_len,
           lv_objdes        TYPE sood-objdes,
           lv_pdf_file      TYPE fpformoutput,
           lv_pdf_content   TYPE solix_tab,
           lv_obj_des       TYPE so_obj_des,
           lv_email_to      TYPE ad_smtpadr.

*==>Constant Declaration
    CONSTANTS: lc_objtp  TYPE soodk-objtp    VALUE 'PDF',
               lc_obj_tp TYPE so_obj_tp      VALUE 'HTM'.

    TRY.
*==>Create Send request
        lo_send_request = cl_bcs=>create_persistent(  ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        ev_message = lx_send_req_bcs->get_text( ).
    ENDTRY.

    TRY.
*==>Set sender to send request
        DATA(lo_sender) = cl_cam_address_bcs=>create_internet_address( is_mailinfo-sender_mail ).
        lo_send_request->set_sender( i_sender = lo_sender ).

      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
      CATCH cx_send_req_bcs INTO DATA(lx_address_req).
        ev_message = lx_address_bcs->get_text( ).
    ENDTRY.
    TRY.
*        IF iv_pdfflag = abap_true. "if flag is X then only pdf will be sent in the mail
*==>PDF Name
        lv_objdes = is_mailinfo-pdfname.
*==>Mail Subject
        lv_obj_des = is_mailinfo-subject.
*==>add document
*==>get PDF xstring and convert it to BCS format
        lv_pdf_size = xstrlen( is_mailinfo-formoutput-pdf ).                     "Converting PDF to binary
        lv_pdf_content = cl_document_bcs=>xstring_to_solix( ip_xstring = is_mailinfo-formoutput-pdf ).
        lo_document = cl_document_bcs=>create_document( i_type    = lc_obj_tp
                                                        i_text    = is_mailinfo-message_body
                                                        i_length  = lv_pdf_size
                                                        i_subject = lv_obj_des ).
        TRY.
            lo_document->add_attachment(  i_attachment_type    = lc_objtp             "Adding PDF as atachment
                                          i_attachment_subject = lv_objdes
                                          i_att_content_hex    = lv_pdf_content ).
          CATCH cx_document_bcs INTO lo_document_bcs.
        ENDTRY.

        TRY.
*==Add document to send request
            lo_send_request->set_document( lo_document ).
          CATCH cx_address_bcs INTO lx_address_bcs.
          CATCH cx_send_req_bcs INTO lx_address_req.
        ENDTRY.
*        ENDIF.

        LOOP AT it_email_ids_to INTO DATA(ls_email_id_to).
          lv_email_to = ls_email_id_to.
          TRY.
*& Set recipient
              lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email_to ).
            CATCH cx_address_bcs INTO lx_address_bcs.
              ev_message = lx_address_bcs->get_text( ).
          ENDTRY.

          TRY.
              lo_send_request->add_recipient( EXPORTING i_recipient = lo_recipient i_express = abap_true ).
            CATCH cx_send_req_bcs INTO lx_send_req_bcs.
              ev_message = lx_send_req_bcs->get_text( ).
          ENDTRY.
        ENDLOOP.

*==>To get Recepient
****        lo_recipient = cl_cam_address_bcs=>create_internet_address( i_address_string = is_mailinfo-toemailaddress ).
****
*****==>add recipient to send request
****        lo_send_request->add_recipient( i_recipient = lo_recipient ).  " Sending request

*==>send document
        lo_send_request->set_send_immediately( i_send_immediately = abap_true ).
        lv_sent_to_all = lo_send_request->send( i_with_error_screen = abap_true ).
        IF lv_sent_to_all = abap_true.
          COMMIT WORK.
          ev_success_flag =  /isdfps/cl_const_abc_123=>gc_s.
          ev_message = gc_mailsent_successfully.
        ENDIF.
*==>exception handling
      CATCH cx_bcs INTO lo_bcs_exception.
        ev_success_flag =  /isdfps/cl_const_abc_123=>gc_e.
        ev_message = gc_failedtosendmail   .
    ENDTRY.
  ENDMETHOD.


  METHOD f4_help.
*==>Function Module for F4 help
    IF it_value_tab IS NOT INITIAL.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = iv_retfield
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          dynprofield     = iv_dynprofield
          value_org       = /isdfps/cl_const_abc_123=>gc_s "'S'
        TABLES
          value_tab       = it_value_tab
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.
*==>If any error/exception occurs during the function module
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_logo.
    CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
      EXPORTING
        p_object       = gc_object
        p_name         = iv_name
        p_id           = gc_id
        p_btype        = gc_btype
      RECEIVING
        p_bmp          = rv_image
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
  ENDMETHOD.


  METHOD get_tvarvc.
*==>Get the parameters data

    IF iv_type = /isdfps/cl_const_abc_123=>gc_p.

      SELECT SINGLE low FROM tvarvc
                        INTO @rv_value
                        WHERE name = @iv_name
                        AND type = @iv_type.
      IF sy-subrc = 0.
      ELSE.
      ENDIF.
*==>Get the select options data
    ELSEIF iv_type = /isdfps/cl_const_abc_123=>gc_s AND iv_low IS INITIAL.

      SELECT sign,opti,low,high FROM tvarvc
                                INTO TABLE @et_values
                                WHERE name = @iv_name
                                AND type = @iv_type.
      IF sy-subrc = 0.
      ELSE.
      ENDIF.
    ELSEIF iv_type = /isdfps/cl_const_abc_123=>gc_s AND iv_low IS NOT INITIAL.

      SELECT sign,opti,low,high FROM tvarvc
                           INTO TABLE @et_values
                           WHERE name = @iv_name
                           AND type = @iv_type
                           AND low = @iv_low.
      IF sy-subrc = 0.
      ELSE.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD job_close.
**==>Function Module to close Form
    CALL FUNCTION 'FP_JOB_CLOSE'
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc = 0.
    ELSE.
**==>If any error/exception occurs during the function module
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD job_open.

    DATA: gs_outputparams TYPE sfpoutputparams,
          gv_device       TYPE rspopname,
          gv_download     TYPE char1,
          gv_preview      TYPE fppreview.
*==>Preparing the Out Parameters
    SELECT SINGLE bname,spld FROM usr01 INTO @DATA(ls_usr01) WHERE bname = @sy-uname.
    IF NOT ls_usr01-spld IS INITIAL AND sy-subrc  = 0.
      gv_device = ls_usr01-spld.
    ENDIF.

    IF gv_download IS INITIAL AND gv_device IS NOT INITIAL .
      gs_outputparams-dest     = gv_device.
*      IF gv_preview IS NOT INITIAL.
      gs_outputparams-preview  = abap_true..
*      ELSE.
*        IF gv_device IS INITIAL.
*          gs_outputparams-nodialog = abap_true.
*          gs_outputparams-reqnew = abap_true.     "31.05.2024
*        ENDIF.
*        CLEAR gs_outputparams-preview.
*      ENDIF.
    ELSE.
      gs_outputparams-preview   = space.
*      gs_outputparams-nodialog  = abap_true.
*      gs_outputparams-getpdf    = abap_true.    "31.05.2024
    ENDIF.

    IF iv_skip = abap_true.
      gs_outputparams-preview   = space.
      gs_outputparams-nodialog  = abap_true.
      gs_outputparams-getpdf    = abap_true.
    ENDIF.
*==>Function module job open form
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = gs_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc = 0.
      es_outputparams = gs_outputparams.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.


  METHOD lock_unlock.
    IF iv_lock = /isdfps/cl_const_abc_123=>gc_l.
* Locking the Table
      CALL FUNCTION 'ENQUEUE_E_TABLE'
        EXPORTING
          mode_rstable   = /isdfps/cl_const_abc_123=>gc_e
          tabname        = iv_tabname
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      rv_subrc = sy-subrc.
    ELSEIF iv_lock = /isdfps/cl_const_abc_123=>gc_u.
* Unlocking the Table
      CALL FUNCTION 'DEQUEUE_E_TABLE'
        EXPORTING
          mode_rstable = /isdfps/cl_const_abc_123=>gc_e
          tabname      = iv_tabname.
      IF sy-subrc <> 0.     ##FM_SUBRC_OK
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      rv_subrc = sy-subrc.
    ENDIF.
  ENDMETHOD.


  METHOD read_text.
    DATA: lt_line TYPE TABLE OF tline.
**==>F.M. to read text fields
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = iv_textid
        language                = sy-langu
        name                    = iv_name
        object                  = iv_textobj
      TABLES
        lines                   = lt_line
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc = 0.
      LOOP AT lt_line ASSIGNING FIELD-SYMBOL(<fs_line>).
        IF sy-tabix = 1.
          ev_text = |{ <fs_line>-tdline }|.
        ELSE.
          ev_text = |{ ev_text }\n{ <fs_line>-tdline }|.
        ENDIF.
      ENDLOOP.
    ELSE.
*==>If any error/exception occurs during the function module
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD read_text_tab.
*==>F.M. to read text fields
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
*       CLIENT                  = SY-MANDT
        id                      = is_textdetails-textid
        language                = sy-langu
        name                    = is_textdetails-name
        object                  = is_textdetails-textobj
      TABLES
        lines                   = rt_text
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc = 0.
    ELSE.
    ENDIF.
  ENDMETHOD.


  METHOD sendemail.
    DATA:lo_send_request TYPE REF TO cl_bcs,
         lo_sender       TYPE REF TO if_sender_bcs,
         lo_recipient    TYPE REF TO if_recipient_bcs,
         lo_document     TYPE REF TO cl_document_bcs,
         lv_email_to     TYPE ad_smtpadr,
         lv_email_cc     TYPE ad_smtpadr.

    CLEAR:ev_sucess_flag,lv_email_to,lv_email_cc.

    TRY.
*** Create Send request
        lo_send_request = cl_bcs=>create_persistent(  ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        ev_message = lx_send_req_bcs->get_text( ).
    ENDTRY.

    TRY.

*** Set sender to send request
        lo_sender = cl_cam_address_bcs=>create_internet_address( iv_email_sender ).
        lo_send_request->set_sender( EXPORTING i_sender = lo_sender ).

      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
      CATCH cx_send_req_bcs INTO lx_send_req_bcs.
    ENDTRY.

*** Processing internal to fill multiple emails in to
    LOOP AT it_email_ids_to INTO DATA(ls_email_id_to).
      lv_email_to = ls_email_id_to.
      TRY.
*** Set recipient
          lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email_to ).
        CATCH cx_address_bcs INTO lx_address_bcs.
          ev_message = lx_address_bcs->get_text( ).
      ENDTRY.

      TRY.
          lo_send_request->add_recipient( EXPORTING i_recipient = lo_recipient i_express = 'X' ).
        CATCH cx_send_req_bcs INTO lx_send_req_bcs.
          ev_message = lx_send_req_bcs->get_text( ).
      ENDTRY.
    ENDLOOP.

*** Looping internal table to fill multiple emails to copy (CC).
    LOOP AT it_email_ids_cc INTO DATA(ls_email_id_cc).
      lv_email_cc = ls_email_id_cc.
      TRY.
*** Set recipient
          lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email_cc ).
        CATCH cx_address_bcs INTO lx_address_bcs.
      ENDTRY.

      TRY.
          lo_send_request->add_recipient( EXPORTING i_recipient = lo_recipient i_copy = 'X' i_express = 'X' ).
        CATCH cx_send_req_bcs INTO lx_send_req_bcs.
      ENDTRY.
    ENDLOOP. "  LOOP AT lt_cc_email_id INTO DATA(ls_cc_email_id).

    TRY.
*** Create document
        lo_document = cl_document_bcs=>create_document( i_type    = 'HTM'
                                                        i_text    = it_email_body
                                                        i_subject = iv_email_subject ).
      CATCH cx_document_bcs INTO DATA(lx_document_bcs).
        ev_message = lx_document_bcs->get_text( ).
    ENDTRY.

    TRY.
*** Set documents
        lo_send_request->set_document( lo_document ).
      CATCH cx_send_req_bcs INTO lx_send_req_bcs.
        ev_message = lx_send_req_bcs->get_text( ).
    ENDTRY.

    TRY.

*** Set immediate send
        CALL METHOD lo_send_request->set_send_immediately
          EXPORTING
            i_send_immediately = abap_true.

      CATCH cx_send_req_bcs INTO  lx_send_req_bcs.
        ev_message = lx_send_req_bcs->get_text( ).
    ENDTRY.

    TRY.

*** Send email
        lo_send_request->send( EXPORTING i_with_error_screen = 'X' ).
        WAIT UP TO 1 SECONDS.
        COMMIT WORK.
        WAIT UP TO 1 SECONDS.
        ev_message = TEXT-001.
        ev_sucess_flag = abap_true.

      CATCH cx_send_req_bcs INTO lx_send_req_bcs.
        ev_message = lx_send_req_bcs->get_text( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
***********************************************************************************************************************************

class ZCL_SD_001_SO_APPROVAL definition
  public
  final
  create public .

public section.

  interfaces BI_OBJECT .
  interfaces BI_PERSISTENT .
  interfaces IF_WORKFLOW .

  types:
*  types:
*    tt_email_ids TYPE STANDARD TABLE OF ad_smtpadr .
    ty_t_email_ids TYPE TABLE OF ad_smtpadr .
  types:
    ty_t_ccmail_ids TYPE TABLE OF ad_smtpadr .
  types:
    BEGIN OF ty_mailinfo,
        message_body   TYPE  bcsy_text,
        formoutput     TYPE  fpformoutput,
        toemailaddress TYPE  ad_smtpadr,
        ccemailaddress TYPE  ad_smtpadr,
        sender_mail    TYPE  ad_smtpadr,
        subject        TYPE  so_obj_des,
        pdfname        TYPE  string,
      END OF ty_mailinfo .
  types:
    tt_email_ids TYPE STANDARD TABLE OF ad_smtpadr .
  types:
    BEGIN OF ty_pending,
        vbeln TYPE vbak-vbeln,
        zdctp TYPE vbak-auart,
        ernam TYPE vbak-ernam,
        erdat TYPE vbak-erdat,
        zuid1 TYPE zsd_so_wf_log-zuid1,
        zuid2 TYPE zsd_so_wf_log-zuid2,
        zuid3 TYPE zsd_so_wf_log-zuid3,
      END OF ty_pending .
  types:
    BEGIN OF approval,
        vkorg          TYPE zsd_approvals-vkorg,
        auart          TYPE zsd_approvals-auart,
        zlevelapproval TYPE zsd_approvals-zlevelapproval,
        zusername      TYPE zsd_approvals-zusername,
      END OF approval .
  types:
    tt_approval TYPE STANDARD TABLE OF approval .
  types:
    BEGIN OF call_wf,
        salesorder         TYPE vbak-vbeln,
        salesorg           TYPE zsd_approvals-vkorg,
        saledoctype        TYPE zsd_approvals-auart,
        initiator          TYPE vbak-ernam,
        rejectedfrom       TYPE char2,
        newnetvalue        TYPE vbak-netwr,
        newportofloading   TYPE vbak-zz1_prtld_sdh,
        newportofunloading TYPE vbak-zz1_prtuld_sdh,
        newpaymentterms    TYPE vbkd-zterm,
        newincoterms       TYPE vbkd-inco1,
        oldportofloading   TYPE vbak-zz1_prtld_sdh,
        oldportofunloading TYPE vbak-zz1_prtuld_sdh,
        oldpaymentterms    TYPE vbkd-zterm,
        oldincoterms       TYPE vbkd-inco1,
        oldnetvalue        TYPE vbak-netwr,

        soldtoparty        TYPE kunnr,
        customercode       TYPE kunnr,
        material           TYPE matnr,
        quantity           TYPE menge_d,
        quantityunit       TYPE meins,
        valueofmaterial    TYPE netwr,
        payment            TYPE char250,
        old_qty            TYPE kwmeng,
        new_qty            TYPE kwmeng,
      END OF call_wf .

  constants GC_US type STRING value 'US' ##NO_TEXT.
  constants GC_APPROVED type STRING value 'APPROVED' ##NO_TEXT.
  constants GC_REJECT type STRING value 'REJECTED' ##NO_TEXT.
  constants GC_WORFLOWID type STRING value 'WS90000002' ##NO_TEXT.
  class-data LV_60_DAYS type T5A4A-DLYDY value '60' ##NO_TEXT.
  class-data LV_MONTHS type T5A4A-DLYMO .
  class-data LV_SIGNUM type T5A4A-SPLIT value '-' ##NO_TEXT.
  class-data LV_YEARS type T5A4A-DLYYR .
  class-data LV_CALC_DATE type P0001-BEGDA .
  class-data LV_DATE type P0001-BEGDA .
  class-data GC_PENDING type STRING value 'PENDING' ##NO_TEXT.

  class-methods GET_USER
    importing
      !SALESORDER type VBELN_VA
      !REJECTEDFROM type CHAR2
    exporting
      !USERS type ZSD_T_SO_APPR
      !TASKLINK type BCSY_TEXT .
  class-methods APPROVER
    importing
      value(SALESORDER) type VBELN_VA optional
      value(LEVEL) type CHAR2 optional
    exporting
      !APPROVER type SWW_AAGENT
      !INITIATOR type AD_NAMTEXT .
  class-methods REJECT
    importing
      value(SALESORDER) type VBELN_VA optional
      value(LEVEL) type CHAR2 optional
    exporting
      !REJECTOR type SWW_AAGENT
      !INITIATOR type AD_NAMTEXT
      !RJFLAG type CHAR1 .
  class-methods GETVALN
    importing
      value(SALESDOCTYPE) type VBAK-VKORG optional
      value(SALESORG) type VBAK-AUART optional
      !CUSTOMER type KUNNR
      !DIVISION type VBAK-SPART
      !DISTRIBUTIONCHANNEL type VBAK-VTWEG
    exporting
      !APPROVALS type TT_APPROVAL
      !FLAG type BOOLEAN
      !PAYMENT type DZTERM
      !INCOTERM type INCO1 .
  class-methods PENDING
    importing
      !IS_PENDING type TY_PENDING .
  class-methods SALEORD_RECHECK
    importing
      !SALESORDER type VBAK-VBELN
    exporting
      !DELIVERY type LIKP-VBELN
      !QTYCHANGE type CHAR1
    returning
      value(RECHECK) type CHAR1 .
  class-methods EMAIL
    importing
      !SENDER type AD_SMTPADR optional
      !SUBJECT type SO_OBJ_DES optional
      !EMAIL_BODY type BCSY_TEXT optional
      !EMAILIDS_CC type TT_EMAIL_IDS optional
      !EMAILIDS_TO type TT_EMAIL_IDS optional
    exporting
      !SUCESS_FLAG type BOOLEAN
      !MESSAGE type STRING .
  class-methods CALLWF
    importing
      value(CALL_WF) type CALL_WF optional .
  class-methods SOCONTENT
    importing
      !NEW_QTY type KWMENG optional
      !OLD_QTY type KWMENG optional
      !SALESORDER type VBELN_VA optional
      !NEWNETVALUE type CHAR30 optional
      !NEWPORTOFLOADING type VBAK-ZZ1_PRTLD_SDH optional
      !NEWPORTOFUNLOADING type VBAK-ZZ1_PRTULD_SDH optional
      !NEWINCOTERMS type VBKD-INCO1 optional
      !NEWPAYMENTTERMS type VBKD-ZTERM optional
      !OLDNETVALUE type CHAR30 optional
      !OLDPORTOFLOADING type VBAK-ZZ1_PRTLD_SDH optional
      !OLDPORTOFUNLOADING type VBAK-ZZ1_PRTULD_SDH optional
      !OLDINCOTERMS type VBKD-INCO1 optional
      !OLDPAYMENTTERMS type VBKD-ZTERM optional
      !LEVEL type CHAR2 optional
      !SOLDTOPARTY type KUNNR optional
      !CUSTOMERCODE type KUNNR optional
      !QUANTITY type MENGE_D optional
      !QUANTITYUNIT type MEINS optional
      !VALUEOFMATERIAL type NETWR optional
    exporting
      !MAILCONTENT type ZMAIL_CONTENT
      !UPDSODETAILS type CHAR200
      !CUSTOMFIELD type CHAR200
      !PAYMENTTERM type CHAR200
      !INCOTERM type CHAR200
      !SELLINGPRICE type CHAR200
      !QUANTITY_CHANGE type CHAR200 .
  class-methods EMAIL_ATTACHMENT
    importing
      !IT_EMAIL_IDS_TO type TY_T_EMAIL_IDS
      !IS_MAILINFO type TY_MAILINFO
      !IT_EMAIL_IDS_CC type TY_T_CCMAIL_IDS
      !IV_PDFFLAG type CHAR1 optional
    exporting
      !EV_SUCCESS_FLAG type BOOLEAN
      !EV_MESSAGE type STRING .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_SD_001_SO_APPROVAL IMPLEMENTATION.


  METHOD approver.
*----------------------------------------------------------------------*
* Class           ZCL_SD_001_SO_APPROVAL
*----------------------------------------------------------------------*
* Title:          Sales Order approval                                 *
* RICEF#:         SD_F_W_001                                           *
* Transaction:    VA01                                                 *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         Synthimed Pharma                                     *
*----------------------------------------------------------------------*
* Developer:      Mukhesh Chilukoti & (Ntt_abap2)                      *
* Creation Date:  24/10/2025                                           *
* Description:    Sales Order approval                                 *
*----------------------------------------------------------------------*
* Modification History                                                 *
*----------------------------------------------------------------------*
* Modified by:    Mukhesh Chilukoti & (Ntt_abap2)                      *
* Date:           24/10/2024                                           *
* Transport:                                                           *
* Description:    Sales Order approval                                 *
*----------------------------------------------------------------------*
    "  Data Declaration
    TYPES:ty_t_email_ids TYPE TABLE OF ad_smtpadr .
    DATA:wflogdetail             TYPE zsd_so_wf_log,
         workitem                TYPE swotobjid,
         workitemid              TYPE swotobjid-objkey,
         simplecontainers        TYPE TABLE OF swr_cont,
         messagelines            TYPE TABLE OF swr_messag,
         messagestructs          TYPE TABLE OF swr_mstruc,
         lsubcontainerborobjects TYPE TABLE OF swr_cont,
         subcontainerallobjects  TYPE TABLE OF swr_cont,
         wkitemid                TYPE swr_struct-workitemid,
         documentid              TYPE sofolenti1-doc_id,
         objectcontents          TYPE TABLE OF solisti1,
         emailcontent            TYPE bcsy_text,
         so                      TYPE string,
         emailids_to             TYPE zpmcomponentapproval=>tt_email_ids.
    CONSTANTS: linebreak TYPE c LENGTH 8 VALUE '<br><br>'.

    "  Get Workitem Details
    CALL FUNCTION 'SWE_WI_GET_FROM_REQUESTER'
      IMPORTING
        requester_workitem   = workitem
        requester_workitemid = workitemid.

    wkitemid             = workitemid.

    "  Get Container Details
    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id              = wkitemid
        language                 = sy-langu
        user                     = sy-uname
      TABLES
        simple_container         = simplecontainers
        message_lines            = messagelines
        message_struct           = messagestructs
        subcontainer_bor_objects = lsubcontainerborobjects
        subcontainer_all_objects = subcontainerallobjects.

    IF subcontainerallobjects IS NOT INITIAL.
      DELETE subcontainerallobjects WHERE element <> '_ATTACH_OBJECTS'.
      SORT subcontainerallobjects DESCENDING BY value.
    ENDIF.

    "  Read the _ATTACH_OBJECTS element
    IF ( line_exists( subcontainerallobjects[ element = '_ATTACH_OBJECTS' ] ) ). "#EC CI_STDSEQ "_ATTACH_OBJECTS
      documentid = subcontainerallobjects[ element = '_ATTACH_OBJECTS' ]-value.
    ENDIF.
    "  Read the SOFM Document
    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
      EXPORTING
        document_id                = documentid
      TABLES
        object_content             = objectcontents
      EXCEPTIONS
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        OTHERS                     = 4.
    IF sy-subrc = 0.
      "  Get Remarks
      DATA(remark) = REDUCE #( INIT remarks TYPE string
                                FOR <remarks> IN objectcontents
                                NEXT remarks = COND #( WHEN remarks IS INITIAL
                                                         THEN <remarks>-line
                                                         ELSE remarks && ',' && <remarks>-line ) ).
    ENDIF.
    SELECT SINGLE lastchangedate,
                  lastchangedbyuser,
                  createdbyuser,
                  salesdocumenttype,
                  sddocumentcategory,
                  salesorganization FROM i_salesdocument INTO @DATA(orderdeatils) WHERE salesdocument = @salesorder.
    IF sy-subrc  = 0.
      initiator = COND #( WHEN orderdeatils-lastchangedbyuser IS NOT INITIAL
                          THEN orderdeatils-lastchangedbyuser
                          ELSE  orderdeatils-createdbyuser ).
    ENDIF.

    SELECT SINGLE a~* FROM zsd_so_wf_log AS a
                     WHERE a~vbeln  = @salesorder
                     INTO  @DATA(approvallogdetail).

    IF sy-subrc  = 0.
      IF level = 'L1'.
        approver = approvallogdetail-zuid1.
        approvallogdetail-zsts1 = zcl_sd_001_so_approval=>gc_approved.   "Approved
        approvallogdetail-zaprdt1 = sy-datum .
        approvallogdetail-zaprre1 = remark .
        approvallogdetail-zaprtm1 = sy-uzeit .
      ELSEIF level = 'L2'.
        approver = approvallogdetail-zuid2.
        approvallogdetail-zsts2 = zcl_sd_001_so_approval=>gc_approved.     "Approved
        approvallogdetail-zaprdt2 = sy-datum .
        approvallogdetail-zaprre2 = remark .
        approvallogdetail-zaprtm2 = sy-uzeit .
      ELSEIF level = 'L3'.
        approver = approvallogdetail-zuid3.
        approvallogdetail-zsts3 = zcl_sd_001_so_approval=>gc_approved."TEXT-005.     "Approved
        approvallogdetail-zaprdt3 = sy-datum .
        approvallogdetail-zaprre3 = remark .
        approvallogdetail-zaprtm3 = sy-uzeit .
      ENDIF.
    ENDIF.

    wflogdetail = CORRESPONDING #( approvallogdetail ).

    IF wflogdetail IS NOT INITIAL.

      SELECT zemailid FROM zsd_approvals INTO TABLE @emailids_to
                                   WHERE vkorg = @orderdeatils-salesorganization
                                     AND auart = @orderdeatils-salesdocumenttype
                                     AND zusername = @approver
                                     AND zlevelapproval = @level.
      IF sy-subrc  = 0.


*      SELECT a~smtp_addr FROM adr6 AS a INNER JOIN usr21 ON usr21~addrnumber = a~addrnumber
*                                                     AND usr21~persnumber = a~persnumber
*                                                INTO TABLE @emailids_to WHERE usr21~bname IN ( @initiator ).",@approver ).


      ELSE.
      ENDIF.
      so = |{ salesorder ALPHA = OUT }|.
      emailcontent =   VALUE #( ( line = | { TEXT-006 } | && initiator && |,| ) "Hi
                                ( line = linebreak )
                                ( line =  TEXT-007 &&  | | &&  so &&  | | &&   TEXT-009 &&  | | &&  approver  ) "The SalesOrder has been approved by the
                                ( line = TEXT-010 && | | && remark )
                                ( line = linebreak )
                                ( line = TEXT-008 ) ). "  Thanks You.

      "Trigger Email
      TRY.
          zpmcomponentapproval=>email(
            EXPORTING
              sender      = 'Saakshi.Deshpande@bs.nttdata.com'              " E-Mail Address
              subject     = TEXT-007 &&  | | &&  so && | | && TEXT-011
              emailids_to = emailids_to                                   " TO
              email_body  = emailcontent                                  " Email Body  " CC
            IMPORTING
              sucess_flag = DATA(flag)
              message     = DATA(message) ).
        CATCH cx_root.
      ENDTRY.

      TRY.
          MODIFY zsd_so_wf_log FROM  wflogdetail.
          IF sy-subrc  = 0.
            COMMIT WORK.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD callwf.
    "Data Declaration
    DATA: message_lines   TYPE TABLE OF swr_messag,
          message_struct  TYPE TABLE OF swr_mstruc,
          agents          TYPE TABLE OF swragent,
          return_code     TYPE sy-subrc,
          workitem_id     TYPE swr_struct-workitemid,
          new_status      TYPE swr_wistat,
          input_container TYPE TABLE OF swr_cont,
          event           TYPE swr_struct-event,
          object_type     TYPE swr_struct-object_typ,
          logupdate       TYPE zsd_so_wf_log,
          valueofmat      TYPE char40,
          oldqty          TYPE char40,
          newqty          TYPE char40,
          oldnet_value    TYPE char30,
          newnet_value    TYPE char30.

*BREAK-POINT.
    SELECT SINGLE * FROM zsd_so_wf_log  WHERE vbeln = @call_wf-salesorder INTO @logupdate.
    IF sy-subrc  = 0 AND ( logupdate-zsts1 = zcl_sd_001_so_approval=>gc_reject OR
                           logupdate-zsts2 = zcl_sd_001_so_approval=>gc_reject OR
                           logupdate-zsts3 = zcl_sd_001_so_approval=>gc_reject ).

      DATA(rejectedfrom) = COND #( WHEN logupdate-zsts1 = zcl_sd_001_so_approval=>gc_reject
                                   THEN 'L1'
                                   WHEN logupdate-zsts2 = zcl_sd_001_so_approval=>gc_reject
                                   THEN 'L2'
                                   WHEN logupdate-zsts3 = zcl_sd_001_so_approval=>gc_reject
                                   THEN 'L2' ).
      IF rejectedfrom = 'L1'.
        CLEAR: logupdate-zaprdt1,logupdate-zsts1,logupdate-zaprre1,logupdate-zuid1 ,logupdate-zaprtm1.
        MODIFY zsd_so_wf_log FROM logupdate.
      ELSEIF rejectedfrom = 'L2'.
        CLEAR: logupdate-zaprdt2,logupdate-zsts2,logupdate-zaprre2,logupdate-zuid2,logupdate-zaprtm2.

        IF logupdate-zsts3 IS NOT INITIAL.
          CLEAR: logupdate-zaprdt3,logupdate-zsts3,logupdate-zaprre3,logupdate-zuid3,logupdate-zaprtm3.
        ENDIF.

        MODIFY zsd_so_wf_log FROM logupdate.
      ELSEIF rejectedfrom = 'L3'.
        CLEAR: logupdate-zaprdt2,logupdate-zsts2,logupdate-zaprre2,logupdate-zuid2,logupdate-zaprtm2,
               logupdate-zaprdt3,logupdate-zsts3,logupdate-zaprre3,logupdate-zuid3,logupdate-zaprtm3.
        MODIFY zsd_so_wf_log FROM logupdate.
      ENDIF.

    ELSEIF sy-subrc  = 0 AND ( logupdate-zsts1 <> zcl_sd_001_so_approval=>gc_reject AND "If every one approved and also user want to save anything then again it will trigger to l2
                         logupdate-zsts2 <> zcl_sd_001_so_approval=>gc_reject AND
                         logupdate-zsts3 <> zcl_sd_001_so_approval=>gc_reject ).
      rejectedfrom = '2'.
      CLEAR: logupdate-zaprdt2,logupdate-zsts2,logupdate-zaprre2,logupdate-zuid2,logupdate-zaprtm2,
             logupdate-zaprdt3,logupdate-zsts3,logupdate-zaprre3,logupdate-zuid3,logupdate-zaprtm3.
      logupdate-zlevel = 'L2'.

      MODIFY zsd_so_wf_log FROM logupdate.

    ELSEIF  sy-subrc  <> 0.
      logupdate = VALUE #( zwfid = 'WS90000004'
                           vbeln = call_wf-salesorder
                           zdctp = call_wf-saledoctype
                           ernam = call_wf-initiator
                           erdat = sy-datum ).
      MODIFY zsd_so_wf_log FROM logupdate.
    ENDIF.

    oldnet_value = call_wf-oldnetvalue.
    newnet_value = call_wf-newnetvalue.
    valueofmat = call_wf-valueofmaterial.
    newqty = call_wf-new_qty.
    oldqty = call_wf-old_qty.
    " Fill Container
    input_container = VALUE #( ( element = TEXT-015 "salesorder
                                 value   = call_wf-salesorder )
                               ( element = TEXT-016 "salesorg
                                 value   = call_wf-salesorg )
                               ( element = TEXT-017 "saledoctype
                                 value   = call_wf-saledoctype )
                               ( element = TEXT-018 "initiator
                                 value   = call_wf-initiator )
                               ( element = TEXT-019 "rejectedfrom
                                 value   = rejectedfrom )
                               ( element = TEXT-020
                                 value   = newnet_value )
                               ( element = TEXT-021
                                 value   = call_wf-newportofloading  )
                               ( element = TEXT-022
                                 value   = call_wf-newportofunloading )
                               ( element = TEXT-023
                                value   = call_wf-newpaymentterms    )
                               ( element = TEXT-024
                                value   = call_wf-newincoterms       )
                               ( element = TEXT-025
                                value   = call_wf-oldportofloading   )
                               ( element = TEXT-026
                                value   = call_wf-oldportofunloading )
                               ( element = TEXT-027
                                value   = call_wf-oldpaymentterms    )
                               ( element = TEXT-028
                                value   = call_wf-oldincoterms       )
                               ( element = TEXT-029
                                value   = oldnet_value )
                               ( element = TEXT-031
                                value   = call_wf-soldtoparty )
                               ( element = TEXT-032
                                value   = call_wf-customercode )
                               ( element = TEXT-033
                                value   = call_wf-material )
                               ( element = TEXT-034
                                value   = call_wf-quantity )
                               ( element = TEXT-035
                                value   = call_wf-quantityunit )
                               ( element = TEXT-038
                                value   = valueofmat )
                               ( element = TEXT-037
                                value   = call_wf-payment )
                                 ( element = 'NEW_QTY'
                                value   = newqty )
                                 ( element = 'OLD_QTY'
                                value   = oldqty )  ).

    CALL FUNCTION 'SAP_WAPI_START_WORKFLOW'
      EXPORTING
        task            = 'WS90000004'
        language        = sy-langu
        user            = sy-uname
      IMPORTING
        return_code     = return_code
        workitem_id     = workitem_id
        new_status      = new_status
      TABLES
        input_container = input_container
        message_lines   = message_lines
        message_struct  = message_struct
        agents          = agents.
  ENDMETHOD.


  method EMAIL.
     DATA:lo_send_request TYPE REF TO cl_bcs,
         lo_sender       TYPE REF TO if_sender_bcs,
         lo_recipient    TYPE REF TO if_recipient_bcs,
         lo_document     TYPE REF TO cl_document_bcs,
         email_to        TYPE ad_smtpadr,
         email_cc        TYPE ad_smtpadr.

    TRY.
*** Create Send request
        lo_send_request = cl_bcs=>create_persistent(  ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        message = lx_send_req_bcs->get_text( ).
    ENDTRY.

    TRY.
*** Set sender to send request
        lo_sender = cl_cam_address_bcs=>create_internet_address( sender ).
        lo_send_request->set_sender( EXPORTING i_sender = lo_sender ).

      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
      CATCH cx_send_req_bcs INTO lx_send_req_bcs.
    ENDTRY.

*** Processing internal to fill multiple emails in to
    LOOP AT emailids_to INTO DATA(ls_email_id_to).
      email_to = ls_email_id_to.
      TRY.
*** Set recipient
          lo_recipient = cl_cam_address_bcs=>create_internet_address( email_to ).
        CATCH cx_address_bcs INTO lx_address_bcs.
          message = lx_address_bcs->get_text( ).
      ENDTRY.

      TRY.
          lo_send_request->add_recipient( EXPORTING i_recipient = lo_recipient i_express = 'X' ).
        CATCH cx_send_req_bcs INTO lx_send_req_bcs.
          message = lx_send_req_bcs->get_text( ).
      ENDTRY.
    ENDLOOP.

*** Looping internal table to fill multiple emails to copy (CC).
    LOOP AT emailids_cc INTO DATA(ls_email_id_cc).
      email_cc = ls_email_id_cc.
      TRY.
*** Set recipient
          lo_recipient = cl_cam_address_bcs=>create_internet_address( email_cc ).
        CATCH cx_address_bcs INTO lx_address_bcs.
      ENDTRY.

      TRY.
          lo_send_request->add_recipient( EXPORTING i_recipient = lo_recipient i_copy = 'X' i_express = 'X' ).
        CATCH cx_send_req_bcs INTO lx_send_req_bcs.
      ENDTRY.
    ENDLOOP. "  LOOP AT lt_cc_email_id INTO DATA(ls_cc_email_id).

    TRY.
*** Create document
        lo_document = cl_document_bcs=>create_document( i_type    = 'HTM'
                                                        i_text    = email_body
                                                        i_subject = subject ).
      CATCH cx_document_bcs INTO DATA(lx_document_bcs).
        message = lx_document_bcs->get_text( ).
    ENDTRY.

    TRY.
*** Set documents
        lo_send_request->set_document( lo_document ).
      CATCH cx_send_req_bcs INTO lx_send_req_bcs.
        message = lx_send_req_bcs->get_text( ).
    ENDTRY.

    TRY.

*** Set immediate send
        CALL METHOD lo_send_request->set_send_immediately
          EXPORTING
            i_send_immediately = abap_true.

      CATCH cx_send_req_bcs INTO  lx_send_req_bcs.
        message = lx_send_req_bcs->get_text( ).
    ENDTRY.

    TRY.

*** Send email
        lo_send_request->send( EXPORTING i_with_error_screen = 'X' ).
        WAIT UP TO 1 SECONDS.
*        COMMIT WORK.
        WAIT UP TO 1 SECONDS.
        message = TEXT-001.
        sucess_flag = abap_true.

      CATCH cx_send_req_bcs INTO lx_send_req_bcs.
        message = lx_send_req_bcs->get_text( ).
    ENDTRY.
  endmethod.


  METHOD getvaln.
    SELECT SINGLE customerpaymentterms,incotermsclassification FROM i_customersalesarea INTO ( @payment, @INCOTERM )
                                       WHERE salesorganization = @salesorg AND
                                             distributionchannel = @distributionchannel AND
                                             division = @division AND
                                             customer = @customer.

*& validate to trigger workflow

    SELECT vkorg,
           auart,
           zlevelapproval,
           zusername FROM zsd_approvals INTO TABLE @approvals
                                              WHERE vkorg = @salesorg
                                                AND auart = @salesdoctype.

    CHECK sy-subrc = 0.
    flag = abap_true.
  ENDMETHOD.


  METHOD get_user.
*----------------------------------------------------------------------*
* Class           ZCL_SD_001_SO_APPROVAL
*----------------------------------------------------------------------*
* Title:          Sales Order approval                                 *
* RICEF#:         SD_F_W_001                                           *
* Transaction:    VA01                                                 *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         Synthimed Pharma                                     *
*----------------------------------------------------------------------*
* Developer:      Mukhesh Chilukoti & (Ntt_abap2)                      *
* Creation Date:  24/10/2025                                           *
* Description:    Sales Order approval                                 *
*----------------------------------------------------------------------*
* Modification History                                                 *
*----------------------------------------------------------------------*
* Modified by:    Mukhesh Chilukoti & (Ntt_abap2)                      *
* Date:           24/10/2024                                           *
* Transport:                                                           *
* Description:    Sales Order approval                                 *
*----------------------------------------------------------------------*
    " Data Declaration
*    DATA:ls_tasklink   TYPE soli.
    DATA:wflog                   TYPE TABLE OF zsd_so_wf_log.
    CHECK salesorder IS NOT INITIAL.

    SELECT SINGLE salesorganization,
                  salesdocument,
                  salesdocumenttype FROM  i_salesdocument
                  INTO @DATA(salesdetails)
                  WHERE salesdocument = @salesorder.
    IF sy-subrc = 0.
    ELSE.
    ENDIF.
    " Get Users / Approvers
    SELECT zlevelapproval,
           vkorg,
           auart FROM zsd_approvals INTO TABLE @DATA(apprrvals)
                                    WHERE vkorg = @salesdetails-salesorganization
                                      AND auart = @salesdetails-salesdocumenttype.
    IF sy-subrc  = 0.
      SORT apprrvals ASCENDING BY zlevelapproval.
      DELETE ADJACENT DUPLICATES FROM apprrvals COMPARING zlevelapproval.
      users =  VALUE #( FOR apprrval IN apprrvals        "#EC CI_STDSEQ
            ( level = apprrval-zlevelapproval ) ).

      DELETE users WHERE level IS INITIAL.
    ENDIF.

    IF rejectedfrom IS NOT INITIAL AND rejectedfrom <> 'L3' and rejectedfrom <> '2'.
      DELETE users WHERE level < rejectedfrom.
    ELSEIF rejectedfrom IS NOT INITIAL AND rejectedfrom = 'L3'.
      DELETE users WHERE level = 'L1'.
    ELSEIF rejectedfrom = '2'.
      DELETE users WHERE level <> 'L2'.
    ENDIF.


***& Populate the Sales Order Change URL
**    SELECT SINGLE low FROM tvarvc INTO @DATA(lv_link)   "#EC CI_NOORDER
**     WHERE name = @text-017."'ZSD_001_SO_DL_LINK' .
**
**    DATA(lv_link4) = '<a href=' && lv_link && '> here</a>'.
**
**    IF et_tasklink IS INITIAL.
**      CONCATENATE TEXT-018 lv_link4 INTO ls_tasklink SEPARATED BY space ."'To Review the SO, Please click '  lv_link4 INTO ls_tasklink SEPARATED BY space .
**      APPEND ls_tasklink TO et_tasklink.
**      CLEAR:ls_tasklink.
**    ENDIF.


**& Get Sales Order Details
*    SELECT SINGLE netwr,
*                  kunnr,
*                  auart,
*                  ernam,
*                  last_changed_by_user,
*                  erdat
*                  FROM vbak INTO ( @ev_netwr, @ev_kunnr, @lv_salesdocty ,@DATA(lv_uname), @DATA(lv_changed), @DATA(lv_credt) )"@DATA(ls_vbak)
*                            WHERE vbeln = @iv_salesorder.
*    IF sy-subrc EQ 0.

*      IF lv_changed IS NOT INITIAL.
*        DATA(initiator) = lv_changed.
*      ELSE.
*        initiator = lv_uname .
*      ENDIF.

**& Get address number from USR21
*      SELECT SINGLE addrnumber,
*                    persnumber FROM usr21
*                               INTO @DATA(ls_user_add)
*                               WHERE bname = @lv_initiator."@lv_uname.
*
*      IF sy-subrc = 0.

**& Get full name from ADRP using the address number and person number
*        SELECT SINGLE  name_first, name_last  FROM adrp
*                                                     WHERE persnumber = @ls_user_add-persnumber
*                                                      INTO @DATA(ls_req)."ev_requestor.
*        IF sy-subrc  = 0.
*          ev_requestor = ls_req-name_first && | | && ls_req-name_last.
*        ENDIF.
*
*      ENDIF.

**& Get Item Details
*      SELECT vbeln,
*             posnr,
*             matkl,
*             kwmeng,
*             werks,
*             matnr
*             FROM vbap INTO TABLE @DATA(lt_vbap)
*                       WHERE vbeln = @iv_salesorder."ls_vbak-vbeln.
*      IF sy-subrc EQ 0.
*        et_item = CORRESPONDING #( lt_vbap ).
*        DATA(lv_plant) = VALUE #( lt_vbap[ vbeln = iv_salesorder ]-werks OPTIONAL ).
**& Get item Quantity
*        ev_kwmeng = REDUCE #( INIT lv_quantity TYPE kwmeng
*                              FOR <fs_qty> IN lt_vbap
*                              NEXT lv_quantity =  lv_quantity + <fs_qty>-kwmeng  ).
**& Get Customer Name
*        SELECT SINGLE name1 FROM kna1 INTO @ev_name1
*                                      WHERE kunnr = @ev_kunnr."ls_vbak-kunnr.
*      ENDIF.
*    ENDIF.



**& Get Users / Approvers
*    SELECT werks,
*    zsdtp,
*    zusrl,
*    zuid1,
*    zuid2,
*    zuid3,
*    zuid4,
*    zuid5 FROM zsd_sales_appmtr INTO TABLE @DATA(lt_appr)
*                                WHERE werks = @lv_plant AND zsdtp = @lv_salesdocty.
*    IF sy-subrc  = 0.
*      et_user =  VALUE #( FOR fs_apprl IN lt_appr        "#EC CI_STDSEQ
*            ( user = gc_us && fs_apprl-zuid1 )
*            ( user = gc_us && fs_apprl-zuid2 )
*            ( user = gc_us && fs_apprl-zuid3 )
*            ( user = gc_us && fs_apprl-zuid4 )
*            ( user = gc_us && fs_apprl-zuid5 ) ).
*
*      DELETE et_user WHERE user = 'US' && lv_initiator.
*    ENDIF.
*
****& Populate the Sales Order Change URL
***    SELECT SINGLE low FROM tvarvc INTO @DATA(lv_link)   "#EC CI_NOORDER
***     WHERE name = @text-017."'ZSD_001_SO_DL_LINK' .
***
***    DATA(lv_link4) = '<a href=' && lv_link && '> here</a>'.
***
***    IF et_tasklink IS INITIAL.
***      CONCATENATE TEXT-018 lv_link4 INTO ls_tasklink SEPARATED BY space ."'To Review the SO, Please click '  lv_link4 INTO ls_tasklink SEPARATED BY space .
***      APPEND ls_tasklink TO et_tasklink.
***      CLEAR:ls_tasklink.
***    ENDIF.
  ENDMETHOD.


  METHOD pending.
*" Data Declaration
*    DATA:lt_wflog TYPE TABLE OF zsd_so_wf_log.
*
*    lt_wflog =  VALUE #(  BASE lt_wflog  ( zwfid   = gc_worflowid
*                                           vbeln   = is_pending-vbeln
*                                           zdctp   = is_pending-zdctp
*                                           ernam   = is_pending-ernam
*                                           erdat   = is_pending-erdat
*                                           zuid1   = is_pending-zuid1
*                                           zsts1   = gc_pending
*                                           zaprdt1 = sy-datum ) ).
*
*    CHECK lt_wflog IS NOT INITIAL.
*
*    MODIFY zsd_so_wf_log FROM TABLE lt_wflog.
*    IF sy-subrc  = 0.
*      COMMIT WORK.
*    ENDIF.
  ENDMETHOD.


  METHOD reject.
*----------------------------------------------------------------------*
* Class           ZCL_SD_001_SO_APPROVAL
*----------------------------------------------------------------------*
* Title:          Sales Order approval                                 *
* RICEF#:         SD_F_W_001                                           *
* Transaction:    VA01                                                 *
*----------------------------------------------------------------------*
* Copyright:      NDBS, Inc.                                           *
* Client:         Synthimed Pharma                                     *
*----------------------------------------------------------------------*
* Developer:      Mukhesh Chilukoti & (Ntt_abap2)                      *
* Creation Date:  24/10/2025                                           *
* Description:    Sales Order approval                                 *
*----------------------------------------------------------------------*
* Modification History                                                 *
*----------------------------------------------------------------------*
* Modified by:    Mukhesh Chilukoti & (Ntt_abap2)                      *
* Date:           24/10/2024                                           *
* Transport:                                                           *
* Description:    Sales Order approval                                 *
*----------------------------------------------------------------------*
    "  Data Declaration
    TYPES:ty_t_email_ids TYPE TABLE OF ad_smtpadr .
    DATA:wflogdetail             TYPE zsd_so_wf_log,
         workitem                TYPE swotobjid,
         workitemid              TYPE swotobjid-objkey,
         simplecontainers        TYPE TABLE OF swr_cont,
         messagelines            TYPE TABLE OF swr_messag,
         messagestructs          TYPE TABLE OF swr_mstruc,
         lsubcontainerborobjects TYPE TABLE OF swr_cont,
         subcontainerallobjects  TYPE TABLE OF swr_cont,
         wkitemid                TYPE swr_struct-workitemid,
         documentid              TYPE sofolenti1-doc_id,
         objectcontents          TYPE TABLE OF solisti1,
         emailcontent            TYPE bcsy_text,
         emailids_to             TYPE zpmcomponentapproval=>tt_email_ids.
    CONSTANTS: linebreak TYPE c LENGTH 8 VALUE '<br><br>'.

    rjflag = abap_true.
    "  Get Workitem Details
    CALL FUNCTION 'SWE_WI_GET_FROM_REQUESTER'
      IMPORTING
        requester_workitem   = workitem
        requester_workitemid = workitemid.

    wkitemid             = workitemid.

    "  Get Container Details
    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id              = wkitemid
        language                 = sy-langu
        user                     = sy-uname
      TABLES
        simple_container         = simplecontainers
        message_lines            = messagelines
        message_struct           = messagestructs
        subcontainer_bor_objects = lsubcontainerborobjects
        subcontainer_all_objects = subcontainerallobjects.

    IF subcontainerallobjects IS NOT INITIAL.
      DELETE subcontainerallobjects WHERE element <> '_ATTACH_OBJECTS'.
      SORT subcontainerallobjects DESCENDING BY value.
    ENDIF.

    "  Read the _ATTACH_OBJECTS element
    IF ( line_exists( subcontainerallobjects[ element = '_ATTACH_OBJECTS' ] ) ). "#EC CI_STDSEQ "_ATTACH_OBJECTS
      documentid = subcontainerallobjects[ element = '_ATTACH_OBJECTS' ]-value.
    ENDIF.
    "  Read the SOFM Document
    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
      EXPORTING
        document_id                = documentid
      TABLES
        object_content             = objectcontents
      EXCEPTIONS
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        OTHERS                     = 4.
    IF sy-subrc = 0.
      "  Get Remarks
      DATA(remark) = REDUCE #( INIT remarks TYPE string
                                FOR <remarks> IN objectcontents
                                NEXT remarks = COND #( WHEN remarks IS INITIAL
                                                         THEN <remarks>-line
                                                         ELSE remarks && ',' && <remarks>-line ) ).
    ENDIF.
    SELECT SINGLE lastchangedate,
                  createdbyuser,
                  lastchangedbyuser FROM i_salesdocument INTO @DATA(orderdeatils) WHERE salesdocument = @salesorder.

    IF sy-subrc  = 0.
      initiator = COND #( WHEN orderdeatils-lastchangedbyuser IS NOT INITIAL
                          THEN  orderdeatils-lastchangedbyuser
                          ELSE  orderdeatils-createdbyuser ).
    ENDIF.

*    "  Fetch privious workitem id
*    SELECT SINGLE top_wi_id              "#EC CI_NOORDER or "#EC WARNOK
*                 FROM swwwihead "wi_cruser
*                        INTO @DATA(priviousid)
*                        WHERE wi_id = @workitemid.
*    IF sy-subrc  = 0.
*      "  Get user for current workitem id for without levels "auto
*      SELECT SINGLE wi_aagent FROM swwwihead "#EC CI_NOORDER or "#EC WARNOK
*                              INTO @DATA(approver)
*                              WHERE top_wi_id = @priviousid
*                                AND wi_rh_task = 'TS90000002'.
*    ENDIF.


    SELECT SINGLE a~* FROM zsd_so_wf_log AS a
                     WHERE a~vbeln  = @salesorder
                     INTO  @DATA(approvallogdetail).

    IF sy-subrc  = 0.
      IF level = 'L1'.
        rejector = approvallogdetail-zuid1.
        approvallogdetail-zsts1 = zcl_sd_001_so_approval=>gc_reject.    "rejected
        approvallogdetail-zaprdt1 = sy-datum .
        approvallogdetail-zaprre1 = remark .
        approvallogdetail-zaprtm1 = sy-uzeit .
      ELSEIF level = 'L2'.
        rejector = approvallogdetail-zuid2.
        approvallogdetail-zsts2 = zcl_sd_001_so_approval=>gc_reject.    "rejected
        approvallogdetail-zaprdt2 = sy-datum .
        approvallogdetail-zaprre2 = remark .
        approvallogdetail-zaprtm2 = sy-uzeit .
      ELSEIF level = 'L3'.
        rejector = approvallogdetail-zuid3.
        approvallogdetail-zsts3 = zcl_sd_001_so_approval=>gc_reject.      "rejected
        approvallogdetail-zaprdt3 = sy-datum .
        approvallogdetail-zaprre3 = remark .
        approvallogdetail-zaprtm3 = sy-uzeit .
      ENDIF.
    ENDIF.

    wflogdetail = CORRESPONDING #( approvallogdetail ).

    IF wflogdetail IS NOT INITIAL.

*         SELECT zemailid FROM zsd_approvals INTO TABLE @emailids_to
*                                   WHERE vkorg = @orderdeatils-salesorganization
*                                     AND auart = @orderdeatils-salesdocumenttype
*                                     AND zusername = @initiator
*                                     AND zlevelapproval = @level.

      SELECT a~smtp_addr FROM adr6 AS a INNER JOIN usr21 ON usr21~addrnumber = a~addrnumber
                                                     AND usr21~persnumber = a~persnumber
                                                INTO TABLE @emailids_to WHERE usr21~bname IN ( @initiator ).",@REJECTOR ).
      IF sy-subrc  = 0.
      ELSE.
      ENDIF.

      emailcontent =   VALUE #( ( line = | { TEXT-006 } | && initiator && |,| ) "Hi
                                ( line = linebreak )
                                ( line =  TEXT-007 &&  | | &&  salesorder &&  | | &&   TEXT-012 &&  | | &&  rejector  ) "The SalesOrder has been approved by the
                                ( line = TEXT-010 && | | && remark )
                                ( line = linebreak )
                                ( line = TEXT-008 ) ). "  Thanks You.

      "Trigger Email
      TRY.
          zpmcomponentapproval=>email(
            EXPORTING
              sender      = 'Saakshi.Deshpande@bs.nttdata.com'              " E-Mail Address
              subject     = TEXT-007 &&  | | &&  salesorder && | | && TEXT-013
              emailids_to = emailids_to                                   " TO
              email_body  = emailcontent                                  " Email Body  " CC
            IMPORTING
              sucess_flag = DATA(flag)
              message     = DATA(message) ).
        CATCH cx_root.
      ENDTRY.

      TRY.
          MODIFY zsd_so_wf_log FROM  wflogdetail.
          IF sy-subrc  = 0.
            COMMIT WORK.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
    ENDIF.






























































































































*    TYPES:ty_t_email_ids TYPE TABLE OF ad_smtpadr .
*    DATA:lt_wflog                    TYPE TABLE OF zsd_so_wf_log,
*         ls_workitem                 TYPE swotobjid,
*         lv_workitemid               TYPE swotobjid-objkey,
*         lt_simple_container         TYPE TABLE OF swr_cont,
*         lt_message_lines            TYPE TABLE OF swr_messag,
*         lt_message_struct           TYPE TABLE OF swr_mstruc,
*         lt_subcontainer_bor_objects TYPE TABLE OF swr_cont,
*         lt_subcontainer_all_objects TYPE TABLE OF swr_cont,
*         lv_workitem_id              TYPE swr_struct-workitemid,
*         lv_document_id              TYPE sofolenti1-doc_id,
*         lt_object_content           TYPE TABLE OF solisti1,
*         lt_email_ids_to             TYPE  ty_t_email_ids,
*         lt_email_body               TYPE  bcsy_text,
*         lv_textid                   TYPE  thead-tdid,
*         lv_name                     TYPE  thead-tdname,
*         lv_textobj                  TYPE  thead-tdobject,
*         lv_subject                  TYPE so_obj_des.
*
**& Get Workitem Details
*    CALL FUNCTION 'SWE_WI_GET_FROM_REQUESTER'
*      IMPORTING
*        requester_workitem   = ls_workitem
*        requester_workitemid = lv_workitemid.
*    lv_workitem_id       = lv_workitemid.
*
**& Get Container Details
*    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
*      EXPORTING
*        workitem_id              = lv_workitem_id
*        language                 = sy-langu
*        user                     = sy-uname
*      TABLES
*        simple_container         = lt_simple_container
*        message_lines            = lt_message_lines
*        message_struct           = lt_message_struct
*        subcontainer_bor_objects = lt_subcontainer_bor_objects
*        subcontainer_all_objects = lt_subcontainer_all_objects.
*
**& Read the _ATTACH_OBJECTS element
*    IF ( line_exists( lt_subcontainer_all_objects[ element = TEXT-016 ] ) ). "_ATTACH_OBJECTS
*      lv_document_id = lt_subcontainer_all_objects[ element = TEXT-016 ]-value.
*    ENDIF.
*
**& Read the SOFM Document
*    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
*      EXPORTING
*        document_id                = lv_document_id
*      TABLES
*        object_content             = lt_object_content
*      EXCEPTIONS
*        document_id_not_exist      = 1
*        operation_no_authorization = 2
*        x_error                    = 3
*        OTHERS                     = 4.
*    IF sy-subrc = 0.
**& Get Remarks
*      DATA(lv_remarks) = REDUCE #( INIT lv_remark TYPE string
*                                FOR <fs_remarks> IN lt_object_content
*                                NEXT lv_remark = COND #( WHEN lv_remark IS INITIAL
*                                                         THEN <fs_remarks>-line
*                                                         ELSE lv_remark && ',' && <fs_remarks>-line ) ).
*      ev_remarks = lv_remarks.
*    ENDIF.
*
**& Get Sales Order Details
*    SELECT vbap~werks,                   "#EC CI_NOORDER or "#EC WARNOK
*           vbak~ernam,
*           vbak~erdat,
*           vbak~auart,
*           vbak~netwr,
*           vbap~kwmeng,
*           vbap~matnr,
*           vbap~posnr,
*           vbak~vbeln,
*           vbak~last_changed_by_user
*           FROM vbap INNER JOIN vbak ON vbak~vbeln = vbap~vbeln
*                            INTO TABLE @DATA(lt_so_details)
*                            WHERE vbak~vbeln = @iv_salesorder.
*
*    IF sy-subrc  = 0.
*      DATA(lv_qty) = REDUCE kwmeng( INIT lv_qtycr TYPE vbap-kwmeng "#EC CI_STDSEQ
*                                         FOR ls_qty IN lt_so_details[]
*                                         NEXT lv_qtycr =   lv_qtycr + ls_qty-kwmeng ).
*
*
*      SELECT SINGLE werks,
*                    ernam,
*                    erdat,
*                    auart,
*                    netwr FROM @lt_so_details AS lt_so_details
*                          WHERE vbeln = @iv_salesorder INTO @DATA(ls_so_details).
**    IF sy-subrc  = 0.
*
**& Fetch privious workitem id
*      SELECT SINGLE top_wi_id "wi_cruser
*                   FROM swwwihead "wi_cruser
*                          INTO @DATA(lv_privious_id)
*                          WHERE wi_id = @lv_workitemid.
*      IF sy-subrc  = 0.
**& Get user for current workitem id for without levels
*        SELECT SINGLE wi_aagent FROM swwwihead "wi_cruser
*                         INTO @DATA(lv_approver)
*                         WHERE top_wi_id = @lv_privious_id
*                            AND wi_rh_task = 'TS90000031'."@text-021 ."'TS90000024'.
*        IF sy-subrc  = 0.
*          ev_rejector = lv_approver.
*        ENDIF.
*      ENDIF.
*      DATA(lv_workitem) = lv_workitemid - 4.
*      SELECT SINGLE top_wi_id "wi_cruser
*                  FROM swwwihead "wi_cruser
*           INTO  @DATA(lv_topid)
*                          WHERE wi_id = @lv_workitem.
*      IF sy-subrc  = 0.
*        SELECT SINGLE wi_creator "wi_cruser
*             FROM swwwihead "wi_cruser
*             INTO  @DATA(lv_initiator)
*                            WHERE wi_id = @lv_topid.
*
*        IF sy-subrc  = 0.
*          ev_initiator = lv_initiator.
*        ENDIF.
*      ENDIF.
*
*      IF lv_approver IS NOT INITIAL.
*        SELECT SINGLE zusrl,             "#EC CI_NOORDER or "#EC WARNOK
*                        zuid1,
*                        zuid2,
*                        zuid3,
*                        zuid4,
*                        zuid5,
*                        werks FROM zsd_sales_appmtr INTO @DATA(ls_appr)
*                             WHERE werks = @ls_so_details-werks
*                               AND zsdtp = @ls_so_details-auart
*                               AND ( zuid1 = @lv_approver OR zuid2 = @lv_approver OR zuid3 = @lv_approver OR
*                                     zuid4 = @lv_approver OR zuid5 = @lv_approver ).
*        IF sy-subrc  = 0.
*        ELSE.
*        ENDIF.
**        ENDIF.
**      ENDIF.
*
*        SELECT * FROM zsd_so_wf_log INTO TABLE @DATA(lt_approval_log)
*                                WHERE vbeln = @iv_salesorder.
*        IF sy-subrc = 0 AND ls_appr-zusrl IS INITIAL.
*          lt_wflog =  VALUE #( FOR <fs_wflog> IN lt_approval_log "#EC CI_STDSEQ
*                                 ( zwfid   = <fs_wflog>-zwfid
*                                   vbeln   = <fs_wflog>-vbeln
*                                   zdctp   = <fs_wflog>-zdctp
*                                   ernam   = <fs_wflog>-ernam
*                                   erdat   = <fs_wflog>-erdat
*                                   zlevel  = ls_appr-zusrl
*                                   zuid1   = lv_approver
*                                   zsts1   = gc_reject
*                                   zaprdt1 = sy-datum
*                                   zaprre1 = lv_remarks
*                                   zuid2   = ''
*                                   zaprdt2 = ''
*                                   zaprre2 = ''
*                                   zuid3   = ''
*                                   zaprdt3 = ''
*                                   zaprre3 = ''
*                                   zuid4   = ''
*                                   zaprdt4 = ''
*                                   zaprre4 = ''
*                                   zuid5   = ''
*                                   zaprdt5 = ''
*                                   zaprre5 = '' ) ).
*
*        ELSEIF sy-subrc <> 0 AND ls_appr-zusrl IS INITIAL.
*
*          lt_wflog =  VALUE #(  BASE lt_wflog  ( zwfid   = gc_worflowid
*                                                 vbeln   = iv_salesorder
*                                                 zdctp   = ls_so_details-auart
*                                                 ernam   = ls_so_details-ernam
*                                                 erdat   = ls_so_details-erdat
*                                                 zlevel  = ls_appr-zusrl
*                                                 zuid1   = lv_approver
*                                                 zsts1   = gc_reject
*                                                 zaprdt1 = sy-datum
*                                                 zaprre1 = lv_remarks  ) )."lv_remarks ) ).
*        ENDIF.
*      ENDIF.
*      IF lt_wflog IS NOT INITIAL.
*        MODIFY zsd_so_wf_log FROM TABLE lt_wflog.
*        IF sy-subrc  = 0.
*          COMMIT WORK.
*        ENDIF.
*
*        SELECT  bname,
*                persnumber FROM usr21 INTO TABLE @DATA(lt_personno) WHERE bname = @ev_initiator.
*        IF  sy-subrc  = 0.
*          SELECT  adr6~smtp_addr FROM adr6
*                                 INNER JOIN @lt_personno AS lt_personno
*                                 ON lt_personno~persnumber = adr6~persnumber
*                                 INTO TABLE @lt_email_ids_to.
*        ENDIF.
*
**& Fetching from Mail Address
*        SELECT SINGLE  emailaddress FROM i_addressemailaddress WITH PRIVILEGED ACCESS
*                                    INNER JOIN i_plant ON i_plant~plant = @ls_so_details-werks
*                                    AND i_plant~addressid = i_addressemailaddress~addressid
*                                    AND i_addressemailaddress~ordinalnumber = @/isdfps/cl_const_abc_123=>gc_3
*                                    INTO @DATA(lv_from_mail).
*        IF sy-subrc  = 0.
*          "do nothing
*        ELSE.
*        ENDIF.
*        lv_textid = TEXT-009."  ST
*        lv_textobj = TEXT-010."  TEXT
*        lv_name = TEXT-012."ZSD_001_SO_REJECTION
*
*        IF VALUE #( lt_so_details[ vbeln = iv_salesorder ]-last_changed_by_user OPTIONAL ) IS NOT INITIAL.
*          ev_initiator = VALUE #( lt_so_details[ vbeln = iv_salesorder ]-last_changed_by_user OPTIONAL ).
*        ELSE.
*          ev_initiator = VALUE #( lt_so_details[ vbeln = iv_salesorder ]-ernam OPTIONAL ).
*        ENDIF.
**& get mail content
*        DATA(lt_mailbody) = zcl_global_utilities=>read_text_tab( is_textdetails = VALUE #(  textid  = lv_textid
*                                                                                            name    = lv_name
*                                                                                            textobj = lv_textobj ) ).
*        LOOP AT lt_mailbody ASSIGNING FIELD-SYMBOL(<fs_mailbody>).
*          REPLACE ALL OCCURRENCES OF '&INITIATOR&' IN <fs_mailbody> WITH ev_initiator.
*          REPLACE ALL OCCURRENCES OF '&APPROVER&' IN <fs_mailbody> WITH lv_approver.
*          REPLACE ALL OCCURRENCES OF '&REMARKS&'  IN <fs_mailbody> WITH lv_remarks.
*          APPEND VALUE #( line = <fs_mailbody> ) TO lt_email_body .
*        ENDLOOP.
**& Subject
*        lv_subject = 'Sales Order' && | | && iv_salesorder && ' Rejected'.
**& Send Email to initiator to notify approval and rejection
*
*        TRY.
*            zcl_global_utilities=>sendemail(
*              EXPORTING
*                iv_email_sender  = lv_from_mail                 " E-Mail Address
*                iv_email_subject = lv_subject                   " Short description of contents
*                it_email_ids_to  = lt_email_ids_to
*                it_email_body    = lt_email_body                " Text Table
*              IMPORTING
*                ev_sucess_flag   = DATA(ev_sucess_flag)         " Boolean Variable (X = True, - = False, Space = Unknown)
*                ev_message       = DATA(ev_message)
*            ).
*          CATCH cx_root.
*        ENDTRY.
*
*
*
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD saleord_recheck.
    SELECT vbeln,
          zsts1,
          zlevel,
          zsts2,
          zsts3 FROM zsd_so_wf_log
                INTO TABLE @DATA(lt_log) WHERE vbeln = @salesorder.
    IF sy-subrc = 0.
      SELECT log~vbeln,
             log~zsts1,
             log~zsts2,
             log~zsts3 FROM @lt_log AS log
                   WHERE vbeln = @salesorder
                   AND ( log~zsts1 = @zcl_sd_001_so_approval=>gc_pending OR
                         log~zsts2 = @zcl_sd_001_so_approval=>gc_pending OR
                         log~zsts3 = @zcl_sd_001_so_approval=>gc_pending ) INTO TABLE @DATA(lt_log_check) .
      IF sy-subrc = 0.
        recheck = abap_true.
      ENDIF.

      IF VALUE #( lt_log[ 1 ]-zlevel OPTIONAL ) = 'L2' AND VALUE #( lt_log[ 1 ]-zsts2 OPTIONAL ) = zcl_sd_001_so_approval=>gc_approved.
        qtychange = abap_true.
      ELSEIF VALUE #( lt_log[ 1 ]-zlevel OPTIONAL ) = 'L3' AND VALUE #( lt_log[ 1 ]-zsts3 OPTIONAL ) = zcl_sd_001_so_approval=>gc_approved.
        qtychange = abap_true.
      ELSEIF VALUE #( lt_log[ 1 ]-zlevel OPTIONAL ) = 'L1' AND VALUE #( lt_log[ 1 ]-zsts1 OPTIONAL ) = zcl_sd_001_so_approval=>gc_approved.
        qtychange = abap_true.
      ENDIF.

    ENDIF.

    " Check Retriggering of the workflow (before delivery need to triggere for all the changes and
    "after delivery need to triogger when only quantity get's changed)

    SELECT SINGLE precedingdocument,
                  subsequentdocument,
                  subsequentdocumentcategory FROM i_sddocumentmultilevelprocflow
                                             INTO @DATA(deliverycheck)
                                             WHERE precedingdocument = @salesorder AND subsequentdocumentcategory = 'J' .
    IF sy-subrc  = 0.
      delivery = deliverycheck-subsequentdocument.
    ENDIF.

  ENDMETHOD.


  METHOD socontent.
    DATA:workitem                TYPE swotobjid,
         workitemid              TYPE swotobjid-objkey,
         simplecontainers        TYPE TABLE OF swr_cont,
         messagelines            TYPE TABLE OF swr_messag,
         messagestructs          TYPE TABLE OF swr_mstruc,
         lsubcontainerborobjects TYPE TABLE OF swr_cont,
         subcontainerallobjects  TYPE TABLE OF swr_cont,
         wkitemid                TYPE swr_struct-workitemid,
         documentid              TYPE sofolenti1-doc_id,
         wa_output               TYPE w3html,
         so                      TYPE string,
         soldtoparty_text        TYPE string,
         customercode_text       TYPE string,
         saleorder_text          TYPE string,
         quantity_text           TYPE string,
         quantityunit_text       TYPE string,
         valueofmaterial_text    TYPE string,
         changed_on              TYPE string,
         created_on              TYPE string,
         last_cg_on              TYPE string,
         grossvalue              TYPE string,
         taxvalue                TYPE string,
         form_name               TYPE  fpname,
         outputparams            TYPE sfpoutputparams,
         emailids_to             TYPE zcl_sd_001_so_approval=>tt_email_ids,
         to_mail                 TYPE TABLE OF ad_smtpadr,
         cc_mailid               TYPE TABLE OF ad_smtpadr,
         function_name           TYPE rs38l_fnam,
         formoutput              TYPE fpformoutput,
         mail_body               TYPE bcsy_text,
         lt_formoutput           TYPE tfpcontent,
         merged_document         TYPE xstring,
         docparams               TYPE sfpdocparams,
         rc                      TYPE i,
         lo_pdf_merger           TYPE REF TO cl_rspo_pdf_merge,
         salesdocument           TYPE  vbeln,
         salesdocumentitem       TYPE  posnr.

    CONSTANTS:              linebreak TYPE c LENGTH 8 VALUE '<br><br>'.

    "get customer name
    SELECT SINGLE customername FROM i_customer INTO @DATA(customer_name) WHERE customer = @customercode.
    "sales header details
    SELECT SINGLE salesorganization,
                  salesdocument,
                  salesdocumenttype,
                  lastchangedbyuser,
                  createdbyuser,
                  creationdate,
                  creationtime,
                  lastchangedate,
                  lastchangedatetime,
                  salesdocumentcondition,
                  purchaseorderbycustomer,
                  organizationdivision,
                  distributionchannel,
                  soldtoparty,
                  customerpaymentterms
                  FROM i_salesdocument INTO @DATA(salesdetails)
                                     WHERE salesdocument = @salesorder.
    IF sy-subrc = 0.
      last_cg_on = salesdetails-lastchangedatetime.

      " customer payment terms
      SELECT SINGLE paymentterms,
                    customer,
                    salesorganization,
                    distributionchannel,
                    division FROM i_customersalesarea
                             INTO @DATA(custpayment)
                             WHERE customer = @salesdetails-soldtoparty
                               AND salesorganization = @salesdetails-salesorganization
                               AND distributionchannel = @salesdetails-distributionchannel
                               AND division = '00'." @salesdetails-organizationdivision.

      SELECT kbetr,
             kschl,
             kwert FROM prcd_elements INTO TABLE @DATA(conditionvalue)
                   WHERE kschl IN ( 'ZBAS','Z004','Z005','Z007','ZD00','JOIG','JOCG','JOSG','JOUG','ZMIS','ZMIS' )
                     AND knumv = @salesdetails-salesdocumentcondition.
      IF sy-subrc  = 0.
        grossvalue =   REDUCE kwert( INIT grossval TYPE i_slsprcgconditionrecord-conditionratevalue
                                             FOR grossvaldet IN conditionvalue "#EC CI_STDSEQ
                                      NEXT grossval =   grossval + grossvaldet-kwert ).

        taxvalue  =   REDUCE kwert( INIT taxval TYPE i_slsprcgconditionrecord-conditionratevalue
                                             FOR taxvaldet IN conditionvalue "#EC CI_STDSEQ
                                             WHERE ( kschl = 'ZBAS' OR kschl =  'Z004' OR kschl = 'Z005' OR kschl = 'Z007' OR kschl = 'ZD00' )
                                      NEXT taxval =   taxval + taxvaldet-kwert ).
      ENDIF.
    ENDIF.

*ZDOM Domestic  Gross Value in SOC
*ZSEZ	SEZ	Taxable Value
*ZDEE DEEmed  Gross Value in SOC
*ZEXP Export  Taxable Value
*ZMER Marchent  Gross Value in SOC
*ZDSF,ZESF  Sample  Based on Process


*ZDSF	Gross Value
***ZESF	Taxable Value
*ZRSF	Gross Value
*ZMSF	Gross Value
*ZUSF	Gross Value
*ZSCR	Gross Value
***ZSEZ	Taxable Value
***ZZSF	Taxable Value
***ZEXP	Taxable Value
*ZDOM	Gross Value
*ZEOU	Gross Value
*ZMER	Gross Value
*ZRMP	Gross Value


    IF ( level = 'L1' OR  level = 'L2'  ).

*        IF ( level = 'L1' OR  level = 'L2'  ).

      IF ( salesdetails-salesdocumenttype = 'ZESF'   OR salesdetails-salesdocumenttype = 'ZSEZ' OR
           salesdetails-salesdocumenttype =  'ZZSF'  OR salesdetails-salesdocumenttype = 'ZEXP' ).
        DATA(value) = taxvalue.
        IF level = 'L2'.

          wa_output-line = '<HTML><BODY><TABLE BORDER="2" ><TR><TH>SO</TH><TH>Customer code</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR: wa_output.

          wa_output-line =  '<TH>Customer Name</TH><TH>PO Number</TH><TH>Quantity</TH><TH>Quantity Unit</TH><TH>Taxable Value</TH>'
                                && '<TH>SO Payment Terms</TH><TH>Customer Payment Terms</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR: wa_output.

        ELSEIF level = 'L1'.

          wa_output-line = '<HTML><BODY><TABLE BORDER="2" ><TR><TH>SO</TH><TH>Customer code</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR: wa_output.

          wa_output-line =  '<TH>Customer Name</TH><TH>Quantity</TH><TH>Quantity Unit</TH><TH>Taxable Value</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR: wa_output.

          wa_output-line = '<TH>SO Payment Terms</TH><TH>Customer Payment Terms</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR: wa_output.

        ENDIF.
*        APPEND wa_output TO mailcontent.
*        CLEAR: wa_output.

      ELSE.

        value = grossvalue.
        IF level = 'L2'.
          wa_output-line = '<HTML><BODY><TABLE BORDER="2" ><TR><TH>SO</TH><TH>Customer code</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR:wa_output.

          wa_output-line =  '<TH>Customer Name</TH><TH>PO Number</TH><TH>Quantity</TH><TH>Quantity Unit</TH><TH>Gross Value</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR:wa_output.

          wa_output-line = '<TH>SO Payment Terms</TH><TH>Customer Payment Terms</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR:wa_output.

        ELSEIF level = 'L1'.
          wa_output-line = '<HTML><BODY><TABLE BORDER="2" ><TR><TH>SO</TH><TH>Customer code</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR:wa_output.

          wa_output-line =  '<TH>Customer Name</TH><TH>Quantity</TH><TH>Quantity Unit</TH><TH>Gross Value</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR:wa_output.

          wa_output-line = '<TH>SO Payment Terms</TH><TH>Customer Payment Terms</TH>'.
          APPEND wa_output TO mailcontent.
          CLEAR:wa_output.

        ENDIF.
*        APPEND wa_output TO mailcontent.
*        CLEAR: wa_output.
      ENDIF.


      IF salesdetails-lastchangedbyuser IS NOT INITIAL.
        wa_output-line = '<TH>Created By</TH><TH>Created On</TH><TH>Changed By</TH><TH>Changed On</TH></TR>'.
        APPEND wa_output TO mailcontent.
        CLEAR: wa_output.
      ELSE.
        wa_output-line = '<TH>Created By</TH><TH>Created On</TH></TR>'.
        APPEND wa_output TO mailcontent.
        CLEAR: wa_output.
      ENDIF.
*      ENDIF.

      IF salesdetails-lastchangedatetime IS NOT INITIAL.
        changed_on = 'DT:' && | | && last_cg_on+6(2) && |.| && last_cg_on+4(2) && |.| &&  last_cg_on+0(4)
                     && | | &&  |TM:| && | | && last_cg_on+8(2) && |:| && last_cg_on+10(2) && |:| &&  last_cg_on+12(2) .
      ENDIF.
      IF salesdetails-creationdate IS NOT INITIAL AND   salesdetails-creationtime IS NOT INITIAL.
        created_on = 'DT:' && | | && salesdetails-creationdate+6(2) && |.| && salesdetails-creationdate+4(2) && |.| &&  salesdetails-creationdate+0(4)
                     && | | &&  |TM:| && | | && salesdetails-creationtime+0(2) && |:| && salesdetails-creationtime+2(2) && |:| &&  salesdetails-creationtime+4(2) .
      ENDIF.

      soldtoparty_text        = |{ soldtoparty ALPHA = OUT }|..
      customercode_text       = |{ customercode ALPHA = OUT }|.. .
      quantity_text           = quantity.
      quantityunit_text       = quantityunit.
*      valueofmaterial_text    = valueofmaterial.
      saleorder_text = |{ salesorder ALPHA = OUT }|.

      IF ( level = 'L2' OR level = 'L1' ).
        IF level = 'L2'.
          CONCATENATE '<TR><TD>' saleorder_text '</TD><TD>' customercode_text '</TD><TD>'  customer_name '</TD><TD>' salesdetails-purchaseorderbycustomer '</TD><TD>' quantity_text '</TD><TD>'
                                 quantityunit_text '</TD><TD>' value '</TD><TD>' salesdetails-customerpaymentterms '</TD><TD>' custpayment-paymentterms '</TD>'  INTO wa_output-line.
        ELSEIF level = 'L1'.
          CONCATENATE '<TR><TD>' saleorder_text '</TD><TD>' customercode_text '</TD><TD>'  customer_name  '</TD><TD>' quantity_text '</TD><TD>'
                                 quantityunit_text '</TD><TD>' value '</TD><TD>' salesdetails-customerpaymentterms '</TD><TD>' custpayment-paymentterms '</TD>' INTO wa_output-line.
        ENDIF.
        APPEND wa_output TO mailcontent.
        CLEAR:wa_output.

        IF salesdetails-lastchangedbyuser IS NOT INITIAL.
          CONCATENATE '<TD>' salesdetails-createdbyuser '</TD><TD>' created_on '</TD><TD>' salesdetails-lastchangedbyuser  '</TD><TD>' changed_on '</TD><TR>' INTO wa_output-line.
          APPEND wa_output TO mailcontent.
          CLEAR:wa_output.
        ELSE.
          CONCATENATE '<TD>' salesdetails-createdbyuser '</TD><TD>' created_on '</TD><TR>' INTO wa_output-line.
          APPEND wa_output TO mailcontent.
          CLEAR:wa_output.
        ENDIF.
      ENDIF.


      wa_output-line = '</TABLE></BODY></HTML>'. " Close HTML table
      APPEND wa_output TO mailcontent.
      CLEAR: wa_output.

      IF  level = 'L1'.
        paymentterm = COND #( WHEN  newpaymentterms <> oldpaymentterms AND newpaymentterms IS NOT INITIAL AND oldpaymentterms IS NOT INITIAL
                              THEN  'Payment Terms Changed from' && | | && oldpaymentterms && | to | && newpaymentterms ).

        incoterm    = COND #( WHEN  newincoterms <> oldincoterms AND newincoterms IS NOT INITIAL AND oldincoterms IS NOT INITIAL
                              THEN  'Inco Terms Changed from' && | | && oldincoterms && | to | && newincoterms ).

      ENDIF.

    ELSEIF  level = 'L3'.

      wa_output-line = '<HTML><BODY><TABLE BORDER="2" ><TR><TH>SO</TH><TH>Customer code</TH>'.
      APPEND wa_output TO mailcontent.
      CLEAR: wa_output.

      wa_output-line = '<TH>Customer Name</TH><TH>Quantity</TH><TH>Quantity Unit</TH><TH>SO Payment Terms</TH><TH>Customer Payment Terms</TH>'.
      APPEND wa_output TO mailcontent.
      CLEAR: wa_output.


      IF salesdetails-lastchangedbyuser IS NOT INITIAL.
        wa_output-line = '<TH>Created By</TH><TH>Created On</TH><TH>Changed By</TH><TH>Changed On</TH></TR>'.
        APPEND wa_output TO mailcontent.
        CLEAR: wa_output.
      ELSE.
        wa_output-line = '<TH>Created By</TH><TH>Created On</TH></TR>'.
        APPEND wa_output TO mailcontent.
        CLEAR: wa_output.
      ENDIF.


      IF salesdetails-lastchangedatetime IS NOT INITIAL.
        changed_on = 'DT:' && | | && last_cg_on+6(2) && |.| && last_cg_on+4(2) && |.| &&  last_cg_on+0(4)
                     && | | &&  |TM:| && | | && last_cg_on+8(2) && |:| && last_cg_on+10(2) && |:| &&  last_cg_on+12(2) .
      ENDIF.
      IF salesdetails-creationdate IS NOT INITIAL AND   salesdetails-creationtime IS NOT INITIAL.
        created_on = 'DT:' && | | && salesdetails-creationdate+6(2) && |.| && salesdetails-creationdate+4(2) && |.| &&  salesdetails-creationdate+0(4)
                     && | | &&  |TM:| && | | && salesdetails-creationtime+0(2) && |:| && salesdetails-creationtime+2(2) && |:| &&  salesdetails-creationtime+4(2) .
      ENDIF.

      soldtoparty_text        = soldtoparty.
      customercode_text       = customercode.
      quantity_text           = quantity.
      quantityunit_text       = quantityunit.
*      valueofmaterial_text    = valueofmaterial.
      saleorder_text = |{ salesorder ALPHA = OUT }|.
      CONCATENATE '<TR><TD>' saleorder_text '</TD><TD>' customercode_text '</TD><TD>'  customer_name '</TD><TD>' quantity_text '</TD><TD>'
                             quantityunit_text '</TD><TD>' salesdetails-customerpaymentterms '</TD><TD>' custpayment-paymentterms '</TD>'  INTO wa_output-line.
      APPEND wa_output TO mailcontent.
      CLEAR:wa_output.

      IF salesdetails-lastchangedbyuser IS NOT INITIAL.
        CONCATENATE '<TD>' salesdetails-createdbyuser '</TD><TD>' created_on '</TD><TD>' salesdetails-lastchangedbyuser  '</TD><TD>' changed_on '</TD><TR>' INTO wa_output-line.
        APPEND wa_output TO mailcontent.
        CLEAR:wa_output.
      ELSE.
        CONCATENATE '<TD>' salesdetails-createdbyuser '</TD><TD>' created_on '</TD><TR>' INTO wa_output-line.
        APPEND wa_output TO mailcontent.
        CLEAR:wa_output.
      ENDIF.

      wa_output-line = '</TABLE></BODY></HTML>'. " Close HTML table
      APPEND wa_output TO mailcontent.
      CLEAR: wa_output.
    ENDIF.

    IF level = 'L2'.
      IF ( newnetvalue <> oldnetvalue OR
           newportofloading <> oldportofloading OR
           newportofunloading <> oldportofunloading OR
           newincoterms <> oldincoterms OR
           newpaymentterms <> oldpaymentterms ).

        updsodetails = TEXT-030.
      ENDIF.

      customfield = COND #( WHEN  newportofloading <> oldportofloading
                       THEN  'Port of Loading Changed from' && | | && oldportofloading && | to | && newportofloading ).

      customfield = customfield && COND #( WHEN  oldportofunloading <> newportofunloading
                       THEN  'Port of Loading Changed from' && | | && oldportofunloading && | to | && newportofunloading ).

      paymentterm = COND #( WHEN  newpaymentterms <> oldpaymentterms AND newpaymentterms IS NOT INITIAL AND oldpaymentterms IS NOT INITIAL
                            THEN  'Payment Terms Changed from' && | | && oldpaymentterms && | to | && newpaymentterms ).

      incoterm    = COND #( WHEN  newincoterms <> oldincoterms  AND newincoterms IS NOT INITIAL AND oldincoterms IS NOT INITIAL
                            THEN  'Inco Terms Changed from' && | | && oldincoterms && | to | && newincoterms ).

      sellingprice = COND #( WHEN  newnetvalue <> oldnetvalue  AND newnetvalue IS NOT INITIAL AND oldnetvalue IS NOT INITIAL
                            THEN  'selling Price Changed from' && | | && oldnetvalue && | to | && newnetvalue ).

      quantity_change = COND #( WHEN  new_qty <> old_qty  AND new_qty IS NOT INITIAL AND old_qty IS NOT INITIAL
                           THEN  'Sales Quantity Changed from' && | | && new_qty  && | to | && old_qty ).
      CONDENSE:sellingprice.

    ENDIF.
    IF paymentterm IS NOT INITIAL.
      paymentterm = 'Note:' && | | && paymentterm.
    ELSEIF incoterm IS NOT INITIAL.
      incoterm = 'Note:' && | | && incoterm.
    ELSEIF sellingprice IS NOT INITIAL.
      sellingprice = 'Note:' && | | && sellingprice.
    ENDIF.

    "sales order item details
    SELECT salesdocument,
           salesdocumentitem FROM i_salesdocumentitem INTO TABLE @DATA(salesitem)
                             WHERE salesdocument = @salesorder.
    IF sy-subrc = 0.
      SORT salesitem ASCENDING BY salesdocumentitem.
    ENDIF.
*    "sales order details
    IF salesdetails IS NOT INITIAL.
      " Get Users / Approvers
      SELECT zemailid ,zusername FROM zsd_approvals INTO TABLE @DATA(app_to_mail)
                                      WHERE vkorg = @salesdetails-salesorganization
                                        AND auart = @salesdetails-salesdocumenttype
                                        AND zlevelapproval = @level.
      IF sy-subrc  = 0.
        SELECT zemailid  FROM @app_to_mail AS app_to_mail INTO TABLE @to_mail.
      ENDIF.
    ENDIF.

    saleorder_text = |{ salesorder ALPHA = OUT }|.
    mail_body =   VALUE #( ( line = | { TEXT-006 } | && VALUE #( app_to_mail[ 1 ]-zusername OPTIONAL ) && |,| ) "Hi
                             ( line = linebreak )
                             ( line =  TEXT-039 && | | && saleorder_text )
                             ( line = linebreak )
                             ( line = TEXT-040  )
                             ( line = linebreak ) ).

    mail_body =   VALUE #( BASE mail_body ( LINES OF mailcontent ) ).

    IF ( level = 'L1' OR   level = 'L2' ).
      mail_body = VALUE #( BASE mail_body
     ( line = COND #( WHEN paymentterm  IS NOT INITIAL THEN paymentterm  ) )
     ( line = COND #( WHEN paymentterm  IS NOT INITIAL THEN  linebreak   ) )

     ( line = COND #( WHEN incoterm     IS NOT INITIAL THEN incoterm     ) )
      ( line = COND #( WHEN incoterm  IS NOT INITIAL THEN  linebreak   ) )

     ( line = COND #( WHEN sellingprice IS NOT INITIAL THEN sellingprice ) )
        ( line = COND #( WHEN incoterm  IS NOT INITIAL THEN  linebreak   ) ) ).
    ENDIF.

    mail_body = VALUE #( BASE mail_body
                       ( line = TEXT-008 ) ). "  Thanks You.


    IF ( level = 'L1' OR   level = 'L2' ).

      outputparams-preview = ' '.
      outputparams-nodialog = abap_true.
      outputparams-getpdf = abap_true.
*& Job open
      CALL FUNCTION 'FP_JOB_OPEN'
        CHANGING
          ie_outputparams = outputparams
        EXCEPTIONS
          cancel          = 1
          usage_error     = 2
          system_error    = 3
          internal_error  = 4
          OTHERS          = 5.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      IF level = 'L1' OR level = 'L2'.
        form_name = 'ZSD_F001_COMMERCIALINVOICE_FSD'.
*      ELSEIF  level = 'L3'.
*        form_name = 'ZSD_F002_SALE_ORDER_QA_PRD'.
      ENDIF.

*& get fm name
      TRY.
          CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
            EXPORTING
              i_name     = form_name
            IMPORTING
              e_funcname = function_name.
        CATCH cx_fp_api_usage cx_fp_api_repository cx_fp_api_internal.
      ENDTRY.

      salesdocument = salesorder.
      salesdocumentitem = VALUE #( salesitem[ 1 ]-salesdocumentitem OPTIONAL )."'10'.

**      LOOP AT salesitem ASSIGNING FIELD-SYMBOL(<item>) .
      IF function_name IS NOT INITIAL AND ( level = 'L1' OR  level = 'L2' ) .


        CALL FUNCTION function_name "'/1BCDWB/SM00000042'
          EXPORTING
            /1bcdwb/docparams  = docparams
            iv_salesdocno      = salesdocument
            iv_salesitem       = salesdocumentitem
          IMPORTING
            /1bcdwb/formoutput = formoutput
          EXCEPTIONS
            usage_error        = 1
            system_error       = 2
            internal_error     = 3
            OTHERS             = 4.
      ENDIF.

*& Job close
      CALL FUNCTION 'FP_JOB_CLOSE'
        EXCEPTIONS
          usage_error    = 1
          system_error   = 2
          internal_error = 3
          OTHERS         = 4.

**      CREATE OBJECT lo_pdf_merger.

*** Add documents to attribute table of PDF merger
**      LOOP AT lt_formoutput INTO DATA(form).
**        lo_pdf_merger->add_document( form ).
**      ENDLOOP.

*** Call kernel method to do the merge of the specified files.
**      lo_pdf_merger->merge_documents( IMPORTING merged_document = merged_document
**                                                             rc = rc ).

**      REFRESH: lt_formoutput.
**      formoutput-pdf = merged_document.
      saleorder_text =  |{ salesorder ALPHA = OUT }|.

      TRY.
          zcl_sd_001_so_approval=>email_attachment(
            EXPORTING
              it_email_ids_to = to_mail
              is_mailinfo     =  VALUE #( message_body     = mail_body
                                          formoutput       = formoutput
                                          subject = 'Sales Order Approval -' && | | && saleorder_text
                                          pdfname =  salesorder
                                          sender_mail = 'Saakshi.Deshpande@bs.nttdata.com' )
              it_email_ids_cc = cc_mailid
              iv_pdfflag      = abap_true                 " Single-Character Flag
            IMPORTING
              ev_success_flag =   DATA(success_flag)        " Boolean Variable (X = True, - = False, Space = Unknown)
              ev_message      =   DATA(message) ) .

        CATCH cx_root.
      ENDTRY.
    ELSEIF level = 'L3' .

      "Trigger Email
      TRY.
          zpmcomponentapproval=>email(
            EXPORTING
              sender      = 'Saakshi.Deshpande@bs.nttdata.com'              " E-Mail Address
              subject     = TEXT-041 &&  | | &&  salesorder
              emailids_to = to_mail"emailids_to                                   " TO
              email_body  = mail_body                                  " Email Body  " CC
            IMPORTING
              sucess_flag = DATA(flag)
              message     = message ).
        CATCH cx_root.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD email_attachment.
    "Data Declaration for Mail
    DATA : lo_send_request  TYPE REF TO cl_bcs,
           lo_document      TYPE REF TO cl_document_bcs,
           lo_recipient     TYPE REF TO if_recipient_bcs,
           lo_cc_recipient  TYPE REF TO if_recipient_bcs,
           lo_document_bcs  TYPE REF TO cx_document_bcs,
           lo_bcs_exception TYPE REF TO cx_bcs,
           lv_sent_to_all   TYPE os_boolean,
           lv_pdf_size      TYPE so_obj_len,
           lv_objdes        TYPE sood-objdes,
           lv_pdf_file      TYPE fpformoutput,
           lv_pdf_content   TYPE solix_tab,
           lv_obj_des       TYPE so_obj_des,
           lv_email_to      TYPE ad_smtpadr,
           lv_email_cc      TYPE ad_smtpadr.

    "Constant Declaration
    CONSTANTS: lc_objtp  TYPE soodk-objtp VALUE 'PDF',
               lc_obj_tp TYPE so_obj_tp   VALUE 'HTM'.


    TRY.
        "Create Send request
        lo_send_request = cl_bcs=>create_persistent(  ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        ev_message = lx_send_req_bcs->get_text( ).
    ENDTRY.

    TRY.
        "Set sender to send request
        DATA(lo_sender) = cl_cam_address_bcs=>create_internet_address( is_mailinfo-sender_mail ).
        lo_send_request->set_sender( i_sender = lo_sender ).

      CATCH cx_address_bcs INTO DATA(lx_address_bcs).
      CATCH cx_send_req_bcs INTO DATA(lx_address_req).
        ev_message = lx_address_bcs->get_text( ).
    ENDTRY.
    TRY.
*        IF iv_pdfflag = abap_true. "if flag is X then only pdf will be sent in the mail
        "PDF Name
        lv_objdes = is_mailinfo-pdfname.
        "Mail Subject
        lv_obj_des = is_mailinfo-subject.
        "add document
        "get PDF xstring and convert it to BCS format
        lv_pdf_size = xstrlen( is_mailinfo-formoutput-pdf ).                     "Converting PDF to binary
        lv_pdf_content = cl_document_bcs=>xstring_to_solix( ip_xstring = is_mailinfo-formoutput-pdf ).
        lo_document = cl_document_bcs=>create_document( i_type    = lc_obj_tp
                                                        i_text    = is_mailinfo-message_body
                                                        i_length  = lv_pdf_size
                                                        i_subject = lv_obj_des ).
        TRY.
            lo_document->add_attachment(  i_attachment_type    = lc_objtp             "Adding PDF as atachment
                                          i_attachment_subject = lv_objdes
                                          i_att_content_hex    = lv_pdf_content ).
          CATCH cx_document_bcs INTO lo_document_bcs.
        ENDTRY.

        TRY.
*==Add document to send request
            lo_send_request->set_document( lo_document ).
          CATCH cx_address_bcs INTO lx_address_bcs.
          CATCH cx_send_req_bcs INTO lx_address_req.
        ENDTRY.
*        ENDIF.

        LOOP AT it_email_ids_to INTO DATA(ls_email_id_to).
          lv_email_to = ls_email_id_to.
          TRY.
*& Set recipient
              lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email_to ).
            CATCH cx_address_bcs INTO lx_address_bcs.
              ev_message = lx_address_bcs->get_text( ).
          ENDTRY.

          TRY.
              lo_send_request->add_recipient( EXPORTING i_recipient = lo_recipient i_express = abap_true ).
            CATCH cx_send_req_bcs INTO lx_send_req_bcs.
              ev_message = lx_send_req_bcs->get_text( ).
          ENDTRY.
        ENDLOOP.

        "CC mailids
        LOOP AT it_email_ids_cc INTO DATA(ls_email_id_cc).
          lv_email_cc = ls_email_id_cc.
          TRY.
*& Set recipient
              lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email_cc ).
            CATCH cx_address_bcs INTO lx_address_bcs.
              ev_message = lx_address_bcs->get_text( ).
          ENDTRY.

          TRY.
              lo_send_request->add_recipient( EXPORTING i_recipient = lo_recipient i_copy = 'C'  ).
            CATCH cx_send_req_bcs INTO lx_send_req_bcs.
              ev_message = lx_send_req_bcs->get_text( ).
          ENDTRY.
        ENDLOOP.

        "send document
        lo_send_request->set_send_immediately( i_send_immediately = abap_true ).
        lv_sent_to_all = lo_send_request->send( i_with_error_screen = abap_true ).
        IF lv_sent_to_all = abap_true.
          COMMIT WORK.
          ev_success_flag =  /isdfps/cl_const_abc_123=>gc_s.
          ev_message = 'Mail sent successfully.'.
        ENDIF.
        "exception handling
      CATCH cx_bcs INTO lo_bcs_exception.
        ev_success_flag =  /isdfps/cl_const_abc_123=>gc_e.
        ev_message = 'failed to sendmail.'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*******************************************************************************************************************************************

Includes 

*&---------------------------------------------------------------------*
*& Include          ZSD_I001_SO_APPROVAL
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* Modification History
*-----------------------------------------------------------------------
* Modified by:    NTT_ABAP2 (Mukhesh)
* Date:
* Transport:      <Transport Request #>
* Description:
*<Description of the change (or the source for the initial creation if a
* template or SAP program was used as a starting point>
*-----------------------------------------------------------------------
*& Trigger Sales Order Workflow
*IF sy-uname = 'NTT_ABAP6'.

  DATA:workflowdetails TYPE zswfdetails,
       new_qty         TYPE  vbap-kwmeng,
       old_qty         TYPE  vbap-kwmeng.

  TRY.
      zcl_sd_001_so_approval=>getvaln( EXPORTING salesdoctype = vbak-auart                " Sales Organization
                                                 salesorg     = vbak-vkorg                " Sales Document Type
                                                 customer  = vbak-kunnr
                                                 division  = vbak-spart
                                                 distributionchannel = vbak-vtweg
                                       IMPORTING approvals    = DATA(approvals)
                                                 flag         = DATA(triggerwf)
                                                 payment      = DATA(payment_term)
                                                 incoterm     = DATA(incoterm) ).
    CATCH cx_root.
  ENDTRY.
  CHECK triggerwf = abap_true.

  IF sy-tcode = 'VA01' .
*    SELECT SINGLE customerpaymentterms FROM i_customersalesarea INTO @DATA(payment_term)
*                                       WHERE salesorganization = @vbak-vkorg AND
*                                             distributionchannel = @vbak-vtweg AND
*                                             division = @vbak-spart AND
*                                             customer = @vbak-kunnr.

    workflowdetails = VALUE #( salesorder   = vbak-vbeln
                               salesorg     = vbak-vkorg
                               saledoctype  = vbak-auart
                               initiator    = vbak-ernam
                               rejectedfrom = ''
                               soldtoparty     = kuagv-kunnr
                               customercode    = vbak-kunnr
                               quantity        = REDUCE menge_d( INIT qty TYPE menge_d
                                                    FOR itemso IN xvbap[] "#EC CI_STDSEQ
                                                    NEXT qty  =   qty + itemso-kwmeng )
                               quantityunit    = xvbap-vrkme
                               valueofmaterial = REDUCE netwr_ap( INIT netval TYPE netwr_ap
                                                    FOR itemso IN xvbap[] "#EC CI_STDSEQ
                                                   NEXT netval =   netval + itemso-netwr )

                               newpaymentterms     =  xvbkd-zterm

                               newincoterms        =  xvbkd-inco1


                               oldpaymentterms     = payment_term

                               oldincoterms        = incoterm ) .
*                               payment = COND #( WHEN payment_term <> xvbkd-zterm AND payment_term IS NOT INITIAL
*                                                 THEN 'Payment Terms Changed from' && payment_term && 'to' && xvbkd-zterm
*                                                 ELSE '' ) ) .
    TRY.
        CALL FUNCTION 'ZSD_001_WF_SEND_SO' IN BACKGROUND TASK
          EXPORTING
            workflowdetails = workflowdetails.
      CATCH cx_root.
    ENDTRY.
  ELSE.

    DATA(item) = xvbap[].
    DATA(item_change) = yvbap[].
*    DATA(oldgt) = ykomv[].
    new_qty = REDUCE #( INIT lv_qtynw TYPE vbap-kwmeng
                                        FOR nw_qty IN item "#EC CI_STDSEQ
                                       WHERE ( updkz  <> /isdfps/cl_const_abc_123=>gc_d  )
                                        NEXT lv_qtynw =   lv_qtynw + nw_qty-kwmeng ).

    old_qty = REDUCE #( INIT lv_qtyold TYPE vbap-kwmeng
                                       FOR lol_qty IN item_change "#EC CI_STDSEQ
                                      WHERE ( updkz  <> /isdfps/cl_const_abc_123=>gc_d  )
                                       NEXT lv_qtyold =   lv_qtyold + lol_qty-kwmeng ).

*    IF ( vbak-auart = 'ZDOM' OR  vbak-auart = 'ZDEE' OR  vbak-auart = 'ZMER' ).
*
*      DATA(old_gt) = REDUCE kbetr( INIT lv_old TYPE ykomv-kbetr
*                                        FOR old_gtcal IN oldgt "#EC CI_STDSEQ
*                                       WHERE (  kschl = 'ZBAS' OR  kschl =  'Z004' OR  kschl = 'Z005' OR  kschl = 'Z007' OR  kschl =  'ZD00'
*                                              OR  kschl = 'JOIG' OR  kschl = 'JOCG' OR  kschl = 'JOSG'
*                                              OR  kschl = 'JOUG' OR  kschl =  'ZMIS' OR  kschl = 'ZMIS'  )
*                                        NEXT lv_old =   lv_old + old_gtcal-kbetr ).
*
*    ELSEIF ( vbak-auart = 'ZSEZ' OR  vbak-auart = 'ZEXP' OR  vbak-auart = 'ZDSF' OR  vbak-auart =  'ZESF' ).
*      old_gt = REDUCE kbetr( INIT lv_old TYPE ykomv-kbetr
*                                              FOR old_gtcal IN oldgt "#EC CI_STDSEQ
*                                             WHERE (  kschl = 'ZBAS' OR kschl =  'Z004' OR kschl = 'Z005' OR kschl = 'Z007' OR kschl = 'ZD00' )
*                                              NEXT lv_old =   lv_old + old_gtcal-kbetr ).
*    ENDIF.




*& Re Trigger the workflow if user changes the netvalue or qty or addition and delition of item
*  IF ( line_exists( lt_item[ updkz  = /isdfps/cl_const_abc_123=>gc_i ] ) OR  line_exists( lt_item[ updkz  = /isdfps/cl_const_abc_123=>gc_d ] )
*     OR lv_flag = abap_true OR yvbak-netwr <> xvbak-netwr  ).


*ZDOM Domestic  Gross Value in SOC
*ZSEZ	SEZ	Taxable Value
*ZDEE DEEmed  Gross Value in SOC
*ZEXP Export  Taxable Value
*ZMER Marchent  Gross Value in SOC
*ZDSF,ZESF  Sample  Based on Process



    IF ( yvbak-netwr <> xvbak-netwr OR  yvbak-zz1_prtld_sdh <> xvbak-zz1_prtld_sdh
                                    OR  yvbak-zz1_prtuld_sdh <> xvbak-zz1_prtuld_sdh
                                    OR  yvbkd-zterm <> xvbkd-zterm
                                    OR  yvbkd-inco1 <> xvbkd-inco1
                                    OR old_qty <> new_qty ).
      TRY.

          zcl_sd_001_so_approval=>saleord_recheck( EXPORTING salesorder = vbak-vbeln                       " Sales Document
                                                   IMPORTING qtychange  = DATA(qtycheck)
                                                             delivery   = DATA(delivery)
                                                   RECEIVING recheck  = DATA(recheck) ).                   " Single-Character Flag
        CATCH cx_root.
      ENDTRY.

      "after all approvals and after creating the delivery also if user changed only quantity then only trigger the workflow otherwise no change at all
      IF delivery IS NOT INITIAL AND  old_qty <> new_qty.
        recheck = abap_false.
      ELSEIF delivery IS NOT INITIAL AND  old_qty = new_qty..
        recheck = abap_true.
      ENDIF.



      IF recheck <> abap_true.
        workflowdetails  = VALUE #( salesorder   = vbak-vbeln
                                                           salesorg     = vbak-vkorg
                                                           saledoctype  = vbak-auart
                                                           initiator    = vbak-ernam
                                                           rejectedfrom = ''
                                                           newnetvalue         = COND #( WHEN xvbak-netwr <> yvbak-netwr
                                                                                         THEN xvbak-netwr )

                                                           newportofloading    = COND #( WHEN xvbak-zz1_prtld_sdh <> yvbak-zz1_prtld_sdh
                                                                                         THEN xvbak-zz1_prtld_sdh )

                                                           newportofunloading  = COND #( WHEN xvbak-zz1_prtuld_sdh <> yvbak-zz1_prtuld_sdh
                                                                                         THEN xvbak-zz1_prtuld_sdh )

                                                           newpaymentterms     = COND #( WHEN yvbkd-zterm <> xvbkd-zterm
                                                                                         THEN xvbkd-zterm )

                                                           newincoterms        = COND #( WHEN xvbkd-inco1 <> yvbkd-inco1
                                                                                         THEN xvbkd-inco1 )

                                                           oldportofloading    = COND #( WHEN yvbak-zz1_prtld_sdh <> xvbak-zz1_prtld_sdh
                                                                                         THEN yvbak-zz1_prtld_sdh )

                                                           oldportofunloading  = COND #( WHEN xvbak-zz1_prtuld_sdh <> yvbak-zz1_prtuld_sdh
                                                                                         THEN yvbak-zz1_prtuld_sdh )

                                                           oldpaymentterms     = COND #( WHEN yvbkd-zterm <> xvbkd-zterm
                                                                                         THEN yvbkd-zterm )

                                                           oldincoterms        = COND #( WHEN yvbkd-inco1 <> xvbkd-inco1
                                                                                         THEN yvbkd-inco1 )

                                                           oldnetvalue         = COND #( WHEN yvbak-netwr  <> xvbak-netwr
                                                                                         THEN yvbak-netwr  )

                                                          soldtoparty          = kuagv-kunnr
                                                          customercode         = vbak-kunnr
*                                                         material             =
                                                          quantity             =  REDUCE menge_d( INIT qty TYPE menge_d
                                                                                   FOR itemcg IN xvbap[] "#EC CI_STDSEQ
                                                                                   NEXT qty  =   qty + itemcg-kwmeng )
                                                          quantityunit         = xvbap-vrkme
*                                                          valueofmaterial      = old_gt
                                                          valueofmaterial      = REDUCE netwr_ap( INIT netval TYPE netwr_ap
                                                                                    FOR itemcg IN xvbap[] "#EC CI_STDSEQ
                                                                                   NEXT netval =   netval + itemcg-netwr )
                                                          old_qty              = old_qty
                                                          new_qty              = new_qty                          ).



        TRY.
            CALL FUNCTION 'ZSD_001_WF_SEND_SO' IN BACKGROUND TASK
              EXPORTING
                workflowdetails = workflowdetails.
          CATCH cx_root.
        ENDTRY.

      ENDIF.
    ENDIF.

************************************************************************************************************************************

*&---------------------------------------------------------------------*
*& Include          ZSD_I001_SO_APPROVE_VLN
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------*
* Modification History                                                  *
*-----------------------------------------------------------------------*
* Modified by:    NTT_ABAP2 (Mukhesh)                                   *
* Date:           <Date>                                                *
* Transport:      <Transport Request #>                                 *
* Description:    Gst Invoice Number Range                              *
*<Description of the change (or the source for the initial creation if a*
* template or SAP program was used as a starting point>                 *
*-----------------------------------------------------------------------*
*& Sales order approval validation
CHECK ( vbak-auart =  'ZDOM' OR  vbak-auart =  'ZEXP' OR vbak-auart =  'ZDEE' OR vbak-auart =  'ZSEZ' OR
vbak-auart =  'ZDSF' OR  vbak-auart =  'ZESF' OR vbak-auart =  'ZSCR' OR vbak-auart =  'ZOR' OR  vbak-auart =  'ZRSF'  OR
vbak-auart = 'ZDSF' OR vbak-auart = 'ZESF' OR	 vbak-auart = 'ZRSF' OR vbak-auart = 'ZSEZ' OR
vbak-auart = 'ZMSF' OR vbak-auart = 'ZUSF' OR vbak-auart = 'ZSCR' OR  vbak-auart = 'ZZSF' OR  vbak-auart = 'ZEXP' OR
vbak-auart = 'ZDOM' OR  vbak-auart = 'ZEOU' OR vbak-auart = 'ZMER' OR  vbak-auart = 'ZRMP' ).


*SELECT zusername,
*       auart,
*       vkorg FROM zsd_approvals INTO TABLE @DATA(appr)
*                                   WHERE auart = @vbak-auart
*                                     AND vkorg = @vbap-vkorg.
**& Get workflow log details for so approval
*IF sy-subrc = 0 AND appr IS NOT INITIAL.
DATA(salesorder) = COND #( WHEN vbak-vbeln IS NOT INITIAL
                        THEN vbak-vbeln
                        ELSE lips-vgbel ).
SELECT SINGLE zlevel,
              zuid1,
              zuid2,
              zuid3,
              vbeln,
              zsts1,
              zsts2,
              zsts3 FROM zsd_so_wf_log INTO  @DATA(logdetail) WHERE  vbeln = @salesorder.

IF sy-subrc  = 0.
ELSE.
ENDIF.

" Send error message if sale order is not approved

*IF  logdetail-zlevel = 'L1' AND logdetail-zsts1 <> zcl_sd_001_so_approval=>gc_approved.
*  MESSAGE e061(zsynth_sd) WITH logdetail-zlevel logdetail-zuid1. . "Sale order Approval is pending
*ELSEIF logdetail-zlevel = 'L2' AND logdetail-zsts2 <> zcl_sd_001_so_approval=>gc_approved .
*  MESSAGE e061(zsynth_sd) WITH logdetail-zlevel logdetail-zuid2. . "Sale order Approval is pending
*ELSEIF   logdetail-zlevel = 'L3' AND  logdetail-zsts3 <> zcl_sd_001_so_approval=>gc_approved.
*  MESSAGE e061(zsynth_sd) WITH logdetail-zlevel logdetail-zuid3. . "Sale order Approval is pending
*ENDIF.
IF logdetail-zlevel = 'L1' AND logdetail-zsts1 <> zcl_sd_001_so_approval=>gc_approved.
  MESSAGE e061(zsynth_sd) WITH 'L1' logdetail-zuid1. "Sales Order Approval is pending

ELSEIF logdetail-zlevel = 'L2' .

  IF logdetail-zsts1 <> zcl_sd_001_so_approval=>gc_approved.
    MESSAGE e061(zsynth_sd) WITH 'L1' logdetail-zuid1. "Sales Order Approval is pending
  ELSEIF logdetail-zsts2 <> zcl_sd_001_so_approval=>gc_approved.
    MESSAGE e061(zsynth_sd) WITH 'L2' logdetail-zuid2. "Sales Order Approval is pending
  ENDIF.

ELSEIF logdetail-zlevel = 'L3' AND logdetail-zsts3 <> zcl_sd_001_so_approval=>gc_approved.

  IF logdetail-zsts1 <> zcl_sd_001_so_approval=>gc_approved.
    MESSAGE e061(zsynth_sd) WITH 'L1' logdetail-zuid1. "Sales Order Approval is pending
  ELSEIF logdetail-zsts2 <> zcl_sd_001_so_approval=>gc_approved.
    MESSAGE e061(zsynth_sd) WITH 'L2' logdetail-zuid2. "Sales Order Approval is pending
  ELSEIF logdetail-zsts3 <> zcl_sd_001_so_approval=>gc_approved.
    MESSAGE e061(zsynth_sd) WITH 'L3' logdetail-zuid3. "Sales Order Approval is pending
  ENDIF.
ENDIF.
****************************************************************************************************************************************

*&----------------------------------------------------------------------*
*& Include          ZSD_I002_GENERATE_INVOICE_NO                        *
*&----------------------------------------------------------------------*
*-----------------------------------------------------------------------*
* Modification History                                                  *
*-----------------------------------------------------------------------*
* Modified by:    NTT_ABAP2 (Mukhesh)                                   *
* Date:           <Date>                                                *
* Transport:      <Transport Request #>                                 *
* Description:    Gst Invoice Number Range                              *
*<Description of the change (or the source for the initial creation if a*
* template or SAP program was used as a starting point>                 *
*-----------------------------------------------------------------------*
" Sales order approval validation
SELECT zusername,
       auart,
       vkorg FROM zsd_approvals INTO TABLE @DATA(appr)
                                   WHERE auart = @vbak-auart
                                     AND vkorg = @vbak-vkorg.
" Get workflow log details for so approval
IF sy-subrc = 0 AND appr IS NOT INITIAL.
  SELECT SINGLE zlevel,
                zuid1,
                vbeln,
                zsts1,
                zsts2,
                zsts3,
                zuid2,
                zuid3 FROM zsd_so_wf_log INTO  @DATA(logdetail) WHERE  vbeln = @vbak-vbeln.

  IF sy-subrc  = 0.
  ELSE.
  ENDIF.

  " Send error message if sale order is not approved
  IF logdetail-zlevel = 'L1' AND logdetail-zsts1 <> zcl_sd_001_so_approval=>gc_approved.
    MESSAGE e061(zsynth_sd) WITH 'L1' logdetail-zuid1. "Sales Order Approval is pending

  ELSEIF logdetail-zlevel = 'L2' .

    IF logdetail-zsts1 <> zcl_sd_001_so_approval=>gc_approved.
      MESSAGE e061(zsynth_sd) WITH 'L1' logdetail-zuid1. "Sales Order Approval is pending
    ELSEIF logdetail-zsts2 <> zcl_sd_001_so_approval=>gc_approved.
      MESSAGE e061(zsynth_sd) WITH 'L2' logdetail-zuid2. "Sales Order Approval is pending
    ENDIF.

  ELSEIF logdetail-zlevel = 'L3' AND logdetail-zsts3 <> zcl_sd_001_so_approval=>gc_approved.

    IF logdetail-zsts1 <> zcl_sd_001_so_approval=>gc_approved.
      MESSAGE e061(zsynth_sd) WITH 'L1' logdetail-zuid1. "Sales Order Approval is pending
    ELSEIF logdetail-zsts2 <> zcl_sd_001_so_approval=>gc_approved.
      MESSAGE e061(zsynth_sd) WITH 'L2' logdetail-zuid2. "Sales Order Approval is pending
    ELSEIF logdetail-zsts3 <> zcl_sd_001_so_approval=>gc_approved.
      MESSAGE e061(zsynth_sd) WITH 'L3' logdetail-zuid3. "Sales Order Approval is pending
    ENDIF.

*    MESSAGE e061(zsynth_sd) WITH logdetail-zlevel logdetail-zuid3. "Sales Order Approval is pending
  ENDIF.

ENDIF.
********************************************************************************************************************************************

