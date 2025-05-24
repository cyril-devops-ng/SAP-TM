TYPE-POOL ZPDR .

TYPES: BEGIN OF zpdr_ty_items,
         fwo      TYPE /bobf/conf_key,
         manifest TYPE ztm049_manif_itm_tt,
         discharg TYPE ztm049_disch_itm_tt,
       END OF zpdr_ty_items,
       zpdr_ty_items_tt TYPE STANDARD TABLE OF zpdr_ty_items,

       BEGIN OF zpdr_ty_fwo,
         zzbolnumber       TYPE /scmtms/d_trqitm-zzbolnumber,
         zzcust            TYPE /scmtms/d_trqitm-zzcust,
         zzpol             TYPE /scmtms/d_trqitm-zzpol,
         db_key            TYPE /scmtms/d_trqrot-db_key,
         trq_id            TYPE /scmtms/d_trqrot-trq_id,
         tsp_id            TYPE /scmtms/d_trqrot-tsp_id,
         des_loc_id        TYPE /scmtms/d_trqrot-des_loc_id,
         src_loc_id        TYPE /scmtms/d_trqrot-src_loc_id,
         mc_name1          TYPE but000-mc_name1,
         trq_type          TYPE /scmtms/d_trqrot-trq_type,
         zzresource        TYPE /scmtms/d_trqrot-zzresource,
         quot_trq_key      TYPE /scmtms/d_trqrot-quot_trq_key,
         item_id           TYPE /scmtms/d_trqitm-item_id,
         item_key          TYPE /scmtms/d_trqitm-db_key,
         item_type         TYPE /scmtms/d_trqitm-item_type,
         item_cat          TYPE /scmtms/d_trqitm-item_cat,
         item_parent_key   TYPE /scmtms/d_trqitm-item_parent_key,
         job_order_no      TYPE /scmtms/tor_id,
         job_order_no_item TYPE /scmtms/item_id,
         fwq_id            TYPE /scmtms/d_trqrot-trq_id,
         zzmanifest_type   TYPE /scmtms/d_trqitm-zzmanifest_type,
         platenumber       TYPE /scmtms/d_trqitm-platenumber,
         item_descr        TYPE /scmtms/d_trqitm-item_descr,
         zzfrzone          TYPE /scmtms/d_trqitm-zzfrzone,
       END OF zpdr_ty_fwo,
       BEGIN OF zpdr_ty_cargo_summ,
         zzcust        TYPE /scmtms/d_trqitm-zzcust,
         zzpol         TYPE /scmtms/d_trqitm-zzpol,
         cargo_summary TYPE zyl028s_cargo_summary,
       END OF zpdr_ty_cargo_summ,

       BEGIN OF zpdr_ty_alv,
         fwq_id              TYPE /scmtms/trq_id,
         direction           TYPE ztm001e_manifest_type,
         vessel              TYPE ztm001e_resource,
         arrival             TYPE ztm005e_actual_date,
         departure           TYPE ztm005e_actual_date,
         carrier_name        TYPE bu_mcname1,
         base                TYPE /scmtms/destination_location,
         zzbolnumber         TYPE ztm001e_bl,
         zzcust              TYPE ztm001e_consignee,
         custname            TYPE name1_gp,
         zzpol               TYPE ztm001e_port_of_loading,
         platenumber_manif   TYPE /scmtms/resplatenr,
         qua_pcs_val_manif   TYPE /scmtms/qua_pcs_val,
         gro_wei_val_manif   TYPE /scmtms/qua_gro_wei_val,
         gro_vol_val_manif   TYPE /scmtms/qua_gro_vol_val,
         tonne_manif         TYPE /scmtms/qua_gro_wei_val,
         tures_tco_manif     TYPE /scmtms/equip_type,
         qua_pcs_val_disch   TYPE /scmtms/qua_pcs_val,
         date_disch          TYPE datum,
         tu_type_disch       TYPE /sapyl/e_tu_type,
         short_landed        TYPE i,
         over_landed         TYPE i,
         dest_loc            TYPE /scwm/lgpla,
         product_id          TYPE /scmtms/product_id,
         item_descr          TYPE /scmtms/item_description,
         zzfrzone            TYPE ZTM001E_FREE_ZONE,
         print               type char4,
       END OF zpdr_ty_alv,

         zpdr_ty_cargo_summary_tt TYPE STANDARD TABLE OF zpdr_ty_cargo_summ,
         zpdr_ty_fwo_tt           TYPE STANDARD TABLE OF zpdr_ty_fwo,
         zpdr_ty_alv_tt           TYPE STANDARD TABLE OF zpdr_ty_alv.

 DEFINE zpdr_hide_screen_fields.
  LOOP AT SCREEN.
    IF screen-group1 = &1.
      screen-input     = '0'.
      screen-invisible = '1'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
END-OF-DEFINITION.

DEFINE zpdr_show_screen_fields.
  LOOP AT SCREEN.
    IF screen-group1 = &1.
      screen-input     = '1'.
      screen-invisible = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
  END-OF-DEFINITION.
  DEFINE zpdr_thiscolumn.
  go_column ?= go_columns->get_column( <fs_component>-name ).
END-OF-DEFINITION.
  CONSTANTS zpdr_trq_id             TYPE string              VALUE 'TRQ_ID' ##NO_TEXT.
CONSTANTS zpdr_gc_arrival_event   TYPE ztm005e_event_code  VALUE 'Arrival' ##NO_TEXT.
CONSTANTS zpdr_gc_departure_event TYPE ztm005e_event_code  VALUE 'Departure' ##NO_TEXT.
CONSTANTS zpdr_gc_zexec_assoc_key TYPE /bobf/obm_assoc_key VALUE '5D786398C4FA1EDDAFEB80C51BD3B2BC' ##NO_TEXT.
types: zpdr_tm049s_t_header type table of ZTM049S_HEADER with default key.
