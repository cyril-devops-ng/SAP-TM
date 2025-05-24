TYPE-POOL zotr.
TYPES: BEGIN OF zotr_ty_items,
         fwo      TYPE /bobf/conf_key,
         manifest TYPE ztm039_manif_itm_tt,
         discharg TYPE ztm039_disch_itm_tt,
       END OF zotr_ty_items.
TYPES zotr_ty_items_tt TYPE STANDARD TABLE OF zotr_ty_items.
TYPES: BEGIN OF zotr_ty_fwo,
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
       END OF zotr_ty_fwo.
TYPES: BEGIN OF zotr_ty_cargo_summ,
         zzcust        TYPE /scmtms/d_trqitm-zzcust,
         zzpol         TYPE /scmtms/d_trqitm-zzpol,
         cargo_summary TYPE zyl028s_cargo_summary,
       END OF zotr_ty_cargo_summ.
TYPES zotr_ty_cargo_summary_tt TYPE STANDARD TABLE OF zotr_ty_cargo_summ.
TYPES zotr_ty_fwo_tt           TYPE STANDARD TABLE OF zotr_ty_fwo.
TYPES: BEGIN OF zotr_t_cargo_details,
         trq_id          TYPE /scmtms/trq_id,
         cargo_exists    TYPE flag,
         cargo_summary   TYPE zyl028_cargo_summary_tt,
         cargo_container TYPE zyl028_cargo_summary_tt,
       END OF zotr_t_cargo_details.
TYPES zotr_tt_cargo_details TYPE TABLE OF zotr_t_cargo_details WITH DEFAULT KEY.
TYPES  BEGIN OF zotr_alv_output.
         INCLUDE TYPE ztm039s_header.
TYPES:   print  TYPE char4,
       END OF zotr_alv_output.
