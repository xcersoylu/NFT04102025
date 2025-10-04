  METHOD if_http_service_extension~handle_request.
    TYPES: BEGIN OF ty_r002,
             companycode            TYPE ynft_t_r002-companycode,
             accountingdocument     TYPE ynft_t_r002-accountingdocument,
             fiscalyear             TYPE ynft_t_r002-fiscalyear,
             accountingdocumentitem TYPE ynft_t_r002-accountingdocumentitem,
             deliverydocument       TYPE ynft_t_r002-deliverydocument,
             deliverydocumentitem   TYPE ynft_t_r002-deliverydocumentitem,
             documentcurrenyamount  TYPE ynft_t_r002-documentcurrenyamount,
             documentcurrency       TYPE ynft_t_r002-documentcurrency,
             costtype               TYPE ynft_t_r002-costtype,
             accountnumber          TYPE ynft_t_r002-accountnumber,
             documenttype           TYPE ynft_t_dlv_cus-documenttype,
             deliveryquantity       TYPE i_deliverydocumentitem-actualdeliveryquantity,
             accountingdocument_inv TYPE ynft_t_r005-accountingdocument_inv,
             fiscalyear_inv         TYPE ynft_t_r005-fiscalyear_inv,
           END OF ty_r002.

    DATA: lt_r002       TYPE TABLE OF ty_r002,
          lt_ship_costs TYPE TABLE OF ty_r002.

    DATA(lv_request_body) = request->get_text( ).
    DATA(lv_get_method) = request->get_method( ).

    /ui2/cl_json=>deserialize( EXPORTING json = lv_request_body CHANGING data = ms_request ).

    "Önce tüm masraf belgelerini oku
    SELECT r002~companycode,
           r002~accountingdocument,
           r002~fiscalyear,
           r002~accountingdocumentitem,
           r002~deliverydocument,
           r002~deliverydocumentitem,
           r002~documentcurrenyamount,
           r002~documentcurrency,
           r002~costtype,
           r002~accountnumber,
           ynft_t_dlv_cus~documenttype,
           ynft_t_dlvit_cus~shipquantity AS deliveryquantity
       FROM ynft_t_r002 AS r002
       INNER JOIN ynft_t_dlv_cus ON ynft_t_dlv_cus~deliverydocument        = r002~deliverydocument
       LEFT JOIN ynft_t_dlvit_cus ON ynft_t_dlvit_cus~deliverydocument     = r002~deliverydocument
                                 AND ynft_t_dlvit_cus~deliverydocumentitem = r002~deliverydocumentitem
       INTO CORRESPONDING FIELDS OF TABLE @lt_r002.

    "Yükleme belgesi ile girilen masrafları çekme belgeleri üzerine dağıt
    IF lt_r002 IS NOT INITIAL.
      "Yükleme belgesi ile girilmiş masrafların çekme belgelerini bul
      DATA(lt_shipment)  = lt_r002.
      DELETE lt_shipment WHERE documenttype <> '1'.

      SELECT cus_clea~deliverydocument,
             cus_clea~deliverydocumentitem,
             lips~actualdeliveryquantity    AS clearencequantity,
             cus_clea~referencedocument     AS shipmentdocument,
             cus_clea~referencedocumentitem AS shipmentdocumentitem,
             cus_ship~shipquantity
        FROM @lt_shipment AS shipment
        INNER JOIN ynft_t_dlvit_cus AS cus_clea ON cus_clea~referencedocument     = shipment~deliverydocument
                                               AND cus_clea~referencedocumentitem = shipment~deliverydocumentitem
        INNER JOIN ynft_t_dlvit_cus AS cus_ship ON cus_ship~deliverydocument      = shipment~deliverydocument
                                               AND cus_ship~deliverydocumentitem  = shipment~deliverydocumentitem
        INNER JOIN i_deliverydocumentitem AS lips ON lips~deliverydocument     = cus_clea~deliverydocument
                                                 AND lips~deliverydocumentitem = cus_clea~deliverydocumentitem
        INTO TABLE @DATA(lt_shipment_clearence).

      LOOP AT lt_r002 INTO DATA(ls_r002) WHERE documenttype = '1'.
        "Yükleme ile girilen masrafları çekme belgeleri üzerine dağıt
        LOOP AT lt_shipment_clearence INTO DATA(ls_shipment_clearence) WHERE shipmentdocument     = ls_r002-deliverydocument
                                                                         AND shipmentdocumentitem = ls_r002-deliverydocumentitem.
          APPEND INITIAL LINE TO lt_ship_costs ASSIGNING FIELD-SYMBOL(<fs_ship_costs>).
          MOVE-CORRESPONDING ls_r002 TO <fs_ship_costs>.

          "Yükleme belgelerininin yerine çekme belgelerini yaz
          <fs_ship_costs>-deliverydocument     = ls_shipment_clearence-deliverydocument.
          <fs_ship_costs>-deliverydocumentitem = ls_shipment_clearence-deliverydocumentitem.
          <fs_ship_costs>-deliveryquantity     = ls_shipment_clearence-clearencequantity.

          "Yükleme ile girilen masraf tutarını yükleme miktarı/çekme miktarı oranına göre dağıt
          <fs_ship_costs>-documentcurrenyamount = <fs_ship_costs>-documentcurrenyamount * ( ls_shipment_clearence-clearencequantity / ls_r002-deliveryquantity ).
        ENDLOOP.

        IF sy-subrc IS INITIAL.
          DELETE lt_r002.
        ENDIF.
      ENDLOOP.
      IF lt_ship_costs[] IS NOT INITIAL.
        APPEND LINES OF lt_ship_costs TO lt_r002.
        SORT lt_r002 BY companycode accountingdocument fiscalyear accountingdocumentitem.
        DELETE ADJACENT DUPLICATES FROM lt_r002 COMPARING companycode accountingdocument fiscalyear accountingdocumentitem.
      ENDIF.
    ENDIF.

    "Servisten teslimat geldiyse filtreleme yap
    IF ms_request-deliverydocument IS NOT INITIAL.
      DELETE lt_r002 WHERE deliverydocument <> |{ ms_request-deliverydocument ALPHA = IN }|.
    ENDIF.

    "Tüm masrafların dosya kapama kayıtlarını oku
    SELECT r005~companycode,
           r005~accountingdocument,
           r005~fiscalyear,
           r005~accountingdocumentitem,
           r005~deliverydocument,
           r005~deliverydocumentitem,
           r005~amount,
           r005~accountingdocument_inv,
           r005~fiscalyear_inv
      FROM @lt_r002 AS r002
      INNER JOIN ynft_t_r005 AS r005 ON r002~companycode            = r005~companycode
                                    AND r002~accountingdocument     = r005~accountingdocument
                                    AND r002~fiscalyear             = r005~fiscalyear
                                    AND r002~accountingdocumentitem = r005~accountingdocumentitem
                                    AND r002~deliverydocument       = r005~deliverydocument
                                    AND r002~deliverydocumentitem   = r005~deliverydocumentitem
      ORDER BY r005~companycode, r005~accountingdocument, r005~fiscalyear, r005~accountingdocumentitem, r005~deliverydocument, r005~deliverydocumentitem
      INTO TABLE @DATA(lt_r005).


    "Dosya kapaması yapılmış masrafları sil
    LOOP AT lt_r002 ASSIGNING FIELD-SYMBOL(<ls_r002>).
      READ TABLE lt_r005 INTO DATA(ls_r005) WITH KEY companycode            = <ls_r002>-companycode
                                                     accountingdocument     = <ls_r002>-accountingdocument
                                                     fiscalyear             = <ls_r002>-fiscalyear
                                                     accountingdocumentitem = <ls_r002>-accountingdocumentitem
                                                     deliverydocument       = <ls_r002>-deliverydocument
                                                     deliverydocumentitem   = <ls_r002>-deliverydocumentitem BINARY SEARCH.
      IF sy-subrc = 0.
        IF ms_request-reversal IS INITIAL AND ls_r005-amount >= <ls_r002>-documentcurrenyamount.
          <ls_r002>-accountingdocument = 'XXDELETEXX'.
        ENDIF.
      ELSE.
        IF ms_request-reversal IS NOT INITIAL.
          <ls_r002>-accountingdocument = 'XXDELETEXX'.
        ENDIF.
      ENDIF.

      <ls_r002>-accountingdocument_inv = ls_r005-accountingdocument_inv.
      <ls_r002>-fiscalyear_inv         = ls_r005-fiscalyear_inv.

      CLEAR ls_r005.
    ENDLOOP.

    DELETE lt_r002 WHERE accountingdocument = 'XXDELETEXX'.

    IF lt_r002 IS NOT INITIAL.
      "Çekme belgelerinin bilgilerini oku (İlk başlık ekranında gösterilecek)
      SELECT DISTINCT delivery~deliverydocument,
                      delivery~supplier,
                      delivery~creationdate,
                      i_supplier~suppliername,
                      dlv_cus~documenttype,
                      ddtyp_tx~text AS documenttype_tx
        FROM @lt_r002 AS r002
        INNER JOIN ynft_t_dlv_cus AS dlv_cus ON dlv_cus~deliverydocument = r002~deliverydocument
        INNER JOIN i_deliverydocument AS delivery ON delivery~deliverydocument = dlv_cus~deliverydocument
        LEFT OUTER JOIN i_supplier ON i_supplier~supplier = delivery~supplier
        LEFT OUTER JOIN ddcds_customer_domain_value_t( p_domain_name = 'YNFT_D_DDTYP' ) AS ddtyp_tx ON ddtyp_tx~value_low = dlv_cus~documenttype
                                                                                                   AND ddtyp_tx~language  = @sy-langu
        INTO CORRESPONDING FIELDS OF TABLE @ms_response-header.

      "Her bir çekmenin altında gösterilecek kalem tablosunu hazırla
      SELECT item~deliverydocument,
             item~deliverydocumentitem,
             item~companycode,
             item~accountingdocument,
             item~fiscalyear,
             item~accountingdocumentitem,
             bkpf~documentdate,
             bkpf~postingdate,
             item~costtype,
             costtype~description        AS costtype_tx ,
             cus_it~purchaseorder        AS referencesddocument,
             cus_it~purchaseorderitem    AS referencesddocumentitem,
             item~documentcurrenyamount,
             item~documentcurrency,
             po~material                 AS product,
             producttext~productname,
             bseg~supplier,
             supplier~suppliername,
             item~accountnumber,
             CASE cus~documenttype WHEN '1' THEN cus_it~shipquantity ELSE cus_it~clearencequantity END AS deliveryquantity,
             cus_it~quantityunit         AS deliveryquantityunit,
             po~plant,
             item~accountingdocument_inv,
             item~fiscalyear_inv
        FROM @lt_r002                       AS item
        INNER JOIN i_journalentry           AS bkpf        ON bkpf~companycode            = item~companycode
                                                          AND bkpf~accountingdocument     = item~accountingdocument
                                                          AND bkpf~fiscalyear             = item~fiscalyear
        INNER JOIN i_journalentryitem       AS bseg        ON bseg~companycode            = item~companycode
                                                          AND bseg~accountingdocument     = item~accountingdocument
                                                          AND bseg~fiscalyear             = item~fiscalyear
                                                          AND bseg~financialaccounttype   = 'K'
                                                          AND bseg~ledger                 = '0L'
        INNER JOIN ynft_t_dlv_cus           AS cus         ON cus~deliverydocument        = item~deliverydocument
        INNER JOIN ynft_t_dlvit_cus         AS cus_it      ON cus_it~deliverydocument     = item~deliverydocument
                                                          AND cus_it~deliverydocumentitem = item~deliverydocumentitem
        INNER JOIN i_purchaseorderitemapi01 AS po          ON po~purchaseorder            = cus_it~purchaseorder
                                                          AND po~purchaseorderitem        = cus_it~purchaseorderitem
        LEFT OUTER JOIN yvh_nft_ctype       AS costtype    ON costtype~ctype              = item~costtype
        LEFT OUTER JOIN i_producttext       AS producttext ON producttext~product         = po~material
                                                          AND producttext~language        = @sy-langu
        LEFT OUTER JOIN i_supplier         AS supplier ON supplier~supplier = bseg~supplier
        INTO CORRESPONDING FIELDS OF TABLE @ms_response-items.

      "Sort & Conversion işlemleri
      SORT: ms_response-header BY deliverydocument,
            ms_response-items  BY accountingdocument accountingdocumentitem.

      LOOP AT ms_response-header ASSIGNING FIELD-SYMBOL(<fs_header>).
        <fs_header>-deliverydocument = |{ <fs_header>-deliverydocument ALPHA = OUT }|.
        <fs_header>-supplier         = |{ <fs_header>-supplier ALPHA = OUT }|.
      ENDLOOP.
      LOOP AT ms_response-items ASSIGNING FIELD-SYMBOL(<fs_item>).
        <fs_item>-deliverydocument = |{ <fs_item>-deliverydocument ALPHA = OUT }|.
        <fs_item>-supplier         = |{ <fs_item>-supplier ALPHA = OUT }|.
        <fs_item>-product          = |{ <fs_item>-product ALPHA = OUT }|.
      ENDLOOP.
    ENDIF.

    DATA(lv_response_body) = /ui2/cl_json=>serialize( EXPORTING data = ms_response ).
    response->set_text( lv_response_body ).
    response->set_header_field( i_name = mc_header_content i_value = mc_content_type ).
  ENDMETHOD.