  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA lv_tabix TYPE sy-tabix.
    DATA lv_count TYPE int4.
    DATA lt_output TYPE STANDARD TABLE OF yi_nft_ddl_imp_cost_report.
    lt_output = CORRESPONDING #( it_original_data ).
    CHECK lt_output IS NOT INITIAL.

    SELECT r005~companycode,
           r005~accountingdocument,
           r005~fiscalyear,
           r005~accountingdocumentitem,
           r005~referenceitem,
           r005~accountingdocument_inv,
           r005~fiscalyear_inv
       FROM @lt_output AS lt_output INNER JOIN ynft_t_r005 AS r005 ON r005~companycode = lt_output~companycode
                                                                  AND r005~accountingdocument = lt_output~accountingdocument
                                                                  AND r005~fiscalyear = lt_output~fiscalyear
                                                                  AND r005~accountingdocumentitem = lt_output~accountingdocumentitem
       ORDER BY r005~companycode,
           r005~accountingdocument,
           r005~fiscalyear,
           r005~accountingdocumentitem
       INTO TABLE @DATA(lt_r005).

    SELECT  exchangerate~validitystartdate , exchangerate~exchangerate
    FROM @lt_output AS lt_output INNER JOIN i_exchangeraterawdata AS exchangerate ON exchangerate~validitystartdate = lt_output~netduedate
    WHERE exchangerate~sourcecurrency = 'USD'
      AND exchangerate~targetcurrency = 'TRY'
      AND exchangerate~exchangeratetype = 'M'
      AND lt_output~documentcurrency = 'TRY'
    ORDER BY exchangerate~validitystartdate
    INTO TABLE @DATA(lt_usd).

    SELECT  exchangerate~validitystartdate , exchangerate~exchangerate
    FROM @lt_output AS lt_output INNER JOIN i_exchangeraterawdata AS exchangerate ON exchangerate~validitystartdate = lt_output~netduedate
    WHERE exchangerate~sourcecurrency = 'EUR'
      AND exchangerate~targetcurrency = 'TRY'
      AND exchangerate~exchangeratetype = 'M'
      AND lt_output~documentcurrency = 'TRY'
    ORDER BY exchangerate~validitystartdate
    INTO TABLE @DATA(lt_eur).

    LOOP AT lt_output ASSIGNING FIELD-SYMBOL(<ls_output>).
      IF <ls_output>-documentcurrency = 'TRY'.
        READ TABLE lt_usd INTO DATA(ls_usd) WITH KEY validitystartdate = <ls_output>-netduedate BINARY SEARCH.
        IF sy-subrc = 0 AND ls_usd-exchangerate <> 0.
          <ls_output>-documentamountinusd = <ls_output>-documentcurrenyamount / ls_usd-exchangerate.
        ENDIF.
        READ TABLE lt_eur INTO DATA(ls_eur) WITH KEY validitystartdate = <ls_output>-netduedate BINARY SEARCH.
        IF sy-subrc = 0 AND ls_eur-exchangerate <> 0.
          <ls_output>-documentamountineur = <ls_output>-documentcurrenyamount / ls_eur-exchangerate.
        ENDIF.
      ENDIF.
      READ TABLE lt_r005 INTO DATA(ls_r005) WITH KEY companycode = <ls_output>-companycode
                                                     accountingdocument = <ls_output>-accountingdocument
                                                     fiscalyear = <ls_output>-fiscalyear
                                                     accountingdocumentitem = <ls_output>-accountingdocumentitem BINARY SEARCH.
      IF sy-subrc = 0.
        lv_tabix = sy-tabix.
        CLEAR lv_count.
        LOOP AT lt_r005 INTO DATA(ls_r005_line) FROM lv_tabix.
          IF ls_r005_line-companycode <> <ls_output>-companycode OR
             ls_r005_line-accountingdocument <> <ls_output>-accountingdocument OR
             ls_r005_line-fiscalyear <> <ls_output>-fiscalyear OR
             ls_r005_line-accountingdocumentitem <> <ls_output>-accountingdocumentitem.
            EXIT.
          ELSE.
            ADD 1 TO lv_count.
            IF lv_count > 1.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF lv_count = 1.
          <ls_output>-accountingdocument_inv = ls_r005-accountingdocument_inv.
          <ls_output>-fiscalyear_inv = ls_r005-fiscalyear_inv.
        ELSEIF lv_count > 1.
          <ls_output>-accountingdocument_inv = abap_true.
*          <ls_output>-fiscalyear_inv = ls_r005-fiscalyear_inv.
        ENDIF.
      ENDIF.
    ENDLOOP.
    ct_calculated_data = CORRESPONDING #( lt_output ).
  ENDMETHOD.