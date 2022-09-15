CLASS y_ndbs_atc_demo_failures DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.

    METHODS where_cond_for_select .
    METHODS check_of_sy_subrc_handling .
    METHODS use_of_select_asterisk
      IMPORTING
        !material TYPE matnr .
    METHODS nested_loops
      IMPORTING
        !sales_order_number TYPE vbeln_va .
    METHODS problematic_select_endselect
      IMPORTING
        !carrier_id    TYPE s_carr_id
        !connection_id TYPE s_conn_id .
    METHODS internal_table_low_performance .
    METHODS unsecure_for_all_entries
      IMPORTING
        !vbeln TYPE vbeln_va .
    METHODS table_order_dependencies
      IMPORTING
        !sales_order_number TYPE vbeln_va .
    METHODS updates_to_standard_tables .
    METHODS missing_space .
    METHODS copy_current_table_row
      IMPORTING
        !sales_order_number TYPE vbeln_va .
    METHODS use_of_boolean_input_parameter
      IMPORTING
        !flag TYPE abap_bool .
    METHODS use_of_call_method .
    METHODS use_of_chain_declaration .
    METHODS check_in_loop
      IMPORTING
        !vkorg TYPE vkorg .
    METHODS comment_position
      IMPORTING
        !sales_order_number TYPE vbeln_va .
    METHODS collect_restriction .
    METHODS usage_of_cx_root .
    METHODS avoid_default_key .
    METHODS empty_if_branch
      IMPORTING
        !sales_order_number TYPE vbeln_va .
    METHODS usage_of_magic_number .
    METHODS nesting_depth_check
      IMPORTING
        !type1        TYPE char1
        !type2        TYPE char2
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS prefer_case_to_else_if
      IMPORTING
        !auart        TYPE auart
      RETURNING
        VALUE(result) TYPE char1 .
    METHODS prefer_is_not_to_not_is
      IMPORTING
        !sales_order_number TYPE vbeln_va .
    METHODS prefer_line_exists_line_index
      IMPORTING
        !sales_orders TYPE tab_vbak .
    METHODS prefer_new_to_creat_object .
    METHODS prefer_returning_to_exporting
      IMPORTING
        !sales_order_number TYPE vbeln_va
      EXPORTING
        !sales_order_detail TYPE vbak .
  PRIVATE SECTION.
ENDCLASS.



CLASS Y_NDBS_ATC_DEMO_FAILURES IMPLEMENTATION.


  METHOD avoid_default_key.

    DATA itab1 TYPE STANDARD TABLE OF vbak WITH DEFAULT KEY.

    "Default keys are often only added to get the newer functional statements working.
    "The keys themselves in fact are usually superfluous and waste resources for nothing.
    "Either specify the key components explicitly
    "or resort to EMPTY KEY if you don't need a key at all.
  ENDMETHOD.


  METHOD check_in_loop.

    SELECT vbeln,
           auart
      FROM vbak
      INTO TABLE @DATA(sales_orders)
      WHERE vkorg = @vkorg.

    LOOP AT sales_orders ASSIGNING FIELD-SYMBOL(<sales_order>).

      CHECK <sales_order>-auart = 'OR'.

    ENDLOOP.

*This check verifies if a CHECK statement is being used inside of a LOOP structure. A CHECK within a LOOP,
*ends the current iteration and proceeds to the next one.
*This behavior might lead to some confusion like: Does it end the method processing or does it exit the loop?
*
*
*Prefer using CONTINUE (within an IF-Statement) instead of using the CHECK Statement in this case.
*Since the keyword CONTINUE can only be used in LOOPS, the intention is then automatic clear to everyone reading the code.
*Keep also in mind, that other Keywords like: EXIT or RETURN are also more explicit than CHECK.

  ENDMETHOD.


  METHOD check_of_sy_subrc_handling.

    CONSTANTS: example_material TYPE matnr VALUE 'MATERIAL_01'.

    DATA: demo_record TYPE yndbs_atc_demo.

    " Check of SY-SUBRC handling after database read access
    SELECT SINGLE matnr
      FROM mara
      INTO @DATA(material)
      WHERE matnr = @example_material.

    " Check of SY-SUBRC handling after database change
    UPDATE yndbs_atc_demo FROM demo_record.

  ENDMETHOD.


  METHOD collect_restriction.
    TYPES: BEGIN OF seat,
             carrid   TYPE sflight-carrid,
             connid   TYPE sflight-connid,
             seatsocc TYPE sflight-seatsocc,
           END OF seat,
           seats TYPE TABLE OF seat.
    DATA seat TYPE seat.
    DATA seats TYPE seats.

    SELECT carrid, connid, seatsocc
           FROM sflight
           INTO @seat.
      COLLECT seat INTO seats.
    ENDSELECT.
    "Only use the statement COLLECT for hashed tables or sorted tables with a unique key. Do not use it any more for standard tables.
    "Change the internal table to SORTED or HASHED as recommended, or perform the collection manually.
  ENDMETHOD.


  METHOD comment_position.


    " Load sales orders
    SELECT vbeln,
           auart,
           vkorg
      FROM vbak
      INTO TABLE @DATA(sales_orders)
      WHERE vbeln = @sales_order_number.

    "This check searches for "Quote comments" which are not indented along with the statements they belong to.
    "You should indent the comments along with the statements they are commenting.

  ENDMETHOD.


  METHOD copy_current_table_row.

    SELECT vbeln,
           posnr,
           matnr
      FROM vbap
      INTO TABLE @DATA(sales_order_items)
      WHERE vbeln = @sales_order_number.

    IF sy-subrc = 0.
      LOOP AT sales_order_items INTO DATA(sales_order_item).
        EXIT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD empty_if_branch.


    IF sales_order_number IS INITIAL.

    ELSE.
      SELECT vbeln,
     auart,
     vkorg
      FROM vbak
      INTO TABLE @DATA(sales_orders)
      WHERE vbeln = @sales_order_number.

    ENDIF.

    "This check searches for empty IF statements or branches.
    "Fill the empty IF structure with code or remove it by refactoring the condition.
  ENDMETHOD.


  METHOD internal_table_low_performance.

    TYPES: BEGIN OF sales_order_item,
             vbeln  TYPE vbeln_va,
             posnr  TYPE posnr_va,
             matnr  TYPE matnr,
             kwmeng TYPE kwmeng,
             vrkme  TYPE vrkme,
           END OF sales_order_item.

    DATA: sales_order_items_standard TYPE STANDARD TABLE OF sales_order_item.
    DATA: sales_order_items_sorted   TYPE SORTED TABLE OF sales_order_item WITH UNIQUE KEY vbeln
                                                                                           posnr.
    DATA: sales_order_range          TYPE RANGE OF vbeln_va.

    " Example 1 - Sequential read on standard table
    " Load sales order items
    SELECT vbeln
           posnr
           matnr
           kwmeng
           vrkme
      FROM vbap
      INTO TABLE sales_order_items_standard
      WHERE vbeln IN sales_order_range.

    IF sy-subrc <> 0.
      " No items found
      RETURN.
    ENDIF.

    LOOP AT sales_order_items_standard ASSIGNING FIELD-SYMBOL(<sales_order_item_standard>)
      WHERE vbeln = '010'.
      " Do something...
    ENDLOOP.

    " Example 2 - Sequential read on sorted table
    " Load sales order items
    SELECT vbeln
           posnr
           matnr
           kwmeng
           vrkme
      FROM vbap
      INTO TABLE sales_order_items_sorted
      WHERE vbeln IN sales_order_range.

    IF sy-subrc <> 0.
      " No items found
      RETURN.
    ENDIF.

    READ TABLE sales_order_items_sorted ASSIGNING FIELD-SYMBOL(<sales_order_item_sorted>)
    WITH KEY matnr = 'DUMMY_MATERIAL'.

    IF sy-subrc <> 0.
      " Sales order item not found
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD missing_space.

    DATA(dummy_text) ='TEST'.

  ENDMETHOD.


  METHOD nested_loops.

    TYPES: BEGIN OF sales_order_item,
             vbeln  TYPE vbeln_va,
             posnr  TYPE posnr_va,
             matnr  TYPE matnr,
             kwmeng TYPE kwmeng,
             vrkme  TYPE vrkme,
           END OF sales_order_item.

    DATA: sales_order_items TYPE SORTED TABLE OF sales_order_item WITH UNIQUE KEY vbeln
                                                                                  posnr.

    " Load sales orders
    SELECT vbeln,
           auart,
           vkorg
      FROM vbak
      INTO TABLE @DATA(sales_orders)
      WHERE vbeln = @sales_order_number.

    IF sy-subrc = 0.
      CLEAR sales_orders.
    ENDIF.

    IF sales_orders IS NOT INITIAL.
      " Load sales order items
      SELECT vbeln,
             posnr,
             matnr,
             kwmeng,
             vrkme
        FROM vbap
        INTO TABLE @sales_order_items
        FOR ALL ENTRIES IN @sales_orders
        WHERE vbeln = @sales_orders-vbeln.

      IF sy-subrc <> 0.
        CLEAR sales_order_items.
      ENDIF.
    ENDIF.

    LOOP AT sales_orders ASSIGNING FIELD-SYMBOL(<sales_order>).

      LOOP AT sales_order_items ASSIGNING FIELD-SYMBOL(<sales_order_item>)
        WHERE vbeln = <sales_order>-vbeln.
        " Do something...
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD nesting_depth_check.

    IF type1 = 'A'.
      IF type2 = 'B'.
        IF sy-mandt = 200.
          IF sy-uname = 'me'.
            IF sy-langu = 'EN'.
              result = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    "This check counts the nesting-depth level of a method, function-module, form-routine or module
    "Modularize the functionality into smaller blocks. This increases the readability and efficiency.
  ENDMETHOD.


  METHOD prefer_case_to_else_if.

    IF auart = 'OR'.
      result = '1'.
    ELSEIF auart = 'KB'.
      result = '2'.
    ELSEIF auart = 'QT'.
      result = '3'.
    ELSEIF auart = 'CR'.
      result = '4'.
    ELSEIF auart = 'DR'.
      result = '5'.
    ELSEIF auart = ''.
      result = '6'.
    ELSE.
      result = '7'.
    ENDIF.

    "Prefer CASE to ELSEIF for multiple alternative conditions because CASE makes it easy to see a set of alternatives that exclude each other.
    "Use CASE instead of ELSEIF for multiple alternative conditions.
  ENDMETHOD.


  METHOD prefer_is_not_to_not_is.

    IF NOT sales_order_number IS  INITIAL.

      SELECT vbeln,
       auart,
       vkorg
       FROM vbak
       INTO TABLE @DATA(sales_orders)
       WHERE vbeln = @sales_order_number.

    ENDIF.
    "Prefer IS ... NOT to NOT ... IS because it requires a "mental turnaround" that makes it harder to understand the negation logic.
    "Preferably, use a positive condition; but if the negative condition is easier to understand, change the NOT ... IS to IS ... NOT.
  ENDMETHOD.


  METHOD prefer_line_exists_line_index.

    DATA line_index LIKE sy-tabix.
    DATA line_exists TYPE abap_bool.

    READ TABLE sales_orders TRANSPORTING NO FIELDS WITH KEY auart = 'OR'.

    IF sy-subrc = 0.
      line_index = sy-tabix.
      line_exists = abap_true.
    ENDIF.

    LOOP AT sales_orders REFERENCE INTO DATA(line) WHERE auart = 'OR'.
      line_index = sy-tabix.
      line_exists = abap_true.
      EXIT.
    ENDLOOP.
    "Prefer LINE_EXISTS or LINE_INDEX over READ TABLE or LOOP AT as they avoid needlessly longer statements.
    "Preferably, use LINE_EXISTS to check whether the row of an internal table exists, and LINE_INDEX to check the row index.
  ENDMETHOD.


  METHOD prefer_new_to_creat_object.

    DATA prefer_new_to_create_object TYPE REF TO y_check_prefer_new_to_crt_obj.
    CREATE OBJECT prefer_new_to_create_object.
    "Prefer NEW over CREATE OBJECT as it avoids needlessly longer statements.
    "Preferably, use NEW for creating new objects/instances.
  ENDMETHOD.


  METHOD prefer_returning_to_exporting.

    SELECT SINGLE vbeln,
                  auart,
                  vkorg
                  FROM vbak
                  INTO CORRESPONDING FIELDS OF @sales_order_detail
                  WHERE vbeln = @sales_order_number.
    "Based on the Clean ABAP, this check searches in classes and interfaces for methods that have only one exporting parameter.
    "If it finds one, it will recommend you to change it from EXPORTING to RETURNING.
    "Change the EXPORTING parameter to RETURNING.
  ENDMETHOD.


  METHOD problematic_select_endselect.

    TYPES: BEGIN OF flight,
             carrid TYPE s_carr_id,
             connid TYPE s_conn_id,
             fldate TYPE s_date,
           END OF flight.

    DATA: flight       TYPE flight,
          record_found TYPE flag.

    " EXIT in SELECT...ENDSELECT
    SELECT carrid,
           connid,
           fldate
      FROM sflight
      WHERE carrid = @carrier_id
      AND   connid = @connection_id
      INTO @flight.
      EXIT.
    ENDSELECT.

    IF sy-subrc <> 0.
      record_found = abap_false.
    ENDIF.

    " Empty SELECT...ENDSELECT statement
    SELECT carrid,
           connid,
           fldate
      FROM sflight
      INTO @flight.                                "#EC "#EC CI_NOWHERE
    ENDSELECT.

    IF sy-subrc <> 0.
      record_found = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD table_order_dependencies.

    TYPES: BEGIN OF sales_order_item,
             vbeln TYPE vbeln_va,
             posnr TYPE posnr_va,
             matnr TYPE matnr,
           END OF sales_order_item.

    DATA: sales_order_items TYPE STANDARD TABLE OF sales_order_item.

    " Example 1 - Binary search on non-sorted table
    SELECT vbeln
           posnr
           matnr
      FROM vbap
      INTO TABLE sales_order_items
      WHERE vbeln = sales_order_number.

    IF sy-subrc <> 0.
      CLEAR sales_order_items.
    ENDIF.

    READ TABLE sales_order_items TRANSPORTING NO FIELDS
    BINARY SEARCH WITH KEY vbeln = sales_order_number. "#EC PREF_LINE_EX

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " Example 2 - DELETE ADJACENT DUPLICATES FROM on non-sorted table
    SELECT vbeln
           posnr
           matnr
      FROM vbap
      INTO TABLE sales_order_items
      WHERE vbeln = sales_order_number.

    IF sy-subrc = 0.
      DELETE ADJACENT DUPLICATES FROM sales_order_items.
    ENDIF.

  ENDMETHOD.


  METHOD unsecure_for_all_entries.

    " Load sales orders
    SELECT vbeln,
           auart
      FROM vbak
      INTO TABLE @DATA(sales_orders)
      WHERE vbeln = @vbeln.                          "#EC "#EC CI_SUBRC

    " Load sales order items
    SELECT vbeln,
           posnr,
           matnr
      FROM vbap
      INTO TABLE @DATA(sales_order_items)
      FOR ALL ENTRIES IN @sales_orders
      WHERE vbeln = @sales_orders-vbeln.

    IF sy-subrc <> 0.
      CLEAR sales_order_items.
    ENDIF.

  ENDMETHOD.


  METHOD updates_to_standard_tables.

    DATA: sales_order TYPE vbak.

    IF 1 = 0.
      DELETE vbak FROM sales_order.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.

      UPDATE vbak FROM sales_order.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.

      MODIFY vbak FROM sales_order.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.

      UPDATE vbak FROM sales_order.
      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD usage_of_cx_root.
    TRY.
        cl_demo_output=>write( 'CX_ROOT Usage Check' ).
      CATCH cx_root.
        cl_demo_output=>display( 'Catching exception' ).
    ENDTRY.
    "This check searches for direct "CX_ROOT" exceptions being used in the code (e.g.: In a TRY-CATCH block).
    "The solution is to use well defined and specific class-based exceptions.
  ENDMETHOD.


  METHOD usage_of_magic_number.
    DATA : myvar TYPE i VALUE 0.
    DO 5 TIMES.
      myvar = myvar + 1.
    ENDDO.
    "This check searches for arbitrary values in the source code having no meaningful connotation.
    "Create constants.

  ENDMETHOD.


  METHOD use_of_boolean_input_parameter.
    CHECK flag = abap_true.
*    Boolean input parameters are often an indicator that a method does two things instead of one.
*    Splitting the method may simplify the methods' code and describe the different intentions better
  ENDMETHOD.


  METHOD use_of_call_method.

    CALL METHOD use_of_boolean_input_parameter EXPORTING flag = abap_true.

*It checks the usage of CALL METHOD statement in the code.
*Change the long method calls using CALL METHOD statement to short method calls using parenthesis notation (dynamic call).
  ENDMETHOD.


  METHOD use_of_chain_declaration.
    DATA:
      string TYPE string,
      client LIKE sy-mandt.

    TYPES:
      name  TYPE string,
      myvar TYPE int8.

    CONSTANTS:
      min_age       TYPE i VALUE 18,
      min_name_size TYPE i VALUE 3.

*This check verifies the usage of chain up-front declarations.
*Change the chain up-front declarations to inline declarations.
  ENDMETHOD.


  METHOD use_of_select_asterisk.

    SELECT *
      FROM mara
      INTO TABLE @DATA(materials)
      WHERE matnr = @material.

    IF sy-subrc = 0.
      SORT materials BY matnr.
    ENDIF.

  ENDMETHOD.


  METHOD where_cond_for_select.

    CONSTANTS: example_delivery_item TYPE posnr VALUE '000001'.

    " No WHERE condition
    SELECT matnr
      FROM mara
      INTO TABLE @DATA(material_numbers).

    IF sy-subrc <> 0.
      CLEAR material_numbers.
    ENDIF.

    " No index field in WHERE
    SELECT vbeln,
           posnr,
           matnr
      FROM vbap
      INTO TABLE @DATA(sales_order_items)
      WHERE erdat = @sy-datum.

    IF sy-subrc <> 0.
      CLEAR sales_order_items.
    ENDIF.

    " No first index field in WHERE
    SELECT vbeln,
           posnr,
           matnr
      FROM lips
      INTO TABLE @DATA(delivery_items)
      WHERE posnr = @example_delivery_item.

    IF sy-subrc <> 0.
      CLEAR delivery_items.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
