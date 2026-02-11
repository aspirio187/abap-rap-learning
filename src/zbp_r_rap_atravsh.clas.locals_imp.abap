CLASS lhc_zr_rap_atravsh DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF travel_status,
        open     TYPE c LENGTH 1 VALUE 'O',
        accepted TYPE c LENGTH 1 VALUE 'A',
        rejected TYPE c LENGTH 1 VALUE 'R',
      END OF travel_status.

    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR ZrRapAtravsh
        RESULT result,
      earlynumbering_create FOR NUMBERING
        IMPORTING entities FOR CREATE ZrRapAtravsh,
      setStatusToOpen FOR DETERMINE ON MODIFY
        IMPORTING keys FOR ZrRapAtravsh~setStatusToOpen,
      validateCustomer FOR VALIDATE ON SAVE
        IMPORTING keys FOR ZrRapAtravsh~validateCustomer.

    METHODS ValidateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR ZrRapAtravsh~ValidateDates.
ENDCLASS.

CLASS lhc_zr_rap_atravsh IMPLEMENTATION.

  "=====================================================================
  "! METHOD: get_global_authorizations
  "! PURPOSE: Handles global authorization checks for the Travel entity
  "! CALLED BY: RAP framework during data access operations
  "=====================================================================
  METHOD get_global_authorizations.
  ENDMETHOD.

  "=====================================================================
  "! METHOD: earlynumbering_create
  "! PURPOSE: Implements early numbering for Travel ID auto-generation
  "! CALLED BY: RAP framework before creating a new travel instance
  "! LOGIC: Generates unique Travel IDs either via number range or DB query
  "=====================================================================
  METHOD earlynumbering_create.

    DATA:
      entity           TYPE STRUCTURE FOR CREATE zr_rap_atravsh,
      travel_id_max    TYPE /dmo/travel_id,
      " change to abap_false if you get the ABAP Runtime error 'BEHAVIOR_ILLEGAL_STATEMENT'
      use_number_range TYPE abap_bool VALUE abap_false.

    "Ensure Travel ID is not set yet (idempotent)- must be checked when BO is draft-enabled
    LOOP AT entities INTO entity WHERE TravelID IS NOT INITIAL.
      APPEND CORRESPONDING #( entity ) TO mapped-zrrapatravsh.
    ENDLOOP.

    DATA(entities_wo_travelid) = entities.
    "Remove the entries with an existing Travel ID

    DELETE entities_wo_travelid WHERE TravelID IS NOT INITIAL.

    IF use_number_range = abap_true.
      "GET numbers"
      TRY.
          cl_numberrange_runtime=>number_get(
              EXPORTING
                  nr_range_nr = '01'
                  object = '/DMO/TRV_M'
                  quantity = CONV #( lines( entities_wo_travelid ) )
              IMPORTING
                  number = DATA(number_range_key)
                  returncode = DATA(number_range_return_code)
                  returned_quantity = DATA(number_range_returned_quantity)
          ).
        CATCH cx_number_ranges INTO DATA(lx_number_ranges).
          LOOP AT entities_wo_travelid INTO entity.
            APPEND VALUE #(
                %cid = entity-%cid
                %key = entity-%key
                %is_draft = entity-%is_draft
                %msg = lx_number_ranges
            ) TO reported-zrrapatravsh.
            APPEND VALUE #(
              %cid = entity-%cid
              %key = entity-%key
              %is_draft = entity-%is_draft
            ) TO  failed-zrrapatravsh.
          ENDLOOP.
          EXIT.
      ENDTRY.

      "determine the first free travel ID from the number range
      travel_id_max = number_range_key - number_range_returned_quantity.
    ELSE.
      "determine the first free travel ID without number range
      "Get max travel ID from active table
      SELECT SINGLE FROM zr_rap_atravsh FIELDS MAX( TravelID ) AS travelID INTO @travel_id_max.
      "Get max travel ID from draft table
      SELECT SINGLE FROM zrap_atravsh_d FIELDS MAX( travelid ) INTO @DATA(max_travelid_draft).
      IF max_travelid_draft > travel_id_max.
        travel_id_max = max_travelid_draft.
      ENDIF.
    ENDIF.

    LOOP AT entities_wo_travelid INTO entity.
      travel_id_max += 1.
      entity-TravelID = travel_id_max.

      APPEND VALUE #(
          %cid = entity-%cid
          %key = entity-%key
          %is_draft = entity-%is_draft
      ) TO mapped-zrrapatravsh.
    ENDLOOP.
  ENDMETHOD.

  "=====================================================================
  "! METHOD: setStatusToOpen
  "! PURPOSE: Sets the overall travel status to 'Open' on creation
  "! CALLED BY: RAP framework as a DETERMINE ON MODIFY handler
  "! LOGIC: Sets status only if not already initialized
  "=====================================================================
  METHOD setStatusToOpen.
    "Read travel instances of the transferred keys
    READ ENTITIES OF zr_rap_atravsh IN LOCAL MODE
        ENTITY ZrRapAtravsh
            FIELDS ( OverallStatus )
            WITH CORRESPONDING #(  keys )
        RESULT DATA(travels)
        FAILED DATA(read_failed).

    "If overall travel status is already set, do nothing, i.e. remove such instances
    DELETE travels WHERE OverallStatus IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    "else set overall travel status to open ('O')
    MODIFY ENTITIES OF zr_rap_atravsh IN LOCAL MODE
        ENTITY ZrRapAtravsh
            UPDATE SET FIELDS
            WITH VALUE #( FOR travel IN travels ( %tky = travel-%tky
                                                  OverallStatus = travel_status-open ) )
    REPORTED DATA(update_reported).

    "Set the changing parameter
    reported = CORRESPONDING #( DEEP update_reported ).
  ENDMETHOD.

  "=====================================================================
  "! METHOD: validateCustomer
  "! PURPOSE: Validates the customer reference in the travel entity
  "! CALLED BY: RAP framework during save validation
  "! LOGIC: [Implementation pending]
  "=====================================================================
  METHOD validateCustomer.
    "Read relevant travel instance data
    READ ENTITIES OF zr_rap_atravsh IN LOCAL MODE
        ENTITY ZrRapAtravsh
            FIELDS ( CustomerID )
            WITH CORRESPONDING #( keys )
        RESULT DATA(travels).

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    "Optimization of DB select: extract distinct non-initial custom IDs
    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING  customer_id = CustomerID EXCEPT *  ).

    DELETE customers WHERE customer_id IS INITIAL.

    IF customers IS NOT INITIAL.
      "check if customer ID exists
      SELECT FROM /dmo/customer FIELDS customer_id
                                   FOR ALL ENTRIES IN @customers
                                   WHERE customer_id = @customers-customer_id
       INTO TABLE @DATA(valid_customers).
    ENDIF.

    "raise msg for non existing and initial customer id
    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #( %tky = travel-%tky
                        %state_area = 'VALIDATE_CUSTOMER' )
                    TO reported-zrrapatravsh.

      IF travel-CustomerID IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-zrrapatravsh.

        APPEND VALUE #( %tky = travel-%tky
                        %state_area = 'VALIDATE_CUSTOMER'
                        %msg = NEW /dmo/cm_flight_messages(
                            textid = /dmo/cm_flight_messages=>enter_customer_id
                            severity = if_abap_behv_message=>severity-error )
                        %element-CustomerID = if_abap_behv=>mk-on
                        ) TO reported-zrrapatravsh.

      ELSEIF travel-CustomerID IS NOT INITIAL AND NOT line_exists( valid_customers[ customer_id = travel-CustomerID ] ).
        APPEND VALUE #( %tky = travel-%tky ) TO failed-zrrapatravsh.

        APPEND VALUE #( %tky = travel-%tky
                        %state_area = 'VALIDATE_CUSTOMER'
                        %msg = NEW /dmo/cm_flight_messages(
                            customer_id = travel-customerid
                            textid = /dmo/cm_flight_messages=>customer_unkown
                            severity = if_abap_behv_message=>severity-error )
                        %element-CustomerID = if_abap_behv=>mk-on
                        ) TO reported-zrrapatravsh.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  "=====================================================================
  "! METHOD: ValidateDates
  "! PURPOSE: Validates travel start and end dates
  "! CALLED BY: RAP framework during save validation
  "! LOGIC: [Implementation pending]
  "=====================================================================
  METHOD ValidateDates.
    READ ENTITIES OF zr_rap_atravsh IN LOCAL MODE
      ENTITY ZrRapAtravsh
        FIELDS (  BeginDate EndDate TravelID )
        WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #(  %tky               = travel-%tky
                       %state_area        = 'VALIDATE_DATES' ) TO reported-zrrapatravsh.

      IF travel-BeginDate IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-zrrapatravsh.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg              = NEW /dmo/cm_flight_messages(
                                                                textid   = /dmo/cm_flight_messages=>enter_begin_date
                                                                severity = if_abap_behv_message=>severity-error )
                      %element-BeginDate = if_abap_behv=>mk-on ) TO reported-zrrapatravsh.
      ENDIF.
      IF travel-BeginDate < cl_abap_context_info=>get_system_date( ) AND travel-BeginDate IS NOT INITIAL.
        APPEND VALUE #( %tky               = travel-%tky ) TO failed-zrrapatravsh.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg              = NEW /dmo/cm_flight_messages(
                                                                begin_date = travel-BeginDate
                                                                textid     = /dmo/cm_flight_messages=>begin_date_on_or_bef_sysdate
                                                                severity   = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on ) TO reported-zrrapatravsh.
      ENDIF.
      IF travel-EndDate IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-zrrapatravsh.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                textid   = /dmo/cm_flight_messages=>enter_end_date
                                                               severity = if_abap_behv_message=>severity-error )
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-zrrapatravsh.
      ENDIF.
      IF travel-EndDate < travel-BeginDate AND travel-BeginDate IS NOT INITIAL
                                           AND travel-EndDate IS NOT INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-zrrapatravsh.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW /dmo/cm_flight_messages(
                                                                textid     = /dmo/cm_flight_messages=>begin_date_bef_end_date
                                                                begin_date = travel-BeginDate
                                                                end_date   = travel-EndDate
                                                                severity   = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-zrrapatravsh.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
