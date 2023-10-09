CLASS zcl_sfli_data_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    CLASS-METHODS:
      reset_numberrange_interval
        IMPORTING
          numberrange_object   TYPE cl_numberrange_runtime=>nr_object
          numberrange_interval TYPE cl_numberrange_runtime=>nr_interval
          subobject            TYPE cl_numberrange_intervals=>nr_subobject OPTIONAL
          fromnumber           TYPE cl_numberrange_intervals=>nr_nriv_line-fromnumber
          tonumber             TYPE cl_numberrange_intervals=>nr_nriv_line-tonumber
          nrlevel              TYPE cl_numberrange_intervals=>nr_nriv_line-nrlevel DEFAULT 0.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      "! Calculation of Price <br/>
      "!  <br/>
      "! Propagation to Z_SFLI_cl_flight_legacy=>calculate_flight_price.<br/>
      "! @parameter iv_seats_occupied_percent | occupied seats
      "! @parameter iv_flight_distance | flight distance in kilometer
      "! @parameter rv_price | calculated flight price
      calculate_flight_price
        IMPORTING
          iv_seats_occupied_percent TYPE Z_SFLI_plane_seats_occupied
          iv_flight_distance        TYPE Z_SFLI_flight_distance
        RETURNING
          VALUE(rv_price)           TYPE Z_SFLI_flight_price.

ENDCLASS.



CLASS zcl_sfli_data_generator IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    out->write( 'Starting Data Generation' ) ##NO_TEXT.

    out->write( 'Generate Data: Airport      Z_SFLI_AIRPORT' ) ##NO_TEXT.
    lcl_airport_data_generator=>lif_data_generator~create( out ).

    out->write( 'Generate Data: Carrier      Z_SFLI_CARRIER' ) ##NO_TEXT.
    lcl_carrier_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Connection   Z_SFLI_CONNECTION' ) ##NO_TEXT.
    lcl_connection_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Flight       Z_SFLI_FLIGHT' ) ##NO_TEXT.
    lcl_flight_data_generator=>lif_data_generator~create( out ).


    out->write( 'Generate Data: Agency       Z_SFLI_AGENCY' ) ##NO_TEXT.
    lcl_agency_data_generator=>lif_data_generator~create( out ).

    out->write( 'Generate Data: Customer      Z_SFLI_CUSTOMER' ) ##NO_TEXT.
    lcl_customer_data_generator=>lif_data_generator~create( out ).

    out->write( 'Generate Data: Supplement      Z_SFLI_SUPPLEMENT' ) ##NO_TEXT.
    lcl_supplement_data_generator=>lif_data_generator~create( out ).

    out->write( 'Generate Data: Travel      Z_SFLI_TRAVEL' ) ##NO_TEXT.
    out->write( 'Generate Data: Booking      Z_SFLI_BOOKING' ) ##NO_TEXT.
    out->write( 'Generate Data: Booking Supplement      Z_SFLI_BOOK_SUPPL' ) ##NO_TEXT.
    lcl_travel_data_generator=>lif_data_generator~create( out ).

    out->write( 'Generate Data: Status ValueHelps' ) ##NO_TEXT.
    lcl_status_vh_data_generator=>lif_data_generator~create( out ).


*    out->write(  'Calling BAdIs' ) ##NO_TEXT.
*
*    DATA lo_badi TYPE REF TO Z_SFLI_data_generation_badi.
*    GET BADI lo_badi.
*    CALL BADI lo_badi->data_generation
*      EXPORTING
*        out = out.
*    out->write(  'Finished Calling BAdIs' ) ##NO_TEXT.

    out->write( 'Finished Data Generation' ) ##NO_TEXT.
  ENDMETHOD.


  METHOD calculate_flight_price.
*    rv_price = Z_SFLI_cl_flight_legacy=>calculate_flight_price(
*                 iv_seats_occupied_percent = iv_seats_occupied_percent
*                 iv_flight_distance        = iv_flight_distance
*               ).
  ENDMETHOD.


  METHOD reset_numberrange_interval.

    DATA interval_found TYPE c.

    TRY.
        cl_numberrange_intervals=>read(
          EXPORTING
            object       = numberrange_object
            subobject    = subobject
          IMPORTING
            interval     = DATA(intervals) ).

*       Remove Intervals other than the requested one
        LOOP AT intervals INTO DATA(interval).
          IF interval-nrrangenr NE numberrange_interval.
*           Set the level to 0 before removing the interval (API requires this)
            IF interval-nrlevel NE 0.
              interval-nrlevel = 0.
              interval-procind = 'U'.
              cl_numberrange_intervals=>update(
                EXPORTING
                  interval  = VALUE #( ( interval ) )
                  object    = numberrange_object
                  subobject = subobject ).
            ENDIF.
            interval-procind = 'D'.
            cl_numberrange_intervals=>delete(
              EXPORTING
                interval  = VALUE #( ( interval ) )
                object    = numberrange_object
                subobject = subobject ).
          ENDIF.
        ENDLOOP.

*       Process the requested Interval
        CLEAR interval_found.
        LOOP AT intervals INTO interval.
          IF interval-nrrangenr EQ numberrange_interval.
            interval_found = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF interval_found IS INITIAL.  "Interval doesn't exist -> Create
          cl_numberrange_intervals=>create(
            EXPORTING
              interval  = VALUE #( ( subobject  = subobject
                                     nrrangenr  = numberrange_interval
                                     fromnumber = fromnumber
                                     tonumber   = tonumber
                                     nrlevel    = nrlevel
                                     procind    = 'I' ) )
              object    = numberrange_object
              subobject = subobject ).

        ELSE.  "Requested Interval exists -> Update, if required
          IF interval-nrlevel NE 0.
            interval-nrlevel = 0.
            interval-procind = 'U'.
            cl_numberrange_intervals=>update(
              EXPORTING
                interval  = VALUE #( ( interval ) )
                object    = numberrange_object
                subobject = subobject ).
          ENDIF.
          IF interval-fromnumber NE fromnumber OR
             interval-tonumber   NE tonumber.
            interval-procind = 'U'.
            interval-fromnumber = fromnumber.
            interval-tonumber   = tonumber.
            cl_numberrange_intervals=>update(
              EXPORTING
                interval  = VALUE #( ( interval ) )
                object    = numberrange_object
                subobject = subobject ).
          ENDIF.
*         Set the level to a default value, if requested
          IF nrlevel NE 0.
            interval-nrlevel = nrlevel.
            interval-procind = 'U'.
            cl_numberrange_intervals=>update(
              EXPORTING
                interval  = VALUE #( ( interval ) )
                object    = numberrange_object
                subobject = subobject ).
          ENDIF.
        ENDIF.

        COMMIT WORK.

      CATCH cx_nr_object_not_found INTO DATA(lx_nr_object_not_found).
      CATCH cx_nr_subobject        INTO DATA(lx_nr_subobject).
      CATCH cx_number_ranges       INTO DATA(lx_number_ranges).
    ENDTRY.

    IF lx_nr_object_not_found IS BOUND OR
       lx_nr_subobject        IS BOUND OR
       lx_number_ranges       IS BOUND.
      ASSERT 1 = 2.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
