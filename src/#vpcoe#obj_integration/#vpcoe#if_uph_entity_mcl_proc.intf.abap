INTERFACE /vpcoe/if_uph_entity_mcl_proc
  PUBLIC .


  TYPES:
    BEGIN OF gty_mat_class,
      class_number TYPE klah-class,
    END OF gty_mat_class .
  TYPES:
    gty_t_mat_class TYPE STANDARD TABLE OF gty_mat_class WITH KEY class_number .
  TYPES:
    BEGIN OF gty_mat_class_parameter,
      objek TYPE cuobn,
      matnr type matnr,
      datuv TYPE datuv,
      class TYPE klah-class,
      lvorm TYPE lvorm,
    END OF gty_mat_class_parameter .
  TYPES:
    gty_t_mat_class_param TYPE STANDARD TABLE OF gty_mat_class_parameter WITH DEFAULT KEY .

  "! Get all relevant Material Classifications.
  METHODS get_relevant_mat_clas DEFAULT IGNORE
    RETURNING
      VALUE(rt_relevant_mat_clas) TYPE gty_t_mat_class .
  "! Retrieve data from material classification data based on the selection screen parameters.
  METHODS retrieve_mat_clas_data DEFAULT IGNORE
    IMPORTING
      !it_mat_clas_params     TYPE gty_t_mat_class_param
    RETURNING
      VALUE(rt_mat_clas_data) TYPE /vpcoe/uph_wrap_mcl .
  "! All mappings between material classification data and the target Entity Data (Packaging Element, Packaging Fraction, ...) must be implemented in this method.
  METHODS map_mat_clas_data DEFAULT IGNORE
    IMPORTING
      !it_mat_clas_data     TYPE /vpcoe/uph_wrap_mcl
    RETURNING
      VALUE(rt_entity_data) TYPE /vpcoe/t_uph_entity_data .
ENDINTERFACE.
