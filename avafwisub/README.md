### To generate python lib

f2py3 -m avafwisub_static_lib -h avafwisub_static_lib.pyf avafwisub_interface.f90

f2py3 -c avafwisub_static_lib.pyf -L. avafwilib.a

### Pay attention

When erasing the generic name create in the SO file, keep the original name.