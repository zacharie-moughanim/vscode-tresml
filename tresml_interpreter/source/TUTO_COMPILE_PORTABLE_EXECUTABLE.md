To open tresml_nolib in the docker image to compile to a portable executable:
- `sudo docker run --rm -it -v ~/Documents/tresfreros/tresml_nolib_project/tresml_nolib:/home/ocaml/tresml_nolib ocamlpro/ocaml:5.4` and then, within docker :
        Setting up the thingy, installing static libs and creating an opam switch
    - `sudo apk add openssl-libs-static`
    - `opam switch create . --deps ocaml-system`
    - `eval $(opam env)`
        Copying the source code of tresml_nolib, because we don't have permission to modify it directly apparently
    - `mkdir sandbox`
    - `cp -r tresml_nolib sandbox`
    - `cd sandbox/tresml_nolib`
        Installing libraries needed for compilation of tresml_nolib
    - `opam install ocamlfind`
    - `make`
        Moving the compiled stuff in the shared folder
    - `cd ..`
    - `mv tresml_nolib tresml_nolib_compiled`
    - `cd ..`
    - `sudo cp -r sandbox/treml_nolib_compiled tresml_nolib`
    - `exit`
Test it worked with `lld` and `file`, expected output:
```
% file produce_page.x
produce_page.x: ELF 64-bit LSB xcutable, x86-64, version 1 (SYSV), statically linked, BuildID[sha1]=46a3f7827e894f1eee2eba17a42be2643b61ab1f, with debug_info, not stripped
```
```
% ldd produce_page.x
    not a dynamic xcutable
```

https://ocamlpro.com/fr/blog/2021_09_02_generating_static_and_portable_executables_with_ocaml/
