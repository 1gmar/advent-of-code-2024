:- module(packs, []).
:- use_module(library(prolog_pack)).
:- attach_packs(packs, [replace(true)]).
:- initialization(install, main).

pack(lsp_server, []).

install :- pack_install_local(pack, packs, []).
