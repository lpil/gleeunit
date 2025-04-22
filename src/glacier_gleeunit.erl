-module(gleeunit_ffi).

-export([
         get_cwd_as_binary/0]]).

get_cwd_as_binary() ->
    iolist_to_binary(get_cwd()).
