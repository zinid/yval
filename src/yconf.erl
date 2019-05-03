%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2002-2019 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(yconf).

%% API
-export([start/0, stop/0, parse/1, parse/2, fail/2]).
-export([format_error/1, format_error/2, best_match/2]).
%% Simple types
-export([pos_int/0, pos_int/1, non_neg_int/0, non_neg_int/1]).
-export([int/0, int/2, number/1, octal/0]).
-export([binary/0, binary/1]).
-export([enum/1, bool/0, atom/0, string/0, any/0]).
%% Complex types
-export([url/0, url_or_file/0]).
-export([file/0, file/1]).
-export([directory/0, directory/1]).
-export([ip/0, ipv4/0, ipv6/0, ip_mask/0, port/0]).
-export([re/0, glob/0]).
-export([path/0, binary_sep/1]).
-export([beam/0, beam/1]).
-export([timeout/1, timeout/2]).
%% Composite types
-export([list/1, sorted_list/1]).
-export([list_or_single/1, sorted_list_or_single/1]).
-export([map/1, sorted_map/1, map/2, sorted_map/2]).
-export([either/2, and_then/2]).
-export([options/1, options/2, options/3]).

-define(is_validator(Term), is_function(Term, 1)).
-define(STACK, yconf_stack).

-type infinity() :: infinity | infinite | unlimited.
-type timeout_unit() :: millisecond | second | minute | hour | day.
-type exports() :: [{atom(), arity()} | [{atom(), arity()}]].
-type options() :: [{atom(), term()}].
-type stack() :: [atom()].
-type yaml_val() :: atom() | number() | binary().
-type yaml_list() :: [yaml()].
-type yaml_map() :: [{yaml_val(), yaml()}].
-type yaml() :: yaml_val() | yaml_list() | yaml_map().
-type parse_option() :: replace_macros | {replace_macros, boolean()} |
			include_files | {include_files, boolean()}.
-type validator() :: fun((yaml()) -> term()).
-type validator(T) :: fun((yaml()) -> T).
-type validators() :: #{atom() => validator()}.
-type error_reason() :: term().

-export_type([validator/0]).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    case application:ensure_all_started(?MODULE) of
	{ok, _} -> ok;
	Err -> Err
    end.

stop() ->
    ok.

-spec parse(file:filename_all()) -> {ok, yaml_map()} | {error, error_reason()}.
parse(Path) ->
    parse(Path, [replace_macros, include_files]).

-spec parse(file:filename_all(), [parse_option()]) ->
		   {ok, yaml_map()} | {error, error_reason()}.
parse(Path0, Opts) ->
    Path = unicode:characters_to_binary(Path0),
    Validators = validators(Opts),
    try {ok, read_yaml(prep_path(Path), Validators, [])}
    catch _:{?MODULE, Why, Stack} ->
	    io:format("~s~n", [format_error(Why, Stack)]),
	    {error, Why, Stack}
    end.

-spec best_match(atom(), [atom()]) -> atom().
best_match(Pattern, []) ->
    Pattern;
best_match(Pattern, Opts) ->
    String = atom_to_list(Pattern),
    {Ds, _} = lists:mapfoldl(
                fun(Opt, Cache) ->
                        {Distance, Cache1} = ld(String, atom_to_list(Opt), Cache),
                        {{Distance, Opt}, Cache1}
                end, #{}, Opts),
    element(2, lists:min(Ds)).

-spec fail(term(), term()) -> no_return().
fail(Tag, Reason) ->
    erlang:error({Tag, Reason, erase_stack()}).

%%%===================================================================
%%% Validators
%%%===================================================================
-spec enum([atom()]) -> validator(atom()).
enum(List) ->
    fun(Val) ->
	    Atom = to_atom(Val),
	    case lists:member(Atom, List) of
		true -> Val;
		false -> fail({bad_enum, List, Atom})
	    end
    end.

-spec bool() -> validator(boolean()).
bool() ->
    fun(Val) ->
	    case to_atom(Val) of
		on -> true;
		off -> false;
		true -> true;
		false -> false;
		Bad -> fail({bad_bool, Bad})
	    end
    end.

-spec pos_int() -> validator(pos_integer()).
pos_int() ->
    fun(Val) ->
	    case to_int(Val) of
		I when I>0 -> I;
		Bad -> fail({bad_pos_int, Bad})
	    end
    end.

-spec pos_int(infinity()) -> validator(pos_integer() | infinity()).
pos_int(Inf) when Inf == infinity; Inf == infinite; Inf == unlimited ->
    fun(Val) ->
	    case to_int(Val, Inf) of
		I when I>0 -> I;
	        Bad -> fail({bad_pos_int, Inf, Bad})
	    end
    end.

-spec non_neg_int() -> validator(non_neg_integer()).
non_neg_int() ->
    fun(Val) ->
	    case to_int(Val) of
		I when I>=0 -> I;
		Bad -> fail({bad_non_neg_int, Bad})
	    end
    end.

-spec non_neg_int(infinity()) -> validator(non_neg_integer() | infinity()).
non_neg_int(Inf) when Inf == infinity; Inf == infinite; Inf == unlimited ->
    fun(Val) ->
	    case to_int(Val, Inf) of
		I when I>=0 -> I;
		Bad -> fail({bad_non_neg_int, Inf, Bad})
	    end
    end.

-spec int() -> validator(integer()).
int() ->
    fun to_int/1.

-spec int(integer(), integer()) -> validator(integer()).
int(Min, Max) when is_integer(Min), is_integer(Max), Min =< Max ->
    fun(Val) ->
	    case to_int(Val) of
		I when I>=Min, I=<Max -> I;
		Bad -> fail({bad_int, Min, Max, Bad})
	    end
    end.

-spec number(number()) -> validator(number()).
number(Min) ->
    fun(Val) ->
	    case to_number(Val) of
		N when N >= Min -> N;
		Bad -> fail({bad_number, Min, Bad})
	    end
    end.

-spec binary() -> validator(binary()).
binary() ->
    fun to_binary/1.

-spec binary(iodata()) -> validator(binary()).
binary(Regexp) when is_list(Regexp) orelse is_binary(Regexp) ->
    fun(Val) ->
	    Bin = to_binary(Val),
	    case re:run(Bin, Regexp) of
		{match, _} -> Bin;
		nomatch -> fail({nomatch, Regexp, Bin})
	    end
    end.

-spec atom() -> validator(atom()).
atom() ->
    fun to_atom/1.

-spec string() -> validator(string()).
string() ->
    fun to_string/1.

-spec binary_sep(iodata()) -> validator([binary()]).
binary_sep(Sep) ->
    fun(Val) ->
	    Bin = to_binary(Val),
	    lists:filtermap(
	      fun(<<>>) -> false;
		 (S) -> {true, S}
	      end, re:split(Bin, Sep))
    end.

-spec path() -> validator(binary()).
path() ->
    fun prep_path/1.

-spec file() -> validator(binary()).
file() ->
    file(read).

-spec file(read | write) -> validator(binary()).
file(read) ->
    fun(Val) ->
	    Path = prep_path(Val),
	    case file:open(Path, [read]) of
		{ok, Fd} ->
		    file:close(Fd),
		    Path;
		{error, Why} ->
		    fail({read_file, Why, Path})
	    end
    end;
file(write) ->
    fun(Val) ->
	    Path = prep_path(Val),
	    case filelib:ensure_dir(Path) of
		ok ->
		    case file:open(Path, [append]) of
			{ok, Fd} ->
			    file:close(Fd),
			    Path;
			{error, Why} ->
			    fail({create_file, Why, Path})
		    end;
		{error, Why} ->
		    fail({create_dir, Why, Path})
	    end
    end.

-spec directory() -> validator(binary()).
directory() ->
    directory(read).

-spec directory(read | write) -> validator(binary()).
directory(read) ->
    fun(Val) ->
	    Path = prep_path(Val),
	    case filelib:is_dir(Path) of
		true ->
		    Path;
		false ->
		    case file:list_dir(Path) of
			{error, Why} ->
			    fail({read_dir, Why, Path});
			{ok, _} ->
			    Path
		    end
	    end
    end;
directory(write) ->
    fun(Val) ->
	    Path = prep_path(Val),
	    case filelib:ensure_dir(filename:join(Path, "foo")) of
		ok ->
		    Path;
		{error, Why} ->
		    fail({create_dir, Why, Path})
	    end
    end.

-spec url() -> validator(binary()).
url() ->
    fun(Val) ->
	    URL = to_binary(Val),
	    case http_uri:parse(to_string(URL)) of
		{ok, {Scheme, _, _, _, _, _}} when Scheme /= http, Scheme /= https ->
		    fail({bad_url, {unsupported_scheme, Scheme}, URL});
		{ok, {_, _, Host, _, _, _}} when Host == ""; Host == <<"">> ->
		    fail({bad_url, empty_host, URL});
		{ok, _} ->
		    URL;
		{error, _} ->
		    fail({bad_url, URL})
	    end
    end.

-spec url_or_file() -> validator({url | file, binary()}).
url_or_file() ->
    fun(Val) ->
	    Bin = to_binary(Val),
	    case string:to_lower(binary_to_list(Bin)) of
		"http:/" ++ _ -> {url, (url())(Bin)};
		"https:/" ++ _ -> {url, (url())(Bin)};
		_ -> {file, (file())(Bin)}
	    end
    end.

-spec octal() -> validator(non_neg_integer()).
octal() ->
    fun(Val) ->
	    Bin = to_binary(Val),
	    try binary_to_integer(Bin, 8)
	    catch _:_ -> fail({bad_octal, Bin})
	    end
    end.

-spec ipv4() -> validator(inet:ip4_address()).
ipv4() ->
    fun(Val) ->
	    S = to_string(Val),
	    case inet:parse_ipv4_address(to_string(Val)) of
		{ok, IP} -> IP;
		_ -> fail({bad_ipv4, S})
	    end
    end.

-spec ipv6() -> validator(inet:ip6_address()).
ipv6() ->
    fun(Val) ->
	    S = to_string(Val),
	    case inet:parse_ipv6_address(S) of
		{ok, IP} -> IP;
		_ -> fail({bad_ipv6, S})
	    end
    end.

-spec ip() -> validator(inet:ip_address()).
ip() ->
    fun(Val) ->
	    S = to_string(Val),
	    case inet:parse_address(S) of
		{ok, IP} -> IP;
		_ -> fail({bad_ip, S})
	    end
    end.

-spec ip_mask() -> validator(
		     {inet:ip4_address(), 0..32} |
		     {inet:ip6_address(), 0..128}).
ip_mask() ->
    fun(Val) ->
	    S = to_string(Val),
	    case parse_ip_netmask(S) of
		{ok, IP, Mask} -> {IP, Mask};
		_ -> fail({bad_ip_mask, S})
	    end
    end.

-spec port() -> validator(1..65535).
port() ->
    int(1, 65535).

-spec timeout(timeout_unit()) -> validator(non_neg_integer()).
timeout(Unit) ->
    fun(Val) ->
	    case to_int(Val) of
		I when I>0 -> to_ms(I, Unit);
		Bad -> fail({bad_timeout, Bad})
	    end
    end.

-spec timeout(timeout_unit(), infinity()) -> validator(non_neg_integer() | infinity()).
timeout(Unit, Inf) ->
    fun(Val) ->
	    case to_int(Val, Inf) of
		I when I>0 -> to_ms(I, Unit);
		Bad -> fail({bad_timeout, Inf, Bad})
	    end
    end.

-spec re() -> re:mp().
re() ->
    fun(Val) ->
	    Bin = to_binary(Val),
	    case re:compile(Bin) of
		{ok, RE} -> RE;
		_ -> fail({bad_regexp, Bin})
	    end
    end.

-spec glob() -> validator(re:mp()).
glob() ->
    fun(Val) ->
	    S = to_string(Val),
	    case re:compile(sh_to_awk(S)) of
		{ok, RE} -> RE;
		_ -> fail({bad_glob, S})
	    end
    end.

-spec beam() -> validator(module()).
beam() ->
    beam([]).

-spec beam(exports()) -> validator(module()).
beam(Exports) ->
    fun(Val) ->
	    Mod = to_atom(Val),
	    case code:ensure_loaded(Mod) of
		{module, Mod} ->
		    lists:foreach(
		      fun([]) ->
			      ok;
			 (L) when is_list(L) ->
			      case lists:any(
				     fun({F, A}) ->
					     erlang:function_exported(Mod, F, A)
				     end, L) of
				  true -> ok;
				  false -> fail({bad_export, hd(L), Mod})
			      end;
			 ({F, A}) ->
			      case erlang:function_exported(Mod, F, A) of
				  true -> ok;
				  false -> fail({bad_export, {F, A}, Mod})
			      end
		      end, Exports),
		    Mod;
		_ ->
		    fail({bad_module, Mod})
	    end
    end.

-spec list(validator(T)) -> validator([T]).
list(Fun) when ?is_validator(Fun) ->
    fun(L) when is_list(L) ->
	    lists:map(Fun, L);
       (Bad) ->
	    fail({bad_list, Bad})
    end.

-spec sorted_list(validator(T)) -> validator([T]).
sorted_list(Fun) when ?is_validator(Fun) ->
    fun(L) when is_list(L) ->
	    lists:map(Fun, lists:sort(L));
       (Bad) ->
	    fail({bad_list, Bad})
    end.

-spec list_or_single(validator(T)) -> validator([T]).
list_or_single(Fun) when ?is_validator(Fun) ->
    fun(L) when is_list(L) ->
	    lists:map(Fun, L);
       (V) ->
	    [Fun(V)]
    end.

-spec sorted_list_or_single(validator(T)) -> validator([T]).
sorted_list_or_single(Fun) when ?is_validator(Fun) ->
    fun(L) when is_list(L) ->
	    lists:map(Fun, lists:sort(L));
       (V) ->
	    [Fun(V)]
    end.

-spec map(fun((yaml_val(), yaml()) -> T)) -> validator([T]).
map(Fun) when is_function(Fun, 2) ->
    fun(L) when is_list(L) ->
	    lists:map(
	      fun({Key, Val}) ->
		      Fun(Key, Val);
		 (_) ->
		      fail({bad_map, L})
	      end, L);
       (Bad) ->
	    fail({bad_map, Bad})
    end.

-spec sorted_map(fun((yaml_val(), yaml()) -> T)) -> validator([T]).
sorted_map(Fun) when is_function(Fun, 2) ->
    fun(L) when is_list(L) ->
	    try lists:keysort(1, L) of
		L1 ->
		    lists:map(
		      fun({Key, Val}) ->
			      Fun(Key, Val);
			 (_) ->
			      fail({bad_map, L})
		      end, L1)
	    catch _:badarg ->
		    fail({bad_map, L})
	    end;
       (Bad) ->
	    fail({bad_map, Bad})
    end.

-spec map(validator(T1), validator(T2)) -> validator([{T1, T2}]).
map(Fun1, Fun2) when ?is_validator(Fun1) andalso
		     ?is_validator(Fun2) ->
    fun(L) when is_list(L) ->
	    lists:map(
	      fun({Key, Val}) ->
		      {Fun1(Key), Fun2(Val)};
		 (_) ->
		      fail({bad_map, L})
	      end, L);
       (Bad) ->
	    fail({bad_map, Bad})
    end.

-spec sorted_map(validator(T1), validator(T2)) -> validator([{T1, T2}]).
sorted_map(Fun1, Fun2) when ?is_validator(Fun1) andalso
			    ?is_validator(Fun2) ->
    fun(L) when is_list(L) ->
	    try lists:keysort(1, L) of
		L1 ->
		    lists:map(
		      fun({Key, Val}) ->
			      {Fun1(Key), Fun2(Val)};
			 (_) ->
			      fail({bad_map, L})
		      end, L1)
	    catch _:badarg ->
		    fail({bad_map, L})
	    end;
       (Bad) ->
	    fail({bad_map, Bad})
    end.

-spec either(atom(), validator(T)) -> validator(atom() | T);
	    (validator(T1), validator(T2)) -> validator(T1 | T2).
either(Atom, Fun) when is_atom(Atom) andalso ?is_validator(Fun) ->
    either(enum([Atom]), Fun);
either(Fun1, Fun2) when ?is_validator(Fun1) andalso
			?is_validator(Fun2) ->
    fun(Val) ->
	    Stack = get(?STACK),
	    try Fun1(Val)
	    catch _:_ ->
		    put(?STACK, Stack),
		    Fun2(Val)
	    end
    end.

-spec and_then(validator(T1), fun((T1) -> T2)) -> validator(T2).
and_then(Fun, Then) when ?is_validator(Fun) andalso
			 is_function(Then, 1) ->
    fun(Val) -> Then(Fun(Val)) end.

-spec any() -> validator(yaml()).
any() ->
    fun(Val) -> Val end.

-spec options(validators()) -> validator().
options(Validators) ->
    options(Validators, []).

-spec options(validators(), [atom()]) -> validator().
options(Validators, Required) ->
    options(Validators, Required, undefined).

-spec options(validators(), [atom()],
	      undefined | fun((options(), options()) -> term())) -> validator().
options(Validators, Required, Fun) ->
    fun(Opts) when is_list(Opts) ->
	    case validate_options(Opts, Validators, Required) of
		{Known, [], []} ->
		    Known;
		{_, _, [Opt|_]} ->
		    fail({missing_option, Opt});
		{_, [{Opt, _}|_], []} when Fun == undefined ->
		    fail({unknown_option, Opt, maps:keys(Validators)});
		{Known, Unknown, []} ->
		    Fun(Known, Unknown)
	    end;
       (Bad) ->
	    fail({bad_map, Bad})
    end.

%%%===================================================================
%%% Formatters
%%%===================================================================
-spec format_error(error_reason(), stack()) -> string().
format_error(Why, []) ->
    format_error(Why);
format_error(Why, Stack) ->
    [H|T] = format_error(Why),
    format_stack(Stack) ++ ": " ++ [string:to_lower(H)|T].

-spec format_error(error_reason()) -> string().
format_error({bad_atom, Bad}) ->
    format("Expected string, got ~s", [format_yaml_type(Bad)]);
format_error({bad_binary, Bad}) ->
    format("Expected string, got ~s", [format_yaml_type(Bad)]);
format_error({bad_bool, Bad}) ->
    format("Expected boolean, got ~s", [format_yaml_type(Bad)]);
format_error({bad_enum, _, Atom}) ->
    format("Unexpected value: ~s", [Atom]);
format_error({bad_export, {F, A}, Mod}) ->
    format("Module '~s' doesn't export function ~s/~B", [Mod, F, A]);
format_error({bad_glob, S}) ->
    format("Invalid glob expression: ~s", [S]);
format_error({bad_int, Bad}) ->
    format("Expected integer, got ~s", [format_yaml_type(Bad)]);
format_error({bad_int, Min, Max, Bad}) ->
    format("Expected integer between ~B and ~B, got: ~B", [Min, Max, Bad]);
format_error({bad_ip_mask, S}) ->
    format("Invalid IP address or network mask: ~s", [S]);
format_error({bad_ip, S}) ->
    format("Invalid IP address: ~s", [S]);
format_error({bad_ipv4, S}) ->
    format("Invalid IPv4 address: ~s", [S]);
format_error({bad_ipv6, S}) ->
    format("Invalid IPv6 address: ~s", [S]);
format_error({bad_list, Bad}) ->
    format("Expected list, got ~s", [format_yaml_type(Bad)]);
format_error({bad_map, Bad}) ->
    format("Expected map, got ~s", [format_yaml_type(Bad)]);
format_error({bad_module, Mod}) ->
    format("Unknown Erlang module: ~s", [Mod]);
format_error({bad_non_neg_int, Bad}) ->
    format("Expected non negative integer, got: ~B", [Bad]);
format_error({bad_non_neg_int, Inf, Bad}) ->
    format("Expected non negative integer or '~s', got: ~B", [Inf, Bad]);
format_error({bad_number, Bad}) ->
    format("Expected number, got ~s", [format_yaml_type(Bad)]);
format_error({bad_number, Min, Bad}) ->
    format("Expected number >= ~B, got: ~p", [Min, Bad]);
format_error({bad_octal, Bad}) ->
    format("Expected octal, got: ~s", [Bad]);
format_error({bad_pos_int, Bad}) ->
    format("Expected positive integer, got: ~B", [Bad]);
format_error({bad_pos_int, Inf, Bad}) ->
    format("Expected positive integer or '~s', got: ~B", [Inf, Bad]);
format_error({bad_regexp, Bin}) ->
    format("Invalid regular expression: ~s", [Bin]);
format_error({bad_timeout, Bad}) ->
    format_error({bad_pos_int, Bad});
format_error({bad_timeout, Inf, Bad}) ->
    format_error({bad_pos_int, Inf, Bad});
format_error({bad_url, empty_host, URL}) ->
    format("Empty hostname in the URL: ~s", [URL]);
format_error({bad_url, {unsupported_scheme, Scheme}, URL}) ->
    format("Unsupported scheme '~s' in the URL: ~s", [Scheme, URL]);
format_error({bad_url, URL}) ->
    format("Invalid URL: ~s", [URL]);
format_error({bad_yaml, circular_include, Path}) ->
    format("Circularly included YAML file: ~s", [Path]);
format_error({bad_yaml, Why, Path}) ->
    format("Failed to read YAML file '~s': ~s",
	   [Path, fast_yaml:format_error(Why)]);
format_error({create_dir, Why, Path}) ->
    format("Failed to create directory '~s': ~s",
	   [Path, file:format_error(Why)]);
format_error({create_file, Why, Path}) ->
    format("Failed to open file '~s' for writing: ~s",
	   [Path, file:format_error(Why)]);
format_error({missing_option, Opt}) ->
    format("Missing required options: ~s", [Opt]);
format_error({nomatch, Regexp, Bin}) ->
    format("String '~s' doesn't match regular expression: ~s",
	   [Bin, Regexp]);
format_error({read_dir, Why, Path}) ->
    format("Failed to read directory '~s': ~s",
	   [Path, file:format_error(Why)]);
format_error({read_file, Why, Path}) ->
    format("Failed to read file '~s': ~s",
	   [Path, file:format_error(Why)]);
format_error({unknown_option, Opt, _}) ->
    format("Unknown option: ~s", [Opt]);
format_error(Bad) ->
    format("Unexpected error reason: ~p", [Bad]).

-spec format_stack(stack()) -> string().
format_stack([_|_] = Stack) ->
    format("Invalid value of option ~s",
	   [string:join([atom_to_list(A) || A <- Stack], "->")]).

-spec format(iodata(), list()) -> string().
format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

-spec format_yaml_type(yaml()) -> string().
format_yaml_type(<<>>) ->
    "empty string";
format_yaml_type('') ->
    "empty string";
format_yaml_type([]) ->
    "empty list";
format_yaml_type(I) when is_integer(I) ->
    "integer";
format_yaml_type(N) when is_number(N) ->
    "number";
format_yaml_type(B) when is_binary(B) ->
    "string";
format_yaml_type(A) when is_atom(A) ->
    "string";
format_yaml_type([{_, _}|_]) ->
    "map";
format_yaml_type([_|_]) ->
    "list".

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================
%%% Initial YAML parsing
%%%===================================================================
-spec read_yaml(binary(), validators(), [binary()]) -> yaml_map().
read_yaml(Path, Validators, Paths) ->
    case lists:member(Path, Paths) of
	true ->
	    fail({bad_yaml, circular_include, Path});
	false ->
	    case fast_yaml:decode_from_file(Path) of
		{ok, [Y]} ->
		    V = options(
			  Validators, [],
			  fun(Os, Y1) ->
				  Includes = lists:flatmap(
					       fun({include_config_file, I}) -> I;
						  (_) -> []
					       end, Os),
				  Macros = lists:flatmap(
					     fun({define_macro, M}) -> M;
						(_) -> []
					     end, Os),
				  Y2 = Y1 ++ include_files(Includes, Validators, [Path|Paths]),
				  replace_macros(Y2, Macros)
			  end),
		    V(Y);
		{error, Why} ->
		    fail({bad_yaml, Why, Path})
	    end
    end.

-spec replace_macros(yaml_map(), [{binary(), yaml()}]) -> yaml_map().
replace_macros(Y1, Macros) ->
    lists:foldl(
      fun(Macro, Y2) ->
	      replace_macro(Y2, Macro)
      end, Y1, lists:flatten(Macros)).

-spec replace_macro(yaml(), {binary(), yaml()}) -> yaml().
replace_macro(L, Macro) when is_list(L) ->
    [replace_macro(E, Macro) || E <- L];
replace_macro({K, V}, Macro) ->
    {K, replace_macro(V, Macro)};
replace_macro(Name, {Name, Val}) ->
    Val;
replace_macro(Val, _) ->
    Val.

-spec include_files(yaml(), validators(), [binary()]) -> yaml_map().
include_files(Includes, Validators, Paths) ->
    lists:flatmap(
      fun({Path, {Disallow, AllowOnly}}) ->
	      Y = read_yaml(Path, Validators, Paths),
	      lists:filter(
		fun({Opt, _}) ->
			case AllowOnly of
			    [] ->
				not lists:member(Opt, Disallow);
			    _ ->
				lists:member(Opt, AllowOnly)
			end
		end, Y);
	 (Path) ->
	      read_yaml(Path, Validators, Paths)
      end, Includes).

%%%===================================================================
%%% Auxiliary functions
%%%===================================================================
-spec to_binary(term()) -> binary().
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) ->
    B;
to_binary(Bad) ->
    fail({bad_binary, Bad}).

-spec to_atom(term()) -> atom().
to_atom(B) when is_binary(B) ->
    list_to_atom(string:to_lower(binary_to_list(B)));
to_atom(A) when is_atom(A) ->
    list_to_atom(string:to_lower(atom_to_list(A)));
to_atom(Bad) ->
    fail({bad_atom, Bad}).

-spec to_string(term()) -> string().
to_string(S) ->
    binary_to_list(to_binary(S)).

to_int(I) when is_integer(I) ->
    I;
to_int(B) when is_binary(B) ->
    try binary_to_integer(B)
    catch _:_ -> fail({bad_int, B})
    end;
to_int(Bad) ->
    fail({bad_int, Bad}).

to_int(I, _) when is_integer(I) -> I;
to_int(infinity, Inf) -> Inf;
to_int(infinite, Inf) -> Inf;
to_int(unlimited, Inf) -> Inf;
to_int('∞', Inf) -> Inf;
to_int(B, Inf) when is_binary(B) ->
    try binary_to_integer(B)
    catch _:_ ->
	    try binary_to_atom(B, utf8) of
		A -> to_int(A, Inf)
	    catch _:_ ->
		    fail({bad_int, B})
	    end
    end;
to_int(Bad, _) ->
    fail({bad_int, Bad}).

to_number(N) when is_number(N) ->
    N;
to_number(B) when is_binary(B) ->
    try binary_to_integer(B)
    catch _:_ ->
	    try binary_to_float(B)
	    catch _:_ -> {bad_number, B}
	    end
    end;
to_number(Bad) ->
    fail({bad_number, Bad}).

-spec to_ms(non_neg_integer() | infinity(), timeout_unit()) -> non_neg_integer() | infinity().
to_ms(Inf, _) when is_atom(Inf) ->
    Inf;
to_ms(I, Unit) ->
    case Unit of
	millisecond -> I;
	second -> timer:seconds(I);
	minute -> timer:minutes(I);
	hour -> timer:hours(I);
	day -> timer:hours(I*24)
    end.

-spec parse_ip_netmask(string()) -> {ok, inet:ip4_address(), 0..32} |
				    {ok, inet:ip6_address(), 0..128} |
				    error.
parse_ip_netmask(S) ->
    case string:tokens(S, "/") of
	[IPStr] ->
	    case inet:parse_address(IPStr) of
		{ok, {_, _, _, _} = IP} -> {ok, IP, 32};
		{ok, {_, _, _, _, _, _, _, _} = IP} -> {ok, IP, 128};
		_ -> error
	    end;
	[IPStr, MaskStr] ->
	    try list_to_integer(MaskStr) of
		Mask when Mask >= 0 ->
		    case inet:parse_address(IPStr) of
			{ok, {_, _, _, _} = IP} when Mask =< 32 ->
			    {ok, IP, Mask};
			{ok, {_, _, _, _, _, _, _, _} = IP} when Mask =< 128 ->
			    {ok, IP, Mask};
			_ ->
			    error
		    end;
		_ ->
		    error
	    catch _:_ ->
		    error
	    end;
	_ ->
	    error
    end.

%% Levenshtein distance
-spec ld(string(), string(), map()) -> {non_neg_integer(), map()}.
ld([] = S, T, Cache) ->
    {length(T), maps:put({S, T}, length(T), Cache)};
ld(S, [] = T, Cache) ->
    {length(S), maps:put({S, T}, length(S), Cache)};
ld([X|S], [X|T], Cache) ->
    ld(S, T, Cache);
ld([_|ST] = S, [_|TT] = T, Cache) ->
    try {maps:get({S, T}, Cache), Cache}
    catch _:{badkey, _} ->
            {L1, C1} = ld(S, TT, Cache),
            {L2, C2} = ld(ST, T, C1),
            {L3, C3} = ld(ST, TT, C2),
            L = 1 + lists:min([L1, L2, L3]),
            {L, maps:put({S, T}, L, C3)}
    end.

-spec fail(error_reason()) -> no_return().
fail(Reason) ->
    fail(?MODULE, Reason).

-spec prep_path(term()) -> binary().
prep_path(Path0) ->
    Path1 = to_binary(Path0),
    case filename:pathtype(Path1) of
	relative ->
	    case file:get_cwd() of
		{ok, CWD} ->
		    filename:join(
		      unicode:characters_to_binary(CWD), Path1);
		{error, Reason} ->
		    error_logger:warning_msg(
		      "Failed to get current directory name: ~s",
		      [file:format_error(Reason)]),
		    Path1
	    end;
	_ ->
	    Path1
    end.

-spec validate_options(list(), validators(), [atom()]) ->
			      {options(), options(), [atom()]}.
validate_options(Opts, Validators, Required) ->
    validate_options(Opts, Validators, Required, [], []).

-spec validate_options(list(), validators(), [atom()], options(), options()) ->
			      {options(), options(), [atom()]}.
validate_options([{O, Val}|Opts], Validators, Required, Known, Unknown) ->
    Opt = to_atom(O),
    try maps:get(Opt, Validators) of
	Validator ->
	    Required1 = lists:delete(Opt, Required),
	    Known1 = [{Opt, validate_option(Opt, Val, Validator)}|Known],
	    validate_options(Opts, Validators, Required1, Known1, Unknown)
    catch _:{badkey, _} ->
	    Unknown1 = [{Opt, Val}|Unknown],
	    validate_options(Opts, Validators, Required, Known, Unknown1)
    end;
validate_options([], _, Required, Known, Unknown) ->
    {lists:reverse(Known), lists:reverse(Unknown), Required};
validate_options(Bad, _, _, _, _) ->
    fail({bad_map, Bad}).

validate_option(Opt, Val, Validator) ->
    Stack = case get(?STACK) of
		undefined -> [];
		S -> S
	   end,
    put(?STACK, [Opt|Stack]),
    Ret = Validator(Val),
    put(?STACK, Stack),
    Ret.

-spec erase_stack() -> stack().
erase_stack() ->
    case erase(?STACK) of
	S when is_list(S) -> lists:reverse(S);
	_ -> []
    end.

%%%===================================================================
%%% Copied from xmerl_regexp.erl to avoid xmerl dependency
%%%===================================================================
-spec sh_to_awk(string()) -> string().
sh_to_awk(Sh) -> "^(" ++ sh_to_awk_1(Sh).	%Fix the beginning

sh_to_awk_1([$*|Sh]) ->				%This matches any string
    ".*" ++ sh_to_awk_1(Sh);
sh_to_awk_1([$?|Sh]) ->				%This matches any character
    [$.|sh_to_awk_1(Sh)];
sh_to_awk_1([$[,$^,$]|Sh]) ->			%This takes careful handling
    "\\^" ++ sh_to_awk_1(Sh);
%% Must move '^' to end.
sh_to_awk_1("[^" ++ Sh) -> [$[|sh_to_awk_2(Sh, true)];
sh_to_awk_1("[!" ++ Sh) -> "[^" ++ sh_to_awk_2(Sh, false);
sh_to_awk_1([$[|Sh]) -> [$[|sh_to_awk_2(Sh, false)];
sh_to_awk_1([C|Sh]) ->
    %% Unspecialise everything else which is not an escape character.
    case sh_special_char(C) of
	true -> [$\\,C|sh_to_awk_1(Sh)];
	false -> [C|sh_to_awk_1(Sh)]
    end;
sh_to_awk_1([]) -> ")$".			%Fix the end

sh_to_awk_2([$]|Sh], UpArrow) -> [$]|sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_2(Sh, UpArrow) -> sh_to_awk_3(Sh, UpArrow).

sh_to_awk_3([$]|Sh], true) -> "^]" ++ sh_to_awk_1(Sh);
sh_to_awk_3([$]|Sh], false) -> [$]|sh_to_awk_1(Sh)];
sh_to_awk_3([C|Sh], UpArrow) -> [C|sh_to_awk_3(Sh, UpArrow)];
sh_to_awk_3([], true) -> [$^|sh_to_awk_1([])];
sh_to_awk_3([], false) -> sh_to_awk_1([]).

%% Test if a character is a special character.
-spec sh_special_char(char()) -> boolean().
sh_special_char($|) -> true;
sh_special_char($*) -> true;
sh_special_char($+) -> true;
sh_special_char($?) -> true;
sh_special_char($() -> true;
sh_special_char($)) -> true;
sh_special_char($\\) -> true;
sh_special_char($^) -> true;
sh_special_char($$) -> true;
sh_special_char($.) -> true;
sh_special_char($[) -> true;
sh_special_char($]) -> true;
sh_special_char($") -> true;
sh_special_char(_C) -> false.

%%%===================================================================
%%% Bootstrapping validator
%%%===================================================================
-spec validators([parse_option()]) -> validators().
validators(Opts) when is_list(Opts) ->
    lists:foldl(
      fun(O, V) when O == replace_macros; O == {replace_macros, true} ->
	      maps:put(define_macro, validator(define_macro), V);
	 (O, V) when O == include_files; O == {include_files, true} ->
	      maps:put(include_config_file, validator(include_config_file), V);
	 (_, V) ->
	      V
      end, #{}, Opts).

validator(define_macro) ->
    map(binary("^[A-Z_0-9]+$"), any());
validator(include_config_file) ->
    either(
      list_or_single(path()),
      map(path(),
	  and_then(
	    options(
	      #{disallow => list(atom()),
		allow_only => list(atom())}),
	    fun(Opts) ->
		    {proplists:get_value(disallow, Opts, []),
		     proplists:get_value(allow_only, Opts, [])}
	    end))).
