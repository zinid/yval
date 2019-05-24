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
-export([start/0, stop/0]).
-export([parse/2, parse/3, validate/2, fail/2]).
-export([format_error/1, format_error/2, format_ctx/1]).
%% Simple types
-export([pos_int/0, pos_int/1, non_neg_int/0, non_neg_int/1]).
-export([int/0, int/2, number/1, octal/0]).
-export([binary/0, binary/1]).
-export([enum/1, bool/0, atom/0, string/0, any/0]).
%% Complex types
-export([url/0, url/1]).
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
-export([either/2, and_then/2, non_empty/1]).
-export([options/1, options/2]).

-define(is_validator(Term), is_function(Term, 1)).
-ifndef(deprecated_stacktrace).
-define(EX_RULE(Class, Reason, Stack), Class:Reason:Stack).
-define(EX_STACK(Stack), Stack).
-else.
-define(EX_RULE(Class, Reason, _), Class:Reason).
-define(EX_STACK(_), erlang:get_stacktrace()).
-endif.

-type infinity() :: infinity | infinite | unlimited.
-type timeout_unit() :: millisecond | second | minute | hour | day.
-type exports() :: [{atom(), arity()} | [{atom(), arity()}]].
-type options() :: [{atom(), term()}].
-type macro() :: {binary(), yaml()}.
-type includes() :: [{binary(), {[atom()], [atom()]}} | binary()].
-type ctx() :: [atom() | binary() | integer()].
-type yaml_val() :: atom() | number() | binary().
-type yaml_list() :: [yaml()].
-type yaml_map() :: [{yaml_val(), yaml()}].
-type yaml() :: yaml_val() | yaml_list() | yaml_map().
-type parse_option() :: replace_macros | {replace_macros, boolean()} |
			include_files | {include_files, boolean()} |
			plain_as_atom | {plain_as_atom, boolean()}.
-type validator_option() :: {required, [atom()]} |
			    {disallowed, [atom()]} |
			    check_dups | {check_dups, boolean()}.
-type validator() :: fun((yaml()) -> term()).
-type validator(T) :: fun((yaml()) -> T).
-type validators() :: #{atom() => validator()}.
-type error_reason() :: term().
-type error_return() :: {error, error_reason(), ctx()}.

-export_type([validator/0, validator/1, validators/0]).
-export_type([error_return/0, error_reason/0, ctx/0]).

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

-spec parse(file:filename_all(), validator() | validators()) ->
		   {ok, options()} | {error, error_reason()}.
parse(Path, Validators) ->
    parse(Path, Validators, [check_dups]).

-spec parse(file:filename_all(), validator() | validators(),
	    [parse_option() | validator_option()]) ->
		   {ok, term()} | {error, error_reason()}.
parse(Path0, Validators, Opts) ->
    Path = unicode:characters_to_binary(Path0),
    {Opts1, Opts2} = proplists:split(
		       proplists:compact(Opts),
		       [replace_macros, include_files, plain_as_atom]),
    Opts3 = lists:flatten(Opts1),
    try
	Y = read_yaml(prep_path(Path), Opts3, []),
	Validators1 = maps:merge(Validators, validators(Opts3)),
	{ok, (options(Validators1, Opts2))(Y)}
    catch _:{?MODULE, Why, Ctx} ->
	    {error, Why, Ctx};
	  ?EX_RULE(Class, Reason, Stacktrace) ->
	    erase_ctx(),
	    erlang:raise(Class, Reason, ?EX_STACK(Stacktrace))
    end.

validate(Validator, Y) ->
    try {ok, Validator(Y)}
    catch _:{?MODULE, Why, Ctx} ->
	    {error, Why, Ctx};
	  ?EX_RULE(Class, Reason, Stacktrace) ->
	    erase_ctx(),
	    erlang:raise(Class, Reason, ?EX_STACK(Stacktrace))
    end.

-spec fail(term(), term()) -> no_return().
fail(Tag, Reason) ->
    erlang:error({Tag, Reason, erase_ctx()}).

%%%===================================================================
%%% Validators
%%%===================================================================
-spec enum([atom() | binary()]) -> validator(atom() | binary()).
enum([H|_] = List) when is_atom(H); is_binary(H) ->
    fun(Val) ->
	    Member = if is_binary(H) -> to_binary(Val);
			is_atom(H) -> to_atom(Val)
		     end,
	    case lists:member(Member, List) of
		true -> Member;
		false -> fail({bad_enum, List, Member})
	    end
    end.

-spec bool() -> validator(boolean()).
bool() ->
    fun(Val) ->
	    case to_atom(Val) of
		on -> true;
		off -> false;
		yes -> true;
		no -> false;
		y -> true;
		n -> false;
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
    url([http, https]).

-spec url([atom()]) -> validator(binary()).
url(Schemes) ->
    fun(Val) ->
	    URL = to_binary(Val),
	    case http_uri:parse(to_string(URL)) of
		{ok, {_, _, Host, _, _, _}} when Host == ""; Host == <<"">> ->
		    fail({bad_url, empty_host, URL});
		{ok, {Scheme, _, _, _, _, _}} when Schemes /= [] ->
		    case lists:member(Scheme, Schemes) of
			true -> URL;
			false ->
			    fail({bad_url, {unsupported_scheme, Scheme}, URL})
		    end;
		{ok, _} ->
		    URL;
		{error, Why} ->
		    fail({bad_url, Why, URL})
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
	    case inet:parse_ipv6strict_address(S) of
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
		{error, Why} -> fail({bad_regexp, Why, Bin})
	    end
    end.

-spec glob() -> validator(re:mp()).
glob() ->
    fun(Val) ->
	    S = to_string(Val),
	    case re:compile(sh_to_awk(S)) of
		{ok, RE} -> RE;
		{error, Why} -> fail({bad_glob, Why, S})
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

-spec non_empty(validator(T)) -> validator(T).
non_empty(Fun) ->
    fun(Val) ->
	    case Fun(Val) of
		'' -> fail(empty_atom);
		<<"">> -> fail(empty_binary);
		[] -> fail(empty_list);
		Ret -> Ret
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
		      Key1 = Fun1(Key),
		      Ctx = get_ctx(),
		      put_ctx([Key1|Ctx]),
		      Val1 = Fun2(Val),
		      put_ctx(Ctx),
		      {Key1, Val1};
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
			      Key1 = Fun1(Key),
			      Ctx = get_ctx(),
			      put_ctx([Key1|Ctx]),
			      Val1 = Fun2(Val),
			      put_ctx(Ctx),
			      {Key1, Val1};
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
	    Ctx = get_ctx(),
	    try Fun1(Val)
	    catch _:_ ->
		    put_ctx(Ctx),
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
    options(Validators, [check_dups]).

-spec options(validators(), [validator_option()]) -> validator().
options(Validators, Options) ->
    fun(Opts) when is_list(Opts) ->
	    Required = proplists:get_value(required, Options, []),
	    Disallowed = proplists:get_value(disallowed, Options, []),
	    CheckDups = proplists:get_bool(check_dups, Options),
	    DefaultValidator = maps:get('_', Validators, undefined),
	    validate_options(Opts, Validators, DefaultValidator,
			     Required, Disallowed, CheckDups);
       (Bad) ->
	    fail({bad_map, Bad})
    end.

%%%===================================================================
%%% Formatters
%%%===================================================================
-spec format_error(error_reason(), ctx()) -> string().
format_error(Why, []) ->
    format_error(Why);
format_error(Why, Ctx) ->
    [H|T] = format_error(Why),
    format_ctx(Ctx) ++ ": " ++ [string:to_lower(H)|T].

-spec format_error(error_reason()) -> string().
format_error({bad_atom, Bad}) ->
    format("Expected string, got ~s instead", [format_yaml_type(Bad)]);
format_error({bad_binary, Bad}) ->
    format("Expected string, got ~s instead", [format_yaml_type(Bad)]);
format_error({bad_bool, Bad}) ->
    format("Expected boolean, got ~s instead", [format_yaml_type(Bad)]);
format_error({bad_cwd, Why}) ->
    format("Failed to get current directory name: ~s",
	   [file:format_error(Why)]);
format_error({bad_enum, _, Val}) ->
    format("Unexpected value: ~s", [Val]);
format_error({bad_export, {F, A}, Mod}) ->
    format("Module '~s' doesn't export function ~s/~B", [Mod, F, A]);
format_error({bad_glob, {Reason, _}, _}) ->
    format("Invalid glob expression: ~s", [Reason]);
format_error({bad_int, Bad}) ->
    format("Expected integer, got ~s instead", [format_yaml_type(Bad)]);
format_error({bad_int, Min, Max, Bad}) ->
    format("Expected integer from ~B to ~B, got: ~B", [Min, Max, Bad]);
format_error({bad_ip_mask, S}) ->
    format("Invalid IP address or network mask: ~s", [S]);
format_error({bad_ip, S}) ->
    format("Invalid IP address: ~s", [S]);
format_error({bad_ipv4, S}) ->
    format("Invalid IPv4 address: ~s", [S]);
format_error({bad_ipv6, S}) ->
    format("Invalid IPv6 address: ~s", [S]);
format_error({bad_length, Limit}) ->
    format("The value must not exceed ~B octets in length", [Limit]);
format_error({bad_list, Bad}) ->
    format("Expected list, got ~s instead", [format_yaml_type(Bad)]);
format_error({bad_map, Bad}) ->
    format("Expected map, got ~s instead", [format_yaml_type(Bad)]);
format_error({bad_module, Mod}) ->
    format("Unknown module: ~s", [Mod]);
format_error({bad_non_neg_int, Bad}) ->
    format("Expected non negative integer, got: ~B", [Bad]);
format_error({bad_non_neg_int, Inf, Bad}) ->
    format("Expected non negative integer or '~s', got: ~B", [Inf, Bad]);
format_error({bad_number, Bad}) ->
    format("Expected number, got ~s instead", [format_yaml_type(Bad)]);
format_error({bad_number, Min, Bad}) ->
    format("Expected number >= ~p, got: ~p", [Min, Bad]);
format_error({bad_octal, Bad}) ->
    format("Expected octal, got: ~s", [Bad]);
format_error({bad_pos_int, Bad}) ->
    format("Expected positive integer, got: ~B", [Bad]);
format_error({bad_pos_int, Inf, Bad}) ->
    format("Expected positive integer or '~s', got: ~B", [Inf, Bad]);
format_error({bad_regexp, {Reason, _}, _}) ->
    format("Invalid regular expression: ~s", [Reason]);
format_error({bad_timeout, Bad}) ->
    format_error({bad_pos_int, Bad});
format_error({bad_timeout, Inf, Bad}) ->
    format_error({bad_pos_int, Inf, Bad});
format_error({bad_url, empty_host, URL}) ->
    format("Empty hostname in the URL: ~s", [URL]);
format_error({bad_url, {unsupported_scheme, Scheme}, URL}) ->
    format("Unsupported scheme '~s' in the URL: ~s", [Scheme, URL]);
format_error({bad_url, {no_default_port, _, _}, URL}) ->
    format("Missing port in the URL: ~s", [URL]);
format_error({bad_url, _, URL}) ->
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
format_error({disallowed_option, Opt}) ->
    format("Option '~s' is not allowed in this context", [Opt]);
format_error({duplicated_option, Opt}) ->
    format("Duplicated option: ~s", [Opt]);
format_error(empty_atom) ->
    format("Empty string is not allowed", []);
format_error(empty_binary) ->
    format("Empty string is not allowed", []);
format_error(empty_list) ->
    format("Empty list is not allowed", []);
format_error({missing_option, Opt}) ->
    format("Missing required option: ~s", [Opt]);
format_error({nomatch, Regexp, Bin}) ->
    format("String '~s' doesn't match regular expression: ~s",
	   [Bin, Regexp]);
format_error({read_dir, Why, Path}) ->
    format("Failed to read directory '~s': ~s",
	   [Path, file:format_error(Why)]);
format_error({read_file, Why, Path}) ->
    format("Failed to read file '~s': ~s",
	   [Path, file:format_error(Why)]);
format_error({unknown_option, _, Opt}) ->
    format("Unknown option: ~s", [Opt]);
format_error(Unexpected) ->
    format("Unexpected error reason: ~p", [Unexpected]).

-spec format_ctx(ctx()) -> string().
format_ctx([]) ->
    "Configuration error";
format_ctx([_|_] = Ctx) ->
    format("Invalid value of option ~s",
	   [string:join(
	      lists:map(
		fun(A) when is_atom(A) ->
			atom_to_list(A);
		   (B) when is_binary(B) ->
			"\"" ++ binary_to_list(B) ++ "\"";
		   (I) when is_integer(I) ->
			integer_to_list(I);
		   (Unexpected) ->
			lists:flatten(io_lib:format("~p", [Unexpected]))
		end, Ctx),
	      "->")]).

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
    "list";
format_yaml_type(Unexpected) ->
    lists:flatten(io_lib:format("~p", [Unexpected])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================
%%% Initial YAML parsing
%%%===================================================================
-spec read_yaml(binary(), [parse_option()], [binary()]) -> yaml_map().
read_yaml(Path, Opts, Paths) ->
    case lists:member(Path, Paths) of
	true ->
	    fail({bad_yaml, circular_include, Path});
	false ->
	    PlainAsAtom = proplists:get_bool(plain_as_atom, Opts),
	    ReplaceMacros = proplists:get_bool(replace_macros, Opts),
	    IncludeFiles = proplists:get_bool(include_files, Opts),
	    case fast_yaml:decode_from_file(
		   Path, [{plain_as_atom, PlainAsAtom}]) of
		{ok, [Y]} ->
		    Validators = validators(Opts),
		    V = and_then(
			  options(Validators#{'_' => any()}, Opts),
			  fun(Y1) ->
				  {Macros, Y2} = partition_macros(Y1, ReplaceMacros),
				  Y3 = include_files(IncludeFiles, Y2, Opts, [Path|Paths]),
				  replace_macros(Y3, Macros)
			  end),
		    V(Y);
		{ok, []} ->
		    [];
		{error, Why} ->
		    fail({bad_yaml, Why, Path})
	    end
    end.

-spec partition_macros(options(), boolean()) -> {[macro()], yaml_map()}.
partition_macros(Opts, true) ->
    lists:foldr(
      fun({define_macro, M}, {Ms, Os}) ->
	      {M ++ Ms, Os};
	 (O, {Ms, Os}) ->
	      {Ms, [O|Os]}
      end, {[], []}, Opts);
partition_macros(Opts, false) ->
    {[], Opts}.

-spec replace_macros(yaml_map(), [macro()]) -> yaml_map().
replace_macros(Y1, Macros) ->
    lists:foldl(
      fun(Macro, Y2) ->
	      replace_macro(Y2, Macro)
      end, Y1, lists:flatten(Macros)).

-spec replace_macro(yaml(), macro()) -> yaml().
replace_macro(L, Macro) when is_list(L) ->
    [replace_macro(E, Macro) || E <- L];
replace_macro({K, V}, Macro) ->
    {K, replace_macro(V, Macro)};
replace_macro(Name, {Name, Val}) ->
    Val;
replace_macro(A, {Name, Val}) when is_atom(A) ->
    case atom_to_binary(A, utf8) of
	Name -> Val;
	_ -> A
    end;
replace_macro(Val, _) ->
    Val.

-spec include_files(boolean(), options(), [parse_option()], [binary()]) -> yaml_map().
include_files(true, List, Opts, Paths) ->
    lists:flatmap(
      fun({include_config_file, Includes}) ->
	      include_files(Includes, Opts, Paths);
	 (Y) ->
	      [Y]
      end, List);
include_files(false, List, _, _) ->
    List.

-spec include_files(includes(), [parse_option()], [binary()]) -> yaml_map().
include_files(Includes, Opts, Paths) ->
    lists:flatmap(
      fun({File, {Disallow, AllowOnly}}) ->
	      Y = read_yaml(File, Opts, Paths),
	      lists:filter(
		fun({Opt, _}) ->
			case AllowOnly of
			    [] ->
				not lists:member(Opt, Disallow);
			    _ ->
				lists:member(Opt, AllowOnly)
			end
		end, Y);
	 (File) ->
	      read_yaml(File, Opts, Paths)
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
    try binary_to_atom(B, utf8)
    catch _:system_limit -> fail({bad_length, 255})
    end;
to_atom(A) when is_atom(A) ->
    A;
to_atom(Bad) ->
    fail({bad_atom, Bad}).

-spec to_string(term()) -> string().
to_string(S) ->
    binary_to_list(to_binary(S)).

-spec to_int(term()) -> integer().
to_int(I) when is_integer(I) ->
    I;
to_int(Bad) ->
    fail({bad_int, Bad}).

-spec to_int(term(), infinity()) -> integer() | infinity().
to_int(I, _) when is_integer(I) -> I;
to_int(infinity, Inf) -> Inf;
to_int(infinite, Inf) -> Inf;
to_int(unlimited, Inf) -> Inf;
to_int(B, Inf) when is_binary(B) ->
    try binary_to_existing_atom(B, latin1) of
	A -> to_int(A, Inf)
    catch _:_ ->
	    fail({bad_int, B})
    end;
to_int(Bad, _) ->
    fail({bad_int, Bad}).

-spec to_number(term()) -> number().
to_number(N) when is_number(N) ->
    N;
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
		    fail({bad_cwd, Reason})
	    end;
	_ ->
	    Path1
    end.

-spec validate_options(list(), validators(), validator() | undefined,
		       [atom()], [atom()], boolean()) -> options().
validate_options(Opts, Validators, DefaultValidator,
		 Required, Disallowed, CheckDups) ->
    validate_options(Opts, Validators, DefaultValidator,
		     Required, Disallowed, CheckDups, []).

-spec validate_options(list(), validators(), validator() | undefined,
		       [atom()], [atom()], boolean(), options()) -> options().
validate_options([{O, Val}|Opts], Validators, DefaultValidator,
		 Required, Disallowed, CheckDups, Acc) ->
    Opt = to_atom(O),
    case lists:member(Opt, Disallowed) of
	true -> fail({disallowed_option, Opt});
	false ->
	    case maps:get(Opt, Validators, DefaultValidator) of
		undefined ->
		    Allowed = maps:keys(Validators) -- Disallowed,
		    fail({unknown_option, Allowed, Opt});
		Validator ->
		    case CheckDups andalso lists:keymember(Opt, 1, Acc) of
			true -> fail({duplicated_option, Opt});
			false ->
			    Required1 = lists:delete(Opt, Required),
			    Acc1 = [{Opt, validate_option(Opt, Val, Validator)}|Acc],
			    validate_options(Opts, Validators, DefaultValidator,
					     Required1, Disallowed, CheckDups, Acc1)
		    end
	    end
    end;
validate_options([], _, _, [], _, _, Acc) ->
    lists:reverse(Acc);
validate_options([], _, _, [Required|_], _,  _, _) ->
    fail({missing_option, Required});
validate_options(Bad, _, _, _, _, _, _) ->
    fail({bad_map, Bad}).

-spec validate_option(atom(), yaml(), validator(T)) -> T.
validate_option(Opt, Val, Validator) ->
    Ctx = get_ctx(),
    put_ctx([Opt|Ctx]),
    Ret = Validator(Val),
    put_ctx(Ctx),
    Ret.

%%%===================================================================
%%% Mutable context processing
%%%===================================================================
-spec get_ctx() -> ctx().
get_ctx() ->
    case get(yconf_ctx) of
	undefined -> [];
	Opts -> Opts
    end.

-spec put_ctx(ctx()) -> ctx().
put_ctx(Opts) ->
    put(yconf_ctx, Opts).

-spec erase_ctx() -> ctx().
erase_ctx() ->
    case erase(yconf_ctx) of
	Opts when is_list(Opts) -> lists:reverse(Opts);
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
      fun(replace_macros, V) ->
	      maps:put(define_macro, validator(define_macro), V);
	 (include_files, V) ->
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
