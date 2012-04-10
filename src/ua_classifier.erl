%% Copyright 2012 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%  http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

%% @doc A NIF implementation of WeatherChannel's dClass user-agent classifier.

-module(ua_classifier).

-export([
    init/0,
    classify/1,
    device_type/1
]).
-export_type([
    device_type/0
]).

-on_load(init/0).

-type device_type() :: text | phone | table | desktop.

-spec init() -> true | ok.
init() ->
    SoName = case code:priv_dir(?MODULE) of
                {error, bad_name} ->
                    case filelib:is_dir(filename:join(["..", "priv"])) of
                        true ->
                            filename:join(["..", "priv", "ua_classifier_nif"]);
                        false ->
                            filename:join(["priv", "ua_classifier_nif"])
                    end;
                Dir ->
                    filename:join(Dir, "ua_classifier_nif")
            end,
    Dtree = list_to_binary(filename:join(filename:dirname(SoName), "openddr.dtree")),
    catch erlang:load_nif(SoName, Dtree),
    case erlang:system_info(otp_release) of
        "R13B03" -> true;
        _ -> ok
    end.

%% @doc Check a user-agent string against the OpenDDR classifications.
-spec classify( UserAgentString :: iolist() ) -> {ok, Properties :: list()} | {error, Reason :: term() }.
classify(_UserAgent) ->
    exit(ua_classifier_nif_not_loaded).


%% @doc Map a list of user-agent properties (as returned by classify/1) to a simple device type atom.
-spec device_type( Properties :: list() ) -> device_type().
device_type([]) ->
    text;
device_type(Properties) when is_list(Properties) ->
    case proplists:get_value(id, Properties) of
        <<"desktopDevice">> -> desktop;
        <<"unknown">> -> desktop;
        _ -> check_tablet(Properties)
    end.

    check_tablet(Ps) ->
        case proplists:get_value(is_tablet, Ps, false) of
            true -> tablet;
            false -> check_phone(Ps)
        end.

    % We call a phone when it can do ajax.
    check_phone(Ps) ->
        case proplists:get_value(ajax_support_javascript, Ps, false) of
            true -> phone;
            false -> text
        end.
