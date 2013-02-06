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
    device_type/1,
    has_pointer_device/1,
    
    test/0
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
    case catch erlang:load_nif(SoName, Dtree) of
        ok -> ok;
        LoadError -> error_logger:error_msg("ua_classifier: error loading NIF (~p): ~p", 
                                            [SoName, LoadError])  
    end,
    case erlang:system_info(otp_release) of
        "R13B03" -> true;
        _ -> ok
    end.

%% @doc Check a user-agent string against the OpenDDR classifications.
-spec classify( UserAgentString :: iolist() ) -> {ok, Properties :: list()} | {error, Reason :: term() }.
classify(_UserAgent) ->
    {error, ua_classifier_nif_not_loaded}.


%% @doc Map a list of user-agent properties (as returned by classify/1) to a simple device type atom.
-spec device_type( Properties :: list() ) -> device_type().
device_type([]) ->
    text;
device_type(Properties) when is_list(Properties) ->
    case is_desktop(Properties) of
        true -> 
            desktop;
        false ->
            case proplists:get_value(id, Properties) of
                <<"genericTouchPhone">> -> phone;
                <<"textDevice">> -> text;
                _ -> check_tablet(Properties)
            end
    end.

    check_tablet(Ps) ->
        case proplists:get_value(is_tablet, Ps, false) of
            true -> tablet;
            false -> check_phone(Ps)
        end.

    % A device is a (smart)phone when it can do ajax and has a touch screen (or stylus)
    check_phone(Ps) ->
        case proplists:get_value(ajax_support_javascript, Ps, false) of
            true -> 
                case has_pointer_device(Ps) of
                    true -> phone;
                    false -> text
                end;
            false -> text
        end.

%% @doc Check if the classified device has a pointer
-spec has_pointer_device(list()) -> boolean().
has_pointer_device(Ps) ->
    case is_desktop(Ps) of
        true ->
            true;
        false ->
            case proplists:get_value(inputDevices, Ps) of
               Ds when is_list(Ds) -> 
                           lists:member(<<"touchscreen">>, Ds) 
                    orelse lists:member(<<"stylus">>, Ds)
                    orelse lists:member(<<"clickwheel">>, Ds);
               <<"touchscreen">> -> true;
               <<"stylus">> -> true;
               <<"clickwheel">> -> true;
               _ -> false
           end
    end.

%% @doc Check if the classified device is a desktop
is_desktop(Ps) ->
    case proplists:get_value(parentId, Ps) of
        <<"desktopDevice">> -> true;
        _ ->
            case proplists:get_value(id, Ps) of
                <<"desktopDevice">> -> true;
                <<"desktopCrawler">> -> true;
                <<"unknown">> -> true;
                _ -> false
            end
    end.



%% @doc Test some known user-agent strings and the values they must return.
-spec test() -> ok.
test() ->
    UAs = [
        % Various Google bots
        {desktop, "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"},
        {desktop, "Googlebot/2.1 (+http://www.google.com/bot.html)"},
        {text, "SAMSUNG-SGH-E250/1.0 Profile/MIDP-2.0 Configuration/CLDC-1.1 UP.Browser/6.2.3.3.c.1.101 (GUI) MMP/2.0 (compatible; Googlebot-Mobile/2.1; +http://www.google.com/bot.html)"},
        {phone, "Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_1 like Mac OS X; en-us) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8B117 Safari/6531.22.7 (compatible; Googlebot-Mobile/2.1; +http://www.google.com/bot.html)"},
        
        % Text devices
        {text, "Lynx/2.8.8dev.3 libwww-FM/2.14 SSL-MM/1.4.1"},
        
        % Feature phone (some like to be smart)
        {text, "Mozilla/4.0 (compatible; MSIE 5.0; Series60/2.8 Nokia6630/4.06.0 Profile/MIDP-2.0 Configuration/CLDC-1.1)"},
        
        % Smart phones
        {phone, "Mozilla/4.0 (compatible; MSIE 6.0; Windows CE; IEMobile 7.11) SonyEricssonX1a/R3AA Profile/MIDP-2.0 Configuration/CLDC-1.1 UNTRUSTED/1.0"},
        {phone, "Mozilla/5.0 (iPhone; CPU iPhone OS 6_0_1 like Mac OS X) AppleWebKit/536.26 (KHTML, like Gecko) Version/6.0 Mobile/10A523 Safari/8536.25"},
        
        % Command line tools
        {desktop, "curl/7.8 (i386-redhat-linux-gnu) libcurl 7.8 (OpenSSL 0.9.6b) (ipv6 enabled)"},
        
        % Tablets
        {tablet, "Mozilla/5.0 (iPad; U; CPU OS 3_2 like Mac OS X; en-us) AppleWebKit/531.21.10 (KHTML, like Gecko) Version/4.0.4 Mobile/7B334b Safari/531.21.10"},
        {tablet, "Mozilla/5.0 (Linux; U; Android 2.2; en-us; SCH-I800 Build/FROYO) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.1"},
        {tablet, "Mozilla/5.0 (Linux; U; Android 2.2; es-es; GT-P1000 Build/FROYO) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.1"}
    ],
    [ begin
        {ok, Ps} = classify(UA),
        {Type, UA} = {device_type(Ps), UA}
      end
      || {Type, UA} <- UAs ],
    ok.
