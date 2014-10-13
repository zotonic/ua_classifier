-module(ua_classifier_test).

-include_lib("eunit/include/eunit.hrl").

run_embedded_ua_classifier_test() ->
    ua_classifier:test().

browser_classify_test() ->
    %% Junk input
    {ok, [{id, <<"unknown">>}]} = ua_classifier:browser_classify(""),
    {ok, [{id, <<"unknown">>}]} = ua_classifier:browser_classify(<<"junk">>),
    {ok, [{id, <<"unknown">>}]} = ua_classifier:browser_classify(<<"junk">>),

    %% Bots 
    {ok, [{id, <<"unknown">>}]} = ua_classifier:browser_classify(<<"Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)">>),

    %% Working UA's
    UAs = [
        %% FF Strings
        {{<<"Windows">>, <<"Firefox">>, 30000, <<"firefox3xw">>}, <<"Mozilla/5.0 (Windows NT 5.1; rv:31.0) Gecko/20100101 Firefox/31.0">>},
        {{<<"Linux">>, <<"Firefox">>, 20000, <<"firefox2xl">>}, <<"Mozilla/5.0 (X11; Linux x86_64; rv:28.0) Gecko/20100101 Firefox/28.0">>},

        %% IE strings
        {{<<"Windows">>, <<"Internet Explorer">>, 7000, <<"msie7">>}, <<"Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; )">>},
        {{<<"Windows">>, <<"Internet Explorer">>, 7000, <<"msie7">>}, <<"Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.3; Trident/7.0; .NET4.0E; .NET4.0C)">>},
        {{<<"Windows">>, <<"Internet Explorer">>, 10000, <<"msie10">>}, "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; Trident/6.0)"},
        {{<<"Windows">>, <<"Internet Explorer">>, 11000, <<"msiet11">>}, <<"Mozilla/5.0 (Windows NT 6.3; Trident/7.0; rv:11.0) like Gecko">>},
        {{<<"Windows">>, <<"Internet Explorer">>, 11000, <<"msiet11">>}, <<"Mozilla/5.0 (Windows NT 6.3; Trident/7.0; .NET4.0E; .NET4.0C; rv:11.0) like Gecko">>},

        %% Safari Strings
        {{<<"">>, <<"WebKit">>, 0, <<"safari">>}, 
            <<"Mozilla/5.0 (Windows; U; Windows NT 5.1; cs-CZ) AppleWebKit/523.15 (KHTML, like Gecko) Version/3.0 Safari/523.15">>},
        {{<<"Macintosh">>, <<"Safari">>, 6000, <<"safari6m">>}, 
            <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_5) AppleWebKit/537.75.14 (KHTML, like Gecko) Version/6.1.3 Safari/537.75.14">>},

        %% iPhone
        {{<<"iOS">>, <<"Safari">>, 4050, <<"iossafari405">>}, 
            "Mozilla/5.0 (iPod; U; CPU iPhone OS 4_0 like Mac OS X; en-us) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A293 Safari/6531.22.7"}
    ],

    [begin
        {ok, Props} = ua_classifier:browser_classify(UA),
        Os = proplists:get_value(os, Props),
        Browser = proplists:get_value(browser, Props),
        Version = proplists:get_value(version, Props),
        Id = proplists:get_value(id, Props)
     end || {{Os, Browser, Version, Id}, UA}  <- UAs ],

    ok.
