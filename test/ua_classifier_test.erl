-module(ua_classifier_test).

-include_lib("eunit/include/eunit.hrl").

run_embedded_ua_classifier_test() ->
    ua_classifier:test().
