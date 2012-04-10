User-Agent classification using TheWeatherChannel's dClass and OpenDDR
======================================================================

An implementation of the user agent classification library [dClass by TheWeatherChannel](https://github.com/TheWeatherChannel/dClass) using an Erlang NIF.


The main interface is `ua_classifier:classify/1`.  It returns a property list with user agent properties from OpenDDR.

```erlang
{ok, Ps} = ua_classifier:classify("Mozilla/5.0 (iPod; U; CPU iPhone OS 4_0 like Mac OS X; en-us) AppleWebKit/532.9 (KHTML, like Gecko) Version/4.0.5 Mobile/8A293 Safari/6531.22.7").
```

Returns:

```erlang
{ok,[{is_wireless_device,true},
     {is_tablet,false},
     {ajax_support_javascript,true},
     {device_os,<<"iPhone OS">>},
     {displayWidth,320},
     {displayHeight,480},
     {inputDevices,<<"touchscreen">>},
     {parentId,<<"genericApple">>},
     {model,<<"iPod Touch">>},
     {vendor,<<"Apple">>},
     {id, <<"iPod">>]}
```

This property list can be mapped to a device type with `ua_classifier:device_type/1`.
Known device types are: `text`, `phone`, `tablet` and `desktop`.

```erlang
ua_classifier:device_type(Ps).
```

Returns:

```erlang
phone
```


The OpenDDR data file
---------------------

This library uses the ".dtree" file of dClass. It is placed in `priv/openddr.dtree`.
The dtree file is loaded when the NIF is initialized and when the NIF is upgraded.


