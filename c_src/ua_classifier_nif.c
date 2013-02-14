/**
 * Copyright 2012 Marc Worrell
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

#include "erl_nif_compat.h"
#include "dclass/dclass_client.h"

#include <ctype.h>
#include <string.h>

#ifdef OTP_R13B03
#error OTP R13B03 not supported. Upgrade to R13B04 or later.
#endif


typedef struct {
    char *filename;
    dclass_index di;
} ua_state;


static int is_number(const char *s)
{
    if (*s) {
        while (*s) {
            if (!isdigit(*s)) {
                return 0;
            }
            s++;
        }
        return 1;
    }
    return 0;
}


static ERL_NIF_TERM 
make_atom(ErlNifEnv *env, const char *name)
{
    ERL_NIF_TERM ret;

    if (enif_make_existing_atom_compat(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

static ERL_NIF_TERM 
make_error(ErlNifEnv *env, const char *mesg)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, mesg));
}

static ERL_NIF_TERM
make_binary_string(ErlNifEnv *env, const char *s)
{
    ErlNifBinary b;
    int n;
    
    n = strlen(s);
    enif_alloc_binary(n, &b);
    memcpy(b.data, s, n);
    b.size = n;
    return enif_make_binary(env, &b);
}


static ERL_NIF_TERM
make_value(ErlNifEnv *env, const char *key, const char *s)
{
    if (strcmp(s, "false") == 0) {
        return make_atom(env, "false");
    }
    if (strcmp(s, "true") == 0) {
        return make_atom(env, "true");
    }
    if (   is_number(s)
        && strcmp(key, "id")
        && strcmp(key, "model")) {
        return enif_make_int(env, atoi(s));
    }
    return make_binary_string(env, s);
}


static ERL_NIF_TERM 
ua_classify(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    ua_state *state;
    char *ua;
    const dclass_keyvalue *kvd;
    ERL_NIF_TERM hd;
    ERL_NIF_TERM tl;
    int i;
    
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }
    ua = (char *) enif_alloc(input.size+1);
    if (!ua) {
        return make_error(env, "out_of_memory");
    }
    memcpy(ua, input.data, input.size);
    ua[input.size] = '\0';

    tl = enif_make_list(env, 0);
    
    /* Handle some exceptions from the dClass library */
    if (   strncmp(ua, "Lynx", 4) == 0    /* Lynx text browser */
        || strstr(ua, "Series60/") != NULL /* Nokia phone */
        || strstr(ua, "Series50/") != NULL /* Nokia phone */
        )
    {
        hd = enif_make_tuple2(env,
                              make_atom(env, "id"),
                              make_value(env, "id", "textDevice"));
        tl = enif_make_list_cell(env, hd, tl);
    } else {
        /* Find the classification in the dclass tree */
        state = (ua_state *) enif_priv_data_compat(env);
        kvd = dclass_classify(&state->di, ua);

        /* Make a list of all key/value pairs */
        if (kvd) {
            hd = enif_make_tuple2(env,
                                  make_atom(env, "id"),
                                  make_value(env, "id", kvd->id));
            tl = enif_make_list_cell(env, hd, tl);
            if (kvd->size) {
                for (i = 0; i < kvd->size; i++) {
                    hd = enif_make_tuple2(env,
                                          make_atom(env, kvd->keys[i]),
                                          make_value(env, kvd->keys[i], kvd->values[i]));
                    tl = enif_make_list_cell(env, hd, tl);
                }
            }
        }
    }
    enif_free(ua);
    return enif_make_tuple2(env, make_atom(env, "ok"), tl);
}


static int
on_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info)
{
    int ret;
    ua_state *state;
    ErlNifBinary dtree;
    
    if(!enif_inspect_binary(env, info, &dtree)) {
        return enif_make_badarg(env);
    }
    
    /* Allocate the dclass tree datastructure, this will be kept between NIF calls */
    state = (ua_state *) enif_alloc(sizeof(ua_state));
    if (!state) {
        return enif_make_badarg(env); 
    }
    state->filename = enif_alloc(dtree.size + 1);
    if (state->filename == NULL) {
        enif_free(state);
        return enif_make_badarg(env); 
    }
    memcpy(state->filename, dtree.data, dtree.size);
    state->filename[dtree.size] = '\0';
    
    /* Parse the dtree file, bail out on an error */
    dclass_init_index(&state->di);
    ret = dclass_load_file(&state->di, state->filename);
    if (ret < 0) {
        enif_free(state->filename);
        enif_free(state);
        return enif_make_badarg(env); 
    }
    /* Keep the dtree as priv data */
    *priv = (void *) state;
    return 0;
}


static int
on_upgrade(ErlNifEnv *env, void **new_priv, void **old_priv, ERL_NIF_TERM info)
{
    return on_load(env, new_priv, info);
}


static void
on_unload(ErlNifEnv *env, void *priv)
{
    ua_state *state;
    
    state = (ua_state *) priv;
    dclass_free(&state->di);
    enif_free(state->filename);
    enif_free(state);
}


static ErlNifFunc nif_functions[] = {
    {"classify", 1, ua_classify}
};

ERL_NIF_INIT(ua_classifier, nif_functions, &on_load, NULL, &on_upgrade, &on_unload);



