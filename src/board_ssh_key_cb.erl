%% @doc A very simple ssh callback which handles keys.
%%
%% This module has no concerns about 'know_hosts' file. Or any checks about host
%% fingerprints.
%%
%% @reference <a href="https://github.com/labzero/ssh_client_key_api/blob/master/lib/ssh_client_key_api.ex">ssh_client_key_api.ex</a> and
%%            <a href="https://github.com/erlang/otp/blob/master/lib/ssh/src/ssh_file.erl">ssh_file.erl</a>
 
-module(board_ssh_key_cb).

-behaviour(ssh_client_key_api).

-export([add_host_key/3, is_host_key/4, user_key/2]).

add_host_key(_, _, _) ->
    ok.

is_host_key(_, _, _, _) ->
    true.

user_key(Algorithm, ConnectOptions) ->
    File = file_name(user, identity_key_filename(Algorithm), ConnectOptions),
    Password = proplists:get_value(identity_pass_phrase(Algorithm), ConnectOptions, ignore),
    case decode(File, Password) of
        {ok, Key} ->
            check_key_type(Key, Algorithm);
        Error ->
            Error
    end.

check_key_type(Key, Algorithm) ->
    case ssh_transport:valid_key_sha_alg(Key,Algorithm) of
        true -> {ok,Key};
        false -> {error,bad_keytype_in_file}
    end.

decode(File, Password) ->
    try {ok, decode_ssh_file(read_ssh_file(File), Password)}
    catch 
	throw:Reason ->
	    {error, Reason};
	error:Reason ->
	    {error, Reason}
    end.      

read_ssh_file(File) ->
    {ok, Bin} = file:read_file(File),
    Bin.

%% Public key
decode_ssh_file(SshBin, public_key) ->
    public_key:ssh_decode(SshBin, public_key);

%% Private Key
decode_ssh_file(Pem, Password) ->
    case public_key:pem_decode(Pem) of
	[{_, _, not_encrypted} = Entry]  -> 
	    public_key:pem_entry_decode(Entry);
	[Entry] when Password =/= ignore ->
	    public_key:pem_entry_decode(Entry, Password);
	 _ ->
	    throw("No pass phrase provided for private key file")
    end.

identity_key_filename('ssh-dss'            ) -> "id_dsa";
identity_key_filename('ssh-rsa'            ) -> "id_rsa";
identity_key_filename('rsa-sha2-256'       ) -> "id_rsa";
identity_key_filename('rsa-sha2-384'       ) -> "id_rsa";
identity_key_filename('rsa-sha2-512'       ) -> "id_rsa".

identity_pass_phrase("ssh-rsa"       ) -> rsa_pass_phrase;
identity_pass_phrase("rsa-sha2-256"  ) -> rsa_pass_phrase;
identity_pass_phrase("rsa-sha2-384"  ) -> rsa_pass_phrase;
identity_pass_phrase("rsa-sha2-512"  ) -> rsa_pass_phrase;
identity_pass_phrase(P) when is_atom(P) ->
    identity_pass_phrase(atom_to_list(P));
identity_pass_phrase(_) -> undefined.

ssh_dir(user, Opts) ->
    case proplists:get_value(user_dir, Opts, false) of
	false -> throw("No user_dir provided for ssh files");
	D -> D
    end.

file_name(Type, Name, Opts) ->
    FN = filename:join(ssh_dir(Type, Opts), Name),
    FN.