%% @doc
%% First layer is a top packet of the format:
%% Length | Content
%% 1      | packet kind
%% 0,..   | payload
%% All other packet types are wrapped inside this format:
%% Value  | Packet Kind
%% 16#05  | 'dht close request' 
%% 16#a5  | bootstrap info  :TODO:
%% 
%% 'Dht close request' packet is of the format: 
%% Length | Content
%% 32     | sender's public key
%% 24     | nonce
%% 16,..  | encrypted payload
%% ------ 16 is a message authentication code (tag) + any services (top service).
%% Top service format (that will be added to encrypted payload above):
%% Length | Content
%% 0,..   | payload
%% 8      | request id (in response packet it should be the same as in request)
%% Payload may be one of the following:
%% -----ping service-----
%% Length | Content
%% 1      | 16#00 for request; 16#01 for response
%% -----nodes service----
%% -----------nodes request--------- (query another node for up to 4 nodes)
%% Length | Content
%% 1      | 16#02
%% 32     | public key (requested public key)
%% -----------nodes response-------- (
%% Length | Content
%% 1      | 16#03
%% 1      | number of nodes in the response (up to 4)
%% ..     | nodes in packed node format
%%
%%-----------------------------------------------------------
%% 
%%
%% @end


-module(mdht_proto).
-include("consts.hrl").

-export([decode/1, encode/2]).

-type decoded_result() :: 
	{ok, PK :: mdht:public_key(), {ping_request, RequestId :: binary()}} |
	{ok, PK :: mdht:public_key(), {ping_response, RequestId :: binary()}} |
	{ok, PK :: mdht:public_key(), {{nodes_request, PK :: mdht:public_key()}, RequestId :: binary()}} | 
	{ok, PK :: mdht:public_key(), {{nodes_response, PackedNodes :: list()}, RequestId :: binary()}} |
	{error, ErrorType :: atom()}.


%% @doc Decode given packet according to specification.
-spec decode(binary()) -> decoded_result().
decode(<<16#05:8, DhtPacket/binary>>) ->
    decode_dht_close(DhtPacket);
decode(_) ->
    {error, unknown_packet_type}.


decode_dht_close(<<SenderPublicKey:32/binary,
		    Nonce:24/binary,
		    EncryptedPayload/binary>>) ->
    %% make request to encryption_server to 
    Res = encryption_server:decrypt_message(EncryptedPayload, Nonce, SenderPublicKey),
    case Res of
	{error, Reason} ->
	    {error, Reason, dht_packed_payload_undecrypted};
	{ok, BinaryMsg} ->
	    case decode_dht_packet_service(BinaryMsg) of
		{error, _} = Err ->
		    Err;
		Response ->
		    {ok, SenderPublicKey, Response}
	    end
    end;
decode_dht_close(_) ->
    {error, wrong_dht_packet_format}.


decode_dht_packet_service(<<_Tag:16/binary, 16#00:8, ReqId:8/binary>>) -> 
    {ping_request, ReqId};
decode_dht_packet_service(<<_Tag:16/binary, 16#01:8, ReqId:8/binary>>) ->
    {ping_response, ReqId};
decode_dht_packet_service(<<_Tag:16/binary, 16#02:8, RequestedPK:32/binary, ReqId:8/binary>>) ->
    {{nodes_request, RequestedPK}, ReqId};
decode_dht_packet_service(<<_Tag:16/binary, 16#03:8, LeftBinary/binary>>) ->
    case decode_dht_packet_service_nodes_response(LeftBinary) of
	{error, _} = Error ->
	    Error;
	{_NumberOfNodes, PackedNodes, ReqId} ->
	    {{nodes_response, PackedNodes}, ReqId}
    end;
decode_dht_packet_service(_) ->
    {error, malformed_request_packet}.


%% firstly check for ipv6; 1 ipv6 node is 51 bytes, 1 ipv4 node is 39 bytes
decode_dht_packet_service_nodes_response(<<NNodes:8, ElseBin/binary>>) ->
    decode_nodes_response_parse(NNodes, ElseBin, [], firstround).

decode_nodes_response_parse(0, <<ReqId:8/binary>>, AccNodes, _) ->
    {length(AccNodes), lists:reverse(AccNodes), ReqId};
decode_nodes_response_parse(0, _, _, _) ->
    {error, malformed_nodes};
decode_nodes_response_parse(N, <<Ipv6Node:51/binary, Rest/binary>> = Bin, AccNodes, firstround) ->
    case packed_node:decode(Ipv6Node) of
	none ->
	    %% try to decode with ipv4
	    decode_nodes_response_parse(N, Bin, AccNodes, secondround);
	Decoded ->
	    decode_nodes_response_parse(N-1, Rest, [Decoded | AccNodes], firstround)
    end;
%% we can get here only if there is 1 ipv4node and ReqId are left
decode_nodes_response_parse(1, <<Ipv4Node:39/binary, ReqId:8/binary>>, AccNodes, firstround) ->
    case packed_node:decode(Ipv4Node) of
	none ->
	    {error, malformed_nodes};
	Decoded ->
	    {length(AccNodes)+1, lists:reverse([Decoded|AccNodes]), ReqId}
    end;
decode_nodes_response_parse(N, <<Ipv4Node:39/binary, Rest/binary>>, AccNodes, secondround) ->
    case packed_node:decode(Ipv4Node) of
	none ->
	   {error, malformed_nodes};
	Decoded ->
	    decode_nodes_response_parse(N-1, Rest, [Decoded|AccNodes], firstround)
    end;
decode_nodes_response_parse(_, _, _, _) ->
    {error, malformed_nodes}.


%% @doc Encode given tuple to appropriate binary form.
encode({Request, RequestId}, <<ReceiverPK:32/binary>>) ->
    case encode_request(Request, RequestId, ReceiverPK) of
	{ok, Binary} ->
	    Binary;
	{error, _Reason} = Err ->
	    Err
    end;	    
encode(_, _) ->
    {error, unknown_encode_format}.

encode_request(ping_request, RequestId, <<ReceiverPK:32/binary>>) ->
    {OwnPK, Nonce, Tag} = receive_pk_nonce_tag(),
    MsgToEncrypt = <<Tag/binary, 16#00:8, RequestId/binary>>,
    case encryption_server:encrypt_message(MsgToEncrypt, Nonce, ReceiverPK) of
	{ok, EncryptedMsg} ->
	    {ok,<<16#05:8, OwnPK/binary, Nonce/binary, EncryptedMsg/binary>>};
	_ ->
	    {error, wrong_encryption}
    end;
encode_request(ping_response, RequestId, ReceiverPK) ->
    {OwnPK, Nonce, Tag} = receive_pk_nonce_tag(),
    MsgToEncrypt = <<Tag/binary, 16#01:8, RequestId/binary>>,
    case encryption_server:encrypt_message(MsgToEncrypt, Nonce, ReceiverPK) of
	{ok, EncryptedMsg} ->
	    {ok,<<16#05:8, OwnPK/binary, Nonce/binary, EncryptedMsg/binary>>};
	_ ->
	    {error, wrong_encryption}
    end;
encode_request({nodes_request, <<PublicKey:32/binary>>}, RequestId, ReceiverPK) ->
    {OwnPK, Nonce, Tag} = receive_pk_nonce_tag(),
    MsgToEncrypt = <<Tag/binary, 16#02:8, PublicKey:32/binary, RequestId/binary>>,
    case encryption_server:encrypt_message(MsgToEncrypt, Nonce, ReceiverPK) of
	{ok, EncryptedMsg} ->
	    {ok,<<16#05:8, OwnPK/binary, Nonce/binary, EncryptedMsg/binary>>};
	_ ->
	    {error, wrong_encryption}
    end;
%% packednodes in list format
encode_request({nodes_response, PackedNodes}, RequestId, ReceiverPK) ->
    {OwnPK, Nonce, Tag} = receive_pk_nonce_tag(), 
    NN = <<(length(PackedNodes)):8>>,
    EncodedPackedNodes = lists:foldr(fun(Node, Bin) ->
					     EncN = packed_node:encode(Node),
					     <<EncN/binary, Bin/binary>>
				     end, <<>>, PackedNodes),
    MsgToEncrypt = <<Tag/binary, 16#03:8, NN/binary, EncodedPackedNodes/binary, RequestId/binary>>, 
    case encryption_server:encrypt_message(MsgToEncrypt, Nonce, ReceiverPK) of
	{ok, EncryptedMsg} ->
	    {ok,<<16#05:8, OwnPK/binary, Nonce/binary, EncryptedMsg/binary>>};
	_ ->
	  {error, wrong_encryption}
    end;
encode_request( _, _, _) ->
    {error, unknown_request}.

receive_pk_nonce_tag() ->
    {ok, OwnPK} = encryption_server:get_pk(),
    {ok, Nonce} = encryption_server:get_nonce(),
    {ok, Tag} = encryption_server:get_tag(),
    {OwnPK, Nonce, Tag}.
    

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

decode_wrong_packed_type_test() ->
    WrongBinary = <<16#ff:8, 1:8, 2:8, 3:8>>,
    ?assertEqual({error, unknown_packet_type}, mdht_proto:decode(WrongBinary)).

decode_dht_packet_test() ->
    WrongBinary = <<16#05:8, 1:8, 2:8, 3:8>>,
    ?assertEqual({error, wrong_dht_packet_format}, mdht_proto:decode(WrongBinary)),
    {SomePK,_} = mdht_encryption:generate_crypto_pair(),
    Nonce = mdht_encryption:generate_nonce(),
    OkBinary = <<16#05:8, SomePK/binary, Nonce/binary, 1:8, 2:8>>,
    meck:new(encryption_server, [unstick, passthrough]),
    meck:expect(encryption_server, decrypt_message, fun(Encrypted, _, _) -> {ok,Encrypted} end),
    ?assertNotEqual({error, wrong_dht_packet_format}, mdht_proto:decode(OkBinary)),
    meck:unload(encryption_server).

decode_dht_packet_service_test() ->
    {SomePK,_} = mdht_encryption:generate_crypto_pair(),
    Nonce = mdht_encryption:generate_nonce(),
    WrongBinary = <<16#05:8, SomePK/binary, Nonce/binary, 25:8, 2:8>>,
    meck:new(encryption_server, [unstick, passthrough]),
    meck:expect(encryption_server, decrypt_message, fun(Encrypted, _, _) -> {ok,Encrypted} end),
    ?assertEqual({error, malformed_request_packet}, mdht_proto:decode(WrongBinary)),
    OKBinary1T = <<16#05:8, SomePK/binary, Nonce/binary>>, 
    SomeTag = binary:copy(<<0:8>>, ?TAGBYTES),
    PingCode = <<16#00:8>>,
    ReqId = binary:copy(<<0:8>>, ?REQUESTBYTES),
    OKBinary1 = <<OKBinary1T/binary, SomeTag/binary, PingCode/binary, ReqId/binary>>,
    %% correct ping request
    ?assertEqual({ok, SomePK, {ping_request, ReqId}}, mdht_proto:decode(OKBinary1)),
    GetNodes = <<16#02:8>>,
    OKBinary2 = <<OKBinary1T/binary, SomeTag/binary, GetNodes/binary, SomePK/binary, ReqId/binary>>,
    %% correct get_nodes request
    ?assertEqual({ok, SomePK, {{nodes_request, SomePK}, ReqId}}, mdht_proto:decode(OKBinary2)),
    meck:unload(encryption_server).

decode_nodes_response_test() ->
    {SomePK,_} = mdht_encryption:generate_crypto_pair(),
    Nonce = mdht_encryption:generate_nonce(),
    meck:new(encryption_server, [unstick, passthrough]),
    meck:expect(encryption_server, decrypt_message, fun(Encrypted, _, _) -> {ok,Encrypted} end),
    OKBinary1T = <<16#05:8, SomePK/binary, Nonce/binary>>,
    SomeTag = binary:copy(<<0:8>>, ?TAGBYTES),
    ResponseCode = <<16#03:8>>,
    %% 3 ipv4 nodes
    NN1 = <<3:8>>,
    Node1S = packed_node:create_node_packed_ipv4(12345, <<1:8, 2:8, 3:8, 4:8>>,
			       binary:copy(<<1:8>>, ?PUBLICKEYBYTES)),
    Node1 = packed_node:encode(Node1S),
    Node2S = packed_node:create_node_packed_ipv4(12346, <<1:8, 2:8, 3:8, 4:8>>,
			       binary:copy(<<2:8>>, ?PUBLICKEYBYTES)),
    Node2 = packed_node:encode(Node2S),
    Node3S = packed_node:create_node_packed_ipv4(12347, <<1:8, 2:8, 3:8, 4:8>>,
			       binary:copy(<<3:8>>, ?PUBLICKEYBYTES)),
    Node3 = packed_node:encode(Node3S),
    NodesReq1 = <<NN1/binary, Node1/binary, Node2/binary, Node3/binary>>,
    ReqId1 = binary:copy(<<0:8>>, ?REQUESTBYTES),
    RightPart1 = <<NodesReq1/binary, ReqId1/binary>>,
    CompleteResponse1 = <<OKBinary1T/binary, SomeTag/binary, ResponseCode/binary, RightPart1/binary>>,
    ?assertEqual({ok, SomePK, {{nodes_response, [Node1S, Node2S, Node3S]}, ReqId1}}, 
		 mdht_proto:decode(CompleteResponse1)),
    %% 4 ipv6 nodes
    NN2 = <<4:8>>,
    IpV6Addr = binary:copy(<<16#CD00:16>>, 8),
    Node4S = packed_node:create_node_packed_ipv6(12345, IpV6Addr, 
						 binary:copy(<<1:8>>, ?PUBLICKEYBYTES)),
    Node4 = packed_node:encode(Node4S),
    Node5S = packed_node:create_node_packed_ipv6(12345, IpV6Addr, 
						 binary:copy(<<2:8>>, ?PUBLICKEYBYTES)),
    Node5 = packed_node:encode(Node5S),
    Node6S = packed_node:create_node_packed_ipv6(12345, IpV6Addr, 
						 binary:copy(<<3:8>>, ?PUBLICKEYBYTES)),
    Node6 = packed_node:encode(Node6S),
    Node7S = packed_node:create_node_packed_ipv6(12345, IpV6Addr, 
						 binary:copy(<<4:8>>, ?PUBLICKEYBYTES)),
    Node7 = packed_node:encode(Node7S),
    NodesReq2 = <<NN2/binary, Node4/binary, Node5/binary, Node6/binary, Node7/binary>>,
    ReqId2 = binary:copy(<<0:8>>, ?REQUESTBYTES),
    RightPart2 = <<NodesReq2/binary, ReqId2/binary>>,
    CompleteResponse2 = <<OKBinary1T/binary, SomeTag/binary, ResponseCode/binary, RightPart2/binary>>,
    ?assertEqual({ok, SomePK, {{nodes_response, [Node4S, Node5S, Node6S, Node7S]}, ReqId2}}, 
		 mdht_proto:decode(CompleteResponse2)),
    %% mixed 1 ipv4 1 ipv6 1 ipv4 1 ipv6
    NN3 = <<4:8>>,
    NodesReq3 = <<NN3/binary, Node1/binary, Node5/binary, Node3/binary, Node7/binary>>,
    ReqId3 = binary:copy(<<0:8>>, ?REQUESTBYTES),
    RightPart3 = <<NodesReq3/binary, ReqId3/binary>>,
    CompleteResponse3 = <<OKBinary1T/binary, SomeTag/binary, ResponseCode/binary, RightPart3/binary>>,
    ?assertEqual({ok, SomePK, {{nodes_response, [Node1S, Node5S, Node3S, Node7S]}, ReqId2}}, 
		 mdht_proto:decode(CompleteResponse3)),
    meck:unload(encryption_server).

encode_decode_test() ->
    meck:new(encryption_server, [unstick, passthrough]),
    meck:expect(encryption_server, decrypt_message, fun(Encrypted, _, _) -> {ok,Encrypted} end),
    meck:expect(encryption_server, encrypt_message, fun(Binary, _, _) -> {ok,Binary} end),
    meck:expect(encryption_server, get_pk, fun() -> {ok, binary:copy(<<0:8>>, ?SECRETKEYBYTES)} end),
    meck:expect(encryption_server, get_nonce, fun() -> {ok, binary:copy(<<0:8>>, ?NONCEBYTES)} end),
    meck:expect(encryption_server, get_tag, fun() -> {ok, binary:copy(<<0:8>>, ?TAGBYTES)} end),
    %% encode ping_request, ping_response
    ReqId1 = binary:copy(<<0:8>>, ?REQUESTBYTES),
    ReceiverPK1 = binary:copy(<<1:8>>, ?PUBLICKEYBYTES),
    Req1 = ping_request,
    Encoded1 = mdht_proto:encode({Req1, ReqId1},ReceiverPK1),
    ?assertNotMatch({error, _}, Encoded1),
    ?assertMatch({ok, _, _}, mdht_proto:decode(Encoded1)),
    Req2 = ping_response,
    Encoded2 = mdht_proto:encode({Req2, ReqId1},ReceiverPK1),
    ?assertMatch({ok, _, _}, mdht_proto:decode(Encoded2)),
    %% nodes request
    Req3 = {nodes_request, ReceiverPK1},
    Encoded3 = mdht_proto:encode({Req3, ReqId1},ReceiverPK1),
    ?assertMatch({ok, _, _}, mdht_proto:decode(Encoded3)),
    %% nodes response
    Node1 = packed_node:create_node_packed_ipv4(12345, <<1:8, 2:8, 3:8, 4:8>>,
			       binary:copy(<<1:8>>, ?PUBLICKEYBYTES)),
    Req4 = {nodes_response, [Node1]},
    Encoded4 = mdht_proto:encode({Req4, ReqId1},ReceiverPK1),
    ?assertMatch({ok, _, _}, mdht_proto:decode(Encoded4)),
    meck:unload(encryption_server).


-endif.
