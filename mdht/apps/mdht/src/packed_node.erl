%% @doc packed_node format is a pair of socket address & public key.

-module(packed_node).
-include("common_records.hrl").

%% encoding/decoding into packed node
-export([
	 encode/1,
	 decode/1
	]).

%% packed_node manipulations
-export([
	 new_packed_node/2,
	 get_pk/1,
	 get_saddr/1,
	 create_node_packed_ipv4/3,
	 create_node_packed_ipv6/3
	]).

%% Way to store the node info in a small format.
%% This record mainly built in packed_node.erl module
-record(packed_node, {
		      saddr :: socket(),
		      pk :: mdht:public_key()
		     }).
-type packed_node() :: #packed_node{}.
-export_type([packed_node/0]).

%% @doc Encoding in the next format:
%% Length | Content
%% 1      | v4 or v6
%% 4 or 16| ipv4 or ipv6
%% 2      | port
%% 32     | node PK
%% @end
-spec encode(packed_node()) -> binary().
encode(PackedNode) ->
    SAddr = PackedNode#packed_node.saddr,
    SPreInfo = SAddr#socket.socket_pre_info,
    %% check for socket options and convert in to binary format
    Port = SPreInfo#socket_pre_info.port,
    PortB = case Port of
		P when is_integer(P) ->
		     <<Port:16>>;
		P when is_list(P) ->
		    PN = list_to_integer(P),
		    <<PN:16>>
	    end,
    PublicKey = PackedNode#packed_node.pk, % already in binary format
    {FormatB, AddrB} = case SPreInfo#socket_pre_info.ip6_address of
			   %% ipv4 version
			   undefined ->
			       FormatBB = <<2:8>>,
			       <<_I1:8,_I2:8,_I3:8,_I4:8>> = IpV4Addr = SPreInfo#socket_pre_info.ip4_address,
			       {FormatBB, IpV4Addr};
			   %% ipv6 version
			   IpV6 ->
			       FormatBB = <<10:8>>,
			       <<_I1:16,_I2:16,_I3:16,_I4:16,_I5:16,_I6:16,_I7:16,_I8:16>> = IpV6,
			       {FormatBB, IpV6}
		     end,
    <<FormatB/binary, AddrB/binary, PortB/binary, PublicKey/binary>>.


%% @doc Decode given binary into packed_node format
%% or none if ther was some parsing error
%% @end
-spec decode(binary()) -> mdht:option(packed_node()).
decode(Binary) ->
    try
	<<Format:8, AfterFormatBinary/binary>> = Binary,
	case Format of 
	    2 ->
		<<I1:8,I2:8,I3:8,I4:8, AfterAddressBinary/binary>> =  AfterFormatBinary,
		Address = <<I1:8,I2:8,I3:8,I4:8>>;
	    10 ->
		<<I1:16,I2:16,I3:16,I4:16,
		  I5:16,I6:16,I7:16,I8:16, AfterAddressBinary/binary>> = AfterFormatBinary,
		Address = <<I1:16,I2:16,I3:16,I4:16,I5:16,I6:16,I7:16,I8:16>>
	end,
	<<Port:16, AfterPortBinary/binary>> = AfterAddressBinary,
	<<PublicKey:(32*8)/bitstring>> = AfterPortBinary,
	%% construct new packed_node
	case Format of 
	    2 ->
		SocketPreInfo = #socket_pre_info{port = Port, ip4_address = Address};
	    10 ->
		SocketPreInfo = #socket_pre_info{port = Port, ip6_address = Address}
	end,
	new_packed_node(#socket{socket_pre_info = SocketPreInfo}, PublicKey)
    catch
	_Exception:_Reason ->
	    none
    end.
	    

%% @doc Create new packed node with given socket and pk.
-spec new_packed_node(socket(), mdht:public_key()) -> packed_node().
new_packed_node(Socket, PublicKey) ->
    logger:debug("Creating new packed_node ~n"),
    logger:debug("With args socket_address: ~p, PK: ~p ~n", [Socket, PublicKey]),
    #packed_node{saddr = Socket, pk = PublicKey}.
    
    
%% @doc Getter for extracting pk out of a packed node
-spec get_pk(packed_node()) -> mdht:public_key().
get_pk(PckdN) ->
    PckdN#packed_node.pk.
    

%% @doc Getter for extracting saddr out of a packed node
-spec get_saddr(packed_node()) -> socket().
get_saddr(PckdN) ->
    PckdN#packed_node.saddr.

%% @doc Create new ipv4 packed node
create_node_packed_ipv4(Port, Addr, PK) ->
    SocketPreInfo = #socket_pre_info{
		       port = Port,
		       ip4_address = Addr},
    Socket = #socket{socket_pre_info = SocketPreInfo},
    packed_node:new_packed_node(Socket,PK).

%% @doc Create new ipv6 packed node
create_node_packed_ipv6(Port, Addr, PK) ->
    SocketPreInfo = #socket_pre_info{
		       port = Port,
		       ip6_address = Addr},
    Socket = #socket{socket_pre_info = SocketPreInfo},
    packed_node:new_packed_node(Socket,PK).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("consts.hrl").

decode_encode_test() ->
    Node1 = create_node_packed_ipv4(12345, <<1:8, 2:8, 3:8, 4:8>>,
				    binary:copy(<<1:8>>, ?PUBLICKEYBYTES)),
    Encoded1 = encode(Node1),
    ?assertEqual(39, size(Encoded1)),
    Decoded1 = decode(Encoded1),
    ?assertEqual(Node1, Decoded1),
    IpV6Addr = binary:copy(<<16#CD00:16>>, 8),
    Node2 = create_node_packed_ipv6(12345, IpV6Addr,
				    binary:copy(<<1:8>>, ?PUBLICKEYBYTES)),
    Encoded2 = encode(Node2),
    ?assertEqual(51, size(Encoded2)),
    Decoded2 = decode(Encoded2),
    ?assertEqual(Node2, Decoded2).

-endif.
