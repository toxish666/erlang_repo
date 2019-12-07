%% Structure to hold up to 
%% {@link get_bucket_max_entries/0} * {@link kbucket:get_bucket_default_size/0}
%% nodes close to own PK.
%% Buckets in ktree are sorted by closeness to the PK;
%% closest bucket is the last one, while furthest is the first one.
-record(ktree, {
		pk :: mdht:public_key(),
		kbuckets :: ets:tab()
	       }).
-type ktree() :: #ktree{}.


%% This structure will be stored into the ets.
-record(ind_kbucket, {index, kbucket}).
