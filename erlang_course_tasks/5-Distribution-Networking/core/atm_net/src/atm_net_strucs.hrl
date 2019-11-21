-record(card_state, {
		     card_no,
		     pin,
		     balance,
		     %% offset from beggining in the store file
		     offset,
		     busy = false
		    }).
