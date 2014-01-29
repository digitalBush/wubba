defrecord Wubba.Limits, 
	accept_timeout: 10000,
    request_timeout: 60000,
    header_timeout: 10000,
    body_timeout: 30000,
    max_body_size: 1024000