defrecord Wubba.Options,
	callback: nil,
	callback_args: nil,
	ip: {0,0,0,0}, 
	port: 8080, 
	min_acceptors: 20,
	name: nil