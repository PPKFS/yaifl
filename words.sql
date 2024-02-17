CREATE TABLE combos (
	x text not null,
	y text not null,
	z text,
	rank int not null,
	is_new tinyint(1) not null,
	primary key (x, y)
);

CREATE TABLE stats (
	method text not null,
	total int,
	success int,
	attempts int,
	primary key (method, total)
);