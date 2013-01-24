create table question (
	qid integer primary key AUTOINCREMENT,
	typeid integer not null,
	code integer,
	subject varchar(50),
	content text,
	author varchar(20),
	open_date varchar(50),
	due_day integer,
	state varchar(10),
	price integer
);
create table answer (
	aid integer primary key autoincrement,
	qid integer,
	content text,
	author varchar(20),
	answer_date varchar(50)
);
create table user (
	uid varchar(20) primary key,
	uname varchar(50),
	open_date varchar(50),
	close_date varchar(50),
	passwd varchar(20),
	email varchar(30),
	phone varchar(30),
	mobile varchar(30),
	balance integer
);
create table txlog (
	logid integer primary key  autoincrement,
	who varchar(20),
	debit varchar(10),
	credit varchar(10),
	value integer,
	tx_date varchar(50),
	memo varchar(50)
);
