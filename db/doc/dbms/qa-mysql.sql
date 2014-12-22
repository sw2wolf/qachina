create table question (
	qid integer unsigned not null auto_increment,
	typeid smallint not null,
	code smallint not null,
	subject varchar(50) not null,
	content text,
	author varchar(20) not null,
	open_date datetime not null,
	expire_date datetime,
	state varchar(10) not null,
	price integer,
	primary key (qid)
);

create table answer (
	aid integer unsigned not null auto_increment,
	qid integer unsigned not null,
	content text not null,
	author varchar(20) not null,
	answer_date datetime not null,
	primary key (aid)
);

create table user (
	uid varchar(20) not null,
	uname varchar(50),
	open_date datetime not null,
	close_date datetime not null,
	passwd varchar(20) not null,
	email varchar(30) not null,
	phone varchar(30) not null,
	mobile varchar(30) not null,
	balance integer,
	primary key (uid),
	key (uname)
);

create table txlog (
	logid integer not null auto_increment,
	who varchar(20) not null,
	debit varchar(10) not null,
	credit varchar(10) not null,
	value integer not null,
	tx_date datetime not null,
	memo varchar(50),
	primary key (logid),
	key (who)
);
