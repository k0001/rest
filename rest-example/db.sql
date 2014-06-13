create table users
( name     text primary key
, password text not null
);

create table posts
( id           serial      primary key
, author       text        not null references users (name)
, created_time timestamptz not null
, title        text        not null
, content      text        not null
);
