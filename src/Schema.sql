CREATE TABLE post 
  ( id           serial      PRIMARY KEY
  , title        text        NOT NULL
  , content      text        NOT NULL 
  , created      timestamptz NOT NULL DEFAULT now()
  , updated      timestamptz DEFAULT null
  );

CREATE TABLE author 
  ( id      serial   PRIMARY KEY 
  , address inet     NOT NULL
  , name    text     NOT NULL
  , email   text     DEFAULT null
  );

CREATE UNIQUE INDEX address_idx ON author(id, address);

CREATE TABLE post_to_author 
  ( post    integer  NOT NULL REFERENCES post(id) 
  , author  integer  NOT NULL REFERENCES author(id) 
  );

CREATE UNIQUE INDEX post_to_author_idx ON post_to_author(post, author) ;

CREATE TABLE tag
  ( id   serial   PRIMARY KEY 
  , name text     NOT NULL 
  );

CREATE UNIQUE INDEX tag_idx      ON tag(id);
CREATE UNIQUE INDEX tag_name_idx ON tag(name);

CREATE TABLE post_to_tag
  ( post integer     NOT NULL REFERENCES post(id)
  , tag  integer     NOT NULL REFERENCES tag(id)
  );

CREATE UNIQUE INDEX post_to_tag_idx ON post_to_tag(post, tag);

CREATE TABLE comment 
  ( id          serial      PRIMARY KEY 
  , parent      integer     NOT NULL REFERENCES post(id) 
  , author      integer     REFERENCES author(id) -- anonymous if null
  , content     text        NOT NULL
  , created     timestamptz NOT NULL DEFAULT now()
  , updated     timestamptz DEFAULT null
  );

CREATE UNIQUE INDEX id_parent_idx ON comment(id, parent);

insert into post values(1, 'hello', 'hi', default, null) ;
insert into post values(2, 'hi', 'hello', default, null) ;
insert into author values(1, '1.2.3.4', 'alex catalan flores', null) ;
insert into author values(2, '2.3.4.5', 'jacob ingram', null) ;
insert into tag values(1, 'news') ;
insert into tag values(2, 'politics') ;
insert into tag values(3, 'things') ;
insert into post_to_author values(1, 1);
insert into post_to_author values(2, 1);
insert into post_to_author values(2, 2);
insert into post_to_tag values(1, 1);
insert into post_to_tag values(1, 2);
insert into post_to_tag values(2, 3);

insert into comment values(1, 1, 2, 'hey man what''s up', default, null);

explain analyse
select post.*
  from tag
  left join post_to_tag on post_to_tag.tag  = tag.id
  left join post        on post_to_tag.post = post.id
  where tag.name in ('politics', 'news')
  group by post.id;

select comment.*
  from post
  join comment on post.id = comment.parent;
