CREATE TABLE post 
  ( id           serial      PRIMARY KEY
  , feature      boolean     NOT NULL
  , image        text        
  , title        text        NOT NULL
  , content      text        NOT NULL 
  , created      timestamptz NOT NULL DEFAULT now()
  , updated      timestamptz DEFAULT null
  );

CREATE TABLE summary
  ( id           integer     PRIMARY KEY REFERENCES post(id)
  , title        text        NOT NULL
  , content      text        NOT NULL
  , created      timestamptz NOT NULL DEFAULT now()
  );

CREATE TABLE author 
  ( id      serial   PRIMARY KEY 
  , address inet     NOT NULL
  , name    text     NOT NULL
  , email   text     DEFAULT null
  );

CREATE UNIQUE INDEX address_idx ON author(id, address);
CREATE UNIQUE INDEX address_name_idx ON author(id, name);
CREATE UNIQUE INDEX address_email_idx ON author(id, email);

CREATE TABLE post_to_author 
  ( post    integer  NOT NULL REFERENCES post(id) ON DELETE CASCADE
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
  ( post integer     NOT NULL REFERENCES post(id) ON DELETE CASCADE
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

insert into post values(default, true, null,
'This is not a very long title',
'<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum vitae pharetra urna. Curabitur massa odio, feugiat in vestibulum vitae, iaculis ac ipsum. Aenean molestie faucibus metus eget consectetur. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Nulla libero dui, semper a velit in, laoreet mattis felis. Duis et faucibus sem. In mattis congue magna non vestibulum. Morbi scelerisque tellus dolor, eget porta nisi fringilla id. Fusce scelerisque felis eget molestie placerat. Fusce vulputate nisl vitae lorem bibendum, in tempus lacus consectetur. Duis quis imperdiet enim, eget pharetra eros. Sed blandit erat sed est consequat, vel vestibulum nunc feugiat. Etiam blandit faucibus ipsum, a sodales leo pretium vitae. Proin lacus sem, vulputate eu hendrerit ut, viverra non ipsum. Vestibulum metus nisi, suscipit quis finibus id, porta vitae lorem. Vivamus dapibus libero et cursus faucibus.</p>
<p>Donec dictum congue libero elementum auctor. Cras viverra velit sit amet ultricies condimentum. In porttitor velit sed enim tempor, nec fringilla purus aliquam. Sed odio ipsum, vulputate quis dui a, porta semper ex. Cras posuere mi id erat pellentesque pretium. Nulla augue eros, pharetra et ex et, venenatis laoreet sapien. Nullam tincidunt tempor augue, quis scelerisque massa feugiat ac. Donec varius finibus leo, porttitor aliquet justo congue eu. In hac habitasse platea dictumst. Morbi suscipit ipsum ut justo luctus, ac molestie neque tincidunt. Proin id libero facilisis urna hendrerit ultricies. Pellentesque sit amet tempor dolor, id convallis sapien. Etiam ullamcorper ex et libero posuere, in vehicula nibh sagittis. Aliquam laoreet mauris eget arcu blandit, quis venenatis diam consequat. Nam finibus justo velit, nec commodo orci aliquam eu.</p>
<p>Curabitur dapibus id elit id porttitor. Phasellus a dolor at nulla hendrerit vehicula id sit amet neque. Nullam aliquam ac velit ac lobortis. Sed elementum urna erat, eget mollis orci interdum pharetra. Cras vel tincidunt odio, sed interdum odio. Vestibulum egestas, arcu et bibendum auctor, felis nulla rutrum sem, vitae rutrum ante diam sit amet magna. Nulla posuere libero elit, eget posuere erat tempor nec. Cras quis laoreet dolor. Fusce quis venenatis arcu, eget tincidunt nulla. Nullam dignissim, massa nec finibus volutpat, quam ex vehicula sapien, sed bibendum leo lectus sit amet nunc. Proin et ipsum auctor, faucibus ex eu, ornare mi. Maecenas vitae ex fringilla, convallis ligula eget, dapibus ipsum. Nam ut dolor odio. Cras consectetur congue lobortis.</p>
<p>Ut velit nibh, tincidunt et ligula vel, feugiat mattis dolor. Curabitur ac diam eu libero tincidunt vulputate. Nulla gravida quam sit amet turpis lobortis faucibus. Nullam eget est et sem ornare pretium. Mauris tincidunt risus sed lacus porta euismod. Nullam id convallis ipsum. Integer eu tortor nibh. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. In sollicitudin, nisl eget vulputate cursus, augue mi tincidunt leo, quis elementum dolor orci non est. Sed imperdiet augue ac dui placerat, ut accumsan justo eleifend. Mauris at ex tincidunt, maximus leo accumsan, viverra purus. Integer sit amet nisi at ante aliquam ornare. Pellentesque ultricies vestibulum sollicitudin. Proin molestie lectus eu felis ultrices, tincidunt gravida leo cursus. Cras a sem vitae ligula luctus iaculis a ultrices lacus.</p>
<p>Morbi sodales orci leo, in lacinia nisl lobortis vel. Vestibulum posuere dolor a aliquet mattis. Pellentesque mollis, neque at fermentum sagittis, urna nisl vestibulum orci, eu lobortis lectus dui vitae sapien. Duis vestibulum nisl id mauris eleifend, id gravida tellus scelerisque. Quisque consectetur vel mi ut sollicitudin. Nullam luctus elit sit amet ante pharetra malesuada. Curabitur consectetur tellus risus. Curabitur ligula nisl, tristique ac tellus vulputate, lobortis bibendum quam. Donec placerat leo ac mattis ultrices. </p>
', default, null) ;

insert into post values(default, false, null, 'Fenner ''White Powder'' Scare; College Under Quarantine',
'<p>According to a preliminary police report, at 3pm this afternoon a resident of Fenner Hall received an unexpected package, which he proceeded to open inside the Fenner Hall Common Room. Upon opening, “an unidentified white substance appears to have discharged out of the package, covering the student’s face and the surrounding furniture,” said the report.</p>
<p>Having witnessed the event, fellow students informed reception staff of the situation, who inspected the site and instructed the student to “remain still”. Dr Jasmine Jury, Head of Fenner Hall, was subsequently advised of the situation.</p>
<blockquote>
<p>I am required to immediately inform the Facilities &amp; Services Division of the ANU in the event of potentially hazardous substances being released into residential environment, particularly since we don’t have the capacity for containment</p>
</blockquote>
<p>Christine Allard, Director of Facilities &amp; Services, provided Woroni with information as to the protocol followed.</p>
<blockquote>
<p>We have informed ACT Health as well as the Australian Federal Police of the situation. They have in turn made the decision to declare all of Fenner Hall a quarantine zone until they’re able to confidently identify the substance. They’ve estimated this will take 10 to 15 days,” she said. “Further, if the substance is identified to be hazardous, the college will be quarantined indefinitely until the ACT HAZMAT Response Unit declares it habitable.</p>
</blockquote>
<p>In response to the displacement of over 500 ANU students, Dr Jury informed this newspaper that Fenner Hall is “working together with ANUSA, the ANU and the ACT Government to formulate a durable solution to the issue.”</p>
<p>“In the meantime and until we come up with a more permanent alternative, the Federal Government has offered to relocate all 500 students to their state-of-the-art facilities on Manus Island,” said Dr Jury.</p>
<p>“We have accepted the offer.”In response to the displacement of over 500 ANU students, Dr Jury informed this newspaper that Fenner Hall is “working together with ANUSA, the ANU and the ACT Government to formulate a durable solution to the issue.”</p>
<p>“In the meantime and until we come up with a more permanent alternative, the Federal Government has offered to relocate all 500 students to their state-of-the-art facilities on Manus Island,” said Dr Jury.</p>
<blockquote>
<p>We have accepted the offer.</p>
</blockquote>
', default, null) ;

insert into author values(default, '1.2.3.4', 'Joff', null) ;
insert into author values(default, '2.3.4.5', 'Bony the skeleton', null) ;
insert into tag values(default, 'news') ;
insert into tag values(default, 'politics') ;
insert into tag values(default, 'things') ;

insert into post_to_author values(1, 1);
insert into post_to_author values(2, 1);
insert into post_to_author values(2, 2);

insert into post_to_tag values(1, 1);
insert into post_to_tag values(1, 2);
insert into post_to_tag values(2, 3);

insert into comment values(default, 1, 2, 'This is really offensive. I''m very offended. What an absolute disgrace.', default, null);
insert into comment values(default, 1, 2, '@1 Agreed, what an absolute farce this is, and a sobering day to be a human bean. Now I will repeat myself a few times, and patronise you. What ignorance. This is just my sincere opinion, it''s O.K. if you agree with it. Shithead.', default, null);
