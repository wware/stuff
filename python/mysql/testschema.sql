-- --------------------------------------------------
-- --------------------------------------------------
-- ------------ Stars, planets, moons ---------------
-- --------------------------------------------------
-- --------------------------------------------------
-- Stars, planets, moons
create table body (
    id    BIGINT       NOT NULL AUTO_INCREMENT PRIMARY KEY,
    name  VARCHAR(64)  NOT NULL
) ENGINE=InnoDB;

-- Sol and its planets
insert into body (id, name) values (1, 'Sol');
insert into body (id, name) values (2, 'Mercury');
insert into body (id, name) values (3, 'Venus');
insert into body (id, name) values (4, 'Earth');
insert into body (id, name) values (5, 'Mars');
insert into body (id, name) values (6, 'Jupiter');
insert into body (id, name) values (7, 'Saturn');
insert into body (id, name) values (8, 'Uranus');
insert into body (id, name) values (9, 'Neptune');
insert into body (id, name) values (10, 'Pluto');

-- Earth's moon
insert into body (id, name) values (11, 'Luna');

-- Moons of Mars
insert into body (id, name) values (12, 'Phobos');
insert into body (id, name) values (13, 'Deimos');

-- Moons of Jupiter (the ones I remember anyway)
insert into body (id, name) values (14, 'Io');
insert into body (id, name) values (15, 'Europa');
insert into body (id, name) values (16, 'Ganymede');

-- --------------------------------------------------
-- --------------------------------------------------
-- ------------- Who orbits whom --------------------
-- --------------------------------------------------
-- --------------------------------------------------

create table orbit (
    big_id    BIGINT       NOT NULL,
    small_id  BIGINT       NOT NULL,
    PRIMARY KEY (big_id, small_id)
) ENGINE=InnoDB;

-- Sol and its planets
insert into orbit (big_id, small_id) values (1, 2);
insert into orbit (big_id, small_id) values (1, 3);
insert into orbit (big_id, small_id) values (1, 4);
insert into orbit (big_id, small_id) values (1, 5);
insert into orbit (big_id, small_id) values (1, 6);
insert into orbit (big_id, small_id) values (1, 7);
insert into orbit (big_id, small_id) values (1, 8);
insert into orbit (big_id, small_id) values (1, 9);
insert into orbit (big_id, small_id) values (1, 10);

-- Earth's moon
insert into orbit (big_id, small_id) values (4, 11);

-- Moons of Mars
insert into orbit (big_id, small_id) values (5, 12);
insert into orbit (big_id, small_id) values (5, 13);

-- Moons of Jupiter (the ones I remember anyway)
insert into orbit (big_id, small_id) values (6, 14);
insert into orbit (big_id, small_id) values (6, 15);
insert into orbit (big_id, small_id) values (6, 16);

-- --------------------------------------------------
-- --------------------------------------------------
-- ------- Table with multiple primary keys ---------
-- --------------------------------------------------
-- --------------------------------------------------

-- http://stackoverflow.com/questions/217945/
create table userdata (
  userid     BIGINT NOT NULL,
  userdataid BIGINT NOT NULL,
  info VARCHAR(20),
  PRIMARY KEY (userid, userdataid)
) ENGINE=InnoDB;

insert into userdata (userid, userdataid, info)
            values (1, 1, "Alpha");
insert into userdata (userid, userdataid, info)
            values (1, 2, "Bravo");
insert into userdata (userid, userdataid, info)
            values (1, 3, "Charlie");
insert into userdata (userid, userdataid, info)
            values (2, 1, "Delta");
insert into userdata (userid, userdataid, info)
            values (2, 2, "Echo");
insert into userdata (userid, userdataid, info)
            values (2, 3, "Foxtrot");
insert into userdata (userid, userdataid, info)
            values (3, 1, "Golf");
insert into userdata (userid, userdataid, info)
            values (3, 2, "Hotel");
insert into userdata (userid, userdataid, info)
            values (3, 3, "India");
insert into userdata (userid, userdataid, info)
            values (4, 1, "Juliet");
insert into userdata (userid, userdataid, info)
            values (4, 2, "Kilo");
insert into userdata (userid, userdataid, info)
            values (4, 3, "Lima");

-- --------------------------------------------------
-- --------------------------------------------------
-- -------- Tables for row method tests -------------
-- --------------------------------------------------
-- --------------------------------------------------

create table A (
  id     BIGINT NOT NULL PRIMARY KEY,
  info   VARCHAR(20)
) ENGINE=InnoDB;

insert into A (id, info)
       values (1, "Alpha");
insert into A (id, info)
       values (2, "Bravo");
insert into A (id, info)
       values (3, "Charlie");

create table B (
  id     BIGINT NOT NULL PRIMARY KEY,
  info   VARCHAR(20)
) ENGINE=InnoDB;

insert into B (id, info)
       values (1, "XXXX");
insert into B (id, info)
       values (2, "XXXXX");
insert into B (id, info)
       values (3, "XXXXXX");
