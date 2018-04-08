-- DROP TABLE public.games;
-- DROP SEQUENCE public.games_id_seq;

CREATE SEQUENCE public.games_id_seq
INCREMENT BY 1
MINVALUE 1
MAXVALUE 9223372036854775807
START 1;

CREATE TABLE public.games (
  id int4 NOT NULL DEFAULT nextval('games_id_seq'::regclass),
  league_id int4 NOT NULL,
  progress text NOT NULL DEFAULT 'new',
  notes text NOT NULL DEFAULT '',
  game_time timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
  home_team_id int4 NOT NULL DEFAULT 0,
  away_team_id int4 NOT NULL DEFAULT 0,
  home_team_score int4 NOT NULL DEFAULT 0,
  away_team_score int4 NOT NULL DEFAULT 0,
  created_on timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
  created_by text NOT NULL DEFAULT '',
  modified_on timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
  modified_by text NOT NULL DEFAULT '',
  CONSTRAINT games_pk PRIMARY KEY (id)
)
WITH (
  OIDS=FALSE
) ;
