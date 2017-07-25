CREATE EXTENSION pg_trgm;

ALTER TABLE entry ADD COLUMN author_list TEXT NOT NULL DEFAULT '';
ALTER TABLE entry ADD COLUMN search_string TEXT NOT NULL DEFAULT '';

CREATE OR REPLACE FUNCTION entry_search_trigger() RETURNS trigger AS $$
begin
  new.author_list := array_to_string(new.authors, ' ');
  new.search_string := new.author_list || ' ' || coalesce(new.title, '');
  new.tsv :=
    setweight(to_tsvector('english', coalesce(new.title, '')), 'A') ||
    setweight(to_tsvector(array_to_string(new.authors, ' ')), 'A') ||
    setweight(to_tsvector('english', coalesce(new.journal,'')), 'B') ||
    setweight(to_tsvector('english', coalesce(new.series,'')), 'B') ||
    setweight(to_tsvector(coalesce(new.editor,'')), 'B');
  return new;
end
$$ LANGUAGE plpgsql;

CREATE INDEX entry_title_trgm ON entry USING gist(title gist_trgm_ops);
CREATE INDEX entry_author_list_trgm ON entry USING gist(author_list gist_trgm_ops);
CREATE INDEX entry_year ON entry (year);
CREATE INDEX entry_search_string ON entry USING gist(search_string gist_trgm_ops);
