ALTER TABLE entry ALTER COLUMN title DROP NOT NULL;

CREATE OR REPLACE FUNCTION entry_search_trigger() RETURNS trigger AS $$
begin
  new.tsv :=
    setweight(to_tsvector('english', coalesce(new.title, '')), 'A') ||
    setweight(to_tsvector(array_to_string(new.authors, ' ')), 'A') ||
    setweight(to_tsvector('english', coalesce(new.journal,'')), 'B') ||
    setweight(to_tsvector('english', coalesce(new.series,'')), 'B') ||
    setweight(to_tsvector(coalesce(new.editor,'')), 'B');
  return new;
end
$$ LANGUAGE plpgsql;
