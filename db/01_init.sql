CREATE TABLE entry (
        id SERIAL UNIQUE,
        key VARCHAR(255) UNIQUE,
        ty VARCHAR(255) NOT NULL,
        authors VARCHAR(255)[] NOT NULL,
        title TEXT NOT NULL,
        year INT4,
        journal VARCHAR(255),
        url TEXT,
        ee TEXT,
        pages VARCHAR(255),
        volume VARCHAR(255),
        editor VARCHAR(255),
        series VARCHAR(255),
        tsv TSVECTOR NOT NULL,
        CONSTRAINT "entry_id" PRIMARY KEY (id)
);

CREATE INDEX entry_ty ON entry (ty);
CREATE INDEX entry_tsv ON entry USING gin(tsv);

CREATE FUNCTION entry_search_trigger() RETURNS trigger AS $$
begin
  new.tsv :=
    setweight(to_tsvector('english', new.title), 'A') ||
    setweight(to_tsvector(array_to_string(new.authors, ' ')), 'A') ||
    setweight(to_tsvector('english', coalesce(new.journal,'')), 'B') ||
    setweight(to_tsvector('english', coalesce(new.series,'')), 'B') ||
    setweight(to_tsvector(coalesce(new.editor,'')), 'B');
  return new;
end
$$ LANGUAGE plpgsql;

CREATE TRIGGER entry_update_tsv BEFORE INSERT OR UPDATE
    ON entry FOR EACH ROW EXECUTE PROCEDURE entry_search_trigger();
