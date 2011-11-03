
-- This is some clean-up I did to remove some columns I'm not using
-- immediately. This migrates the schema of older databases to match the schema
-- currently in the code.

DROP TABLE token;
CREATE TABLE token (
    id INTEGER PRIMARY KEY,
    text TEXT UNIQUE ON CONFLICT IGNORE
);
CREATE INDEX idx_token ON token (id, text);

DROP TABLE position;
CREATE TABLE position (
    id INTEGER PRIMARY KEY,
    token_id INTEGER,
    message_id INTEGER,
    FOREIGN KEY (token_id) REFERENCES token(id),
    FOREIGN KEY (message_id) REFERENCES message(id)
);
CREATE INDEX idx_position ON position (id, token_id, message_id);

