
ATTACH DATABASE 'ribot.db' AS src;
ATTACH DATABASE 'test.db' AS dest;

BEGIN TRANSACTION;

    -- Clear out the data currently in dest.
    DELETE FROM dest."User";
    DELETE FROM dest."Message";
    DELETE FROM dest."Topic";
    DELETE FROM dest."Token";
    DELETE FROM dest."Position";

    -- Copy users
    INSERT INTO dest."User" ("id", "username", "loggingOn")
        SELECT id, username, logging_on
        FROM src.user;

    -- Copy messages
    INSERT INTO dest."Message" ("id", "userId", "text", "posted")
        SELECT id, user_id, text, posted
        FROM src.message;

COMMIT TRANSACTION;

