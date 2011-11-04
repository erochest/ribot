
SELECT t.text, u.username, m.text
FROM position p
JOIN token t ON t.id=p.token_id
JOIN message m ON m.id=p.message_id
JOIN user u ON u.id=m.user_id
ORDER BY t.text, u.username, m.posted;

