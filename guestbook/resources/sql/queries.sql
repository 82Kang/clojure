-- :name save-message! :! :n
-- :doc creates a new message using the name and the message key
INSERT INTO guestbook
(name, message)
VALUES (:name, :message)

-- :name get-messages :? :*
-- :doc select all available messages
SELECT * FROM guestbook
