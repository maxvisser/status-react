(ns status-im.chat.models.unviewed-messages)

(defn load-unviewed-messages [db unviewed-messages]
  (assoc db :unviewed-messages
         (into {}
               (map (fn [[id messages]]
                     [id (into #{} (map :message-id) messages)]))
               (group-by :chat-id unviewed-messages))))

(defn add-unviewed-message [db chat-id message-id] 
  (update-in db [:unviewed-messages chat-id] (fnil conj #{}) message-id))

(defn remove-unviewed-message [db chat-id message-id]
  (update-in db [:unviewed-messages chat-id] disj message-id))
