(ns status-im.chat.utils
  (:require [clojure.string :as str]
            [status-im.constants :as consts]
            [status-im.chat.constants :as chat-const]))

(defn console? [s]
  (= consts/console-chat-id s))

(def not-console?
  (complement console?))

(defn safe-trim [s]
  (when (string? s)
    (str/trim s)))

(defn add-message-to-db
  ([db add-to-chat-id chat-id message] (add-message-to-db db add-to-chat-id chat-id message true))
  ([db add-to-chat-id chat-id {:keys [message-id] :as message} new?]
   (let [prepared-message (assoc message
                                 :chat-id chat-id
                                 :new? (if (nil? new?) true new?))] 
     (update-in db [:chats add-to-chat-id :messages] assoc message-id prepared-message))))

(defn command-name [{:keys [bot name scope]}]
  (cond
    (:global? scope)
    (str chat-const/bot-char name)

    :default
    (str chat-const/command-char name)))
