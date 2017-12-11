(ns status-im.chat.subs
  (:require [re-frame.core :refer [reg-sub subscribe]]
            [status-im.constants :as const]
            [status-im.chat.constants :as chat-const]
            [status-im.chat.models.input :as input-model]
            [status-im.chat.models.commands :as commands-model]
            [status-im.chat.utils :as chat-utils]
            [status-im.chat.views.input.utils :as input-utils]
            [status-im.commands.utils :as commands-utils]
            [status-im.utils.datetime :as time]
            [status-im.utils.platform :refer [ios?]]
            [taoensso.timbre :as log]
            [clojure.string :as str]))

(reg-sub :chats :chats)

(reg-sub :get-current-chat-id :current-chat-id)

(reg-sub :chat-ui-props :chat-ui-props)

(reg-sub
  :get-current-chat-ui-props
  :<- [:chat-ui-props]
  :<- [:get-current-chat-id]
  (fn [[chat-ui-props id]]
    (get chat-ui-props id)))

(reg-sub
  :get-current-chat-ui-prop
  :<- [:get-current-chat-ui-props]
  (fn [ui-props [_ prop]]
    (get ui-props prop)))

(reg-sub
  :validation-messages
  :<- [:get-current-chat-ui-props]
  (fn [ui-props]
    (some-> ui-props :validation-messages commands-utils/generate-hiccup)))

(reg-sub
  :result-box-markup
  :<- [:get-current-chat-ui-props]
  (fn [ui-props]
    (some-> ui-props :result-box :markup commands-utils/generate-hiccup)))

(reg-sub
  :chat-input-margin
  :<- [:get :keyboard-height]
  (fn [kb-height]
    (if ios? kb-height 0)))

(reg-sub
  :get-chat
  :<- [:chats]
  (fn [chats [_ chat-id]]
    (get chats chat-id)))

(reg-sub
  :get-current-chat
  (fn [_]
    (let [current-chat-id (subscribe [:get-current-chat-id])]
      (subscribe [:get-chat @current-chat-id])))
  identity)

(reg-sub
  :chat
  :<- [:chats]
  :<- [:get-current-chat-id]
  (fn [[chats id] [_ k chat-id]]
    (get-in chats [(or chat-id id) k])))

(defn message-datemark-groups
  "Transforms map of messages into sequence of `[datemark messages]` tuples, where
  messages with particular datemark are sorted according to their `:clock-value` and
  tuples themeselves are sorted according to the highest `:clock-value` in the messages."
  [id->messages]
  (let [datemark->messages (transduce (comp (map second)
                                            (filter :show?)
                                            (map (fn [{:keys [timestamp] :as msg}]
                                                   (assoc msg :datemark (time/day-relative timestamp)))))
                                      (completing (fn [acc {:keys [datemark] :as msg}]
                                                    (update acc datemark conj msg)))
                                      {}
                                      id->messages)]
    (->> datemark->messages
         (map (fn [[datemark messages]]
                [datemark (sort-by :clock-value > messages)]))
         (sort-by (comp :clock-value first second) >))))

(reg-sub
  :get-chat-message-datemark-groups
  (fn [[_ chat-id]]
    (subscribe [:get-chat chat-id]))
  (fn [{:keys [messages]}]
    (message-datemark-groups messages)))

(defn messages-stream
  "Transforms message-datemark-groups into flat sequence of messages interspersed with
  datemark messages.
  Additionaly enhances the messages in message sequence with derived stream context information,
  like `:same-author?`, `:same-direction?` and `:last-outgoing?` flags + contact info/status
  message for the last dategroup."
  [[[last-datemark last-messages] :as message-datemark-groups]]
  (let [{last-outgoing-message-id :message-id} (->> message-datemark-groups
                                                    (mapcat second)
                                                    (filter :outgoing)
                                                    first)]
    ;; TODO janherich: why the heck do we display contact user info/status in chat as a message in stream ?
    ;; This makes no sense, user wants to have this information always available, not as something which
    ;; scrolls with message stream
    (->> (conj (rest message-datemark-groups)
               [last-datemark (conj (into [] last-messages) {:content-type const/content-type-status})])
         (mapcat (fn [[datemark messages]]
                   (let [prepared-messages (into []
                                                 (map (fn [{:keys [message-id] :as message} previous-message]
                                                        (assoc message
                                                               :same-author? (= (:from message)
                                                                                (:from previous-message))
                                                               :same-direction? (= (:outgoing message)
                                                                                   (:outgoing previous-message))
                                                               :last-outgoing? (= message-id
                                                                                  last-outgoing-message-id)))
                                                      messages
                                                      (concat (rest messages) '(nil))))]
                     (conj prepared-messages {:type :datemark
                                              :value datemark})))))))

(reg-sub
  :get-current-chat-messages
  (fn [_]
    (let [current-chat-id (subscribe [:get-current-chat-id])]
      (subscribe [:get-chat-message-datemark-groups @current-chat-id])))
  (fn [message-datemark-groups]
    (messages-stream message-datemark-groups)))

(reg-sub
  :get-commands-for-chat
  :<- [:get-commands-responses-by-access-scope]
  :<- [:get-current-account]
  :<- [:get-current-chat]
  :<- [:get-contacts]
  (fn [[commands-responses account chat contacts]]
    (commands-model/commands-responses :command commands-responses account chat contacts)))

(reg-sub
  :get-responses-for-chat
  :<- [:get-commands-responses-by-access-scope]
  :<- [:get-current-account]
  :<- [:get-current-chat]
  :<- [:get-contacts]
  :<- [:chat :requests]
  (fn [[commands-responses account chat contacts requests]]
    (commands-model/requested-responses commands-responses account chat contacts (vals requests))))

(def ^:private map->sorted-seq (comp (partial map second) (partial sort-by first)))

(defn- available-commands-responses [[commands-responses input-text]]
  (->> commands-responses
       map->sorted-seq
       (filter #(str/includes? (chat-utils/command-name %) (or input-text "")))))

(reg-sub
  :get-available-commands
  :<- [:get-commands-for-chat]
  :<- [:chat :input-text]
  available-commands-responses)

(reg-sub
  :get-available-responses
  :<- [:get-responses-for-chat]
  :<- [:chat :input-text]
  available-commands-responses)

(reg-sub
  :get-available-commands-responses
  :<- [:get-commands-for-chat]
  :<- [:get-responses-for-chat]
  (fn [[commands responses]]
    (map->sorted-seq (merge commands responses))))

(reg-sub
  :selected-chat-command
  :<- [:get-current-chat]
  :<- [:get-commands-for-chat]
  :<- [:get-responses-for-chat]
  (fn [[chat commands responses]]
    (input-model/selected-chat-command chat commands responses)))

(reg-sub
  :current-chat-argument-position
  :<- [:selected-chat-command]
  :<- [:chat :input-text]
  :<- [:chat :seq-arguments]
  :<- [:get-current-chat-ui-prop :selection]
  (fn [[command input-text seq-arguments selection]]
    (input-model/current-chat-argument-position command input-text selection seq-arguments)))

(reg-sub
  :chat-parameter-box
  :<- [:get-current-chat]
  :<- [:selected-chat-command]
  :<- [:current-chat-argument-position]
  (fn [[current-chat selected-chat-command argument-position]]
    (cond
      (and selected-chat-command
           (not= argument-position input-model/*no-argument-error*))
      (get-in current-chat [:parameter-boxes
                            (get-in selected-chat-command [:command :name])
                            argument-position])

      (not selected-chat-command)
      (get-in current-chat [:parameter-boxes :message])

      :default
      nil)))

(reg-sub
  :show-parameter-box?
  :<- [:chat-parameter-box]
  :<- [:show-suggestions?]
  :<- [:chat :input-text]
  :<- [:validation-messages]
  (fn [[chat-parameter-box show-suggestions? input-text validation-messages]]
    (and (get chat-parameter-box :markup)
         (not validation-messages)
         (not show-suggestions?))))

(reg-sub
  :command-completion
  :<- [:selected-chat-command]
  input-model/command-completion)

(reg-sub
  :show-suggestions?
  :<- [:get-current-chat-ui-prop :show-suggestions?]
  :<- [:chat :input-text]
  :<- [:selected-chat-command]
  :<- [:get-available-commands-responses]
  (fn [[show-suggestions? input-text selected-command commands-responses]]
    (and (or show-suggestions? (input-model/starts-as-command? (str/trim (or input-text ""))))
         (not (:command selected-command))
         (seq commands-responses))))

(reg-sub
  :is-request-answered?
  :<- [:chat :requests]
  (fn [requests [_ message-id]]
    (not= "open" (get-in requests [message-id :status]))))

(reg-sub
  :unviewed-messages-count
  (fn [db [_ chat-id]]
    (count (get-in db [:unviewed-messages chat-id]))))

(reg-sub
  :web-view-extra-js
  :<- [:get-current-chat]
  (fn [current-chat]
    (:web-view-extra-js current-chat)))

(reg-sub
  :all-messages-loaded?
  :<- [:get-current-chat]
  (fn [current-chat]
    (:all-loaded? current-chat)))

(reg-sub
  :photo-path
  :<- [:get-contacts]
  (fn [contacts [_ id]]
    (:photo-path (contacts id))))

(reg-sub
  :get-last-message
  (fn [[_ chat-id]]
    (subscribe [:get-chat-message-datemark-groups chat-id]))
  (comp first second first))

(reg-sub
  :get-message-short-preview-markup
  (fn [db [_ message-id]]
    (get-in db [:message-data :short-preview message-id :markup])))

(reg-sub
  :get-last-message-short-preview
  (fn [db [_ chat-id]]
    (let [last-message (subscribe [:get-last-message chat-id])
          preview (subscribe [:get-message-short-preview-markup (:message-id @last-message)])]
      (when-let [markup @preview]
        (commands-utils/generate-hiccup markup)))))

(reg-sub
  :get-default-container-area-height
  :<- [:get-current-chat-ui-prop :input-height]
  :<- [:get :layout-height]
  :<- [:chat-input-margin]
  (fn [[input-height layout-height chat-input-margin]]
    (let [bottom (+ input-height chat-input-margin)]
      (input-utils/default-container-area-height bottom layout-height))))

(reg-sub
  :get-max-container-area-height
  :<- [:get-current-chat-ui-prop :input-height]
  :<- [:get :layout-height]
  :<- [:chat-input-margin]
  (fn [[input-height layout-height chat-input-margin]]
    (let [bottom (+ input-height chat-input-margin)]
      (input-utils/max-container-area-height bottom layout-height))))

(reg-sub
  :chat-animations
  (fn [db [_ key type]]
    (let [chat-id (subscribe [:get-current-chat-id])]
      (get-in db [:chat-animations @chat-id key type]))))

(reg-sub
  :get-message-preview-markup
  (fn [db [_ message-id]]
    (get-in db [:message-data :preview message-id :markup])))

(reg-sub
  :get-message-preview
  (fn [[_ message-id]]
    [(subscribe [:get-message-preview-markup message-id])])
  (fn [[markup]]
    (when markup
      (commands-utils/generate-hiccup markup))))
