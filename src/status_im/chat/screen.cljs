(ns status-im.chat.screen
  (:require-macros [status-im.utils.views :refer [defview letsubs]])
  (:require [re-frame.core :refer [subscribe dispatch]]
            [status-im.ui.components.react :as react]
            [status-im.ui.components.icons.vector-icons :as vi]
            [status-im.ui.components.status-bar :as status-bar]
            [status-im.ui.components.chat-icon.screen :refer [chat-icon-view-action
                                                              chat-icon-view-menu-item]]
            [status-im.chat.styles.screen :as st]
            [status-im.utils.listview :as listview] 
            [status-im.utils.datetime :as time]
            [status-im.utils.platform :as platform]
            [status-im.ui.components.invertible-scroll-view :refer [invertible-scroll-view]]
            [status-im.ui.components.toolbar.view :as toolbar]
            [status-im.chat.views.toolbar-content :refer [toolbar-content-view]]
            [status-im.chat.views.message.message :refer [chat-message]]
            [status-im.chat.views.message.datemark :refer [chat-datemark]]
            [status-im.chat.views.input.input :as input]
            [status-im.chat.views.actions :refer [actions-view]]
            [status-im.chat.views.bottom-info :refer [bottom-info-view]]
            [status-im.chat.constants :as chat-const]
            [status-im.i18n :refer [label label-pluralize]]
            [status-im.ui.components.animation :as anim]
            [status-im.ui.components.sync-state.offline :refer [offline-view]]
            [taoensso.timbre :as log]
            [clojure.string :as str]))

(defview chat-icon []
  (letsubs [{:keys [chat-id group-chat name color]} [:get-current-chat]]
    [chat-icon-view-action chat-id group-chat name color true]))

(defn- toolbar-action [show-actions?] 
  [react/touchable-highlight
   {:on-press            #(dispatch [:set-chat-ui-props {:show-actions? (not show-actions?)}])
    :accessibility-label :chat-menu}
   [react/view st/action
    (if show-actions?
      [vi/icon :icons/dropdown-up]
      [chat-icon])]])

(defview add-contact-bar []
  (letsubs [chat-id [:get-current-chat-id]
            pending-contact? [:current-contact :pending?]]
    (when pending-contact?
      [react/touchable-highlight
       {:on-press #(dispatch [:add-pending-contact chat-id])}
       [react/view st/add-contact
        [react/text {:style st/add-contact-text}
         (label :t/add-to-contacts)]]])))

(defview chat-toolbar []
  (letsubs [show-actions? [:get-current-chat-ui-prop :show-actions?]
            accounts [:get-accounts]
            creating? [:get :accounts/creating-account?]]
    [react/view
     [status-bar/status-bar]
     [toolbar/toolbar {:show-sync-bar? true}
      (when-not (or show-actions? creating?)
        (if (empty? accounts)
          [toolbar/nav-clear-text (label :t/recover) #(dispatch [:navigate-to-modal :recover-modal])]
          toolbar/default-nav-back))
      [toolbar-content-view]
      [toolbar-action show-actions?]]
     [add-contact-bar]]))

(defmulti message-row (fn [{{:keys [type]} :row}] type))

(defmethod message-row :datemark
  [{{:keys [value]} :row}]
  (react/list-item [chat-datemark value]))

(defmethod message-row :default
  [{:keys [group-chat row]}]
  (react/list-item [chat-message (assoc row :group-chat group-chat)]))

(defview messages-view [group-chat]
  (letsubs [messages [:get-current-chat-messages]
            loaded? [:all-messages-loaded?]]
    [react/list-view {:renderRow                 (fn [row _ index]
                                                   (message-row {:group-chat group-chat
                                                                 :row        row}))
                      :renderScrollComponent     #(invertible-scroll-view (js->clj %))
                      :onEndReached              (when-not loaded? #(dispatch [:load-more-messages]))
                      :enableEmptySections       true
                      :keyboardShouldPersistTaps (if platform/android? :always :handled)
                      :dataSource                (listview/to-datasource-inverted messages)}]))

(defview chat []
  (letsubs [{:keys [group-chat input-text]} [:get-current-chat]
            show-actions? [:get-current-chat-ui-prop :show-actions?]
            show-bottom-info? [:get-current-chat-ui-prop :show-bottom-info?]
            show-emoji? [:get-current-chat-ui-prop :show-emoji?]
            layout-height [:get :layout-height]]
    {:component-did-mount    #(dispatch [:check-and-open-dapp!])
     :component-will-unmount #(dispatch [:set-chat-ui-props {:show-emoji? false}])}
    [react/view {:style st/chat-view
                 :on-layout (fn [event]
                              (let [height (.. event -nativeEvent -layout -height)]
                                (when (not= height layout-height)
                                  (dispatch [:set-layout-height height]))))}
     [chat-toolbar]
     [messages-view group-chat]
     [input/container {:text-empty? (str/blank? input-text)}]
     (when show-actions?
       [actions-view])
     (when show-bottom-info?
       [bottom-info-view])
     [offline-view {:top (get-in platform/platform-specific
                                 [:component-styles :status-bar :default :height])}]]))
