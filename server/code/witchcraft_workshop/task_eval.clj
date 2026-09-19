(ns witchcraft-workshop.task-eval
  "nREPL middleware that runs every `eval` on the Minecraft server thread, so
  you don't have to wrap world-mutating code in [[wc/run-task]].

  This replaces `lambdaisland.witchcraft.nrepl.task-eval`, which was written
  against nREPL 0.x. The witchcraft plugin now ships nREPL 1.6, where:

  - a session's `:exec` fn is called with four args (id, thunk, ack, msg), not
    three, so the old middleware threw an ArityException on every eval;
  - the session middleware no longer applies the session's dynamic bindings
    (`*ns*`, `*1`, `*e`, `*out*`, ...) around the thunk itself. That is done by
    the per-session thread loop that `:exec` submits to. Since we bypass that
    loop, we have to establish and persist those bindings ourselves.

  Enable it in `plugins/witchcraft.edn`:

      :nrepl {:middleware [witchcraft-workshop.task-eval/wrap-eval]}

  Optional: put `:witchcraft/whoami \"<player name>\"` on a session's metadata
  and `wc/*default-world*` will be bound to that player's world during eval."
  (:require [lambdaisland.witchcraft :as wc]
            [nrepl.middleware :refer [set-descriptor!]]
            [nrepl.middleware.interruptible-eval :refer [*msg*]]
            [nrepl.middleware.session :as session]))

;; Private in nREPL, but it is exactly what the session loop uses to set up
;; *out*/*err*/*msg* for a message, so reuse it rather than re-implementing.
(def ^:private add-per-message-bindings @#'session/add-per-message-bindings)

(defn- eval-classloader
  "Classloader for compiling evaluated forms.

  nREPL's own choice (`nrepl.util.classloader/dynamic-classloader`) picks the
  topmost DynamicClassLoader in the chain, which sits *above* the
  lambdaisland.classpath loader holding everything from deps.edn. Parenting a
  fresh DynamicClassLoader on the server thread's context classloader keeps
  deps.edn libraries requireable from the REPL."
  []
  (clojure.lang.DynamicClassLoader. (.getContextClassLoader (Thread/currentThread))))

(defn- default-world-for [session]
  (when-not wc/*default-world*
    (when-let [whoami (:witchcraft/whoami (meta session))]
      (some-> (wc/player whoami) wc/world))))

(defn- run-on-server-thread!
  [session msg ^Runnable thunk ^Runnable ack]
  (wc/run-task
   (fn []
     (let [bindings (-> (add-per-message-bindings msg @session)
                        (assoc Compiler/LOADER (eval-classloader)))
           world    (default-world-for session)]
       (push-thread-bindings bindings)
       (try
         (if world
           (binding [wc/*default-world* world] (.run thunk))
           (.run thunk))
         ;; Persist *ns*, *1/*2/*3, *e etc. back into the session, like
         ;; nrepl.middleware.session/session-exec does.
         (swap! session (fn [current]
                          (-> (merge current (get-thread-bindings))
                              (dissoc #'*msg* Compiler/LOADER))))
         (finally
           (pop-thread-bindings)
           (.run ack)))))))

(defn scheduled-exec
  "Replacement for a session's `:exec` fn. Same two arities nREPL 1.x uses."
  [session]
  (fn this
    ([id thunk ack] (this id thunk ack *msg*))
    ([id thunk ack msg] (run-on-server-thread! session msg thunk ack))))

(defn wrap-eval
  [h]
  (fn [{:keys [session] :as msg}]
    (when (and session
               (:exec (meta session))
               (not (::decorated (meta session))))
      (alter-meta! session assoc
                   :exec (scheduled-exec session)
                   ::decorated true))
    (h msg)))

(set-descriptor! #'wrap-eval
                 {:requires #{"clone"}
                  :expects  #{"eval"}
                  :handles  {}})
