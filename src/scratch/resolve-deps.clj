(defn resolve-spec-deps [scope name resolved unresolved]
  (let [[rule]
        [(get scope name nil)]
       ]
    (if (nil? rule) (throw (Exception. (str name " is not defined in the table..."))))
    (if (is-dep-edge rule (union resolved (set [name])))
        (do
          (print-node rule)
          (union resolved (set [name])))
        (let [[c] [(is-dep-circular rule unresolved)]]
          (if (nil? c)
            (recur scope
                   name
                   ; recurse and try to solve deps down there...
                   (try
                     (apply union (map
                            #(resolve-spec-deps scope % resolved
                                  ; this rule is unresolved for the intents of
                                  ; the recursion...
                                  (union unresolved (set [name])))
                            (difference (. rule dependencies) resolved)))
                     (catch Exception e (println (str "Caught exception in the dependencies of " name ":\n    " (.getMessage e)))))
                 unresolved)
            (throw (Exception. (str "Circular dependency in " name "\n    unmet:" c)))
          )
        )
      )
  )
)
