;;; ============================================================== [ maybe.clj ]

(ns cats.frege.maybe
  (:require [cats
             [context :as ctx]
             [protocols :as p]
             [util :as util]])
  (:import (frege.prelude
            Maybe
            PreludeBase$TMaybe
            PreludeBase$TMaybe$DJust
            PreludeBase$TMaybe$DNothing)
           (frege.run8 Thunk)))

(declare context)

(extend-type PreludeBase$TMaybe$DJust
  p/Contextual
  (-get-context [_]
    context)

  p/Extract
  (-extract [this]
    (Maybe/unJust this))

  p/Printable
  (-repr [this]
    (str "#<Just " (pr-str (p/-extract this)) ">")))

(extend-type PreludeBase$TMaybe$DNothing
  p/Contextual
  (-get-context [_]
    context)

  p/Extract
  (-extract [_]
    nil)

  p/Printable
  (-repr [_]
    "#<Nothing>"))

(util/make-printable PreludeBase$TMaybe$DJust)
(util/make-printable PreludeBase$TMaybe$DNothing)

;;; =========================================================== [ Constructors ]

(defn just [v]
  {:pre [(some? v)]}
  (PreludeBase$TMaybe$DJust/mk (Thunk/lazy v)))

(defn nothing []
  (PreludeBase$TMaybe$DNothing/mk))

;;; ============================================================= [ Predicates ]

(defn just? [^PreludeBase$TMaybe v]
  (Maybe/isJust v))

(defn nothing? [^PreludeBase$TMaybe v]
  (or (nil? v)
      (Maybe/isNothing v)))

(defn maybe? [v]
  ;; (or (just? v)
  ;;     (nothing? v)
  ;;     (and (satisfies? p/Contextual v)
  ;;          (identical? (p/-get-context v) context)))
  (instance? PreludeBase$TMaybe v))

;;; =================================================== [ Data.Maybe.fromMaybe ]

(defn from-maybe
  ([^PreludeBase$TMaybe mv]
   (from-maybe mv nil))
  ([^PreludeBase$TMaybe mv default]
   (Maybe/fromMaybe (Thunk/lazy default) mv)))

;;; ===================================================== [ Context Definition ]

(def context
  (reify
    p/Context

    p/Semigroup
    (-mappend [ctx mv mv']
      (cond
        (nothing? mv) mv'
        (nothing? mv') mv
        :else (just (let [mv (p/-extract mv)
                          mv' (p/-extract mv')]
                      (p/-mappend (p/-get-context mv) mv mv')))))
    p/Monoid
    (-mempty [_]
      (nothing))

    p/Functor
    (-fmap [_ f mv]
      (if (nothing? mv)
        mv
        (just (f (p/-extract mv)))))

    p/Applicative
    (-pure [_ v]
      (just v))
    (-fapply [m af av]
      (if (nothing? af)
        af
        (p/-fmap m (p/-extract af) av)))

    p/Monad
    (-mreturn [_ v]
      (if (nil? v)
        (nothing)
        (just v)))
    (-mbind [_ mv f]
      (assert (maybe? mv)
              (format "Context mismatch: %s is not a Frege Maybe."
                      (p/-repr mv)))
      (if (nothing? mv)
        mv
        (f (p/-extract mv))))

    p/MonadZero
    (-mzero [_]
      (nothing))

    p/MonadPlus
    (-mplus [_ mv mv']
      (if (just? mv)
        mv
        mv'))

    p/Foldable
    (-foldl [_ f z mv]
      (if (just? mv)
        (f z (p/-extract mv))
        z))

    (-foldr [_ f z mv]
      (if (just? mv)
        (f (p/-extract mv) z)
        z))

    p/Traversable
    (-traverse [_ f mv]
      (if (just? mv)
        (let [a (f (p/-extract mv))]
          (p/-fmap (p/-get-context a) just a))
        (p/-pure (ctx/infer) mv)))

    p/Printable
    (-repr [_]
      "#<Maybe>")))

(util/make-printable (type context))

;;; ====================================================== [ Utility Functions ]

(defn maybe [default ^PreludeBase$TMaybe mv f]
  (if (nothing? mv)
    default
    (f (p/-extract mv))))

(defn seq->maybe [^PreludeBase$TMaybe mv]
  (if (nothing? mv)
    (lazy-seq)
    (lazy-seq [(p/-extract mv)])))

(defn cat-maybes [coll]
  (-> (comp (filter just?) (map p/-extract))
      (sequence coll)))

(defn map-maybe [mf coll]
  (cat-maybes (map mf coll)))

;;; ==================================================================== [ EOF ]
