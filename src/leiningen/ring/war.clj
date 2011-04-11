(ns leiningen.ring.war
  (:require [leiningen.compile :as compile]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:use [clojure.contrib.prxml :only (prxml)]
        [clojure.walk :only (postwalk)])
  (:import [java.util.jar Manifest
                          JarEntry
                          JarOutputStream]
           [java.io BufferedOutputStream 
                    FileOutputStream 
                    ByteArrayInputStream]))

(defn default-war-name [project]
  (or (get-in project [:ring :war-name])
      (str (:name project) "-" (:version project) ".war")))

(defn war-file-path [project war-name]
  (let [target-dir (:target-dir project)]
    (.mkdirs (io/file target-dir))
    (str target-dir "/" war-name)))

(defn skip-file? [project war-path file]
  (or (re-find #"^\.?#" (.getName file))
      (re-find #"~$" (.getName file))
      (some #(re-find % war-path)
            (get-in project [:ring :war-exclusions]))))

(defn- to-byte-stream [^String s]
  (ByteArrayInputStream. (.getBytes s)))

(defn make-manifest []
  (Manifest.
   (to-byte-stream
    (str 
     "Manifest-Version: 1.0\n"
     "Created-By: Leiningen Ring Plugin\n"
     "Built-By: " (System/getProperty "user.name") "\n"
     "Build-Jdk: " (System/getProperty "java.version") "\n\n"))))

(defn default-servlet-class [project]
  (let [handler-sym (get-in project [:ring :handler])
        ns-parts    (-> (namespace handler-sym)
                        (string/replace "-" "_")
                        (string/split #"\.")
                        (butlast)
                        (vec)
                        (conj "servlet"))]
    (string/join "." ns-parts)))

(defn servlet-class [project]
  (or (get-in project [:ring :servlet-class])
      (default-servlet-class project)))

(defn servlet-ns [project]
  (-> (servlet-class project)
      (string/replace "_" "-")))

(defn servlet-name [project]
  (or (get-in project [:ring :servlet-name])
      (str (get-in project [:ring :handler])
           " servlet")))

(defn url-pattern [project]
  (or (get-in project [:ring :url-pattern])
      "/*"))

(defn- make-filter [{:keys [name class]}]
  [:filter
   [:filter-name name]
   [:filter-class class]])

(defn- make-filter-mapping [{:keys [name url-pattern]}]
  [:filter-mapping
   [:filter-name name]
   [:url-pattern url-pattern]])

(defn- make-listener [class-name]
  [:listener
   [:listener-class class-name]])

(defn- make-servlet [servlet]
  [:servlet
   [:servlet-name (:name servlet)]
   [:servlet-class (:class servlet)]
   (when-let [load-on-startup (:load-on-startup servlet)]
     [:load-on-startup load-on-startup])])

(defn- make-servlet-mapping [{:keys [name url-pattern]}]
  [:servlet-mapping
   [:servlet-name name]
   [:url-pattern url-pattern]])

(defn- make-resource-ref [{:keys [name type auth scope]}]
  [:resource-ref
   [:res-ref-name name]
   [:res-type type]
   [:res-auth (or auth "Container")]
   [:res-sharing-scope (or scope "Shareable")]])

(defn- make-web-dom [project]
  (let [webxml (get-in project [:ring :webxml])]
    (postwalk
     #(if (vector? %) (vec (filter (comp not nil?) %)) %)
     (vec (concat
	   [:web-app]
	   ;; filters
	   (let [res (map make-filter (:filters webxml))]
	     (when-not (empty? res) res))
	   ;; filter-mappings
	   (let [res (map make-filter-mapping (:filter-mappings webxml))]
	     (when-not (empty? res) res))
	   ;; listeners
	   (let [res (map make-listener (:listeners webxml))]
	     (when-not (empty? res) res))
	   ;; servlets
	   (map make-servlet
		(conj (:servlets webxml)
		      {:name (servlet-name project)
		       :class (servlet-class project)}))
	   ;; servlet-mappings
	   (map make-servlet-mapping
		(conj (:servlet-mappings webxml)
		      {:name (servlet-name project)
		       :url-pattern (url-pattern project)}))
	   ;; resource-refs
	   (let [res (map make-resource-ref (:resource-refs webxml))]
	     (when-not (empty? res) res)))))))

(defn make-web-xml [project]
  (-> (make-web-dom project)
      (prxml)
      (with-out-str)))

(defn source-file [project namespace]
  (io/file (:compile-path project)
           (-> (str namespace)
               (string/replace "-" "_")
               (string/replace "." java.io.File/separator)
               (str ".clj"))))

(defn compile-form [project namespace form]
  (let [out-file (source-file project namespace)]
    (.mkdirs (.getParentFile out-file))
    (with-open [out (io/writer out-file)]
      (binding [*out* out] (prn form))))
  (compile/eval-in-project project
    `(do (clojure.core/compile '~namespace) nil)))

(defn generate-handler [project handler-sym]
  (if (get-in project [:ring :servlet-path-info?] true)
    `(fn [request#]
       (~handler-sym
         (assoc request#
           :path-info (.getPathInfo (:servlet-request request#))
           :context   (.getContextPath (:servlet-request request#)))))
    handler-sym))

(defn compile-servlet [project]
  (let [handler-sym (get-in project [:ring :handler])
        handler-ns  (symbol (namespace handler-sym))
        servlet-ns  (symbol (servlet-ns project))]
    (compile-form project servlet-ns
      `(do (ns ~servlet-ns
             (:require ring.util.servlet ~handler-ns)
             (:gen-class :extends javax.servlet.http.HttpServlet))
           (ring.util.servlet/defservice
             ~(generate-handler project handler-sym))))))

(defn create-war [project file-path]
  (-> (FileOutputStream. file-path)
      (BufferedOutputStream.)
      (JarOutputStream. (make-manifest))))

(defn write-entry [war war-path entry]
  (.putNextEntry war (JarEntry. war-path))
  (io/copy entry war))

(defn str-entry [war war-path content]
  (write-entry war war-path (to-byte-stream content)))

(defn in-war-path [war-path root file]
  (str war-path
       (-> (.toURI (io/file root)) 
           (.relativize (.toURI file))
           (.getPath))))

(defn file-entry [war project war-path file]
  (when (and (.exists file)
             (.isFile file)
             (not (skip-file? project war-path file)))
    (write-entry war war-path file)))

(defn dir-entry [war project war-root dir-path]
  (doseq [file (file-seq (io/file dir-path))]
    (let [war-path (in-war-path war-root dir-path file)]
      (file-entry war project war-path file))))

(defn war-resources-path [project]
  (:war-resources-path project "war-resources"))

(defn write-war [project war-path]
  (with-open [war-stream (create-war project war-path)]
    (doto war-stream
      (str-entry "WEB-INF/web.xml" (make-web-xml project))
      (dir-entry project "WEB-INF/classes/" (:compile-path project))
      (dir-entry project "WEB-INF/classes/" (:source-path project))
      (dir-entry project "WEB-INF/classes/" (:resources-path project))
      (dir-entry project "" (war-resources-path project)))))

(defn war
  "Create a $PROJECT-$VERSION.war file suitable for use in servlet containers."
  ([project]
     (war project (default-war-name project)))
  ([project war-name]
     (binding [compile/*silently* true]
       (when (zero? (compile/compile project))
         (let [war-path (war-file-path project war-name)]
           (compile-servlet project)
           (write-war project war-path)
           (println "Created" war-path)
           war-path)))))
