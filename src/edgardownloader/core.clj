(ns edgardownloader.core
  (use [clojure.contrib.io :as io :include (to-byte-array input-string as-url) :exclude [spit make-parents]])
  (:require  [clojure.xml :as xml]
             [clojure.zip :as zip]
             [clojure.contrib.string]
             [clojure.contrib.zip-filter.xml :as zf]
             [clojure.java.io]
             )
  (:import [java.io File FileOutputStream])
  (:import [java.util.zip ZipFile]))


;; Definitions
(def *edgar-rss-url* "http://www.sec.gov/Archives/edgar/xbrlrss.all.xml")
(def *download-dir* "downloads")


(defn get-commodity-xls-files []
  (re-seq #"[a-z0-9-]+\.xls" (slurp "http://minerals.usgs.gov/ds/2005/140/")) )

(defn file-exists [filename]
  (let [f (File. filename)]
    (.isFile f)))


(defn download-binary [from to]
  (with-open [out (FileOutputStream. to)]
    (.write out
            (io/to-byte-array
             (io/input-stream
              (io/as-url from))))))

(defn download-commodities []
  (doseq [fname (get-commodity-xls-files)]
    (when-not (file-exists (str "commodities-xls/" fname))
      (download-binary (str "http://minerals.usgs.gov/ds/2005/140/" fname) (str "commodities-xls/" fname)))))



(defn get-edgar-rss []
  (slurp "http://www.sec.gov/Archives/edgar/xbrlrss.all.xml"))

(defn parse-items [xml-file]
  (let [xml-parse (xml/parse xml-file)
        ;; attrs (:attrs xml-parse)
        ;; year (:year attrs)
        ;; month (:month attrs)
        ;; day (:day attrs)
        ]
    (let [zipper (zip/xml-zip xml-parse)]
      (zf/xml-> zipper :item))))



;; (def feed (xml-seq (xml/parse (java.io.File. "rss.txt"))))
;; (def rss-entries (filter #(= :item (:tag %)) feed))


;; {:tag :edgar:formType, :attrs nil, :content ["10-Q"]}

;; Parse the "All XBRL Data Submitted to the SEC" link http://www.sec.gov/Archives/edgar/xbrlrss.all.xml
;; This is a list of up to 100 of the latest filings containing XBRL, updated every 10 minutes.


;; Figure out which entries are 10-Q files

;; Download the 'Media Files' zip file and unzip it


;; :tag rss
;;    :content
;;       :tag channel
;;          :content
;;              8 items [title, link, atom, description, language, pubDate, item[n]
;;              
;; (:content (first (:content (first (xml-seq (xml/parse "rss2.txt"))))))

;; (def items (filter #(= :item (:tag %)) (:content (first (:content (first (xml-seq (xml/parse "rss2.txt"))))))))


;; rss
;;   channel
;;      item
;;         title
;;         enclosure
;;         edgar:xbrlFiling
;;             edgar:formType == 10-Q
;;             edgar:cikNumber

(defn get-items [url]
  (filter #(= :item (:tag %)) (:content (first (:content (first (xml-seq (xml/parse url))))))))

(defn- get-value [node]
  (first (:content node)))
 
(defn- get-attr [node]
  (:attrs node))

(defn- enclosure [entry]
  (get-attr (first (filter #(= :enclosure (:tag %)) (:content entry)))))

(defn- item-node [entry key]
  (get-value (first (filter #(= (keyword key) (:tag %))
                            (:content entry)))))
(defn- xbrlFiling [entry key]
  (get-value (first (filter #(= (keyword key) (:tag %)) (:content (first (filter #(= :edgar:xbrlFiling (:tag %)) (:content entry))))))))


(defn title [entry]
  (item-node entry "title"))

(defn link [entry]
  (item-node entry "link"))

(defn url [entry]
  (:url (enclosure entry)))

(defn url-filename [entry]
  (last (clojure.contrib.string/split #"/" (url entry))))

(defn formType [entry]
  (xbrlFiling entry "edgar:formType"))

(defn companyName [entry]
  (xbrlFiling entry "edgar:companyName"))

(defn fileDate [entry]
  (xbrlFiling entry "edgar:filingDate"))

(defn cikNumber [entry]
  (xbrlFiling entry "edgar:cikNumber"))
                       

(defn get-ten-qs [items]
  (filter #(= "10-Q" (formType %)) items))


                       
;; for each item, store it's zip file in the directory cikNumber

;; for each item that is a 10-Q
;;   get it's cikNumber and see if directory exists
;;   if not create directory
;;     get url
;;     get filename
;;     download file
;;     create text file (title, company, pubdate, etc)
;;     save file

(defn download-file [from to]
  (spit (slurp from)))             

;; http://stackoverflow.com/questions/5419125/using-java-api-from-clojure-reading-zip-file
(defn entries [zipfile]
  (enumeration-seq (.entries zipfile)))

(defn walkzip [fileName]
  (with-open [z (java.util.zip.ZipFile. fileName)]
             (doseq [e (entries z)]
                    (println (.getName e)))))

(defn filenames-in-zip [filename]
  (let [z (java.util.zip.ZipFile. filename)] 
    (map #(.getName %) (enumeration-seq (.entries z)))))

(defn- save-entry [entry]
  (with-open [out (FileOutputStream. (.getName entry))]
     (.write out
            (io/to-byte-array
             (.getInputStream entry)))))
             
(defn unzip-zipfile [filename]
  (let [z (java.util.zip.ZipFile. filename)]
    (map #(save-entry %) (enumeration-seq (.entries z)))))

;; http://www.assembla.com/spaces/clojure-contrib/documents/d2a-EiTi0r3Oz7eJe5afGb
(defn unzip
  "Unzip zipfile into the target directory."
  [zipfile target]
  (with-open [zipfile (ZipFile. zipfile)]
    (let [entries (enumeration-seq (.entries zipfile))]
      (doseq [file (remove (memfn isDirectory) entries)]
        (.mkdirs (.getParentFile (java.io.File. (str target (.getName file)))))
        (with-open [out (FileOutputStream. (str target (.getName file)))]
          (copy (.getInputStream zipfile file) out))))))

(defn save-item [item]
  (let [t (title item)
        l (link item)
        c (companyName item)
        d (fileDate item)
        n (cikNumber item)
        dir (str *download-dir* "/" n "/" d "/")
        zip-filename (str dir (url-filename item))
        ]
    (println dir)
    (println zip-filename)
    
    ;; create directory
    (clojure.java.io/make-parents zip-filename)
    ;; save zip
    (download-binary (url item) zip-filename)
    ;; create/save text file
    (let [info-details (str
                        t "\n"
                        c "\n"
                        d "\n"
                        n "\n"
                        l "\n")
          text-filename (str dir n ".txt")
          ]
      (spit text-filename info-details))
    (doall (map #(println %) (filenames-in-zip zip-filename)))
    (unzip zip-filename dir)
    ))

(defn download-ten-qs []
  (let [items (get-ten-qs (get-items *edgar-rss-url*))]
    (doseq [item items]
      (download-binary (url-filename item) (url item)))))

    
(defn download-ten-qs-file [file]
  (let [items (get-ten-qs (get-items file))]
    (doseq [item items]
      (save-item item))))
  


;; download-dir


