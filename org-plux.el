(defvar plx/home-blog-dir-path "/home/fred/Data/Donnees/Sites/fredtantini/")
(defvar plx/data-path "/var/www/html/own_data/fred/files/Plux/data/")
(defvar plx/articles-path (concat plx/data-path "articles/"))
(defvar plx/categories-file (concat plx/data-path "configuration/categories.xml"))
(defvar plx/users-file (concat plx/data-path "configuration/users.xml"))
(defvar plx/tags-file (concat plx/data-path "configuration/tags.xml"))

(defvar plx/default-author "Fred")
(defvar plx/default-cat "")
(defvar plx/default-draft "t")
(defvar plx/default-waiting-validation "")
(defvar plx/default-allow-com "1")
(defvar plx/default-template "article.php")
(defvar plx/default-tags "")
(defvar plx/default-meta-descr "")
(defvar plx/default-meta-keyword "")


(defun plx/get-all-categories ()
  (with-current-buffer (find-file-noselect plx/categories-file)
    (save-excursion
      (goto-char (point-min))
      (let (list-of-categories)
        (save-match-data
          (while (re-search-forward
                  "number=\"\\([0-9]+\\).*url=\"\\([^\"]+\\)\"" nil t)
            (setq list-of-categories
                  (cons (cons (match-string-no-properties 2)
                              (match-string-no-properties 1))
                        list-of-categories)))
          list-of-categories)))))

(defun plx/get-all-users ()
  (with-current-buffer (find-file-noselect plx/users-file)
    (save-excursion
      (goto-char (point-min))
      (let (list-of-categories)
        (save-match-data
          (while (re-search-forward
                  "number=\"\\([0-9]+\\).*
.*
.*name.*CDATA.\\(.*\\)..../name" nil t)
            (setq list-of-categories
                  (cons (cons (match-string-no-properties 2)
                              (match-string-no-properties 1))
                        list-of-categories)))
          list-of-categories)))))



(defun plx/sanitize (title)
  "Remove unwanted characters.

  adapted from `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'.
  "
  (let ((charMap
         [
          ["à\\|â" "a"]
          ["é\\|è\\|ê\\|ë" "e"]
          ["ï" "i"]
          ["ù\\|û\\|ü" "u"]
          ["ç" "c"]
          ["æ" "ae"]
          ["œ" "oe"]
          ["_" " "]
          ["[^[:alnum:] ]" ""]
          ["\\Ca" ""]
          [" " "-"]
          ]))
    (if title
        (mapc
         (lambda(pair)
           (setq title
                 (replace-regexp-in-string
                  (elt pair 0)
                  (elt pair 1)
                  title)))
         charMap))
    title))

(defun plx/define-xml-name (waiting-validation
                            id is-draft categories-list
                            author publication-date title)
  "define the filename of the .xml
    WAITING-VALIDATION if true, xml name begins with _
    ID article id: \"0045\"
    IS-DRAFT is draft a category
    CATEGORIES-LIST list of category ids: (\"001\" \"003\")
    AUTHOR author id: \"001\"
    PUBLICATION-DATE: yyyymmddhhmmss  
    TITLE: \"A great title!\"
    "
  (let* ((validated
          (if (and waiting-validation (not (string= waiting-validation "")))
              "_"))
         (draft
          (if (and is-draft (not (string= is-draft "")))
              "draft,"))
         (categories (mapconcat 'identity categories-list ","))
         (cat-with-draft (concat draft categories))
         (article-url (if title (downcase (plx/sanitize title)) nil))
         )
    (concat validated
            (mapconcat 'identity
                       (list id cat-with-draft author
                             publication-date article-url)
                       ".")
            ".xml")
    ))


(defun plx/get-keyword (kw)
  "return the option keyword KW's value of the buffer"
  (org-with-wide-buffer
   (goto-char (point-min))
   (if (re-search-forward (format "^[ \t]*#\\+%s:" kw) nil t)
       (org-element-property :value (org-element-at-point)))))

(defun plx/get-categories ()
  "return the id of the categories in PLX_CAT"
  (let ((cat (plx/get-keyword "PLX_CAT")))
    (if cat 
        (mapcar
         (lambda (kw)
           (cdr (assoc
                 (replace-regexp-in-string " " "" kw)
                 (plx/get-all-categories))))
         (split-string (plx/get-keyword "PLX_CAT") ","))
      '("000"))))

(defun plx/get-author ()
  "return the id of the author in AUTHOR"
  (cdr (assoc
        (plx/get-keyword "AUTHOR")
        (plx/get-all-users)))
  )

(defun plx/get-article-id()
  "return the last id+1 from the articles folder"
  (let ((article-id (plx/get-keyword "PLX_ARTICLE_ID")))
    (if (and article-id (not (string= article-id ""))) article-id
      (format "%04d"
              (+ 1 (string-to-number
                    (substring
                     (car
                      (split-string
                       (car
                        (last
                         (directory-files plx/articles-path)))
                       "\\\.")) -4)))))))

(plx/get-article-id)

(defun plx/get-date ()
  (let* (
         (kw-date (plx/get-keyword "DATE"))
         (parts (if kw-date (split-string (substring kw-date 1 -1))))
         (ymd (if parts (car parts)))
         (hms (if parts (car (last parts))))
         )
    (if parts 
        (concat (replace-regexp-in-string "-" "" ymd) (substring (replace-regexp-in-string ":" "" hms) 0 4))
      )
    )
  )


(defun plx/get-template ()
  (let ((tmp  (plx/get-keyword "PLX_TEMPLATE")))
    (if tmp tmp "artiiiicle.php"))
  )

(plx/get-template)


(defun plx/new-post (title)
  "open a new file in YEAR/MONTH/YearMonthDay_title_with_underscore.org"
  (interactive "sTitre: ")
  (let* ((tmptitle (plx/sanitize title))
         (year (format-time-string "%Y"))
         (month (format-time-string "%m"))
         (day (format-time-string "%d"))
         (hour (format-time-string "%H"))
         (min (format-time-string "%M"))
         (sec (format-time-string "%S"))
         (newtitle (downcase (concat year month day "_" tmptitle ".org")))
         (newdir (concat plx/home-blog-dir-path year "/" month "/"))
         )
    (make-directory newdir t)
    (find-file (concat newdir newtitle))
    (insert (concat "#+TITLE: " title "\n"))
    (insert (concat "#+DATE: <" year "-" month "-" day " " hour ":" min ":" sec ">\n"))
    (insert (concat "#+AUTHOR: " plx/default-author "\n"
                    "#+PLX_CAT: " plx/default-cat "\n"
                    "#+PLX_DRAFT: " plx/default-draft "\n"
                    "#+PLX_WAITING_VALIDATION: " plx/default-waiting-validation "\n"
                    "#+PLX_ALLOW_COM: " plx/default-allow-com "\n"
                    "#+PLX_TEMPLATE: " plx/default-template "\n"
                    "#+PLX_TAGS: " plx/default-tags "\n"
                    "#+PLX_META_DESCR: " plx/default-meta-descr "\n"
                    "#+PLX_META_KEYWORDS: " plx/default-meta-keyword "\n"
                    "#+PLX_ARTICLE_ID: " (plx/get-article-id) "\n"


                    ))))


(defun plx/write-xml (xml-name title allow-com template tags
                               meta-descr meta-keywords from-subtree)
  "Write the xml file for pluxml."

  (with-current-buffer (org-html-export-as-html nil from-subtree nil 't)
    (kill-new (buffer-string))
    (kill-buffer)
    )
  (with-current-buffer (find-file (concat plx/articles-path xml-name))
    (erase-buffer) ;if actually we are updating
    (insert
     (concat
      "<?xml version='1.0' encoding='UTF-8' ?>\n"
      "<document>\n"
      "       <title><![CDATA[" title  "]]></title>\n"
      "       <allow_com>" allow-com "</allow_com>\n"
      "       <template><![CDATA[" template "]]></template>\n"
      "       <chapo><![CDATA[]]></chapo>\n"
      "       <content><![CDATA[" ))
    (yank)
    (insert (concat "]]></content>\n"
                    "       <tags><![CDATA[" tags "]]></tags>\n"
                    "       <meta_description><![CDATA[" meta-descr "]]></meta_description>\n"
                    "       <meta_keywords><![CDATA[" meta-keywords "]]></meta_keywords>\n"
                    "       <title_htmltag><![CDATA[]]></title_htmltag>\n"
                    "</document>\n"
                    ))
    (save-buffer)
    )
  )

(defun plx/update-tags-file (article-id date is-active tags)
  "Update tags.xml"
  (with-current-buffer (find-file-noselect plx/tags-file)
    (goto-char (point-min))
    (if (search-forward-regexp (concat "number=\\\"" article-id "\\\"") nil t)
        ;;id exists, we kill the line
        (kill-region (progn (forward-visible-line 0) (point)) (progn (forward-line) (point)))
      ;;id does not exist, we go to </document> line
      (search-forward-regexp "/document>")
      (forward-visible-line 0)
      )
    (insert (concat "\t<article number=\""
                    article-id
                    "\" date=\""
                    date
                    "\" active=\""
                    (if is-active "1" "0")
                    "\"><![CDATA["
                    tags
                    "]]></article>\n"
                    ))
    (save-buffer)
    ))


(defun plx/create-article ()
  "Write the .xml and update tags.xml"
  (interactive)
  (let* (
         (article-id (plx/get-article-id))
         (waiting-validation (plx/get-keyword "PLX_WAITING_VALIDATION"))
         (is-draft (plx/get-keyword "PLX_DRAFT"))
         (date (plx/get-date))
         (title (plx/get-keyword "TITLE"))
         (xml-name (plx/define-xml-name 
                    waiting-validation
                    article-id
                    is-draft  
                    (plx/get-categories)
                    (plx/get-author)
                    date
                    title))
         (allow-com (plx/get-keyword "PLX_ALLOW_COM"))
         (template (plx/get-template))
         (tags (plx/get-keyword "PLX_TAGS"))
         (meta-descr (plx/get-keyword "PLX_META_DESCR"))
         (meta-keywords (plx/get-keyword "PLX_META_KEYWORDS"))
         (is-active (and
                     (or (not waiting-validation) (string= waiting-validation ""))
                     (or (not is-draft) (string= is-draft ""))))
         )
    
       (let ((files-with-same-id (directory-files plx/articles-path
       nil (concat "^" (plx/get-article-id)))))
         (if files-with-same-id
           (if (y-or-n-p (concat "Delete existing files starting with id "
           (plx/get-article-id)"?"))
             (dolist (same-id-file files-with-same-id)
             (delete-file (concat plx/articles-path same-id-file))
             ))))

       
    (plx/write-xml xml-name title allow-com template tags
                   meta-descr meta-keywords nil)
    (plx/update-tags-file article-id date is-active tags)
    ))

(defun plx/get-infos-from-subtree ()
  (let* ((title (nth 4 (org-heading-components)))
         (tmp-tags (nth 5 (org-heading-components)))
         (tags (if tmp-tags
                   (replace-regexp-in-string
                    ":" ", " (substring tmp-tags 1 -1))))
         )
    (save-excursion
      (outline-up-heading 1)
      (let ((category (nth 4 (org-heading-components))))
        (list title tags category)
        )))
  )


(defun plx/create-article-from-subtree ()
  "Write the .xml and update tags.xml from a category subtree"
  (interactive)
  (let* (
         (title (nth 4 (org-heading-components)))
         (tags (nth 5 (org-heading-components)))
         (year (format-time-string "%Y"))
         (month (format-time-string "%m"))
         (day (format-time-string "%d"))
         (hour (format-time-string "%H"))
         (min (format-time-string "%M"))
         (date (concat year month day hour min))
         (article-id (plx/get-article-id))
         (waiting-validation nil)
         (is-draft t)
         (infos (plx/get-infos-from-subtree))
         (title (nth 0 infos))
         (tags (nth 1 infos))
         (category  (cdr (assoc
                          (replace-regexp-in-string " " "" (nth 2 infos))
                          (plx/get-all-categories))))
         (author (cdr (assoc
                       plx/default-author
                       (plx/get-all-users))))
         (xml-name (plx/define-xml-name 
                    waiting-validation
                    article-id
                    is-draft  
                    (list category)
                    author
                    date
                    title))
         (allow-com plx/default-allow-com)
         (template plx/default-template)
         (meta-descr plx/default-meta-descr)
         (meta-keywords plx/default-meta-keyword)
         (is-active t)
         )       
    (plx/write-xml xml-name title allow-com template tags
                   meta-descr meta-keywords t)
    (plx/update-tags-file article-id date is-active tags)
    ))


(provide 'org-plux)

(global-set-key (kbd "C-c x") nil)
(global-set-key (kbd "C-c x a") 'plx/create-article)
(global-set-key (kbd "C-c x s") 'plx/create-article-from-subtree)
(global-set-key (kbd "C-c x n") 'plx/new-post)

;TODO
;delete buffer?
;article inline: default values using drawers

