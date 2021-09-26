(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

(defconst packages '(evil transient))
(dolist (pkg packages)
      (package-install pkg))
