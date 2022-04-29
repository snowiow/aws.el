(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(defconst packages '(evil))
(dolist (pkg packages)
      (package-install pkg))
