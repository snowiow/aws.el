(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(defconst packages '(evil transient))
(dolist (pkg packages)
      (package-install pkg))
