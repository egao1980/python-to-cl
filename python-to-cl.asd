;;;; Not a library: the site's build dependencies, so cl-repo installs cl-blog
;;;; and its dependency closure for bin/build and the site workflow. (Before
;;;; cl-repo 0.27.35 a qlfile alone got cl-blog without its dependencies:
;;;; egao1980/cl-repository#170.)
(defsystem "python-to-cl"
  :description "Build dependencies of the python-to-cl site (bin/build); not a library"
  :author "Nikolai Matiushev"
  :license "MIT"
  :depends-on ((:version "cl-blog" "1.1.0")))
