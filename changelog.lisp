(defpackage #:docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package docs/changelog)


(defchangelog ()
  (1.3.2 2022-11-11
         "Fixed how does .qlot get updated. Now we are running `qlot update --no-deps`.")
  (1.3.1 2022-08-01
         "Don't push docs to the github if last commit is last documentation update
        (has commit message is \"Update docs\").

         This way you can set up a scheduled execution for the workflow, and
         it will not fill git repository with endless updates.")
  (1.3.0 2021-10-19
         "Don't upload docs when action is used on pull-request.")
  (1.2.0 2021-05-10
         "Now action will check if `log4cl-extras`, `docs-builder` or `ngrok`
          are present in `qlfile`.")
  (1.1.0 2021-04-15
         "Now builder will be runned in the mode when any warning
          is considered as an error. To prevent this behaviour,
          pass `error-on-warnings: false` argument. ")
  (1.0.7 2021-04-05
         "Install `log4cl-extras` because docs-builder now depends on it
          and this project is not in the Quicklisp yet.")
  (1.0.6 2021-04-05
         "Switched to the 40ANTS-DOC for its own documentation.")
  (1.0.5 2021-03-08
         "Fixed the case when during the docs building
          some file in the `.github/workflows` has changed.")
  (1.0.4 2021-02-28
         "Fixed update of the qlfile in case there is no new line at the end.")
  (1.0.3 2021-02-24
         "Fixed utilities running when cache is enabled.
          We need to call them under the qlot.")
  (1.0.2 2021-02-16
         "Yet another fix of pushing changes into the current branch on push event.")
  (1.0.1 2021-02-16
         "Fixed error when we are uploading local changes on \"push event\".")
  (1.0.0 2021-01-07
         "Initial version."))
