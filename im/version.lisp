(in-package #:im)

(export '(version version-number version-date))

(defalias version #'im-cffi::%im-version)
(defalias version-date #'im-cffi::%im-version-date)
(defalias version-number #'im-cffi::%im-version-number)
