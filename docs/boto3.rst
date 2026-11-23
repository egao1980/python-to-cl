boto3
-----

https://boto3.amazonaws.com/v1/documentation/api/latest/index.html

Boto3 is the Amazon Web Services (AWS) Software Development Kit (SDK) for Python.

Features
~~~~~~~~

- Clients for every AWS service, generated from botocore models

- Credential chain: environment, config files, SSO, instance metadata

- Paginators and waiters

- S3: managed multipart transfers, presigned URLs

- Works against S3-compatible stores (MinIO, R2) via ``endpoint_url``

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

Only S3 has usable libraries. The 2024 survey lists "an improved AWS SDK" among wanted libraries.

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `ZS3 <https://github.com/xach/zs3>`__
     - 2025-03
     - release-1.3.4. S3 buckets and objects, ACLs, presigned URLs, CloudFront. Mature, small. Not in cl-repo.
   * - `aws-sdk-lisp <https://github.com/fukamachi/aws-sdk-lisp>`__
     - 2025-07
     - One system per service, generated from botocore. Alpha, per its README. Not in cl-repo.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `object-store-protocol <https://github.com/egao1980/object-store-protocol>`__
     - 0.1.0
     - S3-shaped API: ``put-object`` / ``get-object`` / ``head-object`` / ``list-objects``, conditional puts, presigned URLs, multipart. In-memory store for tests.
   * - `object-store-backend-s3 <https://github.com/egao1980/object-store-backend-s3>`__
     - 0.1.2
     - AWS SigV4 S3 / MinIO backend. You pass the HTTP function; no credential chain.

No other AWS services.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

.. code:: python

    s3 = boto3.client("s3")
    s3.put_object(Bucket="my-bucket", Key="a/b.txt", Body=b"hello")
    s3.get_object(Bucket="my-bucket", Key="a/b.txt")["Body"].read()

ZS3
^^^

Not in cl-repo. From its documentation, not run:

.. code:: common-lisp

    (setf zs3:*credentials* (zs3:file-credentials "~/.aws-keys"))
    (zs3:put-string "hello" "my-bucket" "a/b.txt")
    (zs3:get-string "my-bucket" "a/b.txt")
    (zs3:authorized-url :bucket "my-bucket" :key "a/b.txt" :expires (+ (get-universal-time) 3600))

::

    not run: ZS3 is not in cl-repo

cl-stack
^^^^^^^^

.. code:: common-lisp

    (asdf:load-system "object-store-backend-s3")

    (defpackage #:boto3-demo
      (:use #:cl)
      (:local-nicknames (#:os #:stack-object-store)))
    (in-package #:boto3-demo)

The protocol against the in-memory store:

.. code:: common-lisp

    (let ((store (os:make-in-memory-object-store)))
      (os:put-object store "a/b.txt" "hello")
      (os:put-object store "a/c.txt" "world")
      (list (babel:octets-to-string (os:get-object store "a/b.txt"))
            (handler-case (os:get-object store "missing")
              (os:object-not-found (e) (type-of e)))))

::

    ("hello" OS:OBJECT-NOT-FOUND)

The S3 backend signs requests locally. A presigned URL needs no network (keys from the AWS documentation):

.. code:: python

    s3.generate_presigned_url("get_object", Params={"Bucket": "examplebucket", "Key": "test.txt"}, ExpiresIn=86400)

.. code:: common-lisp

    (let ((s3 (object-store-backend-s3:make-s3-backend
               :endpoint "https://s3.amazonaws.com"
               :region "us-east-1"
               :bucket "examplebucket"
               :access-key "AKIAIOSFODNN7EXAMPLE"
               :secret-key "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY")))
      (os:presign-url s3 "test.txt" :expires 86400))

::

    https://s3.amazonaws.com/examplebucket/test.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIOSFODNN7EXAMPL...

Real requests go through an HTTP function you pass, e.g. cl-stack-http against MinIO (not run):

.. code:: common-lisp

    (let ((s3 (object-store-backend-s3:make-s3-backend
               :endpoint "http://127.0.0.1:9000" :region "us-east-1" :bucket "my-bucket"
               :access-key (uiop:getenv "AWS_ACCESS_KEY_ID")
               :secret-key (uiop:getenv "AWS_SECRET_ACCESS_KEY")
               :http-fn (lambda (request)
                          (http-protocol:send (cl-stack-http:ensure-http-backend)
                                              (http-protocol:make-http-client
                                               (cl-stack-http:ensure-http-backend))
                                              request)))))
      (os:put-object s3 "a/b.txt" "hello")
      (os:get-object s3 "a/b.txt"))

::

    not run: needs an S3 / MinIO endpoint
