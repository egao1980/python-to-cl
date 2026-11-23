cryptography / hashlib / PyJWT
------------------------------

https://cryptography.io/

https://docs.python.org/3/library/hashlib.html

https://pyjwt.readthedocs.io/

cryptography is a package which provides cryptographic recipes and primitives to Python developers.

Features
~~~~~~~~

- Recipes: Fernet (authenticated symmetric encryption)

- Primitives: AES-GCM, ChaCha20-Poly1305, RSA, EC, Ed25519, X25519

- Key derivation: PBKDF2, scrypt, HKDF, Argon2

- X.509 certificates

- hashlib / hmac: SHA-2, SHA-3, BLAKE2; secrets: tokens, ``compare_digest``

- PyJWT: encode / decode JSON Web Tokens (HS256, RS256, ES256, EdDSA)

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `Ironclad <https://github.com/sharplispers/ironclad>`__
     - 2026-11
     - v0.61, in cl-repo. Pure Lisp: digests, HMAC, ciphers, AEAD (GCM, EAX), RSA, ECDSA, Ed25519, X25519, PBKDF2, scrypt, Argon2, CSPRNG. Low level, no recipes.
   * - `cl+ssl <https://github.com/cl-plus-ssl/cl-plus-ssl>`__
     - 2026-10
     - OpenSSL binding: TLS streams, X.509 certificate parsing and verification.
   * - `jose <https://github.com/fukamachi/jose>`__
     - 2025-02
     - JWT / JWS (HS\*, RS\*, ES\*). Imported into cl-repo but not installable (`cl-stack-systems#75 <https://github.com/egao1980/cl-stack-systems/issues/75>`__).
   * - `md5 <https://github.com/pmai/md5>`__, `sha1 <https://github.com/massung/SHA1>`__
     - 
     - Single-digest libraries, in cl-repo. Prefer Ironclad.

cl-stack
^^^^^^^^

.. list-table::
   :header-rows: 1

   * - Library
     - Version
     - Verdict
   * - `crypto-protocol <https://github.com/egao1980/crypto-protocol>`__
     - 0.2.0
     - Recipes first: ``seal`` / ``unseal`` (AES-GCM, fresh nonce, fail closed), ``digest``, ``hmac``, ``sign`` / ``verify`` (Ed25519, RSA-PSS, ECDSA P-256), incremental hashers.
   * - `crypto-backend-ironclad <https://github.com/egao1980/crypto-backend-ironclad>`__
     - 0.2.2
     - Backend for crypto and secrets, on Ironclad. RFC 8032 and Wycheproof test vectors.
   * - `secrets-protocol <https://github.com/egao1980/secrets-protocol>`__
     - 0.1.3
     - ``token-hex`` / ``token-urlsafe``, UUID v4 / v7, ``constant-time-equal``, ``hash-password`` (Argon2i, PBKDF2).
   * - `cl-stack-jwt <https://github.com/egao1980/cl-stack-jwt>`__
     - 0.3.3
     - JWT HS\*, RS256, PS256, ES256, EdDSA; tested against PyJWT.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

.. code:: common-lisp

    (asdf:load-system "ironclad")
    (asdf:load-system "babel")

Ironclad
^^^^^^^^

.. code:: python

    hashlib.sha256(b"abc").hexdigest()

.. code:: common-lisp

    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence :sha256 (babel:string-to-octets "abc")))

::

    ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad

.. code:: python

    hmac.new(b"key", b"message", "sha256").hexdigest()

.. code:: common-lisp

    (let ((mac (ironclad:make-hmac (babel:string-to-octets "key") :sha256)))
      (ironclad:update-hmac mac (babel:string-to-octets "message"))
      (ironclad:byte-array-to-hex-string (ironclad:hmac-digest mac)))

::

    6e9ef29b75fffc5b7abae527d58fdadb2fe42e7219011976917343065f58ed4a

Ed25519:

.. code:: common-lisp

    (multiple-value-bind (private public) (ironclad:generate-key-pair :ed25519)
      (let* ((message (babel:string-to-octets "hello"))
             (signature (ironclad:sign-message private message)))
        (list (length signature)
              (ironclad:verify-signature public message signature)
              (ironclad:verify-signature public (babel:string-to-octets "hellO") signature))))

::

    (64 T NIL)

PBKDF2:

.. code:: common-lisp

    (ironclad:byte-array-to-hex-string
     (ironclad:pbkdf2-hash-password (babel:string-to-octets "password")
                                    :salt (babel:string-to-octets "salt")
                                    :digest :sha256 :iterations 1))

::

    120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b

cl-stack
^^^^^^^^

https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/crypto.md

.. code:: common-lisp

    (asdf:load-system "crypto-backend-ironclad")
    (asdf:load-system "cl-stack-jwt")

    (defpackage #:crypto-demo
      (:use #:cl)
      (:local-nicknames (#:crypto #:stack-crypto)
                        (#:secrets #:stack-secrets)))
    (in-package #:crypto-demo)

Fernet-style sealed blobs:

.. code:: python

    f = Fernet(Fernet.generate_key())
    f.decrypt(f.encrypt(b"payload"))

.. code:: common-lisp

    (let* ((key (crypto:generate-key))
           (blob (crypto:seal (babel:string-to-octets "payload") :key key)))
      (list (babel:octets-to-string (crypto:unseal blob :key key))
            (handler-case (crypto:unseal blob :key (crypto:generate-key))
              (crypto:crypto-authentication-error (e) (type-of e)))))

::

    ("payload" CRYPTO:CRYPTO-AUTHENTICATION-ERROR)

Digests, HMAC, signatures:

.. code:: common-lisp

    (list (ironclad:byte-array-to-hex-string
           (crypto:digest (babel:string-to-octets "abc") :algorithm :sha256))
          (multiple-value-bind (sk pk) (crypto:generate-key-pair :ed25519)
            (let ((sig (crypto:sign #(1 2 3) :algorithm :ed25519 :key sk)))
              (crypto:verify #(1 2 3) sig :algorithm :ed25519 :key pk))))

::

    ("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" T)

Secrets and passwords:

.. code:: python

    secrets.token_hex(16), uuid.uuid4()

.. code:: common-lisp

    (list (length (secrets:token-hex 16))
          (length (princ-to-string (secrets:uuid :version :v7)))
          (let ((hash (secrets:hash-password "s3cret")))
            (list (secrets:verify-password "s3cret" hash)
                  (secrets:verify-password "guess" hash))))

::

    (32 36 (T NIL))

JWT:

.. code:: python

    token = jwt.encode({"sub": "ada"}, "secret", algorithm="HS256")
    jwt.decode(token, "secret", algorithms=["HS256"])

.. code:: common-lisp

    (let* ((key (babel:string-to-octets "secret"))
           (token (stack-jwt:encode :hs256 key '(("sub" . "ada")))))
      (list (count #\. token)
            (stack-jwt:decode :hs256 key token)))

::

    (2 (("sub" . "ada")))
