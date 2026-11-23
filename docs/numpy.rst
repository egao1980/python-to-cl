NumPy
-----

https://numpy.org/doc/stable/

NumPy is the fundamental package for scientific computing in Python.

Features
~~~~~~~~

- ``ndarray``: n-dimensional typed arrays, views, slicing

- Broadcasting and vectorized universal functions

- Linear algebra (BLAS / LAPACK), FFT, random

- dtypes, structured arrays, memory-mapped files

- C API, the base of SciPy, pandas, scikit-learn

Common Lisp alternatives
~~~~~~~~~~~~~~~~~~~~~~~~

CL arrays are n-dimensional with specialized element types (``double-float``, ``(unsigned-byte 8)``) and displaced views. There is no vectorized arithmetic in the standard.

.. list-table::
   :header-rows: 1

   * - Library
     - Last commit
     - Verdict
   * - `MAGICL <https://github.com/quil-lang/magicl>`__
     - 2026-06
     - v0.11.0. Matrices and tensors; BLAS / LAPACK through CFFI with Lisp fallbacks. Used in production (quilc). Not in cl-repo.
   * - `numericals <https://github.com/digikar99/numericals>`__
     - 2025-06
     - 2024.12.0. NumPy-like API, SIMD (BMAS) and BLAS backends, broadcasting. Not in cl-repo.
   * - `Petalisp <https://github.com/marcoheisig/Petalisp>`__
     - 2025-06
     - Lazy array programs compiled and run in parallel. Research-grade. Not in cl-repo.
   * - `array-operations <https://github.com/Lisp-Stat/array-operations>`__
     - 2026-03
     - 1.2.1. ``generate``, ``each``, ``reshape``, ``stack``, slices on plain CL arrays. Part of Lisp-Stat. Not in cl-repo.
   * - `LLA <https://github.com/Lisp-Stat/lla>`__
     - 2026-11
     - Linear algebra over BLAS / LAPACK, from Lisp-Stat. Not in cl-repo.
   * - `NumCL <https://github.com/numcl/numcl>`__
     - 2022-10
     - Pure CL NumPy clone. Dormant. Not in cl-repo.

The 2023–2024 review lists numericals, Petalisp and cl-waffe2 (tensors for deep learning, experimental).

cl-stack
^^^^^^^^

No array library. `arrow-protocol <https://github.com/egao1980/arrow-protocol>`__ (0.1.2) reads and writes Arrow IPC and Parquet columns, see :doc:`pandas <pandas>`.

Python to CL examples
~~~~~~~~~~~~~~~~~~~~~

Plain CL arrays
^^^^^^^^^^^^^^^

.. code:: python

    a = np.arange(6, dtype=float).reshape(2, 3)
    a.shape, a[1, 2], a.sum(), (a * 2).tolist()

.. code:: common-lisp

    (let* ((flat (make-array 6 :element-type 'double-float
                               :initial-contents (loop for i below 6 collect (float i 1d0))))
           (a (make-array '(2 3) :element-type 'double-float :displaced-to flat)))
      (list (array-dimensions a)
            (aref a 1 2)
            (reduce #'+ flat)
            (map 'list (lambda (x) (* 2 x)) flat)))

::

    ((2 3) 5.0d0 15.0d0 (0.0d0 2.0d0 4.0d0 6.0d0 8.0d0 10.0d0))

Matrix product by hand:

.. code:: common-lisp

    (defun matmul (a b)
      (let* ((n (array-dimension a 0)) (m (array-dimension b 1)) (k (array-dimension a 1))
             (c (make-array (list n m) :element-type 'double-float :initial-element 0d0)))
        (dotimes (i n c)
          (dotimes (j m)
            (dotimes (p k)
              (incf (aref c i j) (* (aref a i p) (aref b p j))))))))

    (matmul (make-array '(2 2) :element-type 'double-float :initial-contents '((1d0 2d0) (3d0 4d0)))
            (make-array '(2 2) :element-type 'double-float :initial-contents '((0d0 1d0) (1d0 0d0))))

::

    #2A((2.0d0 1.0d0) (4.0d0 3.0d0))

MAGICL, numericals, array-operations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Not in cl-repo. From their documentation, not run:

.. code:: common-lisp

    (magicl:@ (magicl:from-list '(1d0 2d0 3d0 4d0) '(2 2))
              (magicl:eye 2 :type 'double-float))

    (numericals:+ (numericals:asarray '((1 2) (3 4))) 1)

    (aops:reshape #(0 1 2 3 4 5) '(2 3))

::

    not run: MAGICL, numericals and array-operations are not in cl-repo
