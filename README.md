# python-to-cl
Common Lisp alternatives to popular Python packages: <https://python-to-cl.readthedocs.io/>

Each page: the Python package's features, the third-party Common Lisp libraries, the [cl-stack](https://github.com/egao1980/cl-stack) ones, then examples with their output. Reviewed November 2026; index and sources in [alternatives.org](alternatives.org).

| Area | Pages |
|------|-------|
| HTTP and web | [Requests](requests.org), [HTTPX / aiohttp](httpx.org), [Flask / FastAPI](flask.org), [boto3](boto3.org) |
| Data formats | [json / orjson](json.org), [PyYAML](pyyaml.org), [Pydantic](pydantic.org), [lxml / Beautiful Soup](lxml.org) |
| Text and time | [Dateutil](dateutils.org), [re / regex](regex.org), [Jinja](jinja2.org) |
| Data | [SQLAlchemy](sqlalchemy.org), [pandas / pyarrow](pandas.org), [NumPy](numpy.org) |
| System | [pathlib](pathlib.org), [subprocess](subprocess.org), [asyncio](asyncio.org), [logging](logging.org), [Click / argparse](click.org) |
| Security and testing | [cryptography / hashlib / PyJWT](cryptography.org), [pytest](pytest.org) |

Examples load libraries with [cl-repo](https://github.com/egao1980/cl-repository) from `ghcr.io/egao1980/cl-systems`, not Quicklisp. Results come from SBCL 2.4.11 on macOS arm64; examples of libraries cl-repo doesn't carry are marked "not run".

The `.org` files are the source (org-babel, `:exports both`); `docs/` is the Sphinx mirror Read the Docs builds:

```sh
uv run --with sphinx --with sphinx_rtd_theme sphinx-build -b html docs _build/html
```
