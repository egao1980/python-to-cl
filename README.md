# python-to-cl
Common Lisp alternatives to popular Python packages: <https://egao1980.github.io/python-to-cl/>

Each page: the Python package's features, the third-party Common Lisp libraries, the [cl-stack](https://github.com/egao1980/cl-stack) ones, then examples with their output. The front page, [alternatives.org](alternatives.org), is the state of the alternatives in November 2026: a verdict per page, what the community survey and ecosystem reviews say, what changed since 2020, where cl-stack fits, and sources.

| Area | Pages |
|------|-------|
| HTTP and web | [Requests](requests.org), [HTTPX / aiohttp](httpx.org), [Flask / FastAPI](flask.org), [boto3](boto3.org) |
| Data formats | [json / orjson](json.org), [PyYAML](pyyaml.org), [Pydantic](pydantic.org), [lxml / Beautiful Soup](lxml.org) |
| Text and time | [Dateutil](dateutils.org), [re / regex](regex.org), [Jinja](jinja2.org) |
| Data | [SQLAlchemy](sqlalchemy.org), [pandas / pyarrow](pandas.org), [NumPy](numpy.org) |
| System | [pathlib](pathlib.org), [subprocess](subprocess.org), [asyncio](asyncio.org), [logging](logging.org), [Click / argparse](click.org) |
| Security and testing | [cryptography / hashlib / PyJWT](cryptography.org), [pytest](pytest.org) |

Examples load libraries with [cl-repo](https://github.com/egao1980/cl-repository) from `ghcr.io/egao1980/cl-systems`, not Quicklisp. Results come from SBCL 2.4.11 on macOS arm64; examples of libraries cl-repo doesn't carry are marked "not run".

## The site

The `.org` files are the source (org-babel, `:exports both`). [cl-blog](https://github.com/egao1980/cl-blog) builds them into the site: `site.yaml` sets it up (`kind: docs`, `alternatives.org` is `index.html`), and `qlfile` gets cl-blog from GHCR through cl-repo.

```sh
bin/build     # → _site/; open _site/index.html (links are relative)
```

`.github/workflows/site.yml` builds every push and PR and deploys `master` to GitHub Pages. A new page is a new `.org` file here plus a line under its area in `alternatives.org`; links between pages are `[[file:other.org][Other]]`.
