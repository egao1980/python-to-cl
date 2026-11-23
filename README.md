# python-to-cl
Common Lisp alternatives to popular Python packages: <https://egao1980.github.io/python-to-cl/>

Each page: the Python package's features, the third-party Common Lisp libraries, the [cl-stack](https://github.com/egao1980/cl-stack) ones, then examples with their output. The front page, [alternatives.org](alternatives.org), is the state of the alternatives in November 2026: a verdict per page, what the community survey and ecosystem reviews say, what changed since 2020, where cl-stack fits, and sources.

| Area | Pages |
|------|-------|
| Use cases | [I want to … → in Common Lisp](usecases.org) |
| HTTP and web | [Requests](requests.org), [HTTPX / aiohttp](httpx.org), [Flask / FastAPI](flask.org), [boto3](boto3.org), [websockets](websockets.org), [Scrapy / Selenium / Playwright](scrapy.org), [Google Cloud / Azure](cloud.org), [Paramiko](paramiko.org) |
| Data formats | [json / orjson](json.org), [PyYAML](pyyaml.org), [Pydantic](pydantic.org), [lxml / Beautiful Soup](lxml.org), [csv](csv.org), [msgpack / pickle](msgpack.org), [protobuf / gRPC](protobuf.org), [Markdown](markdown.org), [pyparsing / Lark](pyparsing.org), [python-dotenv](dotenv.org) |
| Text and time | [Dateutil](dateutils.org), [re / regex](regex.org), [Jinja](jinja2.org) |
| Data | [SQLAlchemy](sqlalchemy.org), [pandas / pyarrow](pandas.org), [NumPy](numpy.org), [SciPy](scipy.org), [SymPy](sympy.org), [matplotlib / Plotly](matplotlib.org), [Jupyter](jupyter.org), [redis-py](redis.org), [cachetools](cachetools.org) |
| AI and machine learning | [OpenAI / Anthropic](openai.org), [LangChain / MCP](langchain.org), [PyTorch / transformers](pytorch.org), [scikit-learn](sklearn.org) |
| Documents and media | [Pillow / OpenCV](pillow.org), [openpyxl / python-docx](openpyxl.org), [pypdf / ReportLab](pdf.org) |
| Messaging and jobs | [Kafka / RabbitMQ](kafka.org), [Celery / RQ](celery.org), [smtplib / email](email.org) |
| System | [pathlib](pathlib.org), [subprocess](subprocess.org), [asyncio](asyncio.org), [logging](logging.org), [Click / argparse](click.org), [multiprocessing](multiprocessing.org), [psutil](psutil.org), [Docker](docker.org), [GitPython](gitpython.org), [Rich / Textual](rich.org), [tkinter / PyQt](tkinter.org) |
| Security, testing and tooling | [cryptography / hashlib / PyJWT](cryptography.org), [pytest](pytest.org), [Hypothesis](hypothesis.org), [mypy](mypy.org), [pip / Poetry / uv](packaging.org) |

Examples load libraries with [cl-repo](https://github.com/egao1980/cl-repository) from `ghcr.io/egao1980/cl-systems`, not Quicklisp. Results come from SBCL 2.4.11 on macOS arm64; examples of libraries cl-repo doesn't carry are marked "not run". Some examples use local services (Docker containers, Google Chrome, a mock LLM API in [examples/mock-llm.lisp](examples/mock-llm.lisp)).

## The site

The `.org` files are the source (org-babel, `:exports both`). [cl-blog](https://github.com/egao1980/cl-blog) builds them into the site: `site.yaml` sets it up (`kind: docs`, `alternatives.org` is `index.html`), and `python-to-cl.asd` (build dependencies only, not a library) gets cl-blog from GHCR through cl-repo.

```sh
bin/build     # → _site/; open _site/index.html (links are relative)
```

`.github/workflows/site.yml` builds every push and PR and deploys `master` to GitHub Pages. A new page is a new `.org` file here plus a line under its area in `alternatives.org`; links between pages are `[[file:other.org][Other]]`.
