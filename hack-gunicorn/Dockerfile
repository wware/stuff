FROM ubuntu

RUN apt-get update
RUN apt-get install -y python-pip python-dev nginx vim
RUN pip install --upgrade pip
RUN pip install gunicorn flask

COPY foo.py /
COPY wsgi.py /

EXPOSE 8000

CMD ["gunicorn", "--bind", "0.0.0.0:8000", "wsgi"]
