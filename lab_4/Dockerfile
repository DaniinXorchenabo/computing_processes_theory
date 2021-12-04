FROM python:3.9.7-slim-buster AS builder

COPY requirements.txt ./requirements.txt
RUN pip install --upgrade pip && pip install -r ./requirements.txt --no-cache-dir
COPY recursive_parser.py ./parser.py
