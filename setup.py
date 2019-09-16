import sys
from setuptools import setup

install_requires = []

setup(
    #basic package data
    name = 'SQLyser',
    version = '0.1',
    author = 'Shakeel Ansari',
    author_email = 'shakeel.ansari@gmail.com',
    license = '',
    url='https://github.com/shakeelansari63/sqlyser',
    description=(''),
    long_description='',
    keywords = ('sql sqlyser sql-analyser'),
    classifiers = [],
    packages=['sqlyser'],
    install_requires=install_requires,
    )