from setuptools import setup, find_packages

setup(
    name='1l-tools',
    version='1.0',
    packages=find_packages(),
    install_requires=[
        'pandas',
        'openpyxl',
        'numpy>=1.16',
        'matplotlib',
        'toml'
    ],
    include_package_data=True,
    scripts=['1l-tools']
)
