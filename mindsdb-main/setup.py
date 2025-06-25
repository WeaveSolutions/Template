from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

with open("requirements.txt", "r", encoding="utf-8") as fh:
    requirements = fh.read().splitlines()

setup(
    name="mindsdb-integration",
    version="0.1.0",
    author="Your Name",
    author_email="your.email@example.com",
    description="MindsDB integration with multi-cloud support",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/yourusername/Nexpo",
    packages=find_packages(),
    package_data={
        "": ["*.json", "*.yaml", "*.yml"],
    },
    install_requires=requirements,
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.8',
    entry_points={
        'console_scripts': [
            'init-mindsdb=init_mindsdb:main',
        ],
    },
)
