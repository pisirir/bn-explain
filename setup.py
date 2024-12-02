import setuptools

setuptools.setup(
    name="bnexplain",
    version="1.0.0",
    author="Erhan Pisirir",
    author_email="e.pisirir@qmul.ac.uk",
    description="A python/R environment to define explainable BNs and generate explanations of reasoning for a given BN and a case.",
    long_description=open("README.md", "r").read(),
    long_description_content_type="text/markdown",
    url="https://github.research.its.qmul.ac.uk/deri-rim/bnexplain",
    download_url="",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    install_requires=['rpy2','simplenlg','numpy','json'],
    include_package_data=True
)