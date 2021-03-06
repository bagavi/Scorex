Scorex Bitcoin - Lightweight Bitcoin protocol framework.
====================================================================================================================================================================================
This is developed over Score [https://github.com/input-output-hk/Scorex](https://github.com/input-output-hk/Scorex).


Motivation
----------

 Bitcoin Core source code contains more 100K lines of code(80K of C++ only). All parts of the design(network/transactional/consensus protocols) are mixed in a hard way.
 So researchers and developers are not in good start positions to make experiments.

**In Scorex Bitcoin, we've implemented the Bitcoin protocol in less than 5k lines of Scala code and this allows researchers to prototype over the bitcoin protocol.**

Features
--------

* Compact, functional code
* Modular design with fine granularity
* Scala language
* Asynchronous network layer on top of TCP
* JSON API
* Command line client for the JSON API
* Cryptographic primitives externalized into [separate scrypto framework](https://github.com/input-output-hk/scrypto)
* Few examples out-of-box

Documentation
-------------

Coming soon.

License
-------

To the extent possible under law, the authors have dedicated all copyright and related and neighboring
rights to this software to the public domain worldwide. This software is distributed without any warranty.
You can find applied CC0 license legalcode in the [COPYING](COPYING)
