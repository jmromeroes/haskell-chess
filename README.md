Apecs/Gloss starter project
===========================

* https://github.com/jonascarpay/apecs
  * https://github.com/jonascarpay/apecs/tree/master/examples
* https://hackage.haskell.org/package/apecs-gloss
  * https://hackage.haskell.org/package/apecs

Installation
------------

### Ubuntu

```shell
sudo apt install freeglut3-dev libsdl2-mixer-dev libsdl2-image-dev
```

### Nix

🤷‍ Check out the original repo.

Running
-------

```shell
stack run
```

To find where the compiled binary is:

```shell
stack build
stack exec -- which apecs-gloss-starter
```
