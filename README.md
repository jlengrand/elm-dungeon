# elm-dungeon

Simple dungeon tile based game written in Elm 

## Demo

[You can try the game here](https://www.lengrand.fr/elm-dungeon/) or [here](https://jlengrand.github.io/elm-dungeon)

## Install

```
yarn install
```

## Run development

```
yarn dev
```

## Compilation (compiles to the doc folder)

```
yarn prod
```


It will open a browser


## Stuff still to be implemented (Spoiler : Almost everything)

* We shouldn't update the durability of a weapon (two swords don't make a twice a much powerful sword...)
* Detroy WeaponComponents instead of setting the durability to 0. This will allow to change the weapon type, ...
* Refactor duplications
* Traps
* Chests
* Coins have a value (currently always 1)
* Enemies have a strength (currently always 1)
* Actual textures
* Weapon and Enemy types
* Tile movement system / generator (with increasing difficulty)


## License

MIT

## Thanks to

[@JordyMoos for all the help](https://github.com/JordyMoos)