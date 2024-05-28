# sqitch_tool

A utility for refactoring [sqitch](https://sqitch.org/) changes.

You can run the program using:
```
nix run github:brokenpylons/squitch_tool
```

Or install it permanently using:
```
nix profile install github:brokenpylons/squitch_tool
```

## Supported commands

### Plan
Prints the formated plan file.

```
squitch_tool plan
```

### Remove
Removes the change from the ```sqitch.plan```, and removes the associated files in the ```deploy```, ```verify```, ```revert``` folders.

```
squitch_tool rm change
```

### Move
Renames the change in the ```sqitch.plan```, and moves the associated files in the ```deploy```, ```verify```, ```revert``` folders.

```
squitch_tool mv from_change to_change
```

## Status
As far as I can tell it works. No tests yet.

## License
ISC
