# Advent Of Code 2023

Written in PureScript for learning purposes.

# How to run

```
spago test -p <package-name>
spago run -p <package-name>
```

Example:

```
spago test -p 01-trebuchet
spago run -p 01-trebuchet
```

# Project structure

This project is a monorepo containing the following projects:

- utils: common utils needed in multiple assignments
- <day_num>-title: separate project for each day of AoC
- template: A template to copy-paste as new project for each day