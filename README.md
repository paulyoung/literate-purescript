# literate-purescript

Literate programming in PureScript.

## Usage

Run `lit` in the root of a project to populate the `generate-src` directory with PureScript modules based on code blocks defined in Markdown files.

Then, include the `generated-src` directory when compiling PureScript to verify that everything is valid.

e.g.

```
purs compile "bower_components/*/src/**/*.purs" "src/**/*.purs" "generated-src/**/*.purs"
```

or

```
pulp build --include generated-src
```
