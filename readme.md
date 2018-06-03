# Purescript Template

We regularly need to run experiments to explore library approaches or have a minimal Halogen playground. This repository serves as a working base for experimentation. You will at least need these tools installed:

- `bower` for PureScript package management (feel free to replace this with `psc-package`)
- `yarn` for Node package management and scripts

## Use

Get started quickly:

```sh
yarn && yarn build
open ./dist/index.html
```

- `yarn` will install everything you need
- `yarn build` will build the project and bundle the JS so you can view your app at `dist/index.html`
- `yarn watch` will build and bundle the project, and re-run every time a source file changes

I recommend opening your terminal or IDE to work on the source, and have a terminal running `yarn watch` in the background. Then, you can reliably save your code, wait for the build/bundle to complete, and then refresh your browser to see the updated application.

## Caveats

Halogen has not been updated to `4.0.0` yet, which means it is not compatible with PureScript `0.12` yet. Make sure you are still on the `11.7` toolchain. 

If you use the package scripts run with `yarn build` and `yarn watch` then you should be good to go because they'll use local installations of PureScript and Pulp.
