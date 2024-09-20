# genexpr-mode

An Emacs major mode for editing [GenExpr](https://docs.cycling74.com/max8/vignettes/gen_genexpr) files used in [Max/MSP's gen~](https://docs.cycling74.com/max8/vignettes/gen_overview) object.

## Features

- Syntax highlighting for GenExpr code
- Indentation support

## Installation

### MELPA

Once this package is available on MELPA, you can install it using `package.el`:

1. Add MELPA to your `package-archives` if you haven't already:

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
```

2. Refresh the package contents:

```
M-x package-refresh-contents
```

3. Install the package:

```
M-x package-install RET genexpr-mode RET
```

### Manual Installation

1. Download `genexpr-mode.el` to your local machine.
2. Add the following to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/directory/containing/genexpr-mode/")
(require 'genexpr-mode)
(add-to-list 'auto-mode-alist '("\\.genexpr\\'" . genexpr-mode))
```

## Usage

Once installed, Emacs will automatically use `genexpr-mode` for files with the `.genexpr` extension. You can also manually activate it for any buffer:

```
M-x genexpr-mode
```

## Customization

- Var `genexpr-indent-level` (default `4`): indentation offset in spaces

## Acknowledgment

- [simpc-mode](https://github.com/rexim/simpc-mode)
- [GenExprForSublime](https://github.com/emmanueljourdan/GenExprForSublime)
- [lua-mode](https://github.com/immerrr/lua-mode)

## License

This project is licensed under the GPL-3.0 License - see the [LICENSE](LICENSE) file for details.
