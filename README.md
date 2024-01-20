# tabby-mode

An Emacs minor mode for the [Tabby](https://tabby.tabbyml.com) AI coding assistant.

**WARNING**: This is an unofficial package for Tabby.
<details>
**NOTE**: This package is inteded for use without the `tabby-agent` protocol (that uses LSP).
</details>

## Installation

### From [MELPA](https://melpa.org)

<details><summary> Using `use-package`</summary>

```elisp
(use-package tabby-mode
  :ensure t
  :custom
  (tabby-api-url "http://localhost:8080"))
```

</details>

<details><summary> Using `package-install`</summary>
As `tabby-mode` is available on MELPA, simply run
`M-x package-install RET tabby-mode RET`.
</details>

### From GitHub

<details><summary> Using `straight` </summary>

```elisp
(use-package tabby-mode
  :straight (:host github :repo "ragnard/tabby-mode" :files ("dist" "*.el"))
  :custom
  (tabby-api-url "http://localhost:8080")
  :ensure t)
```

</details>

<details><summary> Using `elpaca` </summary>

```elisp
(use-package tabby-mode
  :elpaca (:host github :repo "ragnard/tabby-mode" :branch "main")
  :custom
  (tabby-api-url "http://localhost:8080")
  :ensure t)
```

<details>

## Configuration

Please ensure that you have a working installation of Tabby either locally or remotely.
If in doubt, please consult the [official documentation](https://tabby.tabbyml.com/docs/installation/).

Next please set the `tabby-api-url` to the listening address of the `tabby` server.
<details><summary>Local Example</summary>
If you are running `tabby` self-hosted with the default settings, you should set it to:

```elisp
(setq tabby-api-url "http://localhost:8080")
```

</details>

## Usage

Manually invoke `tabby-complete` (bound to `C-<tab>` by default)

**NOTE**: This is in contrast to `company`-based completion engines, that complete on idle.

## Contributing

Open an issue or a PR.

## License

This package is licensed under the permissive [FSF-endorsed](https://directory.fsf.org/wiki/License:Apache2.0) Apache-2.0 license.
