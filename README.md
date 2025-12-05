<img src="https://media.github.tools.sap/user/54420/files/8a0fe5cd-94d0-4bf2-8107-2c5994862f2d" width="100%" />

# Typst Template DHBW <a href="https://github.tools.sap/vt-tools/vt-template-typst/generate"><img src="https://img.shields.io/badge/Generate_from_Template-8A2BE2?logo=github" /></a>

This is a university report template written in [Typst](https://typst.app/),
based on the [vtgermany/LaTeX-Template-DHBW](https://github.wdf.sap.corp/vtgermany/LaTeX-Template-DHBW).

> Typst was born out of our frustration with LaTeX. Not knowing what we were in for, we decided to take matters into our own hands and started building. -Typst

Typst is a replacement for LaTeX which was designed to be **as powerful as LaTeX while being much easier to learn and use**.
It has a much simpler syntax (similar to Markdown), _actual good error messages_ (looking at you, LaTeX)
and out-of-the-box bibliography features using `.bib` or `.yaml` files (see `example.yaml`) (again, looking at you, LaTeX).

If you want to explore other features here is the link for the offical [documentation](https://typst.app/docs/).

## 🏃‍♂️ Getting Started

<table>
  <tr>
    <td width="50%">
      <p>Getting started is easy!</p>
      <ul>
        <li>Follow the steps as described in <a href="#local-compiler">🛠️ Setup</a></li>
        <li>Open <code>main.typ</code> and edit the document metadata</li>
        <li>If you're new to Typst, take a look at the <a href="https://typst.app/docs/tutorial">Tutorial</a></li>
        <li>Compile PDF using <code>typst compile main.typ</code></li>
      </ul>
    </td>
    <td width="50%">
      <a href="https://video.sap.com/media/t/1_nrsgn0jc">
        <img src="https://github.tools.sap/vt-tools/vt-template-typst/assets/98885/e8b874bf-a31a-4723-93a1-2f47996e16c4" />
      </a>
    </td>
  </tr>
  <tr>
    <th>Setup in Local Machine</th>
    <th>Quick Start in 2 Minutes</th>
  </tr>
</table>

## ✨ Features

Here are some basic features our template provides:

### Pre-defined Pages

The template supports the following pages out-of-the-box:

- Title Page
- Declaration of Originality
- Confidentiality Clause
- Abstracts (in multiple languages)
- Table of Contents
- List of Abbreviations
- List of Figures
- List of Tables
- Source Code Directory
- Bibliography

The Confidentiality Clause can be toggled by setting `confidentiality_clause` and the abstracts can be modified by changing the `abstract` field:

<table>
  <tr>
    <td>
      <code>main.typ</code> @ <code>#show: project.with(</code>
    </td>
  </tr>
  <tr>
    <td><pre lang="yaml">confidentiality_clause: true
abstract: (
  ("de", "Deutsch",
    include("abstracts/abstract_german.typ")
  ),
  ("en", "English",
    [*Hello World!* This is my abstract.]
  )
),</pre>
    </td>
  </tr>
</table>

> [!NOTE]
> Some outlines are only shown if your document contains figures which should be outlined.
> For example, `List of Figures` will be hidden if you don't have any images in your document.

---

### Languages

You can switch to a different language easily by setting `lang` in your main document. This will also affect some region-based writing style.
If the language is set to "de", quotes will be `„test"`, otherwise `"test"`.

<table>
  <tr>
    <td>
      <code>main.typ</code> @ <code>#show: project.with(</code>
    </td>
  </tr>
  <tr>
    <td><pre lang="yaml">lang: "de"</pre>
    </td>
  </tr>
</table>

Currently, there are 2 languages supported:

| Language | Code |
| -------- | ---- |
| German   | `de` |
| English  | `en` |

---

### Abbreviations

First, define the acronyms in the `acronyms` field then initialize the template. Under the hood it is using [glossarium](https://github.com/typst-community/glossarium).

```typ
#show: project.with(
  ...
  acronyms: (
    (key: "NN", short: "NN", long: "Neural Network"),
    (key: "SG", short: "SG", long: "Singular"),
  )
  ...
)
```

Once the acronyms are defined, you can use them in the text with the `@key`. To get the plural version of the acronym, you can use `@key:pl` that adds an 's' after the acronym. If a plural version of the definition is provided, it will be used if the first use of the acronym is plural. Otherwise, the singular version is used, and a trailing 's' is added.

## 🛠️ Setup

Here are some basic setup instructions, if you need more details please refer to the [documentation](https://typst.app/docs/).

1. Click on [![Generate](https://img.shields.io/badge/Generate-8A2BE2?logo=github)](https://github.tools.sap/vt-tools/vt-template-typst/generate) and name the repository (e. g. `pa-1`)
2. Clone the created repository to your local machine: `git clone https://github.tools.sap/I550629/pa-1.git`
3. Navigate to the repository `cd pa-1`
4. Setup your dev environment [manually](#manual-setup), with [dev containers](#devcontainer) or using [nix](#nix-shell).
5. Compile your PDF: `typst compile main.typ`
6. Open the PDF in your favorite PDF-Viewer

If you want live updates, you can use `typst watch main.typ` for automatically watch for changes in your code.

#### Manual Setup

1. Install [Typst](https://github.com/typst/typst)
  - Pre-compiled Binaries: [typst/releases](https://github.com/typst/typst/releases/)
  - macOS/Linux: `brew install typst`

2. Install [Typstyle](https://typstyle-rs.github.io/typstyle/)
  - Pre-compiled Binaries: [typstyle/releases](https://github.com/typstyle-rs/typstyle/releases)
  - macOS/Linux: `brew install typstyle`

#### Devcontainer

[Devcontainers](https://code.visualstudio.com/docs/devcontainers/containers) allow you to work in an isolated development environment with all dependencies installed using Docker.

Install the [Dev Container Extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) and run the command `> Dev Containers: Reopen in Container` from the command pallet.

Note that you need a Docker installation. This setup was tested with [colima](https://github.com/abiosoft/colima), a compliant docker runtime.

> [!IMPORTANT]
> This is the most straight forward approach of creating a development environment, as it not only installs the needed dependencies to compile your project, but also all the extensions needed to work in VS Code.

To change the LTeX+ spell checker language from default English to German navigate to `.devcontainer/devcontainer.json` and change `"ltex.language": "de-DE",`

#### Nix shell

[Nix shells](https://nixos.wiki/wiki/Development_environment_with_nix-shell) allow you to create a temporary shell with all dependencies installed.

Run `nix develop --command zsh` (or something different to `zsh` if you are using a different shell). From inside the resulting shell run `code .` to start VS Code with the installed dependencies.

> [!WARNING]
> This is not the most beginner friendly way of setting up a dev environment, if you are not using nix already.
> Only use this approach if you are already using nix as your package manager.

---

### Web-IDE

> [!WARNING]
> SAP does not allow students to use web-hosted editors like [typst.app](https://typst.app/) to compose scientific papers.


## 💡 Feedback

**Anything Missing?** Please [create an issue](https://github.tools.sap/vt-tools/vt-template-typst/issues/new) or open a Pull Request right away.

## 🤝 Contribute

1. Pick an issue you want to work on and/or contact [Max Mechler](mailto:max.mechler@sap.com), [Leon Fertig](mailto:leon.fertig@sap.com), [Marvin Fuchs](mailto:marvin.fuchs@sap.com) or any other contributor.
2. Follow [setup guide](#️-setup) for your local development environment.
3. Make sure to format your changes with `typstyle . -i` before committing.

# History and Future of this Template
This repository is based on the great work of Daniel Statzner and others on [vt-template-typst](https://github.tools.sap/I550629/vt-template-typst).


This repository was contrived as a continuation of Daniel's work, because we wanted this important template to be part of a bigger project, where it can be managed by multiple people actively using it.
For this, our choice fell on [STAR Tools](https://github.tools.sap/star-tools).

**The template lives from the contribution of its users. Finding bugs, adapting to new versions of dependencies, or starting a discussion about any change, are only three ways to contribute to this project. So, do not hesitate to start your [contribution](#-Contribute) now :)**

The goal of this template is to make it as easy as possible for you to write your thesis at SAP! That being said, there are some alternatives out there:

- [clean-dhbw](https://typst.app/universe/package/clean-dhbw/): The "official" template for Computer Science at DHBW Karlsruhe. We found this template to be pretty opinionated and not so keen on customization.
- [supercharged-dhbw](https://github.com/DannySeidel/typst-dhbw-template): The "unofficial" template for DHBW students. As of 16.07.2025, the last commit was more than half a year ago, so outdated package versions may lead to unwanted behavior with the latest version of typst. Especially for non-typst-savvy users, we do not recommend using a not actively maintained template!
- [typst-template-dhbw](https://github.tools.sap/I568996/typst-template-dhbw): Another fork of Daniel Statzner's repository - also tailored for SAP use.


In the future, we may want to publish a company-agnostic version of this template on [github.com](github.com) so that every student at DHBW can access and use this template, not only SAP students. But at the moment, we cannot give a schedule for this plan.
