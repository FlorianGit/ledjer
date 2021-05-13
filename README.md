# ledjer

Hi! Welcome to Ledjer. This is a personal project that's suitable for my particular situation. However, feel free to read the code or reuse parts of it.
Ledjer aims to be a [plain text accounting](https://plaintextaccounting.org/) tool, similar to a tool like [hledger](https://hledger.org/).

The goal of this project is for me to become more adept with Clojure. When learning a new language, I think it's a good idea to try to recreate something you use often, to keep the motivation. So the first goal of Ledjer will be to iplement functionalities that I use often and/or find interesting to implement, not to provide a complete package.

## Usage

TODO: describe how to use

## Terminology

| Term | Definition |
| --- | --- |
| journal | A representation of the  information contained in (a list of) journal files |
| transaction | A movement of value from source posting(s) to destination posting(s). Holds the information about date, description, accounts and amounts. |
| posting | The combination of an amount and an account. |
