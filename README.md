# gptel-extensions

Extra functions for [gptel](https://github.com/karthink/gptel), a package that allows you to use [OpenAI's GPT](https://openai.com/) for text generation and completion directly from Emacs.

## Installation

1. Clone the repository to a local directory with:

   ```
   git clone https://github.com/kamushadenes/gptel-extensions.git
   ```

2. Add the following to your Emacs init file (usually `~/.emacs` or `~/.emacs.d/init.el`):

   ```
   (add-to-list 'load-path "path/to/gptel-extensions")
   (require 'gptel-extensions)
   ```

   Replace `path/to/gptel-extensions` with the path where you cloned the repository.

3. Restart Emacs, or evaluate the added code with `M-x eval-buffer` while in the init file.

## Functions

### `gptel-ext-send-whole-buffer`

Send the whole buffer to ChatGPT.

### `gptel-ext-ask-document`

Load the current buffer into a session so you can ask questions about it.

### `gptel-ext-rewrite-and-replace`

Rewrite the region or sentence at point and replace it with the response. Extracted from the [Wiki](https://github.com/karthink/gptel/wiki).

### `gptel-ext-refactor`

Refactor the region or sentence at point.

## Variables

### `gptel-ext-ask-document-prefix`

Prefix to use when asking questions about a document. Defaults to:

```text
Your task is to answer questions about the following document. If you don't know the answer, reply with "I don't know"

###### DOCUMENT START ######

```

### `gptel-ext-ask-document-suffix`

Suffix to use when asking questions about a document. Defaults to:

```text

###### DOCUMENT END ######

### Question: 
```

### `gptel-ext-refactor-directive`

Directive to use when refactoring code. Defaults to:

```text
You are a programmer. Refactor my code to improve readability. Reply only with the code.
```

## Credits

This merely extends the amazing [gptel](https://github.com/karthink/gptel/) by Karthik Chikmagalur, who performed all the hard work.
