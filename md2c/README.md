# Markdown to Confluence

`atbp md2c` publishes a Markdown directory tree to Confluence. A `.md2c.conf`
file selects the Confluence space (`spaceKey`) and root page (`pageId`).

Links to Markdown documents in the same published tree become links to their
Confluence pages. Paths resolve relative to the document containing the link:

```markdown
[Getting started](Getting%20started.md)
[Reference](../Reference.md#details)
[Folder overview](guide/README.md)
```

Inline and reference-style links work across siblings, parents and descendants,
including forward and circular references. Folder notes (`README.md`, `index.md`
or a file named after its directory) link to the corresponding branch page.
Page titles set in YAML front matter do not change how paths resolve. Link text,
formatting, titles, query strings and fragments are preserved; heading fragments
are not translated between Markdown and Confluence anchor conventions.

Only destinations present in the published tree are converted. External links,
images and code retain their existing behavior. Other relative links remain
subject to the Markdown-to-ADF library's behavior, which can render them as plain
text.

md2c resolves or creates all destination pages before publishing their content.
Resolved destinations are included in the content hash, so existing pages receive
converted links on the next publish, and links update if a destination page is
recreated. Identical subsequent publishes are still skipped.

## Memory use on CI runners

Publishing reserves page IDs using titles and empty drafts, then converts and
publishes one page at a time, children before parents. Completed page bodies are
discarded. Diagram rendering and attachment uploads also run one at a time during
publishing. This trades parallel throughput for lower peak heap use.

The source tree and page-ID map still grow with the number of files, but Markdown
and rendered page bodies no longer accumulate across the document set. The heap
must accommodate the largest page and its diagram renderer. Keep source files
unchanged while a publish is running, since bodies are read as each page is
published.

For an explicit JVM heap cap with the packaged launcher:

```bash
atbp -J-Xmx512m md2c /path/to/docs
```

The cap covers Java heap, not total process memory or D2 subprocess memory.
Large individual pages or diagrams may need a larger cap.
