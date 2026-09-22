# lukwagoallan.com

Plain static site served by GitHub Pages from the `develop` branch, root path.
No build step: edit the HTML, commit, push.

## Layout

| Path | Content |
| --- | --- |
| `index.html` | Home page: links to LinkedIn and GitHub |
| `blog/` | The old Hakyll blog, kept as the HTML it last built. `blog/src/posts/` holds the markdown sources |
| `upper-body-demos/` | Upper-body strength plan exercise demos |
| `fat-loss-strength/` | Fat-loss and full-body strength 8-week plan |
| `sql-notes/` | SQL notes in markdown |
| `404.html` | Custom not-found page |
| `CNAME` | Custom domain for GitHub Pages |

`.nojekyll` disables Jekyll processing so files are served exactly as committed.

The `master` branch holds the deploy history of the old Hakyll build and is no longer served.
