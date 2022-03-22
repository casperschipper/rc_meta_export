# Data fetcher RC

First fetch a fresh rss feed, using wget for example:

    `wget -O rss.xml "https://www.researchcatalogue.net/feed?portal=6"`

Then server this folder locally, for example using [http-server](https://www.npmjs.com/package/http-server).

Open index.html:

http://127.0.0.1:8080/index.html

The json should load directly.