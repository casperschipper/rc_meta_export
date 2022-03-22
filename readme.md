# Exposition MetaData fetcher RC

If you just want the data without the images, use example.json.

However, the image links in example.json be invalid (because of the timeout).
You can create a "fresh" export by:

1. Download the KC portal xml feed, for example using wget:

    wget -O rss.xml "https://www.researchcatalogue.net/feed?portal=6"

Save it as rss.xml in the same folder as index.html.

2. Then serve index.html locally, I used [http-server](https://www.npmjs.com/package/http-server).
    Open index.html in a browser:
    http://127.0.0.1:8080/index.html

It will show then (merged) JSON, there is also a button to download it.

