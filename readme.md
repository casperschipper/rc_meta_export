# Exposition MetaData fetcher tool

If you just want the data without the images, use `example.json`.

There is one limitation with this `example.json`: the image links will be invalid (because of the timeout).
You can create a "fresh" export (with working image links!) by:

1. Download the KC portal xml feed, for example using wget:

    wget -O rss.xml "https://www.researchcatalogue.net/feed?portal=6"

Save the feed as rss.xml in the same folder as index.html.

2. Then serve index.html locally, I used [http-server](https://www.npmjs.com/package/http-server).
    Open index.html in a browser:
    http://127.0.0.1:8080/index.html

It will show then (merged) JSON, there is also a button to download it. I think the image links will be valid

# Format details:

* The description is formatted using \n as newlines. 
* Officially we ask students to follow a format for the description field mentioning their supervisor, research question etc.. but this is not 100% followed. Best to ignore it.
* The date is following IMF format I think: https://datatracker.ietf.org/doc/html/rfc5322#section-3.3
* DOI's are null at the moment, but may become a proper string (will just be an url).



