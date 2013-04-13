#!/usr/local/bin/bash

fetch()
{
    api_key="0xdeadbeef"
    offset=0

    while ((offset < 500)); do
        echo "fetching post from offset $offset";
        wget -O "posts.$offset" \
            "http://api.tumblr.com/v2/blog/cannedlinks.tumblr.com/posts?api_key=$api_key&offset=$offset" \
            >/dev/null 2>&1;
        offset=$((offset+20));
    done
}

post()
{
    auth_token="cannedprimates:0xdeadbeef"

    i=1
    cat posts.* | jq -c '.response.posts[] | .url,.title,.tags' | while read line; do
        if ((i == 1)); then
            # trim "
            url=$(echo $line | sed -e 's/"//g')
            i=$((i+1))
        elif ((i == 2)); then
            #trim ", escape whitespace
            title=$(echo $line | sed -e 's/"//g' | sed -e 's/ /%20/g')
            i=$((i+1))
        elif ((i == 3)); then
            #trim #, handle whitespace, trim []
            tags=$(echo $line | sed -e 's/"//g' | sed -e 's/, /,/g'| sed -e 's/ /-/g' | sed -e 's/\[//g' | sed -e 's/\]//g')
            i=1
            curl "https://api.pinboard.in/v1/posts/add?auth_token=$auth_token&url=$url&description=$title&tags=$tags"
        fi
    done
}

# eof
