---
layout: post
title: Design, documentation and exploration of REST APIs
permalink: /rest-api-design
---

*Updated:* Nov 1, 2018

*Note:* This is a living document -- I update it occasionally as my thinking around this scheme evolves.

* Replace with TOC
{:toc}

## Introduction

Let's say you've setup a brand new webapp at `example.com` and want to expose a REST API. How do you design the URLs for API requests and documentation? How do you handle versioning?

One popular option is to use `api.example.com` for API requests, another endpoint for documentation, and possibly a third endpoint for an API explorer (if it exists).

For authentication, the preferred option it seems is to generate an API key or get an OAuth access token, then send it using bearer authorization in the request: `Authorization: Bearer <access_token>`

Versioning is usually handled in one of two ways:

1. As part of the path e.g. `api.example.com/v1/`
2. Using vendor MIME types i.e. sending something like `Accept: application/vnd.api.v1+json` in the request headers

All of this works, however it takes a bit of time to figure out. You have to find the API docs, then figure out the endpoint, authentication, versioning, etc. Moreover, unless you have an API explorer, trying out an actual response takes even longer (figure out the right `curl` incancation or something similar). Testing even GET requests in the browser is really hard with many APIs.

This document proposes a small set of conventions to make working with REST APIs (discovery, testing and exploration) a little bit easier.

## One endpoint to rule them all

Given a webapp on `example.com`, let's use `api.example.com` for exposing the API. We will use this endpoint not just for API requests but also for documentation.

### Documentation

Here's how the URLs will look for documentation:

* `api.example.com` -- API documentation home page (introduction, authentication, versioning, etc.)
* `api.example.com/resource` -- documentation for the resource `example.com/resource`
* `api.example.com/collection/:id` -- documentation for `example.com/collection/:id`.

As you can see, for any given resource on `example.com`, to check its documentation just change the domain to `api.example.com`.

### API requests

The same URLs are used for API requests. However, one needs to append the API version as a query parameter in the URL to make the API request. So,

* `api.example.com/resource?v=1` will be used send an API request for `example.com/resource` at API version 1.

Note that for URLs of type `api.example.com/collection/:id?v=1`, `:id` should obviously be a real id in the database when making an API request; when viewing documentation it can be anything.

This scheme, combined with basic authentication as explained below, means that a user can easily explore your API using GET requests in the browser itself.

### Documentation for Older Versions

Specify the version value alongwith `show=doc` to show documentation for an older version. For example: `api.example.com/resource?v=1&show=doc`.

### API Explorer

We already allow users to send GET requests in the browser, but we can do much better.

1. Link to various relations of the resource in your API response. For example, when looking at a post, provide an API link in the response that allows one to get a list of comments for that post.[^1]
    ```
    # Request
    GET /posts/1?v=1
    Host: api.example.com

    # Response
    200 OK
    {
        id: 1,
        title: "foo bar",
        body: "...",
        links: {
            "comments": "https://api.example.com/posts/1/comments?v=1
        }
    }
    ```
2. Allow users to send `show=pretty` alongwith the version to get the same response, except it returns HTML which renders the JSON in a pretty way -- indented, syntax highlighted and with clickable links.

This is fairly simple to implement but allows users to explore related API resources with just a click. Plus, since we use basic authentication, the credentials are cached automatically by the browser so the user doesn't need to provide them every time they follow a link.

That said, this exploration is limited only to GET requests. If you want a full fledged API explorer, you can instead provide something like `api.example.com/resource?show=explorer` which lists all the supported methods for the given resource and allows the user to test any of them.

## Authentication

Besides bearer authentication, I also recommend supporting basic authentication because of its support in the browser. Go with username and password, or if you only want to support access tokens, use `bearer` as the username and the access token as the password. This, again, ensures that GET requests can be easily tested in the browser.

Some services allow sending the access token as a query parameter. I **DON'T** recommend doing this. That's because an access token is sensitive data, but unfortunately query parameters are included by default in almost all HTTP logs. You might also inadvertently share an API URL with your access token in it. Use basic authentication instead, its much safer.

## Conclusion

By following these two conventions:

1. One single endpoint for API calls, documentation and exploration
2. One-to-one mapping of resource paths from the main to the API domain

We make discovery of our REST API and its documentation much easier.

Also, for any resource under `example.com`, allow users to reach the API documentation, pretty or raw JSON response with just one or two clicks. This should allow users to get started with your API much quicker.

----

[^1]: H/T [@rakesh314](https://twitter.com/rakesh314)
