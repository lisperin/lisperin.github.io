---
layout: post
title: Design, documentation and exploration of REST APIs
permalink: /rest-api-design
---

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

The same URLs are used for API requests. To do this,

1. Either use the query parameter `accept=json` in the URL (for URL encoded or multipart POST requests this param can optionally be sent in the body instead),
2. Or use the header `Accept: application/json` in the request

Personally I prefer the former since it allows one to test GET requests directly in the browser, something that I consider important.

Note that for URLs of type `api.example.com/collection/:id`, `:id` should obviously be a real id in the database when making an API request; when viewing documentation it can be anything (though the recommended convention is to use the literal string `:id` when you don't have a real one).

### Versioning

By default, the above calls will return the latest API version (both for documentation and API calls). Switching to another version is easy:

1. Either use a query parameter like `v=1` in the URL (and like `accept=json` it can be sent in the POST body too).
2. Or use vendor mime types: `Accept: application/vnd.api.v1+json`

Again, I personally prefer the former because of ease of use in the browser.

So if you wanted to see documentation for a resource at a particular version, just use: `api.example.com/resource?v=1`. The corresponding GET API URL becomes `api.example.com/resource?accept=json&v=1`.

## Authentication

Besides bearer authentication, I also recommend supporting basic authentication because of its support in the browser. Go with username and password, or if you only want to support access tokens, use `bearer` as the username and the access token as the password. This, again, ensures that GET requests can be easily tested in the browser.

Some services support sending the access token as a query parameter too. I **don't** recommend doing this though. That's because an access token is sensitive data, but unfortunately query parameters are included by default in almost all HTTP logs. Its becomes very easy to leak access tokens this way. Use basic authentication instead.

## API Explorer

An API explorer allows one to test the API right in the browser. It is not trivial to implement, but if done well can make life much easier for developers using your API.

We already allow users to send GET requests in the browser, but we can do much better.

* Link to various relations of the resource in your API response. For example, when looking at a post, provide an API URL[^1] in the response that allows one to get a list of comments[^2].
* Allow users to send `accept=pretty` instead of `accept=json` to get the same response, except it returns HTML which renders the JSON in a pretty way -- indented, syntax highlighted and with clickable links.

This is fairly simple to implement but allows users to explore related API resources very easily. Plus, since we use basic authentication, the credentials are cached automatically by the browser so we don't need to provide them every time we visit a new link. 

That said, this exploration is limited only to GET requests. If you want a full fledged API explorer, you can instead use something like `api.example.com/resource?explore=true` which lists all the supported methods for the given resource and allows the user to test any of them. Granted, this is not a trivial exercise, but at least the discovery of your API explorer becomes trivial.

## Conclusion

By following these two conventions:

1. One single endpoint for API calls, documentation and exploration
2. One-to-one mapping of resource paths from the main to the API domain

We make discovery of our REST API and its documentation much easier.

Also, for any resource under `example.com`, allow users to reach the API documentation, pretty or raw JSON response with just one or two clicks. This should allow users to get started with your API much quicker.

----

[^1]: Ensure that URLs of relations include the current API version and `accept=json` in query parameters. Otherwise linking to them (covered in the next point), won't do any good.

[^2]: H/T [@rakesh314](https://twitter.com/rakesh314)
