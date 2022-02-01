---
title: "JavaScript's Date is just a timestamp"
permalink: /javascript-date
category: javascript
layout: post
---

1. TOC
{:toc}

## Introduction

The thing with the Javascript [`Date`][Date] object is that, what it prints is
misleading.

```js
new Date()
// => Date Mon Jan 31 2022 02:32:37 GMT+0530 (India Standard Time)
```

If you think a `Date` contains all the things it prints, you are wrong.

* A `Date` does not contain any year, month or day
* A `Date` does not contain hours, minutes, seconds or milliseconds
* And, most importantly, a `Date` certainly does not contain any time zone

As the title says, the `Date` is just a timestamp. It's a number that represents
*milliseconds* since January 1, 1970 [UTC][]. That is, it's a single moment in
time -- that moment (and the corresponding number) remains the same regardless
of what time zone you are living in.

You can create a `Date` using this timestamp directly -- just pass it as the
only value to the constructor. For example,

```js
new Date(1640995200000)
// => Date Sat Jan 01 2022 05:30:00 GMT+0530 (India Standard Time)
```

You can also get or set this timestamp on a `Date` object using the `getTime()`
and `setTime()` methods respectively.

Everything else that the `Date` object exposes is either computed from this
timestamp, cached or used from the environment.

This applies to the getter methods like `getFullYear()`, `getMonth()`,
`getDate()`, `getHours()`, etc. 

That also applies to the `getTimezoneOffset()` -- this method just returns the
offset in minutes for the given timestamp in the *local time zone*. No matter
what you pass to the `Date` object, `getTimezoneOffset()` will always work with
the local time zone.

This misconception around what a `Date` is and what it contains leads to a lot
of confusion, especially when it comes to time zone conversions.

## Time zone conversions

Given a `Date` (or a timestamp), can you tell what time the clock would say for
it in a time zone that is not the same as your local time zone? Or, say you need
to schedule a meeting across time zones. Is 10 AM in India too late in New York
-- what would the local time be in another time zone at a particular moment?

For the longest time, browsers did not expose time zone data to JavaScript APIs,
so if you wanted to do time zone conversions on the client, you had to use a
library like [Moment Timezone][].

These days, the [Intl][] API ships in most modern browsers. That has meant
modern date/time libraries like [Luxon][] can be much smaller since they don't
need to ship locales or tz files.

However, Luxon has its own API for dealing with date/times that is different
from `Date`, and you might not want to bring an external dependency.

Can you, in this case, store the result of a time zone conversion in a `Date`
object? You can't. While technically you can do it, the fact that it is a
timestamp will end up creating problems for you down the line.

Unfortunately, that is how some libraries (like [date-fns-tz][]) do it.

```js
x = new Date() 
// => Wed Feb 02 2022 04:06:14 GMT+0530 (India Standard Time)

y = utcToZonedTime(x, 'America/New_York') 
// => Tue Feb 01 2022 17:36:14 GMT+0530 (India Standard Time)
```

`utcToZonedTime()` takes an input date and a target time zone, and returns a new
`Date` that's set up in such a way that the *local* time components
i.e. `getHours()`, `getMinutes()`, etc. return what they would have for the
target time zone.

However, since `Date` is just a timestamp, what it's doing is that it is
actually modifying the underlying timestamp. This can be confirmed by printing
the timestamp for both the dates.

```js
x.getTime() 
// => 1643754974808

y.getTime() 
// => 1643717174808
```

Not only is this semantically incorrect (the timestamp should have remained the
same), it will also create problems down the line if one is not careful.
For example, if this date is used in arithmetic, it should only ever be used
with dates which have similarly been converted to the same time zone using
`utcToZonedTime()`. If that's not followed, your date arithmetic will go wrong.

Given these issues, is it possible to do time zone conversions without moving
all date/time handling to a new library like Luxon? The answer is yes, and that
is what [naive-date][] does.

## Naive Date

Use a [`NaiveDate`][naive-date] as opposed to a `Date` when you want a Date like
object, but one that's not a timestamp. For example,

1. You want a YMD date and a time, but these are not linked to any time zone
2. You want to perform timezone conversions i.e. given a timestamp, what is the
   local time in Asia/Kolkata v/s America/New_York?
3. You want to perform calendrical calculations without worrying about the
   impact of DST transitions (e.g. would adding 86400 seconds always add one
   whole day?)
   
`NaiveDate`'s API is very similar to that of `Date` and includes all of its
warts, like month indexes starting from 0.

By the way, the term *naive* is inspired by its usage in the [Python datetime
module][], which categorizes date and time objects as "aware" or "naive"
depending on whether they include time zone information or not.

### Basic Usage

To create a `NaiveDate`, you pass a YMD date, or the full date/time components:

```js
// date only
// since we use 0 based indexes, the month below is Feb, not Jan
x = new NaiveDate(2022, 1, 1)

// date and time
y = new NaiveDate(2022, 1, 1, 10, 0, 0)
```

Since a `NaiveDate` is not linked to any time zone (and it's not a timestamp),
when you print it you won't see any zone info:

```js
x.toString()
// => '2022-02-01T00:00:00.000'

y.toString()
// => '2022-02-01T10:00:00.000'
```

The getters `getFullYear()`, `getHours()` etc. do what you expect. However,
There's no equivalent for `getUTC...` and `setUTC...` methods since they don't
make sense (`NaiveDate` is not a timestamp).

There's no equivalent for `getTimezoneOffset()` either, since a `NaiveDate`, by
definition, is not linked to any time zone.

And, most importantly, time zone conversions do the right thing. They return a
`NaiveDate` when you want the local time, and a `Date` when you want a
timestamp.

### Time zone conversions the right way

Let's say I want a timestamp which is equivalent to 12 PM on 1st of Feb, 2022 in
New York, which is not my local time zone. This is how you would get it using
`NaiveDate`.

```js
// First I create a NaiveDate to capture the local date/time components
const nyDate = new NaiveDate(2022, 1, 1, 12, 0, 0)

// Then I convert it into a timestamp using the toDate() instance method
nyDate.toDate('America/New_York')
// => Date Tue Feb 01 2022 22:30:00 GMT+0530 (India Standard Time)
```

Again, remember that the `Date` is a timestamp. The fact that it's printing it
in my local time zone is irrelevant.

Similarly, if I want to find the local time in another time zone for a given
timestamp, this is how it can be done:

```js
const timestamp = new Date(2022, 1, 2, 5, 0, 0)
timestamp
// => Date Wed Feb 02 2022 05:00:00 GMT+0530 (India Standard Time)

const nyDate = NaiveDate.from(timestamp, 'America/New_York')
nyDate.toString()
// => "2022-02-01T18:30:00.000"
```

To know more, see the [naive-date][] README.

## Conclusion

Just keep in mind two things:

1. `Date` is a timestamp
2. Don't use `Date` for time zone conversions -- use a library like Luxon or
   NaiveDate instead

[Date]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
[UTC]: https://en.wikipedia.org/wiki/Coordinated_Universal_Time
[Moment Timezone]: https://momentjs.com/timezone/
[Intl]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl
[Luxon]: https://moment.github.io/luxon/#/
[date-fns-tz]: https://github.com/marnusw/date-fns-tz
[naive-date]: https://github.com/chaitanyagupta/naive-date
