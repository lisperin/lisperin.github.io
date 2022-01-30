---
layout: post
title: "The JavaScript Date is a lie"
permalink: /naive-date
category: unix
---

## Introduction

The thing with the Javascript [`Date`][Date] object is that it is a lie. What it
prints leads you to believe it has things that, it really does not.


```js
new Date()
// => Date Mon Jan 31 2022 02:32:37 GMT+0530 (India Standard Time)
```

* The Date object does not contain any year, month or day
* The Date object does not contain hours, minutes, seconds or milliseconds
* And, most importantly, the Date object does not contain any timezone
  information

So, what exactly does it contain?

Well, it contains a Number. That number represents milliseconds since January 1,
1970 [UTC][]±00:00. Its a single moment in time -- that moment (and the
corresponding number) remains the same regardless of what time zone you are
living in.

This number is called the *time value* or the *timestamp number* of a Date.

For example, the time value 1640995200000 repesents midnight of January 1, 2022
UTC±00:00. In India, at that moment, it was 5:30 in the morning. In New York, at
the same moment, it was 7 PM of December 31, 2021. Same moment, same time value,
but different local times.

You can create a `Date` using this number directly -- just pass it as the only
value to the constructor. For example, this is what Firefox does:

```js
new Date(1640995200000)
// => Date Sat Jan 01 2022 05:30:00 GMT+0530 (India Standard Time)
```

On the other hand, if I create the same Date using node.js:

```js
new Date(1640995200000)
// => 2022-01-01T00:00:00.000Z
```

Ignore the formatting differences, and notice that the actual time printed
differs between the two implementations because they used different time zones
while printing the date.

Javascript also provides helper methods to extract the individual components
from the date -- both in local time and in UTC. For example, `getHours()` and
`getMinutes()` return the hours and minutes in local time -- for the date we
used above these methods return 5 and 30 respectively. However, the UTC helper
methods `getUTCHours()` and `getUTCMinutes()` return 0 and 0.

As I said, the only number that matters for a date is its time value. Everything
else is either cached or computed on the fly using this number and/or the
system time zone.

You can get the time value out from any Date object by using the [`getTime()`][]
method.

```js
new Date(1640995200000).getTime()
// => 1640995200000

// The date components passed to the constructor are interpreted using
// the system time zone.
// Also note that months start from 0 in JS
new Date(2022, 0, 1, 5, 30).getTime()
// => 1640995200000
```

[Date]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
[UTC]: https://en.wikipedia.org/wiki/Coordinated_Universal_Time
[getTime]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime


