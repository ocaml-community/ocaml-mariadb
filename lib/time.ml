type kind = [`Time | `Timestamp | `Date | `Datetime]

type t =
  { year : int
  ; month : int
  ; day : int
  ; hour : int
  ; minute : int
  ; second : int
  ; kind : kind
  }

let year t = t.year
let month t = t.month
let day t = t.day
let hour t = t.hour
let minute t = t.minute
let second t = t.second

let time ~hour ~minute ~second =
  { year = 0
  ; month = 0
  ; day = 0
  ; hour
  ; minute
  ; second
  ; kind = `Time
  }

let timestamp f t =
  let tm = f t in
  { year = tm.Unix.tm_year
  ; month = tm.Unix.tm_mon
  ; day = tm.Unix.tm_mday
  ; hour = tm.Unix.tm_hour
  ; minute = tm.Unix.tm_min
  ; second = tm.Unix.tm_sec
  ; kind = `Timestamp
  }

let local_timestamp t =
  timestamp Unix.localtime t

let utc_timestamp t =
  timestamp Unix.gmtime t

let date ~year ~month ~day =
  { year
  ; month
  ; day
  ; hour = 0
  ; minute = 0
  ; second = 0
  ; kind = `Date
  }

let datetime ~year ~month ~day ~hour ~minute ~second =
  { year
  ; month
  ; day
  ; hour
  ; minute
  ; second
  ; kind = `Datetime
  }
