-module(time).
-export([swedish_date/0]).

swedish_date() ->
	extract_year()+modify_month()+modify_day().

extract_year() ->
	{Year, _, _} = date(),
	(Year rem 100)*10000.
	
modify_month() ->
	{_, Month, _} = date(),
	(Month*100).

modify_day() ->
	{_, _, Day} = date(),
	Day.
