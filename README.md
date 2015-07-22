#HsRegex

##Goals

HsRegex provides a way to express regular expressions in a native Haskell
functional way. This library seeks to combine the Haskell function flexibility
with regular expressions. Unlike other regular expression engines that express
patterns as string, HsRegex exposes functions that can be combined together to
match a pattern directly in Haskell.

##Usage

The regular expressions are composed with the help of a list of functions and
the (=~) operator.

###Example:
```
*HsRegex> let str = "The ssssnake says sssss..."
*HsRegex> sstr =~ [char 'T', spc, star (char 's')]
["The ssss"]
```

## List of patterns

Function   | Pattern
-----------|----------------
 char     |  any character
 dot      |  .
 endl     |  $
 stl      |  ^
 spc      |  \s
 notSpc   |  opposite of \s
 wc       |  \w
 notWc    |  opposite of \w
 digit    |  \d
 notDigit |  opposite of \d
 alnum    |  [:alnum:]
 plus     |  +
 star     |  *
 pipe     |  \|
 range    |  []
 notRange |  [^]
 qMark    |  ?
 wb       |  \b
 reGroup  |  (regex)
 var      |  $N where N is group number
 mN       |  {N}
 mLN      |  {,N}
 mN1N2    |  {N_1, N_2}

## TODOs
- Stop greedy operators from consuming text belonging the next regexp match.
##Maintener
- Author: Groza Cristian
- Email: kristi9524@gmail.com

