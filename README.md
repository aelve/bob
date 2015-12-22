# Bob

## How do people search for characters?

1. **Google:** I'm looking for the rupee sign and I don't even know how it looks. I type `rupee sign`.
2. **Compose key:** I know that I can type letter+`'` to get this letter with acute, so I do this without thinking when I type.
3. **OS X accent menu:** I want to type e.g. ‘¥’ or ‘ÿ’; I press and hold `y` and expect to see those characters there (and then I'll select one of them by pressing 0–9). If I want to type ‘®’, I might press `R` or `r`.
4. **Approximation:** I want to type e.g. ‘⇔’, so I write `<=>`.
5. **Feature composition:** I want to type ‘↗’, so I write `-> diagonal`, or I want to type ‘©’ and write `c o`.
6. **Proper names:** I want to type ‘ő’, but ultimately I want to write ‘Erdős’, so I enter `erdos`.

This gives us a list of things we need:

  * **Google:** imprecise search; instant/partial search (`ru`/`rup`/`rupe`)
  * **Compose key:** for every character there's a base character; rules don't have exceptions (e.g. letter+apostrophe always gives acute)
  * **OS X accent menu:** for every character there's a base character; most popular characters are ranked high; characters can be selected quickly (e.g. by pressing 0–9)
  * **Approximation:** a list of possible/known approximations
  * **Feature composition:** for every character there's a list of features; when the base character is pressed, the list of possible added features and corresponding characters is shown
  * **Proper names:** a list of proper names
