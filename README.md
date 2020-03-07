# Elm Donation Demo

An Elm application to set up donations, as outlined in https://discourse.elm-lang.org/t/help-with-foundation-elm-lang-org/5276.

NOTE: Currently there is no real wiring to get the checkout ID.
Also, the "custom amount" is not implemented yet. Need to think how to best expose it in an accessible way. It would probably be a radio button such as "Custom amount (select to show input)" or something along those lines.

## Prerequisites

You will need to [install the Elm compiler](https://guide.elm-lang.org/install/elm.html)

You will also need a terminal, such as Terminal on Mac, Gnome Terminal on Linux, or Powershell on Windows.

Optionally, if you want to use the development toolchain that watches and updates files, you will need to [install Node](https://nodejs.org/en/download/).

## Build

The main Elm files are under `src`.
Static files, like the HTML and CSS are under the `static` directory.

To build this project, in a terminal, type:

```shell
mkdir dist
cp static/* dist/
elm make src/Donate.elm --output dist/donate.js --optimize
```

The resulting files should be under the `dist` directory!

### Using npm scripts

Alternatively, if you have Node installed, you have access to `npm` scripts.

You can then type in a terminal:

```shell
npm ci
npm run build
```

And it should hopefully have the same effect!
(Note: I have not tested this on Windows; it is likely that you will need to change the scripts in package.json for that to work. Please send a PR if you work this out)

## Development

For development, I would recommend installing Node as mentioned previously.

Then, in a terminal, type:

```shell
npm ci
npm run dev
```

If you navigate to localhost:5000, you should see the application.
