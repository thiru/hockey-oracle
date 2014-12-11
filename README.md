# Hockey Oracle

## Overview

An aid for pick-up hockey enthusiasts.

**NOTE:** So far, I've only implemented the ability to generate two teams selected randomly from a pool of active players.

## Development

### Prerequisites

1. [Node.js](http://nodejs.org/) (with NPM).
2. [nodemon](https://github.com/remy/nodemon)
  * This isn't strictly required however, the handy *start* script uses it to start the web server
  * Run the following to install it:
    * `npm -g install nodemon`
3. [browserify](http://browserify.org/)
  * This is used to generate the javascript used throughout the website
  * Run the following to install it:
    * `npm -g install browserify`
4. [watchify](https://github.com/substack/watchify)
  * This is used to automate updating the javascript used throughout the website
  * Run the following to install it:
    * `npm -g install watchify`

### Running the website

1. Open a command prompt and go to the directory containing the source
2. `watchify -d public/components/app.js -o public/components/bundle.js -v `
3. Open another command prompt and go to the directory containing the source
4. `npm start`
