const got = require('got')

const url = "https://package.elm-lang.org/packages/elm/core/latest/docs.json";
const responsePromise = got(url);
const jsonPromise = responsePromise.json();

jsonPromise
	.then((data, i) => {
		const values = data.map(d => JSON.stringify(JSON.stringify(d)).slice(1, -1));
		return console.log(`"""[${values.join("\n,")}]"""`);
	})
