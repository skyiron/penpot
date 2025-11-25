import * as h from "./_helpers.js";

await h.compileStorybookStyles();
await h.copyAssets();
await h.compileSvgSprites();
await h.compileTranslations();
await h.compileTemplates();
await h.compilePolyfills();
