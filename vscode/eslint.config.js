"use strict";

const js = require("@eslint/js");
const globals = require("globals");
const tseslint = require("typescript-eslint");
const stylistic = require("@stylistic/eslint-plugin");
const { defineConfig, globalIgnores } = require("eslint/config");

module.exports = defineConfig([
  globalIgnores(["dist", "node_modules", ".vscode-test"]),
  {
    files: ["**/*.ts"],
    extends: [
      js.configs.recommended,
      ...tseslint.configs.strictTypeChecked,
      ...tseslint.configs.stylisticTypeChecked,
      stylistic.configs.recommended,
    ],
    languageOptions: {
      ecmaVersion: 2024,
      globals: {
        ...globals.node,
        ...globals.mocha,
      },
      parserOptions: {
        projectService: true,
        tsconfigRootDir: __dirname,
      },
    },
  },
]);
