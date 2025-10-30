import { describe, test, expect } from "vitest";
import { StyleDeclaration } from "./StyleDeclaration.js";

describe("StyleDeclaration", () => {
  test("Create a new StyleDeclaration", () => {
    const styleDeclaration = new StyleDeclaration();
    expect(styleDeclaration).toBeInstanceOf(StyleDeclaration);
  })
})
