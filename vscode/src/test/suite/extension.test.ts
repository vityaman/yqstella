import * as assert from 'assert'
import * as vscode from 'vscode'

suite('YQStella extension', () => {
  test('contributes Stella language id', async () => {
    const ids = await vscode.languages.getLanguages()
    assert.ok(ids.includes('stella'))
  })
})
