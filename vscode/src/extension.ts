import * as vscode from 'vscode'

let logChannel: vscode.OutputChannel | undefined

export function activate(): void {
  logChannel = vscode.window.createOutputChannel('YQStella')
  const msg = '[YQStella] Extension active: Stella grammar registered.'
  logChannel.appendLine(msg)
  console.log(msg)
}

export function deactivate(): void {
  logChannel?.dispose()
  logChannel = undefined
}
