import * as cp from 'child_process'
import * as path from 'path'
import {
  downloadAndUnzipVSCode,
  resolveCliPathFromVSCodeExecutablePath,
} from '@vscode/test-electron'

async function main(): Promise<void> {
  const extensionDevelopmentPath = path.resolve(__dirname, '../..')
  const extensionTestsPath = path.resolve(__dirname, './suite/index.js')

  const vscodeExecutablePath = await downloadAndUnzipVSCode({})
  const codeCli = resolveCliPathFromVSCodeExecutablePath(vscodeExecutablePath)

  const args: string[] = [
    '--no-sandbox',
    '--disable-gpu-sandbox',
    '--disable-updates',
    '--skip-welcome',
    '--skip-release-notes',
    '--disable-workspace-trust',
    `--extensionTestsPath=${extensionTestsPath}`,
    `--extensionDevelopmentPath=${extensionDevelopmentPath}`,
  ]

  const vscodeTestRoot = path.join(process.cwd(), '.vscode-test')
  args.push(
    `--extensions-dir=${path.join(vscodeTestRoot, 'extensions')}`,
    `--user-data-dir=${path.join(vscodeTestRoot, 'user-data')}`,
  )

  const shell = process.platform === 'win32'
  const child = cp.spawn(shell ? `"${codeCli}"` : codeCli, args, {
    stdio: 'inherit',
    env: process.env,
    shell,
  })

  const exitCode = await new Promise<number>((resolve, reject) => {
    child.on('error', reject)
    child.on('exit', (code) => {
      resolve(code ?? 1)
    })
  })

  if (exitCode !== 0) {
    throw new Error(`Tests failed with exit code ${String(exitCode)}`)
  }
}

main().catch((err: unknown) => {
  console.error(err)
  process.exit(1)
})
