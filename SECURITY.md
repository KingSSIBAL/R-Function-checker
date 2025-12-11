# Security Policy

## Supported versions

R-Function-checker is under active development. The latest `main` branch and the most recent tagged release are considered supported. Older versions may not receive fixes.

## Reporting a vulnerability

If you discover a security-related issue (for example, involving authentication, token handling, encrypted test cases, or data access), please follow these steps:

- **Do not** open a public GitHub issue describing the vulnerability.
- Instead, contact the maintainer directly at `rcagub@up.edu.ph` with:
  - A clear description of the issue
  - Steps to reproduce or conditions where it occurs
  - Any proof-of-concept code, if available
  - Your thoughts on potential impact and mitigation

You will receive an acknowledgment after your report is reviewed. If appropriate, a fix will be prepared and released, and documentation will be updated.

## Token and credential safety

When using secure mode with GitHub tokens and `.env` files:

- Never commit `.env` or any file containing secrets to a public repository.
- Use tokens with the **minimum required scopes** (e.g., read access for content where possible).
- Rotate tokens periodically and revoke tokens that are no longer needed.

## Encryption and test content

The project includes encryption helpers to protect test case content and URLs. When extending or modifying this functionality:

- Avoid weakening encryption primitives or key management.
- Do not log secrets, tokens, or decrypted content.
- Be cautious when adding debug output around authentication or decryption.

If you are unsure whether something has security implications, please ask via email before opening a public discussion.
