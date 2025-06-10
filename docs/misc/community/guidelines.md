# Contribution Guidelines

Thank you for your interest in contributing to HyperBEAM! This page outlines the process for contributing to the project and provides guidelines to ensure your contributions align with the project's goals and standards.

## Code of Conduct

By participating in this project, you agree to abide by our Code of Conduct. We expect all contributors to be respectful, inclusive, and considerate in all interactions.

## Ways to Contribute

There are many ways to contribute to HyperBEAM:

- **Code contributions**: Implementing new features or fixing bugs
- **Documentation**: Improving or adding to the documentation
- **Testing**: Writing tests or manually testing functionality
- **Bug reports**: Reporting issues you encounter
- **Feature requests**: Suggesting new features or improvements
- **Community support**: Helping other users in forums or discussions

## Getting Started

1. **Fork the repository**: Create your own fork of the [HyperBEAM repository](https://github.com/permaweb/HyperBEAM).
2. **Set up your development environment**: Follow the setup instructions in the [Development Setup](setup.md) guide.
3. **Find an issue to work on**: Look for issues labeled "good first issue" or "help wanted" in the [GitHub issue tracker](https://github.com/permaweb/HyperBEAM/issues).
4. **Create a branch**: Create a new branch for your work with a descriptive name.

## Development Workflow

### Making Changes

1. **Make your changes**: Implement your feature or fix the bug.
2. **Follow coding standards**: Ensure your code follows the project's [coding standards](#coding-standards).
3. **Write tests**: Add tests for your changes to ensure functionality and prevent regressions.
4. **Update documentation**: Update or add documentation to reflect your changes.

### Submitting Changes

1. **Commit your changes**: Use clear and descriptive commit messages.
2. **Push to your fork**: Push your changes to your GitHub fork.
3. **Create a pull request**: Submit a pull request from your fork to the main repository.
4. **Describe your changes**: In the pull request, describe what you've changed and why.
5. **Link related issues**: Link any related issues in your pull request description.

### Code Review Process

1. **Initial review**: A maintainer will review your pull request for basic compliance.
2. **Feedback**: You may receive feedback requesting changes or clarification.
3. **Iteration**: Make requested changes and push them to your branch.
4. **Approval**: Once approved, a maintainer will merge your changes.

## Coding Standards

### Erlang Code

- Look at the existing code and match its style
- If you see a pattern in how things are written, follow it
- If you're not sure, check what the majority of the codebase does
- Write tests that match the existing test style
- Focus on writing working code, not debating style
- Remember: Cypherpunks write code!

### JavaScript/Node.js Code

- Follow the [Airbnb JavaScript Style Guide](https://github.com/airbnb/javascript)
- Use async/await for asynchronous operations
- Document functions with JSDoc comments
- Use meaningful variable and function names
- Write unit tests for all functionality

### General Guidelines

- Keep commits focused on a single change
- Write clear commit messages that explain *why* a change was made
- Avoid large, monolithic pull requests
- Add comments for complex logic
- Prioritize readability and maintainability

## Documentation Guidelines

- Use Markdown for all documentation
- Follow a consistent structure and style
- Include examples where appropriate
- Update documentation when changing functionality
- Ensure links work correctly
- Check spelling and grammar

## Testing Guidelines

- Write unit tests for all new functionality
- Ensure tests are deterministic (no flaky tests)
- Mock external dependencies
- Consider edge cases in your tests
- Aim for high test coverage
- Include integration tests where appropriate

## Reporting Bugs

When reporting bugs, please include:

1. A clear, descriptive title
2. Steps to reproduce the issue
3. Expected behavior
4. Actual behavior
5. Environment details (OS, versions, etc.)
6. Screenshots or logs if applicable

## Requesting Features

When requesting features, please include:

1. A clear, descriptive title
2. A detailed description of the feature
3. The problem it solves or benefit it provides
4. Any alternative solutions you've considered
5. Mockups or examples if applicable

## Pull Request Template

When creating a pull request, please use the following template:

```markdown
## Description
[Describe the changes you've made]

## Related Issue
[Link to the related issue]

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Documentation update
- [ ] Performance improvement
- [ ] Code refactoring
- [ ] Other (please describe)

## Checklist
- [ ] I have read the contribution guidelines
- [ ] My code follows the project's coding standards
- [ ] I have added tests that prove my fix/feature works
- [ ] I have updated documentation to reflect my changes
- [ ] All new and existing tests pass
```

## Communication Channels

- **GitHub Issues**: For bug reports and feature requests
- **Discord**: For general discussion and questions
- **Pull Requests**: For code review and discussion of implementations

Thank you for contributing to HyperBEAM! 