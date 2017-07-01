def indent(text, prefix='  '):
    """
    Indent all lines in `text` with the given prefix.

    :param str text: Text to indent.
    :param str prefix: Indentation string.
    :rtype: str
    """
    return '\n'.join(prefix + line for line in text.splitlines())
