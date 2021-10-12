from __future__ import annotations

from dataclasses import dataclass
import os.path
from typing import List, Optional


def indent(text: str, prefix: str = "  ") -> str:
    """
    Indent all lines in `text` with the given prefix.

    :param text: Text to indent.
    :param prefix: Indentation string.
    """
    return "\n".join(prefix + line for line in text.splitlines())


def env_path_split(path_list: Optional[str]) -> List[str]:
    """
    Split a *PATH environment variable into a list of paths.

    :param path_list: Content of a *PATH environment variable, for instance
        PYTHONPATH.
    """
    return path_list.split(os.path.pathsep) if path_list else []


def env_path_format(path_list: List[str]) -> str:
    """
    Format a list of paths for a *PATH environment variable.

    :param path_list: List of paths.
    """
    return os.path.pathsep.join(path_list)


@dataclass
class TestsuiteConfig:
    no_auto_pythonpath: bool
    """Whether to automatically add "gnatdbg" to the PYTHONPATH."""

    coverage: bool
    """Whether gnatdbg code coverage is enabled."""

    coverage_dir: str
    """Path to the directory where to store coverage data for each test."""

    coverage_rcfile: str
    """Path to the configuration file for Python's coverage module."""
