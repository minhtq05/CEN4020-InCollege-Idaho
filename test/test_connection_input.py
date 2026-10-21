"""End-to-end connection tests using only InCollege-Input.txt."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
PASSWORD = "Test123!"


def profile(username, first, last):
    """A fixed-width record matching InCollege-ProfileRecord.cpy."""
    return (
        f"{username:<20}{first:<30}{last:<30}"
        f"{'Test University':<50}{'Computer Science':<50}2027"
        f"{'Hello':<200}1"
        f"{'Intern':<50}{'Example Company':<50}{'2025-2026':<30}"
        f"{'Built tests':<100}" + " " * (230 * 2)
        + "1" + f"{'BS':<50}{'Test University':<50}{'2023-2027':<20}"
        + " " * (120 * 2) + "Y\n"
    )


def request(sender="alice", receiver="bob", status="PENDING"):
    return f"{sender:<20}{receiver:<20}{status:<10}".rstrip() + "\n"


class ConnectionFileInputTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.build = tempfile.TemporaryDirectory(prefix="incollege-build-")
        cls.addClassCleanup(cls.build.cleanup)
        cls.binary = Path(cls.build.name) / "InCollege"
        subprocess.run(
            ["make", f"TARGET={cls.binary}", f"COBC={os.environ.get('COBC', 'cobc')}"],
            cwd=ROOT, check=True, capture_output=True, text=True,
        )

    def setUp(self):
        self.work = tempfile.TemporaryDirectory(prefix="incollege-test-")
        self.addCleanup(self.work.cleanup)
        self.directory = Path(self.work.name)
        (self.directory / "InCollege-Accounts.txt").write_text(
            "".join(f"{name:<20}{PASSWORD:<12}\n" for name in ("alice", "long", "bob"))
        )
        (self.directory / "InCollege-Profiles.txt").write_text(
            profile("alice", "Alice", "Tester")
            + profile("long", "Alexandria", "Longlastname")
            + profile("bob", "Bob", "Tester")
        )

    def run_input(self, lines, username="alice"):
        (self.directory / "InCollege-Input.txt").write_text(
            "\n".join(["1", username, PASSWORD, *lines]) + "\n"
        )
        result = subprocess.run(
            [str(self.binary)], cwd=self.directory, stdin=subprocess.DEVNULL,
            capture_output=True, text=True, timeout=5, check=True,
        )
        self.assertEqual(result.stderr, "")
        self.assertEqual(
            result.stdout, (self.directory / "InCollege-Output.txt").read_text()
        )
        return result.stdout

    def saved_requests(self):
        path = self.directory / "InCollege-Requests.txt"
        return path.read_text() if path.exists() else ""

    def test_send_and_reload_pending_request(self):
        output = self.run_input(["4", "Bob Tester", "1", "6"])
        self.assertIn("Connection request sent successfully.\n", output)
        self.assertIn("Profile of Bob Tester\n", output)
        self.assertIn("  - Intern at Example Company (2025-2026)\n", output)
        self.assertIn("  - BS, Test University (2023-2027)\n", output)
        self.assertEqual(self.saved_requests(), request())
        output = self.run_input(["7", "6"], username="bob")
        self.assertIn("Pending connection requests:\n  From: alice\n", output)

    def test_back_does_not_send(self):
        self.run_input(["4", "Bob Tester", "2", "6"])
        self.assertEqual(self.saved_requests(), "")

    def test_invalid_choices_retry_from_file(self):
        output = self.run_input(["4", "Bob Tester", "wrong", "", "3", "1", "6"])
        self.assertEqual(output.count("Invalid choice, please try again."), 3)
        self.assertIn("wrong\n", output)
        self.assertEqual(self.saved_requests(), request())

    def test_duplicate_in_same_run(self):
        output = self.run_input(["4", "Bob Tester", "1", "4", "Bob Tester", "1", "6"])
        self.assertIn("You have already sent a connection request to this user.", output)
        self.assertEqual(self.saved_requests(), request())

    def test_duplicate_after_restart(self):
        (self.directory / "InCollege-Requests.txt").write_text(request())
        output = self.run_input(["4", "Bob Tester", "1", "6"])
        self.assertIn("You have already sent a connection request to this user.", output)
        self.assertEqual(self.saved_requests(), request())

    def test_incoming_request(self):
        incoming = request("bob", "alice")
        (self.directory / "InCollege-Requests.txt").write_text(incoming)
        output = self.run_input(["4", "Bob Tester", "1", "7", "6"])
        self.assertIn("This user already sent you a request. See option 7.", output)
        self.assertIn("  From: bob\n", output)
        self.assertEqual(self.saved_requests(), incoming)

    def test_eof_at_each_connection_prompt(self):
        for lines in (["4"], ["4", "Bob Tester"], ["4", "Bob Tester", "invalid"]):
            with self.subTest(lines=lines):
                output = self.run_input(lines)
                self.assertNotIn("Connection request sent successfully.", output)
                self.assertEqual(self.saved_requests(), "")

    def test_eof_after_send(self):
        self.run_input(["4", "Bob Tester", "1"])
        self.assertEqual(self.saved_requests(), request())

    def test_repeated_search_and_back(self):
        output = self.run_input([
            "4", "Alexandria Longlastname", "2", "4", "Bob Tester", "1", "6"
        ])
        self.assertIn("Profile of Bob Tester\n", output)
        self.assertEqual(self.saved_requests(), request())


if __name__ == "__main__":
    unittest.main(verbosity=2)
