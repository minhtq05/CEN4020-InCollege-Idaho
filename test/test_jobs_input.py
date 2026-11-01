"""End-to-end Epic #7 job tests using the application's file input."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
PASSWORD = "Test123!"


class JobFileInputTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.build = tempfile.TemporaryDirectory(prefix="incollege-jobs-build-")
        cls.addClassCleanup(cls.build.cleanup)
        cls.binary = Path(cls.build.name) / "InCollege"
        subprocess.run(
            ["make", f"TARGET={cls.binary}", f"COBC={os.environ.get('COBC', 'cobc')}"],
            cwd=ROOT, check=True, capture_output=True, text=True,
        )

    def setUp(self):
        self.work = tempfile.TemporaryDirectory(prefix="incollege-jobs-test-")
        self.addCleanup(self.work.cleanup)
        self.directory = Path(self.work.name)
        (self.directory / "InCollege-Accounts.txt").write_text(
            f"{'alice':<20}{PASSWORD:<12}\n"
        )
        (self.directory / "InCollege-JobList.txt").write_text(
            f"{1:03}{'poster':<20}{'Software Engineer Intern':<50}"
            f"{'Build and test software with an experienced engineering team.':<200}"
            f"{'Idaho Technology Group':<50}{'Boise, Idaho':<50}{'$22 per hour':<20}\n"
            f"{2:03}{'poster':<20}{'Junior Data Analyst':<50}"
            f"{'Analyze business data and prepare dashboards and reports.':<200}"
            f"{'Gem State Analytics':<50}{'Moscow, Idaho':<50}{'$55,000 per year':<20}\n"
            f"{3:03}{'poster':<20}{'Marketing Internship':<50}"
            f"{'Support campaigns, social media, and market research.':<200}"
            f"{'Sawtooth Creative':<50}{'Remote':<50}{'NONE':<20}\n"
        )

    def run_input(self, lines):
        (self.directory / "InCollege-Input.txt").write_text(
            "\n".join(["1", "alice", PASSWORD, *lines]) + "\n"
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

    def test_browse_summaries_and_return(self):
        output = self.run_input(["3", "2", "0", "3", "6"])
        self.assertIn("Software Engineer Intern | Idaho Technology Group | Boise, Idaho", output)
        self.assertIn("Junior Data Analyst | Gem State Analytics | Moscow, Idaho", output)
        self.assertIn("Marketing Internship | Sawtooth Creative | Remote", output)

    def test_view_full_details(self):
        output = self.run_input(["3", "2", "1", "2", "0", "3", "6"])
        self.assertIn("Title: Software Engineer Intern\n", output)
        self.assertIn("Employer: Idaho Technology Group\n", output)
        self.assertIn("Location: Boise, Idaho\n", output)
        self.assertIn("Description: Build and test software", output)
        self.assertIn("Salary: $22 per hour\n", output)

    def test_apply_and_persist_duplicate_protection(self):
        output = self.run_input(["3", "2", "1", "1", "0", "3", "6"])
        self.assertIn(
            "Your application for Software Engineer Intern at Idaho Technology Group has been submitted.",
            output,
        )
        saved = (self.directory / "InCollege-Applications.txt").read_text()
        self.assertEqual(saved, f"{'alice':<20}{1:03}".rstrip() + "\n")
        output = self.run_input(["3", "2", "1", "1", "0", "3", "6"])
        self.assertIn("You have already applied for this job.\n", output)
        self.assertEqual((self.directory / "InCollege-Applications.txt").read_text(), saved)

    def test_invalid_listing_and_eof_are_safe(self):
        output = self.run_input(["3", "2", "not-a-number", "9", "0", "3", "6"])
        self.assertEqual(output.count("Invalid job ID."), 2)
        output = self.run_input(["3", "2", "1"])
        self.assertNotIn("has been submitted", output)


if __name__ == "__main__":
    unittest.main(verbosity=2)
