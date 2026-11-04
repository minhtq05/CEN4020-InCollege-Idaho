"""End-to-end Epic #7 job tests using the application's file input."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
PASSWORD = "Test123!"
SEPARATOR = "------------------------------\n"


def report(username, *jobs):
    """Expected application report for (id, title, employer, location) jobs."""
    entries = "---\n".join(
        f"Job ID: {job_id}\nJob Title: {title}\nEmployer: {employer}\nLocation: {location}\n"
        for job_id, title, employer, location in jobs
    )
    if not jobs:
        entries = "You have not applied for any jobs yet.\n"
    return (
        f"--- Your Job Applications ---\nApplication Summary for {username}\n"
        f"{SEPARATOR}{entries}{SEPARATOR}Total Applications: {len(jobs)}\n{SEPARATOR}"
    )


INTERN = (1, "Software Engineer Intern", "Idaho Technology Group", "Boise, Idaho")
ANALYST = (2, "Junior Data Analyst", "Gem State Analytics", "Moscow, Idaho")
MARKETING = (3, "Marketing Internship", "Sawtooth Creative", "Remote")


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
            f"{'alice':<20}{PASSWORD:<12}\n{'bob':<20}{PASSWORD:<12}\n"
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

    def applications(self):
        return (self.directory / "InCollege-Applications.txt").read_text()

    def test_job_menu_lists_view_my_applications(self):
        output = self.run_input(["3", "5", "6"])
        self.assertIn(
            "--- Job Search/Internship Menu ---\n1. Post a Job/Internship\n"
            "2. Browse Jobs/Internships\n3. View My Applications\n4. Saved Jobs\n"
            "5. Back to Main Menu\nEnter your choice:\n5\n1. Create/Edit My Profile\n",
            output,
        )

    def test_browse_summaries_and_return(self):
        output = self.run_input(["3", "2", "0", "5", "6"])
        self.assertIn(
            "--- Available Job Listings ---\n"
            "1. Software Engineer Intern at Idaho Technology Group (Boise, Idaho)\n"
            "2. Junior Data Analyst at Gem State Analytics (Moscow, Idaho)\n"
            "3. Marketing Internship at Sawtooth Creative (Remote)\n"
            "-----------------------------\n"
            "Enter job number to view details, or 0 to go back:\n0\n"
            "--- Job Search/Internship Menu ---\n",
            output,
        )

    def test_browse_empty_job_list(self):
        (self.directory / "InCollege-JobList.txt").unlink()
        output = self.run_input(["3", "2", "5", "6"])
        self.assertIn(
            "--- Available Job Listings ---\nThere are no jobs available.\n"
            "--- Job Search/Internship Menu ---\n",
            output,
        )

    def test_browse_single_job(self):
        jobs = (self.directory / "InCollege-JobList.txt").read_text().splitlines()
        (self.directory / "InCollege-JobList.txt").write_text(jobs[0] + "\n")
        output = self.run_input(["3", "2", "0", "5", "6"])
        self.assertIn(
            "--- Available Job Listings ---\n"
            "1. Software Engineer Intern at Idaho Technology Group (Boise, Idaho)\n"
            "-----------------------------\n",
            output,
        )

    def test_view_full_details(self):
        output = self.run_input(["3", "2", "1", "2", "0", "5", "6"])
        self.assertIn(
            "--- Job Details ---\nTitle: Software Engineer Intern\n"
            "Description: Build and test software with an experienced engineering team.\n"
            "Employer: Idaho Technology Group\nLocation: Boise, Idaho\n"
            "Salary: $22 per hour\n-------------------\n"
            "1. Apply for this Job\n2. Back to Job List\nEnter your choice:\n2\n"
            "--- Available Job Listings ---\n",
            output,
        )
        self.assertFalse((self.directory / "InCollege-Applications.txt").exists())

    def test_details_without_salary(self):
        output = self.run_input(["3", "2", "3", "2", "0", "5", "6"])
        self.assertIn("Location: Remote\n-------------------\n", output)
        self.assertNotIn("Salary:", output)

    def test_apply_and_persist_duplicate_protection(self):
        output = self.run_input(["3", "2", "1", "1", "0", "5", "6"])
        self.assertIn(
            "Your application for Software Engineer Intern at Idaho Technology Group "
            "has been submitted.\n--- Available Job Listings ---\n",
            output,
        )
        saved = self.applications()
        self.assertEqual(saved, f"{'alice':<20}{1:03}\n")
        output = self.run_input(["3", "2", "1", "1", "0", "5", "6"])
        self.assertIn("You have already applied for this job.\n", output)
        self.assertEqual(self.applications(), saved)

    def test_invalid_listing_and_detail_choices(self):
        output = self.run_input(["3", "2", "not-a-number", "9", "", "1", "x", "2", "0", "5", "6"])
        self.assertEqual(output.count("Invalid job number."), 3)
        self.assertIn("Enter your choice:\nx\nInvalid choice, please try again.\n", output)
        self.assertFalse((self.directory / "InCollege-Applications.txt").exists())

    def test_eof_is_safe(self):
        for lines in (["3", "2"], ["3", "2", "1"], ["3", "3"]):
            output = self.run_input(lines)
            self.assertNotIn("has been submitted", output)
        self.assertFalse((self.directory / "InCollege-Applications.txt").exists())

    def test_report_with_no_applications(self):
        output = self.run_input(["3", "3", "5", "6"])
        self.assertIn(report("alice") + "--- Job Search/Internship Menu ---\n", output)

    def test_report_with_one_application_after_restart(self):
        self.run_input(["3", "2", "2", "1", "0", "5", "6"])
        output = self.run_input(["3", "3", "5", "6"])
        self.assertIn(report("alice", ANALYST), output)

    def test_report_with_multiple_applications(self):
        output = self.run_input(["3", "2", "3", "1", "1", "1", "0", "3", "5", "6"])
        self.assertIn(report("alice", MARKETING, INTERN), output)

    def test_report_only_shows_current_user_and_known_jobs(self):
        (self.directory / "InCollege-Applications.txt").write_text(
            f"{'alice':<20}{1:03}\n{'bob':<20}{2:03}\n"
            f"{'alice':<20}{99:03}\n{'alice':<20}{3:03}\n"
        )
        output = self.run_input(["3", "3", "5", "6"])
        self.assertIn(report("alice", INTERN, MARKETING), output)
        output = self.run_input(["3", "3", "5", "6"], username="bob")
        self.assertIn(report("bob", ANALYST), output)


if __name__ == "__main__":
    unittest.main(verbosity=2)
