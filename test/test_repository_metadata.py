from __future__ import annotations

import collections
import json
import pathlib
import re
import unittest


REPOSITORY_ROOT = pathlib.Path(__file__).resolve().parent.parent
GITHUB = REPOSITORY_ROOT / ".github"
FORM_NAMES = {"bug.yml", "contract-change.yml", "cross-repository.yml", "feature.yml"}
DEFAULT_LABELS = {"bug", "enhancement"}


class RepositoryMetadataTest(unittest.TestCase):
    def test_repository_profile_is_specific_and_bounded(self) -> None:
        profile = json.loads((GITHUB / "repository.json").read_text(encoding="utf-8"))

        self.assertEqual(
            profile["description"],
            "Deterministic OCaml trading replay engine",
        )
        self.assertEqual(profile["homepage"], "https://fallblu.github.io/trading-engine/")
        self.assertEqual(
            profile["topics"],
            [
                "backtesting",
                "deterministic",
                "event-driven",
                "execution-simulator",
                "json-schema",
                "ocaml",
                "quantitative-finance",
                "trading-engine",
            ],
        )
        self.assertEqual(len(profile["topics"]), len(set(profile["topics"])))
        self.assertFalse(profile["allow_merge_commit"])
        self.assertTrue(profile["allow_rebase_merge"])
        self.assertFalse(profile["allow_squash_merge"])
        self.assertTrue(profile["delete_branch_on_merge"])

    def test_main_branch_protection_matches_integration_policy(self) -> None:
        manifest = json.loads((GITHUB / "branch-protection.json").read_text(encoding="utf-8"))

        self.assertEqual(set(manifest["branches"]), {"main"})
        policy = manifest["branches"]["main"]
        self.assertEqual(
            policy["required_status_checks"],
            {"strict": True, "contexts": ["check", "persistra-compatibility"]},
        )
        self.assertEqual(
            policy["required_pull_request_reviews"],
            {
                "dismiss_stale_reviews": False,
                "require_code_owner_reviews": False,
                "required_approving_review_count": 0,
                "require_last_push_approval": False,
            },
        )
        self.assertTrue(policy["enforce_admins"])
        self.assertTrue(policy["required_conversation_resolution"])
        self.assertTrue(policy["required_linear_history"])
        self.assertFalse(policy["allow_force_pushes"])
        self.assertFalse(policy["allow_deletions"])
        self.assertIsNone(policy["restrictions"])

    def test_label_manifest_covers_stable_planning_dimensions(self) -> None:
        labels = json.loads((GITHUB / "labels.json").read_text(encoding="utf-8"))
        names = [label["name"] for label in labels]
        categories = collections.Counter(label["category"] for label in labels)

        self.assertEqual(len(names), len(set(names)))
        self.assertEqual(
            categories,
            {"component": 10, "priority": 4, "effort": 3, "contract": 2, "dependency": 3},
        )
        self.assertTrue(all(re.fullmatch(r"[0-9a-f]{6}", label["color"]) for label in labels))
        self.assertTrue(all(label["description"].strip() for label in labels))
        self.assertIn("dependency: persistra", names)
        self.assertIn("contract: scenario-v1", names)
        self.assertIn("contract: strategy-v1", names)

    def test_structured_forms_reference_defined_labels_and_require_evidence(self) -> None:
        template_directory = GITHUB / "ISSUE_TEMPLATE"
        forms = {path.name: path for path in template_directory.glob("*.yml") if path.name != "config.yml"}
        labels = json.loads((GITHUB / "labels.json").read_text(encoding="utf-8"))
        allowed_labels = DEFAULT_LABELS | {label["name"] for label in labels}

        self.assertEqual(set(forms), FORM_NAMES)
        for name, path in forms.items():
            text = path.read_text(encoding="utf-8")
            self.assertRegex(text, r"(?m)^name: .+$", name)
            self.assertRegex(text, r"(?m)^description: .+$", name)
            self.assertIn("\nbody:\n", text, name)
            self.assertIn("validations:\n      required: true", text, name)
            label_match = re.search(r"(?m)^labels: \[(.+)\]$", text)
            self.assertIsNotNone(label_match, name)
            assigned = set(json.loads(f"[{label_match.group(1)}]"))
            self.assertLessEqual(assigned, allowed_labels, name)
            ids = re.findall(r"(?m)^    id: ([a-z0-9-]+)$", text)
            self.assertEqual(len(ids), len(set(ids)), name)
        self.assertNotIn("priority:", "\n".join(path.read_text() for path in forms.values()))
        self.assertNotIn("effort:", "\n".join(path.read_text() for path in forms.values()))

        config = (template_directory / "config.yml").read_text(encoding="utf-8")
        self.assertIn("blank_issues_enabled: false", config)
        self.assertIn(".github/SUPPORT.md", config)

    def test_pull_request_and_support_templates_preserve_required_sections(self) -> None:
        pull_request = (GITHUB / "pull_request_template.md").read_text(encoding="utf-8")
        self.assertEqual(pull_request.count("## Summary"), 1)
        self.assertEqual(pull_request.count("## Test plan"), 1)

        support = (GITHUB / "SUPPORT.md").read_text(encoding="utf-8")
        self.assertIn("structured issue forms", support)
        self.assertIn("Do not post credentials", support)

    def test_compatibility_gate_is_pinned_and_canary_is_optional(self) -> None:
        workflow = (GITHUB / "workflows/ci.yml").read_text(encoding="utf-8")
        revision = "2f8beeaab87f4c456f7040f327314afe67306a54"

        self.assertIn(f"PERSISTRA_COMPAT_REVISION: {revision}", workflow)
        self.assertIn("ref: ${{ env.PERSISTRA_COMPAT_REVISION }}", workflow)
        self.assertNotIn("PERSISTRA_COMPAT_REF", workflow)
        self.assertNotIn("vars.", workflow)
        self.assertIn('test "$actual_revision" = "$PERSISTRA_COMPAT_REVISION"', workflow)
        self.assertIn("Persistra compatibility revision: $actual_revision", workflow)
        self.assertIn('>> "$GITHUB_STEP_SUMMARY"', workflow)

        canary = workflow.split("  persistra-latest-head:\n", 1)[1]
        self.assertIn("workflow_dispatch", workflow)
        self.assertIn("inputs.persistra_latest_head", canary)
        self.assertIn("continue-on-error: true", canary)
        self.assertIn("ref: develop", canary)
        self.assertIn("Persistra latest-head canary revision", canary)

        revisions = re.findall(r"uses: [^@\s]+@([^\s]+)", workflow)
        self.assertTrue(revisions)
        self.assertTrue(all(re.fullmatch(r"[0-9a-f]{40}", revision) for revision in revisions))

        compatibility = (REPOSITORY_ROOT / "docs/persistra.md").read_text(encoding="utf-8")
        self.assertIn("versioned files and processes", compatibility)
        self.assertIn("scenario contract v1", compatibility)
        self.assertIn("explicit pair of repository commits", compatibility)
        self.assertIn("Neither repository", compatibility)
        self.assertIn("silently follows a moving branch", compatibility)


if __name__ == "__main__":
    unittest.main()
