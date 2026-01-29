-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: CC0-1.0
import Distribution.Simple
import VexRiscv.Setup (VexRiscvSource (VexRiscvBundled), addVexRiscvHooks)

main :: IO ()
main = defaultMainWithHooks (addVexRiscvHooks simpleUserHooks "data" ["TestCpu"] VexRiscvBundled)
