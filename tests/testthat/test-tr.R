# .tr is not exported
test_that(".tr returns Finnish label when kieli = 'suomi'", {
  expect_equal(sostieteidentaulukointityylit:::.tr("ka", kieli = "suomi"), "ka")
  expect_equal(sostieteidentaulukointityylit:::.tr("Muuttuja", kieli = "suomi"), "Muuttuja")
})

test_that(".tr returns English label when kieli = 'eng'", {
  expect_equal(sostieteidentaulukointityylit:::.tr("ka",      kieli = "eng"), "mean")
  expect_equal(sostieteidentaulukointityylit:::.tr("kh",      kieli = "eng"), "sd")
  expect_equal(sostieteidentaulukointityylit:::.tr("Muuttuja",kieli = "eng"), "Variable")
  expect_equal(sostieteidentaulukointityylit:::.tr("vi",      kieli = "eng"), "skew")
  expect_equal(sostieteidentaulukointityylit:::.tr("hu",      kieli = "eng"), "kurt")
})

test_that(".tr returns the key unchanged for an unknown key", {
  expect_equal(sostieteidentaulukointityylit:::.tr("XyzUnknown", kieli = "suomi"), "XyzUnknown")
  expect_equal(sostieteidentaulukointityylit:::.tr("XyzUnknown", kieli = "eng"),   "XyzUnknown")
})

test_that(".tr covers all expected translation keys for both languages", {
  keys <- c("Luottamusvali", "Vetosuhde", "Selitettava", "Kommunaliteetti",
            "Ominaisarvo", "Selitysosuus", "R2, korjattu", "Malli",
            "minimi", "maksimi", "lv_alaraja", "lv_ylaraja")
  for (k in keys) {
    expect_type(sostieteidentaulukointityylit:::.tr(k, kieli = "suomi"), "character")
    expect_type(sostieteidentaulukointityylit:::.tr(k, kieli = "eng"),   "character")
  }
})
