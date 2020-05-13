xxx_test_1 <- function(){

  folder <- sc::path("output","sykdomspulsen_data_output","test",create_dir=T)
  tempdir <- tempdir()

  rmarkdown::render(
    input = system.file("rmd/test_pdf.Rmd", package="sykdomspulsen"),
    output_dir = tempdir,
    output_file = "test.pdf",
    intermediates_dir = tempdir
  )

  rmarkdown::render(
    input = system.file("rmd/test_word.Rmd", package="sykdomspulsen"),
    output_dir = tempdir,
    output_file = "test.docx",
    intermediates_dir = tempdir
  )

  sc::mv(
    from = fs::path(tempdir,"test.docx"),
    to = fs::path(folder,"test.docx")
    )

}
