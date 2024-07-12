doc:
	R -s -e "roxygen2::roxygenize('pkg', load_code = roxygen2::load_pkgload)"

pkg: doc
	rm -f *.tar.gz
	R CMD build pkg

install: pkg
	R CMD INSTALL *.tar.gz

check: pkg
	R CMD check *.tar.gz

cran: pkg
	R CMD check --as-cran *.tar.gz

manual: doc
	R CMD Rd2pdf --force -o manual.pdf ./pkg

clean:
	rm -rf *.Rcheck
	rm -rf revdep
	rm -f *.tar.gz
	rm -f manual.pdf
