
#------------------------------------------------------------------------------

#Optical character recognition (OCR) non-native PDF

#------------------------------------------------------------------------------


check_and_load_package("tesseract")

if(is.na(match("nld", tesseract_info()$available)))
  tesseract_download("nld")
nl <- tesseract("nld")

#numbers(engine parameter)
#ocr_numbers <- tesseract(options = list(tessedit_char_whitelist = ".0123456789"))

#High quality conversion of pdf page(s) to png, jpeg or tiff format, or render into a raw bitmap array
image_tc<-pdftools::pdf_convert('https://www.raadleiderdorp.nl/documenten/Ingekomen-brieven/Opgave-langer-zelfstandig-wonen-voor-gemeente-brief-Aanjaagteam-Langer-Zelfstandig-Wonen-150112-bijlage.pdf', 
                                format="png", dpi = 600)

text_pdf <- tesseract::ocr(image_tc, engine = nl)
cat(text_pdf)