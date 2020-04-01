(import 'xml :all)


(defvar xml-text (load-file "xml_file.xml"))

(dump xml-text)

(dump (dump-file xml-text "new_xml.xml"))
