// Custom make-entry-apa function
#let make-entry-apa(
  fields,
  multilingual,
  me: none, 
  lang: "en"
) = {
  // Extract the list of authors from the fields
  let authors = fields.author

  // Function to format a list of authors into a string
  let format_authors(auths, max_count, me) = {
    // Initialize an empty list to hold the formatted authors
    let formatted_authors = ()

    // Determine the number of authors to display, limited by `max_count`
    let count = calc.min(auths.len(), max_count) - 1

    // Create a list of indices for authors using a while loop
    let indices = ()
    let i = 0
    while i <= count {
      indices.push(i)
      i += 1
    }

    // Format each author into "Last, F." format and add to the list
    for author in indices {
      let index = indices.at(author)
      let parts = auths.at(index).split(", ")
      let author = text(parts.at(0) + ", " + parts.at(1).first() + ".")

      // Highlight the author's name if it matches `me`
      if not me == none and author == me {
        if "corresponding" in fields and fields.corresponding == true {
          author = strong(me + super("C"))  // Add a superscript "C" for corresponding author
        } else {
          author = strong(me)
        }
      }
      formatted_authors.push(author)
    }

    // Return the formatted list of authors, appending "et al." if necessary
    if auths.len() > max_count {
      return [#formatted_authors.join(", ") _et. al._]
    } else {
      return [#formatted_authors.join(", ", last: " & ")]
    }
  }

  // Function to format editors if present
  let format_editors(eds, max_count, me) = {
    // Initialize an empty list to hold the formatted editors
    let formatted_editors = ()

    // Determine the number of authors to display, limited by `max_count`
    let count = calc.min(eds.len(), max_count) - 1

    // Create a list of indices for authors using a while loop
    let indices = ()
    let i = 0
    while i <= count {
      indices.push(i)
      i += 1
    }

    // Format each editor into "F. Last" format and add to the list
    for editor in indices {
      let index = indices.at(editor)
      let parts = eds.at(index).split(", ")
      let editor = text(parts.at(1).first() + ". " + parts.at(0))

      // Highlight the author's name if it matches `me`
      if not me == none and editor == me {
        if "corresponding" in fields and fields.corresponding == true {
          editor = strong(me + super("C"))  // Add a superscript "C" for corresponding author
        } else {
          editor = strong(me)
        }
      }
      formatted_editors.push(editor)
    }

    // Return the formatted list of editors
    if eds.len() > max_count {
      return [#formatted_editors.join(", ") _et. al._]
    } else {
      return [#formatted_editors.join(", ", last: " & ")]
    }
  }
  
  // Function to format the publication date or publication state
  let format_date(fields, lang) = {
    // Handle cases where no date is available
    if not "date" in fields {
      if not "pubstate" in fields {
        return [ (n.d.). ]  // Return "n.d." (no date) if neither date nor pubstate is available
      } else {
        // Handle cases with a publication state
        let header = fields.at("pubstate")
        let search = "pubstate-" + header

        // Lookup the publication state string in the i18n data
        let subset = multilingual.lang.at(lang)
        if search in subset {
          return [ (#subset.at(search)).]
        } else {
          return [. ]
        }
      }
    } else {
      return [ (#fields.date). ]  // Return the formatted date
    }
  }

  // Function to format the title of the work
  let format_title(title, lang) = {
    // Handle multilingual titles stored in a dict
    if type(title) == dictionary {
      if not lang in title {
        return [ #fields.title.main]  // Return the main title if the specified language is not available
      } else {
        // If the title is available in the document's language, format it accordingly
        for entry in fields.title.keys() {
          if lang in fields.title.keys() {
            return [ #fields.title.main \[#fields.title.at(lang)\]]
          } else {
            return [ #emph("Missing title for document language")]
          }
        } 
      }
    } else {
      return [ #fields.title]  // Return the title as is if it's a simple string
    }
  }

  // Initialize an empty string for building the reference
  let reference = text("")

  // Handle references of type "article"
  if fields.type == "article" {
    reference += format_authors(authors, 6, me)
    reference += format_date(fields, lang)
    reference += format_title(fields.title, lang)

    // Handle special case where the publication state is provided
    if "pubstate" in fields {
      reference += [.]
    } else {
      // Add parent information such as the title of the journal or volume number
      if "parent" in fields and "title" in fields.parent {
        reference += [. #emph(fields.parent.title)]
        // Only add comma if volume or issue follows
        if "parent" in fields and ("volume" in fields.parent or "issue" in fields.parent) {
          reference += [,]
        }
      }
      if "parent" in fields and "volume" in fields.parent {
        reference += [ #emph(str(fields.parent.volume))]
      }
      if "parent" in fields and "issue" in fields.parent {
        reference += [(#emph(str(fields.parent.issue)))]
      }
      if "page-range" in fields {
        reference += [ #fields.page-range.]
      } else if "parent" in fields and "title" in fields.parent {
        // Add period if we have a parent title but no page range
        reference += [.]
      }
      if "serial-number" in fields and "doi" in fields.serial-number {
        let url = "https://doi.org/" + fields.serial-number.doi
        reference += [ #link(url)[#url]]
      }
    }
  } else if fields.type == "chapter" { // Handle references of type "chapter"
    reference += format_authors(authors, 6, me)
    reference += format_date(fields, lang)
    reference += format_title(fields.title, lang)

    // Include information about the parent work (e.g., book or edited volume)
    if "parent" in fields and "author" in fields.parent {
      reference += [. In #format_editors(fields.parent.author, 6, me) (Eds.),]
    } else {
      reference += " In "
    }
    if "parent" in fields and "title" in fields.parent {
      reference += [ #emph(fields.parent.title)]
    }
    if "page-range" in fields {
      reference += [ (pp. #fields.page-range).]
    }
    if "publisher" in fields.parent {
      reference += [ #fields.parent.publisher.]
    }
    if "serial-number" in fields and "doi" in fields.serial-number {
      let url = "https://doi.org/" + fields.serial-number.doi
      reference += [ #link(url)[#url]]
    }
  } else if fields.type == "book" {  // Handle references of type "book"
    reference += format_authors(authors, 6, me)
    reference += format_date(fields, lang)
    reference += format_title(fields.title, lang)

    // Include edition and publisher information if available
    if "edition" in fields {
      reference += [ (#fields.edition).]
    }
    if "publisher" in fields {
      reference += [ #fields.publisher.]
    }
    if "serial-number" in fields and "doi" in fields.serial-number {
      let url = "https://doi.org/" + fields.serial-number.doi
      reference += [ #link(url)[#url]]
    }
  } else { // Handle other types of references
    reference += format_authors(authors, 6, me)
    reference += format_date(fields, lang)
    reference += format_title(fields.title, lang)

    // Include additional fields like edition or archive information if available
    if "edition" in fields {
      reference += [ (#fields.edition).]
    } else {
      reference += [.]
    }
    if "archive" in fields {
      reference += [ #fields.archive.]
    }
    if "serial-number" in fields and "doi" in fields.serial-number {
      let url = "https://doi.org/" + fields.serial-number.doi
      reference += [ #link(url)[#url]]
    }
  }

  // Return the formatted reference
  reference
  parbreak()  // Add a paragraph break after the reference
}

// Custom cv-refs function to fix numbering issues
#let cv-refs(
  what,
  multilingual,  
  entries: (), 
  tag: none,
  me: none,
  lang: "de"
) = {
  // Set paragraph formatting options
  set par(
    hanging-indent: 2em,
    justify: true,
    linebreaks: auto
  )
  
  // Set spacing above each block of text
  set block(above: 0.65em)
  
  // If `entries` is empty, populate it with all keys from the YAML file
  if entries.len() == 0 {
    entries = what.keys()
  }

  // Initialize counters for different types of publications
  let articles = 0
  let conferences = 0
  let incollections = 0
  let books = 0
  let others = 0
  let planned = 0

  // Loop through each entry in the YAML file to count the types of publications
  for (entry, field) in what {
    // Skip entries not specified in entries parameter
    if entry not in entries {
      continue
    }
    
    // If a tag is provided, skip entries that don't have the specified tag
    if not tag == none {
      if "tags" not in field or tag not in field.tags {
        continue
      }
    }
    
    if field.tags == "planned" {
      planned += 1
    } else if field.type == "article" and field.tags == "peer" {
      articles += 1 
    } else if field.type == "article" and field.tags == "conf" {
      conferences += 1
    } else if field.type == "chapter" {
      incollections += 1
    } else if field.type == "book" {
      books += 1
    } else if field.tags == "other" {
      others += 1
    } 
  }

  // Loop through each entry in the YAML file to generate the reference list
  for (entry, fields) in what {
    // Skip entries not specified in entries if entries are specified
    if entry not in entries {
      continue
    }
 
    // If a tag is provided, skip entries that don't have the specified tag
    if not tag == none {
      if "tags" not in fields or tag not in fields.tags {
        continue
      }
    }

    // If entry is in one of the categories, the total number will be subtracted by one to show decreasing numbers aside the publications per section
    if fields.tags == "planned" {
      [\[#planned\] ]
      planned -= 1
    } else if fields.type == "article" and fields.tags == "peer" {
      [\[#articles\] ]
      articles -= 1
    } else if fields.type == "article" and fields.tags == "conf" {
      [\[#conferences\] ]
      conferences -= 1
    } else if fields.type == "chapter" {
      [\[#incollections\] ]
      incollections -= 1
    } else if fields.type == "book" {
      [\[#books\] ]
      books -= 1
    } else if fields.tags == "other" {
      [\[#others\] ]
      others -= 1
    } 

    // Call our custom make-entry-apa function
    make-entry-apa(fields, multilingual, me: me, lang: lang)
  }
}