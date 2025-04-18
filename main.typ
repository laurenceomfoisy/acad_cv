// Import the package
#import "@preview/modern-acad-cv:0.1.0": *

// Import custom functions (with our fixed cv-refs function)
#import "cv-functions.typ": cv-refs

// loading meta data and databases (needs to be ad this directory)
#let metadata = yaml("metadata.yaml") 
#let multilingual = yaml("dbs/i18n.yaml")
#let work = yaml("dbs/work.yaml")
#let education = yaml("dbs/education.yaml")
#let grants = yaml("dbs/grants.yaml")
#let refs = yaml("dbs/refs.yaml")
#let conferences = yaml("dbs/conferences.yaml")
#let talks = yaml("dbs/talks.yaml")
#let committee = yaml("dbs/committee.yaml")
#let teaching = yaml("dbs/teaching.yaml")
#let training = yaml("dbs/training.yaml")
#let skills = yaml("dbs/skills.yaml")

// set the language of the document
#let language = "en"      

// defining variables
#let headerLabs = create-headers(multilingual, lang: language)

#show: modern-acad-cv.with(
  metadata, 
  multilingual,
  lang: language,   
  font: "Fira Sans",
  show-date: true
)    

= #headerLabs.at("work")

#cv-auto-stc(work, multilingual, lang: language)

= #headerLabs.at("education")

#cv-auto-stp(education, multilingual, lang: language) 

= #headerLabs.at("grants")
 
#cv-auto-stp(grants, multilingual, lang: language)  
 
= #headerLabs.at("pubs")

#cv-cols(
  "",
  for lang in multilingual.lang.keys() {
    if language == lang [
      #multilingual.lang.at(lang).pubs-note
    ] 
  }
)  
== #headerLabs.at("pubs-peer")
#cv-refs(refs, multilingual, tag: "peer", me: [M. Foisy, L-O.], lang: language)

== #headerLabs.at("pubs-conf")
#cv-refs(refs, multilingual, tag: "conf", me: [M. Foisy, L-O.], lang: language)

== #headerLabs.at("pubs-edited")
#cv-refs(refs, multilingual, tag: "edited", me: [M. Foisy, L-O.], lang: language)

== #headerLabs.at("pubs-upcoming")
#cv-refs(refs, multilingual, tag: "planned", me: [M. Foisy, L-O.], lang: language) 

= #headerLabs.at("confs") 
== #headerLabs.at("confs-conf")
#cv-cols(
  "", 
  headerLabs.at("exp-confs")
)

#cv-auto-list(conferences, multilingual, lang: language)

== #headerLabs.at("confs-talks")
#cv-auto(talks, multilingual, lang: language)

= #headerLabs.at("committee")

#cv-auto(committee, multilingual, lang: language)

= #headerLabs.at("teaching")

== #headerLabs.at("teaching-courses")

#cv-table-teaching(teaching, multilingual, lang: language)

= #headerLabs.at("training")

#cv-auto-cats(training, multilingual, headerLabs, lang: language)

= #headerLabs.at("others")

#cv-auto-skills(skills, multilingual, metadata, lang: language)
