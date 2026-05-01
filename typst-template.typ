#let orangeline() = {
  line(length: 50%, stroke: 1pt + rgb("#F68838"))
}

#let status-box(top-box-text: "", bottom-box-text: "") = {
  let top_box = box(
    width: 1.5in,
    height: 0.75in,
    fill: rgb("#F68838"),
    inset: 6pt,
    align(center+horizon)[
      #text(fill: white, weight: "bold", size: 20pt)[#top-box-text]
    ]
  )

  let bottom_box = box(
    width: 1.5in,
    height: 0.75in,
    fill: rgb("#EFEFEF"),
    inset: 6pt,
    align(center+horizon)[
      #text(fill: rgb("#F68838"), size: 25pt, weight: "bold")[#bottom-box-text]
    ]
  )

  stack(top_box, bottom_box, spacing: 0pt, dir: ttb)
}

#let article(
  title: none, 
  number: none,
  date: none,
  body
) = {
  set page(
    paper: "a4",
    margin: (x: 4em, y: 4em),
    // fill: rgb("#f5f2ec"),
    footer: context {
      let page_number = counter(page).get().at(0)
      // let left_right = if calc.rem(page_number, 2) == 0 { left } else { right }
      if calc.rem(page_number, 2) != 0 {
        rect(
        grid(
          columns: (4fr, 1fr),
      pad(top: 1em, bottom: 1em, align(left+horizon, text(fill: white, size: 8pt)[#title, #number, #date])),
      []
        ),
      fill: rgb("#F68838"), //e8b85a
      width: 100%, 
      height: 100%, 
      // inset: 2em, 
      outset: (left: 4em, right: 4em)
    )
      } else {
        rect(
        grid(
          columns: (1fr, 4fr),
          [],
      pad(top: 1em, bottom: 1em, align(right+horizon, text(fill: white, size: 8pt)[#title, #number, #date])),
      
        ),
      fill: rgb("#F68838"), //e8b85a
      width: 100%, 
      height: 100%, 
      // inset: 2em, 
      outset: (left: 4em, right: 4em)
    )
      }
      
    },
    header: context {
      let page_number = counter(page).get().at(0)
      let width = 50pt
      let height = 40pt
      let size = 50pt
      let fill = if calc.rem(page_number, 2) == 0 { luma(240) } else { rgb("#f68838") }
      if calc.rem(page_number, 2) == 0 {
        place(right+top, square(fill: fill, width: width, height: height, text(fill: rgb("#f68838"), size: size, weight: "bold")[#page_number]))
       } else {
        place(left+top, square(fill: fill, width: width, height: height,text(fill: luma(200), size: size, weight: "bold")[#page_number]))
       }
    }
  )

  set par(justify: true, leading: 1em, spacing: 2em)

  set text(
    // font: "PT Sans Caption", 
    font: "Lato",
    size: 12pt, 
    fill: luma(90), 
    lang: "id"
    )

  show heading: it => {
    let sizes = (
      "1" : 20pt, // heading level 1
      "2" : 16pt, // heading level 2
      "3": 12pt, // heading level 3
    )

    let level = str(it.level)
    let size = sizes.at(level)
    let formatted_heading = if level == "1" { upper(it) } else { it }
    // let alignment = if level == "2" { center } else { left }
    let color = if level == "1" {rgb("#f68838")} else if level == "2" {rgb("#e8b85a")} else {rgb("#898989")}
    // let spacing =  if level == "1" { 0.25em } else { 0.15em }
    let leading = if level == "1" {0.5em} else {0.25em}
    let spacing = 1em
    // let leading = 0.25em

    set par(leading: leading)
    set block(spacing: spacing)
    set text(
      // font: "PT Sans Caption",
      font: "Lato",
      size: size,
      fill: color,
      weight: "bold"
    )
    box(width: 100%, align(left)[#formatted_heading])
  }

  show heading.where(level: 1): set heading(numbering: "A.")
  set list(marker: text(fill: rgb("#f68838"), size: 15pt)[■])

  show figure.where(kind: image): set figure(supplement: [Gambar])
  
  show figure.where(kind: image): set figure(
  supplement: context {
    if text.lang == "id" {
      "Gambar"
    } else if text.lang == "en" {
      "Figure"
    } else {
      "Fig."
    }
  }
)

show figure: it => {
  set text(size: 10pt, fill: rgb("#F68838"))  // 👈 change size here
  it
}

set table(stroke: 0.25pt + luma(240), inset: 8.5pt)

// show figure.where(kind: image): set figure.caption(prefix: "Gambar")

  body
}