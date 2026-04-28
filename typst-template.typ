#let article(
  title: none, 
  number: none,
  date: none,
  body
) = {
  set page(
    paper: "a4",
    margin: (x: 3em, y: 4em),

    footer: [
      #rect(
        grid(
          columns: (4fr, 1fr),
      pad(top: 1em, bottom: 1em, align(left+horizon, text(fill: white, size: 8pt)[#title, #number, #date])),
      []
        ),
      fill: rgb("#e8b85a"), 
      width: 100%, 
      height: 100%, 
      // inset: 2em, 
      outset: (left: 3em, right: 4em)
    )
    ],
    header: context {
      let page_number = counter(page).get().at(0)
      let size = 50pt
      let fill = if calc.rem(page_number, 2) == 0 { luma(240) } else { rgb("#f68838") }
      if calc.rem(page_number, 2) == 0 {
        place(right+top, square(fill: fill, text(fill: rgb("#f68838"), size: size, weight: "bold")[#page_number]))
       } else {
        place(left+top, square(fill: fill, text(fill: luma(200), size: size, weight: "bold")[#page_number]))
       }
    }
  )

  set par(justify: true, leading: 1em, spacing: 2em, first-line-indent: 2.5em)

  set text(font: "PT Sans Caption", size: 13pt, fill: luma(90), lang: "id")

  show heading: it => {
    let sizes = (
      "1" : 24pt, // heading level 1
      "2" : 18pt, // heading level 2
      "3": 14pt, // heading level 3
    )

    let level = str(it.level)
    let size = sizes.at(level)
    let formatted_heading = if level == "1" { upper(it) } else { it }
    // let alignment = if level == "2" { center } else { left }
    let color = if level == "1" {rgb("#f68838")} else if level == "2" {rgb("#e8b85a")} else {rgb("#898989")}
    let spacing =  if level == "1" { 0.50em } else { 0.25em }
    let leading = if level == "1" {0.5em} else {0.25em}

    set par(leading: leading)
    set block(spacing: spacing)
    set text(
      font: "PT Sans Caption",
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

// show figure.where(kind: image): set figure.caption(prefix: "Gambar")

  body
}