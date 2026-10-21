#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge
#set page(width: auto, height: auto)
#set text(font: "Public Sans")

#let blue = rgb("#4477AA")
#let red = rgb("#EE6677")
#let green = rgb("#228833")

#let edge = edge.with(marks: "-|>")

#let dag(
  fills: ("": none),
  strokes: ("": none),
  labs: ("": none),
  inc-xs: false,
  inc-backdoor1: false
) = {
    
  let text_fill = ("": white)

  for k in fills.keys(){
    text_fill.insert(k, if fills.at(k) == none {black} else {white})
  }

  let make_node(
    pos,
    lab,
    id: none,
    text_fill: ("":1),
    fills: ("":1),
    ..args
  ) = {
    id = if id == none {lab} else {id}
    if id not in text_fill.keys() {
      text_fill.insert(id, black)
    }
    if id not in fills.keys() {
      fills.insert(id, none)
    }
    node(
      pos,
      text(fill: text_fill.at(id), lab),
      name: label(id),
      fill: fills.at(id),
      ..args
    )
  }

  let make_edge(
    name1, name2,
    strokes: ("": none),
    labs: ("": none),
    ..args
  ) = {
    
    let st = strokes.at(name1 + "->" + name2, default: black)
    let l = labs.at(name1 + "->" + name2, default: none)
    
    edge(label(name1), label(name2), stroke: st, label: l, ..args)
  }

  

  
  let make_node = make_node.with(fills: fills, text_fill: text_fill)
  let make_edge = make_edge.with(strokes: strokes, labs: labs)
  
  diagram(
    node-stroke: 1pt + black,
    node-corner-radius: 5pt,
    edge-stroke: 1pt + black,
    label-sep: 0pt,
    {
      make_node(
        (0,0),
        "dob"
      )
      
      make_node(
        (rel:(0,2), to: <dob>),
        "year"
      )

      make_node(
        (rel: (0.5,-1), to: (<year>, 0%, <dob>)),
        "age",
        extrude: (0,-2)
      )

      make_node(
        (rel: (1,1), to: <age>),
        overline("speech rate"),
        id: "arate"
      )

      make_node(
        (rel: (1,0), to: <arate>),
        overline("dur"),
        id: "adur"
      )
      

      make_node(
        (rel: (2.4,0), to: <adur>),
        "relative dur",
        id: "rdur"
      )
      make_node(
        (rel: (0,-1), to: (<adur>, 45%, <rdur>)),
        "dur",
        extrude: (0,-2)        
      )
      
      make_node(
        (rel:(0.5,-1), to: <rdur>),
        $X_t$,
        id: "unkt"
      )
      make_node(
        (rel: (0,-1), to: <dur>),
        "vowel peripherality",
        id: "vq"
      )
      if inc-xs {
        node(
         (rel:(-1.4,0), to:(<arate>, 50%, <vq>)),
         text(fill: white)[$X_s$],
         name: <unks>,
         fill: gradient.linear(blue,blue, green, green)
        )
        make_edge("unks", "vq")
        make_edge("unks", "arate")
      }
    },
  
    node(
      enclose: (<dob>, <adur>),
      name: <senc>,
      stroke: luma(60%)
    ),
    node(
      (rel: (0,-0.3), to: <senc.north>),
      "speaker side",
      stroke: none
    ),
    
  
    {
      make_edge("dob", "age", marks: "=>")
      make_edge("age", "vq", bend: 10deg)
      make_edge("year", "age", marks: "=>")
      make_edge("age", "arate")
      make_edge("arate", "adur")
      make_edge("dob", "vq")
      make_edge("adur", "dur", marks: "=>")
      make_edge("rdur", "dur", marks: "=>")
      make_edge("dur", "vq")
      make_edge("unkt", "vq")
      make_edge("unkt", "rdur")
      if inc-backdoor1 {
        make_edge("adur", "rdur", marks: "<|-|>", bend: 20deg)
      }
    },
  
    {
      node(
        (rel: (-0.4,-2), to: <rdur>),
        name: <enc1>
      )
      node(
        (rel: (0.3 ,1.3), to: <unkt>),
        name: <enc2>
      )
    },
    node(
      enclose: (<enc1>, <enc2>),
      name: <tenc>,
      inset: 1em
    ),
    node(
      (rel: (0,-0.3), to: <tenc.north>),
      "token side",
      stroke: none
    )
  )
}




