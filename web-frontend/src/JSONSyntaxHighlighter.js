import React from 'react';

export default function JsonSyntaxHL(props) {
  const nspace = len => new Array(len+1).join(' ');
  const parser = (x, indentlevel) => {
    switch(typeof(x)){
      case 'string':
        return "<span class=\"string\">\"" + x + "\"</span>";

      case 'number':
      case 'boolean':
        return "<span class=\"" + typeof(x) + "\">" + x + "</span>";

      case 'object':
        if(x == null)
        {
          return "<span class=\"null\">null</span>";

        } else if(Array.isArray(x)){
          let i = indentlevel + 2;
          return "["
               + x.map(y => "\n" + nspace(i) + parser(y, i))
                  .join()
               + "\n" + nspace(indentlevel) + "]";

        } else
        {
          let i = indentlevel + 2;
          let symbols = [];
          for(let y in x)
          {
            let key = "<span class=\"key\">\"" + y + "\"</span>";
            let value = parser(x[y], i);
            symbols.push(key + ": " + value);
          }
          return "{" + symbols.map(s => "\n" + nspace(i) + s)
               + "\n" + nspace(indentlevel) + "}";
        }
      default:
        console.log("unknown case: " + typeof(x));
    }
  }

  return (<pre
            className="JsonSyntaxHL"
            dangerouslySetInnerHTML={{
              __html: parser(props.codeBlock, 0)}
            } />);
}
