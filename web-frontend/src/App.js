import React from 'react';
import ReactJsonSyntaxHighlighter from 'react-json-syntax-highlighter';
import {useState} from 'react';
import * as defaults from './defaults.js';
import './App.css';

function Toggle(props) {

    return (
        <span className="switch"
              onClick={() => props.toggler(!props.value)}
        >
          {props.text }
        </span>
    )
}

function App() {
    let [defsVisible, setDefsVisibility] = useState(false);
    let [inputCode, setInputCode] = useState(defaults.structure);
    let [defs, setDefs] = useState(defaults.defs);
    let [generated, setGenerated] = useState({
        isLoaded: false,
        code: defaults.genObj,
        error: null
    });
    const generate = () => {
        fetch("./api", {method: 'POST', body: JSON.stringify({input: inputCode, defs})})
            .then(response => response.json())
            .then(generatedJson => setGenerated(
                {
                    isLoaded: true,
                    code: generatedJson.code,
                    error: generatedJson.cgError
                }
            ));
    }

    return (
        <div className="App">
          <header><h1>MakeObj JSON Generator</h1></header>

          <div className="rowLane">
            <div className="column"
                 style={{display: defsVisible ? "none" : "flex"}}
            >
              <h2>Structure
                / <Toggle toggler={setDefsVisibility}
                          value={defsVisible}
                          text="Definitions"/>
              </h2>
              <hr />
              <textarea value={inputCode}
                        onChange={ev => setInputCode(ev.target.value)}
              />
              <button className="generate" onClick={() => generate()}>
                Generate JSON!
              </button>

            </div>

            <div className="column"
                 style={{display: defsVisible ? "flex" : "none"}}
            >
              <h2>
                <Toggle toggler={setDefsVisibility}
                        value={defsVisible}
                        text="Structure "/>
                / Definitions
              </h2>
              <hr />
              <textarea value={defs}
                        onChange={ev => setDefs(ev.target.value)}
              />
              <button className="generate" onClick={() => generate()}>
                Generate JSON!
              </button>
            </div>
            <div className="column" >
              <h2>Generated JSON</h2>
              <hr />
              <ReactJsonSyntaxHighlighter obj={generated.code} />
            </div>
          </div>

          <div className="colWidth">
          <h3>What is this?</h3>
          <p>This is <strong>MakeObj</strong>, the JSON object generator.
            MakeObj can create a JSON object from a given input, called
            a <em>structure</em>. Please try it above by
            pressing the button <em>Generate JSON</em> above.            
          </p>

          <p>Next to the structure tab, you will see a tab called <em>definitions</em>. This is used to create
            reusable expression that we may want to reuse or just
            make the syntax slightly easier to read.
          </p>

          <p>For a full syntax guide, see this
            projects <a href="https://github.com/nyson/makeobj">
            GitHub page
            </a>.
          </p>
          </div>

        </div>
    );
}

export default App;
