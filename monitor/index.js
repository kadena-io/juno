// require("./node_modules/bootstrap/dist/css/bootstrap.min.css")
require('./public/styles.css');
import React from 'react';
import ReactDOM from 'react-dom';
import {LineChart} from 'react-d3-basic';

import data from './user_sample.json';
import Health from './health';
import Nodes from './nodes';
import Consensus from './consensus';

function last<A>(arr: Array<A>): A {
  // (handles empty case with undefined)
  return arr[arr.length-1];
}

const ports = [10000, 10001, 10002, 10003];

class App extends React.Component {
  constructor(props) {
    super();

    const data = {}
    for (let port of ports) {
      data[`127.0.0.1:${port}`] = [];
    };

    this.state = {
      data,
      leaderData: null,
    };
  }

  render() {
    const {leaderData, data} = this.state;
    return (
      <div>
        <div>
          JUNO CLUSTER
        </div>
        <Consensus data={leaderData} />
        <Health data={leaderData} />
        <Nodes data={data} />
      </div>
    );
  }

  componentDidMount() {
    this._setInterval();
  }

  componentDidUpdate() {
    this._setInterval();
  }

  _setInterval() {
    for (let port of ports) {
      const id = `_id${port}`;
      window.clearInterval(this[id]);
      this[id] = window.setInterval(() => {
        this._fetch(port+80);
      }, 1000);
    }
  }

  _fetch(port: number) {
    fetch(`//localhost:${port}`, {
      method: 'get',
      headers: new Headers({
        'Accept': 'application/json',
      }),
      mode: 'cors'
    }).then(response => response.json())
      .then(newData => {
        const id = newData.juno.node.id.val;
        const role = newData.juno.node.role.val;

        const leaderData = role === "Leader" ? newData : this.state.leaderData;

        const stateData = this.state.data;
        const data = {
          ...stateData,
          [id]: stateData[id].concat([newData]),
        };

        this.setState({ data, leaderData });
    }).catch(err => console.log(err));
  }

}

ReactDOM.render(<App />, document.querySelector("#myApp"));
