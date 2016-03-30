require('./public/styles.css');
require('./public/light.css');
require('./public/reset.css');
import React from 'react';
import ReactDOM from 'react-dom';
import {LineChart} from 'react-d3-basic';

import Health from './health';
import Nodes from './nodes';
import Consensus from './consensus';
// import rd3 from './d3';

// const metrics = {
//   'commit period': wrapMetric((data, prev) =>
//     data.juno.consensus.commit_period.mean - prev.juno.consensus.commit_period.mean
//   ),
//   // 'cpu time': wrapMetric(data => data.rts.gc.cpu_ms.val),
//   // 'wall ms': wrapMetric(data => data.rts.gc.wall_ms.val),
//   // 'bytes allocated': wrapMetric(data => data.rts.gc.bytes_allocated.val),
//   // 'gcs': wrapMetric(data => data.rts.gc.num_gcs.val),
//   'applied index': wrapMetric(data => data.juno.node.applied_index.val),
// };

function computeDatum(prev, curr) {
  const date = new Date(curr.ekg.server_timestamp_ms.val);
  const commitPeriod = prev == null
    ? 0
    : curr.juno.consensus.commit_period.mean- prev.juno.consensus.commit_period.mean;
  const commitPeriodSd = prev == null
    ? 0
    : curr.juno.consensus.commit_period.variance / 1000000;

  return {
    date,
    commitPeriod,
    commitPeriodSd,
  };
}

const ports = [10000, 10001, 10002, 10003];

// number of datapoints to keep around (5 mins)
const dataWindow = 60 * 5;

class App extends React.Component {
  constructor(props) {
    super();

    const data = {}
    for (let port of ports) {
      data[`127.0.0.1:${port}`] = {
        computedData: [],
        prevDatum: null,
        role: 'Unknown',
      };
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
        <h1 className="cluster-header">
          JUNO CLUSTER
          <div className="border-underline" />
        </h1>
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
        const computedDatum = computeDatum(stateData[id].prevDatum, newData);

        // Add in new data and cap the size at the number of data points we
        // track
        const computedData = stateData[id]
          .computedData
          .concat([computedDatum])
          .slice(-dataWindow);

        const data = {
          ...stateData,
          [id]: {
            prevDatum: newData,
            computedData,
            role
          },
        };

        this.setState({ data, leaderData });
    }).catch(err => console.log(err));
  }

}

ReactDOM.render(<App />, document.querySelector("#myApp"));
