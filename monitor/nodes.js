import 'metrics-graphics/dist/metricsgraphics.css';
import React from 'react';
import d3 from 'd3';
import MG from 'metrics-graphics';
import RadioGroup from 'react-radio-group';

// how many data points to show (one per second)
const points = {
  '30 s': 30,
  '1 m': 60,
  '5 m': 300,
};

// TODO can we show a distribution-ish?
const trailingFrame = {
  '1 s': 1,
  '10 s': 10,
  '1 m': 60,
};

function expMovingAvg(dataWindow, trailing) {
  const alpha = 0.5;

  let result = dataWindow[1][1] - dataWindow[0][1];
  for (let i = 2; i < dataWindow.length; i++) {
    const newVal = dataWindow[i][1] - dataWindow[i-1][1];
    const prevVal = result;
    result = newVal * alpha + prevVal * (1 - alpha);
  }

  return (1 / alpha) * result;
}

function DeadNode({port}): React.Element {
  return (
    <div className="node">
      <div className="node-lost-centerer">
        {'Lost contact with ' + port}
      </div>
    </div>
  );
}

class Node extends React.Component {
  render(): React.Element {
    const {port, role, appliedIndex} = this.props;

    return (
      <div className="node">
        <h3 className="node-header">
          <div>
            <span className="node-header-port">{port}</span>
            <span className={`node-role node-role-${role}`}>{role}</span>
            <span className="node-index">index: {appliedIndex}</span>
          </div>
        </h3>
        <div className="node-graph" ref={ref => this._d3 = ref} />
      </div>
    );
  }

  componentDidMount() {
    this._renderD3();
  }

  componentWillReceiveProps() {
    this._renderD3();
  }

  _renderD3() {
    const {port, data, selectedTime} = this.props;
    const now = new Date();

    const trailing = trailingFrame['10 s'];

    // get last n points to show (plus points to diff off of)
    const computedData = data
      .slice(-(points[selectedTime] + trailing))
      .map((datum, i, arr) => {
        // Is it posssible to even access the earlier datum?
        const preIndex = i - trailing;
        if (preIndex < 0) {
          return null;
        }

        const date = new Date(datum.ekg.server_timestamp_ms.val);
        const dataWindow = arr
          .slice(i - trailing, i)
          .map(windowDatum => [
            new Date(windowDatum.ekg.server_timestamp_ms.val),
            // windowDatum.juno.consensus.commit_period.count
            windowDatum.juno.consensus.commit_index.val,
          ]);
        const avgFreq = expMovingAvg(dataWindow, trailing);

        return {date, value: avgFreq};
      })

      // filter out the points too early to compute
      .filter(datum => datum != null);

    if (computedData.length > 0) {
      MG.data_graphic({
        title: '',
        data: computedData,
        width: 600,
        height: 200,
        target: this._d3,
        x_accessor: 'date',
        xax_format(f) {
          const secsAgo = Math.round((now - f) / 1000, 0);
          return `${secsAgo} s ago`;
        },
        y_accessor: 'value',
        interpolate: 'monotone',
      });
    }
  }
}

// different time domains we're equipped to show: 30 s, 1 m, 5 m
type Time = '30 s' | '1 m' | '5 m';

export default class Nodes extends React.Component {
  constructor() {
    super();
    this.state = {
      selectedMetric: 'appliedIndex',
      selectedTime: '1 m'
    };
  }

  render() {
    // XXX selectedMetric
    const {selectedMetric, selectedTime} = this.state;
    const hosts = [10000, 10001, 10002, 10003];

    const nodeElems = [];
    for (let hostPort of hosts) {
      const hostName = `127.0.0.1:${hostPort}`;
      const hostInfo = this.props.data[hostName];

      if (hostInfo === 'LOST_NODE') {
        nodeElems.push(
          <DeadNode key={hostPort} port={hostPort} />
        );
      } else {
        nodeElems.push(
          <Node
            key={hostPort}
            port={hostPort}
            data={hostInfo.data}
            selectedTime={selectedTime}
            role={hostInfo.role}
            appliedIndex={hostInfo.appliedIndex}
          />
        );
      }
    }

    return (
      <div className="section">
        <h2>
          NODES
          <div className="border-underline" />
        </h2>

        {/*
        <div className="nodes-selector">
          <div>
            <RadioGroup
              selectedValue={selectedMetric}
              onChange={selectedMetric => this.handleMetric(selectedMetric)}
            >
              {Radio => (
                <ul>
                  <li className="node-option"><label><Radio value="appliedIndex" /><span className="check" />Applied Index</label></li>
                  <li className="node-option"><label><Radio value="commitPeriod" /><span className="check" />Commit Period</label></li>
                </ul>
              )}
            </RadioGroup>
          </div>

          <div>
            <RadioGroup
              selectedValue={selectedTime}
              onChange={selectedTime => this.handleTime(selectedTime)}
            >
              {Radio => (
                <ul>
                  <li className="node-option"><label><Radio value="30 s" /><span className="check" />30 s</label></li>
                  <li className="node-option"><label><Radio value="1 m" /><span className="check" />1 min</label></li>
                  <li className="node-option"><label><Radio value="5 m" /><span className="check" />5 min</label></li>
                </ul>
              )}
            </RadioGroup>
          </div>
        </div>
        */}

        {nodeElems}

      </div>
    );
  }

  handleMetric(selectedMetric: string) {
    this.setState({ selectedMetric });
  }

  handleTime(selectedTime: string) {
    this.setState({ selectedTime });
  }
}
