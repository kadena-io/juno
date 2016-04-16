import React from 'react';
import {
  XYPlot,
  XAxis,
  YAxis,
  VerticalGridLines,
  HorizontalGridLines,
  VerticalBarSeries,
} from 'react-vis';
import RadioGroup from 'react-radio-group';

// how many data points to show (one per second)
const points = {
  '30 s': 30,
  '1 m': 60,
  '5 m': 300,
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
    const {port, role, data, appliedIndex, selectedTime} = this.props;
    const now = new Date();

    const countPoints = points[selectedTime];
    const commitIndices = data
      .map(datum => datum.juno.consensus.commit_index.val)
      .slice(-(points[selectedTime] + 1));
    const firstValue = commitIndices[0];

    while (commitIndices.length < countPoints + 1) {
      commitIndices.unshift(firstValue);
    }

    // get last n points to show (plus points to diff off of)
    const computedData = commitIndices
      .map((datum, i, arr) => {
        const prev = arr[i-1];
        if (prev == null) {
          return null;
        }

        const y = datum - prev;

        return {x: i, y};
      })

      // first should be null, pop it
      .filter(datum => datum != null);

    // HACK: replaced the popped first datum
    computedData[0] = {x: 1, y: 0};

    // Also a hack: subtract 60, since react-vis doesn't allow you to set the
    // labels by hand :(
    const computedData2 = computedData.map(({x, y}) => ({x: x - 60, y}));

    return (
      <div className="node">
        <h3 className="node-header">
          <div>
            <span className="node-header-port">{port}</span>
            <span className={`node-role node-role-${role}`}>{role}</span>
            <span className="node-index">index: {appliedIndex}</span>
          </div>
        </h3>
        <div className="node-graph">
          <XYPlot
            width={600}
            height={200}
            margin={{left: 75, right: 40, top: 10, bottom: 40}}
            opacity={0.2}
            color="steelblue"
          >
            <HorizontalGridLines color="steelblue" />
            <VerticalGridLines color="steelblue" />
            <XAxis title="seconds ago" />
            <YAxis title="commits" />
            <VerticalBarSeries data={computedData2} />
          </XYPlot>
        </div>
      </div>
    );
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
        <h3 className="cps-header">Commits per second</h3>

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
