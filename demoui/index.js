require('./public/css/reset.css');
require('./public/css/font-awesome.min.css');
require('./public/css/bootstrap.min.css');
require('./public/css/style.css');
import React from 'react';
import ReactDOM from 'react-dom';

import HeaderNav from './header-nav';
import Sidebar from './sidebar';
import Detail from './detail';


//// const ports = [10000, 10001, 10002, 10003];

////// number of datapoints to keep around (5 mins + 1 min)
//// const dataWindow = 60 * 6;

//// const LOST_NODE = 'LOST_NODE';

const junoUrl = '//localhost:8000/api';
const [londonNostro,tokyoNostro,londonBranch,tokyoBranch]=['101','102','100','103'];
const [nintendo,sony,tesla,amazon]=['003','004','000','001'];
const accounts = {
  nintendo: {
    name: 'Nintendo',
    type: 'Client'
  },
  sony: {
    name: 'Sony',
    type: 'Client'
  },

  tesla: {
    name: 'Tesla',
    type: 'Client'
  },

  amazon: {
    name: 'Amazon',
    type: 'Client'
  },

  londonBranch: {
    name: 'London',
    type: 'Branch'
  },

  londonNostro: {
    name: 'London Nostro',
    type: 'Correspondent'
  },

  tokyoNostro: {
    name: 'Tokyo Nostro',
    type: 'Correspondent'
  },

  tokyoBranch: {
    name: 'Tokyo',
    type: 'Branch'
  }
};


class App extends React.Component {
  constructor(props) {
    super();

    const data = {

    };
    //// for (let port of ports) {
    ////   data[`127.0.0.1:${port}`] = LOST_NODE;
    //// };

    this.state = {
      currentPane: "add-payments"
    };

  }

  handleChangePane(currentPane) {
    this.setState({currentPane});
    switch (currentPane) {
    case "add-payments":
      break;
    case "new-york-branch":
      this.fetchNostro();
      break;
    case "london-branch":
      this.fetchBranch(londonBranch);
      break;
    case "tokyo-branch":
      this.fetchBranch(tokyoBranch);
      break;
    }
  }

  fetchNostro() {
    fetch(`${junoUrl}/ledger-query?account=${londonNostro}`, {
      method: 'get',
      mode: 'cors'
    }).then(response => response.json())
      .then(jsonData => {
        const nostroData = jsonData.trans;
        this.setState({nostroData});
      });
  }

  fetchBranch(branch) {
    fetch(`${junoUrl}/ledger-query?account=${branch}`, {
      method: 'get',
      mode: 'cors'
    }).then(response => response.json())
      .then(jsonData => {
        const branchData = {};
        branchData[branch] = jsonData;
        this.setState({branchData});
      });

  }

  render() {
    return (
        <div className="app">
        <HeaderNav {...this.state} />
        <Sidebar handleChangePane={(pane)=>this.handleChangePane(pane)} {...this.state} />
        <Detail junoUrl={junoUrl} accounts={accounts}
      nintendo={nintendo} sony={sony} amazon={amazon} tesla={tesla}
      tokyoNostro={tokyoNostro} londonNostro={londonNostro}
      londonBranch={londonBranch} tokyoBranch={tokyoBranch} {...this.state} />
        </div>
    );
  }

  componentDidMount() {
    ////this._setInterval();
  }

  componentDidUpdate() {
    ////this._setInterval();
  }

////  _setInterval() {
////    for (let port of ports) {
////      const id = `_id${port}`;
////      window.clearInterval(this[id]);
////      this[id] = window.setInterval(() => {
////        this._fetch(port);
////      }, 1000);
////    }
////  }
////
////  _fetch(port: number) {
////    fetch(`//localhost:${port+80}`, {
////      method: 'get',
////      headers: new Headers({
////        'Accept': 'application/json',
////      }),
////      mode: 'cors'
////    }).then(response => response.json())
////      .then(newDatum => {
////        const nodeDatum = newDatum.juno.node;
////        const id = nodeDatum.id.val;
////        const role = nodeDatum.role.val;
////        const appliedIndex = nodeDatum.applied_index.val;
////        const leaderData = role === "Leader" ? newDatum : this.state.leaderData;
////        const stateData = this.state.data;
////
////        // re-initialize if we've found it again; add in new data and cap the
////        // size at the number of data points we track
////        const newData = stateData[id] === LOST_NODE
////          ? [newDatum]
////          : stateData[id].data.concat([newDatum]).slice(-dataWindow);
////
////        const data = {
////          ...stateData,
////          [id]: {
////            data: newData,
////            role,
////            appliedIndex,
////          },
////        };
////
////        this.setState({ data, leaderData });
////    }).catch(err => {
////      const id = `127.0.0.1:${port}`;
////      const stateData = this.state.data;
////      const data = {
////        ...stateData,
////        [id]: LOST_NODE,
////      };
////
////      this.setState({data});
////    });
////  }
////
}

ReactDOM.render(<App />, document.querySelector("#myApp"));
