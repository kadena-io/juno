import React from 'react';

export default function AddPayments(props) {
  var response;
  if (props.submitResponse != null) {
    const reason = props.submitResponse.reason != null ?
          (<span className="submit-response-reason">({props.submitResponse.reason})</span>) : "";
    response = (<span className="submit-response">{props.submitResponse.status} {reason}</span>);
  } else {
    response = (<div></div>);
  }
  return (
      <div style={{height: '80vh', marginBottom: '10px', overflowX: 'hidden', width: '100%'}} className="panel-body">
      <form>
      <div className="form-group">
      <textarea className="form-control swift" rows="20" onChange={e => props.handleSwiftText(e)}></textarea>
      </div>
      <div className="form-group">
      <button type="button" className="btn btn-primary" onClick={e => props.handleSwiftSubmit(e)}>Submit</button>
      {response}
      </div></form></div>);
}
