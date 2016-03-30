import React from 'react';

function consensusStatus(data) {
  const consensus = data.juno.consensus;
  return {
    commit_index: consensus.commit_index.val,
    commit_period: consensus.commit_period.val,
    current_leader: consensus.current_leader.val,
    term: consensus.term.val,
  };
}

export default function Transactions({data}) {
  if (data == null) {
    return <span />;
  }

  const {
    commit_index,
    term,
  } = consensusStatus(data);

  // <div>Commit Period: {commit_period}</div>
  // <div>Current Leader: {current_leader}</div>

  return (
    <div className="section">
      <h2>
        TRANSACTIONS
        <div className="border-underline" />
      </h2>
      <div className="transaction-point">
        Commit Index: <span className="transaction-datum">{commit_index}</span>
      </div>
      <div className="transaction-point">
        Term: <span className="transaction-datum">{term}</span>
      </div>
    </div>
  );
}
