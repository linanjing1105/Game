{application,agent_ctr,
             [{description,"agent_ctr wrapper"},
              {vsn,"0.0.1"},
              {modules,[agent_ctr_app,agent_ctr_sup,agent_ctr,agent_sup,
                        agent]},
              {registered,[agent_ctr]},
              {applications,[kernel,stdlib]},
              {mod,{agent_ctr_app,[]}},
              {start_phases,[]}]}.

