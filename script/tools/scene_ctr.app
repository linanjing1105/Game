{application,scene_ctr,
             [{description,"scene_ctr wrapper"},
              {vsn,"0.0.1"},
              {modules,[scene_ctr_app,scene_ctr_sup,scene_ctr,agent_sup,
                        agent]},
              {registered,[scene_ctr]},
              {applications,[kernel,stdlib]},
              {mod,{scene_ctr_app,[]}},
              {start_phases,[]}]}.

