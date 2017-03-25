{application,dbm,
             [{description,"dbm wrapper"},
              {vsn,"0.0.1"},
              {modules,[dbm_app,dbm_sup,dbm_svr]},
              {registered,[dbm]},
              {applications,[kernel,stdlib]},
              {mod,{dbm_app,[]}},
              {start_phases,[]}]}.
