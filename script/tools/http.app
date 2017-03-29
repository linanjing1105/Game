{application,http,
             [{description,"http wrapper"},
              {vsn,"0.0.1"},
              {modules,[http_app,http_sup,http_svr]},
              {registered,[http]},
              {applications,[kernel,stdlib]},
              {mod,{http_app,[]}},
              {start_phases,[]}]}.
