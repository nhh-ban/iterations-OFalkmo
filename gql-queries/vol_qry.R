vol_qry <- function(id, from, to) {
  query <- paste0(
    'query {',
    '  trafficData(trafficRegistrationPointId: "', id, '") {',
    '    volume {',
    '      byHour(from: "', from, '", to: "', to, '") {',
    '        edges {',
    '          node {',
    '            from',
    '            to',
    '            total {',
    '              volumeNumbers {',
    '                volume',
    '              }',
    '            }',
    '          }',
    '        }',
    '      }',
    '    }',
    '  }',
    '}'
  )
  
  return(query)
}