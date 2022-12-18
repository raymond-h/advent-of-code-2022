from jinja2 import Environment, Template
from pathlib import Path
import re

solutions_template = '''module Solutions (ProblemRunner, solutions) where

import qualified Data.Map.Strict as M
{%- for s in solutions | sort %}
import qualified {{ s.stem }}
{%- endfor %}

type ProblemRunner = FilePath -> IO ()

solutions :: M.Map Integer (ProblemRunner, ProblemRunner)
solutions =
  M.fromList
{%- for day, s in solution_by_day | sort %}
    {{ "[" if loop.first else " " }} ({{ day }}, ({{ s.stem }}.part1, {{ s.stem }}.part2)){{ "," if not loop.last else "" }}
{%- endfor %}
    ]

'''

env = Environment()

t: Template
t = env.from_string(solutions_template)

src_path = Path('./src')

solution_files = list(Path('./src').glob('Day*.hs'))
solution_by_day = [(int(re.search(r'\d+', x.name).group(0)), x)
                   for x in solution_files]

result = t.render(solutions=solution_files, solution_by_day=solution_by_day)

with open(src_path / 'Solutions.hs', 'w') as h:
    h.write(result)
