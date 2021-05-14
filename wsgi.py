
import falcon
from delphin import ace
from delphin.codecs.mrsjson import encode
from delphin import itsdb
from delphin import tsql
import json

grm = '/Users/ar/hpsg/scitail-1.1/erg.dat'
ts = itsdb.TestSuite('/Users/ar/hpsg/scitail-1.1/dataset.bp')

class Profile:
    def on_get(self, req, resp):
        # response = ace.parse(grm, req.get_param('q', True))
        # mrs0 = response.result(0).mrs()
        # quote = encode(mrs0)
        res = dict([ (r['i-id'],r['i-input']) for r in ts['item']])
        resp.media = json.dumps(res)


app = falcon.App()
app.add_route('/list', Profile())

# app.add_route('/mrs', My.mrs)
# command gunicorn wsgi:app
