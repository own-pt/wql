
import falcon
from delphin import ace
from delphin.codecs.mrsjson import encode as mrsjsonEncode
from delphin.codecs.dmrsjson import encode as dmrsjsonEncode
from delphin import itsdb
from delphin import tsql
from delphin.codecs.simplemrs import loads
from delphin.dmrs import from_mrs

import mimetypes
import io
import json
import os

grm = '/Users/ar/hpsg/simpleDBpediaQA/erg.dat'
ts = itsdb.TestSuite('/Users/ar/hpsg/simpleDBpediaQA/test.p')
mrsStrings = {i['parse-id']: i['mrs'] for i in ts['result']}

class Profile:
    def on_get(self, req, resp):
        # response = ace.parse(grm, req.get_param('q', True))
        # mrs0 = response.result(0).mrs()
        # quote = encode(mrs0)
        res = {r['i-id']: r['i-input'] for r in ts['item']}
        # resp.media = json.dumps(res)
        resp.media = res

# class Mrs:
#     def on_get(self, req, res
        
class MRSsite:
    def on_get(self, req, resp):
        resp.content_type = falcon.MEDIA_HTML
        with open('mrs-viz.html', 'rb') as f: 
            resp.data = f.read()

class DMRSsite:
    def on_get(self, req, resp):
        resp.content_type = falcon.MEDIA_HTML
        with open('dmrs-viz.html', 'rb') as f: 
            resp.data = f.read()

class Qmrs:
    def on_get(self, req, resp):
        reqId = req.get_param('id', True)
        # resp.media = mrsStrings[int(id)]
        mrsObj = loads(mrsStrings[int(reqId)])
        resp.media = mrsjsonEncode(mrsObj[0]) #only first MRS
        
class Qdmrs:
    def on_get(self, req, resp):
        reqId = req.get_param('id', True)
        # resp.media = mrsStrings[int(id)]
        mrsObj = loads(mrsStrings[int(reqId)])[0]
        resp.media = dmrsjsonEncode(from_mrs(mrsObj)) #only first MRS

class Item:
    def __init__(self, path):
        self._root = path

    def on_get(self, req, resp, name):
        apath = os.path.join(self._root, name)
        resp.content_type = mimetypes.guess_type(name)[0]
        resp.stream = io.open(apath, 'rb')
        resp.content_length = os.path.getsize(apath)
        
app = falcon.API()

# app.add_static_route('/foo', '/Users/ar/hpsg/wql/mrsjs_demo/wql/style.css')
# app.add_static_route('/mrsjs', '/home/gambitura/workspace/venv-delphin/wql/mrs.js')
# app.add_route('/mrsjs', MRSJS())
# app.add_route('/style.css', ProvCSS())
# app.add_route('/mrs', My.mrs)

app.add_route('/resources/{name}', Item('/Users/ar/hpsg/wql/mrsjs_demo/resources/'))
app.add_route('/list', Profile())
app.add_route('/mrs-viz', MRSsite())
app.add_route('/dmrs-viz', DMRSsite())
app.add_route('/qmrs', Qmrs())
app.add_route('/qdmrs', Qdmrs())

# command gunicorn wsgi:app
