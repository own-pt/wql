from flask import Flask, request, send_from_directory, render_template
from delphin import ace
from delphin.codecs.mrsjson import encode as mrsjsonEncode
from delphin.codecs.dmrsjson import encode as dmrsjsonEncode
from delphin import itsdb
from delphin import tsql
from delphin.codecs.simplemrs import loads
from delphin.dmrs import from_mrs

# grm = '/Users/ar/hpsg/simpleDBpediaQA/erg.dat'
# ts = itsdb.TestSuite('/Users/ar/hpsg/simpleDBpediaQA/test.p')
ts = itsdb.TestSuite('/home/gambitura/workspace/erg/trunk/tsdb/gold/mrs')
mrsStrings = {i['parse-id']: i['mrs'] for i in ts['result']}

app = Flask(__name__, static_url_path='')

@app.route("/")
def hello_world():
    return "<p>Hello, World!</p>"

@app.route('/html/<path:path>')
def send_html(path):
    return send_from_directory('./htmls', path)

@app.route('/css/<path:path>')
def send_css(path):
    return send_from_directory('./css', path)

@app.route('/js/<path:path>')
def send_js(path):
    return send_from_directory('./js', path)

@app.route('/list', methods = ['GET'])
def get_profile():
    res = {r['i-id']: r['i-input'] for r in ts['item']}
    return res

@app.route('/qmrs', methods = ['GET'])
def queryIdMRS():
    reqId = request.args["id"]
    mrsObj = loads(mrsStrings[int(reqId)])[0]
    return mrsjsonEncode(mrsObj)

@app.route('/qdmrs', methods = ['GET'])
def queryIdDMRS():
    reqId = request.args["id"]
    mrsObj = loads(mrsStrings[int(reqId)])[0]
    return dmrsjsonEncode(from_mrs(mrsObj))

sents = {r['i-id']: r['i-input'] for r in ts['item']}
@app.route('/mrs-list')
def showMRSpage():
    return render_template("mrs-list.html.jinja", sents = list(sents.items()))

@app.route('/dmrs-list')
def showDMRSpage():
    return render_template("dmrs-list.html.jinja", sents = list(sents.items()))

if __name__ == "__main__":
    app.run()