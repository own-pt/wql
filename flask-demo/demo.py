from flask import Flask, request, send_from_directory
from flask import render_template, redirect, session
from flask import url_for
from delphin import ace
from delphin.codecs.mrsjson import encode as mrsjsonEncode
from delphin.codecs.dmrsjson import encode as dmrsjsonEncode
from delphin import itsdb
from delphin import tsql
from delphin.codecs.simplemrs import loads
from delphin.dmrs import from_mrs
import requests
import re

# grm = '/Users/ar/hpsg/simpleDBpediaQA/erg.dat'
# ts = itsdb.TestSuite('/Users/ar/hpsg/simpleDBpediaQA/test.p')
ts = itsdb.TestSuite('/home/gambitura/workspace/erg/trunk/tsdb/gold/mrs')
mrsStrings = {i['parse-id']: i['mrs'] for i in ts['result']}
textsById = {r['i-id']: r['i-input'] for r in ts['item']}

app = Flask(__name__, static_url_path='')
app.secret_key = '112233'

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

@app.route('/test-form', methods = ['GET', 'POST'])
def getName():
    # if request.method == 'POST':
    #     nome1 = request.form['fname']
    #     nome2 = request.form['lname']
    #     session['name'] = nome1 + ' ' + nome2
    #     return redirect(url_for('index'))
    if 'query' in request.args:
        # TODO: Handle request errors:
        wql_q = request.args['query'].strip(' ')
        x = requests.post("http://turastation:8080/query", json = {'q': wql_q})
        sparql_result = requests.get("http://turastation:10035/repositories/gold-erg", 
                                     params={'query': x.text}, 
                                     headers={'Accept':'application/json'})
        mrsURIsMatched = [y[0] for y in sparql_result.json()['values']]
        mrsIdsMatched = [re.match('^<[^s]+/(\d+)/mrsi#mrs>$', uri).groups()[0] for uri in mrsURIsMatched]
        mrsJsons = [mrsjsonEncode(loads(mrsStrings[int(id)])[0]) for id in mrsIdsMatched]
        sentsMatched = [[int(id), textsById[int(id)]] for id in mrsIdsMatched]
        
        return render_template("formresp.html.jinja", 
                                sparql = x.text.replace('<','&lt;').replace('>','&gt;'),
                                query_result = sparql_result.text.replace('<','&lt;').replace('>','&gt;'),
                                wql_query = wql_q, 
                                mrs_jsons = mrsJsons,
                                sentsM = sentsMatched)
    else:
        return render_template("form.html.jinja", wql_query = "[* x]")

if __name__ == "__main__":
    app.run()
