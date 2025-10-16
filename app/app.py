import re
from flask import Flask, request, jsonify, render_template
import subprocess

app = Flask(__name__)

with open('base.scm', 'r') as f:
    BASE_SCHEME_CODE = f.read()

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/run_scheme', methods=['POST'])
def run_scheme():
    user_code = request.json.get('code', '')

    combined_code = BASE_SCHEME_CODE + "\n" + user_code

    try:
        result = subprocess.run(
            ['guile', '--no-auto-compile'],
            input=combined_code.encode(),
            capture_output=True,
            timeout=5
        )


        if result.returncode != 0:
            return jsonify({'error': result.stderr.decode()}), 400

        output_svg = result.stdout.decode()
        # Extract the SVG block
        match = re.search(r'(<svg.*?</svg>)', output_svg, re.DOTALL)
        if match:
            svg_content = match.group(1)
            return jsonify({'svg': svg_content})
        else:
            return jsonify({'error': 'No SVG found'}), 408
        

    except subprocess.TimeoutExpired:
        return jsonify({'error': 'Execution timed out'}), 408
    except Exception as e:
        return jsonify({'error': str(e)}), 500

if __name__ == '__main__':
    app.run(debug=True)
