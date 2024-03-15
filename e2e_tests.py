
import os
import subprocess
import tempfile
import sys
files = [f for f in os.listdir('test_programs')]

if len(sys.argv) > 1 and sys.argv[1] == "local":
    local = True
else:
    local = False

for f in files:
    with open(f'test_programs/{f}', 'r') as file:
        print(f'Running test file {f}')
        test_file = file.read()
        tests = filter(lambda x: x != '', test_file.split('---'))
        for test in tests:
            input_file = str(tempfile.NamedTemporaryFile(mode='w', delete=True).name)
            output_file = str(tempfile.NamedTemporaryFile(mode='w', delete=True).name)

            test_source = test.split('out:\n')[0]
            expected = test.split('out:\n')[1]

            with open(input_file, 'w') as input_f:
                input_f.write(test_source)


            code = os.system(f'./run{"_local" if local else ""}.sh {input_file} {output_file}')
            if code != 0:
                print(f'Failed to compile {f}')
                exit(1) 
            else:
                subprocess.run(['chmod', 'u+x', output_file], check=True)
                program_output = subprocess.check_output(output_file, shell=True).decode()
                if program_output != expected:
                    print(f'Failed test {test_source} in {f}, expected {expected} but got {program_output}')
                else:
                    print(f'Passed test in {f}')
