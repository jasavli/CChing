import program

while True:
		text = input('C$ > ')
		result, error = program.run('<stdin>', text)

		if error: print(error.as_string())
		elif result: print(result)