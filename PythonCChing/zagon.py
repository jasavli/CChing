import program

while True:
		text = input('basic > ')
		result, error = program.run('<stdin>', text)

		if error: print(error.as_string())
		else: print(result)