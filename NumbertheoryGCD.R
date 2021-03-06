def gcd(a, b):
  assert a >= 0 and b >= 0 and a + b > 0

  if a == 0 or b == 0:
    return max(a, b)

  for d in range(min(a, b), 0, -1):
    if a % d == 0 and b % d == 0:
      return d

  return 1

print(gcd(0, 1))
print(gcd(24, 16))
# The following call would take too long
#print(gcd(790933790547, 1849639579327))

def gcd(a, b):
  assert a >= 0 and b >= 0 and a + b > 0

  while a > 0 and b > 0:
    if a >= b:
      a = a - b
    else:
      b = b - a

  return max(a, b)


print(gcd(24, 16))
print(gcd(790933790547, 1849639579327))
# The following call would take too long
#print(gcd(790933790548, 2))


def gcd(a, b):
  assert a >= 0 and b >= 0 and a + b > 0

  while a > 0 and b > 0:
    if a >= b:
      a = a % b
    else:
      b = b % a

  return max(a, b)

gcd3=function(a,b){
	if (a >0 && b > 0 && a=b >0)
	{
		while(a >= b)
		{
			a=as.numeric(a)%%as.numeric(b)
			print(paste(a,b))
		}

	}
}
gcd3(10,3)

print(gcd(24, 16))
print(gcd(790933790547, 1849639579327))
print(gcd(790933790548, 2))