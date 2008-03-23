package net.harnly.aaron.math.algebra
import java.lang.Double.MIN_VALUE

// ----------------- Basic Sets ---------------------
// Wrapper classes that have some limited range
class RangedWrapper[T](
	value: T,
	rangeTest: T => Boolean
){
	if (! rangeTest(value))
		throw new IllegalArgumentException(
			"Value " + value + " is out of the range of " + this.getClass.getName
		)
	
	def v = value
}

// N0, the natural numbers including zero
case class N0(value: Int)
extends RangedWrapper[Int](value, {a: Int => a >= 0})

// N*, the natural numbers excluding zero
case class Nstar(value: Int)
extends RangedWrapper[Int](value, {a: Int => a > 0})

// Q*, rational numbers excluding zero
case class Qstar(value: Double)
extends RangedWrapper[Double](value, 
	{x: Double => (Math.abs(x - MIN_VALUE) <= MIN_VALUE)}
)

// ----------------- Algebraic Structures ---------------------

trait Magma[T]
{
	def op(x: T, y: T): T
}

trait SemiGroup[T] // Associative
extends Magma[T]
{
	// Assert that for all a, b, c: 
	// op(op(a,b),c) = op(a,op(b,c))	
}

trait CommutativeMagma[T] // Commutative
extends Magma[T]
{
	// Assert that for all a, b: 
	// op(a,b) = op(b,a)
}

trait QuasiGroup[T] // Divisibility
extends Magma[T]
{
	// Assert that for all a,b
	// a * leftDivide(a,b) = b
	def leftDivide(a: T, b: T): T

	// Assert that for all a,b
	// rightDivide(a,b) * a = b
	def rightDivide(a: T, b: T): T
}

trait IdentityMagma[T] // Identity
extends Magma[T]
{
	// Assert that for all a, op(identity,a) = a
	// and op(a, identity) = a
	def identity: T
}

// ----------------- Combinations ---------------------
trait Monoid[T]
extends SemiGroup[T] with IdentityMagma[T]

trait Loop[T]
extends QuasiGroup[T] with IdentityMagma[T]
{
	// Divisibility in the present of an identity element
	// implies invertability:

	// Assert that for all a,
	//  leftInverse(a) * a = identity
	def leftInverse(a: T): T
	def rightDivide(a: T, b: T) = op(leftInverse(a),b)

	// Assert that for all a,
	//  a * rightInverse(a) = identity
	def rightInverse(a: T): T
	def leftDivide(a: T,b: T) = op(b,rightInverse(a))
}

trait CommutativeQuasiGroup[T]
extends QuasiGroup[T] with CommutativeMagma[T]
{
	// Assert that for all a,b
	// a * divide(a,b) = b
	// and
	// divide(a,b) * b = b
	def divide(a: T, b: T): T
	override def leftDivide(a: T, b: T) = divide(a,b)
	override def rightDivide(a: T, b: T) = divide(a,b)
}

trait CommutativeLoop[T]
extends Loop[T] with CommutativeQuasiGroup[T]
{
	def inverse(a: T): T
	def leftInverse(a: T) = inverse(a)
	def rightInverse(a: T) = inverse(a)
	def divide(a: T, b: T) = op(inverse(a), b)
}

trait Group[T]
extends Loop[T] with SemiGroup[T] with Monoid[T] with QuasiGroup[T]

trait AbelianGroup[T]
extends Group[T] with CommutativeLoop[T]

// ----------------- Instances ---------------------
object MagmaInstances
{
	// Magma
	implicit object NonzeroNaturalsUnderAdditionIsMagma
	extends Magma[Nstar]
	{
		def op(a: Nstar, b: Nstar) = Nstar(a.value + b.value)
	}
	
	// Associative
	// Identity
	implicit object NaturalsUnderAdditionIsIdentityMagma
	extends IdentityMagma[N0]
	{
		def op(a: N0, b: N0) = N0(a.v + b.v)
		val identity: N0 = N0(0)
	}
	
	// Divisibility
	implicit object IntsUnderSubtractionIsQuasiGroup
	extends QuasiGroup[Int]
	{
		def op(a: Int, b: Int) = a - b
		def leftDivide(a: Int, b: Int) = b - a
		def rightDivide(a: Int, b: Int) = a - b
	}

	implicit object NonzeroRationalsUnderDivisionIsQuasiGroup
	extends QuasiGroup[Qstar]
	{
		def op(a: Qstar, b: Qstar) = Qstar(a.v / b.v)
		def leftDivide(a: Qstar, b: Qstar) = Qstar(a.v / b.v)
		def rightDivide(a: Qstar, b: Qstar) = Qstar(b.v * a.v)
	}
	
	implicit object IntsUnderAdditionIsAbelianGroup
	extends AbelianGroup[Int]
	{
		def op(a: Int, b: Int) = a + b
		val identity = 0
		def inverse(a: Int) = - a
	}

	implicit object RationalsUnderMultiplicationIsAbelianGroup
	extends AbelianGroup[Double]
	{
		def op(a: Double, b: Double) = a * b
		val identity = 1.0
		def inverse(a: Double) = 1.0 / a
	}

}
