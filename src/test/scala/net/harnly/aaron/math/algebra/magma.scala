package net.harnly.aaron.math.algebra
import org.scalacheck._
import Arbitrary._
import Prop._

object NumberSetGenerators
{
	// Generators
	// Numbers
	implicit def makeArbN0: Arbitrary[N0] = Arbitrary(
		arbitrary[Int].map( n =>
			N0(Math.abs(n))
		)
	)
	
	implicit def makeArbNstar: Arbitrary[Nstar] = Arbitrary(
		arbitrary[Int].map( n =>
			Nstar(Math.abs(n) + 1)
		)
	)

	implicit def makeArbQstar: Arbitrary[Qstar] = Arbitrary(
		arbitrary[Double].filter( x => x != 0.0 ).map( x =>
			Qstar(x)
		)
	)
}

trait SemiGroupProperties[T]
extends Properties
{
	implicit def arbSG: Arbitrary[SemiGroup[T]]
	implicit def arbT: Arbitrary[T]
	// Properties
	// Semigroups
	// Assert that for all a, b, c: 
	// op(op(a,b),c) = op(a,op(b,c))	
	specify(
		"semigroupAssociative",
		(g: SemiGroup[T], a: T, b: T, c: T) => 
		g.op(g.op(a,b),c) == g.op(a, g.op(b, c))
	)
}

trait CommutativeMagmaProperties[T]
extends Properties
{
	import Arbitrary._
	implicit def arbCM: Arbitrary[CommutativeMagma[T]]
	implicit def arbT: Arbitrary[T]
	// Commutativity
	// Assert that for all a, b: 
	// op(a,b) = op(b,a)
	specify(
		"commutativeMagma",
		(g: CommutativeMagma[T], a: T, b: T) => 
		g.op(a,b) == g.op(b,a)
	)	
}

trait QuasiGroupProperties[T]
extends Properties
{
	import Arbitrary._
	implicit def arbQG: Arbitrary[QuasiGroup[T]]
	implicit def arbT: Arbitrary[T]
	// QuasiGroups
	// Assert that for all a,b
	// a * leftDivide(a,b) = b
	specify(
		"quasiGroupLeftDivisible",
		(g: QuasiGroup[T], a: T, b: T) => 
		g.op(a,g.leftDivide(a,b)) == b
	)
	// Assert that for all a,b
	// rightDivide(a,b) * a = b
	specify(
		"quasiGroupRightDivisible",
		(g: QuasiGroup[T], a: T, b: T) => 
		g.op(g.rightDivide(a,b),a) == b
	)
}
trait IdentityMagmaProperties[T]
extends Properties
{
	implicit def arbIM: Arbitrary[IdentityMagma[T]]
	implicit def arbT: Arbitrary[T]
	// Identity	Magma
	// Assert that for all a, op(identity,a) = a
	// and op(a, identity) = a
	specify(
		"identityMagmaLeft",
		(g: IdentityMagma[T], a: T) => 
		g.op(g.identity, a) == a
	)
	specify(
		"identityMagmaRight",
		(g: IdentityMagma[T], a: T) => 
		g.op(a, g.identity) == a
	)
}

trait LoopProperties[T]
extends IdentityMagmaProperties[T] with QuasiGroupProperties[T]
{
	implicit val arbL: Arbitrary[Loop[T]]
	// Loop
	// Assert that for all a,
	//  leftInverse(a) * a = identity
	specify(
		"loopLeftInverse",
		(g: Loop[T], a: T) => 
		g.op(g.leftInverse(a), a) == g.identity
	)
	specify(
		"loopRightInverse",
		(g: Loop[T], a: T) => 
		g.op(a,g.rightInverse(a)) == g.identity
	)
}

trait CommutativeQuasiGroupProperties[T]
extends CommutativeMagmaProperties[T] with QuasiGroupProperties[T]
{
	implicit val arbCQG: Arbitrary[CommutativeQuasiGroup[T]]
	// CommutativeQuasiGroup
	// Assert that for all a,b
	// a * divide(a,b) = b
	// and
	// divide(a,b) * b = b
	specify(
		"commutativeQuasiGroupDivisionLeft",
		(g: CommutativeQuasiGroup[T], a: T, b: T) =>
		g.op(a, g.divide(a,b)) == b
	)
	specify(
		"commutativeQuasiGroupDivisionRight",
		(g: CommutativeQuasiGroup[T], a: T, b: T) =>
		g.op(g.divide(a,b), a) == b
	)	
}

class GenericGenerator[T,G[_]](g: G[T])
{
	implicit val generator = Gen.value(g)
	implicit def makeArbitraryG(z: Arbitrary[G[T]])
		(implicit a: Arbitrary[T]): Arbitrary[G[T]] =
		new Arbitrary[G[T]] {
			def getArbitrary = generator
		}
}

object MagmaTest extends Application
{
	import MagmaInstances._

	object bar extends SemiGroupProperties[Int]
	{
		def arbSG = Arbitrary(
			Gen.elements(IntsUnderAdditionIsAbelianGroup)
		)
	}
	bar.checkProperties
}
