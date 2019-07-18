module physics23

	using LinearAlgebra
	
	export reciprocalVectors1

	reciprocalVectors1(a1,a2,a3) = a1./dot(a1,(cross(a2,a3)))

end
