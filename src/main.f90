! Cuthill-McKee algorithm example
! Copyright © 2023 iff

! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU Affero General Public License as
! published by the Free Software Foundation, either version 3 of the
! License, or (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Affero General Public License for more details.

! You should have received a copy of the GNU Affero General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

program cuthill_mckee_example
	use,intrinsic :: iso_fortran_env, only: wp => real64
	use json_module
	use cuthill_mckee_module
	implicit none
	integer :: i,j,k

	integer, allocatable :: triangles(:,:)
	integer, allocatable :: triangle(:)

	logical, allocatable :: adjacency_matrix(:,:)
	! logical, allocatable :: ordered_adjacency_matrix(:,:)
	integer, allocatable :: perm(:), perm2(:), rperm(:), rperm2(:)

	integer :: n_triangles
	integer :: n_nodes

	! Read in the input file (first command line argument)
	character(len=256) :: input_file
	call get_command_argument(1, input_file)

	write(*,*) "input file: ", input_file

	block
		type(json_file) :: json
		logical :: found

		character(len=256) :: path
		character(len=256) :: int_string

		call json%initialize()
		call json%load(filename = input_file)

		! Read in triangles
		call json%get("metaData.triangles", n_triangles, found)
		write(*,*) "number of triangles: ", n_triangles

		call json%get("metaData.nodes", n_nodes, found)
		write(*,*) "number of nodes: ", n_nodes

		allocate(triangles(n_triangles, 3))

		do i = 1, n_triangles
			write(int_string, "(i0)") i

			path = "mesh.triangles[" // trim(int_string) // "]"
			call json%get(path, triangle, found)

			if (.not. found) then
				write(*,*) "Error: Could not find ", path
				stop 1
			endif

			triangles(i,:) = triangle
		enddo

		call json%destroy()
	end block

	! Make adjacency matrix
	allocate(adjacency_matrix(n_nodes, n_nodes))
	adjacency_matrix = .false.
	do i = 1, n_triangles
		triangle = triangles(i,:)

		do j = 1, 3
			k = mod(j, 3) + 1
			adjacency_matrix(triangle(j), triangle(k)) = .true.
			adjacency_matrix(triangle(k), triangle(j)) = .true.
		enddo
	enddo

	allocate(perm(n_nodes))
	allocate(perm2(n_nodes))
	allocate(rperm(n_nodes))
	allocate(rperm2(n_nodes))
	perm = CM_from_list(triangles)
	perm2 = CM_from_matrix(adjacency_matrix)
	rperm = RCM_from_list(triangles)
	rperm2 = RCM_from_matrix(adjacency_matrix)

	if (any(perm /= perm2)) then
		write(*,*) "Error: Permutations do not match"
		stop 1
	endif

	if (any(rperm /= rperm2)) then
		write(*,*) "Error: Reverse permutations do not match"
		stop 1
	endif

	write(*,*) "permutation and reverse: "
	do i = 1, n_nodes
		write(*,*) i, perm(i), rperm2(i)
	enddo


	! lines for output (graphing)

	! allocate(ordered_adjacency_matrix(n_triangles, n_triangles))
	! ordered_adjacency_matrix = .false.
	! do i = 1, n_triangles
	! 		triangle = triangles(i,:)
	! 		do j = 1, 3
	! 				k = perm(triangle(j))
	! 				triangle(j) = k
	! 		enddo

	! 		do j = 1, 3
	! 				k = mod(j, 3) + 1
	! 				ordered_adjacency_matrix(triangle(j), triangle(k)) = .true.
	! 				ordered_adjacency_matrix(triangle(k), triangle(j)) = .true.
	! 		enddo
	! enddo

	! block
	! 	type(json_file) :: json

	! 	character(len=256) :: int_string
	! 	character(len=256) :: output_file

	! 	call get_command_argument(2, output_file)
	! 	if (output_file == "") then
	! 		output_file = "output.json"
	! 	endif

	! 	write(*,*) "writing output file"

	! 	call json%initialize()

	! 	call json%add("permutation", perm)

	! 	do i = 1, n_nodes
	! 		write(int_string, "(i0)") i
	! 		call json%add("original[" // trim(int_string) // "]", adjacency_matrix(i,:))
	! 		call json%add("ordered[" // trim(int_string) // "]", ordered_adjacency_matrix(i,:))
	! 	end do

	! 	call json%print(output_file)

	! end block

end program cuthill_mckee_example
