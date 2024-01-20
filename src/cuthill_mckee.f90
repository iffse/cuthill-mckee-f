! Cuthill-McKee algorithm module
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

! ======================================================================

! This module contains the Cuthill-McKee algorithm for computing a permutation
! vector for a given connection list or adjacency matrix.

! By list of cell we mean a matrix, where the rows represent a cell (edge,
! triangle, square, etc.) and the columns represent the nodes that are forming
! the cell. For cells with more than 3 nodes, the node at index i should be
! connected to the nodes at indices i-1 and i+1.

! By connection list we mean a matrix, where the rows represent a node and the
! columns represent the nodes that are connected to the node at the row index.

! By adjacency matrix we mean a matrix, where the rows and columns represent
! the nodes of the graph and the values represent whether the nodes are
! connected or not. The matrix should be symmetric, with the diagonal being
! all zeros. The matrix is represented as a boolean matrix.

! Functions:
!   - CM_from_list: returns a permutation vector as computed by the
!                   Cuthill-McKee algorithm, with a list of cell as argument
!   - CM_from_connection: returns a permutation vector as computed by the
!                         Cuthill-McKee algorithm, with connection list as argument
!   - CM_from_matrix: returns a permutation vector as computed by the
!                     Cuthill-McKee algorithm, with adjacency matrix as argument
!   - reverse: returns the vector in reverse order. Use this for RCM.

module cuthill_mckee_module
	implicit none

	private
	public :: CM_from_matrix, CM_from_list, CM_from_connection
	public :: reverse, get_connected_nodes_from_list

contains
	! Returns a permutation vector as computed by the Cuthill-McKee algorithm
	! args:
	!	A: the adjacency matrix of the graph, represented as a boolean matrix
	function CM_from_matrix(A) result(perm)
		logical, intent(in) :: A(:,:)
		integer :: i, j, k, n, num, new_num, min_deg, min_idx, count
		integer, allocatable :: degree(:), perm(:), queue(:), new_queue(:)
		logical, allocatable :: visited(:)

		n = size(A, 1)
		allocate(perm(n))
		allocate(queue(n))
		allocate(degree(n))
		allocate(new_queue(n))
		allocate(visited(n))

		perm = 0
		new_queue = 0
		visited = .false.
		degree = get_degree_from_matrix(A)

		min_idx = 0
		num = 0
		new_num = 0

		do count = 1, n
			! looking for the smallest degree vertex
			min_deg = n + 1

			if (num == 0) then
				if (count /= 1) then
					write(*,*) "Error: graph is not connected"
					stop 1
				end if
				do i = 1, n
					if (degree(i) < min_deg) then
						min_deg = degree(i)
						min_idx = i
					end if
				end do
				visited(min_idx) = .true.
			else
				do i = 1, num
					j = queue(i)
					if (degree(j) < min_deg) then
						min_deg = degree(j)
						min_idx = j
						k = i
					else if (degree(j) == min_deg .and. j < min_idx) then
						min_idx = j
						k = i
					end if
				end do
			end if

			perm(min_idx) = count
			if (num /= 0) then
				! remove the current vertex from the queue
				call remove_nth(queue, k)
				num = num - 1
			end if

			! add the neighbors of the current vertex to the new queue
			do i = 1, n
				if (A(min_idx, i) .and. .not. visited(i)) then
					new_num = new_num + 1
					new_queue(new_num) = i
					visited(i) = .true.
				end if
			end do

			if (num == 0) then
				! copy the new queue to the queue
				queue = new_queue
				num = new_num
				new_num = 0
			end if
		end do
	end function CM_from_matrix

	! Returns the reverse of the given permutation vector
	! args:
	!	A: the adjacency matrix of the graph, represented as a boolean matrix
	function CM_from_list(L) result(perm)
		integer, intent(in) :: L(:,:)
		integer, allocatable :: C(:,:), perm(:)
		integer :: n

		n = maxval(L)
		C = get_connected_nodes_from_list(L)
		perm = CM_from_connection(C)
	end function CM_from_list

	function CM_from_connection(C) result(perm)
		integer, intent(in) :: C(:,:)
		integer :: i, j, k, n, num, new_num, min_deg, min_idx, count
		integer, allocatable :: degree(:), perm(:), queue(:), new_queue(:)
		logical, allocatable :: visited(:)

		n = maxval(C)

		allocate(perm(n))
		allocate(new_queue(4))
		allocate(visited(n))

		new_queue = 0
		perm = 0
		visited = .false.

		min_idx = 0
		num = 0
		new_num = 0

		degree = get_degree_from_list(C)

		do count = 1, n
			min_deg = n + 1

			if (num == 0) then
				if (count /= 1) then
					write(*,*) "Error: graph is not connected at node ", count
					stop 1
				end if
				do i = 1, n
					if (degree(i) < min_deg) then
						min_deg = degree(i)
						min_idx = i
					end if
				end do
				visited(min_idx) = .true.
			else
				do i = 1, num
					j = queue(i)
					if (degree(j) < min_deg) then
						min_deg = degree(j)
						min_idx = j
						k = i
					else if (degree(j) == min_deg .and. j < min_idx) then
						min_idx = j
						k = i
					end if
				end do
			end if

			perm(min_idx) = count
			if (num /= 0) then
				! remove the current vertex from the queue
				call remove_nth(queue, k)
				num = num - 1
			end if

			! add the neighbors of the current vertex to the new queue
			do i = 1, size(C, 2)
				j = C(min_idx, i)
				if (j == 0) exit
				if (.not. visited(j)) then
					call add_vec(new_queue, new_num, j)
					visited(j) = .true.
				end if
			end do

			if (num == 0) then
				! copy the new queue to the queue
				i = size(new_queue)
				if (allocated(queue)) deallocate(queue)
				call move_alloc(new_queue, queue)
				num = new_num
				new_num = 0
				allocate(new_queue(i))
			end if

		end do
	end function CM_from_connection

	function reverse(P) result(R)
		integer, allocatable :: P(:), R(:)
		integer :: n, i

		n = size(P)
		allocate(R(n))

		! reverse the array
		do i = 1, n
			R(i) = n - (P(i) - 1)
		end do
	end function reverse

	function get_degree_from_matrix(A) result(deg)
		logical, intent(in) :: A(:,:)
		integer :: i, j, n
		integer, allocatable :: deg(:)

		n = size(A, 1)
		allocate(deg(n))

		deg = 0
		do i = 1, n
			do j = 1, n
				if (A(i,j)) deg(i) = deg(i) + 1
			end do
		end do
	end function get_degree_from_matrix

	function get_connected_nodes_from_list(L) result(C)
		integer, intent(in) :: L(:,:)
		integer :: i, j, n, num_elements
		integer :: curr, next
		integer, allocatable :: C(:,:)

		num_elements = maxval(L)
		allocate(C(num_elements, 2))
		C = 0

		n = size(L, 1)

		do i = 1, n
			do j = 1, size(L, 2)
				curr = L(i, j)
				if (curr == 0) exit
				if (j == size(L, 2)) then
					next = L(i, 1)
				else if (L(i, j + 1) == 0) then
					next = L(i, 1)
				else
					next = L(i, j + 1)
				end if
				call add_mat(C, curr, next)
				call add_mat(C, next, curr)
			end do
		end do
	end function get_connected_nodes_from_list

	function get_degree_from_list(C) result(deg)
		integer, intent(in) :: C(:,:)
		integer :: i, j, n
		integer, allocatable :: deg(:)

		n = size(C, 1)
		allocate(deg(n))
		deg = 0

		do i = 1, n
			do j = 1, size(C, 2)
				if (C(i,j) == 0) exit

				deg(i) = deg(i) + 1
			end do
		end do
	end function get_degree_from_list

	! Removes the nth element from the given array, shifting all the elements
	! after it to the left
	subroutine remove_nth(v, n)
		integer, intent(inout) :: v(:)
		integer, intent(in) :: n
		integer :: i, j

		do i = 1, n - 1
			v(i) = v(i)
		end do
		j = size(v)
		do i = n + 1, j
			v(i - 1) = v(i)
		end do
		v(j) = 0
	end subroutine remove_nth

	! adds the given element to the end of the given array,
	! resizing it if necessary (doubling its size)
	subroutine add_vec(v, n, x)
		integer, allocatable, intent(inout) :: v(:)
		integer, intent(inout) :: n
		integer, intent(in) :: x
		integer, allocatable :: tmp(:)

		if (n == size(v)) then
			! resize the array
			allocate(tmp(2 * n))
			tmp = 0
			tmp(1:n) = v
			deallocate(v)
			call move_alloc(tmp, v)
		end if

		n = n + 1
		v(n) = x
	end subroutine add_vec

	! adds the given element to the given matrix, to the first
	! non-zero position in the row,
	! resizing it if necessary (doubling its size)
	subroutine add_mat(M, node, conn)
		integer, allocatable, intent(inout) :: M(:,:)
		integer, intent(in) :: node
		integer, intent(in) :: conn
		integer :: i, n
		integer, allocatable :: tmp(:,:)

		do i = 1, size(M, 2)
			if (M(node, i) == conn) return
			if (M(node, i) == 0) then
				M(node, i) = conn
				return
			end if
		end do

		! here, we need to resize the matrix
		n = size(M, 2)

		allocate(tmp(size(M, 1), 2 * n))
		tmp = 0
		tmp(:, 1:n) = M
		deallocate(M)
		call move_alloc(tmp, M)

		M(node, n + 1) = conn
	end subroutine add_mat

end module cuthill_mckee_module
