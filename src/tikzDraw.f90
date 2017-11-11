include 'mod_string.f90'
program FeynmanDraw
    use string
    double precision, parameter :: PI=3.141592653589793d0
    integer, parameter :: order = 3
    integer, parameter :: vertexN = 2 * order
    integer :: next(vertexN)
    !---------------------------------------------------------------------------
    ! interaction vertex association and line association
    !---------------------------------------------------------------------------
    integer  :: v_ass(vertexN)
    integer  :: w_vert(vertexN)
    logical :: self_energy, polarization, free_energy
    call initialize

    next = [4, 3, 6, 5, 1, 2]

    self_energy = .True.; polarization = .False.; free_energy = .False.
    call TikzStart
    call TikzDraw
    call TikzEnd

    self_energy = .False.; polarization = .True.; free_energy = .False.
    call TikzStart
    call PolarDraw
    call TikzEnd

    self_energy = .False.; polarization = .False.; free_energy = .True.
    call TikzStart
    call TikzDraw
    call TikzEnd
contains
    subroutine initialize
        integer :: vert
        !-------------------------------------------------------------!
        ! vertex association of interaction line
        !-------------------------------------------------------------!
        do vert=1, order
            v_ass(2*vert-1)=2*vert; v_ass(2*vert)= 2*vert-1;
        end do
        do vert=1, order
            w_vert(2*vert-1)=vert; w_vert(2*vert)=vert;
        end do
    end subroutine initialize

    integer function loops()
        implicit none
        logical :: marked(vertexN) ! mark for the processed propagators
        integer :: i, j
        marked=.false.
        loops=0
        do j=1, vertexN
            if (marked(j)) cycle
            i = j
            do
                marked(i)=.true.
                i=next(i)
                if (i==j) exit
            end do
            loops=loops+1
        end do
    end function loops

    SUBROUTINE TikzStart
        if (self_energy) then
            open(10, file='Sigma'//str(order)//'.tex',status ='replace')
        else if (polarization) then
            open(10, file='Polar'//str(order)//'.tex',status ='replace')
        else if (free_energy) then
            open(10, file='Feynman'//str(order)//'.tex',status ='replace')
        end if
            write(10,"(A)") '\documentclass[margin=10pt]{standalone}'
            write(10,"(A)") '\usepackage{tikz}'
            write(10,"(A)") '\input{tikz}'
            write(10,"(A)") '\begin{document}'
        close(10)
    END SUBROUTINE TikzStart

    SUBROUTINE TikzEnd
        if (self_energy) then
            open(10, file='Sigma'//str(order)//'.tex', position ='append')
        else if (polarization) then
            open(10, file='Polar'//str(order)//'.tex', position ='append')
        else if (free_energy) then
            open(10, file='./Feynman'//str(order)//'.tex', position ='append')
        end if
            write(10,"(A)") '\end{document}'
        close(10)
    END SUBROUTINE TikzEnd

    SUBROUTINE Tikzdraw0
        if (self_energy) then
            open(10, file='Sigma'//str(order)//'.tex',status ='replace')
        else if (polarization) then
            open(10, file='Polar'//str(order)//'.tex',status ='replace')
        else if (free_energy) then
            open(10, file='./Feynman'//str(order)//'.tex',status ='replace')
        end if
            write(10,"(A)") '\documentclass[a4paper,12pt]{article}'
            write(10,"(A)") '\usepackage{longtable}'
            write(10,"(A)") '\usepackage{pdflscape}'
            write(10,"(A)") '\usepackage{graphicx}'
            write(10,"(A)") '\input{tikz}'
            write(10,"(A)") '\usepackage{fancyhdr}'
            write(10,"(A)") '\pagestyle{empty}'
            write(10,"(A)") '\begin{document}'
            write(10,"(A)") '\begin{center}'
            write(10,"(A)") 'Order '//str(order)//'diagrams'
            write(10,"(A)") '\end{center}'
            write(10,"(A)") '\begin{longtable}{@{\extracolsep{\fill}}ccc@{}}'
            write(10,"(A)") '\hline\hline \\ [0.5ex]'
        close(10)
    END SUBROUTINE Tikzdraw0

    SUBROUTINE Tikzdraw1
        if (self_energy) then
            open(10, file='Sigma'//str(order)//'.tex', position ='append')
        else if (polarization) then
            open(10, file='Polar'//str(order)//'.tex', position ='append')
        else if (free_energy) then
            open(10, file='./Feynman'//str(order)//'.tex', position ='append')
        end if
            write(10,"(A)") '\\  [0.5ex]'
            write(10,"(A)") '\hline'
            write(10,"(A)") '\end{longtable}'
            write(10,"(A)") '\end{document}'
        close(10)
    END SUBROUTINE Tikzdraw1

    !--------------------------------------------------------------------------!
    ! IF self_energy == .True.  draw "self-energy" diagram
    ! IF self_energy == .FALSE. draw "free-energy" diagram
    !--------------------------------------------------------------------------!
    SUBROUTINE TikzDraw
        logical :: outgoing, incoming
        integer :: i, j, k, loop, counter, v1, v2, k1, i1
        ! parameter for placing different loop in tikzpicture group
        double precision :: theta
        real :: dx, dy
        ! marked(:) is used to calculate the total loop number, markU(:) is to
        ! make sure every interaction line is drawn in tikz
        logical :: marked(vertexN), markU(order)
        ! LoopCount(k): number of fermions node in k-th loop
        ! nnvv(i,j) is the vertex number of i-th node in j-th loop
        integer, allocatable :: nnvv(:,:), LoopCount(:)
        ! drawing up to diagrams up to 8 looops
        ! 'vname(i)j' is the node label of the j-th vertex in i-th loop
        character :: vname(8)=(/'a', 'b', 'c', 'd', 'e', 'f', 'g','h'/)

        TYPE :: VmapN
            character(1) :: name
            integer   :: index
        END TYPE VmapN
        ! vvnn(j) gives the node label of j-th vertex, e.g. a3 where a is the
        ! name and 3 is the index
        type(VmapN) :: vvnn(vertexN)

        ! cut incoming or outgoing line
        incoming = .FALSE.; outgoing = .True.
        loop = loops()
        allocate( nnvv(vertexN, loop), LoopCount(loop) )

        nnvv =0; LoopCount =0; loop = 0; marked = .FALSE.
        do i=1, vertexN
            if (marked(i) .eqv. .FALSE.) then
                loop = loop + 1
                counter = 1
                nnvv(counter, loop) = i
                marked(i) = .True.
                j = i
                do
                    j= next(j)
                    if(j == i) then
                        ! had a bug here, foreget loop argument
                        ! LoopCount = counter
                        LoopCount(loop) = counter
                        exit
                    else
                        counter = counter + 1
                        nnvv(counter,loop) = j
                        marked(j) = .True.
                    end if
                end do
            end if
        end do

        if ( loop /= loops() ) stop 'check loop number calculation'
        if (loop > 8) stop 'loop number too big to draw'

        ! from the vertex number, construct the node, e.g., (b3).
        do k=1, loop
            do i=1, LoopCount(k)
                j = nnvv(i,k)
                vvnn(j)%name = vname(k)
                ! print *, vvnn(j)%name
                vvnn(j)%index  = i
                ! print *, vvnn(j)%index
            end do
        end do


        if (self_energy) then
            open(10, file='Sigma'//str(order)//'.tex', position ='append')
                ! if (boldG) then
                ! open(10, file='./sigmaGW'//str(order)//'.tex',position='append')
                ! else
                ! open(10, file='./sigmaG0W'//str(order)//'.tex',position='append')
                ! end if
            else
                open(10, file='Feynman'//str(order)//'.tex',position='append')
                end if
                ! label all the vertex for later usage
                !--------------------   Relative Position  ------------------------!
                write(10,*)
                write(10,"(A)") '\begin{tikzpicture}'
                ! radius of the big circle to place different loops evenly
                write(10,"(A)") '\def \R {0.8}'
                ! the radius of a loop is sqrt(loopnum)*\ratio
                write(10,"(A)") '\def \ratio {0.5}'
                ! node Style
                write(10,"(A)") '\tikzstyle{every node}=[draw, fill=yellow, &
                    &circle,inner sep=.5pt,font=\tiny];'
                ! if drawing self-energy, then place the first loop on a line
                ! otherwise draw every loop in circles (scaled)
                markU = .FALSE.
                if (self_energy .eqv. .True.) then
                    write(10,"(A)") '\begin{scope}[yshift= -\R cm, scale=1, xscale = -1]'

                    !==================== relative position approach ==================!
                    ! i = LoopCount(1)
                    ! I'm not sure how to determine the proper offset, -0.10 works fine
                    ! write(10,"(A)") '\coordinate (X) at (-0.09,0);'
                    ! write(10,"(A)") '\node[left= '//trim(str( (i-1)/2.0 ))//&
                    ! &' cm of X] (a'//trim(str(i))//') {$'//trim(str(nnvv(i,1)))//'$};'

                    ! write(10,"(A)") '\path (0'//trim(str( (i-1)/2.0 ))//',0) node(a'&
                    ! &//trim(str(i))//') {$'//trim(str(nnvv(i,1)))//'$};'
                    ! relative position for easy later manual adjustment if necessary
                    ! do i= LoopCount(1)-1, 1, -1
                    !     write(10,"(A)") '\node[right= 1 cm of a'//trim(str(i+1))//&
                    !     &'] (a'//trim(str(i))//') {$'//trim(str(nnvv(i,1)))//'$};'
                    ! end do
                    !==================== relative position approach ==================!

                    !==================== Absolute position approach ==================!
                    ! cut the incoming line from node 1
                    if (incoming) then
                        do i= LoopCount(1), 1, -1
                            write(10,"(A)") '\coordinate (a'//trim(str(i))//') at ('//&
                                &trim(str( (LoopCount(1)+1-2*i)/2.0 ))//', 0);'
                            ! write(10,"(A)") '\node at (a'//trim(str(i))//') {$'//trim(str(nnvv(i,1)))//'$};'
                        end do
                    end if
                    ! cut the outgoing line from node 1
                    if(outgoing) then
                        do i= LoopCount(1), 2, -1
                            write(10,"(A)") '\coordinate (a'//trim(str(i))//') at ('//&
                                &trim(str( (LoopCount(1)+3-2*i)/2.0 ))//', 0);'
                            ! write(10,"(A)") '\node at (a'//trim(str(i))//') {$'//trim(str(nnvv(i,1)))//'$};'
                        end do
                        write(10,"(A)") '\coordinate (a1) at ('//&
                            &trim(str(-(LoopCount(1)-1)/2.0))//', 0);'
                    end if
                    !==================== Absolute position approach ==================!

                    !------------------------------------------------------------------!
                    ! draw propagator and interaction line TOTALLY in this loop
                    ! write(10,"(A)") '\begin{pgfonlayer}{background}'
                    do i= 1, LoopCount(1)-1
                        ! draw propagator lines
                        if(outgoing .AND. i==1) then
                            write(10,"(A)") '\draw[bareG] (a'//trim(str(LoopCount(1)))&
                                &//') -- (a1);'
                        else
                            write(10,"(A)") '\draw[bareG] (a'//trim(str(i))//') -- (a'//&
                                &trim(str(i+1))//');'
                        end if
                        v1 = nnvv(i,1)
                        ! draw interaction line belong to this loop if any
                        do j=i+1, LoopCount(1)
                            v2 = nnvv(j,1)
                            if (v_ass(v1) == v2 ) then
                                ! ALWAYS draw the interaction line on top side
                                if ((v2 == 2) .AND. outgoing) then
                                    write(10,"(A)") '\draw[bareU] (a'//trim(str(i))//&
                                        &') to [bend left] (a'//trim(str(j))//');'
                                else
                                    write(10,"(A)") '\draw[bareU] (a'//trim(str(i))//&
                                        &') to [bend right] (a'//trim(str(j))//');'
                                end if
                                markU(w_vert(v1)) = .True.
                                exit
                            end if
                        end do
                    end do
                    ! write(10,"(A)") '\end{pgfonlayer}'
                    ! draw the big propagator line below
                    ! write(10,"(A)") '\draw[bareG] (a'//trim(str(LoopCount(1)))//') to&
                    ! & [bend right] (a1);'
                    write(10,"(A)") '\end{scope}'
                    k1 = 2
                else
                    k1 = 1
                end if
                do k = k1, loop
                    ! place different loops evenly in a big circle of \R
                    theta = 360.d0/loop *(k-1) - 90.d0
                    theta = theta*PI/180.d0
                    dx = real(dcos(theta))
                    dy = real(dsin(theta))
                    write(10,"(A)") '\begin{scope}[xshift='//trim(str(dx))//&
                        &'*\R cm, yshift ='//trim(str(dy))//'*\R cm, yscale=0.6]'
                    ! place different nodes in a loop evenly on a circle of \r
                    write(10,"(A)") '\def \n {'//trim(str(LoopCount(k)))//'}'
                    dx = sqrt(LoopCount(k)*1.0)
                    write(10,"(A)") '\def \r {'//trim(str(dx))//'*\ratio cm}'
                    if (LoopCount(k) == 2) then
                        write(10,"(A)") '\def \twist {0}'
                    else if (LoopCount(k) == 3) then
                        write(10,"(A)") '\def \twist {30}'
                    else
                        write(10,"(A)") '\def \twist {0}'
                        ! we can fine tune everything here later
                    end if
                    ! write(10,"(A)") '\begin{pgfonlayer}{background}'
                    write(10,"(A)") '\foreach \s in {1,...,\n}{'
                    write(10,"(A)") '\draw[bareG] ({360/\n * (\s - 1)+\twist}:\r) '//&
                        &'arc ({360/\n * (\s - 1)+\twist}:{360/\n * (\s)+\twist}:\r);}'
                    ! write(10,"(A)") '\end{pgfonlayer}'
                    ! label the node using vertex number
                    do i = 1, LoopCount(k)
                        write(10,"(A)") '\coordinate ('//vname(k)//trim(str(i))//&
                            &') at ({360/\n *('//trim(str(i-1))//')+\twist}:\r);'
                        ! write(10,"(A)") '    \node at ('//vname(k)//&
                        ! &trim(str(i))//') {$'//trim(str(nnvv(i,k)))//'$};';
                    end do
                    write(10,"(A)") '\end{scope}'
                end do

                ! draw remaining interaction lines
                ! write(10,"(A)") '\begin{pgfonlayer}{background}'
                do k =1, loop
                    do i=1, LoopCount(k)
                        v1 = nnvv(i,k)
                        if (.not. markU(w_vert(v1)) ) then
                            v2 = v_ass(v1)
                            write(10,"(A)") '\draw[bareU] ('//vvnn(v1)%name//&
                                &trim(str(vvnn(v1)%index))//') -- ('//vvnn(v2)%name//&
                                &trim(str(vvnn(v2)%index))//');'
                            markU(w_vert(v1)) = .True.
                        end if
                    end do
                end do
                ! write(10,"(A)") '\end{pgfonlayer}'
                ! draw the vertex, in this way, we do not need <pgfonlayer> anymore
                do k=1, loops()
                    do i = 1, LoopCount(k)
                        write(10,"(A)") '\node at ('//vname(k)//trim(str(i))//')&
                            &{$'//trim(str(nnvv(i,k)))//'$};'
                        ! write(10,"(A)") '\fill[black] ('//vname(k)//trim(str(i))//&
                        ! &') circle(1.5pt);'
                    end do
                end do
                write(10,"(A)") '\end{tikzpicture}'
                write(10,*)
            close(10)
            deallocate( nnvv, LoopCount )
        END SUBROUTINE TikzDraw

        SUBROUTINE PolarDraw
            integer :: i, j, k, loop, counter, v1, v2, k1, i1, n
            ! parameter for placing different loop in tikzpicture group
            double precision :: theta
            real :: dx, dy
            ! marked(:) is used to calculate the total loop number, markU(:) is to
            ! make sure every interaction line is drawn in tikz
            logical :: marked(vertexN), markU(order)
            ! LoopCount(k): number of fermions node in k-th loop
            ! nnvv(i,j) is the vertex number of i-th node in j-th loop
            integer, allocatable :: nnvv(:,:), LoopCount(:)
            ! drawing up to diagrams up to 8 looops
            ! 'vname(i)j' is the node label of the j-th vertex in i-th loop
            character :: vname(8)=(/'a', 'b', 'c', 'd', 'e', 'f', 'g','h'/)

            TYPE :: VmapN
                character(1) :: name
                integer   :: index
            END TYPE VmapN
            ! vvnn(j) gives the node label of j-th vertex, e.g. a3 where a is the
            ! name and 3 is the index
            type(VmapN) :: vvnn(vertexN)

            loop = loops()
            allocate( nnvv(vertexN, loop), LoopCount(loop) )

            nnvv =0; LoopCount =0; loop = 0; marked = .FALSE.
            do i=1, vertexN
                if (marked(i) .eqv. .FALSE.) then
                    loop = loop + 1
                    counter = 1
                    nnvv(counter, loop) = i
                    marked(i) = .True.
                    j = i
                    do
                        j= next(j)
                        if(j == i) then
                            ! had a bug here, foreget loop argument
                            ! LoopCount = counter
                            LoopCount(loop) = counter
                            exit
                        else
                            counter = counter + 1
                            nnvv(counter,loop) = j
                            marked(j) = .True.
                        end if
                    end do
                end if
            end do

            if ( loop /= loops() ) stop 'check loop number calculation'
            if (loop > 8) stop 'loop number too big to draw'

            ! from the vertex number, construct the node, e.g., (b3).
            do k=1, loop
                do i=1, LoopCount(k)
                    j = nnvv(i,k)
                    vvnn(j)%name = vname(k)
                    ! print *, vvnn(j)%name
                    vvnn(j)%index  = i
                    ! print *, vvnn(j)%index
                end do
            end do

            open(10, file='Polar'//str(order)//'.tex',position='append')
                ! label all the vertex for later usage
                !--------------------   Relative Position  ------------------------!
                write(10,*)
                write(10,"(A)") '\begin{tikzpicture}'
                ! radius of the big circle to place different loops evenly

                ! made a mistake here
                if (loop >= 2) then
                    write(10,"(A)") '\def \R {'//trim(str(0.7*sqrt(1.0*&
                        &max( LoopCount(1),LoopCount(2)))))//'}'
                else
                    write(10,"(A)") '\def \R {1.0}'
                end if
                ! the radius of a loop is sqrt(loopnum)*\ratio
                write(10,"(A)") '\def \ratio {0.5}'
                ! node Style
                write(10,"(A)") '\tikzstyle{every node}=[draw, fill=yellow, &
                    &circle,inner sep=.5pt,font=\tiny];'
                ! if drawing self-energy, then place the first loop on a line
                ! otherwise draw every loop in circles (scaled)
                markU = .FALSE.

                do k = 1, loop
                    if (vvnn(2)%name  == vname(k)) then
                        ! print *, 'vertex 2 is in loop', k
                        k1 = k
                        exit
                    end if
                end do

                if (k1 /= 1 .AND. k1 /= 2 ) stop 'check data structure construction'

                do k = 1, loop
                    ! place different loops evenly in a big circle of \R
                    ! general structure of polarization diagram
                    !         ...      ...
                    !     __/_           __\_
                    ! 1 _/    \ ~~~~~~~ /    \_ 2
                    !    \____/         \____/
                    !       \ ...      .../
                    !
                    if (k == 1 ) then
                        theta = PI
                    else
                        if (k1 == 2) then
                            theta = 180.d0/(loop-1) *(k-2)
                            theta = theta*PI/180.d0
                        else
                            theta = 360.0/loop *(k-1)
                            theta = theta*PI/180.d0 + PI
                        end if
                    end if

                    dx = real(dcos(theta))
                    dy = real(sin(theta))
                    write(10,"(A)") '\begin{scope}[xshift='//trim(str(dx))//&
                        &'*\R cm, yshift ='//trim(str(dy))//'*\R cm, yscale=0.6]'
                    ! place different nodes in a loop evenly on a circle of \r
                    write(10,"(A)") '\def \n {'//trim(str(LoopCount(k)))//'}'
                    dx = sqrt(LoopCount(k)*1.0)
                    write(10,"(A)") '\def \r {'//trim(str(dx))//'*\ratio cm}'
                    if (k == 1) then
                        ! mandatory for 1st loop containing vertex 1
                        write(10,"(A)") '\def \twist {180}'
                    else if ( k == 2 .AND. k1 == 2 ) then
                        ! mandatory for the 2nd-loop if it contains vertex 2
                        write(10,"(A)") '\def \twist {0}'
                    else
                        ! value of \twist can be tuned
                        write(10,"(A)") '\def \twist {0}'
                    end if
                    ! write(10,"(A)") '\begin{pgfonlayer}{background}'
                    if(k==1 .AND. k1==1) then
                        i1 = vvnn(2)%index
                        print *, 'i1 =', i1
                        write(10,"(A)") '\foreach \s in {1,...,'//trim(str(i1-1))//'}{'
                        write(10,"(A)") '\draw[bareG] ({180/'//trim(str(i1-1))//&
                            &' * (\s-1) + \twist}:\r) '//&
                            &'arc ({180/'//trim(str(i1-1))//'*(\s-1)+\twist}:{180/'&
                            &//trim(str(i1-1))//'*\s+\twist}:\r);}'

                        n = LoopCount(1) - i1 + 1
                        write(10,"(A)") '\foreach \s in {1,...,'//trim(str(n))//'}{'
                        write(10,"(A)") '\draw[bareG] ({180/'//trim(str(n))//&
                            ' * (\s-1)}:\r) arc ({180/'//trim(str(n))//&
                            &'*(\s-1)}:{180/'//trim(str(n))//'*\s}:\r);}'
                    else
                        write(10,"(A)") '\foreach \s in {1,...,\n}{'
                        write(10,"(A)") '\draw[bareG] ({360/\n * (\s - 1)+\twist}:\r) '//&
                            &'arc ({360/\n * (\s - 1)+\twist}:{360/\n * (\s)+\twist}:\r);}'
                    end if
                    ! write(10,"(A)") '\end{pgfonlayer}'
                    ! label the node using vertex number
                    if(k==1 .AND. k1==1) then
                        i1 = vvnn(2)%index
                        do i = 1, i1
                            write(10,"(A)") '\coordinate ('//vname(k)//trim(str(i))//&
                                &') at ({180/'//trim(str(i1-1))//' *('//trim(str(i-1))//')+\twist}:\r);'
                        end do

                        n = LoopCount(1) - i1 + 1
                        print *, 'n=', n
                        do i = i1+1,  LoopCount(1)
                            write(10,"(A)") '\coordinate ('//vname(k)//trim(str(i))//&
                                &') at ({180/'//trim(str(n))//&
                                &' *('//trim(str(i-i1))//')}:\r);'
                        end do
                    else
                        do i = 1, LoopCount(k)
                            write(10,"(A)") '\coordinate ('//vname(k)//trim(str(i))//&
                                &') at ({360/\n *('//trim(str(i-1))//')+\twist}:\r);'
                            ! write(10,"(A)") '    \node at ('//vname(k)//&
                            ! &trim(str(i))//') {$'//trim(str(nnvv(i,k)))//'$};';
                        end do
                    end if
                    write(10,"(A)") '\end{scope}'
                end do

                ! draw interaction lines excluding 1~~~~~~~2
                do k =1, loop
                    do i=1, LoopCount(k)
                        v1 = nnvv(i,k)
                        if (.not. markU(w_vert(v1)) ) then
                            v2 = v_ass(v1)
                            if (v2 == 2) then
                                continue
                            else
                                write(10,"(A)") '\draw[bareU] ('//vvnn(v1)%name//&
                                    &trim(str(vvnn(v1)%index))//') -- ('//vvnn(v2)%name//&
                                    &trim(str(vvnn(v2)%index))//');'
                            end if
                            markU(w_vert(v1)) = .True.
                        end if
                    end do
                end do

                ! draw the vertex
                do k=1, loops()
                    do i = 1, LoopCount(k)
                        write(10,"(A)") '\node at ('//vname(k)//trim(str(i))//')&
                            &{$'//trim(str(nnvv(i,k)))//'$};'
                        ! write(10,"(A)") '\fill[black] ('//vname(k)//trim(str(i))//&
                        ! &') circle(1.5pt);'
                    end do
                end do
                write(10,"(A)") '\end{tikzpicture}'
                write(10,*)
            close(10)
            deallocate( nnvv, LoopCount )
        END SUBROUTINE PolarDraw
    end program FeynmanDraw
