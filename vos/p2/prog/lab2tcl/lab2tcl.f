
!	VICAR Program LAB2TCL
!
!   LAB2TCL copies information from the system and history sections of a VICAR
!   label into user specified TCL variables.
!
!   31 OCT 1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
!

	include 'VICMAIN_FOR'
	subroutine main44

	implicit none
	include 'pgminc'	                ! definitions for returning 
						! values to user

	integer*4 MAX_LABEL_ITEM_SIZE, MAX_REQ, MAX_NTASKS, FNDKEY
	parameter (
     +	  MAX_LABEL_ITEM_SIZE = 512,	! max length allowed for keyword value
     +	  MAX_REQ = 20,	! max number of keywords that can be looked up
     +	  MAX_NTASKS = 100,	! max # of tasks that can be grabbed from label
     +	  FNDKEY = -38)	! error from xlinfo: key not found

	logical*4 xvptst		! routine to test for parameter switch
	integer*4 unit,		! i/o unit for input file
     +	  status,		! status from xv* calls
     +	  max_length,		! max value len. found for multivalued keyword
     +	  n_elements,		! num. of values found for multivalued keyword
     +	  num_requests,	        ! number of keywords to search for
     +	  val_length,		! length of a particular keyword's value
     +	  vblock(xprdim),	! vehicle for returning values to user
     +	  i,			! index for current keyword
     +	  i_format,		! integer version of format, below
     +	  srch_which_sect,	! switch indicating search SYS, HIST, or ALL
     +	  retrv_from_sect	! switch indicating read SYS or HIST
	character message*80,	! prep area for messages to user
     +	  format*12,		! format type of keyword value
     +	  lbl_section(2)*8,	! buffer to contain strings SYSTEM & HISTORY
     +	  param_name*3,	        ! for generating names of the variable params Vn
     +	  key_word(MAX_REQ)*8   ! keywords to search for

	integer*4 int_value	! generic label value: integer
	real*4 real_value		! generic label value: real
	character char_value*(MAX_LABEL_ITEM_SIZE) ! generic label value: char

! The subroutine package used by LAB2TCL to access the image label--XL*--is
! very specific. The routine for searching for a keyword in label--XLINFO--
! will only look in one section of the label: SYSTEM or HISTORY. LAB2TCL will
! look in both, and it does this via two XLINFO calls. A search within the
! HISTORY section, however, has an additional restriction: it will only look 
! at the current task. Since the desired capability is to find the keyword no
! matter how many tasks the image has, a special search back through the task
! history is instituted if the search fails in the current task.
!
! This search calls XLHINFO to establish a table of tasks and task instances.
! This table lists the first MAX_NTASKS tasks in order of occurence along with 
! a modifier for each task telling the instance of that task occurence.
!
! For example, consider the following history:
!
!	  #	tasks	instances
!	-------------------------
!	  1	  A	   1
!	  2	  B	   1
!	  3	  A	   2
!	  4	  C	   1
!	  5	  B	   2
!	  6	  A	   3
!	  7	  A	   4
!
! Task "A" occurs 4 times. The first occurence is labeled instance #1; the
! fourth occurence is labeled instance #4.
!
! Once the table is set up, we start working backwards through the table.
! As before, we make XLINFO calls, but we modify them with the HIST (task
! name) and INSTANCE (task instance number) optional arguments. The XLGET
! call used to retrieve a found keyword is also modified with these arguments.
!
! If the number of tasks in the table is the maximum--MAX_NTASKS--we start the 
! backwards search with table entry #MAX_NTASKS, even if it is the current task.
! If the table contains fewer than MAX_NTASKS entries, we start with the second
! to last entry.
!
	integer*4 num_tasks,	! number of tasks found in image label
     +	    use_task,		! which task in table to use for search/retrv
     +	    instances(MAX_NTASKS) ! task instances
	character tasks(MAX_NTASKS)*8 ! task names

        call ifmessage('LAB2TCL - 09 Dec 2012 - rjb - (64-bit)')

	i_format = 0
	retrv_from_sect = 0		!initialize
! Init the label type string buffer
	lbl_section(1) = 'SYSTEM'
	lbl_section(2) = 'HISTORY'

! Get file unit number and open the image file
	call xvunit (unit, 'INP', 1, status,' ')
	call xvopen (unit, status, 'OPEN_ACT', 'SA',' ')

! Create the variable block used to return values through variables. We'll
! initialize the block to abort the program if there's an error.
	call xqini (vblock, xprdim, xabort)

! Get the label section for the search; if we're only searching in one 
! section, srch_which_sect is set to an value that can be used as an 
! index into the label section string buffer lbl_section. If we're searching
! both sections, we'll indicate that with a third, special, value.
! retrv_from_sect is used once we find the keyword: it is set to the section
! in which we found the keyword.
	if (xvptst('ALL')) then
	    srch_which_sect = 3			! SYSTEM & HISTORY
	else if (xvptst('SYSTEM')) then
	    srch_which_sect = 1			! SYSTEM
	    retrv_from_sect = 1
	else
	    srch_which_sect = 2			! HISTORY
	    retrv_from_sect = 2
	endif

! Get the name of the keyword to read
	call xvp ('KEYWORD', key_word, num_requests)

! Loop, searching for each supplied keyword
	do i = 1, num_requests

!     Should we use the results from the last keyword? If first character
!     of the keyword is a blank, yes.
	   if (key_word(i)(1:1) .eq. ' ') then

!	 Insure that this is not the first keyword
	      if (i .eq. 1) then
		 write (message, 10)
 10		 format (' The first keyword in a KEYWORD list ',
     +'cannot be an empty string.')
		    call xvmessage(message,' ')
		    go to 900
	      endif

	   else
!	 Look for the keyword in the label
	      if (srch_which_sect .lt. 3) then
!	     Search SYSTEM or HISTORY
		 call xlinfo (unit, lbl_section(srch_which_sect), 
     +		              key_word(i), format, max_length,
     +                        n_elements, status,' ')

	      else
!	     Search both, starting with SYSTEM (don't bother looking in
!	     HISTORY if the error from the SYSTEM search is "success" or
!	     something other than "key not found"
		call xlinfo (unit, lbl_section(1), key_word(i),
     +		 	     format, max_length, n_elements, status,' ')
		   if (status .eq. 1) then
		      retrv_from_sect = 1
		   elseif (status .eq. FNDKEY) then
		      retrv_from_sect = 2
		      call xlinfo (unit, lbl_section(2), key_word(i),
     +			     format, max_length, n_elements, status,' ')
		   endif
	      endif

!	 Did we find the label?
	      if (status.eq.FNDKEY.and.retrv_from_sect.eq.2) then
!	     Didn't find key, and our last search was in the HISTORY section
!	     This triggers a special backwards search through the image's
!	     task history.

!	     Create the task table, limited in size to MAX_NTASKS
	         num_tasks = MAX_NTASKS
		 call xlhinfo(unit,tasks,instances,num_tasks,status,' ')

!	     If the num_tasks in the table is MAX_NTASKS, start search there,
!	     otherwise, start search at num_tasks-1
		    if (num_tasks .lt. MAX_NTASKS) then
			num_tasks = num_tasks - 1
		    endif

!	     Search!
		    do use_task = num_tasks, 1, -1
			call xlinfo (unit, lbl_section(2), key_word(i),
     +			    format, max_length, n_elements, status,
     +			    'HIST', tasks(use_task), 'INSTANCE',
     +			    instances(use_task),' ')
			if (status .ne. FNDKEY) go to 450
		    enddo

!	     No success; set status so that the case where there's only one
!	     task in the label is handled (if there's only one task, the do 
!	     loop will not be executed and status will equal 1 from the
!	     xlhinfo call; use_task is not used if status=FNDKEY)
		    status = FNDKEY
 450		    continue

		else
!	     Set use_task to 0 to indicate that we're not using the task
!	     table during the xlget call (execution is faster if we can
!	     omit optional arguments)
		    use_task = 0
		endif

		if (status .ne. 1) then
!	     Didn't find key when looking for SYSTEM alone, didn't find key
!	     during special backwards search thru HISTORY section, or error 
!	     encountered during either a SYSTEM or HISTORY search
		    write (message, 
     +			'('' Keyword '',A,'' not found in label.'')')
     +			key_word(i)
		    call xvmessage (message,' ')
		    go to 900

		else
!	 Read the desired label. We'll make a distinction among INT, REAL, and
!	 STRING for the xlget call because we want to return the variable using
!	 its proper type. If the type isn't INT or REAL, we'll return it as a
!	 STRING to protect the software from future types (such as BINARY).
!	 We'll distinguish between task table use and non-use for the xlget
!	 call for efficiency.
		  if (format(1:3) .eq. 'INT') then
		     if (use_task .eq. 0) then
		        call xlget (unit,lbl_section(retrv_from_sect), 
     +			            key_word(i), int_value, status,
     +                              'FORMAT','INT',' ')
		     else
			call xlget (unit,lbl_section(retrv_from_sect), 
     +			     key_word(i), int_value, status, 'HIST',
     +			     tasks(use_task), 'INSTANCE',
     +			     instances(use_task),'FORMAT','INT',' ')
		     endif
		     i_format = 1

		  else if (format(1:3) .eq. 'REA') then
		     if (use_task .eq. 0) then
			call xlget (unit,lbl_section(retrv_from_sect), 
     +			     key_word(i), real_value, status,
     +                       'FORMAT','REAL',' ')
		     else
		        call xlget (unit,lbl_section(retrv_from_sect), 
     +			     key_word(i), real_value, status, 'HIST',
     +			     tasks(use_task), 'INSTANCE',
     +			     instances(use_task),'FORMAT','REAL',' ')
		     endif
			i_format = 2
		  else
		     if (use_task .eq. 0) then
			call xlget (unit,lbl_section(retrv_from_sect), 
     +			     key_word(i),char_value,status, 'FORMAT',
     +			     'STRING', 'LENGTH', val_length,' ')
		     else
			call xlget (unit,lbl_section(retrv_from_sect), 
     +			     key_word(i),char_value,status,'FORMAT',
     +			     'STRING', 'LENGTH', val_length,'HIST',
     +			     tasks(use_task), 'INSTANCE',
     +			     instances(use_task),' ')
		     endif
		     i_format = 3
		  endif
		endif
	    endif

!     Generate the name of the parameter for this value: one of the set
!     {V1, V2, V3,..., V20}
	    if (i .lt. 10) then
		write (param_name, '(''V'',I1,'' '')') i
	    else
		write (param_name, '(''V'',I2)') i
	    endif

!     Return what we have: either a newly read value, or the value from the
!     last read; the write is to the variable pointed to by the parameter 
!     whose name is param_value and we're adding 1 value.

	    go to (610, 620, 630), i_format

!     Integer output
 610	    call xqintg (vblock, param_name, 1, int_value, xadd, status)
	    go to 680

!     Real output
 620	    call xqreal (vblock, param_name, 1, real_value, xadd, status)
	    go to 680

!     Character output
 630	    call xqstr (vblock, param_name, 1, char_value(1:val_length), 
     +		xadd, status)
	    go to 680

 680	    continue
	enddo  ! i from 1 to num_requests


! Send the variable block to the user
	call xvqout (vblock, status)

! Close input file
	call xvclose (unit, status,' ')
	return

! Error: close input file and terminate program abruptly
 900	call xvclose (unit, status,' ')
	call abend

	end	! main44

