!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     ���{���O�w�����ʥ��q����q���x��ܾ��ӱM�׽s�g��     �̫�ק�����91�~8��12��
!              8/8, �o�{�����b�j���פW�����D
!              8/7, ������B�G�פ������
!              8/6, �ץ���5�Ӥp���~
!              8/5, �w�省��װ��D
!              8/1, �w�g�����F119�ؼ���
!              ���{���w�g�N�Ĥ@�Ϯg�M, �q���@���ഫ���ߪ��� ��²�ƤF�p�װ��D(7/26)
!                    type_4 ���W�٨Ӧ۲�4�N�]�p 8/12
!                    type_3 ���W�٨Ӧ۲ĤT�N�]�p
!                                                             �_�l�����91�~7��1��
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program type_4
use msflib
!use avviewer
implicit none
type(windowconfig)::wc
!type(xycoord)::wt
character(15):: xxx
logical::result
INTEGER::flag,FLAG_SURFACE ,AGAIN,CHANGE_SYSTEM ,result_int,G_plot
!�U���o�G��O�p��LED��������쨤�P���g�����N�ưѼ�
integer::led_x,led_y
integer::h,v,num_h,color
real(kind=8)::SCALE,screen_scale,XZ_SF,YZ_SF,pp,led_shift_z
real(kind=8)::angle_theta,angle_whay,del_whay,decay_constant
real(kind=8)::position_led_x,position_led_y,position_led_z,led_period
INTEGER:: num_circle
REAL(KIND=8):: del_arc,length_arc,total_photon
integer,allocatable::num_photon(:),flag_photon(:)
integer::led_r(4),led_theta(4)

real(kind=8)::n_pmma
real(kind=8)::theta  !all parameters for LED
! all parameters for photon
real(kind=8)::px,py,pz,del_x,del_y,del_z,ENERGY,pre_px,pre_py,pre_pz
real(kind=8)::n0,n01,n12
real(kind=8)::ds1,n1 ! surface 1
real(kind=8)::ds2,r2,r2_sqr,n2,f2,cs2,thick1  ! surface 2
real(kind=8)::DSCREEN
real(kind=8), parameter::rad=3.1415926/180.0

!�U���o�@��O�Ѥ�{���Τ��n�Ѽ�
real(kind=8)::pre_P_dot_pre_p, c0_dot_co !,del_dot_del
real(kind=8)::pre_P_dot_del,   del_dot_n,  pre_P_dot_C0,  del_dot_C0
!�U���o�@��O�Ѥ�{���ΥN�ưѼ�
real(kind=8)::a,half_b,c,t,t1,d1

!�U���o�@��O�ѥ��l�P�y�����I���k�V�q
real(kind=8)::length_vector,nx,ny,nz,tx,ty,tz,t1x,t1y,t1z
real(kind=8)::cx,cy,cz ,n_t,n_p

!�U���o�@��O��ӤϮg�M���t��k���Ѽ�
integer::counter_in_1, counter_out_1, counter_R_in, counter_R_out
integer::counter_total,counter_ellip, COUNTER_LED, counter_error
integer:: positive, negative
real(kind=8):: z_check1, z_check2, Z_check3
real(kind=8):: R_check1, R_check2, R_check3
real(kind=8):: R_check1_2, R_check2_2, R_check3_2
real(kind=8):: PR ,tem_x,tem_y,tem_z
real(kind=8):: focal_length_1,Focal_length_2,focus_spot, D_f1 ,zr
real(kind=8):: del,r_min,r_max

!�U���o�@��Oxy_SCREEN�������}�C
real(kind=8),allocatable:: xy_screen_for_r2(:,:)
real(kind=8),allocatable::total_screen(:,:)
integer:: density,light	,xx,yy ,shift_x_on_xy_sreen,shift_y_on_xy_sreen
real(kind=8):: c1,max_light

!�U���o�@��Oxy_SCREEN�����פ��R
integer::  num_del_theta, flag_theta
real(kind=8):: var_theta, RESOLUTION_ANGLE_PHOTON,peak_angle
real(kind=8),allocatable:: theta_distribution(:,:)

!�H�U��  ��ƿ�J  �������w�q
open(unit=5,file='user',iofocus=.true.)
wc.numxpixels=300
wc.numypixels=1000
wc.numtextcols=-1
wc.numtextrows=-1
wc.numcolors=16
wc.title="��J�ε���"
wc.fontsize=-1
result=setwindowconfig(wc)
result=displaycursor($gcursoron)
       !��ƿ�J��������
  open(unit=13,file='user',iofocus=.true.)
  wc.numxpixels=1200
  wc.numypixels=1200
  wc.numtextcols=-1
  wc.numtextrows=200
  wc.numcolors=-1
  wc.title="���G��"
  wc.fontsize=-1
  result=setwindowconfig(wc)


  result=ClickMenuQQ(QWin$Tile)
  OPEN(UNIT=10,FILE='TEST.txt')
!"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
AGAIN=1
SCALE=1.0
led_shift_z=14.58
thick1=1
XZ_SF=250										!
YZ_SF=500										!
DO WHILE(AGAIN==1)	!DO DO DO DO DO DO DO DO DO DO DO DO DO DO DO DO DO DO DO DO DO DO DO DO
counter_total=0        !�p��@���X�����l
counter_in_1=0         !�p��@���X�����l�q�L�Ϯg�M�����}
counter_out_1=0
counter_R_in=0
!counter_ring=0
counter_R_out=0
counter_ellip=0         !�p��@���X�����l�g�L�Ĥ@�Ϯg�M�᪽�F�O�n
counter_led=0           !�p��@�ΤF�X��led
counter_error=0
positive=0
negative=0
led_r(1)=10.0
led_r(2)=15.0
led_r(3)=18.0
led_r(4)=20.0
led_theta(1)=6
led_theta(2)=8
led_theta(3)=12
led_theta(4)=20

! �˴��O�_�n���t�ΩΥu�O����������v~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  result=setactiveqq(5)
  color=rgbtointeger(255,255,255)
  result=settextcolorrgb(color)
  write(5,*)'CHANGE SYSTEM???,YES(1),NO(0)'
  read(5,*)  CHANGE_SYSTEM
  IF (CHANGE_SYSTEM==1)THEN
    scale=1
    write(5,*)'�п�Jscreenø�Ϥ��,screen_SCALE'
    read(5,*)  screen_SCALE
    pp=screen_SCALE
!    write(5,*)'�п�J�ȹ��Z��,dscreen'
	!dscreen=140  !              92+led_shift_z
!    write(5,*)'�п�JLENS�Z��,ds1'
    ds1=1.8+led_shift_z
!    write(5,*)'�п�J��߫p��,thick1'
	thick1=2.69
    write(5,*)'�п�J�Ϯg�M���J�Z'
    READ(5,*) focal_length_1                 !�Ϯg�M���J�Z
    write(5,*)'�п�J�Ϯg�M�����դj�p'
    READ(5,*) R_check1                      !�Ĥ@�ˬd�b�|
    write(5,*)'�п�J�Ϯg�M���̤j�b�|'
    READ(5,*) R_check2                      !�ĤG�ˬd�b�|
focus_spot=50
focal_length_2=50.0	     						!�Ϯg�J���J�Z
R_check3=140
z_check1=focus_spot-Focal_length_1+R_check1*R_check1/(4.0*focal_length_1)  
!�Ĥ@�ˬd��m
z_check2=focus_spot-Focal_length_1+R_check2*R_check2/(4.0*focal_length_1)  
!�ĤG�ˬd��m
Z_check3=focus_spot-Focal_length_2+R_check3*R_check3/(4.0*focal_length_2)  
!�ĤT�ˬd��m
dscreen=Z_check3
write(5,'(a5,f6.2)')'z_1=',z_check1
write(5,'(a5,f6.2)')'z_2=',z_check2
write(5,'(a5,f6.2)')'z_3=',z_check3
!write(5,'(a10,f7.2)')'dscreen=',dscreen
zr=2*(focus_spot+focal_length_2)  !(=200)			!
R_check1_2=R_check1*R_check1
R_check2_2=R_check2*R_check2
R_check3_2=R_check3*R_check3

  END IF
  
!���t�Χ���~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! write(5,*)'�п�JS2���b�|,R2(mm)'  r2=-20
  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~   �]�w�����ù����Ѽ�   ~~~~~~~~~~~~~~~~~~~~~~~~~
  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !write(5,*)'�п�JXYscreen���K��,density(100)' 
!�]�w��}�ӭM��(DENSITY*DENSITY)
  !read(5,*)  density
  density=900
  allocate(xy_screen_for_r2(density,density))
  allocate(total_screen(density,density))
  c1=real(density)/300  !(R_check3*2)
  shift_x_on_xy_sreen=density/2
  shift_Y_on_xy_sreen=density/2
  xy_screen_for_r2=0
  !screen_3d=0
  total_screen=0
  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~   �]�w�����ù������l�����פ��R   ~~~~~~~~~~~~~~~~~~~~~~~~~
  !write(5,*)'�п�J���l�����פ��R���ѪR��,RESOLUTION_ANGLE_PHOTON (0.5)'
  !READ(5,*) RESOLUTION_ANGLE_PHOTON
  RESOLUTION_ANGLE_PHOTON=0.1
  num_del_theta=int(180.0/RESOLUTION_ANGLE_PHOTON)+1
  WRITE(5,*)'num_del_theta=',num_del_theta
  ALLOCATE(THETA_DISTRIBUTION(5,num_del_theta))

n_pmma=1.4917
n0=1.0
n1=n_pmma
n2=n0
n01=n0/n1
n12=n1/n2
r2_sqr=r2*r2
ds2=ds1+thick1
cs2=ds2+r2	 !�ΥH�w�qsurface 2 ���y�߮y��
r_min=-R_check2
r_max=-r_min
del=(r_max-r_min)/300.0
py=r_min

result=setactiveqq(13)	 ! ��v�ȹ�
result=setcolorrgb(rgbtointeger(0,0,255))
call ClearScreen($GCLEARSCREEN)
result=setactiveqq(12)
call ClearScreen($GCLEARSCREEN)
result=setactiveqq(13)!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do h=1,300
    py=py+del
	if (abs(py)>=r_check1) then
	   pz=py*py/(2*2*focal_length_1)+(focus_spot-focal_length_1)
	   result=SETPIXEL((density+1)+pp*pZ,300.0+pp*py)  !ø�� ���l�bY-Z�����y��
    end if
end do
    write(5,'(a3,f6.2,a4,f6.2)')'py=',py,'pz=',pz
	result=setactiveqq(13)
r_min=-140
r_max=-r_min
del=(r_max-r_min)/1000.0
py=r_min
do h=1,1000
    py=py+del
	pz=py*py/(2*2*focal_length_2)
	result=SETPIXEL((density+1)+pp*pZ,300.0+pp*py)  !ø�� ���l�bY-Z�����y��
end do
    write(5,'(a3,f6.2,a4,f6.2)')'py=',py,'pz=',pz
result=setactiveqq(12)!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	color=rgbtointeger(0,0,255)
	result=setcolorrgb(color)
    h=R_check1*pp
	result=ellipse($GBORDER,density+1-h,300-h,density+1+h,300+h)
    h=R_check2*pp
    result=ellipse($GBORDER,density+1-h,300-h,density+1+h,300+h)
	h=R_check3*pp
    result=ellipse($GBORDER,density+1-h,300-h,density+1+h,300+h)
result=setactiveqq(13)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!�w�qLED�����g��
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   write(5,*)'�п�JLED���i��,THETA(����)'
   read(5,*)  THETA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write(5,*)'�п�J���l�K��(del_arc)'
read(5,*)del_arc                               !�w�q���l�K��
write(5,*)'if plot test?(y/n--1/0)'
read(5,*)G_plot
length_arc=theta*rad						   !�`����=theta���H�b�|1
num_h=length_arc/del_arc					   !�`�����b���HDEL_ARC�N���󦳴X��
num_circle=num_h/2+1                           !�p�ⲣ�ʹX�ӥ��@(num_circle)
allocate(num_photon(num_circle))			   
!�w�qnum_photon�}�C���j�p��(num_circle)
allocate(flag_photon(num_circle))
  WRITE(5,*)'���@�@��',num_circle,'�h'
total_photon=0
do h=1, num_circle							   !
	 num_photon(h)=2*3.1415926*sin(real(h*del_arc))/del_arc	   
!�p��C�@�h���@���|���X�����l
	 !write(10,'(a12,i4,a2,i8)')'num_photon(',h,')=',num_photon(h)
	 total_photon=total_photon+num_photon(h)
end do
      WRITE(5,'(a12,i11,a10)')'�C��led�@��',total_photon,'���������l'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!    �}�l�l�ܥ��l
!!!!!	 ���b��z��V
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
led_period=6.0
decay_constant=(theta*theta*rad*rad/4)
flag_photon=0
led_r(1)=12.0
led_r(2)=15.0
led_r(3)=18.0
led_r(4)=20.0
led_theta(1)=6
led_theta(2)=12
led_theta(3)=18
led_theta(4)=24
do LED_x=2,4
   write(5,'(A4,I3)')'r=',led_x	! �g�J��ƿ�J����
do LED_Y=1, led_theta(led_x)
!   IF (.not.((led_X==1.and.(led_y==1.or.led_y==2.or.led_y==7.or.led_y==8)).OR.&
!            &(led_x==8.and.((led_y==1.or.led_y==2.or.led_y==7.or.led_y==8))).OR.&
!            &(LED_X==2.and.(led_y==1.or.led_y==8)).OR.(LED_X==7.and.(led_y==1.or.led_y==8))))THEN
      counter_led=counter_led+1
      position_led_x=led_r(led_x)*cos(rad*360.0/led_theta(led_x)*(led_y-1))
      position_led_y=led_r(led_x)*sin(rad*360.0/led_theta(led_x)*(led_y-1))
      position_led_z=led_shift_z
	  	cx=position_led_x
		cy=position_led_y
		cz=CS2
      result=setactiveqq(13)
	  color=rgbtointeger(255,0,0)
	  result=setcolorrgb(color)
      h=density/2+position_led_x*c1
	  v=density/2+position_led_y*c1
      result=ellipse($GBORDER,h-2,v-2,h+2,v+2)
	  result=SETPIXEL((density+1)+pp*position_led_z,300.0+pp*position_led_y)  
!ø�� ���l�bY-Z�����y��
do h=1, num_circle !int(0.4*num_circle),int(0.8*num_circle) ! ���̦����˥��@
        angle_theta=del_arc*(h)                            ! �p����@���@��
        del_whay=360.0/real(num_photon(h))         ! �wdel_whay�����@���@�W, ��۾F���l�����׮t
        energy=1.0-(angle_theta*angle_theta/decay_constant)
		if(energy<0)then
		energy=0
		end if
		   !flag_photon(h)=1
		   !if (flag_photon(h)/=1) then
		   !write(5,'(a13,i4,a2,i2)')'flag_photon(',h,')=',flag_photon(h)
		   !end if
		   
result=setactiveqq(12)!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           color=rgbtointeger(0,0,255)
	       result=setcolorrgb(color)
           v=R_check1*pp
	       result=ellipse($GBORDER,density+1-v,300-v,density+1+v,300+v)
           v=R_check2*pp
           result=ellipse($GBORDER,density+1-v,300-v,density+1+v,300+v)
	       v=R_check3*pp
           result=ellipse($GBORDER,density+1-v,300-v,density+1+v,300+v)
           result=setactiveqq(13)
flag_photon(h)=0
  do v=1,num_photon(h)	                      ! �A���˨C�@�ӥ��@���C�@�ӥ��l(��@�� �@360��)
	 angle_whay =rad*(del_whay *(v-1))	      ! �p����l, �b���@�W��whay����
	 del_X=sin(angle_theta)*cos(angle_whay)
	 del_y=sin(angle_theta)*sin(angle_whay)
	 del_z=cos(angle_theta)
           flag_photon(h)=flag_photon(h)+1
           if (v==num_photon(h)) then
		     if (num_photon(h)/=flag_photon(h))then
			 write(5,'(a7,i4,i6,i6)')'E?? h=',h,num_photon(h),flag_photon(h)

			 end if
           end if
    var_theta=angle_theta/rad
	flag_theta=int(var_theta/RESOLUTION_ANGLE_PHOTON)+1
	theta_distribution(1,flag_theta)=theta_distribution(1,flag_theta)+1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   pre_px=position_led_x	   ! px py pz�����l���e�@�Ӧ�m
   pre_py=position_led_y
   pre_pz=position_led_z
   px=pre_px
   py=pre_py
   pz=pre_pz
   FLAG_SURFACE=0
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!  single ponton tracing
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !DO while(flag_surface==0)	! the photon in the section 1  do1 do1 do1 do1 do1 do1 do1 do1
	 !�U�����欰�p��U���l��i��V
   t=(ds1-pz)/del_z
   px=pre_px+del_x*t
   py=pre_py+del_y*t
   pz=pre_pz+del_z*t
   !if (pz>=ds1)then	!first fast check for surface1!!!!! 
!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!
	  !  FLAG_SURFACE=1                                  !!  ���l�i�J����    !!
                                                        
!!!!!!!!!!!!!!!!!!!!!!
        if (G_plot==1) then
           result=setactiveQQ(11)
           result=setcolorrgb(rgbtointeger(255,100,0))
           result=SETPIXEL((density)+60*px,(density)+60*py)
        end if
	   if (del_z<0.999999) then	!��del_z����(1.0)��,���ܫ����󦱭��J�g, �~�n�p���,�|���ͤ�����0, �ҥH���}
				    nx=0
				    ny=0
					nz=1
				 t1=(ds1-pre_pz)/del_z
  				 px=pre_px+del_x*t1	!�p�⥭���P���l���I����m
				 py=pre_py+del_y*t1
				 pz=pre_pz+del_z*t1
				 del_dot_N=del_z*nz  !�p�⤺�n(�k�V�q�P���l)
				 n_p=n01*sqrt(1-del_dot_N*del_dot_N)
				 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				 Tx=del_y       !�Ĥ@���~�n
				 Ty=-del_x
				 Tz=0
				 T1x=-Ty		!�ĤG���~�n
				 T1y=Tx
				 T1z=0      !Ty*nx-Tx*ny
				 length_vector=sqrt(t1x*t1x+t1y*t1y)  !�p��k�V�q��������V��
				 t1x=t1x/length_vector					   !�ഫ�������V�q
				 t1y=t1y/length_vector
				 t1z=0      !t1z/length_vector
				 n_t=sqrt(1-n_p*n_p)
				 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
				 t1x=t1x*n_p	!�����p��s�����l��i��V�]�k�V�q��������V�^
				 t1y=t1y*n_p
				 !t1z=t1z*n_p
				 !nx=nx*n_t		!�����p��s�����l��i��V�]�k�V�q��V�^
				 !ny=ny*n_t
				 nz=nz*n_t
				 del_x=t1x	!�����p��s�����l��i��V
				 del_y=t1y
				 del_z=nz
	  end if !if(del_z<0.99999)
   !end if !if (pz>ds1)
   pre_px=PX
   pre_py=PY
   pre_pz=PZ
!end do !do while(flag_surface==0) do1.. do1.. do1.. do1.. do1.. do1.. do1.. do1.. do1..
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!      PHOTON INTO SURFACE 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!DO while(flag_surface==1)	! the photon in the section 2 do2 do2 do2 do2 do2 do2 do2
   !write(5,*) 'surface=2'
   !px=pre_px+del_x
   !py=pre_py+del_y
   !pz=pre_pz+del_z
	!  f2=px*px+py*py+(pz-CS2)*(pz-CS2)-R2_SQR
	!  IF(R2<0)THEN  
!1111111111111111111111111111111111111111111111111111111111
	!  F2=-F2
	!  END IF        !1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1..
	!  if(f2<0)then !double check for surface1 
!1111111111111111111111111111111111
	    FLAG_SURFACE=2
	   			 pre_P_dot_pre_p=pre_px*pre_px+pre_py*pre_py+pre_pz*pre_pz
		   		 pre_P_dot_del=pre_px*del_x+pre_py*del_y+pre_pz*del_z
				 pre_P_dot_C0=pre_px*cx+pre_py*cy+pre_pz*cz
				 del_dot_C0=del_x*cx+del_y*cy+del_z*cz
                 c0_dot_co=cx*cx+cy*cy+cz*cz
				 half_b=(pre_P_dot_del-del_dot_C0)              !for eq:ax2+2bx+c=0
				 c=pre_P_dot_pre_p+c0_dot_co-r2_sqr-2*pre_P_dot_C0  !here a=1
				 d1=half_b*half_b-c
				 !if (d1<0)then		!�U���������k��v!2222222222222222222222222222
                 !   write(5,*)'d1<0'
				 !   d1=0
				 !end if  !2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2..
				 !IF(R2>0)THEN  !222222222222222222222222222222222222222222222222
				 !   t1=-half_b-sqrt(d1) !solve the t
				 !   nx=cx-px			!�p��y���P���l���I���k�V�q
				 !   ny=cy-py			!�k�V�q���V�y��
				 !   nz=cz-pz
				 !ELSE          !2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5
				    t1=-half_b+sqrt(d1) !solve the t
				    nx=px-cx			!�p��y���P���l���I���k�V�q
				    ny=py-cy			!�k�V�q���V�y�~
				    nz=pz-cz
				 !END IF   !2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2..
			     !t=t2
  				 px=pre_px+del_x*t1	!�p��y���P���l���I����m
				 py=pre_py+del_y*t1
				 pz=pre_pz+del_z*t1
                 if(G_plot==1)then !plot plot plot plot plot plot plot plot plot plot plot
                    result=setactiveQQ(12)
                    result=setcolorrgb(rgbtointeger(0,255,255))
                    result=SETPIXEL((density/2)+80*px,density+80*py)
                 end if  !plot.. plot.. plot.. plot.. plot.. plot.. plot.. plot.. plot.. plot..
				 length_vector=sqrt(nx*nx+ny*ny+nz*nz)  !�p��k�V�q��
				 nx=nx/length_vector					   !�ഫ�������V�q
				 ny=ny/length_vector
				 nz=nz/length_vector
				 del_dot_N=del_x*nx+del_y*ny+del_z*nz  !�p�⤺�n(�k�V�q�P���l)
				 n_p=n12*sqrt(1-del_dot_N*del_dot_N)
				 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				 Tx=nz*del_y-ny*del_z
				 Ty=nx*del_z-nz*del_x
				 Tz=ny*del_x-nx*del_y
				 T1x=Tz*ny-Ty*nz
				 T1y=Tx*nz-Tz*nx
				 T1z=Ty*nx-Tx*ny
				 length_vector=sqrt(t1x*t1x+t1y*t1y+t1z*t1z)  !�p��k�V�q��������V��

				 !if (length_vector>0.000001) then !222222222222222222222222222222222222
				 t1x=t1x/length_vector					   !�ഫ�������V�q
				 t1y=t1y/length_vector
				 t1z=t1z/length_vector
				 !end if	  !2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2..
				 n_t=sqrt(1-n_p*n_p)
				 t1x=t1x*n_p	!�����p��s�����l��i��V�]�k�V�q��������V�^
				 t1y=t1y*n_p
				 t1z=t1z*n_p

				 nx=nx*n_t		!�����p��s�����l��i��V�]�k�V�q��V�^
				 ny=ny*n_t
				 nz=nz*n_t
				 del_x=nx+t1x	!�����p��s�����l��i��V
				 del_y=ny+t1y
				 del_z=nz+t1z
	  !end if !if(pz>f1) !1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1..
   pre_px=PX
   pre_py=PY
   pre_pz=PZ
!end do !do while(flag_surface==1)  do2.. do2.. do2.. do2.. do2.. do2.. do2.. do2..
    var_theta=acos(del_z)/rad
	flag_theta=int(var_theta/RESOLUTION_ANGLE_PHOTON)+1
	theta_distribution(2,flag_theta)=theta_distribution(2,flag_theta)+1

                 if(G_plot==1)then
                    result=setactiveQQ(12)
                    result=setcolorrgb(rgbtointeger(255,255,0))
                    result=SETPIXEL((density/2)+20*px,(density/2)+20*py)
                 end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!    ���l�����}�C���  �i�J�Ů�
!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do while(flag_surface==2)
   t=(z_check1-pz)/del_z
   px=pre_px+del_x*t
   py=pre_py+del_y*t
   pz=pre_pz+del_z*t

   !if(pz>=z_check1) then 
!~~nsnsnsnsnssns--begin	111111111111111111111111111111111111111111111111
		 pr=px*px+py*py		              !�˴����l��m���b�|
		 if(pr<R_check1_2)then            !�˴����l�O�_�i�J �t���M�����ߪŬ} 2222222222222222222
			      t=(dscreen-pz)/del_z
			      PX=px+del_x*t	             !�Y�O �h���l�����]��ZCHECK2
			      py=py+del_y*t
			      pz=pz+del_z*t
			      counter_in_1=counter_in_1+1
				  flag=1                                   ! flag=1�O�����l������L���}
				  if (G_plot==1) then !
			      result=setactiveqq(12)
				  result=setcolorrgb(rgbtointeger(0,255,0))
                  result=SETPIXEL((density+1)+pp*px,300.0+pp*py)
                  result=setactiveqq(13)
                  end if
		 else	                          !if 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5
			   		                      !���l����t���M�~��, �A�Ϯg
		           flag_surface=4              !������l������
				   a=del_X*del_x+del_y*del_y		  !�ѥ��l�P�Ϯg�M���J�I�Ѽ�t
			       half_b=(px*del_x+py*del_y-del_z*(2*focal_length_1))	  !
			       c=px*px+py*py-4*(focal_length_1)*(pz-(focus_spot-focal_length_1))			      !
				   D_f1=half_b*half_b-a*c
				   if(D_f1<0)then	   
!4444444444444444444444444444444444444444444444444444444444444444
			         t=(dscreen-pz)/del_z           
!(D_f1<0)���N��O���l�S������Ϯg�M��{��������
			         PX=px+del_x*t	             !�Y�O �h���l�����]��ZCHECK2
			         py=py+del_y*t
			         pz=pz+del_z*t
			         counter_out_1=counter_out_1+1
				     flag=2                                   ! flag=2�O�����l�q�t���M�Ǭ�L�S���P�t���M�۹J
				       flag_surface=4
					   !counter_error=counter_error+1
				   else 	  !4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5.5 4.5 4.5 4.5 4.5
				       t=(-half_b-sqrt(D_f1))/a
					   !write(5,*)'t=',t						  !
				       if (t>0) then		 !5555555555555555555�����, ���l�P�Ϯg�M��{�����������ۼ�
					   !write(5,*)'t>0'
			           tem_X=px+del_x*t	             !�p����l�P�Ϯg�����J�I
			           tem_y=py+del_y*t				 !
			           tem_z=pz+del_z*t				 !
					   if (tem_z<z_check2)  then   
!666666666666666666666666666666666666666666666666666666666
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
					   px=tem_x
					   py=tem_y
					   pz=tem_z
					   pr=px*px+py*py					      !�p��J�I���k�V�q
					   length_vector=sqrt(4*pr+16*focal_length_1*focal_length_1)				       
!
					   nx=2*px/length_vector						  !
					   ny=2*pY/length_vector						  !
					   nz=-4*focal_length_1/length_vector
					   !write(5,*)'length_n=',nx*nx+ny*ny+nz*nz
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
					   del_dot_N=2*(del_x*nx+del_y*ny+del_z*nz)  !�p�⤺�n(�k�V�q�P���l)
					   !write(5,*)del_dot_N
					   del_x=del_x-del_dot_N*nx				     !�p��s�����l�B�ʤ�V x
					   del_y=del_y-del_dot_N*ny				     !					   y
					   del_z=del_z-del_dot_N*nz				     !					   z
					   !write(5,*)'length_del=',del_x*del_x+del_y*del_y+del_z*del_z

					   if (G_plot==1) then
                        	result=setactiveqq(13)	 ! ��v�ȹ�
                            result=setcolorrgb(rgbtointeger(255,0,0))
	                        result=SETPIXEL((density+1)+pp*pZ,300.0+pp*py)
                            result=setactiveqq(12)
	                        result=setcolorrgb(rgbtointeger(255,0,0))
                            result=SETPIXEL((density+1)+pp*px,300.0+pp*py)
                            result=setactiveqq(13)
	                    end if
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
					   a=del_X*del_x+del_y*del_y					  !�ѥ��l�P�ߪ��Ϯg�����J�I�Ѽ�t
					   half_b=(px*del_x+py*del_y-del_z*(100))
					   c=px*px+py*py-Zr*pz
					   d_f1=half_b*half_b-a*c
					   if (d_f1<0) then
					   write(5,*)'D_f3<0'
					   end if
					   t=(-half_b+sqrt(d_f1))/a
					   !t1=(half_b-sqrt(d_f1))/a
			           tem_x=px+del_x*t	             !�p����l�P�ߪ��Ϯg�����J�I
			           tem_y=py+del_y*t				 !
			           tem_z=pz+del_z*t				 !
					   if (tem_z>= dscreen)then		 
!777777777777777777777777777�ˬd�O�_���l�|������zscreen
        		          t=(dscreen-pz)/del_z	      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
						  px=px+del_x*t		      !
			              py=py+del_y*t		      !  �Y�O, �h �ѥ��l�Pscreen���y��
			              pz=pz+del_z*t		      !
						  flag=3
					   else					 !7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5
			              PX=tem_x	             !�p����l�P�ߪ��Ϯg�����J�I
			              py=tem_y				 !
			              pz=tem_z				 !
						  if (G_plot==1) then
						      result=setcolorrgb(rgbtointeger(255,255,0))
  					          result=SETPIXEL((density+1)+pp*pz,300.0+pp*py)  !ø�� ���l�bY-Z�����y��
						      result=setactiveqq(12)
                              result=SETPIXEL(shift_x_on_xy_sreen+c1*px,shift_x_on_xy_sreen+c1*py)
                              result=setactiveqq(13)
                          end if
					      if (pz > led_shift_z) then	!88888888888888888888888888888888888888888888888888888888888888888
					        pr=px*px+py*py
					        length_vector=sqrt(4*pr+zr*zr)
					        nx=-2*px/length_vector
					        ny=-2*py/length_vector
					        nz=zr/length_vector		 !�ѩߪ����Ϯg�誺�k�V�q
					        del_dot_N=2*(del_x*nx+del_y*ny+del_z*nz)
					        del_x=del_x-del_dot_N*nx				     !�p��s�����l�B�ʤ�V x
					        del_y=del_y-del_dot_N*ny				     !					   y
					        del_z=del_z-del_dot_N*nz				     !					   z
        		            t=(dscreen-pz)/del_z	      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
						    px=px+del_x*t		      !�ˬd�O�_���l�|������z
			                py=py+del_y*t		      !
			                pz=pz+del_z*t		      !
                            flag=4
							counter_R_out=counter_R_out+1
					        positive=positive+1
					      else !8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 ���l�Q�l�� (pz<0)
					        flag_surface=4
					        negative=negative+1
					      end if  !8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8..
					   end if   !7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7..
                    else   !6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5
			         t=(dscreen-pz)/del_z           
!(D_f1<0)���N��O���l�S������Ϯg�M��{��������
			         PX=px+del_x*t	             !�Y�O �h���l�����]��ZCHECK2
			         py=py+del_y*t
			         pz=pz+del_z*t
					 !result=SETPIXEL(600.0+scale*px,300.0+scale*py)  !ø�� ���l�bY-Z�����y��
			         counter_OUT_1=counter_OUT_1+1
				     flag=2                     ! flag=2�O�����l�q�t���M�Ǭ�L�S���P�t���M�۹J
				       flag_surface=4
	                end if !6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6..
					end if !5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5..
					end if !4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4..
		 end if !~~~~~~~~~~for R_check1	2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2..
   !end if      
!~~nsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsns--end
        !            result=SETPIXEL(SCALE*pz,YZ_SF+SCALE*py)  !ø�� ���l�bY-Z�����y��
		!            result=SETPIXEL(SCALE*pz,XZ_SF+SCALE*pX)  !ø�� ���l�bX-Z�����y��
   if(pz>=dscreen) then	! DSCREEN�N�O���l�]��F�Ϯg�M�f�F, �l�ܧ���111111111111111111111111111111111111111111111
	flag_surface=3		! �w�q �X�Ь�3
	xx=int(px*c1+shift_x_on_xy_sreen)							   !�p����l�bxy_screen����m
	yy=int(py*c1+shift_x_on_xy_sreen)							   !�p����l�bxy_screen����m
	total_screen(xx,yy)=total_screen(xx,yy)+ENERGY
	!if(flag==1)then
	!xy_screen_for_middle_hole(xx,yy)=xy_screen_for_middle_hole(xx,yy)+1
	!else if(flag==2) then
    !xy_screen_for_r1(xx,yy)=xy_screen_for_r1(xx,yy)+1
	!else if(flag==3)	then
	!xy_screen(xx,yy)=xy_screen(xx,yy)+1			   !�p����l�bxy_screen���ֿn�q
     if(flag==4)	then    
!22222222222222222222222222222222222222222222222222222222222222222222222222222222222
	xy_screen_for_r2(xx,yy)=xy_screen_for_r2(xx,yy)+ENERGY
	end if                  !2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2..
	!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	var_theta=acos(del_z)/rad		!�p����l�bxy_screen������
	              !write(10,'(a7,f6.3)')'final=',var_theta
	flag_theta=int(var_theta/RESOLUTION_ANGLE_PHOTON)+1
	theta_distribution(5,flag_theta)=theta_distribution(5,flag_theta)+1
	!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !write(5,*)xx,yy, xy_screen(xx,yy)
   end if              !1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1.. 1..
   pre_px=PX
   pre_py=PY
   pre_pz=PZ
 end do !do 
  !while(flag_surface==2)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!���U�@�����l!!!!!!
 end do !do v=1,num_v
 end do !do h=1,num_h
!end if !((LEX_X==(1.OR. 8)).and.(LED_Y==(1.OR.8))
end do !do led_x=1,8
end do !do led_Y=1,8
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~���G��~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
max_light=0										       !
      !write(5,*)'max_light=0'					       !
do V=1,density									       !
   do H=1,density								       ! �����j��ΥH�p��   ���G��  �W���̰��G��
       if(total_screen(H,V)>max_light)then			   !
          max_light=total_screen(H,V)				   !  �ΥH�k�@�� ���j��
	      ! write(5,*)	xy_screen(H,V)			       !
   end if										       !
   end do										       !
end do											       !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write(5,*)'max_light(���G��)=',max_light			   !
if (max_light==0) then								   !
max_light=0
else
max_light=255/max_light
end if							                       !		   �ӤW�Ӱj��
result=setactiveqq(13)	 ! ���G��			           !
do V=1,density									   !  �����j��ΥHø�� ���G�� ���G�פ���
   do H=1,density								   !
          LIGHT=total_screen(H,V)*max_light		   !
		  total_screen(H,V)=light
	      color=rgbtointeger(LIGHT,LIGHT,LIGHT)	   !
          result=setcolorrgb(color)				   !
	      result=SETPIXEL(H,V)					   !
		  !write(10,*) light !,advance='no') light
		  !100 format('&',i5)
   end do
end do
	color=rgbtointeger(0,0,255)
	result=setcolorrgb(color)
    h=density/2-R_check1*c1
	v=density/2+R_check1*c1
	result=ellipse($GBORDER,h,h,v,v)
    h=density/2-R_check2*c1
	v=density/2+R_check2*c1
    result=ellipse($GBORDER,h,h,v,v)
    h=density/2-R_check3*c1
	v=density/2+R_check3*c1
	RESULT=ellipse($GBORDER,h,h,v,v)
xxx="c:\12345.BMP"
result_int=saveimage(xxx,0,0,density,density)
write(5,*) 'result=',result_int
!do V=1,density
!do H=1,density
!write(10,'(i5)',advance='no') total_screen(H,V)
!end do
!end do
!close(10)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~���u�z�L��ӤϮg�Ҫ����u����~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
max_light=0										       !
      !write(5,*)'max_light=0'					       !
do V=1,density									       !
   do H=1,density								       ! �����j��ΥH�p��   ���G��  �W���̰��G��
       if(xy_screen_for_r2(H,V)>max_light)then			   !
          max_light=xy_screen_for_r2(H,V)				   !  �ΥH�k�@�� ���j��
	      ! write(5,*)	xy_screen(H,V)			       !
   end if										       !
   end do										       !
end do											       !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write(5,*)'max_light(���G��)=',max_light			   !
if (max_light==0) then								   !
max_light=0
else
max_light=255/max_light
end if							                       !		   �ӤW�Ӱj��
result=setactiveqq(13)	 ! ��v�ȹ�			           !
do V=1,density/2									   !  �����j��ΥHø�� ���G�� ���G�פ���
   do H=1,density								   !
          LIGHT=xy_screen_for_r2(H,V)*max_light		   !
		  xy_screen_for_r2(H,V)=light
	      color=rgbtointeger(LIGHT,LIGHT,LIGHT)	   !
          result=setcolorrgb(color)				   !
	      result=SETPIXEL(H,V)					   !
		  !write(10,*) light !,advance='no') light
		  !100 format('&',i5)
   end do
end do
	color=rgbtointeger(0,0,255)
	result=setcolorrgb(color)
    h=density/2-R_check1*c1
	v=density/2+R_check1*c1
	result=ellipse($GBORDER,h,h,v,v)
    h=density/2-R_check2*c1
	v=density/2+R_check2*c1
    result=ellipse($GBORDER,h,h,v,v)
    h=density/2-R_check3*c1
	v=density/2+R_check3*c1
	RESULT=ellipse($GBORDER,h,h,v,v)

do LED_x=1,4
do LED_Y=1,led_theta(led_x)
!   IF (.not.((led_X==1.and.(led_y==1.or.led_y==2.or.led_y==7.or.led_y==8)).OR.&
!            &(led_x==8.and.((led_y==1.or.led_y==2.or.led_y==7.or.led_y==8))).OR.&
!            &(LED_X==2.and.(led_y==1.or.led_y==8)).OR.(LED_X==7.and.(led_y==1.or.led_y==8))))THEN
      position_led_x=led_r(led_x)*cos(rad*360.0/led_theta(led_x)*(led_y-1))
      position_led_y=led_r(led_x)*sin(rad*360.0/led_theta(led_x)*(led_y-1))
        ! position_led_x=led_x*led_period-led_period*4.5
        ! position_led_y=led_Y*led_period-led_period*4.5
      position_led_z=led_shift_z
   	  color=rgbtointeger(255,0,0)
	  result=setcolorrgb(color)
      h=density/2+position_led_x*c1
	  v=density/2+position_led_y*c1
	  result=ellipse($GBORDER,h-2,v-2,h+2,v+2)
	!end if
end do
end do
xxx="c:\up_down.BMP"
result_int=saveimage(xxx,0,0,density,density)


!do V=1,density
!do H=1,density
!write(10,'(i5)',advance='no') total_screen(H,V)
!end do
!end do
!close(10)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
v=1
peak_angle=0											 ! �����⨤�פ��R
!result=setactiveqq(13)	 !�����⨤�פ��R
!COLOR=rgbtointeger(255,255,0)  !�ϥζ���
!result=setcolorrgb(color)
do while ( theta_distribution(1,v)>0 )
v=v+1
end do
write(5,*)'v=',v
do h=1,v
  write(10,'(a7,f5.2,f12.2,f12.2,f12.2)')'theta1=', (RESOLUTION_ANGLE_PHOTON)*(h-1),theta_distribution(1,h),theta_distribution(2,h),theta_distribution(5,h)
end do

!do h=1,v
! write(10,'(a7,f5.2,f10.2)')'theta5=', (RESOLUTION_ANGLE_PHOTON)*(h-1),theta_distribution(5,h)
!end do
write(5,*)'���ժ�����z=',counter_in_1
write(5,*)'�~��������z=',counter_out_1
!write(5,*)'�t���M�����Ϯg�@��=',counter_R_in
write(5,*)'�t���M�~���Ϯg�@��=',counter_R_out
!write(5,*)'�t���M�_���l��=',counter_ring
WRITE(5,*)'counter_ellip=',counter_ellip
write(5,*)'�`�@�����F=',int(total_photon*counter_led),'�����l'
write(5,'(a14,f6.3)')'���ժ�����z=',real(counter_in_1)/(total_photon*counter_led)
write(5,'(a14,f6.3)')'�~��������z=',real(counter_out_1)/(total_photon*counter_led)
!write(5,'(a14,f6.3)')'�����Ϯg�@��=',real(counter_R_in )/(total_photon*counter_led)
write(5,'(a14,f6.3)')'�~���Ϯg�@��=',real(counter_R_out)/(total_photon*counter_led)
!write(5,'(a14,f6.3)')'    �_���l��=',real(counter_ring)/(total_photon*counter_led)
!write(5,*)real(counter_in_1+counter_out_1+counter_r_in+counter_r_out+counter_ring)/(total_photon*counter_led)
write(5,*)'positive=',positive
write(5,*)'negative=',negative
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
deallocate(xy_screen_for_r2)
deallocate(total_screen)
deallocate(THETA_DISTRIBUTION)
deallocate(num_photon)
deallocate(flag_photon)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
WRITE(5,*)'TRY AGAIN? YES(1) '
READ(5,*)AGAIN
END DO !AGAIN
close(10)
end program type_4
