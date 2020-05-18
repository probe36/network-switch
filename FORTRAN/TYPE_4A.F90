!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     本程式是針對鉌鑫光電的交通號誌顯示器而專案編寫的     最後修改日期為91年8月12號
!              8/8, 發現光源在大角度上有問題
!              8/7, 完成初步亮度分布函數
!              8/6, 修正約5個小錯誤
!              8/5, 針對平行度問題
!              8/1, 已經完成了119種模擬
!              本程式已經將第一反射杯, 從圓錐面轉換成拋物面 並簡化了厚度問題(7/26)
!                    type_4 的名稱來自第4代設計 8/12
!                    type_3 的名稱來自第三代設計
!                                                             起始日期為91年7月1日
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
!下面這二行是計算LED光源的方位角與散射角的代數參數
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

!下面這一行是解方程式用內積參數
real(kind=8)::pre_P_dot_pre_p, c0_dot_co !,del_dot_del
real(kind=8)::pre_P_dot_del,   del_dot_n,  pre_P_dot_C0,  del_dot_C0
!下面這一行是解方程式用代數參數
real(kind=8)::a,half_b,c,t,t1,d1

!下面這一行是解光子與球面交點的法向量
real(kind=8)::length_vector,nx,ny,nz,tx,ty,tz,t1x,t1y,t1z
real(kind=8)::cx,cy,cz ,n_t,n_p

!下面這一行是兩個反射杯的演算法的參數
integer::counter_in_1, counter_out_1, counter_R_in, counter_R_out
integer::counter_total,counter_ellip, COUNTER_LED, counter_error
integer:: positive, negative
real(kind=8):: z_check1, z_check2, Z_check3
real(kind=8):: R_check1, R_check2, R_check3
real(kind=8):: R_check1_2, R_check2_2, R_check3_2
real(kind=8):: PR ,tem_x,tem_y,tem_z
real(kind=8):: focal_length_1,Focal_length_2,focus_spot, D_f1 ,zr
real(kind=8):: del,r_min,r_max

!下面這一行是xy_SCREEN的捕捉陣列
real(kind=8),allocatable:: xy_screen_for_r2(:,:)
real(kind=8),allocatable::total_screen(:,:)
integer:: density,light	,xx,yy ,shift_x_on_xy_sreen,shift_y_on_xy_sreen
real(kind=8):: c1,max_light

!下面這一行是xy_SCREEN的角度分析
integer::  num_del_theta, flag_theta
real(kind=8):: var_theta, RESOLUTION_ANGLE_PHOTON,peak_angle
real(kind=8),allocatable:: theta_distribution(:,:)

!以下為  資料輸入  視窗的定義
open(unit=5,file='user',iofocus=.true.)
wc.numxpixels=300
wc.numypixels=1000
wc.numtextcols=-1
wc.numtextrows=-1
wc.numcolors=16
wc.title="輸入用視窗"
wc.fontsize=-1
result=setwindowconfig(wc)
result=displaycursor($gcursoron)
       !資料輸入視窗完成
  open(unit=13,file='user',iofocus=.true.)
  wc.numxpixels=1200
  wc.numypixels=1200
  wc.numtextcols=-1
  wc.numtextrows=200
  wc.numcolors=-1
  wc.title="全亮光"
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
counter_total=0        !計算共有幾顆光子
counter_in_1=0         !計算共有幾顆光子通過反射杯的中洞
counter_out_1=0
counter_R_in=0
!counter_ring=0
counter_R_out=0
counter_ellip=0         !計算共有幾顆光子經過第一反射杯後直達燈罩
counter_led=0           !計算共用了幾顆led
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

! 檢測是否要更改系統或只是改變鏡片曲率~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  result=setactiveqq(5)
  color=rgbtointeger(255,255,255)
  result=settextcolorrgb(color)
  write(5,*)'CHANGE SYSTEM???,YES(1),NO(0)'
  read(5,*)  CHANGE_SYSTEM
  IF (CHANGE_SYSTEM==1)THEN
    scale=1
    write(5,*)'請輸入screen繪圖比例,screen_SCALE'
    read(5,*)  screen_SCALE
    pp=screen_SCALE
!    write(5,*)'請輸入銀幕距離,dscreen'
	!dscreen=140  !              92+led_shift_z
!    write(5,*)'請輸入LENS距離,ds1'
    ds1=1.8+led_shift_z
!    write(5,*)'請輸入鏡心厚度,thick1'
	thick1=2.69
    write(5,*)'請輸入反射杯的焦距'
    READ(5,*) focal_length_1                 !反射杯的焦距
    write(5,*)'請輸入反射杯的內孔大小'
    READ(5,*) R_check1                      !第一檢查半徑
    write(5,*)'請輸入反射杯的最大半徑'
    READ(5,*) R_check2                      !第二檢查半徑
focus_spot=50
focal_length_2=50.0	     						!反射碗的焦距
R_check3=140
z_check1=focus_spot-Focal_length_1+R_check1*R_check1/(4.0*focal_length_1)  
!第一檢查位置
z_check2=focus_spot-Focal_length_1+R_check2*R_check2/(4.0*focal_length_1)  
!第二檢查位置
Z_check3=focus_spot-Focal_length_2+R_check3*R_check3/(4.0*focal_length_2)  
!第三檢查位置
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
  
!更改系統完成~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! write(5,*)'請輸入S2的半徑,R2(mm)'  r2=-20
  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~   設定捕捉螢幕的參數   ~~~~~~~~~~~~~~~~~~~~~~~~~
  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !write(5,*)'請輸入XYscreen的密度,density(100)' 
!設定方陣細胞為(DENSITY*DENSITY)
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
  !~~~~~~~~~~~~~~~~~~~~   設定捕捉螢幕的光子的角度分析   ~~~~~~~~~~~~~~~~~~~~~~~~~
  !write(5,*)'請輸入光子的角度分析的解析度,RESOLUTION_ANGLE_PHOTON (0.5)'
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
cs2=ds2+r2	 !用以定義surface 2 的球心座標
r_min=-R_check2
r_max=-r_min
del=(r_max-r_min)/300.0
py=r_min

result=setactiveqq(13)	 ! 投影銀幕
result=setcolorrgb(rgbtointeger(0,0,255))
call ClearScreen($GCLEARSCREEN)
result=setactiveqq(12)
call ClearScreen($GCLEARSCREEN)
result=setactiveqq(13)!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do h=1,300
    py=py+del
	if (abs(py)>=r_check1) then
	   pz=py*py/(2*2*focal_length_1)+(focus_spot-focal_length_1)
	   result=SETPIXEL((density+1)+pp*pZ,300.0+pp*py)  !繪圖 光子在Y-Z面的軌跡
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
	result=SETPIXEL((density+1)+pp*pZ,300.0+pp*py)  !繪圖 光子在Y-Z面的軌跡
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
!!定義LED的散射角
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   write(5,*)'請輸入LED的張角,THETA(角度)'
   read(5,*)  THETA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write(5,*)'請輸入光子密度(del_arc)'
read(5,*)del_arc                               !定義光子密度
write(5,*)'if plot test?(y/n--1/0)'
read(5,*)G_plot
length_arc=theta*rad						   !總弧長=theta乘以半徑1
num_h=length_arc/del_arc					   !總弧長在除以DEL_ARC就等於有幾圈
num_circle=num_h/2+1                           !計算產生幾個光錐(num_circle)
allocate(num_photon(num_circle))			   
!定義num_photon陣列的大小為(num_circle)
allocate(flag_photon(num_circle))
  WRITE(5,*)'光錐共有',num_circle,'層'
total_photon=0
do h=1, num_circle							   !
	 num_photon(h)=2*3.1415926*sin(real(h*del_arc))/del_arc	   
!計算每一層光錐內會有幾顆光子
	 !write(10,'(a12,i4,a2,i8)')'num_photon(',h,')=',num_photon(h)
	 total_photon=total_photon+num_photon(h)
end do
      WRITE(5,'(a12,i11,a10)')'每顆led共有',total_photon,'顆模擬光子'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!    開始追蹤光子
!!!!!	 光軸為z方向
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
   write(5,'(A4,I3)')'r=',led_x	! 寫入資料輸入視窗
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
!繪圖 光子在Y-Z面的軌跡
do h=1, num_circle !int(0.4*num_circle),int(0.8*num_circle) ! 先依次掃瞄光錐
        angle_theta=del_arc*(h)                            ! 計算光錐的錐角
        del_whay=360.0/real(num_photon(h))         ! 定del_whay為任一光錐上, 兩相鄰光子的角度差
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
  do v=1,num_photon(h)	                      ! 再掃瞄每一個光錐的每一個光子(轉一圈 共360度)
	 angle_whay =rad*(del_whay *(v-1))	      ! 計算光子, 在光錐上的whay角度
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
   pre_px=position_led_x	   ! px py pz為光子的前一個位置
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
	 !下面五行為計算各光子行進方向
   t=(ds1-pz)/del_z
   px=pre_px+del_x*t
   py=pre_py+del_y*t
   pz=pre_pz+del_z*t
   !if (pz>=ds1)then	!first fast check for surface1!!!!! 
!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!
	  !  FLAG_SURFACE=1                                  !!  光子進入平面    !!
                                                        
!!!!!!!!!!!!!!!!!!!!!!
        if (G_plot==1) then
           result=setactiveQQ(11)
           result=setcolorrgb(rgbtointeger(255,100,0))
           result=SETPIXEL((density)+60*px,(density)+60*py)
        end if
	   if (del_z<0.999999) then	!當del_z等於(1.0)時,表示垂直於曲面入射, 外積計算時,會產生分母為0, 所以跳開
				    nx=0
				    ny=0
					nz=1
				 t1=(ds1-pre_pz)/del_z
  				 px=pre_px+del_x*t1	!計算平面與光子交點的位置
				 py=pre_py+del_y*t1
				 pz=pre_pz+del_z*t1
				 del_dot_N=del_z*nz  !計算內積(法向量與光子)
				 n_p=n01*sqrt(1-del_dot_N*del_dot_N)
				 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				 Tx=del_y       !第一次外積
				 Ty=-del_x
				 Tz=0
				 T1x=-Ty		!第二次外積
				 T1y=Tx
				 T1z=0      !Ty*nx-Tx*ny
				 length_vector=sqrt(t1x*t1x+t1y*t1y)  !計算法向量的垂直方向長
				 t1x=t1x/length_vector					   !轉換成為單位向量
				 t1y=t1y/length_vector
				 t1z=0      !t1z/length_vector
				 n_t=sqrt(1-n_p*n_p)
				 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
				 t1x=t1x*n_p	!完成計算新的光子行進方向（法向量的垂直方向）
				 t1y=t1y*n_p
				 !t1z=t1z*n_p
				 !nx=nx*n_t		!完成計算新的光子行進方向（法向量方向）
				 !ny=ny*n_t
				 nz=nz*n_t
				 del_x=t1x	!完成計算新的光子行進方向
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
				 !if (d1<0)then		!萬分之五的逃脫率!2222222222222222222222222222
                 !   write(5,*)'d1<0'
				 !   d1=0
				 !end if  !2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2..
				 !IF(R2>0)THEN  !222222222222222222222222222222222222222222222222
				 !   t1=-half_b-sqrt(d1) !solve the t
				 !   nx=cx-px			!計算球面與光子交點的法向量
				 !   ny=cy-py			!法向量指向球心
				 !   nz=cz-pz
				 !ELSE          !2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5
				    t1=-half_b+sqrt(d1) !solve the t
				    nx=px-cx			!計算球面與光子交點的法向量
				    ny=py-cy			!法向量指向球外
				    nz=pz-cz
				 !END IF   !2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2..
			     !t=t2
  				 px=pre_px+del_x*t1	!計算球面與光子交點的位置
				 py=pre_py+del_y*t1
				 pz=pre_pz+del_z*t1
                 if(G_plot==1)then !plot plot plot plot plot plot plot plot plot plot plot
                    result=setactiveQQ(12)
                    result=setcolorrgb(rgbtointeger(0,255,255))
                    result=SETPIXEL((density/2)+80*px,density+80*py)
                 end if  !plot.. plot.. plot.. plot.. plot.. plot.. plot.. plot.. plot.. plot..
				 length_vector=sqrt(nx*nx+ny*ny+nz*nz)  !計算法向量長
				 nx=nx/length_vector					   !轉換成為單位向量
				 ny=ny/length_vector
				 nz=nz/length_vector
				 del_dot_N=del_x*nx+del_y*ny+del_z*nz  !計算內積(法向量與光子)
				 n_p=n12*sqrt(1-del_dot_N*del_dot_N)
				 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				 Tx=nz*del_y-ny*del_z
				 Ty=nx*del_z-nz*del_x
				 Tz=ny*del_x-nx*del_y
				 T1x=Tz*ny-Ty*nz
				 T1y=Tx*nz-Tz*nx
				 T1z=Ty*nx-Tx*ny
				 length_vector=sqrt(t1x*t1x+t1y*t1y+t1z*t1z)  !計算法向量的垂直方向長

				 !if (length_vector>0.000001) then !222222222222222222222222222222222222
				 t1x=t1x/length_vector					   !轉換成為單位向量
				 t1y=t1y/length_vector
				 t1z=t1z/length_vector
				 !end if	  !2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2..
				 n_t=sqrt(1-n_p*n_p)
				 t1x=t1x*n_p	!完成計算新的光子行進方向（法向量的垂直方向）
				 t1y=t1y*n_p
				 t1z=t1z*n_p

				 nx=nx*n_t		!完成計算新的光子行進方向（法向量方向）
				 ny=ny*n_t
				 nz=nz*n_t
				 del_x=nx+t1x	!完成計算新的光子行進方向
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
!!!!!    光子脫離陣列鏡片  進入空氣
!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do while(flag_surface==2)
   t=(z_check1-pz)/del_z
   px=pre_px+del_x*t
   py=pre_py+del_y*t
   pz=pre_pz+del_z*t

   !if(pz>=z_check1) then 
!~~nsnsnsnsnssns--begin	111111111111111111111111111111111111111111111111
		 pr=px*px+py*py		              !檢測光子位置的半徑
		 if(pr<R_check1_2)then            !檢測光子是否進入 聖火杯的中心空洞 2222222222222222222
			      t=(dscreen-pz)/del_z
			      PX=px+del_x*t	             !若是 則光子直接跑到ZCHECK2
			      py=py+del_y*t
			      pz=pz+del_z*t
			      counter_in_1=counter_in_1+1
				  flag=1                                   ! flag=1是指光子直接穿過中洞
				  if (G_plot==1) then !
			      result=setactiveqq(12)
				  result=setcolorrgb(rgbtointeger(0,255,0))
                  result=SETPIXEL((density+1)+pp*px,300.0+pp*py)
                  result=setactiveqq(13)
                  end if
		 else	                          !if 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5
			   		                      !光子撞到聖火杯外面, 再反射
		           flag_surface=4              !先把光子消滅掉
				   a=del_X*del_x+del_y*del_y		  !解光子與反射杯的焦點參數t
			       half_b=(px*del_x+py*del_y-del_z*(2*focal_length_1))	  !
			       c=px*px+py*py-4*(focal_length_1)*(pz-(focus_spot-focal_length_1))			      !
				   D_f1=half_b*half_b-a*c
				   if(D_f1<0)then	   
!4444444444444444444444444444444444444444444444444444444444444444
			         t=(dscreen-pz)/del_z           
!(D_f1<0)的意思是光子沒有撞到反射杯方程式的延伸
			         PX=px+del_x*t	             !若是 則光子直接跑到ZCHECK2
			         py=py+del_y*t
			         pz=pz+del_z*t
			         counter_out_1=counter_out_1+1
				     flag=2                                   ! flag=2是指光子從聖火杯旁穿過沒有與聖火杯相遇
				       flag_surface=4
					   !counter_error=counter_error+1
				   else 	  !4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5.5 4.5 4.5 4.5 4.5
				       t=(-half_b-sqrt(D_f1))/a
					   !write(5,*)'t=',t						  !
				       if (t>0) then		 !5555555555555555555有實根, 光子與反射杯方程式的延伸有相撞
					   !write(5,*)'t>0'
			           tem_X=px+del_x*t	             !計算光子與反射面的焦點
			           tem_y=py+del_y*t				 !
			           tem_z=pz+del_z*t				 !
					   if (tem_z<z_check2)  then   
!666666666666666666666666666666666666666666666666666666666
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
					   px=tem_x
					   py=tem_y
					   pz=tem_z
					   pr=px*px+py*py					      !計算焦點的法向量
					   length_vector=sqrt(4*pr+16*focal_length_1*focal_length_1)				       
!
					   nx=2*px/length_vector						  !
					   ny=2*pY/length_vector						  !
					   nz=-4*focal_length_1/length_vector
					   !write(5,*)'length_n=',nx*nx+ny*ny+nz*nz
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
					   del_dot_N=2*(del_x*nx+del_y*ny+del_z*nz)  !計算內積(法向量與光子)
					   !write(5,*)del_dot_N
					   del_x=del_x-del_dot_N*nx				     !計算新的光子運動方向 x
					   del_y=del_y-del_dot_N*ny				     !					   y
					   del_z=del_z-del_dot_N*nz				     !					   z
					   !write(5,*)'length_del=',del_x*del_x+del_y*del_y+del_z*del_z

					   if (G_plot==1) then
                        	result=setactiveqq(13)	 ! 投影銀幕
                            result=setcolorrgb(rgbtointeger(255,0,0))
	                        result=SETPIXEL((density+1)+pp*pZ,300.0+pp*py)
                            result=setactiveqq(12)
	                        result=setcolorrgb(rgbtointeger(255,0,0))
                            result=SETPIXEL((density+1)+pp*px,300.0+pp*py)
                            result=setactiveqq(13)
	                    end if
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
					   a=del_X*del_x+del_y*del_y					  !解光子與拋物反射面的焦點參數t
					   half_b=(px*del_x+py*del_y-del_z*(100))
					   c=px*px+py*py-Zr*pz
					   d_f1=half_b*half_b-a*c
					   if (d_f1<0) then
					   write(5,*)'D_f3<0'
					   end if
					   t=(-half_b+sqrt(d_f1))/a
					   !t1=(half_b-sqrt(d_f1))/a
			           tem_x=px+del_x*t	             !計算光子與拋物反射面的焦點
			           tem_y=py+del_y*t				 !
			           tem_z=pz+del_z*t				 !
					   if (tem_z>= dscreen)then		 
!777777777777777777777777777檢查是否光子會直接穿透screen
        		          t=(dscreen-pz)/del_z	      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
						  px=px+del_x*t		      !
			              py=py+del_y*t		      !  若是, 則 解光子與screen的座標
			              pz=pz+del_z*t		      !
						  flag=3
					   else					 !7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5 7.5
			              PX=tem_x	             !計算光子與拋物反射面的焦點
			              py=tem_y				 !
			              pz=tem_z				 !
						  if (G_plot==1) then
						      result=setcolorrgb(rgbtointeger(255,255,0))
  					          result=SETPIXEL((density+1)+pp*pz,300.0+pp*py)  !繪圖 光子在Y-Z面的軌跡
						      result=setactiveqq(12)
                              result=SETPIXEL(shift_x_on_xy_sreen+c1*px,shift_x_on_xy_sreen+c1*py)
                              result=setactiveqq(13)
                          end if
					      if (pz > led_shift_z) then	!88888888888888888888888888888888888888888888888888888888888888888
					        pr=px*px+py*py
					        length_vector=sqrt(4*pr+zr*zr)
					        nx=-2*px/length_vector
					        ny=-2*py/length_vector
					        nz=zr/length_vector		 !解拋物面反射鏡的法向量
					        del_dot_N=2*(del_x*nx+del_y*ny+del_z*nz)
					        del_x=del_x-del_dot_N*nx				     !計算新的光子運動方向 x
					        del_y=del_y-del_dot_N*ny				     !					   y
					        del_z=del_z-del_dot_N*nz				     !					   z
        		            t=(dscreen-pz)/del_z	      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
						    px=px+del_x*t		      !檢查是否光子會直接穿透
			                py=py+del_y*t		      !
			                pz=pz+del_z*t		      !
                            flag=4
							counter_R_out=counter_R_out+1
					        positive=positive+1
					      else !8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 8.5 光子被吸收 (pz<0)
					        flag_surface=4
					        negative=negative+1
					      end if  !8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8.. 8..
					   end if   !7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7.. 7..
                    else   !6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5 6.5
			         t=(dscreen-pz)/del_z           
!(D_f1<0)的意思是光子沒有撞到反射杯方程式的延伸
			         PX=px+del_x*t	             !若是 則光子直接跑到ZCHECK2
			         py=py+del_y*t
			         pz=pz+del_z*t
					 !result=SETPIXEL(600.0+scale*px,300.0+scale*py)  !繪圖 光子在Y-Z面的軌跡
			         counter_OUT_1=counter_OUT_1+1
				     flag=2                     ! flag=2是指光子從聖火杯旁穿過沒有與聖火杯相遇
				       flag_surface=4
	                end if !6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6.. 6..
					end if !5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5.. 5..
					end if !4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4.. 4..
		 end if !~~~~~~~~~~for R_check1	2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2..
   !end if      
!~~nsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsnsns--end
        !            result=SETPIXEL(SCALE*pz,YZ_SF+SCALE*py)  !繪圖 光子在Y-Z面的軌跡
		!            result=SETPIXEL(SCALE*pz,XZ_SF+SCALE*pX)  !繪圖 光子在X-Z面的軌跡
   if(pz>=dscreen) then	! DSCREEN就是光子跑到了反射杯口了, 追蹤完畢111111111111111111111111111111111111111111111
	flag_surface=3		! 定義 旗標為3
	xx=int(px*c1+shift_x_on_xy_sreen)							   !計算光子在xy_screen的位置
	yy=int(py*c1+shift_x_on_xy_sreen)							   !計算光子在xy_screen的位置
	total_screen(xx,yy)=total_screen(xx,yy)+ENERGY
	!if(flag==1)then
	!xy_screen_for_middle_hole(xx,yy)=xy_screen_for_middle_hole(xx,yy)+1
	!else if(flag==2) then
    !xy_screen_for_r1(xx,yy)=xy_screen_for_r1(xx,yy)+1
	!else if(flag==3)	then
	!xy_screen(xx,yy)=xy_screen(xx,yy)+1			   !計算光子在xy_screen的累積量
     if(flag==4)	then    
!22222222222222222222222222222222222222222222222222222222222222222222222222222222222
	xy_screen_for_r2(xx,yy)=xy_screen_for_r2(xx,yy)+ENERGY
	end if                  !2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2.. 2..
	!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	var_theta=acos(del_z)/rad		!計算光子在xy_screen的角度
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
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!換下一顆光子!!!!!!
 end do !do v=1,num_v
 end do !do h=1,num_h
!end if !((LEX_X==(1.OR. 8)).and.(LED_Y==(1.OR.8))
end do !do led_x=1,8
end do !do led_Y=1,8
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~全亮光~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
max_light=0										       !
      !write(5,*)'max_light=0'					       !
do V=1,density									       !
   do H=1,density								       ! 本雙迴圈用以計算   全亮光  上的最高亮度
       if(total_screen(H,V)>max_light)then			   !
          max_light=total_screen(H,V)				   !  用以歸一化 光強度
	      ! write(5,*)	xy_screen(H,V)			       !
   end if										       !
   end do										       !
end do											       !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write(5,*)'max_light(全亮光)=',max_light			   !
if (max_light==0) then								   !
max_light=0
else
max_light=255/max_light
end if							                       !		   承上個迴圈
result=setactiveqq(13)	 ! 全亮光			           !
do V=1,density									   !  本雙迴圈用以繪圖 全亮光 的亮度分布
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~光線透過兩個反射境的光線分布~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
max_light=0										       !
      !write(5,*)'max_light=0'					       !
do V=1,density									       !
   do H=1,density								       ! 本雙迴圈用以計算   全亮光  上的最高亮度
       if(xy_screen_for_r2(H,V)>max_light)then			   !
          max_light=xy_screen_for_r2(H,V)				   !  用以歸一化 光強度
	      ! write(5,*)	xy_screen(H,V)			       !
   end if										       !
   end do										       !
end do											       !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write(5,*)'max_light(全亮光)=',max_light			   !
if (max_light==0) then								   !
max_light=0
else
max_light=255/max_light
end if							                       !		   承上個迴圈
result=setactiveqq(13)	 ! 投影銀幕			           !
do V=1,density/2									   !  本雙迴圈用以繪圖 全亮光 的亮度分布
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
peak_angle=0											 ! 光散色角度分析
!result=setactiveqq(13)	 !光散色角度分析
!COLOR=rgbtointeger(255,255,0)  !使用黃色
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
write(5,*)'內孔直接穿透=',counter_in_1
write(5,*)'外環直接穿透=',counter_out_1
!write(5,*)'聖火杯內面反射一次=',counter_R_in
write(5,*)'聖火杯外面反射一次=',counter_R_out
!write(5,*)'聖火杯斷面吸收=',counter_ring
WRITE(5,*)'counter_ellip=',counter_ellip
write(5,*)'總共模擬了=',int(total_photon*counter_led),'顆光子'
write(5,'(a14,f6.3)')'內孔直接穿透=',real(counter_in_1)/(total_photon*counter_led)
write(5,'(a14,f6.3)')'外環直接穿透=',real(counter_out_1)/(total_photon*counter_led)
!write(5,'(a14,f6.3)')'內面反射一次=',real(counter_R_in )/(total_photon*counter_led)
write(5,'(a14,f6.3)')'外面反射一次=',real(counter_R_out)/(total_photon*counter_led)
!write(5,'(a14,f6.3)')'    斷面吸收=',real(counter_ring)/(total_photon*counter_led)
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

