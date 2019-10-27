	;;Title: Ultimate Tag: Deathmatch ’83, Beyond Thunderdome
	;;Author: Christopher Steward

	processor 6502
	include "vcs.h"

	;; ===============================================================================
	;; Constant values
	;; ===============================================================================
	
	SEG.U constants

const_player_start_y		= 89 		;starting y position
const_player0_start_x		= 109		;starting x position for player 0
const_player1_start_x		= 85		;starting x position for player 1

const_player_start_speed	= 1 		;players starting speed
const_player_speed_max		= 2 		;players max speed
const_player_speed_hold		= 24		;how many frames to hold the random speed
const_player0_speed_frames	= 12		;where
const_player1_speed_frames	= 0		;
	
const_player_max_y		= 18 		;maximum y position in pixels
const_player_min_y		= 180 		;minimum y position in pixels
const_player_max_x		= 152 		;maximum x position in pixles
const_player_min_x		= 8		;minimum x position in pixels
	
const_sprite_frames		= 10 		;number of frames in the player animation cycle
const_sprite_frame_hold		= 5		;how many tv frames to hold each player animation frame for
const_sprite_size		= 8		;size of the pixels in terms of scanlines

const_playfield_colour		= $12 		;playfield colour
const_background_colour		= $C6 		;background colour
	
const_joystick0_up 		= %00000001 	;
const_joystick0_down		= %00000010	;
const_joystick0_left		= %00000100	;
const_joystick0_right 		= %00001000	;
const_joystick1_up		= %00010000	;
const_joystick1_down		= %00100000	;
const_joystick1_left		= %01000000	;
const_joystick1_right		= %10000000	;bit values needed to compare for joystick movement

const_PRNG_seed			= $4F3A		;just a non-zero seed for the PRNG

const_music_hold_frames		= 10		;hold each peice of music data for 4 frames
	
	;; ===============================================================================
	;; Zero-page variables
	;; ===============================================================================
	
	SEG.U variables
	ORG $0080
	
var_player0_y			ds 1 	;y position of player0
var_player0_x			ds 1	;x position of player0
var_player0_speed		ds 1	;speed of player0
var_player0_scan_left		ds 1	;number of scanlines left
var_player0_gfx_ptr		ds 2	;pointer to graphics in ROM
var_player0_palette_ptr		ds 2	;pointer to palette in ROM
var_player0_frame_count		ds 1	;counter for frames
var_player0_frames_left		ds 1	;tv frames remaining for current sprite frame to switch
var_player0_speed_count		ds 1	;count for when to reset
var_player0_moving		ds 1	;boolean for if player0 is moving
	
var_player1_y			ds 1 	;y position of player1
var_player1_x			ds 1	;x position of player1
var_player1_speed		ds 1	;speed of player1
var_player1_scan_left		ds 1	;number of scanlines left
var_player1_gfx_ptr		ds 2	;pointer to graphics in ROM
var_player1_palette_ptr		ds 2	;pointer to palette in ROM
var_player1_frame_count		ds 1	;counter for frames
var_player1_frames_left		ds 1	;tv frames remaining for current sprite frame to switch
var_player1_speed_count		ds 1	;
var_player1_moving		ds 1	;boolean for if player1 is moving
	
var_rand			ds 2 	;random number generator state
var_touching			ds 1	;keeps track of if on the last frame the players were colliding

var_music_counter		ds 1
var_music_ptr			ds 2
var_music_hold			ds 1
	
	;; ===============================================================================
	;; Rom Code
	;; ===============================================================================
	
 	SEG ROM 
        ORG $F000
fn_start:
	cld				;this little flag caused me undue pain and suffering
	;; 
	;; clear zero-page ram
	;; 
	lda	#0			;set value to #0
	ldx	#$FF			;point x at $FF
	txs				;set the stack pointer to $FF
loop_clear_ram:				;will clear ram from $FF to $00
 	sta 	0,x 			;store #0 at $x
        dex 				;pointer--
        bne 	loop_clear_ram		;loop if x != 0
	lda	#0			;
	ldx	#0			;
	ldy	#0			;set all registers to #0

	;; 
	;; Player0 inits
	;; 
	lda	#const_player0_start_x 		;
	sta	var_player0_x			;set the player0 starting x position
	lda	#const_player_start_y		;
	sta	var_player0_y			;set the player0 starying y position

	lda	#const_player_start_speed 	;
	sta	var_player0_speed		;set the player0 starting speed
	lda	#const_player0_speed_frames	;
	sta	var_player0_speed_count		;set the player0 amount of frames to hold current speed
	
	lda	#const_sprite_frame_hold 	;
	sta	var_player0_frames_left	 	;set the player0 amount of frames to hold the sprite frame
	
	lda	#<gfx_player_sprite_base_addr 	;
	sta	var_player0_gfx_ptr		;
	lda	#>gfx_player_sprite_base_addr	;
	sta	var_player0_gfx_ptr+1		;load the high and low bytes for the  player0 graphics pointer
	
	lda	#<gfx_player0_palette_addr 	;
	sta	var_player0_palette_ptr		;
	lda	#>gfx_player0_palette_addr	;
	sta	var_player0_palette_ptr+1	;load the high and low bytes for the player0 palette pointer

	lda	#const_sprite_frames 		;
	sta	var_player0_frame_count		;set the number of frames that the chacter animation has

	;; 
	;; Player1 inits
	;; 
	lda	#const_player1_start_x 		;
	sta	var_player1_x			;set the player0 starting x position
	lda	#const_player_start_y		;
	sta	var_player1_y			;set the player0 starying y position

	lda	#const_player_start_speed 	;
	sta	var_player1_speed	  	;set the player0 starting speed
	lda	#const_player0_speed_frames	;
	sta	var_player0_speed_count		;set the player0 amount of frames to hold current speed

	lda	#const_sprite_frame_hold 	;
	sta	var_player1_frames_left	 	;set the player0 amount of frames to hold the sprite frame
	
	lda	#<gfx_player_sprite_base_addr 	;
	sta	var_player1_gfx_ptr		;
	lda	#>gfx_player_sprite_base_addr	;
	sta	var_player1_gfx_ptr+1		;load the high and low bytes for the  player0 graphics pointer
	
	lda	#<gfx_player1_palette_addr 	;
	sta	var_player1_palette_ptr		;
	lda	#>gfx_player1_palette_addr	;
	sta	var_player1_palette_ptr+1	;load the high and low bytes for the player0 palette pointer

	lda	#const_sprite_frames 		;
	sta	var_player1_frame_count		;set the number of frames that the chacter animation has
	

	;;
	;;set the TIA state
	;; 
	lda	#0				;value to zero out registers
	sta	ENAM0				;
	sta	ENAM1				;disable missiles
	sta	ENABL				;disable ball
	sta	CXCLR				;reset collision latch
	lda	#1				;load #2 (D0=1, D1=0, D2=0, D4=0, D5=0)
	sta	CTRLPF				;reflect playfield	
	lda	#0				;
	sta	REFP0				;disable reflection for player graphics
	sta	REFP1				;
	sta	GRP0				;
	sta	GRP1				;clear player graphics
	sta	COLUP0				;
	sta	COLUP1				;clear player colours
	lda	#const_playfield_colour 	;
	sta	COLUPF				;set the playfield colour
	lda	#const_background_colour	;
	sta	COLUBK				;set the background colour


	
	lda	 #2			;load D2=1
	sta	 VBLANK			;start vblanking

	;;
	;;seed our PRNG
	;;
	lda	#<const_PRNG_seed 	;load low byte of the PRNG seed
	sta	var_rand		;store it on zero-page
	lda	#>const_PRNG_seed	;load high byte of the PRNG seed
	sta	var_rand+1		;store it on the zero-page

	;;
	;;set up music ptr
	;; 
	lda	#<music_theme_base_addr 	;load the low byte of the music base address
	sta	var_music_ptr			;set the low byte of the music pointer to it
	lda	#>music_theme_base_addr 	;load the high byte of the music base address
	sta	var_music_ptr+1			;set the high byte of the music pointer to it
	lda	#0				;load 0
	sta	var_music_counter		;set the counter to 0
	lda	#const_music_hold_frames 	;load the number of frames to hold each note 
	sta	var_music_hold		 	;set music hold to that
	
fn_vsync:
	sta	WSYNC		;sync with the last overscan line
	lda	#2		;load D2=1
	sta	VSYNC		;start vsync
	sta	WSYNC		;
	sta	WSYNC		;
	sta	WSYNC		;3 vsync lines
	lda	#0		;load D2=0
	sta	VSYNC		;stop vsync


fn_vblank:
	;; vblank time, 37 scanlines before drawing
	;;clock at 1.19 MHz, NTSC frame rate at 59.94Hz, 262 scanlines per frame
	;; 1.19Mhz/262/59.94 = 75.775 ~= 76
	lda	#43			;64*44/76 = 37.05
	sta	TIM64T          	;set timer to wait 37.05 scanlines

	;;
	;;music
	;;
	ldy	#music_theme_end_addr-music_theme_base_addr	;end of music low byte in ROM
	cpy	var_music_counter	;load up the counter
	bne	play_music		;branch to play music greater than 0
reset_music:
	lda	#<music_theme_base_addr ;else load up the low byte of the base address for the music in ROM
	sta	var_music_ptr	  	;store it at the low byte for the music pointer
	lda	#0		  	;
	sta	var_music_counter 	;reset the counter
play_music:
	dec	var_music_hold		;decrement the counter for holding each note
	bne	done_music		;if it isn't zero we will hold the current music
	lda	const_music_hold_frames ;else load the hold counter
	sta	var_music_hold		;reset it to the number of frames to hold each note
	
	;;channel 1
	lda	#%00001000		;have the volume be around the middle
	sta	AUDV0			;load that into channel 1 volume register
	ldy	var_music_counter	;load the music counter
	lda	(var_music_ptr),y	;use indexed indirect addressing to get the frequency from
					;ROM data with the pointer and counter 
	sta	AUDF0			;load that into the frequency
	iny				;counter++
	lda	(var_music_ptr),y	;use indexed indirect addressing to get the control from
					;ROM data with the pointer and counter 
	sta	AUDC0			;load that into the control
	iny				;counter++
	;; channel 2
	lda	#%00001000		;have the volume be around the middle
	sta	AUDV1			;load that into channel 2 volume register
	lda	(var_music_ptr),y	;use indexed indirect addressing to get the frequency from
					;ROM data with the pointer and counter 
	sta	AUDF1			;load that into the frequency
	iny				;counter++
	lda	(var_music_ptr),y	;use indexed indirect addressing to get the control from
					;ROM data with the pointer and counter 
	sta	AUDC1			;load that into the control
	iny				;counter++
	sty	var_music_counter	;set the counter to the updated value
done_music:	

	;; 
	;; reset scanline counters for players
	;; 
	lda	#const_sprite_size 	;get the size of the sprite
	sta	var_player0_scan_left	;reset the amount of scanlines left to the sprite size
	lda	#const_sprite_size 	;get the size of the sprite
	sta	var_player1_scan_left	;reset the amount of scanlines left to the sprite size

	;;
	;;set player speed
	;; 
	dec	var_player0_speed_count		;decrement the hold speed counter
	bne	skip_player0_speed		;if counter>0 don't change anything
	lda	#const_player_speed_hold	;else load the hold number of frames to hold speed
	sta	var_player0_speed_count		;reset the counter to that
	adc	var_player1_y			;add a little bit of entropy to the system
	jsr 	generate_PRNG			;call prng function 
	and	#const_player_speed_max		;convienent power of 2 modulus
	and	#1				;get either 0 or 1
	adc 	#1				;add 1 to make the range 1 or 2
	sta	var_player0_speed		;set p0 random speed	
skip_player0_speed:	

	dec	var_player1_speed_count		;decrement the hold speed counter
	bne	skip_player1_speed		;if counter>0 don't change anything
	lda	#const_player_speed_hold	;else load the hold number of frames to hold speed
	sta	var_player0_speed_count		;reset the counter to that
	adc	var_player0_x			;add a little bit of entropy to the system
	jsr 	generate_PRNG			;call prng function 
	and	#const_player_speed_max		;convienent power of 2 modulus
	and	#1				;get either 0 or 1
	adc 	#1				;add 1 to make the range 1 or 2
	sta	var_player1_speed		;set p1 random speed
skip_player1_speed:
	
	;;
	;; check for player collisions
	;;
	lda	var_touching		;load collision state from last frame 
	cmp	#1			;check if it was set
	bne	check_collision		;if it wasn't check this frame

	lda	#%00001111		;volume from 0-15
	sta	AUDV0			;set the volume to be medium
	lda	#15			;30kHz/15
	sta	AUDF0			;load it for the frequency
	lda	#4			;pure tone
	sta	AUDC0			;load it for the control
	
	bit	CXPPMM			;check if D7=1 for a collision between P0 and P1
	bpl	reset_touching		;if not set, reset the collision state
	jmp 	done_collision		;otherwise do nothingg
check_collision:	
	bit	CXPPMM			;check if D7=1 for a collision between P0 and P1
	bpl	done_collision		;if not set, skip the swap
	lda	var_player0_palette_ptr ;load p0 palette pointer into a
	ldx	var_player1_palette_ptr ;load p1 palette pointer into x
	sta	var_player1_palette_ptr ;store a at p1 palette pointer 
	stx	var_player0_palette_ptr ;store x at p0 palette pointer
	inc	var_touching		;set the collision state
	jmp	done_collision		;done with collision
reset_touching:	
	dec 	var_touching		;reset the collision satte
done_collision
	sta	CXCLR			;clear the collision register

	;;
	;;setup before drawing
	;;
	ldy	#8		;number of scanlines for the top playfield
	lda	#%11111111	;set the playfield to be full
	sta	PF0		;
	sta	PF1		;
	sta	PF2		;store in each playfield register
	
loop_vblank:	
	lda	INTIM		;get time left
	sta	WSYNC		;sync up
	bne	loop_vblank	;look until all vblank scanlines have passed
	lda	#0		;load #0 (D1=0, D6=0, D7=0)
	sta	VBLANK		; turn off VBLANK
	
fn_main:
	sta 	HMOVE		;set the horizontal movement for this frame
loop_draw_top:
	sta	WSYNC		;sync with the current scanline
	dey			;decrement the scanlines left
	bne	loop_draw_top	;loop around if not zero
	
	ldx	#176		;number of scanlines for the middle section
	lda	#%00010000	;only put walls on the far sides
	sta	PF0		;load into side registers
	lda	#%00000000	;no playfield walls
	sta	PF1		;
	sta	PF2		;load into middle registers
loop_draw_middle:	
	lda	#0				;set to no graphics
	sta	WSYNC				;4[0] catch up the scanline to our current location
	;;
	;;check if we have to draw player0 and do it!
	;; the scanlines are counted down from the top and so are the y positions of the players
	;; instruction cycles [total cycles]
	cpx	var_player0_y	      		;3[3] check the current scanline number against the players y position
	bcs	draw_player0_done     		;2[5] if scanline <= player0 y position 
	dec	var_player0_scan_left 		;5[10] decrement the scanlines left for player0
	bmi	draw_player0_done 		;2[12] if it is negative check player1
	
	ldy	var_player0_scan_left	    	;3[15] if it isn't load scanlines left into y
	lda	(var_player0_palette_ptr),y 	;5[21] use indirect indexed addressing to get palette data
	sta	COLUP0				;3[24] store it in player0 colour register
	lda	(var_player0_gfx_ptr),y		;5[29] use indirect indexed addressing to get graphic data
draw_player0_done:
	sta	GRP0				;3[32] store the graphics data in the player0 graphics register

	lda	#0	 			;2[34] set a to 0 so when can load more graphics into it
	;;
	;;check if we have to draw player1 and do it!
	;;
	cpx	var_player1_y			;3[37] check the current scanline number against the players y position
	bcs	draw_player1_done		;2[39] if scanline <= player0 y position 
	dec	var_player1_scan_left		;5[44] decrement the scanlines left for player0
	bmi	draw_player1_done		;2[46] if it is negative check player1

	ldy	var_player1_scan_left 		;3[49] if it isn't load scanlines left into y
	lda	(var_player1_palette_ptr),y	;5[54] use indirect indexed addressing to get palette data
	sta	COLUP1				;3[57] store it in player0 colour register
	lda	(var_player1_gfx_ptr),y		;5[62] use indirect indexed addressing to get graphic data
draw_player1_done:
	sta	GRP1				;3[65] store the graphics data in the player0 graphics register

	dex					;2[67] 
	dex					;2[69] decrement the scanline counter twice

	sta	WSYNC				;4[73] 
	bne	loop_draw_middle 		;2[0] if the dex haven't reached zero loop back up

	lda	#0				;
	sta	GRP0				;
	sta	GRP1				;clear player graphics
		
draw_bottom:	
	ldy	#8			;set the counter for 8 lines
loop_draw_bottom:	
	sta	WSYNC			;sync up with the scanline
	lda	#%11111111		;the playfield will be a solid wall
	sta	PF0			;
	sta	PF1			;
	sta	PF2			;set each palyerfield section to be solid
	dey				;scanline--
	bne	loop_draw_bottom 	;if not zero loop back around
	
	
fn_overscan:
	sta	WSYNC			;sync back up
	lda	#2			;load D1=1
	sta	VBLANK			;turn on vblanking
	
	lda 	#34			;34*64/76 = ~28.5 cycles
        sta 	TIM64T			;set timer to wait 

	sta	HMCLR			;clear all horizonal movement registers

	lda	#0	
	sta	var_player0_moving 	;reset the state for player0 movement

	lda	#0
	sta	var_player1_moving 	;reset the state for player0 movement
	
	;;
	;;update p0 controller inputs/movement
	;; 
	lda	SWCHA			;load joystick0 state
	bit	const_joystick0_up	;compare to joystick up
	beq	player0_joystick_up	;branch if equal
	bit	const_joystick0_down	;if not up compare down
	beq	player0_joystick_down	;branch if equal
	jmp 	player0_vertical_done	;up/down not pressed, check left/right
player0_joystick_up:
	lda	var_player0_y		;load current y position
	cmp	#const_player_min_y	;check if it is too small already
	bcs	player0_update_sprite	;if it is check left/right
	inc	var_player0_moving	;set player0 to be moving
	adc	var_player0_speed	;else add the current player speed to the y position
	sta	var_player0_y		;sore the updated value to the y pos
	jmp 	player0_vertical_done	;check left/right
player0_joystick_down:
	lda	var_player0_y		;load current y position
	cmp	#const_player_max_y	;check if it is too big already
	bcc	player0_vertical_done	;if it is check left/right
	inc	var_player0_moving	;set player0 to be moving
	sbc	var_player0_speed	;else subtract the current player speed to the y position
	sta	var_player0_y		;store the updated value to the y pos
	
player0_vertical_done:
	lda	SWCHA			;load joystick0 state
	bit	const_joystick0_left	;compare to joystick left
	beq	player0_joystick_left	;branch if equal
	bit	const_joystick0_right	;if not compare right
	beq	player0_joystick_right	;branch if equal
	jmp	player0_joystick_done	;left/right not pressed
	
player0_joystick_left:
	lda	var_player0_x		;load current x position
	cmp	#const_player_min_x	;check if it is too small already
	bcc	player0_joystick_done	;if it is end joystick movement
	inc	var_player0_moving	;set player0 to be moving
	sbc	var_player0_speed	;else subtract current player speed from x position
	sta	var_player0_x		;store back the updated x position
	lda	var_player0_speed	;load the speed value
	rol				;
	rol				;
	rol				;
	rol				;shift it 4 times to get it in the high nybble
	sta	HMP0			;store it at the horizontal movement register for p0
	
	lda	#8			;load D4=1 to reflect the player
	sta	REFP0			;reflect player0 sprite
	jmp 	player0_update_sprite
player0_joystick_right:
	lda	var_player0_x		;load current x position
	cmp	#const_player_max_x	;check if it is too large already
	bcs	player0_joystick_done	;if it is end joystick movement
	inc	var_player0_moving	;set player0 to be moving
	clc				;clear carry flag
	adc	var_player0_speed	;add the current player speed to the x position
	sta	var_player0_x		;load back the x position
	
	lda	#%00001111		;load all bits on for HMP0 register
	sec				;I don't even know
	sbc	var_player0_speed	;subtract the speed from all on bits
	clc				;what is going on anymore
	adc	#1			;add one to shift values fro 8,1 to 7,0
	rol				;
	rol				;
	rol				;
	rol				;rotate left 4 times to get bits in high nybble
	sta	HMP0			;write to horizonal motion register for p0
player0_joystick_done:
	
	;;
	;; update p0 graphics
	;; 
	lda	var_player0_moving 		;load the player0 moving state	
	beq	reset_player0_frame_ptr		;if it is zero then reset it
						;else continue on 
player0_update_sprite:
	dec	var_player0_frames_left		;decrement the frames left for this player frame
	bne	player0_sprite_done		;if it isn't zero skip advancing the pointers
	lda	#const_sprite_frame_hold 	;else load up the number of frames to wait
	sta 	var_player0_frames_left		;store it and continue to update pointers
	
update_player0_gfx_ptr:
	dec	var_player0_frame_count		;frame_count--
	beq	reset_player0_frame_ptr		;if frame_count==0 reset the pointer
	lda	var_player0_gfx_ptr		;else load the pointer
	clc			   		;clear carry flag
	adc	#const_sprite_size		;add 8 to addr, advance to next sprite frame
	jmp	done_player0_frame_ptr 		;done
reset_player0_frame_ptr:
	lda	#const_sprite_frames	      	;load the number of frames
	sta	var_player0_frame_count		;store it for player0 frame counter
	lda	#<gfx_player_sprite_base_addr 	;load the first frame addr
done_player0_frame_ptr:	
	sta	var_player0_gfx_ptr		;set the pointer to the new value
player0_sprite_done
	
	;;
	;; update p1 controller inputs/movement
	;; 
	lda	#0			;load D4=0 to not reflect the player
	sta	REFP0			;don't reflect player0 sprite

	lda	SWCHA			;load joystick1 state
	bit	const_joystick1_up	;compare to joystick up
	beq	player1_joystick_up	;branch if equal
	bit	const_joystick1_down	;if not up compare down
	beq	player1_joystick_down	;branch if equal
	jmp 	player1_vertical_done	;up/down not pressed, check left/right
player1_joystick_up:
	lda	var_player1_y		;load current y position
	cmp	#const_player_min_y	;check if it is too small already
	bcs	player1_update_sprite	;if it is check left/right
	inc	var_player1_moving	;set player1 to be moving
	adc	var_player1_speed	;else add the current player speed to the y position
	sta	var_player1_y		;sore the updated value to the y pos
	jmp 	player1_vertical_done	;check left/right
player1_joystick_down:
	lda	var_player1_y		;load current y position
	cmp	#const_player_max_y	;check if it is too big already
	bcc	player1_vertical_done	;if it is check left/right
	inc	var_player1_moving	;set player1 to be moving
	sbc	var_player1_speed	;else subtract the current player speed to the y position
	sta	var_player1_y		;store the updated value to the y pos
	
player1_vertical_done:
	lda	#const_joystick1_left	;load joystick left bits
	bit	SWCHA			;compare to joystick state
	beq	player1_joystick_left	;branch if equal
	lda	#const_joystick1_right	;if not load right joystick bits
	bit	SWCHA			;compare to joystick state
	beq	player1_joystick_right	;branch if equal
	jmp	player1_joystick_done	;left/right not pressed
	
player1_joystick_left:
	lda	var_player1_x		;load current x position
	cmp	#const_player_min_x	;check if it is too small already
	bcc	player1_joystick_done	;if it is end joystick movement
	inc	var_player1_moving	;set player1 to be moving
	sbc	var_player1_speed	;else subtract current player speed from x position
	sta	var_player1_x		;store back the updated x position
	lda	var_player1_speed	;load the speed value
	rol				;
	rol				;
	rol				;
	rol				;shift it 4 times to get it in the high nybble
	sta	HMP1			;store it at the horizontal movement register for p1
	
	lda	#8			;load D4=1 to reflect the player
	sta	REFP1			;reflect player1 sprite
	jmp 	player1_update_sprite
player1_joystick_right:
	lda	var_player1_x		;load current x position
	cmp	#const_player_max_x	;check if it is too large already
	bcs	player1_joystick_done	;if it is end joystick movement
	inc	var_player1_moving	;set player1 to be moving	
	clc
	adc	var_player1_speed	;add the current player speed to the x position
	sta	var_player1_x		;load back the x position
	
	lda	#%00001111		;load all bits on for HMP1 register
	sec				;I don't even know
	sbc	var_player1_speed	;subtract the speed from all on bits
	clc				;what is going on anymore
	adc	#1			;add one to shift values fro 8,1 to 7,1
	rol				;
	rol				;
	rol				;
	rol				;rotate left 4 times to get bits in high nybble
	sta	HMP1			;write to horizonal motion register for p1
player1_joystick_done:

	;;
	;; update p1 graphics
	;; 	
	lda	var_player1_moving 		;load the player1 moving state	
	beq	reset_player1_frame_ptr		;if it is zero then reset it
						;else continue on
player1_update_sprite:
	dec	var_player1_frames_left		;decrement the frames left for this player frame
	bne	player1_sprite_done		;if it isn't zero skip advancing the pointers
	lda	#const_sprite_frame_hold 	;else load up the number of frames to wait
	sta 	var_player1_frames_left		;store it and continue to update pointers
	
update_player1_gfx_ptr:
	dec	var_player1_frame_count		;frame_count--
	beq	reset_player1_frame_ptr		;if frame_count==0 reset the pointer
	lda	var_player1_gfx_ptr		;else load the pointer
	clc					;clear carry flag
	adc	#const_sprite_size		;add 8 to addr, advance to next sprite frame
	jmp	done_player1_frame_ptr 		;done
reset_player1_frame_ptr:
	lda	#const_sprite_frames	      	;load the number of frames	
	sta	var_player1_frame_count		;store it for player0 frame counter
	lda	#<gfx_player_sprite_base_addr 	;load the first frame addr
done_player1_frame_ptr:	
	sta	var_player1_gfx_ptr		;set the pointer to the new value
player1_sprite_done

	
loop_overscan_wait
	sta 	WSYNC			;wait for the next scanline
        lda 	INTIM   		;check timer
        bne 	loop_overscan_wait	;if greater than 0 wait another scanline
	
	jmp	fn_vsync		;loop back up to vsync for the next frame

	;;
	;;pseudo-random number generator, returns in a and var_rand
	;; 16-bit galois linear feedback shift register
generate_PRNG:	
	ldx 	#8		;8 rounds
	lda	var_rand	;load low rand byte
loop1:	
	asl			;shift left
	rol	var_rand+1	;rotate left the high byte
	bcc	loop2		;branch back
	eor	#$2D		;xor 2D the low byte
loop2:	
	dex			;round--
	bne	loop1		;wait for the loop to end
	sta	var_rand	;store the low byte
	rts
	
	;; ===============================================================================
	;; Rom Graphics data
	;; ===============================================================================

	;; The rom graphics are upside down to save cycles drawing them by taking advantage of
	;; the decrement opcode and the bne opcode.
	;; It just means that counting scanlines and y position is from top to bottom counting
	;; down.
	;; Also graphics data is aligned on a memory page to avoid incuring extra cycles from
	;; crossing a page boundry
	;; Note that both players use the same graphics just with a palette swap
	
	ALIGN 	256			;align ROM graphics data on page boundry
gfx_player_sprite_base_addr:	
        .byte	#%01010000
        .byte	#%01010000
        .byte	#%00100000
        .byte	#%10101000
        .byte	#%01110000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
gfx_player_sprite_frame1:	
        .byte	#%01100000
        .byte	#%01100000
        .byte	#%00100000
        .byte	#%10110000
        .byte	#%01110000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
gfx_player_sprite_frame2:	
        .byte	#%01100000
        .byte	#%01100000
        .byte	#%00110000
        .byte	#%01110000
        .byte	#%01100000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
gfx_player_sprite_frame3:	
        .byte	#%01100000
        .byte	#%01100000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01101000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
gfx_player_sprite_frame4:	
        .byte	#%01000000
        .byte	#%01010000
        .byte	#%00110000
        .byte	#%10100000
        .byte	#%01111000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
gfx_player_sprite_frame5:	
        .byte	#%01010000
        .byte	#%01010000
        .byte	#%00100000
        .byte	#%01101000
        .byte	#%01110000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
gfx_player_sprite_frame6:	
        .byte	#%00100000
        .byte	#%00110000
        .byte	#%00110000
        .byte	#%10100000
        .byte	#%01111000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
gfx_player_sprite_frame7:	
        .byte	#%00110000
        .byte	#%00110000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
gfx_player_sprite_frame8:	
        .byte	#%01000000
        .byte	#%01010000
        .byte	#%00110000
        .byte	#%10100000
        .byte	#%01111000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
gfx_player_sprite_frame9:	
        .byte	#%01010000
        .byte	#%01010000
        .byte	#%00100000
        .byte	#%01101000
        .byte	#%01110000
        .byte	#%00100000
        .byte	#%01110000
        .byte	#%01110000
gfx_player_sprite_end_addr:	

gfx_player0_palette_addr:	
        .byte 	#$F0
        .byte 	#$70
        .byte 	#$70
        .byte 	#$70
        .byte 	#$70
        .byte 	#$2E
        .byte 	#$2E
        .byte 	#$70
	
gfx_player1_palette_addr:	
        .byte 	#$F0
        .byte 	#$34
        .byte 	#$34
        .byte 	#$34
        .byte 	#$34
        .byte 	#$2E
        .byte 	#$2E
        .byte 	#$34
	;; ===============================================================================
	;; Rom Music data
	;; ===============================================================================

	;; I didn't really know what I was doing with the music so I just messed around
	;; with different values until I got something that sounded decent enough
	;; the data is just stored linearly with the pattern being:
	;; channel 1 frequency
	;; channel 1 control
	;; channel 2 frequency
	;; channel 2 control
	;; each note being played as long as the music hold is and playing the entire
	;; thing through
	;; I didn't add volume controls but that would expand the range of sounds
	;; it could play
	
	
	ALIGN 	256			;align ROM music data on page boundry
music_theme_base_addr:
	.byte	31	;1
	.byte	13
	.byte	26
	.byte	6
	.byte	  	;2
	.byte	
	.byte	24
	.byte	6	
	.byte	28	;3
	.byte	13
	.byte	26
	.byte	6
	.byte		;4
	.byte	
	.byte	24
	.byte	6
	.byte		;5
	.byte	
	.byte	26
	.byte	6
	.byte		;6
	.byte	
	.byte	20
	.byte	6
	.byte	28	;7
	.byte	13
	.byte	18
	.byte	6
	.byte		;8
	.byte	
	.byte	14
	.byte	6
	.byte		;9
	.byte	
	.byte	18
	.byte	6
	.byte		;10
	.byte	
	.byte	20
	.byte	6
	.byte	28	;11
	.byte	13
	.byte	16
	.byte	6
	.byte		;12
	.byte	
	.byte	20
	.byte	6
	
music_theme_end_addr:	
	
	;; ===============================================================================
	;; Rom Interrupt Vectors
	;; ===============================================================================
	
	ORG $FFFA		
	.word 	fn_start		;$FFFA NMI not in 6507 on the atari 2600
	.word 	fn_start		;$FFFC RESET
	.word 	fn_start		;$FFFE IRQ not in 6507 on the atari 2600, BRK still uses it
	END
