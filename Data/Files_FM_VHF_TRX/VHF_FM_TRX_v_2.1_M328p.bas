'Control firmware for simple 2m FM transceiver with Dorji DRA818V.
'Uses Atmega328(P), LCD 2x16, and 6 buttons.
'Uses internal clock of 8MHz.

'For schema of a hardware and configuration bits see: http://599.cz/view.php?cisloclanku=2016082901

'VERSION 2.1:

$regfile = "m328pdef.dat"                                   'Compilation for Atmega328p
$crystal = 8000000                                          'Clock frequency 8MHz
$hwstack = 64                                               'Memory usage
$swstack = 32
$framesize = 128
$baud = 9600                                                'Serial (UART) speed 9600bps
'Default parameters for UART are:  Parity none, Data bits 8, Stop bits 1, Handshake none

'********* config LCD **************************************
Config Lcd = 16x2
Config Lcdpin = Pin , Db4 = Portd.7 , Db5 = Portb.0 , Db6 = Portb.1 , Db7 = Portb.2 , E = Portd.6 , Rs = Portd.5
Cursor Off
'************ Deffinition of special character *************
Deflcdchar 1 , 32 , 31 , 17 , 27 , 27 , 31 , 32 , 32        'Special character to show (LCD) RX or TR under CTCSS
Cls

'********* Config Interrupt Ovf1 Timer1 ********************
Enable Interrupts
Config Timer1 = Timer , Prescale = 1024
On Ovf1 Turn_light_off                                      'Turn off the Led after some time

'***************************************************************************
'Aliases for Buttons * Config as Inputs *** Set Pull-Ups *******************
'***************************************************************************
Butt1 Alias Pinc.3 : Config Butt1 = Input : Set Butt1
Butt2 Alias Pind.2 : Config Butt2 = Input : Set Butt2
Butt3 Alias Pinc.4 : Config Butt3 = Input : Set Butt3
Butt4 Alias Pind.3 : Config Butt4 = Input : Set Butt4
Butt5 Alias Pinc.5 : Config Butt5 = Input : Set Butt5
Butt6 Alias Pind.4 : Config Butt6 = Input : Set Butt6
Ptt Alias Pinb.7 : Config Ptt = Input : Set Ptt             'Input info of the PTT (Push To Talk button)

'* Alias for LED **** Config as Output ** Set On Led ** (PortC.2 is used as output to turn on the LED backlight)
Led Alias Portc.2 : Config Led = Output : Set Led
'* Alias for PWR *** Config as output *** Set to 1W *** (PortB.5 is used as output to switch the RF power)
Pwr Alias Portb.4 : Config Pwr = Output : Reset Pwr
'* Alias for Piip ** Config as output ****  Reset ***    (PortC.1 is used as audio output 1750Hz)
Piip Alias Portc.1 : Config Piip = Output : Reset Piip


'***************************************************************************
'*** Variables declarations ************************************************
'***************************************************************************
Dim Setgroup$ As String * 46                                'String to send completed data through UART to the DRA818V
Setgroup$ = "AT+DMOSETGROUP=0,"                             'Inicial value of the string

Dim E_freq As Eram Dword                                    'Variable of Frequency saved in EEprom
Dim E_rptr As Eram Byte                                     'Variable of Repeater-operating status saved in EEprom
Dim E_ctcss As Eram Byte                                    'Number of CTCSS tone saved in EEprom
Dim E_subtone_status As Eram Byte                           'Variable of subtones state (Tx, Rx, Both) saved in EEprom

Dim Freq As Dword                                           'Variable of actual "working" frequency
Dim Rptr As Byte                                            'Variable of actual Repeater-operating status
Dim Ctcss As Byte                                           'Number of actualy set CTCSS tone
'Dim Ctcss$ As String * 3                                   'not in use
Dim Subtone_status As Byte                                  'Variable of actual subtones status (Tx, Rx, Both)
Dim Sub_tx As Byte                                          'The number of actually set CTCSS tone for Tx
Dim Sub_rx As Byte                                          'The number of actually set CTCSS tone for Rx
Dim Sub_tx$ As String * 4                                   'The number of CTCSS as string to send via UART (Rx)
Dim Sub_rx$ As String * 4                                   'The number of CTCSS as string to send via UART (Tx)
Dim Sub_status_tx$ As String * 1                            'One char string to show whether CTCSS is active on Tx
Dim Sub_status_rx$ As String * 1                            'One char string to show whether CTCSS is active on Rx

Dim Rx_frq As Dword                                         'Working Rx frequency
Dim Rx_frq$ As String * 8                                   'String of Rx frequency to send through UART to DRA818
Dim Tx_frq As Dword                                         'Working Tx frequency
Dim Tx_frq$ As String * 8                                   'String of Tx frequency to send through UART to DRA818

Dim Tone As Byte                                            'Number represents a frequency of CTCSS tone (to show on LCD)
Dim Tone$ As String * 3                                     'String to show the frequency of CTCSS tone on LCD

Dim Pwr_out As String * 3                                   'A string to show the set power (High, Low)
Pwr_out = " 1W"                                             'At start the High power (1 Watt) is set up
Dim Beep As Dword                                           'Lasting of the tone 1750Hz

'*********************************************************************************
'*** Variables necessary for adjusting basic volume, squelch and audio filters ***
'*********************************************************************************
Dim E_filters As Eram Byte                                  'State of all the audio filters saved in EEprom
Dim Filters As Byte                                         'Actually set state of all audio filters
Dim Preemph As Bit : Dim Preemph$ As String * 1             'Auxiliary variables for setting audio filters
Dim Highpass As Bit : Dim Highpass$ As String * 1           'Auxiliary variables for setting audio filters
Dim Lowpass As Bit : Dim Lowpass$ As String * 1             'Auxiliary variables for setting audio filters
Dim Setfilter$ As String * 18                               'Final string to set the filters by sending it to the DRA818
Dim Pre$ As String * 3                                      'String to show the state of the pre/deemphase on the LCD
Dim Hi$ As String * 3                                       'String to show the state of the high-pass filter on the LCD
Dim Lo$ As String * 3                                       'String to show the state of the low-pass filter on the LCD

Dim E_volume As Eram Byte                                   'Value of volume saved in EEprom
Dim Volume As Byte : Dim Volume$ As String * 1              'Auxiliary variables for setting the volume
Dim Setvolume$ As String * 17                               'Final string to set volume by sending it to the DRA818

Dim E_squelch As Eram Byte                                  'Value of the squelch saved in EEprom
Dim Squelch As Byte : Dim Squelch$ As String * 1            'Auxiliary variables for setting the squelch
                                                            'Value of the quelch is send to the DRA818 together with frequency,
                                                            ', CTCSS, etc.. by the command "AT+DMOSETGROUP=..."
Dim Adjust As Bit : Reset Adjust                            'Flag which indicates whether we are in the adjusting mode or not.


'***************************************************************************
'**** Constants ************************************************************
'***************************************************************************
Const Rptr_shift = 6000                                     'Shift frequency for repeater (600kHz)
Const Step_1 = 125                                          'Smaller frequency step   (12,5kHz)
Const Step_2 = 250                                          'Larger frequency step    (25kHz)

Const Direct = 1                                            'Normal direct (Rx=Tx)
Const Repeater = 2                                          'Normal repater (Tx=Rx-600kHz)
Const Rptr_inp = 3                                          'Listening at the input frequency of a repeater

Const No_sub = 1                                            'CTCSS not used
Const Tx_sub = 2                                            'CTCSS on Tx side
Const Both_sub = 3                                          'CTCSS on both Tx and Rx

Const Band_limit_upper = 1460000                            'Upper limit of Freq which loads from EEprom
Const Band_limit_lower = 1445000                            'Lower limit of Freq which loads from EEprom
Const Band_limit_default = 1455000                          'Default freq when EEprom out of limits

Const Repeater_limit_upper = 1458000                        'Upper limit for automatic switch to repeater mode
Const Repeater_limit_lower = 1456000                        'Lower limit for automatic switch to repeater mode

'***************************************************************************
'***** Read and set frequency from EEprom **********************************
'***************************************************************************
Freq = E_freq
If Freq > Band_limit_upper Then                             'When frequency is above the HAM band...
   Freq = Band_limit_default                                '...then sets the default frequency
End If
If Freq < Band_limit_lower Then                             'When the frequency is below the HAM band (FM)...
   Freq = Band_limit_default                                '...then sets the default frequency
End If

'****************************************************************************
'**** Read and set a repeater state from EEprom *****************************
'****************************************************************************
Rptr = E_rptr
If Rptr > Rptr_inp Then                                     'When the status is "unknown"...
   Rptr = Direct                                            '...then sets the direct mode
End If

'****************************************************************************
'**** Read and set the CTCSS number from EEprom *****************************
'****************************************************************************
Ctcss = E_ctcss
If Ctcss > 38 Then                                          'The 38 is the highest number of the CTCSS tone (see a datasheet of DRA818V)
   Ctcss = 0
End If

'***************************************************************************
'**** Read and set CTCSS status (None, Tx, Tx+Rx) **************************
'***************************************************************************
Subtone_status = E_subtone_status
If Subtone_status > Both_sub Then                           'When the CTCSS status is in "unknown" state...
   Subtone_status = Tx_sub                                  '...then set CTCSS to Tx
End If

'***************************************************************************
'**** Read and set the Squelch from EEprom *********************************
'***************************************************************************
Squelch = E_squelch
   If Squelch > 8 Then
      Squelch = 2
   End If

Gosub Sub_status_set                                        'Set all params from EEprom and send it via UART to DRA818V

'***************************************************************************
'**** Read and set Audio filters from EEprom *******************************
'***************************************************************************
Filters = E_filters
   If Filters > 7 Then
      Filters = 0
      E_filters = Filters
   End If

Gosub Filters_send

'***************************************************************************
'**** Read and set audio volume from EEprom ********************************
'***************************************************************************
Volume = E_volume
   If Volume > 8 Then
      Volume = 1
   End If

Gosub Volume_send

'***************************************************************************
'**** Some advertisement to the display  ;-) *******************************
'**** Here the TRX works, even if display shows this advertisement *********
'***************************************************************************
Locate 1 , 1 : Lcd "VHF FM ver: 2.01"
Locate 2 , 1 : Lcd "DRA818+Atmel AVR"
Wait 3 : Cls
Locate 1 , 1 : Lcd " Author: OK1HDU "
Locate 2 , 1 : Lcd "August 20th 2016"
Wait 3 : Cls
'***************************************************************************

'***************************************************************************
'**** Jump to set-up basic volume when hold button_1 ***********************
'***************************************************************************
If Butt1 = 0 Then
   Gosub Basic_volume
   Waitms 100
End If

If Butt3 = 0 Then
   Gosub Adjust_filters
   Waitms 100
End If

'After advertisement show params from EEprom on LCD
Gosub Show_lcd
Waitms 300

'***************************************************************************
'**** Main loop, waiting for the buttons ***********************************
'***************************************************************************
Do
If Butt6 = 1 Then                                           'Button 6 released
   Debounce Butt1 , 0 , Freq_up , Sub                       'Button 1 - frequency Up (small step)
   Debounce Butt2 , 0 , Freq_dwn , Sub                      'Button 2 - frequency Down (small step)
   Debounce Butt3 , 0 , Ctcss_up , Sub                      'Button 3 - CTCSS Up
   Debounce Butt4 , 0 , Ctcss_dwn , Sub                     'Button 4 - CTCSS Dwn
   Debounce Butt5 , 0 , Rptr_switch , Sub                   'Button 5 - Direct x Repeater mode
      If Ptt = 1 Then
         Debounce Butt6 , 0 , Ptt_1750 , Sub                'PTT + Button 6 = 1750Hz tone (2 seconds)
      End If

Elseif Butt6 = 0 Then                                       'Button 6 hold
   Debounce Butt1 , 0 , Freq_up , Sub                       'Button 1 - frequency UP (large step)
   Debounce Butt2 , 0 , Freq_dwn , Sub                      'Button 2 - frequency Down (large step)
   Debounce Butt3 , 0 , Sub_state , Sub                     'Button 3 - CTCSS mode (None, Tx, Both)
   Debounce Butt4 , 0 , Light , Sub                         'Button 4 - LED (background light)
   Debounce Butt5 , 0 , Power_set , Sub                     'Button 5 - Set the power (1W x Low)
   Debounce Ptt , 1 , Ptt_1750 , Sub                        'Button 6 + PTT = 1750Hz tone (2 seconds)

End If
Loop


'+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
'+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
'+ + +  S U B R U T I N E S  + + + + + + + + + + + + + + + + + + + + + + + +
'+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
'+ + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +


'**************************************************************************
'**** Show Frequencies, CTCSS, etc.... on LCD 2x16 ************************
'**************************************************************************
Show_lcd:
Tone = Lookup(ctcss , Subtones_table)                       'Take CTCSS tone from table
Tone$ = Str(tone) : Tone$ = Format(tone$ , "000")           'change tone to string

Locate 1 , 1 : Lcd "Rx: " ; Rx_frq$                         'Display everything on the LCD
Locate 2 , 1 : Lcd "Tx: " ; Tx_frq$
Locate 2 , 14 : Lcd Pwr_out
Locate 1 , 14 : Lcd Tone$
Locate 1 , 13 : Lcd Sub_status_rx$
Locate 2 , 13 : Lcd Sub_status_tx$
Return

'*************************************************************************
'*** Collect data and send it through UART *******************************
'*************************************************************************
Setgroup:
Enable Timer1                                               'Start light timer
Timer1 = 0
Set Led                                                     'and turn light ON
Enable Ovf1                                                 'Enable overflow interrupt

If Rptr = 1 Then                                            'set frequencies, depending on repeater status
   Tx_frq = Freq
   Rx_frq = Freq
Elseif Rptr = 2 Then
   Tx_frq = Freq - Rptr_shift
   Rx_frq = Freq
Elseif Rptr = 3 Then
   Tx_frq = Freq - rptr_shift
   Rx_frq = Freq - rptr_shift
End If

Tx_frq$ = Str(tx_frq) : Tx_frq$ = Format(tx_frq$ , "   .    ")       'turn frequencies to strings
Rx_frq$ = Str(rx_frq) : Rx_frq$ = Format(rx_frq$ , "   .    ")
Sub_tx$ = Str(sub_tx) : Sub_tx$ = Format(sub_tx$ , "0000")  'turn CTCSS numbers to strings
Sub_rx$ = Str(sub_rx) : Sub_rx$ = Format(sub_rx$ , "0000")
Squelch$ = Str(squelch)

Setgroup$ = Setgroup$ + Tx_frq$ : Setgroup$ = Setgroup$ + ","       'Make the long string to send data...
Setgroup$ = Setgroup$ + Rx_frq$ : Setgroup$ = Setgroup$ + ","       '...via UART to DRA818V (46 characters)
Setgroup$ = Setgroup$ + Sub_tx$ : Setgroup$ = Setgroup$ + ","
Setgroup$ = Setgroup$ + Squelch$ : Setgroup$ = Setgroup$ + ","
Setgroup$ = Setgroup$ + Sub_rx$

Print Setgroup$                                             'Sending string to DRA818V
Setgroup$ = "AT+DMOSETGROUP=0,"                             'Again set the inicial value of the string

If Adjust = 1 Then
   Return
End If

Gosub Show_lcd                                              'Show values on a display
Return


'**************************************************************************
'**** Setting a Power *****************************************************
'**************************************************************************
Power_set:
Toggle Pwr                                                  'Swap the output to switching transistor(PORTb.5)
If Pwr = 0 Then
Pwr_out = " 1W"
Elseif Pwr = 1 Then
Pwr_out = "Low"
End If
Gosub Setgroup
Return

'**************************************************************************
'**** Toggle (swap) background light (LED) ********************************
'**************************************************************************
Light:
Toggle Led
Return

'**************************************************************************
'**** Save Values to EEprom and turn the light Off (LED) ******************
'**************************************************************************
Turn_light_off:
E_freq = Freq                                               'Save actual frequency to EEprom
E_rptr = Rptr                                               'Save actual Repeater status to EEprom
E_ctcss = Ctcss                                             'Save actual CTCSS number
E_subtone_status = Subtone_status                           'Save actual CTCSS status
E_volume = Volume
E_squelch = Squelch
E_filters = Filters

Disable Timer1
Disable Ovf1
Reset Led                                                   'Turn off the LED (background light)
Return

'**************************************************************************
'**** Frequency step UP ***************************************************
'**************************************************************************
Freq_up:
If Butt6 = 0 Then
   Freq = Freq + Step_2                                     'large step (25kHz)
Else
   Freq = Freq + Step_1                                     'Small step (12.5kHz)
End If
If Freq >= Repeater_limit_lower Then                        'Limits for automatic repeater shift
   If Freq < Repeater_limit_upper Then                      '   -//-
      Rptr = Repeater                                       'Set to repeater operation
   Else
      Rptr = Direct                                         'Set to simplex operation
   End If
End If
Gosub Setgroup                                              'Jump to send data through UART and to LCD
Waitms 100
If Butt1 = 0 Then                                           'Button Auto repeat function (after 100ms)
    Goto Freq_up
End If
Return

'***************************************************************************
'**** Frequency step DOWN **************************************************
'***************************************************************************
Freq_dwn:
If Butt6 = 0 Then
   Freq = Freq - Step_2                                     'Large step (25kHz)
Else
   Freq = Freq - Step_1                                     'Small step (12.5kHz)
End If
If Freq < Repeater_limit_upper Then                         'limits for automatic repeater shift
   If Freq >= Repeater_limit_lower Then                     '   -//-
      Rptr = Repeater                                       'Set to repeater operation
   Else
      Rptr = Direct                                         'Set to simplex operation
   End If
End If
Gosub Setgroup                                              'Jump to send data through UART and to LCD
Waitms 100
If Butt2 = 0 Then
   Goto Freq_dwn                                            'Button Auto repeat function
End If
Return

'***************************************************************************
'*** Switch Repeater operation (Simplex, Repeater, Listening of input) *****
'***************************************************************************
Rptr_switch:
Rptr = Rptr + 1                                             ' 1 ~ simplex
If Rptr > Rptr_inp Then                                     ' 2 ~ repeater
Rptr = Direct                                               ' 3 ~ listening input
End If
Gosub Setgroup                                              'Jump to send data through UART and to LCD
Return

'***************************************************************************
'**** CTCSS subtone Up *****************************************************
'***************************************************************************
Ctcss_up:
Ctcss = Ctcss + 1                                           'See a table of subtones in a Data table at the end
If Ctcss > 38 Then                                          'of the program
Ctcss = 0                                                   ' 0 ~ no subtone  (just carrier squelch)
End If
Gosub Sub_status_set
Waitms 100
If Butt3 = 0 Then                                           'Button auto repeat function
   Goto Ctcss_up
End If
Return

'***************************************************************************
'**** CTCSS subtone Down ***************************************************
'***************************************************************************
Ctcss_dwn:
Ctcss = Ctcss - 1                                           'See a table of subtones in a Data table at the end
If Ctcss > 38 Then                                          'of the program
Ctcss = 38                                                  ' 0 ~ no subtone  (just carrier squelch)
End If
Gosub Sub_status_set
Waitms 100
If Butt4 = 0 Then                                           'Button auto repeat function
   Goto Ctcss_dwn
End If
Return

'***************************************************************************
'**** CTCSS subtone status (None, Tx, Tx+Rx) *******************************
'***************************************************************************
Sub_state:
Subtone_status = Subtone_status + 1                         ' 1 ~ CTCSS neither Tx nor Rx
If Subtone_status > Both_sub Then                           ' 2 ~ Tx CTCSS , Rx clear
   Subtone_status = No_sub                                  ' 3 ~ Both Tx and Rx use CTCSS
End If
Gosub Sub_status_set
Return

'***************************************************************************
'**** CTCSS subtone status (None, Tx, Tx+Rx). Second part ******************
'*** Sets strings of CTCSS for sending via UART and a notice on LCD ********
'***************************************************************************
Sub_status_set:
If Subtone_status = No_sub Then                             'CTCSS is not used
   Sub_tx = 0 : Sub_status_tx$ = " "
   Sub_rx = 0 : Sub_status_rx$ = " "
Elseif Subtone_status = Tx_sub Then                         'CTCSS is used on the Tx side
   Sub_tx = Ctcss : Sub_status_tx$ = Chr(1)
   Sub_rx = 0 : Sub_status_rx$ = " "
Elseif Subtone_status = Both_sub Then                       'CTCSS is used on both Tx and Rx side
   Sub_tx = Ctcss : Sub_status_tx$ = Chr(1)
   Sub_rx = Ctcss : Sub_status_rx$ = Chr(1)
End If
Gosub Setgroup                                              'Jump to send data via UART and to LCD
Return

'***************************************************************************
'*** Generate tone 1750Hz for 2 seconds when PTT while holding Button_6 ****
'*** During the tone, the LED (backlight) is ON ****************************
'***************************************************************************
Ptt_1750:
Set Led                                                     'Turn On the backlight of the display
   For Beep = 0 To 4000                                     'make 4000 cycles...
      Set Piip                                              '...of set pin...
      Waitus 282                                            '...wait...
      Reset Piip                                            '...reset pin...
      Waitus 283                                            '...wait...
   Next Beep                                                'Result is about 2 seconds of frequency about 1750Hz
Reset Led                                                   'Turn Off the backlight of the display
Return

'***************************************************************************
'**** Send the status of Audio filters to DRA818V **************************
'***************************************************************************
Filters_send:
Enable Timer1                                               'Start light timer
Timer1 = 0
Set Led                                                     'and turn light ON
Enable Ovf1                                                 'Enable overflow interrupt

Preemph = Filters.0 : Preemph$ = Str(preemph)               'Create a long string to send the filters value to the DRA818
Highpass = Filters.1 : Highpass$ = Str(highpass)
Lowpass = Filters.2 : Lowpass$ = Str(lowpass)

Setfilter$ = "AT+SETFILTER=" + Preemph$ : Setfilter$ = Setfilter$ + ","
Setfilter$ = Setfilter$ + Highpass$ : Setfilter$ = Setfilter$ + ","
Setfilter$ = Setfilter$ + Lowpass$
Print Setfilter$                                            'Sending the long string through UART to DRA818
Return

'***************************************************************************
'**** Adjust audio filters *************************************************
'***************************************************************************
Adjust_filters:
Set Adjust                                                  'set the flag that we are in the adjusting mode

Cls
Locate 1 , 1                                                'print the first line of LCD display
Lcd " Pre  High  Low "

Do
If Butt2 = 0 Then                                           'When button2 is active, then...
   Toggle Filters.0                                         '...change the state of the pre/deemphase...
   Gosub Filters_send                                       '... and send it to the DRA818.
   Waitms 200
End If

If Butt4 = 0 Then                                           'When button4 is active, then...
   Toggle Filters.1                                         '...change the state of the high-pass filter...
   Gosub Filters_send                                       '...and send it to the DRA818.
   Waitms 200
End If

If Butt6 = 0 Then                                           'When button6 is active, then...
   Toggle Filters.2                                         '...change state of the low-pass filter...
   Gosub Filters_send                                       '...and send it to the DRA818.
   Waitms 200
End If

If Butt5 = 0 Then                                           'When button5 is active, then...
   Cls : Waitms 500                                         '...clear the display, wait 1/2 sec...
   Reset Adjust                                             '...reset the flag of the adjusting mode...
   Return                                                   '...and return to normal operation.
End If

   If Preemph = 0 Then
      Pre$ = "On "                                          'Assign the text "On" or "Off" to the new state of the filter
   Else
      Pre$ = "Off"
   End If

   If Highpass = 0 Then
      Hi$ = "On "
   Else
      Hi$ = "Off"
   End If

   If Lowpass = 0 Then
      Lo$ = "On "
   Else
      Lo$ = "Off"
   End If

Locate 2 , 1 : Lcd " " ; Pre$                               'Write to the second line of the display the new state of filters
Locate 2 , 7 : Lcd Hi$
Locate 2 , 13 : Lcd Lo$

Loop
'***************************************************************************
'**** Send the value of Volume to DRA818V **********************************
'***************************************************************************
Volume_send:
Enable Timer1                                               'Start light timer
Timer1 = 0
Set Led                                                     'and turn light ON
Enable Ovf1                                                 'Enable overflow interrupt

Volume$ = Str(volume)                                       'Create a string to send the value of volume to the DRA818
Setvolume$ = "AT+DMOSETVOLUME=" + Volume$
Print Setvolume$                                            'Send the string of volume value through UART to the DRA818.

Return

'***************************************************************************
'**** Adjust the basic Volume and the Squelch ******************************
'***************************************************************************
Basic_volume:
Set Adjust                                                  'Set the flag of the adjusting mode

Cls                                                         'Clear the display ...
Locate 1 , 1 : Lcd "Audio Volume  = "                       '...and put the texts on the dislay.
Locate 2 , 1 : Lcd "Audio Squelch = "


Do
   Locate 1 , 16 : Lcd Volume$                              'Put the values of the volume...
   Locate 2 , 16 : Lcd Squelch$                             '...and the squelch on the display.
   Waitms 200

   If Butt3 = 0 Then                                        'If button3 is active, then...
      Volume = Volume + 1                                   '...increase the volume variable by 1...
      If Volume > 8 Then                                    'When the value is at the top,...
         Volume = 8                                         '...then stay there.
      End If
   Gosub Volume_send                                        'jump to subroutine to send the value to the DRA818
   End If

   If Butt4 = 0 Then                                        'If button4 is active, then...
      Volume = Volume - 1                                   '...decrease the volume variable by 1...
      If Volume < 1 Then                                    'When the value is at the bottom,...
         Volume = 1                                         '...then stay there.
      End If
   Gosub Volume_send                                        'jump to subroutine to send the value to the DRA818
   End If

   If Butt5 = 0 Then                                        'If button5 is active, then...
      Squelch = Squelch + 1                                 '...increase the squelch variable by 1...
      If Squelch > 8 Then                                   'When the value is at the top,...
         Squelch = 8                                        '...then stay there.
      End If
   Gosub Setgroup                                           'jump to the "Setgroup" subroutine to send the value to the DRA818
   End If

   If Butt6 = 0 Then                                        'If button6 is active, then...
      Squelch = Squelch - 1                                 '...decrease the squelch variable by 1...
      If Squelch > 8 Then                                   'When the value is at the bottom,...
         Squelch = 0                                        '...then stay there.
      End If
   Gosub Setgroup                                           'jump to the "Setgroup" subroutine to send the value to the DRA818
   End If
                                                            'If button2 is active,...
   If Butt2 = 0 Then                                        '...then reset the adjust mode flag,...
      Reset Adjust                                          '...clear the display...
      Cls                                                   '...wait 1/2 sec,...
      Waitms 500                                            '...and return to the normal operation.
   Return
   End If
Loop

End                                                         'Kanets filma

'***************************************************************************
'**** Table of subtones. Frequencies displayed without decimal point.) *****
'***************************************************************************
Subtones_table:
Data 0 , 67 , 71 , 74 , 77 , 79 , 82 , 85 , 88 , 91 , 94 , 97 , 100 , 103 , 107 , 110 , 114 , 118 , 123 , 127 , 131 , 136 , 141 , 146 , 151 , 156 , 162 , 167 , 173 , 179 , 186 , 192 , 203 , 210 , 218 , 225 , 233 , 241 , 250