                      Progr           segment
                assume  cs:Progr, ds:dane, ss:stosik
.code
start:          mov     ax,dane
                mov     ds,ax
                mov     ax,stosik
                mov     ss,ax
                mov     sp,offset szczyt 
                
;========INICJALIZACJA========
    
    call otworzPlik
   
	call sprawdzParametry
	call wczytajTytul
	 
	call parseParametry   
	call sprawdzParametry
	
	call wczytajMelodie
	
	call zamknijPlik
	
	call trybGraficzny
	call wypiszTempo
	
	lea si, melodia
	call odtwarzanieMelodii
	
	call wylaczTrybGraficzny   
	
	
	;zakonczenie programu                                    
    mov     ah,4ch
    mov	    al,0
    int	    21h


odtwarzanieMelodii:
	lodsb     ;laduje bit z DS:SI do AL, inkrementuje SI
	cmp al,0
    je koniecPetli
    mov [dZnalezione],0
    mov [oZnalezione],0
    mov [polnuta],0
    mov [kropka],0   
    
    call sprawdzCzas
    call sprawdzNute
    call czyPolnuta
    call czyKropka
    call ktoraOktawa
    
    call przeliczTempo
    call odswiezEkran
    call odtworzDzwiek
    call plusMinus
	
    jmp odtwarzanieMelodii
    
    koniecPetli:
    ret
	
;========FUNKCJE DO INICJALIZACJI========

otworzPlik:
           
 
    call nazwaZbufora       ;odczytanie nazwy z parametru programu
    mov al,0				;
    mov ah, 3dh				;otworzenie pliku
    lea dx,  nazwaPliku	    ;nazwa do interrupta
    int 21h					;
    jc bladOtwieraniaPliku	;jezeli CF=1, blad pliku
    mov uchwytPliku,ax		; w ax-uchwyt do pliku
	
    ret
	    
nazwaZbufora:              ;odczytanie nazwy pliku z wiersza polecen, wpisanie go do nazwaPliku         
        mov dx,@data                
        mov ds,dx                   
        xor cx,cx                   
        mov cl,es:[80h]        
        cmp cx,0                    
        jz stop                     
        mov si,0082h      ;tu znajduje sie start parametrow programu           
        xor bx,bx                   
    l1:                             
                                    
        mov al,es:[si]                  
        cmp al,0Dh           ;jak natrafi na zero, to konczy       
        jz stop   
        
    	
    cont:
        mov ds:[nazwaPliku + bx],al    ;zapis do bufora
        
        inc bx
    lab:
        inc si
        loop l1 
    stop:  
        mov ds:[nazwaPliku + bx],0   ;wpisanie na koncu nazwy 0 (potrzebne do otworzenia pliku)
        ret  



	    
	



pozaZakresem:
    lea dx,bladParametru	;
    call wypisywanie		;
    mov ah,4ch				; 	wyswietla wartosc przy przepelnieniu
	mov al,0				;
	int	21h					;
	
sprawdzParametry:
    cmp [setD],0		    ;
    jne dPoprawne1			;
    mov [setD],1		    ;
							;
    dPoprawne1: 			;	
							;
    cmp [setD],32		    ;
    jna dPoprawne2			;
    mov [setD],1		    ;
						   	;
    dPoprawne2:				;
							;
    cmp [setO],0		    ;	Sprawdzamy poprawnosc parametrow pobranych z pliku
    jne oPoprawne1			;	w przypadku blednych zamieniamy na domyslne
    mov [setO],1		    ;
							;
    oPoprawne1:				;
							;
    cmp [setO],7		    ;	
    jna oPoprawne2			;
    mov [setO],1		    ;
							;
    oPoprawne2:				;
							;
    cmp [setB],25		    ;
    jnb bPoprawne			;
    mov [setB],100		    ;
	                        
    bPoprawne: 
        
    ret
	
wczytajTytul:
	lea dx,tytul			;
    tytulPetla:				;
    mov bx,uchwytPliku		;
    mov cx,1				;
							;
    mov ah,3fh				;		Odczytujemy plik znak po znaku i zapisujemy
    int 21h					;		tytul w pamieci
    jc bladOtwieraniaPliku	;
    inc [dlugoscTytulu]     ;         
    mov bx,dx				;
    inc dx               	;
    cmp [bx],':'			;      : oznacza koniec tytulu, dalej sa parametry d,o,b
    jne tytulPetla			;
    mov [bx],'$'			;       wsadzamy na koncu $
        
    ret
	
parseParametry:
   lea dx, buforDOB	
   parametrypetla:			;
    mov bx,uchwytPliku		;
    mov cx,1				;
							;
    mov ah,3fh				;		odczytujemy plik znak po znaku, uchwyt ju¿ jest ustawiony za tytulem
    int 21h					;		wpisujemy lancuch do bufora 
    jc bladOtwieraniaPliku	;
							;
    mov bx,dx               ;
    inc dx				    ;
    cmp [bx],':'            ;
    		                ;
    jne parametrypetla		;   
    
    
    
    lea  dx, buforDOB      ;
    mov di,dx              ;
    parsowaniePetla:       ;
                           ;
    mov bx,[di]            ;
    xor bh,bh              ;       odczytanie juz z bufora rzeczy, przypasowanie do odpowiedniej sekcji 
    cmp bx,'o'             ;
    jz parseo              ;
    cmp bx, 'b'            ;
    jz parseb              ;
     cmp bx, 'd'           ;
    jz parsed              ;
    cmp bx,':'             ;
    jz wyjscie             ;
     cmp bx,00             ;
    jz wyjscie             ;
    
    parseo:                 ;
    inc di                  ;  skip o=
    inc di                  ;
    mov dx,di               ;
    call parseDec           ;  zmiana z ASCII na wartosc
    mov [setO],ax           ;
                            ;
    inc di                  ; przesuniecie dalej uchwytu
    jmp parsowaniePetla     ;
                            
                            
    parseb:                 ;analogicznie do parseo
    inc di
    inc di  
    mov dx,di
    call parseDec 
    mov [setB],ax     
    inc di
    jmp parsowaniePetla
    
    
    parsed:                 ;analogicznie do parseo
    inc di
    inc di  
    mov dx,di
    call parseDec 
    mov [setD],ax     
    
    inc di
    jmp parsowaniePetla
    
    wyjscie:
    
    ret
    
    
parseDec:     ;dx-ptr, ret ax zamiana na wartosci dziesietne
      push bx
      push cx
      
      xor cx,cx  
      xor bx,bx
      
      loopparse:
      mov di,dx 
     
      mov bl,[di]
      cmp bl,'0'
      jl wyjsciepar
      cmp bl,'9'
      jg wyjsciepar  
      
      sub bx,'0' 
      mov al,10 
      push dx
      mul cx
       
      pop dx  
      mov cx,ax
      add cx,bx 
      
      inc dx
      jmp loopparse
   
      wyjsciepar:  
      mov ax,cx
      pop cx
      pop bx 
      
	  ret           
	                            
wczytajMelodie:
   lea dx,melodia			;
    mov bx,uchwytPliku		;   wskaznik gdzie jestesmy w plikju jest DOSowy
    mov cx,1				;
    melodiaPetla:			;
    mov ah,3fh				;		Wczytujemy melodie do pamieci
    int 21h					;
    jc bladOtwieraniaPliku	;
    inc dx					;
    cmp ax,0				;
    jne melodiaPetla		;
	
    ret
	
zamknijPlik:
	mov bx,uchwytPliku		;
     mov ah,3eh				;		Zamykamy plik
     int 21h				;
  
     ret
	 
trybGraficzny:
    mov ax,0				;	Uruchomienie trybu graficznego    tekstowy, 40x25, segment 0B800
    int 10h					;
	
    mov dl,0				;
    mov dh,0				;	Ustawienie kursora
    mov bh,0				;
    mov ah,2				;	ustawienie kursora na  0 strone,wiersz 0 kolumne 0
    int 10h					;                           
    
    lea dx,tytul			;	Wypisanie tytulu utworu
    call wypisywanie		;
    
    ret

wypiszTempo:

	mov dh,1				;     ustawienie kursora na  0 strone,wiersz 1 kolumne 0
    mov dl,0				;
    mov bh,0				;		Ustawienie kursora
    mov ah,02h				;
    int 10h					;

    mov bx,10					;
    mov ax,[setB]			;
    lea di,koniecCiaguZnakow-1	;
								;
    tempoPetla:					;
    mov dx,0					;
    div bx						;	Wypisujemy ciag znakow Tempo: i wartosc tempa
    add dl,48					;   zamiana wartosci tempa na lancuch
    mov ds:di,dl				;
    dec di						;
    cmp ax,0					;
    jne tempoPetla				;
								;
								;
    lea dx,tempoStr				;	
    call wypisywanie			;
    ret
     
	
    ret
	
wylaczTrybGraficzny:
	mov ah,00h		;
    mov al,03h		;	wylaczenie trybu graficznego
    int 10h			;

    lea dx,koniecProgramu
    call wypisywanie
    
    ret
	
;========FUNKCJE DO ODTWARZANIA MELODII========

sprawdzCzas:
	cmp al,48		;
    jb czasDalej	;	Sprawdza czy znak jest cyfra
    cmp al,57		;
    ja czasDalej	;
    
    sub al,48					;
    mov dl,al					;
    mov al,[dZnalezione]		;		
    mov bl,10					;	Zamienia znaleziony czas na wartosc liczbowa
    mul bl						;
    add al,dl					;
    mov [dZnalezione],al		;
								;
    lodsb						;
    jmp sprawdzCzas				;
    
    czasDalej:		
    cmp [dZnalezione],0			;
    jne zwrocCzas 				; 	Gdy znaleziony czas jest rowny zeru to przypisuje wartosc domyslna
    mov bx,[setD]			;
    mov [dZnalezione],bl    	;
    zwrocCzas:
    ret
	
sprawdzNute:
    cmp al,'a'		;
    jb bladMel		;
    je nutaA		;
					;
    cmp al,'p'		;
    je pauza		;
    ja bladMel		;	Sprawdza nute
					;
    cmp al,'h'		;
    je nutaH		;
    ja bladMel		;
					;
    cmp al,'b'		;
    je nutaB		;
    
    sub al,98      	;
    mov [nuta],al	;
    ret 			;
					;
    nutaB:			;
    mov [nuta],8	;
    ret				;
					;
    nutaH:			;
    mov [nuta],7	;	i zamienia na wartosc liczbowa odpowiadajaca danej nucie
    ret				;
					;
    pauza:			;
    mov [nuta],0	;
    ret				;
					;
    nutaA:			;
    mov [nuta],6	;
    ret 			;

czyPolnuta:
	lodsb				;
    cmp al,'#'			;
    jne czyPolnutaWynik	;
    mov [polnuta],1		;		Sprawdza czy polton
    lodsb				;
						;
    czyPolnutaWynik:	;
    ret					;

czyKropka:				
	cmp al,'.'			;
    jne czyKropkaWynik	;
    mov [kropka],1		;
    lodsb				;		Sprawdza czy kropka (dlugosc tego co jest podane plus pol)
						;
    czyKropkaWynik:		;
    ret					;

ktoraOktawa:				
	cmp al,48			;
    jb oktawaDalej		;	Sprawdza czy znak jest cyfra
    cmp al,57			;
    ja oktawaDalej		;
							
    sub al,48				;
    mov dl,al				;
    mov al,[oZnalezione]	;
    mov bl,10				;
    mul bl					;	Zmienia na wartosc liczbowa
    add al,dl				;
    mov [oZnalezione],al	;
							;
    lodsb					;	
    jmp ktoraOktawa			;
    
    oktawaDalej:
    cmp [oZnalezione],0		;
    jne ktoraOktawaWynik 	;
    mov bx,[setO]		    ;	Gdy znalezionz oktawa jest rowna zeru przypisuje domyslna wartosc
    mov [oZnalezione],bl    ;
    ktoraOktawaWynik:		;
    ret    	

przeliczTempo:
	mov ax,60000		;
    mov dx,00h			;
    div setB     	    ;		przeliczamy wartosc tempa
    mov tempo,ax		;
    
    ret 

odswiezEkran:
	mov ax,Ekran		;   adres of display data segment
    mov es,ax			;
    mov cx,1000			;    ilosc bitów /2 bo co drugi robimy 
    mov ah,00h			;		przygotowujemy wartosci do zmiany koloru ekranu
    mov al,[nuta]		;    
    mov di,ax			;     zaladowanie koloru odpowiadajacego nucie
    mov al,[Kolory+di]	;
						
						
    mov di,1			;
    ekranPetla:			;
    mov es:[di],al		;		Zmieniamy co drugi bit aby nie zmienic wpisanego tekstu
    add di,2			;
    loop ekranPetla		;
    
    ret 

odtworzDzwiek:

    cmp tempo,0 		;
    jne przeskok		;		Sprawdza czy tempo jest rowne 0 jesli tak to robimy defaultowe
    mov tempo,1			;
    
    przeskok:
     mov ax,tempo		;
    xor bx,bx			;
    mov bl,dZnalezione	;
    xor dx,dx         	;
    div bx				;
    mov czasTrwania,ax	;		Obliczamy czas trwania jednego dzwieku
    cmp [kropka],0		;
    je odtworzBezKropki	;
    xor dx,dx			;
    mov bx,2			;
    div bx				;
    add czasTrwania,ax	;
    
    odtworzBezKropki:	;
    cmp [nuta],0     	; 		Sprawdzamy czy grana nuta nie jest pauza
    je odtworzPauze		;
    
    
    xor cx,cx		;
    mov cl,[nuta]	;		pobieramy wartosc liczbowa przypisana do nuty
    mov di,cx		;
    
    cmp [polnuta],0		;	Sprawdzamy czy dzwiek nie jest po
    jne odtworzPolNute 	; 	
	
	
    mov al,[nutyLista+di] ; 	JeÅ›li dzwiek nie jest poltonem to pobieramy wartosc z listy nut
    odtworzDzwiek1:
    mov cl,[oZnalezione]	;
    dec cl            		;
    mov bl,1				;		Pobieramy numer oktawy i mnozymy czestotliwosc nuty z 2^nroktawy
    shl bl,cl ;				;
    mul bl            		;
    mov [NutaOktawa],ax 	;		
    
    mov ax,34DCh		;
    mov dx,0012h		;		Dzielimy stale wartosci z ax:dx przez podzielnik otrzymany wczesniej
    div NutaOktawa      ;
    
    mov [czestotliwosc],ax  ;	dokladna czestotliwosc dzwieku
      
    mov al,182					;
    out 43h,al					;
								;
    mov ax,czestotliwosc		;	Wpisujemy niezbedne dane
    out 42h,al					;
    mov al,ah					;
    out 42h,al					;
								
    in al,61h					;
    or al, 00000011b			;	wlaczenie glosnikak
    out 61h,al					;
								
    odtworzPauze:				;
    mov ax,[czasTrwania]		;
    mov bx,4000					;
    mul bx						;
								;
    mov cx,dx					;	Przytrzymujemy wlaczony glosnik na czas grania nuty
    mov dx,ax					;
								;
								;
    mov ah,86h					;
    int 15h						;
								
    in al,61h					;
    and al,11111100b			;	wylaczamy glosnik
    out 61h,al					;
    
        
    ret
    
    odtworzPolNute:				;
    mov al,[polNutyLista+di]	;			jezeli polnuta, pobieramy z listy czestotliwosci polnutowej
    jmp odtworzDzwiek1    		;


plusMinus:						
    mov ah,06h					;
    mov dl,0ffh					;		Sprawdzamy czy uzytkownik zmienia tempo    (do al wsadzona wartosc)
    int 21h						;
    jz plusMinusWynik
    
    cmp al,'+'					;
    jne niePlus					;
    add [setB],5			;
    jnc zmianaTempa				;		jesli plus,zwiekszamy tempo
    mov [setB],255			;
    jmp zmianaTempa				;
					
    niePlus:					;
    cmp al,'-'					;
    jne plusMinusWynik			;
    sub [setB],5			;		jezeli minus, zmniejszamy tempo
    cmp [setB],25			;
    jnb zmianaTempa				;
    mov [setB],25			;
    
    
    zmianaTempa:
    call wypiszTempo
    
    plusMinusWynik:
    ret
	
bladOtwieraniaPliku:  
 
    lea dx,bladPliku		;	
    call wypisywanie		;
    call zamknijPlik		;
							;		jesli plik sie nie otworzy, to wypisujemy komunikat
    mov     ah,4ch			;
	mov	    al,0			;
	int	    21h				;
             
wypisywanie:
    push ax
    mov ah, 09h
    int 21h
    pop ax
    ret

bladMel:
	lea dx, bladMelodii		;
    mov ah,09h				;
    int 21h					;
							;		jezeli problem z melodia
    mov     ah,4ch			;
	mov	    al,0			;
	int	    21h				;

.data
dane            segment

;========POWIADOMIENIA========
	bladPliku	db "Blad pliku!",0Ah, 0Dh, "$"
	bladMelodii db "Blad melodii",0Ah,0Dh,"$"
	koniecProgramu db "Program zakonczyl swoja prace",0Ah,0Dh,"$"
	nastepnaLinia	db 0Ah,0Dh,"$",0        

	bladParametru db "Wartosc parametru poza zakresem" ,0Ah, 0Dh, "$"
	
;	na ekran
	tempoStr db "Tempo: "
	tempoWartoscStr db 3 dup(?)
	koniecCiaguZnakow db '$'
	
;	kolory
	Kolory db 00h, 10h, 20h, 30h, 40h, 50h, 60h, 70h, 30h
	Ekran equ 0b800h		; adres of display data segment
	
;========DANE========
	nazwaPliku db 64 dup(?)    
	len db 0
	
    setO dw 0 
    setB dw 0
    setD dw 0
	
	
	
	dZnalezione db 0
	oZnalezione db 0
	nuta db 0
	polnuta db 0
	kropka db 0
	
	dlugoscTytulu db 0
	tytul 128 dup(0)
	uchwytPliku dw 0
	melodia db 1024 dup(?)
	buforDOB db 64 dup(?)
	
	tempo dw ?
	nutyLista db 0, 33, 37, 41, 44, 49, 55, 62, 65
	polNutyLista db 0, 35, 39, 42, 46, 52, 58
	
	NutaOktawa dw ?
	czestotliwosc dw ?
	czasTrwania dw ?
	

dane            ends

stosik          segment
                dw    100h dup(0)
szczyt          Label word
stosik          ends

end start