.intel_syntax noprefix
.section .text
.global main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 4
    mov r11, 69
    mov dword[rbp - 4], r11d

    sub rsp, 8
    lea r11, [rbp - 4]
    mov qword[rbp - 12], r11

    mov r11, [rbp - 12]
    mov r10d, dword [r11]
    mov eax, r10d
    add rsp, 12
    mov rsp, rbp
    pop rbp
    ret