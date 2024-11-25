global main
extern printf
section .text
main:
    push rbp
    mov rbp, rsp
    sub rsp, 208  ; Reserve stack space
    mov r11, 1
    mov r10, r9
    mov r8, 1
    mov rdi, rsi
    mov rdx, rcx
    mov rax, rdx
    cmp rax, rbx
    je L0
    mov rax, r8
    add rax, qword [rbp-8]
    mov r8, rax
    jmp L1
L0:
    mov rax, r8
    add rax, qword [rbp-24]
    mov qword [rbp-16], rax
    mov r8, qword [rbp-16]
L1:
section .data
    fmt_str: db '%s', 10, 0
    str: db 'Afraz', 0     ; Format message for string
section .text
    mov rdi, fmt_str
    mov rsi, str
    xor eax, eax
    call printf
L2:
    mov rax, qword [rbp-40]
    mov qword [rbp-32], rax
    mov rax, qword [rbp-32]
    cmp rax, rbx
    je L3
    mov rax, r8
    add rax, qword [rbp-8]
    mov qword [rbp-48], rax
    mov r8, qword [rbp-48]
    jmp L2
L3:
    mov rax, 0
    mov qword [rbp-56], rax
L4:
    mov rax, qword [rbp-72]
    mov qword [rbp-64], rax
    mov rax, qword [rbp-64]
    cmp rax, rbx
    je L5
    mov rax, qword [rbp-88]
    mov qword [rbp-80], rax
    mov r8, qword [rbp-80]
L6:
    mov rax, qword [rbp-56]
    add rax, qword [rbp-8]
    mov qword [rbp-96], rax
    mov rax, qword [rbp-96]
    mov qword [rbp-56], rax
    jmp L4
L5:
    xor eax, eax
    leave
    ret
