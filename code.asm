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
section .data
    fmt_int: db '%d', 10, 0
section .text
    mov rsi, rdx
    mov rdi, fmt_int
    xor eax, eax
    call printf
    mov rcx, rbx
    mov rax, rcx
    cmp rax, rax
    je L0
    mov rax, r8
    add rax, qword [rbp-16]
    mov qword [rbp-8], rax
    mov r8, qword [rbp-8]
    jmp L1
L0:
    mov rax, r8
    add rax, qword [rbp-32]
    mov qword [rbp-24], rax
    mov r8, qword [rbp-24]
L1:
    mov rsi, r8
    mov rdi, fmt_int
    xor eax, eax
    call printf
L2:
    mov rax, qword [rbp-48]
    mov qword [rbp-40], rax
    mov rax, qword [rbp-40]
    cmp rax, rax
    je L3
    mov rax, r8
    add rax, qword [rbp-16]
    mov qword [rbp-56], rax
    mov r8, qword [rbp-56]
    jmp L2
L3:
    mov rax, 0
    mov qword [rbp-64], rax
L4:
    mov rax, qword [rbp-80]
    mov qword [rbp-72], rax
    mov rax, qword [rbp-72]
    cmp rax, rax
    je L5
    mov rax, qword [rbp-96]
    mov qword [rbp-88], rax
    mov r8, qword [rbp-88]
L6:
    mov rax, qword [rbp-64]
    add rax, qword [rbp-16]
    mov qword [rbp-104], rax
    mov rax, qword [rbp-104]
    mov qword [rbp-64], rax
    jmp L4
L5:
    mov rsi, r8
    mov rdi, fmt_int
    xor eax, eax
    call printf
    xor eax, eax
    leave
    ret
