# Text Editor (Kilo Clone)

This project is a simple text editor written in **C**, following the official [kilo tutorial](https://viewsourcecode.org/snaptoken/kilo/).  
It is **not my original creation** — I implemented it step by step by studying the guide.  

The main goal of this project was to gain a deeper understanding of the **C programming language at a low level**, and to work directly with **POSIX system calls** in order to build a functioning text editor from scratch.

---

## Features
- Opens and edits text files directly in the terminal
- Basic cursor movement and text navigation
- Simple text editing (insert and delete)
- File saving
- Status bar and message bar
- Supports Unix-like environments

---

## Learning Objectives
- Explore **low-level C programming** beyond standard libraries  
- Learn to interact with the terminal using **raw input mode**  
- Practice handling **POSIX system calls** (e.g., `read`, `write`, `open`, `tcsetattr`, etc.)  
- Understand how a minimal **text editor** works internally

---

##  Requirements
- A **Unix-like operating system** (Linux, macOS, WSL, etc.)
- A **C compiler** (e.g., `gcc` or `clang`)

---

##  Build & Run
To compile and run the editor:

```bash
make
./kilo filename.txt
```

## Showcase
![output](https://github.com/user-attachments/assets/b1582394-d44f-439a-b0da-762aa875e110)

