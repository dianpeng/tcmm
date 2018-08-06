#ifndef ARCH_H_
#define ARCH_H_

/**
 * Architecuter related information
 */

#define MWORD_SIZE sizeof(void*)
#define PTR_SIZE   sizeof(void*)

#if defined(__x86_64__)
#define ARCH_X64
#endif // __x86_64__

#if defined(__i386__)
#define ARCH_X86
#endif // __i386__

#endif // ARCH_H_
