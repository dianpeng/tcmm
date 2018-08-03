#ifndef CONFIG_H_
#define CONFIG_H_

#ifndef CONFIG_MAX_CALL_ARGS
#define CONFIG_MAX_CALL_ARGS 16
#endif // CONFIG_MAX_CALL_ARGS

#ifndef CONFIG_LIT_POOL_SIZE
#define CONFIG_LIT_POOL_SIZE 64
#endif // CONFIG_LIT_POOL_SIZE

#ifdef CONFIG_ALL_IN_ONE_BUILD
#define PAPI static
#else
#define PAPI
#endif // CONFIG_ALL_IN_ONE_BUILD

#endif // CONFIG_H_
