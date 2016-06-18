arm-palmos-gcc -nostartfiles -nostdlib -fPIC -ffixed-r9 -o test-arm.o test-arm.c
arm-palmos-objcopy -O binary test-arm.o test-arm.bin
